
using BenchmarkTools
using DataFrames
using CSV
using Statistics
using GenDistributions
using Distributions
using Gen

fileStream = open("benchmarks-gen.csv","a")

lr_range = [100,200]#,300,400,500]
hmm_range = [100,200]#,300,400,500]
fixed_mh_steps = 100
fixed_smc_particles = 100
fixed_bbvi_steps = 50
fixed_bbvi_samples = 10

const dirichlet = DistributionsBacked(alpha -> Dirichlet(alpha), (true,), true, Vector{Float64})


function parseBenchmark(label::String, row)
  write(fileStream, label * ",")
  for (i, t) in enumerate(row)
    write(fileStream, string(t))
    if i < length(row)
      write(fileStream, ",")
    end
  end
  write(fileStream, "\n")
end

##### LDA
function wordsToIdxs(words)
  n_words = length(words)
  word_idxs = Vector{Int64}(undef, n_words)
  for i in 1:n_words
    word_idxs[i] = findfirst(w -> w==words[i], fixed_vocab)
  end
  return word_idxs
end

function idxsToWords(word_idxs)
  n_words = length(word_idxs)
  words = Vector{String}(undef, n_words)
  for i in 1:n_words
    words[i] = fixed_vocab[word_idxs[i]]
  end
  return word_idxs
end

fixed_vocab     = ["DNA", "evolution", "parsing", "phonology"]
fixed_topics    = 2

@gen function topicModel(n_words)
  # initialise list of corresponding word indexes observed
  words     = Vector{String}(undef, n_words)
  word_idxs = Vector{Int64}(undef, n_words)

  ps = @trace(dirichlet(ones(fixed_topics)), :ps)
  println(ps)
  # doc_topic_ps ~ Dirichlet(ones(fixed_topics))
  # # print(word_idxs)
  # if topic_word_ps === missing
  #   # initialise list of word probabilities for each topic
  #   topic_word_ps = Vector{Vector{Float64}}(undef, fixed_topics)
  # end

  # # set list of topic probabilities for the document
  # for i in 1:fixed_topics
  #   # set list of word probabilities for each topic
  #   topic_word_ps[i] ~ Dirichlet((ones(length(fixed_vocab))))
  # end

  # # initialise list of topics observed
  # topic_obs = Vector{Int64}(undef, n_words)

  # for i in 1:n_words
  #   # observe a topic
  #   topic_obs[i] ~ Categorical(doc_topic_ps)
  #   # fetch the topic's corresponding word distribution
  #   word_ps = topic_word_ps[topic_obs[i]]
  #   # observe a word index for that topic
  #   word_idxs[i] ~ Categorical(word_ps)
  # end
  # # print(word_idxs)

  # # print(word_idxs)
  # # print(doc_topic_ps)
  # # print(topic_word_ps)
  # return word_idxs
end

topicModel(5)

##### HMM

function hmmData(n_datapoints::Int)
  return hmm(n_datapoints)
end

@gen function hmmGuide(T::Int)
  @param trans_p_a :: Float64
  @param trans_p_b :: Float64
  @param obs_p_a :: Float64
  @param obs_p_b :: Float64
  trans_p = @trace(beta(exp(trans_p_a), exp(trans_p_b)), :trans_p)
  obs_p   = @trace(beta(exp(obs_p_a), exp(obs_p_b)), :obs_p)
end

@gen function hmm(T::Int)
  trans_p = @trace(beta(exp(4.5), exp(4.5)), :trans_p)
  obs_p   = @trace(beta(exp(4.5), exp(4.5)), :obs_p)
  x  = 0.::Float64
  ys = Array{Float64}(undef, T)
  for t=1:T
    dX    = @trace(bernoulli(trans_p), (:x, t))
    x     = x + Float64(Int(dX))
    ys[t] = @trace(normal(x, obs_p), (:y, t))
  end
  return ys
end

function mhHMM(num_iters::Int, n_datapoints::Int)
  # Create a set of constraints fixing the
  # y coordinates to the observed y values
  ys = hmmData(n_datapoints)
  constraints = choicemap()
  for (i, y) in enumerate(ys)
      constraints[(:y, i)] = y
  end

  # # Run the model, constrained by `constraints`,
  # # to get an initial execution trace
  (trace, _) = generate(hmm, (n_datapoints,), constraints)

  # Iteratively update the slope then the intercept,
  # using Gen's metropolis_hastings operator.
  for iter=1:num_iters
      (trace, _) = metropolis_hastings(trace, Gen.select(:trans_p))
      (trace, _) = metropolis_hastings(trace, Gen.select(:obs_p))
  end

  # From the final trace, read out the slope and
  # the intercept.
  choices = get_choices(trace)
  return (choices[:trans_p], choices[:obs_p])
end

function smcHMM(num_particles::Int, n_datapoints::Int)
  ys = hmmData(n_datapoints)
  # construct initial observations
  init_obs = Gen.choicemap(((:y, 1), ys[1]))
  state = Gen.initialize_particle_filter(hmm, (0,), init_obs, num_particles)

  # steps
  for t=1:length(ys)-1
      Gen.maybe_resample!(state, ess_threshold=num_particles/2)
      obs = Gen.choicemap(((:y, t), ys[t+1]))
      Gen.particle_filter_step!(state, (t,), (UnknownChange(),), obs)
  end
end

function bbviHMM(num_iters::Int, n_samples::Int, n_datapoints::Int)
  # Create a set of constraints fixing the
  # y coordinates to the observed y values
  ys = hmmData(n_datapoints)
  constraints = choicemap()
  for (i, y) in enumerate(ys)
    constraints[(:y, i)] = y
  end

  init_param!(hmmGuide, :trans_p_a, 5. :: Float64)
  init_param!(hmmGuide, :trans_p_b, 5.  :: Float64)
  init_param!(hmmGuide, :obs_p_a, 5.  :: Float64)
  init_param!(hmmGuide, :obs_p_b, 5.  :: Float64)

  update = ParamUpdate(GradientDescent(1e-12, 100000), hmmGuide)
  black_box_vi!(hmm, (n_datapoints,), constraints, hmmGuide, (n_datapoints,), update;
    iters=num_iters, samples_per_iter=n_samples, verbose=true)
end


function bench_HMM_MH()
  results = Array{Any}(undef, length(hmm_range))
  for (i, n_datapoints) in enumerate(hmm_range)
    b = @benchmark mhHMM(fixed_mh_steps, $n_datapoints)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("HMM-[ ]-MH-" * string(fixed_mh_steps), results)
end

function bench_HMM_SMC()
  results = Array{Any}(undef, length(hmm_range))
  for (i, n_datapoints) in enumerate(hmm_range)
    b = @benchmark smcHMM(fixed_smc_particles, $n_datapoints)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("HMM-[ ]-SMC-" * string(fixed_smc_particles), results)
end

function bench_HMM_BBVI()
  results = Array{Any}(undef, length(hmm_range))
  for (i, n_datapoints) in enumerate(hmm_range)
    b = @benchmark bbviHMM(fixed_bbvi_steps, fixed_bbvi_samples, $n_datapoints)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("HMM-[ ]-BBVI-" * string(fixed_bbvi_steps) * "-" * string(fixed_bbvi_samples), results)
end

##### LIN REGR

function linRegrData(n_datapoints::Int)
  xs = range(0.0, n_datapoints, length=n_datapoints)
  ys = map(x -> x * 3, xs)
  return (xs, ys)
end

@gen function linRegr(xs)
  m = @trace(normal(0, exp(3)), :m)
  c = @trace(normal(0, exp(5)), :c)
  σ = @trace(uniform(1, 3), :σ)
  for (i, x) in enumerate(xs)
      @trace(normal(m * x + c, exp(σ)), (:y, i))
  end
end

@gen function linRegrSMC(T::Int)
  m = @trace(normal(0, exp(3)), :m)
  c = @trace(normal(0, exp(5)), :c)
  σ = @trace(uniform(1, 3), :σ)
  for t=1:T
      @trace(normal(m * t + c, exp(σ)), (:y, t))
  end
end

@gen function linRegrGuide(xs)
  @param m_mu
  @param m_std
  @param c_mu
  @param c_std
  m = @trace(normal(m_mu, exp(m_std)), :m)
  c = @trace(normal(c_mu, exp(c_std)), :c)
end

function mhLinRegr(num_iters::Int, n_datapoints::Int)
  # Create a set of constraints fixing the
  # y coordinates to the observed y values
  (xs, ys) = linRegrData(n_datapoints)
  constraints = choicemap()
  for (i, y) in enumerate(ys)
      constraints[(:y, i)] = y
  end

  # Run the model, constrained by `constraints`,
  # to get an initial execution trace
  (trace, _) = generate(linRegr, (xs,), constraints)

  # Iteratively update the slope then the intercept,
  # using Gen's metropolis_hastings operator.
  for iter=1:num_iters
      (trace, _) = metropolis_hastings(trace, Gen.select(:m))
      (trace, _) = metropolis_hastings(trace, Gen.select(:c))
      (trace, _) = metropolis_hastings(trace, Gen.select(:σ))
  end

  # From the final trace, read out the slope and
  # the intercept.
  choices = get_choices(trace)
  return (choices[:m], choices[:c])
end

function smcLinRegr(num_particles::Int, n_datapoints::Int)

  (xs, ys) = linRegrData(n_datapoints)
  # construct initial observations
  init_obs = Gen.choicemap(((:y, 1), ys[1]))
  state = Gen.initialize_particle_filter(linRegrSMC, (0,), init_obs, num_particles)

  # steps
  for t=1:length(ys)-1
      Gen.maybe_resample!(state, ess_threshold=num_particles/2)
      obs = Gen.choicemap(((:y, t), ys[t+1]))
      Gen.particle_filter_step!(state, (t,), (UnknownChange(),), obs)
  end

  # return a sample of unweighted traces from the weighted collection
  num_samples = num_particles
  return Gen.sample_unweighted_traces(state, num_samples)
end

function bbviLinRegr(num_iters::Int, n_samples::Int, n_datapoints::Int)
  # Create a set of constraints fixing the
  # y coordinates to the observed y values
  (xs, ys) = linRegrData(n_datapoints)
  constraints = choicemap()
  for (i, y) in enumerate(ys)
    constraints[(:y, i)] = y
  end

  init_param!(linRegrGuide, :m_mu, 0.)
  init_param!(linRegrGuide, :m_std, 0.)
  init_param!(linRegrGuide, :c_mu, 0.)
  init_param!(linRegrGuide, :c_std, 0.)

  update = ParamUpdate(GradientDescent(1e-6, 100000), linRegrGuide)
  black_box_vi!(linRegr, (xs,), constraints, linRegrGuide, (xs,), update;
    iters=num_iters, samples_per_iter=n_samples, verbose=true)
end


function bench_LR_MH()
  results = Array{Any}(undef, length(lr_range))
  for (i, n_datapoints) in enumerate(lr_range)
    b = @benchmark mhLinRegr(fixed_mh_steps, $n_datapoints)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("LR-[ ]-MH-" * string(fixed_mh_steps), results)
end

function bench_LR_SMC()
  results = Array{Any}(undef, length(lr_range))
  for (i, n_datapoints) in enumerate(lr_range)
    b = @benchmark smcLinRegr(fixed_smc_particles, $n_datapoints)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("LR-[ ]-SMC-" * string(fixed_smc_particles), results)
end

# bench_LR_SMC()

function bench_LR_BBVI()
  results = Array{Any}(undef, length(lr_range))
  for (i, n_datapoints) in enumerate(lr_range)
    b = @benchmark bbviLinRegr(fixed_bbvi_steps, fixed_bbvi_samples, $n_datapoints)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("LR-[ ]-BBVI-" * string(fixed_bbvi_steps) * "-" * string(fixed_bbvi_samples), results)
end

function bench_LR()
  parseBenchmark("Dataset size", lr_range)
  bench_LR_MH()
  bench_LR_SMC()
  bench_LR_BBVI()
end


# bbviLinRegr(100, 100, 100)

# bench_LR()

#   mhtrace = @benchmark sample(linRegrInf(200), MH(), 2000)
#   parseBenchmark("linRegr/MH/data-size/200", mhtrace)

#   print("linRegr/MH/data-size/400" )
#   mhtrace = @benchmark sample(linRegrInf(400), MH(), 2000)
#   parseBenchmark("linRegr/MH/data-size/400", mhtrace)

#   print("linRegr/MH/data-size/600" )
#   mhtrace = @benchmark sample(linRegrInf(600), MH(), 2000)
#   parseBenchmark("linRegr/MH/data-size/600", mhtrace)

#   print("linRegr/MH/data-size/800" )
#   mhtrace = @benchmark sample(linRegrInf(800), MH(), 2000)
#   parseBenchmark("linRegr/MH/data-size/800", mhtrace)

#   print("linRegr/MH/data-size/1000" )
#   mhtrace = @benchmark sample(linRegrInf(1000), MH(), 2000)
#   parseBenchmark("linRegr/MH/data-size/1000", mhtrace)
# end

# print(mhLinRegr(50, 50))