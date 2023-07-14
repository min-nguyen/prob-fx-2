
using BenchmarkTools
using DataFrames
using CSV
using Statistics
using GenDistributions # Requires: https://github.com/probcomp/GenDistributions.jl/blob/main/src/GenDistributions.jl
using Distributions
using Gen

input_file  = "params.txt"
output_file = "benchmarks-gen.csv"
fixed_ssmh_steps = 100
fixed_smc_particles = 100
fixed_pmh_mhsteps   = 50
fixed_pmh_particles = 10
fixed_rmpf_particles = 10
fixed_rmpf_mhsteps   = 1
fixed_lr_size = 50
fixed_hmm_size = 50
fixed_lda_size = 50

const dirichlet = DistributionsBacked(alpha -> Dirichlet(alpha), (true,), true, Vector{Float64})

function parseParams()
  data   = filter(xs -> xs[1] != '#', readlines(input_file))
  params = Vector{Vector{Int64}}(undef, length(data))
  for (row_idx, row) in enumerate(data)
    params[row_idx] = [parse(Int, d) for d in split(row, ",")]
  end
  return params
end

function parseBenchmark(label::String, row)
  fileStream = open(output_file,"a")
  write(fileStream, label * ",")
  for (i, t) in enumerate(row)
    write(fileStream, string(t))
    if i < length(row)
      write(fileStream, ",")
    end
  end
  write(fileStream, "\n")
  println(label * ": " * string(row))
  close(fileStream)
end

######################################## LDA

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

fixed_vocab      = ["DNA", "evolution", "parsing", "phonology"]
fixed_vocab_size = 4
fixed_topics     = 2
fixed_words      = ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]

function ldaData(n_words::Int)
  words     = Vector{String}(undef, n_words)
  for i in 1:n_words
    words[i] = fixed_words[mod(i, length(fixed_words)) + 1]
  end
  return wordsToIdxs(words)
end

@gen function ldaGuide(n_words::Int)
  @param doc_topic_conc :: Vector{Float64}
  @param topic1_word_conc :: Vector{Float64}
  @param topic2_word_conc :: Vector{Float64}
  doc_topic_ps  = @trace(dirichlet(doc_topic_conc), :doc_topic_ps)
  topic_word_ps = Vector{Vector{Float64}}(undef, fixed_topics)

  topic_word_ps[1] = @trace(dirichlet(topic1_word_conc), (:topic_word_ps, 1))
  topic_word_ps[2] = @trace(dirichlet(topic2_word_conc), (:topic_word_ps, 2))
end

@gen function lda(n_words::Int)
  # set list of topic probabilities for the document
  doc_topic_ps  = @trace(dirichlet(ones(fixed_topics)), :doc_topic_ps)

  # set list of word probabilities for each topic
  topic_word_ps = Vector{Vector{Float64}}(undef, fixed_topics)
  for i in 1:fixed_topics
    topic_word_ps[i] = @trace(dirichlet(ones(fixed_vocab_size)), (:topic_word_ps, i))
  end

  # initialise list of word indexes observed
  word_idxs = Vector{Int64}(undef, n_words)
  # # initialis list of topics observed
  topic_obs = Vector{Int64}(undef, n_words)

  for i in 1:n_words
    # observe a topic
    topic_obs[i] = @trace(categorical(doc_topic_ps), (:topic, i))
    # fetch the topic's corresponding word distribution
    word_ps      = topic_word_ps[topic_obs[i]]
    # observe a word index for that topic
    word_idxs[i] = @trace(categorical(word_ps), (:word, i))
  end
  # println(idxsToWords(word_idxs))
  return word_idxs
end

function ssmhLDA(num_iters::Int, n_words::Int)
  word_idxs = ldaData(n_words)
  constraints = choicemap()
  for (i, word) in enumerate(word_idxs)
      constraints[(:word, i)] = word
  end

  (trace, _) = generate(lda, (n_words,), constraints)

  for iter=1:num_iters
      (trace, _) = metropolis_hastings(trace, Gen.select(:doc_topic_ps))
      (trace, _) = metropolis_hastings(trace, Gen.select((:topic_word_ps, 1)))
      (trace, _) = metropolis_hastings(trace, Gen.select((:topic_word_ps, 2)))
  end

  choices = get_choices(trace)
  return (choices[:doc_topic_ps])
end

function smcLDA(num_particles::Int, n_words::Int)
  word_idxs = ldaData(n_words)
  init_obs = Gen.choicemap(((:word, 1), word_idxs[1]))
  state = Gen.initialize_particle_filter(lda, (0,), init_obs, num_particles)

  for t=1:n_words-1
      Gen.maybe_resample!(state, ess_threshold=num_particles/2)
      obs = Gen.choicemap(((:word, t), word_idxs[t+1]))
      Gen.particle_filter_step!(state, (t,), (UnknownChange(),), obs)
  end
  return Gen.sample_unweighted_traces(state, num_particles)
end

function rmpfLDA(num_particles::Int, num_mhsteps::Int, n_words::Int)
  word_idxs = ldaData(n_words)
  init_obs = Gen.choicemap(((:word, 1), word_idxs[1]))
  state = Gen.initialize_particle_filter(lda, (0,), init_obs, num_particles)

  for t=1:n_words-1
      for i=1:num_particles
        initial_choices = Gen.select(:doc_topic_ps, (:topic_word_ps, 1), (:topic_word_ps, 2))
        for r=1:num_mhsteps
          state.traces[i], _  = metropolis_hastings(state.traces[i], initial_choices)
        end
      end

      Gen.maybe_resample!(state, ess_threshold=num_particles/2)
      obs = Gen.choicemap(((:word, t), word_idxs[t+1]))
      Gen.particle_filter_step!(state, (t,), (UnknownChange(),), obs)
  end
  return Gen.sample_unweighted_traces(state, num_particles)
end

function bbviLDA(num_iters::Int, n_samples::Int, n_words::Int)
  word_idxs = ldaData(n_words)
  constraints = choicemap()
  for (i, word) in enumerate(word_idxs)
      constraints[(:word, i)] = word
  end

  init_param!(ldaGuide, :doc_topic_conc, ones(fixed_topics) :: Vector{Float64})
  init_param!(ldaGuide, :topic1_word_conc, ones(fixed_vocab_size) :: Vector{Float64})
  init_param!(ldaGuide, :topic2_word_conc, ones(fixed_vocab_size) :: Vector{Float64})
  update = ParamUpdate(GradientDescent(1e-12, 100000), ldaGuide)
  black_box_vi!(lda, (n_words,), constraints, ldaGuide, (n_words,), update;
    iters=num_iters, samples_per_iter=n_samples, verbose=true)
end

function bench_LDA_SSMH(lda_range)
  results = Array{Any}(undef, length(lda_range))
  for (i, n_words) in enumerate(lda_range)
    b = @benchmark ssmhLDA(fixed_ssmh_steps, $n_words)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("LatDiri-[ ]-SSMH-" * string(fixed_ssmh_steps), results)
end

function bench_LDA_SMC(lda_range)
  results = Array{Any}(undef, length(lda_range))
  for (i, n_words) in enumerate(lda_range)
    b = @benchmark smcLDA(fixed_smc_particles, $n_words)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("LatDiri-[ ]-MPF-" * string(fixed_smc_particles), results)
end

function bench_LDA_PMH(lda_range)
  parseBenchmark("LatDiri-[ ]-PMH-" * string(fixed_pmh_mhsteps) * "-" * string(fixed_pmh_particles), zeros(length(lda_range)))
end

function bench_LDA_RMPF(lda_range)
  results = Array{Any}(undef, length(lda_range))
  for (i, n_words) in enumerate(lda_range)
    b = @benchmark rmpfLDA(fixed_rmpf_particles, fixed_rmpf_mhsteps, $n_words)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("LatDiri-[ ]-RMPF-" * string(fixed_rmpf_particles)  * "-" * string(fixed_rmpf_mhsteps), results)
end

######################################## HMM

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

function ssmhHMM(num_iters::Int, n_datapoints::Int)
  ys = hmmData(n_datapoints)
  constraints = choicemap()
  for (i, y) in enumerate(ys)
      constraints[(:y, i)] = y
  end

  (trace, _) = generate(hmm, (n_datapoints,), constraints)

  for iter=1:num_iters
      (trace, _) = metropolis_hastings(trace, Gen.select(:trans_p))
      (trace, _) = metropolis_hastings(trace, Gen.select(:obs_p))
  end

  choices = get_choices(trace)
  return (choices[:trans_p], choices[:obs_p])
end

function smcHMM(num_particles::Int, n_datapoints::Int)
  ys = hmmData(n_datapoints)
  init_obs = Gen.choicemap(((:y, 1), ys[1]))
  state = Gen.initialize_particle_filter(hmm, (0,), init_obs, num_particles)

  # steps
  for t=1:length(ys)-1
      Gen.maybe_resample!(state, ess_threshold=num_particles/2)
      obs = Gen.choicemap(((:y, t), ys[t+1]))
      Gen.particle_filter_step!(state, (t,), (UnknownChange(),), obs)
  end
end

function rmpfHMM(num_particles::Int, num_mhsteps::Int, n_datapoints::Int)
  ys = hmmData(n_datapoints)
  init_obs = Gen.choicemap(((:y, 1), ys[1]))
  state = Gen.initialize_particle_filter(hmm, (0,), init_obs, num_particles)

  # steps
  for t=1:length(ys)-1

      for i=1:num_particles
        initial_choices = Gen.select(:trans_p, :obs_p)
        for r=1:num_mhsteps
          state.traces[i], _  = metropolis_hastings(state.traces[i], initial_choices)
        end
      end

      Gen.maybe_resample!(state, ess_threshold=num_particles/2)
      obs = Gen.choicemap(((:y, t), ys[t+1]))
      Gen.particle_filter_step!(state, (t,), (UnknownChange(),), obs)
  end
end

function bbviHMM(num_iters::Int, n_samples::Int, n_datapoints::Int)
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

function bench_HMM_SSMH(hmm_range)
  results = Array{Any}(undef, length(hmm_range))
  for (i, n_datapoints) in enumerate(hmm_range)
    b = @benchmark ssmhHMM(fixed_ssmh_steps, $n_datapoints)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("HidMark-[ ]-SSMH-" * string(fixed_ssmh_steps), results)
end

function bench_HMM_SMC(hmm_range)
  results = Array{Any}(undef, length(hmm_range))
  for (i, n_datapoints) in enumerate(hmm_range)
    b = @benchmark smcHMM(fixed_smc_particles, $n_datapoints)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("HidMark-[ ]-MPF-" * string(fixed_smc_particles), results)
end

function bench_HMM_PMH(hmm_range)
  parseBenchmark("HidMark-[ ]-PMH-" * string(fixed_pmh_mhsteps)  * "-" * string(fixed_pmh_particles), zeros(length(hmm_range)))
end

function bench_HMM_RMPF(hmm_range)
  results = Array{Any}(undef, length(hmm_range))
  for (i, n_datapoints) in enumerate(hmm_range)
    b = @benchmark rmpfHMM(fixed_rmpf_particles, fixed_rmpf_mhsteps, $n_datapoints)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("HidMark-[ ]-RMPF-" * string(fixed_rmpf_particles)  * "-" * string(fixed_rmpf_mhsteps), results)
end

######################################## LIN REGR

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

function ssmhLinRegr(num_iters::Int, n_datapoints::Int)
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

function rmpfLinRegr(num_particles::Int, num_mhsteps::Int, n_datapoints::Int)
  (xs, ys) = linRegrData(n_datapoints)
  # construct initial observations
  init_obs = Gen.choicemap(((:y, 1), ys[1]))
  state = Gen.initialize_particle_filter(linRegrSMC, (0,), init_obs, num_particles)

  # steps
  for t=1:length(ys)-1

      for i=1:num_particles
        initial_choices = Gen.select(:m, :c, :σ)
        for r=1:num_mhsteps
          state.traces[i], _  = metropolis_hastings(state.traces[i], initial_choices)
        end
      end

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

function bench_LR_SSMH(lr_range)
  results = Array{Any}(undef, length(lr_range))
  for (i, n_datapoints) in enumerate(lr_range)
    b = @benchmark ssmhLinRegr(fixed_ssmh_steps, $n_datapoints)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("LinRegr-[ ]-SSMH-" * string(fixed_ssmh_steps), results)
end

function bench_LR_SMC(lr_range)
  results = Array{Any}(undef, length(lr_range))
  for (i, n_datapoints) in enumerate(lr_range)
    b = @benchmark smcLinRegr(fixed_smc_particles, $n_datapoints)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("LinRegr-[ ]-MPF-" * string(fixed_smc_particles), results)
end

function bench_LR_PMH(lr_range)
  parseBenchmark("LinRegr-[ ]-PMH-" * string(fixed_pmh_mhsteps) * "-" * string(fixed_pmh_particles), zeros(length(lr_range)))
end

function bench_LR_RMPF(lr_range)
  results = Array{Any}(undef, length(lr_range))
  for (i, n_datapoints) in enumerate(lr_range)
    b = @benchmark rmpfLinRegr(fixed_rmpf_particles, fixed_rmpf_mhsteps, $n_datapoints)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("LinRegr-[ ]-RMPF-" * string(fixed_rmpf_particles) * "-" * string(fixed_rmpf_mhsteps), results)
end


######################################## SSMH

function bench_SSMH_LR(ssmh_range)
  results = Array{Any}(undef, length(ssmh_range))
  for (i, ssmh_steps) in enumerate(ssmh_range)
    b = @benchmark ssmhLinRegr($ssmh_steps, fixed_lr_size)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("SSMH-[ ]-LinRegr-" * string(fixed_lr_size), results)
end

function bench_SSMH_HMM(ssmh_range)
  results = Array{Any}(undef, length(ssmh_range))
  for (i, ssmh_steps) in enumerate(ssmh_range)
    b = @benchmark ssmhHMM($ssmh_steps, fixed_hmm_size)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("SSMH-[ ]-HidMark-" * string(fixed_hmm_size), results)
end

function bench_SSMH_LDA(ssmh_range)
  results = Array{Any}(undef, length(ssmh_range))
  for (i, ssmh_steps) in enumerate(ssmh_range)
    b = @benchmark ssmhLDA($ssmh_steps, fixed_lda_size)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("SSMH-[ ]-LatDiri-" * string(fixed_lda_size), results)
end

######################################## SMC

function bench_SMC_LR(smc_range)
  results = Array{Any}(undef, length(smc_range))
  for (i, n_particles) in enumerate(smc_range)
    b = @benchmark smcLinRegr($n_particles, fixed_lr_size)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("MPF-[ ]-LinRegr-" * string(fixed_lr_size), results)
end

function bench_SMC_HMM(smc_range)
  results = Array{Any}(undef, length(smc_range))
  for (i, n_particles) in enumerate(smc_range)
    b = @benchmark smcHMM($n_particles, fixed_hmm_size)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("MPF-[ ]-HidMark-" * string(fixed_hmm_size), results)
end

function bench_SMC_LDA(smc_range)
  results = Array{Any}(undef, length(smc_range))
  for (i, n_particles) in enumerate(smc_range)
    b = @benchmark smcHMM($n_particles, fixed_lda_size)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("MPF-[ ]-LatDiri-" * string(fixed_lda_size), results)
end

######################################## PMH

function bench_PMH_LR(pmh_range)
  parseBenchmark("PMH-" * string(fixed_pmh_mhsteps) * "-[ ]-LinRegr-" * string(fixed_lr_size), zeros(length(pmh_range)))
end

function bench_PMH_HMM(pmh_range)
  parseBenchmark("PMH-" * string(fixed_pmh_mhsteps) * "-[ ]-HidMark-" * string(fixed_hmm_size), zeros(length(pmh_range)))
end

function bench_PMH_LDA(pmh_range)
  parseBenchmark("PMH-" * string(fixed_pmh_mhsteps) * "-[ ]-LatDiri-" * string(fixed_lda_size), zeros(length(pmh_range)))
end

######################################## RMPF

function bench_RMPF_LR(rmpf_range)
  results = Array{Any}(undef, length(rmpf_range))
  for (i, n_mhsteps) in enumerate(rmpf_range)
    b = @benchmark rmpfLinRegr(fixed_rmpf_particles, $n_mhsteps, fixed_lr_size)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("RMPF-" * string(fixed_rmpf_particles) * "-[ ]-LinRegr-" * string(fixed_lr_size), results)
end

function bench_RMPF_HMM(rmpf_range)
  results = Array{Any}(undef, length(rmpf_range))
  for (i, n_mhsteps) in enumerate(rmpf_range)
    b = @benchmark rmpfHMM(fixed_rmpf_particles, $n_mhsteps, fixed_hmm_size)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("RMPF-" * string(fixed_rmpf_particles) * "-[ ]-HidMark-" * string(fixed_hmm_size), results)
end

function bench_RMPF_LDA(rmpf_range)
  results = Array{Any}(undef, length(rmpf_range))
  for (i, n_mhsteps) in enumerate(rmpf_range)
    b = @benchmark rmpfHMM(fixed_rmpf_particles, $n_mhsteps, fixed_lda_size)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("RMPF-" * string(fixed_rmpf_particles) * "-[ ]-LatDiri-" * string(fixed_lda_size), results)
end

######################################## Top-level benchmarks

function bench_LR(lr_range)
  parseBenchmark("Num datapoints", lr_range)
  bench_LR_SSMH(lr_range)
  bench_LR_SMC(lr_range)
  bench_LR_PMH(lr_range)
  bench_LR_RMPF(lr_range)
end

function bench_HMM(hmm_range)
  parseBenchmark("Num nodes", hmm_range)
  bench_HMM_SSMH(hmm_range)
  bench_HMM_SMC(hmm_range)
  bench_HMM_PMH(hmm_range)
  bench_HMM_RMPF(hmm_range)
end

function bench_LDA(lda_range)
  parseBenchmark("Num words", lda_range)
  bench_LDA_SSMH(lda_range)
  bench_LDA_SMC(lda_range)
  bench_LDA_PMH(lda_range)
  bench_LDA_RMPF(lda_range)
end

function bench_SSMH(ssmh_range)
  parseBenchmark("Num SSMH steps", ssmh_range)
  bench_SSMH_LR(ssmh_range)
  bench_SSMH_HMM(ssmh_range)
  bench_SSMH_LDA(ssmh_range)
end

function bench_SMC(smc_range)
  parseBenchmark("Num MPF particles", smc_range)
  bench_SMC_LR(smc_range)
  bench_SMC_HMM(smc_range)
  bench_SMC_LDA(smc_range)
end

function bench_PMH(pmh_range)
  parseBenchmark("Num PMH particles", pmh_range)
  bench_PMH_LR(pmh_range)
  bench_PMH_HMM(pmh_range)
  bench_PMH_LDA(pmh_range)
end

function bench_RMPF(rmpf_range)
  parseBenchmark("Num RMPF mh steps", rmpf_range)
  bench_RMPF_LR(rmpf_range)
  bench_RMPF_HMM(rmpf_range)
  bench_RMPF_LDA(rmpf_range)
end

function runBenchmarks()
  params = parseParams()
  lr_range = params[1]
  hmm_range = params[2]
  lda_range = params[3]
  ssmh_range = params[4]
  smc_range = params[5]
  pmh_range = params[6]
  rmpf_range = params[7]
  bench_LR(lr_range)
  bench_HMM(hmm_range)
  bench_LDA(lda_range)
  bench_SSMH(ssmh_range)
  bench_SMC(smc_range)
  bench_PMH(pmh_range)
  bench_RMPF(rmpf_range)
end

runBenchmarks()