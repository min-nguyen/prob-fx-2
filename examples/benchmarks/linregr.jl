using Gen
using BenchmarkTools
using DataFrames
using CSV
using Statistics

lr_range = [100,200]#,300,400,500]

fixed_mh_steps = 100
fixed_bbvi_steps = 50
fixed_bbvi_samples = 10

fileStream = open("benchmarks-gen.csv","a")

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

@gen function guide(xs)
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

function bbviLinRegr(num_iters::Int, n_samples::Int, n_datapoints::Int)
  # Create a set of constraints fixing the
  # y coordinates to the observed y values
  (xs, ys) = linRegrData(n_datapoints)
  constraints = choicemap()
  for (i, y) in enumerate(ys)
    constraints[(:y, i)] = y
  end

  init_param!(guide, :m_mu, 0.)
  init_param!(guide, :m_std, 0.)
  init_param!(guide, :c_mu, 0.)
  init_param!(guide, :c_std, 0.)

  update = ParamUpdate(GradientDescent(1e-6, 100000), guide)
  black_box_vi!(linRegr, (xs,), constraints, guide, (xs,), update;
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

function bench_LR_BBVI()
  init_param!(guide, :m_mu, 0.)
  init_param!(guide, :m_std, 0.)
  init_param!(guide, :c_mu, 0.)
  init_param!(guide, :c_std, 0.)
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
  bench_LR_BBVI()



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