using Gen
using BenchmarkTools
using DataFrames
using CSV
using Statistics

lr_range = [100,200]#,300,400,500]
fixed_mh_steps = 100

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
  m = @trace(normal(0, 3), :m)
  c = @trace(normal(0, 5), :c)
  σ = @trace(uniform(1, 3), :σ)
  for (i, x) in enumerate(xs)
      @trace(normal(m * x + c, σ), "y-$i")
  end
end

function mhLinRegr(num_iters::Int, n_datapoints::Int)
  # Create a set of constraints fixing the
  # y coordinates to the observed y values
  (xs, ys) = linRegrData(n_datapoints)
  constraints = choicemap()
  for (i, y) in enumerate(ys)
      constraints["y-$i"] = y
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

function particle_filter(n_particles::Int, n_datapoints::Int)

  # construct initial observations
  (xs, ys) = linRegrData(n_datapoints)

  init_obs = Gen.choicemap((:y0, ys[1]))
  state = Gen.initialize_particle_filter(linRegr, (xs,), init_obs, n_particles)

  # steps
  for t=1:length(ys)-1
      Gen.maybe_resample!(state, ess_threshold=n_particles/2)
      obs = Gen.choicemap(("y-$t", ys[t+1]))
      Gen.particle_filter_step!(state, (t,), (UnknownChange(),), obs)
  end

  # return a sample of unweighted traces from the weighted collection
  return Gen.sample_unweighted_traces(state, 1)
end;


function bench_LR()
  parseBenchmark("Dataset size", lr_range)
  results = Array{Any}(undef, length(lr_range))
  for (i, n_datapoints) in enumerate(lr_range)
    b = @benchmark mhLinRegr(fixed_mh_steps, $n_datapoints)
    t = mean(b.times)/(1000000000)
    results[i] = mean(b.times)/(1000000000)
  end
  parseBenchmark("LR-[ ]-MH-" * string(fixed_mh_steps), results)


end

particle_filter(10, 10)

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