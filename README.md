# ProbFX
ProbFX is a Haskell library for modular probabilistic programming using algebraic effects and effect handlers. It is based on the work of:
- **Modular Probabilistic Models via Algebraic Effects** [[**Paper**](https://arxiv.org/pdf/2203.04608.pdf)] which provides a framework for defining modular and reusable probabilistic models, allowing the decision to _sample_ or _observe_ a random variable to be deferred to a choice of "model environment". The original implementation is available at [github.com/min-nguyen/prob-fx](https://github.com/min-nguyen/prob-fx).
- **Effect Handlers for Programmable Inference** [[**Paper**](https://arxiv.org/pdf/2303.01328.pdf)] which provides a framework of abstract inference algorithms that can be modularly interpreted by effect handlers, allowing specific algorithm variants to be derived and their parts recombined into new variants.

## Building and executing models

Examples of probabilistic programs written using ProbFX can be found in the `examples` directory. They show how to define a probabilistic models in terms of primitive distributions from `src/Model.hs`, and execute them using one of the inference algorithms from `src/Inference`.

In general, the process is:

1. Define an appropriate model of type `MulModel env es a`, and (optionally) a corresponding model environment type `env`.

    For example, below is a logistic regression model that takes a list of doubles `xs` and generates a list of booleans `ys`, modelling the probability of an event occurring or not:
    ```haskell
    -- | The model environment type, for readability purposes
    type LogRegrEnv =
      '[  "y" ':= Bool,   -- ^ output
          "m" ':= Double, -- ^ mean
          "c" ':= Double  -- ^ intercept
      ]

    sigmoid :: Double -> Double
    sigmoid x = 1 / (1 + exp((-1) * x))

    -- | Logistic regression model
    logRegr
      :: (Observable env "y" Bool, Observables env '["m", "c"] Double)
      => [Double]
      -> MulModel env es [Bool]
    logRegr xs = do
      -- | Specify the distributions of the model parameters
      m     <- normal 0 5 #m
      b     <- normal 0 1 #c
      sigma <- gamma' 1 1
      -- | Specify distribution of model outputs
      ys    <- mapM (\x -> do
                        -- probability of event occurring
                        p <- normal' (m * x + b) sigma
                        -- generate as output whether the event occurs
                        y <- bernoulli (sigmoid p) #y
                        pure y) xs
      pure ys
    ```
    The `Observables` constraint says that, for example, `"m"` and `"c"` are observable variables in the model environment `env` that may later be assigned some observed values of type `Double`.

    Calling a primitive distribution such as `normal 0 5 #m` lets us later provide observed values for "m" when executing the model.

    Calling a primed variant of primitive distribution such as `gamma' 1 1` will disable observed values from being provided to that distribution.

2.  Execute a model under a user-specified model environment by using one of the `Inference` library functions, producing an output in the `Sampler` monad.

    - Below simulates a logistic regression model using a list of inputs `xs = -1.00, -0.99, ..., 1.00`. The input environment `env_in` sets the model parameters `m = 2` and `c = -0.15`, but provides no values for `y`: this will result in `m` and `c` being _observed_ but `y` being _sampled_. The function `simulateWith` then simulates the model using the inputs and environment; this yields the model outputs, and an output environment `env_out` containing the values sampled for observable variables during runtime.
    ```haskell
    import Inference.MC.SIM as SIM ( simulateWith )

    -- | Simulate data points (x, y) from logistic regression
    simLogRegr :: Sampler [(Double, Bool)]
    simLogRegr = do
      -- | Specify the model inputs xs
      let xs  = map (/50) [(-50) .. 50]
      -- | Specify an input model environment that assigns observed values to the model parameters
          env_in :: LogRegrEnv = (#y := []) <:> (#m := [2]) <:> (#c := [-0.15]) <:> enil
      -- | Simulate from the logistic regression model, producing the model outputs ys and an output model environment
      (ys, env_out) <- SIM.simulateWith (logReg, xs) env_in
      pure (zip xs ys)
    ```

    - Below performs Single-Site Metropolis-Hastings inference on the same model. The environment `env_in` provides values for the model output `y`, and hence _observes_ (conditions against) them, but provides none for the model parameters `m` and `c`, and hence _samples_ for them.
    ```haskell
    import Inference.MC.SSMH as SSMH ( ssmhWith )

    -- | Single-Site Metropolis-Hastings inference for model parameters (m, c) from logistic regression
    ssmhLogRegr ::Sampler ([Double], [Double])
    ssmhLogRegr = do
      -- Simulate data points from logistic regression
      (xs, ys) <- unzip <$> simLogRegr
      -- Specify an input model environment that assigns observed values to the model outputs
      let env_in :: LogRegrEnv = (#y := ys) <:> (#m := []) <:> (#c := []) <:> enil
      -- Run SSMH inference for 2000 iterations
      envs_out :: [LogRegrEnv] <- SSMH.ssmhWith 2000 (logRegr xs) env_in
                                                     (#m <#> #c <#> vnil)
      -- Retrieve values sampled for #m and #c during SSMH
      let ms = concatMap (get #m) envs_out
          cs = concatMap (get #c) envs_out
      pure (ms, cs)
    ```
    Notice that *lists* of values are always provided to observable variables in a model environment; each run-time occurrence of that variable will then result in the head value being observed and consumed, and running out of values will default to sampling. Running the function `ssmhWith` returns a trace of output model environments, from which we can retrieve the trace of sampled model parameters via `get #m` and `get #c`. These represent the posterior distribution over `m` and `c`. (The argument `(#m <#> #c <#> vnil)` to `ssmhWith` is optional for indicating interest in learning `#m` and `#c` in particular).

3.  Run the resulting `Sampler` computation with `sampleIO :: Sampler a -> IO a` to produce a top-level `IO` computation.

    ```haskell
    sampleIO simLogRegr :: IO [(Double, Bool)]
    ```

## Visualising examples in ProbFX

The script `prob-fx.sh` is provided for visualising the outputs of example models when executed under different inference algorithms; see the file for a list of possible arguments to the script.  Alternatively, you can:
  1. Directly execute a ProbFX program via `cabal run prob-fx <arg>` (corresponding to `Main.hs`), whose output will be written to `model-output.txt`
  2. Visualise this output as a graph via `python3 graph.py <arg>`, which will be saved to `model-output.pdf`. This requires `Python3` and the Python packages `ast, matplotlib, scipy, sklearn, numpy`.

For example:
  - `examples/LinRegr.hs` implements a linear regression model that linearly relates a list of inputs `xs` and outputs `ys`.
      - `./prob-fx.sh pmhLinRegr` executes Particle Metropolis-Hastings to generate the approximative posterior distribution over the slope and intercept.
      - `./prob-fx.sh rmpfLinRegr` executes Resample-Move Metropolis-Hastings to generate the approximative posterior distribution over the slope and intercept.
  - `examples/SIR.hs` implements the [SIR](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology) (susceptible-infected-recovered) model for modelling the spread of disease during an epidemic. It includes extensions to the model that consider resusceptibility to disease (SIRS), and also the ability to vaccinate against disease (SIRSV).
      - `./prob-fx.sh simSIRSV` simulates from a model.
  - `examples/LDA.hs` implements Latent Dirichlet Allocation as a [topic model](https://www.tidytextmining.com/topicmodeling.html) for modeling the distribution over topics and words in a text document. The model defines an example vocabulary "DNA", "evolution", "parsing", and "phonology" for text documents, and assumes there are two possible topics.
      - `./prob-fx.sh ssmhLDA` : Given a pre-defined document of words, this runs Single-Site Metropolis-Hastings the generated graph shows a predictive posterior distribution over the two topics occurring in the document, and the distribution over the words occurring in each topic.
  - `examples/Radon.hs` implements a [case study](https://docs.pymc.io/en/v3/pymc-examples/examples/case_studies/multilevel_modeling.html) by Gelman and Hill as a hierarchical linear regression model, modelling the relationship between radon levels in households and whether these houses contain basements:
      - `./prob-fx.sh simRadon` : This simulates the log-radon levels of houses with basements and those without basements.
      - `./prob-fx.sh ssmhRadon` executes Single-Site Metropolis-Hastings to generate the predictive posterior distribution over "gradients" for each county; each gradient models the relationship of log-radon levels of houses with and without basements in that county.
  - `examples/School.hs` implements another Gelman and Hill [case study](https://cran.r-project.org/web/packages/rstan/vignettes/rstan.html) as a hierarchical model, which quantifies the effect of coaching programs from 8 different schools on students' SAT-V scores:
      - `./prob-fx.sh ssmhSchool` executes Single-Site Metropolis-Hastings to generate a posterior distribution over model parameter `mu`, being the effect of general coaching programs on SAT scores, and each school's posterior distribution on model parameter `theta`, being the variation of their effect on SAT scores.