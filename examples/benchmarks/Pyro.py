import torch
import pyro

pyro.set_rng_seed(101)

fixed_mh_steps = 100
fixed_smc_particles = 100
fixed_rmsmc_particles = 10
fixed_rmsmc_mhsteps   = 1
fixed_pmmh_mhsteps   = 50
fixed_pmmh_particles = 10
fixed_bbvi_steps = 50
fixed_bbvi_samples = 10
fixed_invi_steps = 50
fixed_invi_samples = 10
fixed_lr_datasize_inf = 50
fixed_hmm_datasize_inf = 50
fixed_lda_datasize_inf = 50
fixed_pmmh_mhsteps_inf = 50
fixed_bbvi_samples_inf = 10

#LR-[ ]-MH-100, LR-[ ]-SMC-100, LR-[ ]-RMSMC-10-1, LR-[ ]-PMMH-50-10, LR-[ ]-BBVI-50-10
lr_range = [100,200,300,400,500]
#HMM-[ ]-MH-100, HMM-[ ]-SMC-100, HMM-[ ]-RMSMC-10-1, HMM-[ ]-PMMH-50-10, HMM-[ ]-BBVI-50-10
hmm_range = [100,200,300,400,500]
#LDA-[ ]-MH-100, LDA-[ ]-SMC-100, LDA-[ ]-RMSMC-10-1, LDA-[ ]-PMMH-50-10, LDA-[ ]-BBVI-50-10
lda_range = [200,400,600,800,1000]
#MH-[ ]-LR-50, MH-[ ]-HMM-50, MH-[ ]-LDA-100
mh_range = [200,400,600,800,1000]
#SMC-[ ]-LR-50, SMC-[ ]-HMM-50, SMC-[ ]-LDA-100
smc_range = [200,400,600,800,1000]
#PMMH-50-[ ]-LR-50, PMMH-50-[ ]-HMM-50, PMMH-50-[ ]-LDA-100
pmmh_range = [20,40,60,80,100]
#BBVI-[ ]-10-LR-50, BBVI-[ ]-10-HMM-50, BBVI-[ ]-10-LDA-100
bbvi_range = [200,400,600,800,1000]


# linRegrPrior :: MonadDistribution m => m LinRegrParams
# linRegrPrior = do
#   m <- normal 0 3
#   c <- normal 0 5
#   σ <- uniform 1 3
#   return (LinRegrParams m c σ)

# linRegr :: MonadMeasure m => [(Double, Double)] -> LinRegrParams -> m LinRegrParams
# linRegr xys (LinRegrParams m c σ ) = do
#   LinRegrParams m c σ <- linRegrPrior
#   mapM_ (\(x, y_obs) -> score (normalPdf (m * x + c) σ y_obs)) xys
#   return (LinRegrParams m c σ)

def mklinRegrData(n_datapoints):
  xs = range(0, n_datapoints)
  ys = [x * 3 for x in xs]
  return (zip(xs, ys))

def linRegr(xys):
  m = pyro.sample('m', pyro.distributions.Normal(0, 3))
  c = pyro.sample('c', pyro.distributions.Normal(0, 5))
  σ = pyro.sample('σ', pyro.distributions.Uniform(1, 3))
  for i, (x, y) in enumerate(xys):
    pyro.sample('obs_{}'.format(i), pyro.distributions.Normal(m * x + c, σ), obs=y)
  return m

def bench_LR():


def main():
  linRegr()

if __name__ == "__main__":
  main()
