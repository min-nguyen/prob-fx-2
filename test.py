import arviz as az
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pymc3 as pm
import theano
import theano.tensor as tt


az.style.use("arviz-darkgrid")
data = pd.read_csv(pm.get_data("radon.csv"))
data["log_radon"] = data["log_radon"].astype(theano.config.floatX)
county_names = data.county.unique()
county_idx = data.county_code.values

n_counties = len(data.county.unique())

print(data[["county", "log_radon", "floor"]].head())
print(n_counties)
print(len(county_idx))
print(len(data.floor.values))
with pm.Model() as unpooled_model:

    # Independent parameters for each county
    a = pm.Normal("a", 0, sigma=100, shape=n_counties)
    b = pm.Normal("b", 0, sigma=100, shape=n_counties)

    # Model error
    eps = pm.HalfCauchy("eps", 5)

    # Model prediction of radon level
    # a[county_idx] translates to a[0, 0, 0, 1, 1, ...],
    # we thus link multiple household measures of a county
    # to its coefficients.
    radon_est = a[county_idx] + b[county_idx] * data.floor.values

    radon_es = tt.printing.Print("radon_es")(b)
    # Data likelihood
    y = pm.Normal("y", radon_est, sigma=eps, observed=data.log_radon)

with unpooled_model:
    unpooled_trace = pm.sample(100)