# importing the required module
import sys
import matplotlib.pyplot as plt
import ast
import pandas as pd
from sklearn import linear_model
from sklearn import datasets
from sklearn.datasets import make_moons
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import scale
from scipy.special import expit
import numpy as np
from functools import reduce
import itertools
from scipy.stats import gaussian_kde
from scipy.interpolate import make_interp_spline
import pymc3 as pm


# Remove consecutive duplicates
def removeDuplicates(xs):
  return [v for i, v in enumerate(xs) if i == 0 or v != xs[i-1]]

def main():
  arg  = sys.argv[1]
  f    = open("model-output.txt", "r")

  data = ast.literal_eval(f.read().replace('-Infinity', '-2e308')) #
  color_map = plt.cm.get_cmap('Blues')

  if arg == "sir-basic":
    # we expect data to be in the format of (sir-values :: [(Int, Int, Int)], infected count :: [Int])

    # y axis
    sir_values   = np.array(data[0])
    obs_infected = np.array(data[1])
    sus            = np.array([sir[0] for sir in sir_values])
    inf            = np.array([sir[1] for sir in sir_values])
    recov          = np.array([sir[2] for sir in sir_values])
    # x axis
    timeSteps      = np.array([ t for t in range(len(sus))])
    # interpolate data
    X_ = np.linspace(timeSteps.min(), timeSteps.max(), 300)
    X_S_Spline = make_interp_spline(timeSteps.ravel(), sus.ravel())
    X_I_Spline = make_interp_spline(timeSteps.ravel(), inf.ravel())
    X_R_Spline = make_interp_spline(timeSteps.ravel(), recov.ravel())
    X_InfCount_Spline = make_interp_spline(timeSteps.ravel(), obs_infected.ravel())
    S_ = X_S_Spline(X_)
    I_ = X_I_Spline(X_)
    R_ = X_R_Spline(X_)
    IC_ = X_InfCount_Spline(X_)

    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("time steps")
    axs1.set_ylabel("population")
    axs1.plot(X_, S_, color='blue', label='Actual Susceptible')
    axs1.plot(X_, I_, color='red', label='Actual Infected')
    axs1.plot(X_, R_, color='green', label='Actual Recovered')
    axs1.plot(X_, IC_, color='black', label='Recorded Infected')
    axs1.set_title('SIR model - Basic Simulation')
    plt.legend()
    plt.show()
if __name__ == "__main__":
  main()

# 0.161703431 0.0808751181 0.0365742808 0.720847170
# 0.232963296 0.127890173 0.469281425 0.169865107