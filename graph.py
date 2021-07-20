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
  # l = f.read().replace('-inf', '-2e308')
  # print(l)
  data = ast.literal_eval(f.read().replace('-Infinity', '-2e308')) #
  color_map = plt.cm.get_cmap('Blues')

  # data = pd.read_csv(pm.get_data("radon.csv"))
  # data_log_radon = data["log_radon"]
  # county_names = data.county.unique()
  # county_idx = data.county_code.values

  # n_counties = len(data.county.unique())
  # a = np.array([i for i in range(86)])
  # b = a[2,4,4]
  # print(data[["county", "log_radon", "floor"]])
  # print(data)
  # print(data.floor.values)
  if arg == "lin-regr-basic":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    xs = xys[0]
    ys = xys[1]
    plt.scatter(xs, ys)
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('Linear regression')
    plt.show()
  if arg == "lin-regr-lw-sim":
    xys        = [ d[0] for d in data]
    ps         = [ d[2] for d in data]
    xs = [x[0] for x in xys]
    ys = [y[1] for y in xys]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=ps, cmap=color_map)
    axs1.set_title('Linear regression - Likelihood Weighting')
    plt.show()
  if arg == "lin-regr-lw-inf":
    xys        = [ d[0] for d in data]
    sampleMaps = [ d[1] for d in data]
    ps         = [ d[2] for d in data]
    xs = [x[0] for x in xys]
    ys = [y[1] for y in xys]
    mu_samples  = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'm'] for d in sampleMaps ]).ravel()
    mu_samples_unique = removeDuplicates(mu_samples)
    c_samples  = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'c'] for d in sampleMaps ]).ravel()
    c_samples_unique = removeDuplicates(c_samples)
    std_samples   = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'σ'] for d in sampleMaps ]).ravel()
    std_samples_unique = removeDuplicates(std_samples)
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel('mu value')
    axs2.set_ylabel('probability')
    axs2.scatter(mu_samples, ps)
    axs2.set_title('Linear regression - Likelihood Weighting')
    plt.show()
  if arg == "lin-regr-mh-post":
    xys         = [ d[0] for d in data]
    sampleMaps  = [ d[1] for d in data]
    logpMaps    = [ d[2] for d in data]
    mu_samples  = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'm'] for d in sampleMaps ]).ravel()
    mu_samples_unique = removeDuplicates(mu_samples)
    c_samples  = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'c'] for d in sampleMaps ]).ravel()
    c_samples_unique = removeDuplicates(c_samples)
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu values")
    axs1.set_ylabel("frequency")
    axs1.hist(mu_samples_unique, bins=50)
    axs1.set_title('Linear regression - Metropolis Hastings Posterior')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("c values")
    axs2.set_ylabel("frequency")
    axs2.hist(c_samples_unique, bins=50)
    axs2.set_title('Linear regression - Metropolis Hastings Posterior')
    plt.show()
  if arg == "lin-regr-mh-pred":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    xs = xys[0]
    ys = xys[1]
    plt.scatter(xs, ys)
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('Linear regression')
    plt.show()
  if arg == "log-regr-test":
    iris = datasets.load_iris()
    # Get first 100 x data points. iris.data consists of a list of 4-dimensional data points. We will only use the 1st dimension for x data points.
    iris_data = (iris.data[0:100])[:,0]
    # Get first 100 y data points
    iris_target = iris.target[0:100]
    # Create model for logistic regression
    model = linear_model.LogisticRegression(C=1e5, solver='lbfgs')
    # Make xs into a list of lists and fit the data
    model.fit(iris_data.reshape(-1,1), iris_target)
    # predict dummy y_test data based on the logistic model
    x_test = np.linspace(4.0,7.0,num=100)
    y_test = x_test * model.coef_ + model.intercept_
    # take the sigmoid of y_test
    sigmoid = expit(y_test)
    plt.scatter(iris_data, iris_target)
    plt.plot(x_test,sigmoid.ravel(),c="green", label = "logistic fit")
    plt.xlabel("Sepal Length")
    plt.ylabel("Sepal Width")
    plt.show()
  if arg == "log-regr-basic":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    xs = np.array([ [x] for x in xys[0] ])
    ys = np.array([ y for y in xys[1] ])
    model = linear_model.LogisticRegression(C=1e5, solver='lbfgs')
    model.fit(xs.reshape(-1,1), ys)
    x_test = np.linspace(-2.0,2.0,num=100)
    y_test = x_test * model.coef_ + model.intercept_
    sigmoid = expit(y_test)

    plt.scatter(xs, ys)
    plt.plot(x_test, sigmoid.ravel(),c="green", label = "logistic fit")
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('Logistic regression - basic simulation')
    plt.show()
  if arg == "log-regr-lw-sim":
    xys        = [ d[0] for d in data]
    ps         = [ d[2] for d in data]
    xs = [x[0] for x in xys]
    ys = [y[1] for y in xys]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=ps, cmap=color_map)
    axs1.set_title('Logistic regression - likelihood weighting')
    plt.show()
  if arg == "log-regr-lw-inf":
    xys        = [ d[0] for d in data]
    sampleMaps = [ d[1] for d in data]
    ps         = [ d[2] for d in data]
    xs = [x[0] for x in xys]
    ys = [y[1] for y in xys]
    mu_samples  = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'm'] for d in sampleMaps ]).ravel()
    b_samples   = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'b'] for d in sampleMaps ]).ravel()
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("b value")
    axs1.set_ylabel("probability")
    axs1.scatter(b_samples, ps)
    axs1.set_title('Logistic regression - likelihood weighting')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel('mu value')
    axs2.set_ylabel('probability')
    axs2.scatter(mu_samples, ps)
    axs2.set_title('Logistic regression - likelihood weighting')
    plt.show()
  if arg == "log-regr-mh-post":
    xys         = [ d[0] for d in data]
    sampleMaps  = [ d[1] for d in data]
    logpMaps    = [ d[2] for d in data]
    # print(sampleMaps)
    mu_samples  = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'm'] for d in sampleMaps ]).ravel()
    mu_samples_unique = removeDuplicates(mu_samples)
    b_samples   = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'b'] for d in sampleMaps ]).ravel()
    b_samples_unique = removeDuplicates(b_samples)
    # print(b_samples_unique)
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu values")
    axs1.set_ylabel("frequency")
    axs1.hist(mu_samples_unique, bins=50)
    axs1.set_title('Logistic regression - Metropolis Hastings Posterior')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("b values")
    axs2.set_ylabel("frequency")
    axs2.hist(b_samples_unique, bins=50)
    axs2.set_title('Logistic regression - Metropolis Hastings Posterior')
    plt.show()
  if arg == "log-regr-mh-pred":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    xs = xys[0]
    ys = xys[1]
    plt.scatter(xs, ys)
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('Logistic regression - Metropolis Hastings Predictive')
    plt.show()
  if arg == "nn-lin-basic":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    xs = np.array([ x for x in xys[0] ])
    ys = np.array([ y for y in xys[1] ])
    plt.scatter(xs, ys)
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('Neural network')
    plt.show()
  if arg == "nn-lin-lw-sim":
    xys        = [ d[0] for d in data]
    ps         = [ d[2] for d in data]
    xs = [x[0] for x in xys]
    ys = [y[1] for y in xys]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=ps, cmap=color_map)
    axs1.set_title('Bayesian neural network')
    plt.show()
  if arg == "nn-lin-mh-pred":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    xs = xys[0]
    ys = xys[1]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, cmap=color_map)
    axs1.set_title('Bayesian neural network - Metropolis Hastings Predictive')
    plt.show()
  if arg == "nn-step-basic":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    xs = np.array([ x for x in xys[0] ])
    ys = np.array([ y for y in xys[1] ])
    plt.scatter(xs, ys)
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('Neural network')
    plt.show()
  if arg == "nn-step-lw-sim":
    xys        = [ d[0] for d in data]
    sampleMaps = [ d[1] for d in data]
    ps         = [ d[2] for d in data]
    xs = [x[0] for x in xys]
    ys = [y[1] for y in xys]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=ps, cmap=color_map)
    axs1.set_title('Bayesian (step) neural network - Likelihood Weighting Simulation')
    plt.show()
  if arg == "nn-step-mh-pred":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    xs = xys[0]
    ys = xys[1]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, cmap=color_map)
    axs1.set_title('Bayesian neural network - Metropolis Hastings Predictive')
    plt.show()
  if arg == "nn-log-basic":
    xyls =  [[ i for i, j in data ],
             [ j for i, j in data ]]

    xs = [ xys[0] for xys in xyls[0]]
    ys = [ xys[1] for xys in xyls[0]]
    ls = [ ls     for ls in xyls[1]]

    print(xs, ys, ls)
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=ls)
    axs1.set_title('Neural network logistic regression - Simulation')
    plt.show()
  if arg == "nn-log-mh-pred":
    xyls =  [[ i for i, j in data ],
             [ j for i, j in data ]]
    xs = [ xys[0] for xys in xyls[0]]
    ys = [ xys[1] for xys in xyls[0]]
    ls = [ ls     for ls in xyls[1]]
    print(xs, ys, ls)
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=ls)
    axs1.set_title('Neural network logistic regression - Metropolis Hastings Predictive')
    plt.show()
  if arg == "sin-basic":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    xs = xys[0]
    ys = xys[1]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, cmap=color_map)
    axs1.set_title('Sine model - Simulation')
    plt.show()
  if arg == "sin-lw-sim":
    xys        = [ d[0] for d in data]
    ps         = [ d[2] for d in data]
    xs = [x[0] for x in xys]
    ys = [y[1] for y in xys]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=ps, cmap=color_map)
    axs1.set_title('Sine model - Likelihood Weighting')
    plt.show()
  if arg == "sin-lw-inf":
    xys        = [ d[0] for d in data]
    sampleMaps = [ d[1] for d in data]
    ps         = [ d[2] for d in data]
    mu_samples    = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'm'] for d in sampleMaps ]).ravel()
    c_samples     = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'c'] for d in sampleMaps ]).ravel()
    sigma_samples = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'σ'] for d in sampleMaps ]).ravel()
    xs = [x[0] for x in xys]
    ys = [y[1] for y in xys]
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel('mu value')
    axs2.set_ylabel('probability')
    axs2.scatter(mu_samples, ps)
    axs2.set_title('Sine model - likelihood weighting')
    plt.show()
  if arg == "sin-mh-post":
    xys         = [ d[0] for d in data]
    sampleMaps  = [ d[1] for d in data]
    logpMaps    = [ d[2] for d in data]
    mu_samples  = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'm'] for d in sampleMaps ]).ravel()
    mu_samples_unique = removeDuplicates(mu_samples)
    c_samples  = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'c'] for d in sampleMaps ]).ravel()
    c_samples_unique = removeDuplicates(c_samples)
    print(sampleMaps)
    print(mu_samples_unique)
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu values")
    axs1.set_ylabel("frequency")
    axs1.hist(mu_samples_unique, bins=50)
    axs1.set_title('Sine model - Metropolis Hastings Posterior')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("c values")
    axs2.set_ylabel("frequency")
    axs2.hist(c_samples_unique, bins=50)
    axs2.set_title('Sine model - Metropolis Hastings Posterior')
    plt.show()
  if arg == "sin-mh-pred":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    xs = xys[0]
    ys = xys[1]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, cmap=color_map)
    axs1.set_title('Sine model - Metropolis Hastings Predictive')
    plt.show()
  if arg == "hmm-basic":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    xs = np.array([xs[1:] for xs in xys[0]])
    ys = np.array(xys[1])
    print(xs.ravel(), ys.ravel())
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("trans state")
    axs1.set_ylabel("obs state")
    axs1.scatter(xs.ravel(), ys.ravel(), cmap=color_map)
    axs1.set_title('HMM - Simulation')
    plt.show()
  if arg == "hmm-lw-sim":
    # Note: not a very useful simulation
    xys        = [ d[0] for d in data]
    ps         = [ d[2] for d in data]
    xs = [xs[0][1:] for xs in xys]
    ys = [ys[1] for ys in xys]

    ps    = [ [p for i in range(len(ys[0]))] for p in ps ]
    print(xs)
    print(ys)
    print(ps)

    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("trans state")
    axs1.set_ylabel("obs state")
    axs1.scatter(xs, ys, c=ps, cmap=color_map)
    axs1.set_title('HMM - Likelihood Weighting')
    plt.show()
  if arg == "hmm-lw-inf":
    # Note : this inference method knowingly doesn't work well for this model
    xys        = [ d[0] for d in data]
    sampleMaps = [ d[1] for d in data]
    ps         = [ d[2] for d in data]
    xs = [xs[0][1:] for xs in xys]
    ys = [ys[1] for ys in xys]
    trans_ps  = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'trans_p'] for d in sampleMaps ]).ravel()
    obs_ps  = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'obs_p'] for d in sampleMaps ]).ravel()
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("trans_p")
    axs1.set_ylabel("p")
    axs1.scatter(trans_ps, ps, cmap=color_map)
    axs1.set_title('HMM - Likelihood Weighting (Trans p)')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("obs_p")
    axs2.set_ylabel("p")
    axs2.scatter(obs_ps, ps, cmap=color_map)
    axs2.set_title('HMM - Likelihood Weighting (Obs p)')
    plt.show()
  if arg == "hmm-mh-post":
    # Note : this works less well for certain parameters of trans_p and obs_p used for the training data
    xys         = [ d[0] for d in data]
    sampleMaps  = [ d[1] for d in data]
    logpMaps    = [ d[2] for d in data]
    print(sampleMaps)
    trans_ps  = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'trans_p'] for d in sampleMaps ]).ravel()
    trans_ps_unique = removeDuplicates(trans_ps)
    obs_ps  = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'obs_p'] for d in sampleMaps ]).ravel()
    obs_ps_unique = removeDuplicates(obs_ps)
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("trans_ps values")
    axs1.set_ylabel("frequency")
    axs1.hist(trans_ps_unique, bins=50)
    axs1.set_title('HMM - Metropolis Hastings Posterior (Trans P)')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("obs_ps values")
    axs2.set_ylabel("frequency")
    axs2.hist(obs_ps_unique, bins=50)
    axs2.set_title('HMM - Metropolis Hastings Posterior (Obs P)')
    plt.show()
  if arg == "hmm-mh-pred":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    xs = np.array([xs[1:] for xs in xys[0]])
    ys = np.array(xys[1])
    xy = np.vstack([xs.ravel(),ys.ravel()])
    z = gaussian_kde(xy)(xy)
    print(xs, ys)
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("trans state")
    axs1.set_ylabel("obs state")
    axs1.scatter(xs, ys, c=z, cmap=color_map)
    axs1.set_title('HMM - Metropolis Hastings Predictive')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("trans state")
    axs2.set_ylabel("obs state")
    plt.hist2d(xs.ravel(), ys.ravel(), (10, 10), cmap=plt.cm.jet)
    axs2.set_title('HMM - Metropolis Hastings Predictive')
    plt.colorbar()
    plt.show()
  if arg == "sir-basic":
    # y axis
    latentStates   = np.array(data[0][1:])
    infectedCount  = np.array(data[1])
    sus            = np.array([sir[0] for sir in latentStates])
    inf            = np.array([sir[1] for sir in latentStates])
    recov          = np.array([sir[2] for sir in latentStates])
    # x axis
    timeSteps      = np.array([ t for t in range(len(sus))])
    # interpolate data
    X_ = np.linspace(timeSteps.min(), timeSteps.max(), 300)
    X_S_Spline = make_interp_spline(timeSteps.ravel(), sus.ravel())
    X_I_Spline = make_interp_spline(timeSteps.ravel(), inf.ravel())
    X_R_Spline = make_interp_spline(timeSteps.ravel(), recov.ravel())
    X_InfCount_Spline = make_interp_spline(timeSteps.ravel(), infectedCount.ravel())
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
  if arg == "sir-lw-inf":
    latentStates  = [d[0][0] for d in data]
    infectedCount = [d[0][1] for d in data]
    sampleMaps    = [d[1] for d in data]
    ps            = [d[2] for d in data]
    rhos          = np.array([ [ d1[1] for d1 in d if d1[0][0] == '\961'] for d in sampleMaps ]).ravel()
    betas         = np.array([ [ d1[1] for d1 in d if d1[0][0] == '\946'] for d in sampleMaps ]).ravel()
    gammas        = np.array([ [ d1[1] for d1 in d if d1[0][0] == '\947'] for d in sampleMaps ]).ravel()
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("rho")
    axs1.set_ylabel("likelihood")
    axs1.scatter(rhos, ps)
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("beta")
    axs2.set_ylabel("likelihood")
    axs2.scatter(betas, ps)
    fig3, axs3 = plt.subplots(nrows=1)
    axs3.set_xlabel("gamma")
    axs3.set_ylabel("likelihood")
    axs3.scatter(gammas, ps)
    plt.show()
  if arg == "sir-mh-post":
    latentStates  = [d[0][0] for d in data]
    infectedCount = [d[0][1] for d in data]
    sampleMaps    = [d[1] for d in data]
    rhos          = np.array([ [ d1[1] for d1 in d if d1[0][0] == '\961'] for d in sampleMaps ]).ravel()
    rhos_unique = removeDuplicates(rhos)
    betas         = np.array([ [ d1[1] for d1 in d if d1[0][0] == '\946'] for d in sampleMaps ]).ravel()
    betas_unique = removeDuplicates(betas)
    gammas        = np.array([ [ d1[1] for d1 in d if d1[0][0] == '\947'] for d in sampleMaps ]).ravel()
    gammas_unique = removeDuplicates(gammas)
    print(rhos_unique)
    print(betas_unique)
    print(gammas_unique)
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("rho values")
    axs1.set_ylabel("frequency")
    axs1.hist(rhos_unique, bins=50)
    axs1.set_title('SIR - Metropolis Hastings Posterior (Rho)')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("beta values")
    axs2.set_ylabel("frequency")
    axs2.hist(betas_unique, bins=50)
    axs2.set_title('HMM - Metropolis Hastings Posterior (Beta)')
    fig3, axs3 = plt.subplots(nrows=1)
    axs3.set_xlabel("gamma values")
    axs3.set_ylabel("frequency")
    axs3.hist(gammas_unique, bins=50)
    axs3.set_title('HMM - Metropolis Hastings Posterior (Gamma)')
    plt.show()
    # print(rhos_unique)
    # print(betas_unique)
    # print(gammas_unique)
  if arg == "sir-mh-pred":
    # y axis
    latentStates   = np.array(data[0][1:])
    infectedCount  = np.array(data[1])
    sus            = np.array([sir[0] for sir in latentStates])
    inf            = np.array([sir[1] for sir in latentStates])
    recov          = np.array([sir[2] for sir in latentStates])
    # x axis
    timeSteps      = np.array([ t for t in range(len(sus))])
    # interpolate data
    X_ = np.linspace(timeSteps.min(), timeSteps.max(), 300)
    X_S_Spline = make_interp_spline(timeSteps.ravel(), sus.ravel())
    X_I_Spline = make_interp_spline(timeSteps.ravel(), inf.ravel())
    X_R_Spline = make_interp_spline(timeSteps.ravel(), recov.ravel())
    X_InfCount_Spline = make_interp_spline(timeSteps.ravel(), infectedCount.ravel())
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
  if arg == "topic-basic":
    words = list(np.array(data).ravel())
    fig, ax = plt.subplots(nrows=1)
    ws = list(set(words))
    freqs = [ words.count(w)  for w in ws]
    ax.bar(ws, freqs)
    ax.set_xticklabels(ws)
    plt.show()
  if arg == "topic-mh-post":
    words       = list(np.array([ d[0] for d in data]).ravel())
    ws          = ['DNA', 'evolution', 'parsing', 'phonology']
    sampleMaps  = [ d[1] for d in data]
    topic_ps    = np.array([ [ d1[1] for d1 in d if d1[0] == ('topic_p', 0)] for d in sampleMaps ])
    topic_0s    = np.array([ [ d1[1] for d1 in d if d1[0] == ('word_p', 0)] for d in sampleMaps ])
    topic_1s    = np.array([ [ d1[1] for d1 in d if d1[0] == ('word_p', 1)] for d in sampleMaps ])
    topic_p = topic_ps[-1].ravel()
    topic_0 = topic_0s[-1].ravel()
    topic_1 = topic_1s[-1].ravel()
    fig, ax = plt.subplots(nrows=1)
    ax.bar(['Topic 0', 'Topic 1'], topic_p, 0.4)
    ax.set_xticklabels(['Topic 0', 'Topic 1'])
    plt.title('Document - Topic distribution')
    fig0, ax0 = plt.subplots(nrows=1)
    ax0.bar(ws, topic_0, 0.4)
    ax0.set_xticklabels(ws)
    plt.title('Topic 0 - Word distribution')
    fig1, ax1 = plt.subplots(nrows=1)
    ax1.bar(ws, topic_1, 0.4)
    ax1.set_xticklabels(ws)
    plt.title('Topic 1 - Word distribution')
    plt.show()
  if arg == "topics-mh-post":
    words       = list(np.array([ d[0] for d in data]).ravel())
    ws          = ['DNA', 'evolution', 'parsing', 'phonology']
    sampleMaps  = [ d[1] for d in data]
    # Document 0
    d0_topic_ps    = np.array([ [ d1[1] for d1 in d if d1[0] == ('topic_p', 0)] for d in sampleMaps ])
    d0_topic_0s    = np.array([ [ d1[1] for d1 in d if d1[0] == ('word_p', 0)]  for d in sampleMaps ])
    d0_topic_1s    = np.array([ [ d1[1] for d1 in d if d1[0] == ('word_p', 1)]  for d in sampleMaps ])
    d0_topic_p = d0_topic_ps[-1].ravel()
    d0_topic_0 = d0_topic_0s[-1].ravel()
    d0_topic_1 = d0_topic_1s[-1].ravel()
    # Document 1
    d1_topic_ps    = np.array([ [ d1[1] for d1 in d if d1[0] == ('topic_p', 1)] for d in sampleMaps ])
    d1_topic_0s    = np.array([ [ d1[1] for d1 in d if d1[0] == ('word_p', 2)]  for d in sampleMaps ])
    d1_topic_1s    = np.array([ [ d1[1] for d1 in d if d1[0] == ('word_p', 3)]  for d in sampleMaps ])
    d1_topic_p = d1_topic_ps[-1].ravel()
    d1_topic_0 = d1_topic_0s[-1].ravel()
    d1_topic_1 = d1_topic_1s[-1].ravel()
    # Plot document 0
    fig, ax = plt.subplots(nrows=1)
    ax.bar(['Topic 0', 'Topic 1'], d0_topic_p, 0.4)
    ax.set_xticklabels(['Topic 0', 'Topic 1'])
    plt.title('Document 0 - Topic distribution')
    fig0, ax0 = plt.subplots(nrows=1)
    ax0.bar(ws, d0_topic_0, 0.4)
    ax0.set_xticklabels(ws)
    plt.title('Document 0 - Topic 0 - Word distribution')
    fig1, ax1 = plt.subplots(nrows=1)
    ax1.bar(ws, d0_topic_1, 0.4)
    ax1.set_xticklabels(ws)
    plt.title('Document 0 - Topic 1 - Word distribution')
    # Plot document 1
    fig2, ax2 = plt.subplots(nrows=1)
    ax2.bar(['Topic 0', 'Topic 1'], d1_topic_p, 0.4)
    ax2.set_xticklabels(['Topic 0', 'Topic 1'])
    plt.title('Document 1 - Topic distribution')
    fig3, ax3 = plt.subplots(nrows=1)
    ax3.bar(ws, d1_topic_0, 0.4)
    ax3.set_xticklabels(ws)
    plt.title('Document 1 - Topic 0 - Word distribution')
    fig4, ax4 = plt.subplots(nrows=1)
    ax4.bar(ws, d1_topic_1, 0.4)
    ax4.set_xticklabels(ws)
    plt.title('Document 1 - Topic 1 - Word distribution')
    plt.show()
  if arg == "hlr-basic":
    basement_ys   = data[0]
    basement_xs   = [0 for i in range(len(basement_ys))]
    nobasement_ys = data[1]
    nobasement_xs = [1 for i in range(len(nobasement_ys))]
    print(data)
    plt.scatter(basement_xs, basement_ys, color="r")
    plt.scatter(nobasement_xs, nobasement_ys, color='b')
    plt.show()
  # if arg == "hlr-mh-post":
  #   sampleMaps = [d[1] for d in data]
  #   mu_a       = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'mu_a'] for d in sampleMaps ])
  #   mu_b       = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'mu_b'] for d in sampleMaps ])
  #   sigma_a    = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'sigma_a'] for d in sampleMaps ])
  #   sigma_b    = np.array([ [ d1[1] for d1 in d if d1[0][0] == 'sigma_b'] for d in sampleMaps ])
  #   mu_a_unique = removeDuplicates(mu_a)
  #   mu_b_unique = removeDuplicates(mu_b)
  #   sigma_a_unique = removeDuplicates(sigma_a)
  #   sigma_b_unique = removeDuplicates(sigma_b)
  #   print(mu_a_unique)
  #   print(mu_b_unique)
  #   print(sigma_a_unique)
  #   print(sigma_b_unique)
  if arg == "hlr-mh-post":
    mu_a       = data[0][1]
    mu_b       = data[1][1]
    sigma_a    = data[2][1]
    sigma_b    = data[3][1]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu_a values")
    axs1.set_ylabel("frequency")
    axs1.hist(mu_a, bins=50)
    axs1.set_title('HLR - Metropolis Hastings Posterior (mu_a)')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("mu_b values")
    axs2.set_ylabel("frequency")
    axs2.hist(mu_b, bins=50)
    axs2.set_title('HLR - Metropolis Hastings Posterior (mu_b)')
    fig3, axs3 = plt.subplots(nrows=1)
    axs3.set_xlabel("sigma_a values")
    axs3.set_ylabel("frequency")
    axs3.hist(sigma_a, bins=50)
    axs3.set_title('HLR - Metropolis Hastings Posterior (sigma_a)')
    fig4, axs4 = plt.subplots(nrows=1)
    axs4.set_xlabel("sigma_b values")
    axs4.set_ylabel("frequency")
    axs4.hist(sigma_b, bins=50)
    axs4.set_title('HLR - Metropolis Hastings Posterior (sigma_b)')
    plt.show()
  if arg == "hlr-mh-pred":
    sampleMaps = data[1] # Only use last sample
    intercepts = np.array([ d[1] for d in sampleMaps if d[0][0] == 'a' ]).ravel()
    gradients  = np.array([ d[1] for d in sampleMaps if d[0][0] == 'b' ]).ravel()

    plt.xticks([0, 1], ["basement", "no basement"])
    plt.ylabel('Log radon level')

    for (m, c) in zip(gradients, intercepts):
      x = np.linspace(0, 1, 100)
      y = m * x + c
      plt.plot(x, y)
    plt.show()

if __name__ == "__main__":
  main()

# 0.161703431 0.0808751181 0.0365742808 0.720847170
# 0.232963296 0.127890173 0.469281425 0.169865107