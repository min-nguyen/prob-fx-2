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

# Remove consecutive duplicates
def removeDuplicates(xs):
  return [v for i, v in enumerate(xs) if i == 0 or v != xs[i-1]]

def main():
  arg  = sys.argv[1]
  f    = open("model-output.txt", "r")
  # l = f.read().replace('-inf', '-2e308')
  # print(l)
  data = ast.literal_eval(f.read().replace('-Infinity', '-2e308'))
  color_map = plt.cm.get_cmap('Blues')
  # X, Y = make_moons(noise=0.2, random_state=0, n_samples=1000)
  # X = scale(X)
  # X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.5)
  # X_test_tuple = [ tuple(x_test) for x_test in X_test]
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

    mu_samples    = [ d[0][1] for d in sampleMaps ]
    c_samples     = [ d[1][1] for d in sampleMaps ]
    std_samples   = [ d[2][1] for d in sampleMaps ]

    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=ps, cmap=color_map)
    axs1.set_title('Linear regression - Likelihood Weighting')

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
    mu_samples  = [ d[0][1] for d in sampleMaps ]
    mu_samples_unique = removeDuplicates(mu_samples)
    c_samples   = [ d[1][1] for d in sampleMaps ]
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
    sampleMaps = [ d[1] for d in data]
    ps         = [ d[2] for d in data]
    xs = [x[0] for x in xys]
    ys = [y[1] for y in xys]
    mu_samples    = [ d[0][1] for d in sampleMaps ]
    b_samples     = [ d[1][1] for d in sampleMaps ]
    print(ps)
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
    mu_samples    = [ d[0][1] for d in sampleMaps ]
    b_samples     = [ d[1][1] for d in sampleMaps ]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=ps, cmap=color_map)
    axs1.set_title('Logistic regression - likelihood weighting')
    axs1.text(-2.5, 1.15, 'How likely the randomly sampled parameters of the current iteration gives rise to the data point')
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
    mu_samples  = [ d[0][1] for d in sampleMaps ]
    mu_samples_unique = removeDuplicates(mu_samples)
    b_samples   = [ d[1][1] for d in sampleMaps ]
    b_samples_unique = removeDuplicates(b_samples)
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
    print(sampleMaps)
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
    sampleMaps = [ d[1] for d in data]
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
    xs = [x[0] for x in xys]
    ys = [y[1] for y in xys]
    mu_samples    = [ d[0][1] for d in sampleMaps ]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=ps, cmap=color_map)
    axs1.set_title('Sine model - Likelihood Weighting')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel('mu value')
    axs2.set_ylabel('probability')
    axs2.scatter(mu_samples, ps)
    axs2.set_title('Sine model - likelihood weighting')
    plt.show()
    plt.show()
  if arg == "sin-mh-post":
    xys         = [ d[0] for d in data]
    sampleMaps  = [ d[1] for d in data]
    logpMaps    = [ d[2] for d in data]
    mu_samples  = [ d[0][1] for d in sampleMaps ]
    mu_samples_unique = removeDuplicates(mu_samples)
    c_samples   = [ d[1][1] for d in sampleMaps ]
    c_samples_unique = removeDuplicates(c_samples)
    sigma_samples  = [ d[2][1] for d in sampleMaps ]
    sigma_samples_unique = removeDuplicates(sigma_samples)
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu values")
    axs1.set_ylabel("frequency")
    axs1.hist(mu_samples_unique, bins=50)
    axs1.set_title('Sine model - Metropolis Hastings Posterior')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("b values")
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
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs.ravel(), ys.ravel(), cmap=color_map)
    axs1.set_title('HMM - Simulation')
    plt.show()
  if arg == "hmm-lw-sim":
    xys        = [ d[0] for d in data]
    ps         = [ d[2] for d in data]
    xs = [xs[0][1:] for xs in xys]
    ys = [ys[1] for ys in xys]
    ps    = [ [p for i in range(len(ys))] for p in ps ]
    print(ps)

    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=ps, cmap=color_map)
    axs1.set_title('HMM - Likelihood Weighting')
    plt.show()
  if arg == "hmm-lw-inf":

    xys        = [ d[0] for d in data]
    sampleMaps = [ d[1] for d in data]
    ps         = [ d[2] for d in data]
    xs = [xs[0][1:] for xs in xys]
    ys = [ys[1] for ys in xys]
    trans_ps = [ s[0][1] for s in sampleMaps]
    obs_ps   = [ s[1][1] for s in sampleMaps]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("trans")
    axs1.set_ylabel("p")
    axs1.scatter(trans_ps, ps, cmap=color_map)
    axs1.set_title('HMM - Likelihood Weighting (Trans p)')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("obs")
    axs2.set_ylabel("p")
    axs2.scatter(obs_ps, ps, cmap=color_map)
    axs2.set_title('HMM - Likelihood Weighting (Obs p)')
    plt.show()
  if arg == "hmm-mh-post":
    xys         = [ d[0] for d in data]
    sampleMaps  = [ d[1] for d in data]
    logpMaps    = [ d[2] for d in data]
    trans_ps  = [ d[0][1] for d in sampleMaps ]
    trans_ps_unique = removeDuplicates(trans_ps)
    obs_ps   = [ d[1][1] for d in sampleMaps ]
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
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=z, cmap=color_map)
    axs1.set_title('HMM - Metropolis Hastings Predictive')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("x axis")
    axs2.set_ylabel("y axis")
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
    rhos          = [s[0][1] for s in sampleMaps]
    betas         = [s[1][1] for s in sampleMaps]
    gammas        = [s[2][1] for s in sampleMaps]
    print(sampleMaps)
    print(ps)
    print(rhos)
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
    ps            = [d[2] for d in data]
    rhos          = [s[0][1] for s in sampleMaps]
    betas         = [s[1][1] for s in sampleMaps]
    gammas        = [s[2][1] for s in sampleMaps]
    print(rhos)
if __name__ == "__main__":
  main()