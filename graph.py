# importing the required module
import sys
import matplotlib.pyplot as plt
import ast
import pandas as pd
from sklearn import linear_model
from sklearn import datasets
from scipy.special import expit
import numpy as np

# Remove consecutive duplicates
def removeDuplicates(xs):
  return [v for i, v in enumerate(xs) if i == 0 or v != xs[i-1]]

def main():
  arg  = sys.argv[1]
  f    = open("model-output.txt", "r")
  data = ast.literal_eval(f.read())
  if arg == "lin-regr-basic":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    # x axis values
    xs = xys[0]
    # y axis values
    ys = xys[1]
    plt.scatter(xs, ys)
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('Linear regression')
    plt.show()
  if arg == "lin-regr-lw-sim":
    xys        = [ d[0] for d in data]
    # probabilities
    ps         = [ d[2] for d in data]
    # x axis values
    xs = [x[0] for x in xys]
    # y axis values
    ys = [y[1] for y in xys]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=ps, cmap='gray')
    axs1.set_title('Linear regression - Likelihood Weighting')
    plt.show()
  if arg == "lin-regr-lw-inf":
    xys        = [ d[0] for d in data]
    # sample maps
    sampleMaps = [ d[1] for d in data]
    # probabilities
    ps         = [ d[2] for d in data]
    # x axis values
    xs = [x[0] for x in xys]
    # y axis values
    ys = [y[1] for y in xys]

    mu_samples    = [ d[0][1] for d in sampleMaps ]
    c_samples     = [ d[1][1] for d in sampleMaps ]
    std_samples   = [ d[2][1] for d in sampleMaps ]

    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=ps, cmap='gray')
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
    # x axis values
    xs = xys[0]
    # y axis values
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
    axs1.scatter(xs, ys, c=ps, cmap='gray')
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
    axs1.scatter(xs, ys, c=ps, cmap='gray')
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
    xys         = [ d[0] for d in data]
    sampleMaps  = [ d[1] for d in data]
    logpMaps    = [ d[2] for d in data]
    print(sampleMaps)
  if arg == "nn-basic":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]

    xs = np.array([ x for x in xys[0] ])
    ys = np.array([ y for y in xys[1] ])

    plt.scatter(xs, ys)
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('Neural network')
    plt.show()
  if arg == "nn-lw":
    xys        = [ d[0] for d in data]
    sampleMaps = [ d[1] for d in data]
    ps         = [ d[2] for d in data]
    xs = [x[0] for x in xys]
    ys = [y[1] for y in xys]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=ps, cmap='gray')
    axs1.set_title('Bayesian neural network')
    plt.show()
  if arg == "sin-mh":
    xys        = [ d[0] for d in data]
    sampleMaps = [ d[1] for d in data]
    logpMaps   = [ d[2] for d in data]
    xs = [x[0] for x in xys]
    ys = [y[1] for y in xys]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, cmap='gray')
    axs1.set_title('Sin')
    plt.show()
if __name__ == "__main__":
  main()