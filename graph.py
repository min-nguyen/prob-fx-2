# importing the required module
from re import X
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

  data = ast.literal_eval(f.read().replace('-Infinity', '-2e308')) #
  color_map = plt.cm.get_cmap('Blues')

  if arg in ["simLinRegr", "simLinRegrOnce"]:
    xs = [xy[0] for xy in data]
    ys = [xy[1] for xy in data]
    plt.scatter(xs, ys)
    plt.xlabel('x data points')
    plt.ylabel('y data points')
    plt.title('Linear regression')
    plt.show()
  if arg in ["lwLinRegr", "lwLinRegrOnce"]:
    mus = [d[0] for d in data]
    ps  = [d[1] for d in data]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel('mu value')
    axs1.set_ylabel('probability')
    axs1.scatter(mus, ps)
    axs1.set_title('Linear regression - Likelihood Weighting')
    plt.show()
  if arg in ["mhLinRegr","mhLinRegrOnce","smcLinRegr","rmsmcLinRegr","pmmhLinRegr"]:
    mus = data
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu values", fontsize=12)
    axs1.set_ylabel("frequency")
    axs1.hist(mus, bins=25)
    axs1.set_title('Linear regression - Metropolis Hastings')
    plt.show()
  if arg in ["simSIR", "simSIRS"]:
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
    axs1.set_xlabel("days")
    axs1.set_ylabel("population", fontsize=12)
    axs1.plot(X_, S_, color='blue', label='Actual Susceptible')
    axs1.plot(X_, I_, color='red', label='Actual Infected')
    axs1.plot(X_, R_, color='green', label='Actual Recovered')
    axs1.plot(X_, IC_, color='black', label='Reported Infected')
    axs1.set_title('SIR model - Simulation')
    plt.xlim([0,100])
    plt.ylim([0,800])
    plt.legend()
    plt.show()
  if arg == "simSIRSV":
    # y axis
    sirv_values   = np.array(data[0])
    obs_infected = np.array(data[1])
    sus            = np.array([sirv[0] for sirv in sirv_values])
    inf            = np.array([sirv[1] for sirv in sirv_values])
    recov          = np.array([sirv[2] for sirv in sirv_values])
    vacc          = np.array([sirv[3] for sirv in sirv_values])
    # x axis
    timeSteps      = np.array([ t for t in range(len(sus))])
    # interpolate data
    X_ = np.linspace(timeSteps.min(), timeSteps.max(), 300)
    X_S_Spline = make_interp_spline(timeSteps.ravel(), sus.ravel())
    X_I_Spline = make_interp_spline(timeSteps.ravel(), inf.ravel())
    X_R_Spline = make_interp_spline(timeSteps.ravel(), recov.ravel())
    X_V_Spline = make_interp_spline(timeSteps.ravel(), vacc.ravel())
    X_InfCount_Spline = make_interp_spline(timeSteps.ravel(), obs_infected.ravel())
    S_ = X_S_Spline(X_)
    I_ = X_I_Spline(X_)
    R_ = X_R_Spline(X_)
    V_ = X_V_Spline(X_)
    IC_ = X_InfCount_Spline(X_)

    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("days")
    axs1.set_ylabel("population", fontsize=12)
    axs1.plot(X_, S_, color='blue', label='Actual Susceptible')
    axs1.plot(X_, I_, color='red', label='Actual Infected')
    axs1.plot(X_, R_, color='green', label='Actual Recovered')
    axs1.plot(X_, V_, color='orange', label='Actual Vaccinated')
    axs1.plot(X_, IC_, color='black', label='Reported Infected')
    axs1.set_title('SIRSV model - Simulation')
    plt.xlim([0,100])
    plt.ylim([0,800])
    plt.legend()
    plt.show()
  if arg == "mhSIR":
    print(data)
    rhos_unique   = data[0]
    betas_unique  = data[1]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("ρ values", fontsize=12)
    axs1.set_ylabel("frequency")
    axs1.hist(rhos_unique, bins=25)
    axs1.set_title('SIR - Metropolis Hastings Posterior (Rho)')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("β values", fontsize=12)
    axs2.set_ylabel("frequency")
    axs2.hist(betas_unique, bins=25)
    axs2.set_title('HMM - Metropolis Hastings Posterior (Beta)')
    plt.show()
  if arg == "simLogRegr":
    xys = np.array(data)
    xs =  np.array([xy[0] for xy in xys])
    ys =  np.array([xy[1] for xy in xys])
    model = linear_model.LogisticRegression(C=1e5, solver='lbfgs')
    model.fit(xs.reshape(-1,1), ys)
    x_test = np.linspace(-2.0,2.0,num=100)
    y_test = x_test * model.coef_ + model.intercept_
    sigmoid = expit(y_test)
    plt.yticks([1.0, 0.0], ["True",
                            "False"])
    plt.scatter(xs, ys)
    plt.plot(x_test, sigmoid.ravel(),c="green", label = "logistic fit")
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('Logistic regression simulation')
    plt.show()
  if arg == "lwLogRegr":
    mus = [d[0] for d in data]
    ps  = [d[1] for d in data]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel('mu value')
    axs1.set_ylabel('probability')
    axs1.scatter(mus, ps)
    axs1.set_title('Logistic regression - Likelihood Weighting')
    plt.show()
  if arg == "mhLogRegr":
    mu_samples = [d[0] for d in data]
    b_samples  = [d[1] for d in data]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu values")
    axs1.set_ylabel("frequency")
    axs1.hist(mu_samples, bins=50)
    axs1.set_title('Logistic regression - Metropolis Hastings Posterior')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("b values")
    axs2.set_ylabel("frequency")
    axs2.hist(b_samples, bins=50)
    axs2.set_title('Logistic regression - Metropolis Hastings Posterior')
    plt.show()
  if arg == "simHMM":
    xs = [d[0] for d in data]
    ys = [d[1] for d in data]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("trans state")
    axs1.set_ylabel("obs state")
    axs1.scatter(xs, ys, cmap=color_map)
    axs1.set_title('HMM - Simulation')
    plt.show()
  if arg == "mhHMM":
    # Note : this works less well for certain parameters of trans_p and obs_p used for the training data
    trans_ps_unique = data[0]
    obs_ps_unique   = data[1]
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
  if arg == "simLDA":
    print(data)
    words = list(np.array(data).ravel())
    _, ax = plt.subplots(nrows=1)
    ws = list(set(words))
    freqs = [ words.count(w)  for w in ws]
    ax.bar(ws, freqs)
    ax.set_xticklabels(ws)
    plt.show()
  if arg == "mhLDA":
    ws          = ['DNA', 'evolution', 'parsing', 'phonology']
    topic_ps = data[0][0]
    topic_0s = data[1][0]
    topic_1s = data[1][1]
    fig, ax = plt.subplots(nrows=1)
    ax.bar(['Topic 0', 'Topic 1'], topic_ps, 0.8)
    ax.set_xticklabels(['Topic 0', 'Topic 1'])
    plt.title('Document-Topic Distribution')
    fig0, ax0 = plt.subplots(nrows=1)
    ax0.bar(ws, topic_0s, 0.8)
    ax0.set_xticklabels(ws)
    plt.title('Topic-Word Distribution 0')
    fig1, ax1 = plt.subplots(nrows=1)
    ax1.bar(ws, topic_1s, 0.8)
    ax1.set_xticklabels(ws)
    plt.title('Topic-Word Distribution 1')
    plt.show()
  if arg == "simHLinRegr":
    basement_ys   = data[0]
    basement_xs   = [0 for i in range(len(basement_ys))]
    nobasement_ys = data[1]
    nobasement_xs = [1 for i in range(len(nobasement_ys))]
    print(data)
    plt.scatter(basement_xs, basement_ys, color="r")
    plt.scatter(nobasement_xs, nobasement_ys, color='b')
    plt.ylabel('Log radon level')
    plt.xticks([0, 1], ["basement", "no basement"])
    plt.show()
  if arg == "mhHLinRegr":
    intercepts = data[0]
    gradients  = data[1]
    plt.xticks([0, 1], ["basement", "no basement"])
    plt.ylabel('Log radon level')
    for (m, c) in zip(gradients, intercepts):
      x = np.linspace(0, 1, 100)
      y = m * x + c
      plt.plot(x, y)
    plt.show()
  if arg == "mhHLinRegrpost":
    mu_a       = data[0]
    mu_b       = data[1]
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
    plt.show()
  if arg == "mhSchool":
    mu_samples_unique   = data[0]
    thetas              = data[1]
    thetas_             = [[d[i] for d in thetas] for i in range(len(thetas[0]))]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu values")
    axs1.set_ylabel("frequency")
    axs1.hist(mu_samples_unique, bins=50)
    axs1.set_title('School - Metropolis Hastings Posterior (mu)')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("theta")
    axs2.set_ylabel("value")
    axs2.boxplot(thetas_)
    axs2.set_title('School - Metropolis Hastings Posterior (thetas)')
    plt.show()
  if arg == "simGMM":
    xyss = data
    x0s  = [xy[0][0] for xy in xyss]
    x1s  = [xy[0][1] for xy in xyss]
    ys   = [xy[1] for xy in xyss]
    print(xyss)
    plt.scatter(x0s, x1s, c=ys)
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('Gaussian Mixture Model')
    plt.show()
  if arg == "mhGMM":
    mu_0s = [d[0] for d in data]
    mu_1s = [d[1] for d in data]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu_0 values")
    axs1.set_ylabel("frequency")
    axs1.hist(mu_0s, bins=50)
    axs1.set_title('GMM - Metropolis Hastings Posterior (mu_0)')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("mu_1 values")
    axs2.set_ylabel("frequency")
    axs2.hist(mu_1s, bins=50)
    axs2.set_title('GMM - Metropolis Hastings Posterior (mu_1)')
    plt.show()
if __name__ == "__main__":
  main()

# 0.161703431 0.0808751181 0.0365742808 0.720847170
# 0.232963296 0.127890173 0.469281425 0.169865107