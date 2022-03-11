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
import pymc3 as pm


# Remove consecutive duplicates
def removeDuplicates(xs):
  return [v for i, v in enumerate(xs) if i == 0 or v != xs[i-1]]

def main():
  arg  = sys.argv[1]
  f    = open("model-output.txt", "r")

  data = ast.literal_eval(f.read().replace('-Infinity', '-2e308')) #
  color_map = plt.cm.get_cmap('Blues')
  if arg == "lin-regr-sim":
    xys = np.array(data)
    xs = [xy[0] for xy in xys]
    ys = [xy[1] for xy in xys]
    plt.scatter(xs, ys)
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('Linear regression')
    plt.show()
  if arg == "lin-regr-lw":
    params    = [ d[0] for d in data]
    mus       = [ param[0] for param in params]
    cs        = [ param[1] for param in params]
    sigmas    = [ param[2] for param in params]
    ps         = [ d[1] for d in data]
    print(ps)
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel('mu value')
    axs1.set_ylabel('probability')
    axs1.scatter(mus, ps)
    axs1.set_title('Linear regression - Likelihood Weighting')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel('c value')
    axs2.set_ylabel('probability')
    axs2.scatter(cs, ps)
    axs2.set_title('Linear regression - Likelihood Weighting')
    plt.show()
  if arg == "lin-regr-mh":
    mu_samples_unique = np.array(data[0]).ravel()
    c_samples_unique  = np.array(data[1]).ravel()
    sigma_samples_unique  = np.array(data[2]).ravel()
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
  if arg == "lin-regr-smc":
    mus = data
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu values", fontsize=12)
    axs1.set_ylabel("frequency")
    axs1.hist(mus, bins=25)
    axs1.set_title('Lin Regr - Mu SMC Posterior')
    plt.show()
  if arg == "log-regr-sim":
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
    plt.title('Logistic regression - basic simulation')
    plt.show()
  if arg == "log-regr-lw":
    params    = [ d[0] for d in data]
    mus       = [ param[0] for param in params]
    bs        = [ param[1] for param in params]
    ps        = [ d[1] for d in data]
    print(ps)
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel('mu value')
    axs1.set_ylabel('probability')
    axs1.scatter(mus, ps)
    axs1.set_title('Logistic regression - Likelihood Weighting')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel('b value')
    axs2.set_ylabel('probability')
    axs2.scatter(bs, ps)
    axs2.set_title('Logistic regression - Likelihood Weighting')
    plt.show()
  if arg == "log-regr-mh":
    mus = np.array(data[0]).ravel()
    bs = np.array(data[1]).ravel()
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu values")
    axs1.set_ylabel("frequency")
    axs1.hist(mus, bins=50)
    axs1.set_title('Logistic regression - Metropolis Hastings Posterior')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("b values")
    axs2.set_ylabel("frequency")
    axs2.hist(bs, bins=50)
    axs2.set_title('Logistic regression - Metropolis Hastings Posterior')
    plt.show()
  if arg == "hmm-sim":
    xys = np.array(data)
    xs = [xy[0] for xy in xys]
    ys = [xy[1] for xy in xys]
    plt.scatter(xs, ys)
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('HMM Simulation')
    plt.show()
  if arg == "hmm-lw":
    params    = [ d[0] for d in data]
    trans_ps  = [ param[0] for param in params]
    obs_ps    = [ param[1] for param in params]
    ps        = [ d[1] for d in data]
    print(ps)
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel('trans_p value')
    axs1.set_ylabel('probability')
    axs1.scatter(trans_ps, ps)
    axs1.set_title('HMM - Likelihood Weighting')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel('obs_p value value')
    axs2.set_ylabel('probability')
    axs2.scatter(obs_ps, ps)
    axs2.set_title('HMM - Likelihood Weighting')
    plt.show()
  if arg == "hmm-mh":
    trans_ps = np.array(data[0]).ravel()
    obs_ps  = np.array(data[1]).ravel()
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("trans_ps values")
    axs1.set_ylabel("frequency")
    axs1.hist(trans_ps, bins=50)
    axs1.set_title('HMM - Metropolis Hastings Posterior')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("obs_ps values")
    axs2.set_ylabel("frequency")
    axs2.hist(obs_ps, bins=50)
    axs2.set_title('HMM - Metropolis Hastings Posterior')
    plt.show()
  if arg == "topic-sim":
    print(data)
    words = list(np.array(data).ravel())
    _, ax = plt.subplots(nrows=1)
    ws = list(set(words))
    freqs = [ words.count(w)  for w in ws]
    ax.bar(ws, freqs)
    ax.set_xticklabels(ws)
    plt.show()
  if arg == "topic-mh":
    plt.rcParams.update({'font.family': 'serif'})
    # plt.rcParams.update({'font.weight': 'bold'})
    plt.rcParams.update({'font.size': 15})
    ws          = ['DNA', 'evolution', 'parsing', 'phonology']
    # data is in format (topic_ps, word_ps)
    doc_topic_ps     = data[0][-1][0]
    topic_0_word_ps  = data[1][-1][0]
    topic_1_word_ps  = data[1][-1][1]

    fig, ax = plt.subplots(nrows=1)
    ax.bar(['Topic 0', 'Topic 1'], doc_topic_ps, 0.8)
    ax.set_xticklabels(['Topic 0', 'Topic 1'])
    plt.title('Document-Topic Distribution', fontname="serif")
    fig0, ax0 = plt.subplots(nrows=1)
    ax0.bar(ws, topic_0_word_ps, 0.8)
    ax0.set_xticklabels(ws)
    plt.title('Topic-Word Distribution 0', fontname="serif")
    fig1, ax1 = plt.subplots(nrows=1)
    ax1.bar(ws, topic_1_word_ps, 0.8)
    ax1.set_xticklabels(ws)
    plt.title('Topic-Word Distribution 1', fontname="serif")
    plt.show()
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
    axs1.set_xlabel("days")
    axs1.set_ylabel("population", fontsize=12)
    axs1.plot(X_, S_, color='blue', label='Actual Susceptible')
    axs1.plot(X_, I_, color='red', label='Actual Infected')
    axs1.plot(X_, R_, color='green', label='Actual Recovered')
    axs1.plot(X_, IC_, color='black', label='Reported Infected')
    axs1.set_title('SIR model - Basic Simulation')
    plt.xlim([0,100])
    plt.ylim([0,800])
    # plt.legend()
    plt.show()
  if arg == "sir-mh-post":
    print(data)
    rhos_unique   = data[0]
    betas_unique  = data[1]
    gammas_unique = data[2]
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
    fig3, axs3 = plt.subplots(nrows=1)
    axs3.set_xlabel("gamma values", fontsize=12)
    axs3.set_ylabel("frequency")
    axs3.hist(gammas_unique, bins=25)
    axs3.set_title('HMM - Metropolis Hastings Posterior (Gamma)')
    plt.show()
  if arg == "sirv-basic":
    # we expect data to be in the format of (sir-values :: [(Int, Int, Int)], infected count :: [Int])
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
    axs1.set_title('SIRV model - Basic Simulation')
    plt.xlim([0,100])
    plt.ylim([0,800])
    plt.legend(prop={'size': 14})
    plt.show()
if __name__ == "__main__":
  main()

# 0.161703431 0.0808751181 0.0365742808 0.720847170
# 0.232963296 0.127890173 0.469281425 0.169865107