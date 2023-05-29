import sys
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.ticker import AutoMinorLocator
import ast
from sklearn import linear_model
from scipy.special import expit
from scipy.stats import beta
from scipy.stats import norm
import numpy as np
from scipy.interpolate import make_interp_spline

def save_multi_image(filename):
   pp = PdfPages(filename)
   fig_nums = plt.get_fignums()
   figs = [plt.figure(n) for n in fig_nums]
   for fig in figs:
      fig.savefig(pp, format='pdf')
   pp.close()

def main():
  arg  = sys.argv[1]
  f    = open("model-output.txt", "r")

  data = ast.literal_eval(f.read().replace('-Infinity', '-2e308')) #
  color_map = plt.cm.get_cmap('Blues')
  if arg in ["simLinRegrOnce", "simLinRegr"]:
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    xs = xys[0]
    ys = xys[1]
    plt.scatter(xs, ys)
    plt.xlabel('x data points')
    plt.ylabel('y data points')
    plt.title('Linear regression')
  if arg in ["lwLinRegrOnce", "lwLinRegr"]:
    mus = [d[0] for d in data]
    ps  = [d[1] for d in data]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel('mu value')
    axs1.set_ylabel('probability')
    axs1.scatter(mus, ps)
    axs1.set_title('Linear regression - Likelihood Weighting')

  if arg in ["mhLinRegrOnce", "mhLinRegr", "smcLinRegr", "rmsmcLinRegr", "pmmhLinRegr", "smc2LinRegr"]:
    mus = data[0]
    cs  = data[1]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu values", fontsize=12)
    axs1.set_ylabel("frequency")
    axs1.hist(mus, bins=25)
    axs1.set_title('Linear regression - Metropolis Hastings')
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("c values", fontsize=12)
    axs1.set_ylabel("frequency")
    axs1.hist(cs, bins=25)
    axs1.set_title('Linear regression - Metropolis Hastings')
  if arg in ["bbviLinRegr", "mleLinRegr", "mapLinRegr"]:
    mu_mean = data[0][0]
    mu_std  = data[0][1]
    c_mean  = data[1][0]
    c_std   = data[1][1]

    x_mu = np.linspace(mu_mean - 3*mu_std, mu_mean + 3*mu_std, 100)
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.plot(x_mu, norm.pdf(x_mu, mu_mean, mu_std))
    axs1.set_title('Linear regression BBVI - Mu distribution')

    x_c = np.linspace(c_mean - 3*c_std, c_mean + 3*c_std, 100)
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.plot(x_c, norm.pdf(x_c, c_mean, c_std))
    axs2.set_title('Linear regression BBVI - C distribution')

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
  if arg == "mhSIR":
    rhos_unique   = data[0]
    betas_unique  = data[1]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("ρ values", fontsize=12)
    axs1.set_ylabel("frequency")
    axs1.hist(rhos_unique, bins=55)
    axs1.set_title('SIR - Metropolis Hastings Posterior (Rho)')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("β values", fontsize=12)
    axs2.set_ylabel("frequency")
    axs2.hist(betas_unique, bins=55)
    axs2.set_title('HMM - Metropolis Hastings Posterior (Beta)')

  if arg in ["simLogRegrOnce", "simLogRegr"]:
    xys = np.array(data)
    xs =  np.array([xy[0] for xy in xys])
    ys =  np.array([xy[1] for xy in xys])
    model = linear_model.LogisticRegression(C=1e5, solver='lbfgs')
    model.fit(xs.reshape(-1,1), ys)
    x_test = np.linspace(-2.0,2.0,num=100)
    plt.plot(x_test, sigmoid.ravel(),c="green", label = "logistic fit")
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('Logistic regression simulation')
  if arg in ["lwLogRegrOnce", "lwLogRegr"]:
    mus = [d[0][0] for d in data]
    bs  = [d[0][1] for d in data]
    ps  = [d[1] for d in data]
    _, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel('mu value')
    axs1.set_ylabel('probability')
    axs1.scatter(mus, ps)
    _, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel('b value')
    axs2.set_ylabel('probability')
    axs2.scatter(bs, ps)
    axs2.set_title('Logistic regression - Likelihood Weighting')
  if arg in ["mhLogRegrOnce", "mhLogRegr"]:
    mu_samples = data[0]
    b_samples  = data[1]
    _, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu values")
    axs1.set_ylabel("frequency")
    axs1.hist(mu_samples, bins=50)
    axs1.set_title('Logistic regression - Metropolis Hastings Posterior')
    _, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("b values")
    axs2.set_ylabel("frequency")
    axs2.hist(b_samples, bins=50)
    axs2.set_title('Logistic regression - Metropolis Hastings Posterior')

  if arg == "simHMM":
    xs = [d[0] for d in data]
    ys = [d[1] for d in data]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("trans state")
    axs1.set_ylabel("obs state")
    axs1.scatter(xs, ys, cmap=color_map)
    axs1.set_title('HMM - Simulation')
    plt.show()
  if arg in ["lwHMM"]:
    trans_p = data[0]
    obs_p   = data[1]
    ps      = data[2]
    total_p = sum(ps)
    ps_normalised = list(map(lambda p : p/total_p, ps))
    _, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel('trans_p value')
    axs1.set_ylabel('probability')
    axs1.scatter(trans_p, ps_normalised)
    axs1.set_title('HMM - Likelihood Weighting')
    _, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel('obs_p value')
    axs1.set_ylabel('probability')
    axs1.scatter(obs_p, ps_normalised)
    axs1.set_title('HMM - Likelihood Weighting')
  if arg in ["mhHMM", "smcHMM", "rmsmcHMM", "pmmhHMM", "smc2HMM"]:
    # Note : this works less well for certain parameters of trans_p and obs_p used for the training data
    trans_ps_unique = data[0]
    obs_ps_unique   = data[1]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("trans_ps values")
    axs1.set_ylabel("frequency")
    axs1.hist(trans_ps_unique, bins=50)
    axs1.set_title('HMM - Metropolis Hastings Posterior (Trans Idx)')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("obs_ps values")
    axs2.set_ylabel("frequency")
    axs2.hist(obs_ps_unique, bins=50)
    axs2.set_title('HMM - Metropolis Hastings Posterior (Obs Idx)')
    plt.show()
  if arg in ["bbviHMM", "mleHMM", "mapHMM"]:
    trans_alpha = data[0][0]
    trans_beta  = data[0][1]
    obs_alpha   = data[1][0]
    obs_beta    = data[1][1]
    x_trans = np.linspace(beta.ppf(0.01, trans_alpha, trans_beta), beta.ppf(0.99, trans_alpha, trans_beta), 100)
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.plot(x_trans, beta.pdf(x_trans, trans_alpha, trans_beta), 'r-')
    plt.title('BBVI - Transition Beta Distribution')
    plt.xlabel('Values of Random Variable X (0, 1)')
    plt.ylabel('Probability')
    x_obs = np.linspace(beta.ppf(0.01, obs_alpha, obs_beta), beta.ppf(0.99, obs_alpha, obs_beta), 100)
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.plot(x_obs, beta.pdf(x_obs, obs_alpha, obs_beta), 'r-')
    plt.title('BBVI - Observation Beta Distribution')
    plt.xlabel('Values of Random Variable X (0, 1)')
    plt.ylabel('Probability')
    plt.show()

  if arg == "simLDA":
    words = list(np.array(data).ravel())
    _, ax = plt.subplots(nrows=1)
    ws = list(set(words))
    freqs = [ words.count(w)  for w in ws]
    ax.bar(ws, freqs)
  if arg in ["mhLDA", "smcLDA", "rmsmcLDA", "pmmhLDA"]:
    ws       = ['DNA', 'evolution', 'parsing', 'phonology']
    topic_ps = data[0][0]
    topic_0s = data[1][0]
    topic_1s = data[1][1]
    _, ax = plt.subplots(nrows=1)
    ax.bar(['Topic 0', 'Topic 1'], topic_ps, 0.8)
    plt.title('Document-Topic Distribution')
    _, ax0 = plt.subplots(nrows=1)
    ax0.bar(ws, topic_0s, 0.8)
    plt.title('Topic-Word Distribution 0')
    _, ax1 = plt.subplots(nrows=1)
    ax1.bar(ws, topic_1s, 0.8)
    plt.title('Topic-Word Distribution 1')
  if arg in ["bbviLDA", "mleLDA", "mapLDA"]:
    ws       = ['DNA', 'evolution', 'parsing', 'phonology']
    topic_ps = data[0]
    topic_0s = data[1]
    topic_1s = data[2]
    _, ax = plt.subplots(nrows=1)
    ax.bar(['Topic 0', 'Topic 1'], topic_ps, 0.8)
    plt.title('Document-Topic Dirichlet Concentrations')
    _, ax0 = plt.subplots(nrows=1)
    ax0.bar(ws, topic_0s, 0.8)
    plt.title('Topic-Word Dirichlet Concentrations 0')
    _, ax1 = plt.subplots(nrows=1)
    ax1.bar(ws, topic_1s, 0.8)
    plt.title('Topic-Word Dirichlet Concentrations 1')

  if arg == "simRadon":
    basement_ys   = data[0]
    basement_xs   = [0 for i in range(len(basement_ys))]
    nobasement_ys = data[1]
    nobasement_xs = [1 for i in range(len(nobasement_ys))]
    plt.scatter(basement_xs, basement_ys, color="r")
    plt.scatter(nobasement_xs, nobasement_ys, color='b')
    plt.ylabel('Log radon level')
    plt.xticks([0, 1], ["basement", "no basement"])
  if arg == "mhRadon":
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
  if arg == "mhPredRadon":
    intercepts = data[0]
    gradients  = data[1]
    plt.xticks([0, 1], ["basement", "no basement"])
    plt.ylabel('Log radon level')
    for (m, c) in zip(gradients, intercepts):
      x = np.linspace(0, 1, 100)
      y = m * x + c
      plt.plot(x, y)

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

  if arg == "simGMM":
    xyss = data
    x0s  = [xy[0][0] for xy in xyss]
    x1s  = [xy[0][1] for xy in xyss]
    ys   = [xy[1] for xy in xyss]
    plt.scatter(x0s, x1s, c=ys)
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('Gaussian Mixture MulModel')
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

  save_multi_image("model-output.pdf")
  plt.show()
if __name__ == "__main__":
  main()
