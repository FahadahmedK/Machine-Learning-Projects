import math

import pandas   as pd
import numpy    as np
import datetime as dt

from scipy.optimize import minimize
from scipy.optimize import Bounds

from statsmodels.nonparametric.bandwidths import bw_silverman
from sklearn.neighbors import KernelDensity
from KDEpy import FFTKDE
from decimal import Decimal

def weight(i, t, T, omega):
    return (omega**(abs(t-i)) + omega**(2*T-t-i+1) + omega**(t+i-1))*(1-omega)/(1+omega)

def _weight(i, t, T, omega):
    return Decimal((omega**(abs(t-i)) + omega**(2*T-t-i+1) + omega**(t+i-1))*(1-omega)/(1+omega))

weights = np.vectorize(weight)
_weights = np.vectorize(_weight)

def likelihood(omega, X, t):
    T = len(X)
    h = bw_silverman(X)
    iterations = list(range(0,T))           # vector of i for weights
    w     = weights(iterations, t, T,omega) # generating weights
    x, y  = FFTKDE(kernel='gaussian', bw=h).fit(X, weights=w).evaluate(t) # density estimate at t
    l     = -(1/T)*sum(np.log((y)))         # negative because changing max problem into min problem
    return l

def _likelihood(omega, X, t):
    T = len(X)
    h = bw_silverman(X)
    X = X.reshape(1, -1)
    iterations = list(range(0,T))           # vector of i for weights
    w     = weights(iterations, t, T,omega) # generating weights
    kde_weighted = KernelDensity(kernel='gaussian', bandwidth=h).fit(X, sample_weights=w)
    x, y  = np.exp(kde_weighted.score_samples(X))
    #x, y  = FFTKDE(kernel='gaussian', bw=h).fit(X, weights=w).evaluate(t) # density estimate at t
    l     = -(1/T)*sum(np.log((y)))         # negative because changing max problem into min problem
    return l

def TKDE(ts, start='2019-01-02', until='2020-01-01', manual=False):
    
    t = len(ts)
    
    if ts is not None:
        ts = ts.loc[start:until].values
          
    x_init    = np.array([0.5])       # initial guess
    bounds    = Bounds([0.00001],[1]) # bounds for omega 
    omega_opt = minimize(likelihood, 
                         x_init, 
                         method='trust-constr', 
                         args=(ts,t), 
                         options={'disp': True}, 
                         bounds=bounds)['x']
    
    omega_opt = omega_opt
    
    points    = np.linspace(-38, 19, 2000)
    
    y1 = FFTKDE(kernel='gaussian', 
                   bw='silverman').fit(ts).evaluate(points)

    iterations = list(range(0,len(ts)))
    w     = weights(iterations, t, len(ts), omega_opt)
    y2 = FFTKDE(kernel='gaussian', 
                   bw='silverman').fit(ts, weights=w).evaluate(points)
    
    return {'TVKDE'    : y2,
            'Uweights' : y1,
            'omega'    : omega_opt}
    
    
    
    

def _TKDE(ts, start='2019-01-02', until='2020-01-01', manual=False, only_omega=False):
    
    t = len(ts)
    
    if ts is not None:
        ts = ts.loc[start:until].values.reshape(1, -1)
          
    x_init    = np.array([0.5])       # initial guess
    bounds    = Bounds([0.00001],[1]) # bounds for omega 
    omega_opt = minimize(_likelihood, 
                         x_init, 
                         method='trust-constr', 
                         args=(ts,t), 
                         options={'disp': True, 'maxiter' : 500}, 
                         bounds=bounds)['x']
    
    omega_opt = omega_opt
    if only_omega: 
        return only_omega
    
    points    = np.linspace(-38, 19, 2000)
    
    h = bw_silverman(ts)
    print(h)
    
    #kde_weighted = KernelDensity(kernel='gaussian', bandwidth=h).fit(ts)
    #y1 = np.exp(kde_weighted.score_samples(ts))

    iterations = list(range(0,len(ts)))
    w     = weights(iterations, t, len(ts), omega_opt)
    
    kde_weighted = KernelDensity(kernel='gaussian', bandwidth=h).fit(ts, sample_weight=w)
    y2 = np.exp(kde_weighted.score_samples(ts))
    
    return {'TVKDE'    : y2,
            'kde'      : kde_weighted,
            'omega'    : omega_opt}