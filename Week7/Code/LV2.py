#!/usr/bin/env python3

"""Produces 2 plots of consumer resource interactions based on The Lotka-Volterra model"""

__author__ = 'Prannoy Thazhakaden (PVT23@ic.ac.uk)'
__version__ = '0.0.1'


## imports ##
import sys # module to interface our program with the operating system
import numpy as np
import scipy as sc
from scipy import stats
import scipy.integrate as integrate
import matplotlib.pylab as p    

## constants ##

## functions ##

def dCR_dt(pops, t=0):
    """Function for The Lotka-Volterra model"""
    R = pops[0]
    C = pops[1]
    dRdt = r * R * (1 - R/K) - a * R * C 
    dCdt = -z * C + e * a * R * C
    
    return np.array([dRdt, dCdt])   



r = 0.9
a = 0.6
z = 0.9
e = 0.4
K = 8

t = np.linspace(0, 100, 1000)

R0 = 1
C0 = 1 
RC0 = np.array([R0, C0])

RC0

pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)

pops

type(infodict)

infodict.keys()

infodict['message']



f1 = p.figure()

p.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
p.plot(t, pops[:,1]  , 'b-', label='Consumer density')
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population density')
p.title('Consumer-Resource population dynamics')
p.show()# To display the figure



f1.savefig('../Results/LV_model.pdf') #Save figure

p.close() 

f2 = p.figure()

p.plot(pops[:,0], pops[:,1], 'r-', label='Resource density') # Plot
p.grid()
p.xlabel('Resource density')
p.ylabel('Consumer density')
p.title('Consumer-Resource population dynamics')
p.show()# To display the figure


f2.savefig('../Results/LV_model2.pdf') #Save figure

p.close() 

def main(argv):
    """ Main entry point of the program """
    return 0

if __name__ == "__main__":
    """Makes sure the "main" function is called from the command line"""
    status = main(sys.argv)
    sys.exit(status)



