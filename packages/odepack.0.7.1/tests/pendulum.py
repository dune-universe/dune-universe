#!/usr/bin/python

from scipy.integrate import odeint
from math import sin

l = 2.   # m
m1 = 2.  # kg
m2 = 1.  # kg
k = 5.   # kg/m^2, spring rigidity constant
g = 9.81 # m/s^2
a = l / 2.

gl = - g / l
al = a * a * k / (l * l)

def vec_field(u,t):
    dth = u[2] - u[0]
    return [u[1], gl * sin(u[0]) + al / m1 * dth, \
                u[3], gl * sin(u[2]) - al / m2 * dth ]

def main():
    y0 = [ 1., 0., 0., 0. ]
    print (odeint(vec_field, y0, [0., 10.]))[-1]
    odeint(vec_field, y0, range(1,5500))

if __name__ == "__main__":
    main()
