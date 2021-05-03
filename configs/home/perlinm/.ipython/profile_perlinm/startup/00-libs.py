import os
import itertools as it

import numpy as np
import matplotlib.pyplot as plt

import scipy, scipy.optimize

import sympy as sym
sym.init_printing()

import qutip as qt

up = qt.basis(2,0)
dn = qt.basis(2,1)

uu = qt.tensor(up,up)
ud = qt.tensor(up,dn)
du = qt.tensor(dn,up)
dd = qt.tensor(dn,dn)
ud_S = (ud+du).unit()
ud_A = (ud-du).unit()

I = qt.qeye(2)
Z = qt.sigmaz()
X = qt.sigmax()
Y = qt.sigmay()

H = (Z+X)/np.sqrt(2)
S = Z.sqrtm()
T = S.sqrtm()

sz = Z/2
sx = X/2
sy = Y/2
sp = qt.sigmap()
sm = qt.sigmam()

ts = qt.tensor
pi = np.pi
e = np.e
i = 1j
