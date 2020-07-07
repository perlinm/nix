import os
os.environ["QT_LOGGING_RULES"] = "qt5ct.debug=false"

import pylab as pl
import numpy as np
import sympy as sym
import qutip as qt
import tensorflow as tf

import itertools as it

from pylab import *
sym.init_printing()

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

H = (Z+X)/sqrt(2)
S = Z.sqrtm()
T = S.sqrtm()

sz = Z/2
sx = X/2
sy = Y/2
sp = qt.sigmap()
sm = qt.sigmam()

ts = qt.tensor
pi = pl.pi
e = pl.e
i = 1j
