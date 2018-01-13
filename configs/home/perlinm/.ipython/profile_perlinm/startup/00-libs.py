import pylab as pl
import sympy as sp
import scipy.linalg as lin
import qutip as qt
from pylab import *
sp.init_printing()

up = qt.basis(2,0)
dn = qt.basis(2,1)

uu = qt.tensor(up,up)
ud = qt.tensor(up,dn)
du = qt.tensor(dn,up)
dd = qt.tensor(dn,dn)
ud_S = (ud+du).unit()
ud_A = (ud-du).unit()

I2 = qt.qeye(2)
sz = qt.sigmaz()
sx = qt.sigmax()
sy = qt.sigmay()

ts = qt.tensor
i = 1j
pi = pl.pi
e = pl.e
