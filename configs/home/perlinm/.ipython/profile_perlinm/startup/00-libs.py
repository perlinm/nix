import numpy as np
import sympy as sp
import scipy.linalg as lin
from qutip import *
sp.init_printing()

up = basis(2,0)
dn = basis(2,1)

uu = tensor(up,up)
ud = tensor(up,dn)
du = tensor(dn,up)
dd = tensor(dn,dn)
ud_S = (ud+du).unit()
ud_A = (ud-du).unit()

I2 = qeye(2)
sz = sigmaz()
sx = sigmax()
sy = sigmay()

ts = tensor
i = 1j
pi = np.pi
e = np.e
