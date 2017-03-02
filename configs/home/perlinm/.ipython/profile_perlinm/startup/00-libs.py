from scipy import *
from qutip import *
from pylab import *
import sympy as sym

i = complex(0,1)

sx = sigmax()
sy = sigmay()
sz = sigmaz()
I2 = qeye(2)

up = basis(2,0)
dn = basis(2,1)

uu = basis(4,0)
ud = basis(4,1)
du = basis(4,2)
dd = basis(4,3)

ts = tensor
