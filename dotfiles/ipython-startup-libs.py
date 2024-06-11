import functools
import itertools

import numpy as np
import sympy

sympy.init_printing()
np.set_printoptions(linewidth=200)

up = ket_0 = np.array([1, 0])
dn = ket_1 = np.array([0, 1])

uu = ket_00 = np.kron(up, up)
ud = ket_01 = np.kron(up, dn)
du = ket_10 = np.kron(dn, up)
dd = ket_11 = np.kron(dn, dn)

I = op_I = np.eye(2)
Z = op_Z = np.outer(ket_0, ket_0) - np.outer(ket_1, ket_1)
X = op_X = np.outer(ket_0, ket_1) + np.outer(ket_1, ket_0)
Y = op_Y = -1j * op_Z @ op_X


def ts(*args):
    return functools.reduce(np.kron, args)
