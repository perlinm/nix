import os

import functools as ft
import itertools as it

import numpy as np
import matplotlib.pyplot as plt

import scipy, scipy.optimize

import sympy as sym

import cirq

sym.init_printing()
np.set_printoptions(linewidth=200)

up = ket_0 = np.array([1,0])
dn = ket_1 = np.array([0,1])

I = op_I = np.eye(2)
Z = op_Z = np.outer(ket_0,ket_0) - np.outer(ket_1,ket_1)
X = op_X = np.outer(ket_0,ket_1) + np.outer(ket_1,ket_0)
Y = op_Y = -1j * op_Z @ op_X

uu = np.kron(up,up)
ud = np.kron(up,dn)
du = np.kron(dn,up)
dd = np.kron(dn,dn)
ud_S = (ud+du) / np.sqrt(2)
ud_A = (ud-du) / np.sqrt(2)

def ts(*args):
    return ft.reduce(np.kron, args)
