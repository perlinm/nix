import functools
import itertools

import cirq
import numpy as np
import numpy.typing as npt
import scipy.linalg
import sympy

sympy.init_printing()
np.set_printoptions(linewidth=200)

up = ket_0 = np.array([1, 0])
dn = ket_1 = np.array([0, 1])

uu = ket_00 = np.kron(up, up)
ud = ket_01 = np.kron(up, dn)
du = ket_10 = np.kron(dn, up)
dd = ket_11 = np.kron(dn, dn)

up_x = np.array([1, 1]) / np.sqrt(2)
dn_x = np.array([1, -1]) / np.sqrt(2)
up_y = np.array([1, 1j]) / np.sqrt(2)
dn_y = np.array([1, -1j]) / np.sqrt(2)

I = op_I = np.eye(2)  # noqa
Z = op_Z = np.outer(ket_0, ket_0) - np.outer(ket_1, ket_1)
X = op_X = np.outer(ket_0, ket_1) + np.outer(ket_1, ket_0)
Y = op_Y = -1j * op_Z @ op_X

H = op_H = cirq.unitary(cirq.H)
S = op_S = cirq.unitary(cirq.S)
T = op_T = cirq.unitary(cirq.T)


pi = np.pi
sqrt = np.sqrt
expm = scipy.linalg.expm


def ts(*args):
    return functools.reduce(np.kron, args)


def chop(
    matrix: npt.NDArray[np.complex128], *, atol: float = 1e-8
) -> npt.NDArray[np.complex128]:
    new_matrix = matrix.copy()
    for idx in np.ndindex(new_matrix.shape):
        if np.isclose(new_matrix[idx].real, 0, atol=atol):
            new_matrix[idx] = new_matrix[idx].imag * 1j
        if np.isclose(new_matrix[idx].imag, 0, atol=atol):
            new_matrix[idx] = new_matrix[idx].real
    return new_matrix


def get_pauli_terms(
    matrix: npt.NDArray[np.complex128], *, atol: float = 1e-8
) -> dict[str, np.complex128]:
    assert matrix.ndim == 2 and matrix.shape[0] == matrix.shape[1]
    num_factors = np.log2(matrix.shape[0])
    assert num_factors == int(num_factors)

    terms = {}
    _pauli_to_mat = {"I": op_I, "X": op_X, "Y": op_Y, "Z": op_Z}
    for pauli_ops in itertools.product(["I", "X", "Y", "Z"], repeat=int(num_factors)):
        mat = functools.reduce(np.kron, [_pauli_to_mat[pauli] for pauli in pauli_ops])
        val = mat.ravel().conj() @ matrix.ravel() / 2**num_factors
        if np.isclose(val, 0, atol=atol):
            term = "".join(pauli_ops)
            terms[term] = val

    return terms
