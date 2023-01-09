# https://nixos.wiki/wiki/Python
{ pkgs }:
let 
  my-python-packages = python-packages: with python-packages; [
    black[jupyter]
    cirq
    flake8
    ipython
    jupyter
    matplotlib
    mypy
    networkx
    numpy
    pip
    pylint
    pytest
    scipy
    setuptools
    sympy
  ];
  python-with-my-packages = [ (pkgs.python3.withPackages my-python-packages) ];
in
python-with-my-packages ++ [ pkgs.conda ]
