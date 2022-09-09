# https://nixos.wiki/wiki/Python
{ pkgs }:
let 
  my-python-packages = python-packages: with python-packages; [
    black[jupyter]
    flake8
    ipython
    jupyter
    matplotlib
    mypy
    networkx
    numpy
    pylint
    pytest
    scipy
    setuptools
  ];
  python-with-my-packages = pkgs.python3.withPackages my-python-packages;
in
python-with-my-packages
