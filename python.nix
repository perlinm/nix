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
    # pip  # only use inside virtual environments!
    pylint
    pytest
    scipy
    setuptools
    sympy
    # language server protocol packages
    python-lsp-server
    python-lsp-black
    pyls-flake8
    pyls-isort
    pylsp-mypy
  ];
  python-with-my-packages = [ (pkgs.python3.withPackages my-python-packages) ];
in
python-with-my-packages ++ [ pkgs.conda ]
