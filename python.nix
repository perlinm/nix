# https://nixos.wiki/wiki/Python
{ pkgs }:
let
  my-python-packages = python-packages:
    with python-packages; [
      black
      # cirq
      flake8
      ipython
      jupyter
      matplotlib
      mypy
      networkx
      numpy
      pandas
      # pip # only use inside virtual environments!
      pylint
      pytest
      qutip
      scipy
      sympy
      # language server protocol packages
      python-lsp-server
      python-lsp-black
      pyls-flake8
      pyls-isort
      # pylsp-mypy
    ];
  python-with-my-packages = [ (pkgs.python3.withPackages my-python-packages) ];

  extra-libs-for-conda = [ pkgs.git ];
  conda-with-extra-libs =
    pkgs.conda.override { extraPkgs = extra-libs-for-conda; };

in python-with-my-packages ++ [ conda-with-extra-libs pkgs.sage ]
