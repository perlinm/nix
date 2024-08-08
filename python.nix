# https://nixos.wiki/wiki/Python
{ pkgs }:
let
  my-python-packages = python-packages:
    with python-packages; [
      # cirq
      ipython
      jupyter
      matplotlib
      mypy
      networkx
      numpy
      pandas
      # pip # only use inside virtual environments!
      pytest
      qutip
      scipy
      sympy
    ];
  python-with-my-packages = [ (pkgs.python3.withPackages my-python-packages) ];

  extra-libs-for-conda = [ ];
  conda-with-extra-libs =
    pkgs.conda.override { extraPkgs = extra-libs-for-conda; };

# in python-with-my-packages ++ [ conda-with-extra-libs pkgs.ruff pkgs.sage ]
in python-with-my-packages ++ [ conda-with-extra-libs pkgs.ruff ]
