{ config, lib, pkgs, ... }:

{
  extraDependencies = (with pkgs; [
    autoconf
    automake
    boost
    ccache
    cgal
    cmake
    doxygen
    fftw
    fftwFloat
    fftwMpi
    gdb
    gfortran
    gmp
    gsl
    gtest
    (hdf5.override { fortranSupport = true; })
    jupyter
    libtool
    libvdwxc
    libxc
    meson
    mpfr
    ncurses
    netcdf
    nfft
    ninja
    openblasCompat
    openblasCompat
    openmpi
    perl
    pkg-config
    spglib
    sphinx
    valgrind
  ]) ++ (with pkgs.llvmPackages; [
    bintools
    clang
    compiler-rt
    libclang
    libclang.python
    libcxx
    libunwind
    lld
    openmp
  ]) ++ (with pkgs.python3Packages; [
    configobj
    decorator
    h5py
    linkify-it-py
    mako
    matplotlib
    mpi4py
    myst-parser
    nbsphinx
    numpy
    numpydoc
    pip
    pyyaml
    scipy
    sphinx-rtd-theme
    sympy
    wheel
  ]);
}
