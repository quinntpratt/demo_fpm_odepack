# FPM-ODEPACK Demonstration

## Background

This project is meant to mimic certain aspects of large physics codes.
Namely,
1. The core "calculation" is a FORTRAN program using ``ODEPACK`` solvers.
2. Parameters can be adjusted using a FORTRAN namelist (``.nml`` file).
3. There's a python wrapper running the FORTRAN code and plotting the resulting data.

Instead of trying to link the [source code of ``ODEPACK``](https://computing.llnl.gov/projects/odepack/software) myself (written in ``f77``). 
I used the [Fortran Package Manager](https://fpm.fortran-lang.org/en/index.html) (FPM) framework with [this FPM dependnacy](https://github.com/Nicholaswogan/odepack). 

This allows us to use the ``LSODA`` routine within a Modern Fortran program.

## Installation/Building

Since I've used the FPM framework one can use:
```
fpm build
```
... to be completed.

## Example Usage

... to be written.