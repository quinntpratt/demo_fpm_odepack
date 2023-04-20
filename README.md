# FPM-ODEPACK Demonstration

## Background

This project is a demonstration of methods meant to mimic certain aspects of large physics codes.
Namely,
1. The core calculation is a FORTRAN program using ``ODEPACK`` solvers.
2. Parameters of the simulation can be adjusted using an input FORTRAN namelist (``.nml`` file).
3. There's a python wrapper running the FORTRAN code and plotting the resulting data.

Instead of trying to compile/link the [source code of ``ODEPACK``](https://computing.llnl.gov/projects/odepack/software) myself (written in ``f77``), 
I used the [Fortran Package Manager](https://fpm.fortran-lang.org/en/index.html) (FPM) framework with [this FPM dependnacy](https://github.com/Nicholaswogan/odepack). 

This allows us to use the ``LSODA`` routine within a Modern Fortran program. Originally I did try to compile and link ``ODEPACK`` but I ran into problems because the ``gfortran`` compiler on my machine wouldn't cross-compile the 2018 fortran standard and the f77 used by ODEPACK. In the future I could look into the linked ``FPM`` dependancy to determine how they created a modern fortran interface for the f77 routines.

## Installation/Building

I went with the ``conda`` method of installing ``FPM`` (outlined on the FPM website).
Personally I installed it under a new ``conda`` environment called ``fpm`` (this is relevant when it comes time to run the fortran program from a python file which might rely on a separate conda enviornment).

To build the project under the FPM framework one can use:
```
fpm build
```

Building the project will also run the test-series. The test-series covers certain subroutines in the ``./src`` folder. Furthermore it will write a sample ``input.nml`` file as part of the tests. The tests can also be manually run with: ``fpm test``.

To run the fortran program one can use,
```
fpm run -- -i mynamelist.nml -o output.dat
```

## Example Usage

... to be written.