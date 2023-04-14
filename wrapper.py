#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Apr 14

@author: Quinn Pratt
"""
import numpy as np
import matplotlib.pyplot as plt
from scipy.io import FortranFile
import f90nml
import subprocess
import time

# Setup,
namelist = "mynamelist.nml"
nml = f90nml.read(namelist)
# Optionally modify the namelist,
nml['PARAMS']['N'] = 8001
nml.write(namelist, force=True)

output_file = "output.dat"
fpm_command = f"fpm run -- -i '{namelist}' -o '{output_file}'"
t0 = time.time()
subprocess.call(f"conda run -n fpm " + fpm_command, shell=True)
t1 = time.time()
print(f"python wrapper time: {abs(t1-t0)*1000} ms")

NEQ, N = nml["PARAMS"]["NEQ"], nml["PARAMS"]["N"]
f = FortranFile(output_file,"r")
A = f.read_reals(dtype="float64")
A = np.reshape(A, (NEQ, N),order="F")
    
plt.plot(A[0,:], A[2,:], "-",lw=0.5)
plt.title("LSODA Lorenz ODE System")
plt.xlabel("X")
plt.ylabel("Z")
plt.show()    