#!/bin/csh
#SBATCH -J CGEM
#SBATCH -t 0:10:00
#SBATCH -N 2 
#SBATCH -n 2
#SBATCH --gid=glfbreez
#SBATCH -A glfbreez
#SBATCH --partition=debug
#SBATCH --output=logfile%j.log
source modules_intel.sh
mpirun ./CGEM WQEM ./data/Examples/3D_example/WQEM_InputFile_3D_example InitialConditions_WQEM.txt ./NETCDF/3Dexample.
