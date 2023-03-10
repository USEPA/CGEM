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
mpirun ./CGEM CGEM ./data/Examples/2D_example/GEM_InputFile_2D_example InitialConditions.txt ./NETCDF/2Dexample. 
