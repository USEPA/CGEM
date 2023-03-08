#!/bin/csh
#SBATCH -J CGEM
#SBATCH -t 0:10:00
#SBATCH -N 1 
#SBATCH -n 1
#SBATCH --gid=glfbreez
#SBATCH -A glfbreez
#SBATCH --partition=singlepe
#SBATCH --output=logfile%j.log
source modules_intel.sh
./CGEM CGEM ./data/Examples/0D_example/GEM_InputFile_0D_example InitialConditions.txt ./NETCDF/0Dexample. 
