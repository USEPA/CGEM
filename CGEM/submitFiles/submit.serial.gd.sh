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
./CGEM WQEM WQEM_InputFile InitialConditions.txt 
