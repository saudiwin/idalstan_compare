#!/bin/bash
### this sets the job name to TestJob
#SBATCH --job-name=idalstan
### -n requests total cores. In this case, we are requesting 2 cores
#SBATCH -n 64
### -N requests nodes. In this case, we are requesting 1 node
#SBATCH -N 1
#SBATCH -p defq-64core
### sends the stdout output of the script to a file appended with the job ID
#SBATCH --output job%j.out
### sends the stderr output of the script to a file appended with the job ID
#SBATCH --error job%j.err
# demonstrates that you can load a module inside a script. in this case, we are loading matlab
module load python3/anaconda/2023.7
source activate r-environment

export FITTYPE=1
export DATATYPE=115
export MISSING=1

Rscript to_cluster.R
