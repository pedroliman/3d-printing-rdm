#!/bin/bash

#SBATCH --job-name=msrdm
#SBATCH --account=CONDO
#SBATCH --partition=dis
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --output=hpc_output.txt
#SBATCH --time=40:00:00
#SBATCH --array=0-0

set -eu

mkdir -p $SLURM_SUBMIT_DIR/outputs/

#module load gcc/7.1.0-4bgguyp 
#module load mvapich2/2.3a-avvw4kp 
#. /lcrc/project/EMEWS/bebop/repos/spack/share/spack/setup-env.sh
# r 4.0.0
#spack load /plchfp7

source envs/bebop.env

#
#RESULTS_DIR=$1
#echo "RESULTS DIRECTORY: $RESULTS_DIR"

echo "Task $SLURM_ARRAY_TASK_ID Running"
Rscript R/scripts/01_run_simulations.R