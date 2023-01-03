#!/bin/bash

#SBATCH --job-name=msrdm
#SBATCH --account=CONDO
#SBATCH --partition=dis
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --output=hpc_output.txt
#SBATCH --time=40:00:00
#SBATCH --array=0-0


# Selective Openness Code Repository --------------------------------------
# Code Repository for the "Selective Openness" paper
# Copyright (C) 2022 by Pedro Nascimento de Lima
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# Read the README.md file for usage instructions.
# -------------------------------------------------------------------------


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