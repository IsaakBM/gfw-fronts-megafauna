#!/bin/bash
#SBATCH --job-name=agg_gfw
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=15
#SBATCH --time=04:00:00
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=ibrito@eri.ucsb.edu
#SBATCH --output=/home/sandbox-sparc/gfw-fronts-megafauna/logs/agg_gfw_%j.out
#SBATCH --error=/home/sandbox-sparc/gfw-fronts-megafauna/logs/agg_gfw_%j.err
#SBATCH --chdir=/home/sandbox-sparc/gfw-fronts-megafauna   # sets working dir

set -euo pipefail

echo "[SLURM] Host: $(hostname)"
echo "[SLURM] Cores: ${SLURM_CPUS_PER_TASK}"

# Run the R runner (shebang lets us execute it directly)
./scripts/00_run_aggregate_gfw.R