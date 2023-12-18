#!/bin/sh
#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=1:mem=1gb
module load anaconda3/personal
echo "R is about to run"
cp $HOME/pvt23_HPC_2023_main.R $TMPDIR
R --vanilla < $HOME/pvt23_HPC_2023_neutral_cluster.R
mv HPCoutput_* $HOME/output_files/
echo "R has finished running"

