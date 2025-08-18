
#!/bin/bash

#PBS -N pecan_depends

#PBS -l walltime=02:00:00

#PBS -l mem=4gb

#PBS -j oe

#PBS -o /projectnb/dietzelab/bthomas/pecan_depends.log



module load R

export R_LIBS_USER=/projectnb/dietzelab/bthomas/Rlibs



cd /projectnb/dietzelab/bthomas/pecan/docker/depends

Rscript pecan.depends.R


