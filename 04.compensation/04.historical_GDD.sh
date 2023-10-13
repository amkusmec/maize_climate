#!/bin/bash

Rscript 04.compensation/03.historical_GDD.R -m gfdl-esm4 > logs/gfdl-esm4_gdd.log &
Rscript 04.compensation/03.historical_GDD.R -m ipsl-cm6a-lr > logs/ipsl-cm6a-lr_gdd.log &
Rscript 04.compensation/03.historical_GDD.R -m mpi-esm1-2-hr > logs/mpi-esm1-2-hr_gdd.log &
Rscript 04.compensation/03.historical_GDD.R -m mri-esm2-0 > logs/mri-esm2-0_gdd.log &
Rscript 04.compensation/03.historical_GDD.R -m ukesm1-0-ll > logs/ukesm1-0-ll_gdd.log &
