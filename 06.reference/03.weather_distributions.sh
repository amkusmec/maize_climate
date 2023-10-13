#!/bin/bash

Rscript 06.reference/02.weather_distributions.R -m gfdl-esm4 -s historical > logs/gfdl-esm4_historical_dist.log
Rscript 06.reference/02.weather_distributions.R -m gfdl-esm4 -s ssp126 > logs/gfdl-esm4_ssp126_dist.log
Rscript 06.reference/02.weather_distributions.R -m gfdl-esm4 -s ssp370 > logs/gfdl-esm4_ssp370_dist.log
Rscript 06.reference/02.weather_distributions.R -m gfdl-esm4 -s ssp585 > logs/gfdl-esm4_ssp585_dist.log

Rscript 06.reference/02.weather_distributions.R -m ipsl-cm6a-lr -s historical > logs/ipsl-cm6a-lr_historical_dist.log
Rscript 06.reference/02.weather_distributions.R -m ipsl-cm6a-lr -s ssp126 > logs/ipsl-cm6a-lr_ssp126_dist.log
Rscript 06.reference/02.weather_distributions.R -m ipsl-cm6a-lr -s ssp370 > logs/ipsl-cm6a-lr_ssp370_dist.log
Rscript 06.reference/02.weather_distributions.R -m ipsl-cm6a-lr -s ssp585 > logs/ipsl-cm6a-lr_ssp585_dist.log

Rscript 06.reference/02.weather_distributions.R -m mpi-esm1-2-hr -s historical > logs/mpi-esm1-2-hr_historical_dist.log
Rscript 06.reference/02.weather_distributions.R -m mpi-esm1-2-hr -s ssp126 > logs/mpi-esm1-2-hr_ssp126_dist.log
Rscript 06.reference/02.weather_distributions.R -m mpi-esm1-2-hr -s ssp370 > logs/mpi-esm1-2-hr_ssp370_dist.log
Rscript 06.reference/02.weather_distributions.R -m mpi-esm1-2-hr -s ssp585 > logs/mpi-esm1-2-hr_ssp585_dist.log

Rscript 06.reference/02.weather_distributions.R -m mri-esm2-0 -s historical > logs/mri-esm2-0_historical_dist.log
Rscript 06.reference/02.weather_distributions.R -m mri-esm2-0 -s ssp126 > logs/mri-esm2-0_ssp126_dist.log
Rscript 06.reference/02.weather_distributions.R -m mri-esm2-0 -s ssp370 > logs/mri-esm2-0_ssp370_dist.log
Rscript 06.reference/02.weather_distributions.R -m mri-esm2-0 -s ssp585 > logs/mri-esm2-0_ssp585_dist.log

Rscript 06.reference/02.weather_distributions.R -m ukesm1-0-ll -s historical > logs/ukesm1-0-ll_historical_dist.log
Rscript 06.reference/02.weather_distributions.R -m ukesm1-0-ll -s ssp126 > logs/ukesm1-0-ll_ssp126_dist.log
Rscript 06.reference/02.weather_distributions.R -m ukesm1-0-ll -s ssp370 > logs/ukesm1-0-ll_ssp370_dist.log 
Rscript 06.reference/02.weather_distributions.R -m ukesm1-0-ll -s ssp585 > logs/ukesm1-0-ll_ssp585_dist.log
