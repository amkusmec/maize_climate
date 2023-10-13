#!/bin/bash

Rscript 03.avoidance/03.sim_planting_dates.R -m gfdl-esm4 -s historical
Rscript 03.avoidance/03.sim_planting_dates.R -m gfdl-esm4 -s ssp126
Rscript 03.avoidance/03.sim_planting_dates.R -m gfdl-esm4 -s ssp370
Rscript 03.avoidance/03.sim_planting_dates.R -m gfdl-esm4 -s ssp585

Rscript 03.avoidance/03.sim_planting_dates.R -m ipsl-cm6a-lr -s historical
Rscript 03.avoidance/03.sim_planting_dates.R -m ipsl-cm6a-lr -s ssp126
Rscript 03.avoidance/03.sim_planting_dates.R -m ipsl-cm6a-lr -s ssp370
Rscript 03.avoidance/03.sim_planting_dates.R -m ipsl-cm6a-lr -s ssp585

Rscript 03.avoidance/03.sim_planting_dates.R -m mpi-esm1-2-hr -s historical
Rscript 03.avoidance/03.sim_planting_dates.R -m mpi-esm1-2-hr -s ssp126
Rscript 03.avoidance/03.sim_planting_dates.R -m mpi-esm1-2-hr -s ssp370
Rscript 03.avoidance/03.sim_planting_dates.R -m mpi-esm1-2-hr -s ssp585

Rscript 03.avoidance/03.sim_planting_dates.R -m mri-esm2-0 -s historical
Rscript 03.avoidance/03.sim_planting_dates.R -m mri-esm2-0 -s ssp126
Rscript 03.avoidance/03.sim_planting_dates.R -m mri-esm2-0 -s ssp370
Rscript 03.avoidance/03.sim_planting_dates.R -m mri-esm2-0 -s ssp585

Rscript 03.avoidance/03.sim_planting_dates.R -m ukesm1-0-ll -s historical
Rscript 03.avoidance/03.sim_planting_dates.R -m ukesm1-0-ll -s ssp126
Rscript 03.avoidance/03.sim_planting_dates.R -m ukesm1-0-ll -s ssp370
Rscript 03.avoidance/03.sim_planting_dates.R -m ukesm1-0-ll -s ssp585
