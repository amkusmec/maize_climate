#!/bin/bash

Rscript 04.compensation/06.season_lengths.R -m gfdl-esm4 -s ssp126 &
Rscript 04.compensation/06.season_lengths.R -m gfdl-esm4 -s ssp370 &
Rscript 04.compensation/06.season_lengths.R -m gfdl-esm4 -s ssp585

Rscript 04.compensation/06.season_lengths.R -m ipsl-cm6a-lr -s ssp126 &
Rscript 04.compensation/06.season_lengths.R -m ipsl-cm6a-lr -s ssp370 &
Rscript 04.compensation/06.season_lengths.R -m ipsl-cm6a-lr -s ssp585

Rscript 04.compensation/06.season_lengths.R -m mpi-esm1-2-hr -s ssp126 &
Rscript 04.compensation/06.season_lengths.R -m mpi-esm1-2-hr -s ssp370 &
Rscript 04.compensation/06.season_lengths.R -m mpi-esm1-2-hr -s ssp585

Rscript 04.compensation/06.season_lengths.R -m mri-esm2-0 -s ssp126 &
Rscript 04.compensation/06.season_lengths.R -m mri-esm2-0 -s ssp370 &
Rscript 04.compensation/06.season_lengths.R -m mri-esm2-0 -s ssp585

Rscript 04.compensation/06.season_lengths.R -m ukesm1-0-ll -s ssp126 &
Rscript 04.compensation/06.season_lengths.R -m ukesm1-0-ll -s ssp370 &
Rscript 04.compensation/06.season_lengths.R -m ukesm1-0-ll -s ssp585
