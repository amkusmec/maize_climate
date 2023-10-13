#!/bin/bash

Rscript 01.process_gcms/03.process_isimip3a.R -m gfdl-esm4 -s historical
Rscript 01.process_gcms/03.process_isimip3a.R -m gfdl-esm4 -s ssp126
Rscript 01.process_gcms/03.process_isimip3a.R -m gfdl-esm4 -s ssp370
Rscript 01.process_gcms/03.process_isimip3a.R -m gfdl-esm4 -s ssp585

Rscript 01.process_gcms/03.process_isimip3a.R -m ipsl-cm6a-lr -s historical
Rscript 01.process_gcms/03.process_isimip3a.R -m ipsl-cm6a-lr -s ssp126
Rscript 01.process_gcms/03.process_isimip3a.R -m ipsl-cm6a-lr -s ssp370
Rscript 01.process_gcms/03.process_isimip3a.R -m ipsl-cm6a-lr -s ssp585

Rscript 01.process_gcms/03.process_isimip3a.R -m mpi-esm1-2-hr -s historical
Rscript 01.process_gcms/03.process_isimip3a.R -m mpi-esm1-2-hr -s ssp126
Rscript 01.process_gcms/03.process_isimip3a.R -m mpi-esm1-2-hr -s ssp370
Rscript 01.process_gcms/03.process_isimip3a.R -m mpi-esm1-2-hr -s ssp585

Rscript 01.process_gcms/03.process_isimip3a.R -m mri-esm2-0 -s historical
Rscript 01.process_gcms/03.process_isimip3a.R -m mri-esm2-0 -s ssp126
Rscript 01.process_gcms/03.process_isimip3a.R -m mri-esm2-0 -s ssp370
Rscript 01.process_gcms/03.process_isimip3a.R -m mri-esm2-0 -s ssp585

Rscript 01.process_gcms/03.process_isimip3a.R -m ukesm1-0-ll -s historical
Rscript 01.process_gcms/03.process_isimip3a.R -m ukesm1-0-ll -s ssp126
Rscript 01.process_gcms/03.process_isimip3a.R -m ukesm1-0-ll -s ssp370
Rscript 01.process_gcms/03.process_isimip3a.R -m ukesm1-0-ll -s ssp585
