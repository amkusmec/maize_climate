#!/bin/bash

Rscript 05.optimal/01.cropCalendar.R --model gfdl-esm4 --scenario historical
Rscript 05.optimal/01.cropCalendar.R --model gfdl-esm4 --scenario ssp126
Rscript 05.optimal/01.cropCalendar.R --model gfdl-esm4 --scenario ssp370
Rscript 05.optimal/01.cropCalendar.R --model gfdl-esm4 --scenario ssp585

Rscript 05.optimal/01.cropCalendar.R --model ipsl-cm6a-lr --scenario historical
Rscript 05.optimal/01.cropCalendar.R --model ipsl-cm6a-lr --scenario ssp126
Rscript 05.optimal/01.cropCalendar.R --model ipsl-cm6a-lr --scenario ssp370
Rscript 05.optimal/01.cropCalendar.R --model ipsl-cm6a-lr --scenario ssp585

Rscript 05.optimal/01.cropCalendar.R --model mpi-esm1-2-hr --scenario historical
Rscript 05.optimal/01.cropCalendar.R --model mpi-esm1-2-hr --scenario ssp126
Rscript 05.optimal/01.cropCalendar.R --model mpi-esm1-2-hr --scenario ssp370
Rscript 05.optimal/01.cropCalendar.R --model mpi-esm1-2-hr --scenario ssp585

Rscript 05.optimal/01.cropCalendar.R --model mri-esm2-0 --scenario historical
Rscript 05.optimal/01.cropCalendar.R --model mri-esm2-0 --scenario ssp126
Rscript 05.optimal/01.cropCalendar.R --model mri-esm2-0 --scenario ssp370
Rscript 05.optimal/01.cropCalendar.R --model mri-esm2-0 --scenario ssp585

Rscript 05.optimal/01.cropCalendar.R --model ukesm1-0-ll --scenario historical
Rscript 05.optimal/01.cropCalendar.R --model ukesm1-0-ll --scenario ssp126
Rscript 05.optimal/01.cropCalendar.R --model ukesm1-0-ll --scenario ssp370
Rscript 05.optimal/01.cropCalendar.R --model ukesm1-0-ll --scenario ssp585
