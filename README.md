## Phenological adaptation is insufficient to offset climate change-induced yield losses in US maize

### Introduction

This project uses empirical estimates of hybrid maize heat tolerance and climate models to estimate the efficacy of different adaptation strategies on maize yields under climate change.

### Description

Prior to running any of the scripts in the repository, please run `00.install_packages.R` to install all R packages required to reproduce these analyses. The necessary directory structure has been preserved through the use of `.gitignore` files or is constructed by the scripts.

This repository requires the reconstruction of historical climate data in the US Midwest and modeling of historical maize hybrid temperature responses. Historical yield data can be found [here](https://doi.org/10.25380/iastate.21965093); code to reconstruct climate data, [here](https://github.com/amkusmec/heat_selection/tree/main/02.munge_weather); and code to model temperature responses, [here](https://github.com/amkusmec/heat_selection/tree/main/03.model_temp). Detailed descriptions of data sources and methods can be found in [this paper](https://doi.org/doi:10.1371/journal.pgen.1010799).

1. **process_gcms**: These scripts download and process data from five global climate models (GCMs) and three SSP-RCPs provided by the Inter-Sectoral Impact Model Inter-comparison Project (ISIMIP).
2. **phenology**: These scripts analyze trends in maize phenology, growing season, and thermal time accumulation.
3. **avoidance**: These scripts simulate maize yields with earlier planting dates but historical maturities.
4. **compensation**: These scripts simulate maize yields with earlier planting dates and later maturities.
5. **optimal**: These scripts simulate maize yields with crop cycles (planting to maturity) designed to maximize time to maturity while avoiding historically stressful periods.
6. **reference**: These scripts simulate unadapted maize yields using historical planting dates and maturities.
7. **summaries**: These scripts summarize yield simulations and produce main text and supplementary figures.

### License

This repository is free and open source for use in research and is licensed under the terms of [GNU GPLv3](https://choosealicense.com/licenses/gpl-3.0/#).
