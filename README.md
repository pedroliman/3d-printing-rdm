
# Selective Openness in the Additive Manufacturing Industry: An Exploratory Modeling Analysis

This repository contains code used in the *Selective Openness in the Additive Manufacturing Industry* paper.

All figures and tables from our analysis can be reproduced using publicly available software.

### Dependencies

We used R version 4.2.1 (2022-06-23) and RStudio 2022.07.1 to perform this analysis. Using an up-to-date R installation (i.e., R> 4.0.0), run the `R/scripts/00_install_dependencies.R` script to install required dependencies. Alternatively, open the `3d-printing-rdm.Rproj` file with RStudio and use `renv::restore()` to use the same package versions we used. 

### Reproducing Figures and Tables

Open the `3d-printing-rdm.Rproj` project with RStudio and run the `01_tables_plots.R` script to generate the figures and tables included in our manuscript. Outputs will be created in the `./output` folder.

### Folder structure

- **./R/functions**: contains the functions and classes used in the analysis.
- **./R/scripts**: contains scripts to perform analysis. Scripts can be ran independently, in order.
- **./inputs**: contains all analysis inputs.
- **./outputs**: houses intermediate outputs created by the scripts.

## Contact

Reach out to [Pedro Nascimento de Lima](https://www.pedrodelima.com) for questions related to this repository.

# License

This repository is released as open-source software under a GPL-2.0 license. See the LICENSE.md file. Copyright (c) 2022 by Pedro Nascimento de Lima.