# Birds-PBK-Model

Model files and R code of the studies from the published Birds PBK model evaluation.

## Repository Files
The repository contains simulation files with the base birds structure for the different partition coefficient calculation methods. These can be paramterised in R to get the simulation files used for the publication. In order to update simulations of the birds model in MoBi, the AllCalculationMethods.pkml file needs to be replaced.

The folder `Code` contains all scripts to generate the simulations and plots used for the publication. If you want to reproduce the results you need to follow these steps:

- Install R version 4.1.x (for using renv) and RStudio. ( If you want to use a different R version you need to install `renv` from CRAN, run `renv::restore()`, and manually install the packages `rClr`, `ospsuite`, and `esqlabsR` from `Code/packages`)
- Before opening the project make sure you have installed renv from CRAN
- Open the project file `Code/Code/Birds.Rproj`
- Run Birds_main.R to generate the simulations and plots

## Version Information
MoBi®Version 9.1

## License
The model is distributed under the [GPLv2 License](https://github.com/Open-Systems-Pharmacology/Suite/blob/develop/LICENSE).

## References
Vanessa Baier, Alicia Paini, Stephan Schaller, Colin G. Scanes, Audrey J. Bone, Markus Ebeling, Thomas G. Preuss, Johannes Witt, and David Heckmann. 2022. “A Generic Avian Physiologically-Based Kinetic (PBK) Model and Its Application in Three Bird Species.” _Environment International_ 169 (November): 107547. [https://doi.org/10.1016/j.envint.2022.107547](https://doi.org/10.1016/j.envint.2022.107547).