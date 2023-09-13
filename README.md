# Birds-PBK-Model

Model files and R code of the studies from the published Birds PBK model evaluation.

## Repository Files
The repository contains simulation files with the base birds structure for the different partition coefficient calculation methods. These can be paramterised in R to get the simulation files used for the publication. In order to update simulations of the birds model in MoBi, the AllCalculationMethods.pkml file needs to be replaced.

The folder `Code` contains all scripts to generate the simulations and plots used for the publication. If you want to reproduce the results you need to follow these steps:

- Install R version 4.1.x (for using renv) and RStudio. ( If you want to use a different R version you need to install `renv` from CRAN, run `renv::restore()`, and manually install the packages `rClr`, `ospsuite`, and `esqlabsR` from `Code/packages`)
- Before opening the project make sure you have installed renv from CRAN
- Open the project file `Code/Code/Birds.Rproj`
- Run Birds_main.R to generate the simulations and plots

> Note: If you download the Code as a .zip file you might need to enable .dll loading by checking the box "Unblock" under right-click on the .zip file > Properties > Security

## Running the simulations

- The current model version requires MoBi Version 9.1. You can download a portable version [here](https://github.com/Open-Systems-Pharmacology/MoBi/releases/tag/v9.1).
- Within this work, new partitioning coefficients calculation methods have been developed for the Birds PBPK model. These methods are not natively supported by MoBi, and an attempt to create a new simulation with unmodified MoBi will fail with an error "A black box is still in use". You need to download the modified `AllCalculationMethods.pkml` file from this repository and copy it into the location of MoBi 9.1 (typically it will be located under `c:\Program Files\Open Systems Pharmacology\MoBi 9.1.1\`). You might require Administrator rights to do this action.
- With the modified `AllCalculationMethods.pkml` that contains equations for the new partitioning coefficients, you can open the MoBi project and create new simulations.

## Version Information
MoBi®Version 9.1

## License
The model is distributed under the [GPLv2 License](https://github.com/Open-Systems-Pharmacology/Suite/blob/develop/LICENSE).

## References
Vanessa Baier, Alicia Paini, Stephan Schaller, Colin G. Scanes, Audrey J. Bone, Markus Ebeling, Thomas G. Preuss, Johannes Witt, and David Heckmann. 2022. “A Generic Avian Physiologically-Based Kinetic (PBK) Model and Its Application in Three Bird Species.” _Environment International_ 169 (November): 107547. [https://doi.org/10.1016/j.envint.2022.107547](https://doi.org/10.1016/j.envint.2022.107547).
