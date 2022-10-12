#
# use renv with R version 4.0.5 
#

# ---- load libraries ----
library(esqlabsR)
library(ospsuite.utils)
library(pracma)
library(scales)
library(data.table)
library(ggplot2)

# ---- source local code ----
sourceAll(file.path(getwd(), "GeneralCode"))
sourceAll(file.path(getwd(), "ProjectCode"))
sourceAll(file.path(getwd(), "utils")) 

# ---- INPUTS ----
rerunSimulations <-  1  # set TRUE if you wish to recalculate all the simulations)
runs <- "fitted"            # set to NULL if unfitted and fitted should be evaluated
plotEggs <-  TRUE           # set TRUE if you wish to generate the egg plot of all compounds


# ---- run the simulations and generate plots ----
projectConfiguration <- ProjectConfiguration$new()
runConfiguration <- RunConfiguration$new(projectConfiguration)
runConfiguration$setTestParameters <- TRUE
runConfiguration$outputDevice <- "PNG"


dataMappingsEggs <- list()
dataMappingsEggsMel <-  list()

dataMappingsEggs[[1]] <- runBirds(compound = "Chloramphenicol", 
                                  runConfiguration = runConfiguration, 
                                  rerunSimulation = rerunSimulations, 
                                  run = runs, 
                                  models =  'PKSim', 
                                  returnEggs = plotEggs)

dataMappingsEggs[[2]] <- runBirds(compound = "Deltamethrin", 
                                  runConfiguration = runConfiguration, 
                                  rerunSimulation = rerunSimulations, 
                                  run = runs, 
                                  models =  'RR', 
                                  returnEggs = plotEggs)

dataMappingsEggs[[3]] <- runBirds(compound = "Florfenicol", 
                                  runConfiguration = runConfiguration,
                                  rerunSimulation = rerunSimulations, 
                                  run = runs,
                                  models =  'PKSim', 
                                  returnEggs = plotEggs)

dataMappingsEggs[[4]] <- runBirds(compound = "Ivermectin", 
                                  runConfiguration = runConfiguration, 
                                  rerunSimulation = rerunSimulations, 
                                  run = runs, 
                                  models =  'WS', 
                                  returnEggs = plotEggs)
dataMappingsEggs[[6]] <- runBirds(compound = "Monensin", 
                                  runConfiguration = runConfiguration, 
                                  rerunSimulation = rerunSimulations, 
                                  run = runs, 
                                  models =  'RR', 
                                  returnEggs = plotEggs)
melEggs <- runBirds(compound = "Melamine", 
                               runConfiguration = runConfiguration, 
                               rerunSimulation = TRUE,#rerunSimulations, 
                               run = runs, 
                               models =  'RR', 
                               returnEggs = plotEggs)

runBirds(compound = "Midazolam", 
         runConfiguration = runConfiguration, 
         rerunSimulation = rerunSimulations, 
         run = runs, 
         models =  'RR')
runBirds(compound = "Salinomycin", 
         runConfiguration = runConfiguration, 
         rerunSimulation = rerunSimulations, 
         run = runs, 
         models =  'RR')
runBirds(compound = "Itraconazole", 
         runConfiguration = runConfiguration, 
         rerunSimulation = rerunSimulations, 
         run = runs, 
         models =  'RR')


# ---- plot eggs ----
if(plotEggs){
dataMappingsEggs[[5]] <- melEggs$chicken
dataMappingsEggsMel[[2]] <- melEggs$quail
dataMappingsEggsMel[[1]] <- melEggs$duck

lapply(dataMappingsEggs, function(x) {x$legendPosition <-  "bottomleft"})
lapply(dataMappingsEggsMel, function(x) {x$legendPosition <-  "bottomleft"})
dataMappingsEggs[[4]]$legendPosition <- "topright"
       
plotConfiguration <- PlotConfiguration$new()
plotConfiguration$outputDevice <- runConfiguration$outputDevice
plotConfiguration$outputFolder <- runConfiguration$outputFolder
plotConfiguration$addTitle <- FALSE
plotConfiguration$nrOfCols <-3
plotConfiguration$outputName <- "Eggs - all compounds (chicken)"
plotMultiPanel(dataMappingsEggs,
               plotConfiguration = plotConfiguration )
plotConfiguration$nrOfCols <-2
plotConfiguration$outputName <- "Eggs - Melamine (duck & quail)"
plotMultiPanel(dataMappingsEggsMel,
               plotConfiguration = plotConfiguration )
}
