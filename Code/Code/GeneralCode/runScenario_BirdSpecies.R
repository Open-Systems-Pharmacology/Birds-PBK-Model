runScenario_BirdSpecies <- function(simulationName, runConfiguration, species, compoundParams, model = NULL, resultsFolder = NULL) {
  simulation <- loadSimulation(filePath = file.path(runConfiguration$modelFolder, runConfiguration$modelFile), loadFromCache = FALSE)
  #Read parameters from the excel file
  oldSheets <- runConfiguration$paramSheets
  runConfiguration$removeParamSheets(oldSheets)
  
  switch(species,
         chicken = {
           runConfiguration$addParamSheets(c("Chicken female"
                                             , "Chicken Eggs"
           ))
         },
         malechicken = {
           runConfiguration$addParamSheets(c("Chicken male"))
         },
         duck = {
           runConfiguration$addParamSheets(c("Mallard female"
                                             ,"import_Duck RPs female Bayer"
                                             ,"import_Mallard_RP_female_Vfs"
                                             ,"Mallard Eggs"))
         },
         duckmale = {
           runConfiguration$addParamSheets(c("Mallard male"
                                             ,"import_Mallard_male_Vfs"))
         },
         quail = {
           runConfiguration$addParamSheets(c("Bobwhite female"
                                             , "import_Quail NRPs female Bayer"
                                             , "Quail Eggs"))
         }
  )
  runConfiguration$addParamSheets(compoundParams)
  params <- readParametersFromXLS(file.path(runConfiguration$paramsFolder, runConfiguration$paramsFile), runConfiguration$paramSheets)
  if (runConfiguration$setTestParameters) {
    params <- getTestParameters(params)
  }
  #Set the "Active"-parameter of all applications to 0. The paths to the parent container of every application in the simulation are listed in the map "Events"
  disableEvents(simulation)
  #initializeSimulation is part of esqlabsR. Applies the parameters from the excel file to the loaded simulation object
  initializeSimulation(simulation, runConfiguration$individualCharacteristics, params, simulateSteadyState = runConfiguration$simulateSteadyState, steadyStateTime = runConfiguration$steadyStateTime)
  
  # Set the outputs
  clearOutputs(simulation)
  #For convenience I define the paths to all outputs in the map OutputPaths
  addOutputs(quantitiesOrPaths = enumValues(OutputPaths), simulation = simulation)
  # Set simulation time
  setOutputInterval(simulation = simulation, startTime = 0, endTime = runConfiguration$simulationTime, resolution = runConfiguration$pointsPerMinute)
  simulation$solver$absTol <- 1e-13
  simulation$solver$relTol <- 1e-05

  
  # Set administration protocol - or, as discussed, "scenario"
  setApplications(
    simulation = simulation, scenario = simulationName, runConfiguration = runConfiguration, model = model, compound = compoundParams
  )()
  # Run simulation and get the results
  maxTolerance <- 1e-9  
  minTolerance <- 1e-20
  solverTol <-  maxTolerance
  runOptions <- SimulationRunOptions$new(checkForNegativeValues = FALSE)
  saveSimulation(simulation, file.path(resultsFolder, paste0(simulationName, ".pkml")))
  
  simulationRun <- list(NULL, solverTol)
  
  while((simulationRun[[2]] > minTolerance && is.null(simulationRun[[1]]))){
    simulationRun <- 
      tryCatch(
      {
        simulation$solver$absTol <- simulationRun[[2]]
        simulation$solver$relTol <- simulationRun[[2]]
        (list(runSimulation(simulation = simulation, simulationRunOptions = runOptions), 
              simulationRun = simulationRun[[2]]
              )) 
      },
      error = function(e) {
        print(e)
        return(list(NULL, simulationRun[[2]]* 1e-1))
      }
    
    )
  } 
  simulationResults <- simulationRun[[1]]
  #######Save simulation as xml
  saveSimulation(simulation, file.path(resultsFolder, paste0(simulationName, ".pkml")))
  
  return(list(
    simulation = simulation, results = simulationResults,
    outputValues = getOutputValues(simulationResults,
                                   quantitiesOrPaths = getAllQuantitiesMatching(enumValues(OutputPaths), simulation)
    )
  ))
}
