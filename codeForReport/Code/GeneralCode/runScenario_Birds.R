runScenario_Birds <- function(simulationName, runConfiguration) {
  simulation <- loadSimulation(filePath = file.path(runConfiguration$modelFolder, runConfiguration$modelFile), loadFromCache = FALSE)
  #Read parameters from the excel file
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
    simulation = simulation, scenario = simulationName, runConfiguration = runConfiguration
  )
  # Run simulation and get the results
  maxTolerance <- 1e-10  
  minTolerance <- 1e-20
  solverTol <-  maxTolerance
  while(solverTol >= minTolerance | is.null(simulationResults)){
  tryCatch(
    {
      simulationResults <- runSimulation(simulation = simulation)    
    },
    error=function(e) {
      simulation$solver$absTol <- solverTol * 1e-1
    })
  
  }
  
  #######Save simulation as xml
  #saveSimulation(simulation, file.path(runConfiguration$outputFolder, paste0(simulationName, ".pkml")))
  
  return(list(
    simulation = simulation, results = simulationResults,
    outputValues = getOutputValues(simulationResults,
                                   quantitiesOrPaths = getAllQuantitiesMatching(enumValues(OutputPaths), simulation)
    )
  ))
}
