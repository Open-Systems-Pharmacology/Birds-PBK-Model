## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----calculatePKAnalyses------------------------------------------------------
library(ospsuite)

# Load  the simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Run the simulation
simulationResults <- runSimulation(simulation = sim)

# Calculate PK-analyses
pkAnalysis <- calculatePKAnalyses(results = simulationResults)

# Get the path of the simulated output
outputPath <- simulationResults$allQuantityPaths[[1]]
print(outputPath)

# Get all calculated PK parameters for the output path
allPkParams <- pkAnalysis$allPKParametersFor(outputPath)
print(allPkParams)

# Get C_max parameter
c_max <- pkAnalysis$pKParameterFor(quantityPath = outputPath, pkParameter = "C_max")

## ----QuantityPKParameter------------------------------------------------------
# Get C_max parameter
c_max <- pkAnalysis$pKParameterFor(quantityPath = outputPath, pkParameter = "C_max")

print(c_max)

c_max$name
c_max$quantityPath
c_max$values

## ----PKAnalysesExport---------------------------------------------------------
# Load and run the simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)
simulationResults <- runSimulation(simulation = sim)

# Calculate PK-analysis
pkAnalysis <- calculatePKAnalyses(results = simulationResults)

# Export to csv
csvPKAnalysisPath <- system.file("extdata", "PKAnalyses.csv", package = "ospsuite")
exportPKAnalysesToCSV(pkAnalyses = pkAnalysis, filePath = csvPKAnalysisPath)

# Load from csv
pkAnalysisLoaded <- importPKAnalysesFromCSV(filePath = csvPKAnalysisPath, simulation = sim)

