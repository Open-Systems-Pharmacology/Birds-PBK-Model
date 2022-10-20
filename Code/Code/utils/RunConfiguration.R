#' @title RunConfiguration
#' @docType class
#' @description An object storing configuration of a specific run scenario
#' @format NULL
RunConfiguration <- R6::R6Class(
  "RunConfiguration",
  inherit = ospsuite:::Printable,
  cloneable = FALSE,
  active = list(
    #' @field setTestParameters Boolean representing whether parameters defined in \code{TestParameters} are to be applied
    #' to the simulation
    setTestParameters = function(value) {
      if (missing(value)) {
        private$.setTestParameters
      } else {
        ospsuite:::validateIsLogical(value)
        private$.setTestParameters <- value
      }
    },
    #' @field simulateSteadyState Boolean representing whether the simulation will be brought to a steady-state first
    simulateSteadyState = function(value) {
      if (missing(value)) {
        private$.simulateSteadyState
      } else {
        ospsuite:::validateIsLogical(value)
        private$.simulateSteadyState <- value
      }
    },
    #' @field simulationTime Boolean representing whether the simulation will be brought to a steady-state first
    simulationTime = function(value) {
      if (missing(value)) {
        private$.simulationTime
      } else {
        ospsuite:::validateIsNumeric(value)
        if (value < 0) {
          stop(paste0("simulationTime must be a positive numerical value, but the value is ", value))
        }
        private$.simulationTime <- value
      }
    },
    #' @field pointsPerMinute Resolution of the outputs in points per minute
    pointsPerMinute = function(value) {
      if (missing(value)) {
        private$.pointsPerMinute
      } else {
        ospsuite:::validateIsNumeric(value)
        if (value < 0) {
          stop(paste0("pointsPerMinute must be a positive numerical value, but the value is ", value))
        }
        private$.pointsPerMinute <- value
      }
    },
    #' @field steadyStateTime Time in minutes to simulate if simulating steady-state. May be NULL
    steadyStateTime = function(value) {
      if (missing(value)) {
        private$.steadyStateTime
      } else {
        ospsuite:::validateIsNumeric(value)
        if (value < 0) {
          stop(paste0("steadyStateTime must be a positive numerical value, but the value is ", value))
        }
        private$.steadyStateTime <- value
      }
    },
    #' @field individualCharacteristics Object of the class \code{IndividualCharacteristics} describing the
    #' simulated individual. If NULL (default), the individual as defined in the simulation file will be simulated.
    individualCharacteristics = function(value) {
      if (missing(value)) {
        private$.individualCharacteristics
      } else {
        ospsuite:::validateIsOfType(value, IndividualCharacteristics, nullAllowed = T)
        private$.individualCharacteristics <- value
      }
    },
    #' @field paramSheets Names of the sheets from the parameters-excel file that will be applied to the simulation
    paramSheets = function(value) {
      if (missing(value)) {
        private$.paramSheets
      } else {
        stop("Field paramSheets is read-only! Use functions 'addParamSheet' and 'removeParamSheet' to add or remove
a parameter sheet from the list")
      }
    },
    #' @field simulationType Type of the simulation - "Individual" or "Population". If "Population", population characteristics
    #' are created based on information stored in \code{populationParamsFile}.
    #' Default is "Individual"
    simulationType = function(value) {
      if (missing(value)) {
        private$.simulationType
      } else {
        if (value %in% c("Individual", "Population")){
          private$.simulationType <- value
        }
        else{
          stop("Wrong value for 'simulationType'! Accepted values are 'Individual
                and 'Population'")
        }
      }
    },
    #' @field simulationRunOptions Object of type \code{SimulationRunOptions} that will be passed
    #' to simulation runs. IF \code{NULL}, default options are used
    simulationRunOptions = function(value) {
      if (missing(value)) {
        private$.simulationRunOptions
      } else {
        ospsuite:::validateIsOfType(value, SimulationRunOptions, nullAllowed = T)
        private$.simulationRunOptions <- value
      }
    }
  ),
  private = list(
    .setTestParameters = NULL,
    .simulateSteadyState = NULL,
    .simulationTime = NULL,
    .pointsPerMinute = 1,
    .steadyStateTime = NULL,
    .individualCharacteristics = NULL,
    .paramSheets = NULL,
    .simulationType = "Individual",
    .simulationRunOptions = NULL
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param projectConfiguration An object of class \code{ProjectConfiguration}.
    #' @param outputDevice Output target of the plot. If NULL (default), the figure is created in the default "plot"
    #' output. Other values indicate output into a file. A list of supported outputs is provided in \code{GraphicsDevices}-enum.
    #' @param setTestParameters Boolean representing whether parameters defined in \code{TestParameters} are to be applied
    #' to the simulation. FALSE by default.
    #' @param simulateSteadyState Boolean representing whether the simulation will be brought to a steady-state first. FALSE by default
    #' @return A new `RunConfiguration` object.
    initialize = function(projectConfiguration, outputDevice = NULL, setTestParameters = FALSE, simulateSteadyState = FALSE) {
      self$modelFolder <- projectConfiguration$modelFolder
      self$modelFile <- projectConfiguration$modelFile
      self$paramsFolder <- projectConfiguration$paramsFolder
      self$paramsFile <- projectConfiguration$paramsFile
      self$compoundPropertiesFile <- projectConfiguration$compoundPropertiesFile
      self$populationParamsFile <- projectConfiguration$populationParamsFile
      self$dataFolder <- projectConfiguration$dataFolder
      self$dataFile <- projectConfiguration$dataFile
      self$outputFolder <- projectConfiguration$outputFolder
      self$setTestParameters <- setTestParameters
      self$outputDevice <- outputDevice
      self$simulateSteadyState <- simulateSteadyState
      private$.paramSheets <- enum(NULL)
    },

    #' @field modelFolder Path to the folder where the model file is located.
    modelFolder = NULL,
    #' @field modelFile Name of the simulation pkml file
    modelFile = NULL,
    #' @field paramsFolder Path to the folder where the parameters files are located
    paramsFolder = NULL,
    #' @field paramsFile Name of the parameters excel file
    paramsFile = NULL,
    #' @field compoundPropertiesFile Name of the excel file with compound properties
    compoundPropertiesFile = "Compound Properties.xlsx",
    #' @field populationParamsFile Name of the excel file with population parameters
    populationParamsFile = "PopulationParameters",
    #' @field dataFolder Path to the folder where experimental data files are located
    dataFolder = NULL,
    #' @field dataFile Name of the excel file with experimental data
    dataFile = NULL,
    #' @field outputFolder Path to the folder where outputs (e.g. figures or tables) are stored
    outputFolder = NULL,
    #' @field scenarioName Name of the simulated scenario
    scenarioName = NULL,
    #' @field outputDevice Output target of the plot. If NULL (default), the figure is created in the default "plot"
    #' output. Other values indicate output into a file. A list of supported outputs is provided in \code{GraphicsDevices}-enum.
    outputDevice = NULL,

    #' @description Add the names of sheets in the parameters excel-file
    #' that will be applied to the simulation
    #' @param sheetNames A name or a list of names of the excel sheet
    addParamSheets = function(sheetNames) {
      private$.paramSheets <- enumPut(sheetNames, sheetNames, enum = private$.paramSheets, overwrite = TRUE)
    },

    #' @description Remove the names of sheets in the parameters excel-file
    #' from the list of sheets \code{paramSheets}
    #' @param sheetNames A name or a list of names of the excel sheet.
    removeParamSheets = function(sheetNames) {
      private$.paramSheets <- enumRemove(keys = sheetNames, enum = private$.paramSheets)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Scenario name", self$scenarioName)
      private$printLine("Simulation time", self$simulationTime)
      private$printLine("Points per minute", self$pointsPerMinute)
      private$printLine("Model folder", self$modelFolder)
      private$printLine("Model file name", self$modelFile)
      private$printLine("Parameters folder", self$paramsFolder)
      private$printLine("Parameters file name", self$paramsFile)
      private$printLine("Compond properties file name", self$compoundPropertiesFile)
      private$printLine("Parameters sheets", enumKeys(self$paramSheets))
      private$printLine("Experimental data folder", self$dataFolder)
      private$printLine("Experimental data file", self$dataFile)
      private$printLine("Output folder", self$outputFolder)
      private$printLine("Output to PNG", self$outputToPNG)
      private$printLine("Simulate steady-state", self$simulateSteadyState)
      private$printLine("Steady-state time", self$steadyStateTime)
      private$printLine("Set test parameters", self$setTestParameters)
      private$printLine("Individual characteristics", self$individualCharacteristics)
      invisible(self)
    }
  )
)
