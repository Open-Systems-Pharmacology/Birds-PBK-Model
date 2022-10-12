#' @title ProjectConfiguration
#' @docType class
#' @description An object storing configuration used project-wide
#' @format NULL
ProjectConfiguration <- R6::R6Class(
  "ProjectConfiguration",
  inherit = ospsuite:::Printable,
  cloneable = FALSE,
  active = list(),
  private = list(),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @return A new `ProjectConfiguration` object.
    initialize = function() {
    },

    #' @field modelFolder Path to the folder where the model file is located.
    modelFolder = file.path(getwd(), "../Models/Simulations"),
    #' @field modelFile Name of the simulation pkml file
    modelFile = "Birds_RR.pkml",
    #' @field paramsFolder Path to the folder where the parameters files are located
    paramsFolder = file.path(getwd(), "../Parameters"),
    #' @field paramsFile Name of the parameters excel file
    paramsFile = "Model_parameters.xlsx",
    #' @field populationParamsFile Name of the excel file with population parameters
    populationParamsFile = "",
    #' @field dataFolder Path to the folder where experimental data files are located
    dataFolder = file.path(getwd(), "../Data"),
    #' @field dataFile Name of the excel file with experimental data
    dataFile = "PBTKBirds_TimeValuesData.xlsx",
    #' @field compoundPropertiesFile Name of the excel file with compound properties
    compoundPropertiesFile = "Compound Properties_Birds.xlsx",
    #' @field outputFolder Path to the folder where outputs (e.g. figures or tables) are stored
    outputFolder = file.path(getwd(), "../Results"),

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Model folder", self$modelFolder)
      private$printLine("Model file name", self$modelFile)
      private$printLine("Parameters folder", self$paramsFolder)
      private$printLine("Parameters file name", self$paramsFile)
      private$printLine("Compond properties file name", self$compoundPropertiesFile)
      private$printLine("Experimental data folder", self$dataFolder)
      private$printLine("Experimental data file", self$dataFile)
      private$printLine("Output folder", self$outputFolder)
      invisible(self)
    }
  )
)
