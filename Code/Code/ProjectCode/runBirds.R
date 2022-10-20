runBirds <-  function(runConfiguration, rerunSimulation = FALSE, compound, run = NULL, models = NULL, returnEggs = FALSE) {
  switch (compound,
          Chloramphenicol = {
            scenarioName = "Chloramphenicol"
            simulationNames <- c(
              # "CAP_Atef1991_IV_20mgkg",
              "CAP_Dein1980_IV_22mgkg",
              "CAP_Anadon1994_IV_30mgkg",
              # "CAP_Atef1991_IC_20mgkg",
              "CAP_Dein1980_IC_55mgkg",
              "CAP_Anadon1994_IC_30mgkg",
              "CAP_Anadon1994_IC_50mgkg",
              "CAP_Akhtar_IC_100mg",
              "CAP_Akhtar1996_5mgkg_5d",
              "CAP_Anadon1994_50mgkg_4d"
            )
            speciesInScenario <- c(
              # "chicken",
              "duck",
              "malechicken",
              # "chicken",
              "duck",
              "malechicken",
              "malechicken",
              "chicken",
              "chicken",
              "malechicken"
            )
            obsDataSheets <- c(
              "CAP_Anadon1994",
              "CAP_Anadon1994_Tissue",
              "CAP_Akhtar1996_Yolk",
              "CAP_Akhtar1996_albumen",
              "CAP_Akhtar1996",
              "CAP_Atef1991",
              "CAP_Dein1980"
            )
            columnsToSplitBy <- c("Group Id", "Gender", "Patient Id", "Dose", "Route", "Molecule", "Organ", "Compartment") 
            
           plotCompound <- "plotCAP"
          },
          Deltamethrin = {
            scenarioName = "Deltamethrin"
            simulationNames <- c(
              "DTM_Hueyuek2017_IV_075mgkg",
              "DTM_M132448_01_IV_04mgkg",
              "DTM_Hueyuek2017_IC_075mgkg",
              "DTM_MacLachlan2008_2mg_28d"
            )
            speciesInScenario <- c("chicken",
                                   "chicken",
                                   "chicken",
                                   "chicken")
            obsDataSheets <-           c(
              "DTM_Hueyuek2017",
              "DTM_M-132448-01-1",
              "DTM_McLachlan2008_Fig2"
            )
            columnsToSplitBy <- c("Group Id", "Gender", "Patient Id", "Dose", "Route", "Molecule", "Organ", "Compartment")
            plotCompound <- "plotDTM"
          },
          Florfenicol = {
            scenarioName <- 'Florfenicol'
            simulationNames <- c(
              "FFC_Liu2018_IV_10mgkg",
              "FFC_Anadon2008_IV_20mgkg",
              "FFC_Shen2003_IV_15mgkg",
              "FFC_Ismail2009_IV_30mgkg",
              "FFC_Ismail2009_IV_30mgkg_quail",
              "FFC_Shen2003_IV_30mgkg",
              "FFC_Liu2018_IC_20mgkg",
              "FFC_Shen2003_IC_30mgkg",
              "FFC_Shen2003_IC_15mgkg",
              "FFC_Chang2010_IC_30mgkg",
              "FFC_Filazi2014_20mgkg_5d",
              "FFC_Filazi2014_20mgkg_3d",
              "FFC_Afifi1997_30mgkg_5d",
              "FFC_Tikhomirov2019_IC_30mgkg",
              "FFC_Tikhomirov2019_IV_30mgkg",
              "FFC_Koc2009_IV_30mgkg",
              "FFC_ElBanna1998_IV_30mgkg",
              "FFC_Anadon2008_IC_20mgkg",
              "FFC_Anadon2008_40mgkg_3d"
            )
            
            speciesInScenario <- c(
              "chicken",
              "chicken",
              "chicken",
              "chicken",
              "quail",
              "chicken",
              "chicken",
              "chicken",
              "chicken",
              "chicken",
              "chicken",
              "chicken",
              "chicken",
              "duck",
              "duck",
              "quail",
              "duck",
              "chicken",
              "chicken"
            )
            obsDataSheets <-  c(
              "FFC_Afifi1997",
              "FFC_Anadon2008_corrected",
              "FFC_Anadon_multPO",
              "FFC_Chang2010",
              "FFC_Chang2010_Tissue",
              "FFC_ElBanna1998",
              "FFC_Filazi2014_WholeEgg",
              "FFC_Filazi2014_Albumen",
              "FFC_Filazi2014_Yolk",
              "FFC_Ismail2009",
              "FFC_Liu2018_Fig5",
              "FFC_Shen2003",
              "FFC_Koc2009",
              "FFC_Tikhomirov2019"
            )
            columnsToSplitBy <-
              c(
                "Group Id",
                "Gender",
                "Patient Id",
                "Dose",
                "Route",
                "Molecule",
                "Organ",
                "Compartment"
              )
            plotCompound <- "plotFFC"
            
          },
          Itraconazole = {
            scenarioName <- "Itraconazole"
            simulationNames <- c(
              "ITZ_Tell2005_IC_20mgkg"
            )
            
            speciesInScenario <- c("duck")
            obsDataSheets <- c("ITZ_Tell2005")
            columnsToSplitBy <- c("Group Id","Species", "Gender", "Patient Id", "Dose", "Route", "Molecule", "Organ", "Compartment") 
            plotCompound <- "plotITZ"
            
          },
          Monensin = {
            scenarioName <- "Monensin"
            simulationNames <- c(
              "MON_Henri2012_IV_046mgkg",
              "MON_Atef1993_IV_40mgkg",
              "MON_Henri2012_IC_4mgkg",
              "MON_Atef1993_IC_40mgkg",
              "MON_Atef1993_24mg_14d",
              "MON_Henri2012_25mg_33d",
              "MON_Vandenberge2012_1o75mg_14d"
            )
            
            speciesInScenario <- c(
              "chicken",
              "chicken",
              "chicken",
              "chicken",
              "chicken",
              "chicken",
              "chicken"
            )
            obsDataSheets <- c(
              "MON_Atef1993_Fig",
              "MON_Atef1993_Table1",
              "MON_Atef1993_Table5",
              "MON_Atef1993_Table6",
              "MON_Henri2009_import",
              "MON_Henri2009_Table3_import",
              "MON_Vandenberge2012_Fig3",
              "MON_Vandenberge2012_Fig5"
            )
            columnsToSplitBy <- c("Group Id", "Gender", "Patient Id", "Dose", "Route", "Molecule", "Organ", "Compartment") 
            plotCompound <- "plotMON"
            
          },
          Melamine = {
            scenarioName <- "Melamine"
            simulationNames <- c(
              "MEL_Poapolathep2015_IV_5o5mgkg",
              "MEL_Suknikom2016_IV_5o5mgkg",
              "MEL_Poapolathep2015_IC_5o5mgkg",
              "MEL_Suknikom2016_IC_5o5mgkg",
              "MEL_Zhang2012_08mg_30d",
              "MEL_Zhang2012_1o6mg_30d",
              "MEL_Dong2010_100mg_21d",
              "MEL_Dong2010_50mg_21d",
              "MEL_Gao2010_18mg_21d",
              "MEL_Gao2010_9mg_21d",
              "MEL_Bai_140mg_34d",
              "MEL_Bai_62mg_34d",
              "MEL_Bai_33mg_34d",
              "MEL_Bai_17mg_34d",
              "MEL_Bai_8mg_34d"
            )
            speciesInScenario <- c(
              "chicken",
              "duck",
              "chicken",
              "duck",
              "quail",
              "quail",
              "chicken",
              "chicken",
              "duck",
              "duck",
              "chicken",
              "chicken",
              "chicken",
              "chicken",
              "chicken"
            )
            obsDataSheets <- c(
              "MEL_Dong2010_Fig2",
              "MEL_Dong2010_eggs",
              "MEL_Gao2010",
              "MEL_Poapolathep2015",
              "MEL_Suknikom2016",
              "MEL_Zhang2012",
              "MEL_Bai2010_Tissues",
              "MEL_Bai2010_Eggs"
            )
            columnsToSplitBy <- c("Group Id", "Gender", "Patient Id", "Dose", "Route", "Molecule", "Organ", "Compartment") 
            plotCompound <- "plotMEL"
            
          },
          Midazolam = {
            scenarioName <- "Midazolam"
            simulationNames <- c(
              "MDZ_Cortright2007_IV_05mgkg_chicken",
              "MDZ_Cortright2007_IV_05mgkg_quail"
            )
            speciesInScenario <- c("duck",
                                   "quail")
            obsDataSheets <-  c(
              "MDZ_Cortright2007_Fig1"
            )
            columnsToSplitBy <- c("Group Id","Species", "Gender", "Patient Id", "Dose", "Route", "Molecule", "Organ", "Compartment") 
            plotCompound <- "plotMDZ"
          },
          Ivermectin = {
            scenarioName <- "Ivermectin"
            simulationNames <- c(
              "IVM_Cirak2018_IV_02mgkg",
              "IVM_Cirak2018_IC_02mgkg",
              "IVM_Moreno2015_IV_04mgkg",
              "IVM_Moreno2015_04mgkg_5d"
            )
            speciesInScenario <- c("chicken",
                                   "chicken",
                                   "chicken",
                                   "chicken")
            obsDataSheets <-  c(
              "IVM_Cirak2018",
              "IVM_Moreno2015_Fig1_import",
              "IVM_Moreno2015_Fig2_Egg",
              "IVM_Moreno2015_TableTissue"
            )
            columnsToSplitBy <- c("Group Id", "Gender", "Patient Id", "Dose", "Route", "Molecule", "Organ", "Compartment")
            plotCompound <- "plotIVM"
          },
          Salinomycin = {
            scenarioName <- "Salinomycin"
            simulationNames <- c(
              "SAL_Henri2012_IV_025mgkg",
              "SAL_Henri2012_IC_25mgkg",
              "SAL_Henri2012_10mg_35d",
              "SAL_Atef1993_IV_20mgkg",
              "SAL_Atef1993_IC_20mgkg",
              "SAL_Atef1993_6mg_14d"
            )
            speciesInScenario <- c("chicken",
                                   "chicken",
                                   "chicken",
                                   "chicken",
                                   "chicken",
                                   "chicken")
            obsDataSheets <- c("SAL_Henri2012",
                              "SAL_Henri2012_Feed",
                              "SAL_Atef1993")
            columnsToSplitBy <-
              c(
                "Group Id",
                "Gender",
                "Patient Id",
                "Dose",
                "Route",
                "Molecule",
                "Organ",
                "Compartment"
              )
        plotCompound <- "plotSAL"
        },
  {stop("define a valid compound!")}
  )

  # if no run is defined, the unfitted and fitted parametrisations are run 
  if(!is.null(run)){
    if(run == "fitted"){
      compound <- paste0(compound," ", run)
    }
  } else{
    compound <- c( compound,
                   paste0(compound, "")
    )
  }
  
  # if no model for PC is defined, all methods are run. Note that this makes only sense for the unfitted parametrisation 
  if(!is.null(models)){
    models <-  models
  } else{
    models <- c("PKSim",
                "WS",
                "RR",
                "PT",
                "Berezh",
                "pplfer"
    )
  }
  for (c in compound) {
    for(m in models){
      compoundParams <- c
      cmpOutput <- file.path(runConfiguration$outputFolder, c, m)
      
      runConfiguration <- runConfiguration$clone()
      runConfiguration$modelFile <- paste0('Birds_', m, ".pkml")
      runConfiguration$scenarioName <- scenarioName
      runConfiguration$simulationTime <- 24*60
      
      # ---- load observed data ----
      dataConfiguration <-
        DataConfiguration$new(
          runConfiguration$dataFolder,
          runConfiguration$dataFile,
          runConfiguration$compoundPropertiesFile,
          obsDataSheets
        )
      
      dataConfiguration$columnsToSplitBy <- columnsToSplitBy
      dataConfiguration$XValuesColumn <- 10
      dataConfiguration$YValuesColumn <- 11
      dataConfiguration$YErrorColumn <- 12
      observedData <- readOSPSTimeValues(dataConfiguration)
      for(o in observedData){ #workaround
        tmp <- unlist(o)
        for (d in tmp) {
          d$dataType <-  XYDataTypes$Observed
        }
      }
      
      # ---- set up and run the simulations or load the results from xls---- 
    

      
      simulatedScenarios <- list()
      for (simName in seq_along(simulationNames)) {
        if(rerunSimulation){
          simulatedScenarios[[simulationNames[[simName]]]] <-
            runScenario_BirdSpecies(simulationNames[[simName]],
                                    runConfiguration,
                                    speciesInScenario[[simName]],
                                    compoundParams,
                                    model = m,
                                    resultsFolder = cmpOutput )
          exportResultsToCSV(results = simulatedScenarios[[simulationNames[[simName]]]]$results, 
                             filePath = file.path(cmpOutput, paste0(simulationNames[[simName]],".csv")))
        } else {
          referenceSim <-
            loadSimulation(
              filePath = file.path( 
                file.path(cmpOutput, paste0(simulationNames[[simName]], ".pkml"))
              ),
              loadFromCache = FALSE
              )
          simulationResults <- importResultsFromCSV(simulation = referenceSim, filePaths = file.path(cmpOutput, paste0(simulationNames[[simName]],".csv")))
          outputValues  <-  getOutputValues(simulationResults, quantitiesOrPaths = getAllQuantitiesMatching(enumValues(OutputPaths), referenceSim))
          simulatedScenarios[[simulationNames[[simName]]]] <- list (simulation = referenceSim,
                                                                    results = simulationResults,
                                                                    outputValues = outputValues)
        }
      }
      
      # ---- define plots ----

      plotConfiguration <- PlotConfiguration$new()
      plotConfiguration$outputDevice <- runConfiguration$outputDevice
      plotConfiguration$outputFolder <- cmpOutput
      plotConfiguration$pointsize <- 22 #28
      plotConfiguration$res <- 600 #100
      plotConfiguration$width <- 21
      plotConfiguration$height <- 29
      # par(lwd = 2)
      plotConfiguration$addTitle <-  FALSE
      dataMappings <- get(plotCompound)(plotConfiguration, simulatedScenarios, observedData, compound, c, m, simulationNames)
      if(returnEggs){
        return (dataMappings)
      }
    }
  }
}

