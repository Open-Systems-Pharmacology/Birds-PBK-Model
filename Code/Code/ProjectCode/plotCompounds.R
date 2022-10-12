plotITZ <- function(plotConfiguration, simulatedScenarios, observedData, compound, c, m, simulationNames){

  # ---- 1) ITZ Tell Duck ----
  dataMappingTell <- DataMapping$new()
  currDataMapping <- dataMappingTell
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood,
                 OutputPaths$Brain,
                 OutputPaths$Kidney,
                 OutputPaths$Liver,
                 OutputPaths$Lung,
                 OutputPaths$Muscle
    ),
    simulationResults = simulatedScenarios$ITZ_Tell2005_IC_20mgkg$results,
    labels = list(OutputGroups$VenousBlood,
                  OutputGroups$Brain,
                  OutputGroups$Kidney,
                  OutputGroups$Liver,
                  OutputGroups$Lung,
                  OutputGroups$Muscle
    ),
    groups = list(OutputGroups$VenousBlood,
                  OutputGroups$Brain,
                  OutputGroups$Kidney,
                  OutputGroups$Liver,
                  OutputGroups$Lung,
                  OutputGroups$Muscle
    )
  )
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$ITZ_Tell2005$`ITZ-A`$Duck$mixed$mean$`20mgkg`$IC$Itraconazole$VenousBlood$Plasma,
      observedData$ITZ_Tell2005$`ITZ-A`$Duck$mixed$mean$`20mgkg`$IC$Itraconazole$Brain$Tissue,
      observedData$ITZ_Tell2005$`ITZ-A`$Duck$mixed$mean$`20mgkg`$IC$Itraconazole$Kidney$Tissue,
      observedData$ITZ_Tell2005$`ITZ-A`$Duck$mixed$mean$`20mgkg`$IC$Itraconazole$Liver$Tissue,
      observedData$ITZ_Tell2005$`ITZ-A`$Duck$mixed$mean$`20mgkg`$IC$Itraconazole$Lung$Tissue,
      observedData$ITZ_Tell2005$`ITZ-A`$Duck$mixed$mean$`20mgkg`$IC$Itraconazole$Muscle$Tissue
    ),
    groups = list(OutputGroups$VenousBlood,
                  OutputGroups$Brain,
                  OutputGroups$Kidney,
                  OutputGroups$Liver,
                  OutputGroups$Lung,
                  OutputGroups$Muscle
    )
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Tell 2005 IC 20mg/kg"
  currDataMapping$log = "y"
  currDataMapping$legendPosition <-  "bottomright"

  currDataMapping$plot()
  # ---- 1) ITZ Tell Duck blood----
  
  dataMappingTellbl <- DataMapping$new()
  currDataMapping <- dataMappingTellbl
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$ITZ_Tell2005_IC_20mgkg$results,
    labels = list(OutputGroups$VenousBlood),
    groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$ITZ_Tell2005$`ITZ-A`$Duck$mixed$mean$`20mgkg`$IC$Itraconazole$VenousBlood$Plasma),
    groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Tell 2005 IC 20mg/kg"
  currDataMapping$log = "y"
  currDataMapping$legendPosition <-  "bottomright"

  currDataMapping$plot()
  
  dataMappings <- c(dataMappingTell)
  dataMappingsBl <- c(dataMappingTellbl)
  
  # ---- summary plotting ----
  
  plotConfiguration$outputName <- "ITZ - profiles in ducks"
  plotConfiguration$addTitle <- FALSE
  plotMultiPanel(dataMappings,plotConfiguration)
  
  
  # ---- plotting PVO with ggplot ----
  ggdata <- dataMapping2DT(dataMapping = dataMappings, molecule = "ITZ")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDT.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "ITZ - PvO duck", legPos = "bottom")
  } else{pvo_all <- ggPVO(ggdata, "ITZ - PvO duck", legPos = "bottom")
  ggsave(plot = pvo_all,
         filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
         device = 'png',
         width = 13,
         height = 18, units = "cm", dpi = 300
  )
  }
  
  ## build big mapping with all data
  dataMappingAll <- collapseDataMappings(dataMappings)
  dataMappingAll$log = "y"
  dataMappingAll$legendPosition <- "topleft"
  dataMappingAll$addLegend <-  TRUE
  dataMappingAll$plotType <-  PlotTypes$PredictedVsObserved
  generatePlots(
    dataMappings = dataMappings,
    dataMappingAll = dataMappingAll,
    plotConfiguration = plotConfiguration,
    simulationNames = simulationNames
  )
  calculateGOFMeasures(c, m, dataMappings, devFile = file.path('..', 'Results', "duck.txt"))
  calculateGOFMeasures(c, m, dataMappingsBl, devFile = file.path('..', 'Results', "duck_ven.txt"))
return(dataMappings)
  
}

plotCAP <- function(plotConfiguration, simulatedScenarios, observedData, compound, c, m, simulationNames){
  # ---- IV dose normalised observed data ----
  cmpout <-  plotConfiguration$outputFolder
  plotConfiguration$outputFolder <- file.path(plotConfiguration$outputFolder, '../../doseNormPlots')
  dataMappingNormIV <- DataMapping$new()
  currDataMapping <- dataMappingNormIV
  currDataMapping$addXYData(
    XYData = list(
      observedData$CAP_Anadon1994$`2.5kg`$male$mean$`30mgkg`$IV$Chloramphenicol$VenousBlood$Plasma
    ),
    # groups = list("Anadon 30mg/kg (chicken)")
    groups = list("Anadon 30mg/kg")
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$CAP_Atef1991$Healthy$unknown$mean$`20mgkg`$IV$Chloramphenicol$VenousBlood$Plasma),
    # groups = list("Atef 20mg/kg (chicken)")
    groups = list("Atef 20mg/kg")
  )
  # currDataMapping$addXYData(
  #   XYData = list(
  #     observedData$CAP_Dein1980$mixed$mean$`22mgkg`$IV$Chloramphenicol$VenousBlood$Plasma),
  #   groups = list("Dein 22mg/kg (duck)")
  # )
  
  
  currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("b", currDataMapping$xySeriesCount))
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "normalised concentration"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  yfactors <-  c(1/30, 1/20#, 1/22
  )/3.09# dose normalisation
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){
    currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors[i])
  })
  currDataMapping$title <- "CAP IV dose normalised"
  currDataMapping$log <-  "y"
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$plot()

  plotConfiguration$outputName <- currDataMapping$title
  plotConfiguration$nrOfCols <- 1
  plotMultiPanel(dataMappingNormIV,plotConfiguration)
  
  
  # ---- PO dose normalised observed data ----
  dataMappingNormPO <- DataMapping$new()
  currDataMapping <- dataMappingNormPO
  currDataMapping$addXYData(
    XYData = list(
      observedData$CAP_Atef1991$Healthy$unknown$mean$`20mgkg`$PO$Chloramphenicol$VenousBlood$Plasma       ),
    # groups = list("Atef 20mg/kg (chicken)")
    groups = list("Atef 20mg/kg")
  ) 
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$CAP_Anadon1994$`2.5kg`$male$mean$`30mgkg`$PO$Chloramphenicol$VenousBlood$Plasma
    ),
    # groups = list("Anadon 30mg/kg (chicken)")
    groups = list("Anadon 30mg/kg")
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$CAP_Anadon1994$`2.5kg`$male$mean$`50mgkg`$PO$Chloramphenicol$VenousBlood$Plasma
    ),
    # groups = list("Anadon 50mg/kg (chicken)")
    groups = list("Anadon 50mg/kg")
  )
  # currDataMapping$addXYData(
  #   XYData = list(
  #     observedData$CAP_Anadon1994_Tissue$males$mean$`50mgkg`$IC$Chloramphenicol$VenousBlood$Plasma
  #   ),
  #   # groups = list("Anadon 50 mg/kg mult (chicken)")
  #   groups = list("Anadon 50 mg/kg mult")
  # )
  # 
  currDataMapping$addXYData(
    XYData = list(
      observedData$CAP_Akhtar1996$LayingHen$f$mean$`100mg`$IC$Chloramphenicol$VenousBlood$Plasma
    ),
    # groups = list("Akhtar 75mg (chicken)")
    groups = list("Akhtar 75mg")
  )
  
  
  # currDataMapping$addXYData(
  #   XYData = list(
  #     observedData$CAP_Dein1980$mixed$`7`$`55mgkg`$PO$Chloramphenicol$VenousBlood$Plasma
  #   ),
  #   groups = list("Dein 55mg/kg (duck)")
  # )
  
  currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("b", currDataMapping$xySeriesCount))
  # currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "normalised concentration"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  yfactors <-  c(1/30, 
                 1/20, 
                 1/(100/1.36),  
                 1/50#, 1/55
                 )/3.09 # dose normalisation
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){
    currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors[i])
  })
  
  currDataMapping$title <- "CAP PO dose normalised"
  currDataMapping$log <-  "y"
  currDataMapping$legendPosition <- "right"
  
  currDataMapping$plot()
  
  # plotConfiguration$width <-  400
  # plotConfiguration$height <- 200
  plotConfiguration$outputName <- currDataMapping$title
  plotConfiguration$nrOfCols <- 1
  plotMultiPanel(dataMappingNormPO,plotConfiguration)
  
  ## plot as multipanel
  plotConfiguration$outputName <- "CAP - dose normalisation"
  plotConfiguration$nrOfCols <- 2
  plotMultiPanel(list(dataMappingNormIV, dataMappingNormPO), plotConfiguration)
  
  ## reset all factors
  yfactors1 <-  rep(1,currDataMapping$xySeriesCount)
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors1[i])})
  # currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("p", currDataMapping$xySeriesCount))
  
  currDataMapping <- dataMappingNormIV 
  yfactors1 <-  rep(1,currDataMapping$xySeriesCount)
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors1[i])})
  # currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("p", currDataMapping$xySeriesCount))
  
  # ---- 1) Atef 1991 IV 20mgkg ----  
  plotConfiguration$outputFolder <- cmpout
  
  # dataMappingAtef1991IV <- DataMapping$new()
  # currDataMapping <- dataMappingAtef1991IV
  # #How to add simulated results. outputValues is what is returned by "getOutputValues"
  # currDataMapping$addModelOutputs(
  #   paths = list(OutputPaths$VenousBlood),
  #   simulationResults = simulatedScenarios$CAP_Atef1991_IV_20mgkg$results,
  #   labels = list(OutputGroups$VenousBlood),
  #       groups = list(OutputGroups$VenousBlood)
  # )
  # 
  # #Add observed data, usually created by reading from excel.
  # currDataMapping$addXYData(
  #   XYData = list(observedData$CAP_Atef1991$Healthy$unknown$mean$`20mgkg`$IV$Chloramphenicol$VenousBlood$Plasma),
  #       groups = list(OutputGroups$VenousBlood)
  # )
  # 
  # currDataMapping$yLim
  # currDataMapping$xLab <- "Time [h]"
  # currDataMapping$yLab <- "Concentration [µmol/l]"
  # currDataMapping$yUnit <- "µmol/l"
  # currDataMapping$xUnit <- "h"
  # currDataMapping$title <- "Atef 1991 CAP IV 20mg/kg"
  # # currDataMapping$xLim <- c(0, 24)
  # currDataMapping$log = "y"
  # currDataMapping$plot() 
  # 
  # #You can also add custom xy-value with currDataMapping$addXYSeries(c(1,2,3), c(2,3,4), labels = "myData")
  
  # ---- 2) Anadon 1994 IV 30mgkg ----
  dataMappingAnadonIV <- DataMapping$new()
  currDataMapping <- dataMappingAnadonIV
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$CAP_Anadon1994_IV_30mgkg$results,
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  currDataMapping$addXYData(
    XYData = list(observedData$CAP_Anadon1994$`2.5kg`$male$mean$`30mgkg`$IV$Chloramphenicol$VenousBlood$Plasma),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(lloq = 0.005, unit = 'µg/ml', datamapping = currDataMapping, label = "lloq", group = "LLOQ")
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Anadon 1994 IV 30mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot() 
  
  # ---- 3) Anadon 1994 IC 30mgkg ----
  dataMappingAnadonIC30 <- DataMapping$new()
  currDataMapping <- dataMappingAnadonIC30
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$CAP_Anadon1994_IC_30mgkg$results,
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  currDataMapping$addXYData(
    XYData = list(observedData$CAP_Anadon1994$`2.5kg`$male$mean$`30mgkg`$PO$Chloramphenicol$VenousBlood$Plasma),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(lloq = 0.005, unit = 'µg/ml', datamapping = currDataMapping, label = "lloq", group = "LLOQ")
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Anadon 1994 IC 30mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot() 
  
  # ---- 4) Anadon 1994 IC 50mgkg ----
  dataMappingAnadonIC50 <- DataMapping$new()
  currDataMapping <- dataMappingAnadonIC50
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$CAP_Anadon1994_IC_50mgkg$results,
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  currDataMapping$addXYData(
    XYData = list(observedData$CAP_Anadon1994$`2.5kg`$male$mean$`50mgkg`$PO$Chloramphenicol$VenousBlood$Plasma),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(lloq = 0.005, unit = 'µg/ml', datamapping = currDataMapping, label = "lloq", group = "LLOQ")
  
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Anadon 1994 IC 50mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot() 
  
  # ---- 5) Anadon 1994 ICmult 50mgkg ----
  dataMappingAnadonIC50mult <- DataMapping$new()
  currDataMapping <- dataMappingAnadonIC50mult
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood,
                 OutputPaths$Kidney,
                 OutputPaths$Liver,
                 OutputPaths$Muscle
    ),
    simulationResults = simulatedScenarios$CAP_Anadon1994_50mgkg_4d$results,
    labels = list(OutputGroups$VenousBlood,
                  "Kidney",
                  "Liver",
                  "Muscle"),
    groups = list(OutputGroups$VenousBlood,
                  "Kidney",
                  "Liver",
                  "Muscle")
  )
  currDataMapping$addXYData(
    XYData = list(observedData$CAP_Anadon1994_Tissue$males$mean$`50mgkg`$IC$Chloramphenicol$VenousBlood$Plasma,
                  observedData$CAP_Anadon1994_Tissue$males$mean$`50mgkg`$IC$Chloramphenicol$Kidney$Tissue,
                  observedData$CAP_Anadon1994_Tissue$males$mean$`50mgkg`$IC$Chloramphenicol$Liver$Tissue,
                  observedData$CAP_Anadon1994_Tissue$males$mean$`50mgkg`$IC$Chloramphenicol$Muscle$Tissue),
    groups = list(OutputGroups$VenousBlood,
                  "Kidney",
                  "Liver",
                  "Muscle")
  )
  # addLLOQLine(lloq = 0.005, unit = 'µg/ml', datamapping = currDataMapping , label = "lloq", group = "LLOQ")
  
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Anadon 1994 IC 4d 50mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot() 
  
  # ---- 5) Anadon 1994 ICmult 50mgkg only blood----
  dataMappingAnadonIC50multbl <- DataMapping$new()
  currDataMapping <- dataMappingAnadonIC50multbl
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$CAP_Anadon1994_50mgkg_4d$results,
    labels = list(OutputGroups$VenousBlood),
    groups = list(OutputGroups$VenousBlood)
  )
  currDataMapping$addXYData(
    XYData = list(observedData$CAP_Anadon1994_Tissue$males$mean$`50mgkg`$IC$Chloramphenicol$VenousBlood$Plasma),
    groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(lloq = 0.005, unit = 'µg/ml', datamapping = currDataMapping , label = "lloq", group = "LLOQ")
  
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Anadon 1994 IC 4d 50mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot() 
  
  # ---- 6) Atef 1991 IC 20mgkg ----
  # dataMappingAtefIC20 <- DataMapping$new()
  # currDataMapping <- dataMappingAtefIC20
  # #How to add simulated results. outputValues is what is returned by "getOutputValues"
  # currDataMapping$addModelOutputs(
  #   paths = list(OutputPaths$VenousBlood),
  #   simulationResults = simulatedScenarios$CAP_Atef1991_IC_20mgkg$results,
  #   labels = list(OutputGroups$VenousBlood),
  #       groups = list(OutputGroups$VenousBlood)
  # )
  # 
  # #Add observed data, usually created by reading from excel.
  # currDataMapping$addXYData(
  #   XYData = list(observedData$CAP_Atef1991$Healthy$unknown$mean$`20mgkg`$PO$Chloramphenicol$VenousBlood$Plasma),
  #       groups = list(OutputGroups$VenousBlood)
  # )
  # 
  # currDataMapping$xLab <- "Time [h]"
  # currDataMapping$yLab <- "Concentration [µmol/l]"
  # currDataMapping$yUnit <- "µmol/l"
  # currDataMapping$xUnit <- "h"
  # currDataMapping$title <- "Atef 1991 CAP IC 20mg/kg"
  # # currDataMapping$xLim <- c(0, 24)
  # currDataMapping$log = "y"
  # currDataMapping$plot() 
  
  # ---- 7) Akhtar 1996 IC 100mg blood ----
  dataMappingAkhtar100bl <- DataMapping$new()
  currDataMapping <- dataMappingAkhtar100bl
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood,
                 OutputPaths$BloodCells
                 ),
    simulationResults = simulatedScenarios$CAP_Akhtar_IC_100mg$results,
    labels = list(OutputGroups$VenousBlood,
                  OutputGroups$BloodCells),
    groups = list(OutputGroups$VenousBlood,
                  "BloodCells")
  )
  currDataMapping$addXYData(
    XYData = list(observedData$CAP_Akhtar1996$LayingHen$f$mean$`100mg`$IC$Chloramphenicol$VenousBlood$Plasma,
                  observedData$CAP_Akhtar1996$LayingHen$f$mean$`100mg`$IC$Chloramphenicol$VenousBlood$BloodCells
    ),
    groups = list(OutputGroups$VenousBlood,
                  "BloodCells")
  )
  
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Akhtar 1996 IC 100mg"
  currDataMapping$log = "y"
  currDataMapping$plot()  
  # ---- 7) Akhtar 1996 IC 100mg tissue ----
  dataMappingAkhtar100 <- DataMapping$new()
  currDataMapping <- dataMappingAkhtar100
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood,
                 OutputPaths$BloodCells,
                 OutputPaths$Fat,
                 OutputPaths$Kidney,
                 OutputPaths$Liver,
                 OutputPaths$Muscle,
                 OutputPaths$Ovary
    ),
    simulationResults = simulatedScenarios$CAP_Akhtar_IC_100mg$results,
    labels = list(OutputGroups$VenousBlood,
                  "BloodCells",
                  "Fat",
                  "Kidney",
                  "Liver",
                  "Muscle",
                  "Ovary"),
    groups = list(OutputGroups$VenousBlood,
                  "BloodCells",
                  "Fat",
                  "Kidney",
                  "Liver",
                  "Muscle",
                  "Ovary")
  )
  currDataMapping$addXYData(
    XYData = list(observedData$CAP_Akhtar1996$LayingHen$f$mean$`100mg`$IC$Chloramphenicol$VenousBlood$Plasma,
                  observedData$CAP_Akhtar1996$LayingHen$f$mean$`100mg`$IC$Chloramphenicol$VenousBlood$BloodCells,
                  observedData$CAP_Akhtar1996$LayingHen$f$mean$`100mg`$IC$Chloramphenicol$Fat$Tissue,
                  observedData$CAP_Akhtar1996$LayingHen$f$mean$`100mg`$IC$Chloramphenicol$Kidney$Tissue,
                  observedData$CAP_Akhtar1996$LayingHen$f$mean$`100mg`$IC$Chloramphenicol$Liver$Tissue,
                  observedData$CAP_Akhtar1996$LayingHen$f$mean$`100mg`$IC$Chloramphenicol$Muscle$Tissue,
                  observedData$CAP_Akhtar1996$LayingHen$f$mean$`100mg`$IC$Chloramphenicol$OvarianYolk$Tissue
    ),
    groups = list(OutputGroups$VenousBlood,
                  "BloodCells",
                  "Fat",
                  "Kidney",
                  "Liver",
                  "Muscle",
                  "Ovary")
  )
  
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Akhtar 1996 IC 100mg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
  # ---- 8) Akhtar 1996 ICmult 5mgkg ----
  dataMappingAkhtar5 <- DataMapping$new()
  currDataMapping <- dataMappingAkhtar5
  addEggsToDataMapping(observedPath = observedData$CAP_Akhtar1996_Yolk$LayingHen$f$mean$`5mg`$IC$Chloramphenicol,
                       eggPart = "Yolk",
                       simulatedScenario = simulatedScenarios$CAP_Akhtar1996_5mgkg_5d,
                       dataMapping = currDataMapping)
  addEggsToDataMapping(observedPath = observedData$CAP_Akhtar1996_albumen$LayingHen$f$mean$`5mg`$IC$Chloramphenicol,
                       eggPart = "Albumen",
                       simulatedScenario = simulatedScenarios$CAP_Akhtar1996_5mgkg_5d,
                       dataMapping = currDataMapping)
  lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -9)
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Akhtar 1996 IC 5d 5mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot() 
  
  # ---- 9) Duck Dein 1980 IV 22mgkg ----
  dataMappingDeinIV22 <- DataMapping$new()
  currDataMapping <- dataMappingDeinIV22
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$CAP_Dein1980_IV_22mgkg$results,
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  currDataMapping$addXYData(
    XYData = list(observedData$CAP_Dein1980$mixed$mean$`22mgkg`$IV$Chloramphenicol$VenousBlood$Plasma),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$legendPosition <-  "topright"
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Dein 1980 IV 22mg/kg"
  currDataMapping$log = "y"
  currDataMapping$legendPosition <-  'topright'
  currDataMapping$plot() 
  
  # ---- 10) Duck Dein 1980 IC 55mgkg ----
  dataMappingDeinIC55 <- DataMapping$new()
  currDataMapping <- dataMappingDeinIC55
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$CAP_Dein1980_IC_55mgkg$results,
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  currDataMapping$addXYData(
    XYData = list(observedData$CAP_Dein1980$mixed$`7`$`55mgkg`$PO$Chloramphenicol$VenousBlood$Plasma),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Dein 1980 IC 55mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot() 
  
  # ---- generate multiplots ----
  dataMappings <- c(
    #dataMappingAtef1991IV,
    dataMappingAnadonIV,
    dataMappingAnadonIC30,
    dataMappingAnadonIC50,
    dataMappingAnadonIC50mult,
    # dataMappingAtefIC20,
    dataMappingAkhtar100,
    dataMappingAkhtar5,
    dataMappingDeinIV22,
    dataMappingDeinIC55
  )
  saveNameList <- simulationNames[c(2,4,5,8,6,7,1,3)]
  
  # build big mapping with all data
  dataMappingAll <- collapseDataMappings(dataMappings)
  dataMappingAll$log = "y"
  dataMappingAll$legendPosition <- "topleft"
  dataMappingAll$addLegend <-  TRUE
  dataMappingAll$plotType <-  PlotTypes$PredictedVsObserved
  noOfCols <- 3

  generatePlots(
    dataMappings = dataMappings,
    dataMappingsAUC = NULL,
    dataMappingAll = dataMappingAll,
    plotConfiguration = plotConfiguration,
    simulationNames = saveNameList
  )  
  plotConfiguration$nrOfCols <- noOfCols
  plotConfiguration$outputName <- "CAP - profiles"
  plotConfiguration$addTitle <-  FALSE
  dataMappingsMultiPanel <- list(
    #dataMappingAtef1991IV,
    dataMappingAnadonIV,
    dataMappingAnadonIC50,
    dataMappingAkhtar100,
    dataMappingDeinIV22,
    dataMappingAnadonIC30,
    dataMappingAnadonIC50mult,
    # dataMappingAtefIC20,
    dataMappingAkhtar5,
    dataMappingDeinIC55)
  lapply(dataMappingsMultiPanel, function(x) {x$plotType <-  PlotTypes$IndividualProfile
  x$legendPosition <-  "bottomright"}
  )
  dataMappingAkhtar100$legendPosition <-  "bottomright"
  plotMultiPanel(dataMappingsMultiPanel,
                 plotConfiguration = plotConfiguration )
  
  # ---- dhicken ven blood datamappings ----
  dataMappingsCAPChickenBlood <- list(dataMappingAnadonIV,
                                   dataMappingAnadonIC30,
                                   dataMappingAnadonIC50,
                                   dataMappingAnadonIC50multbl,
                                   dataMappingAkhtar100bl)
  # ---- species-wise plotting ----
  # chicken
  dataMappingsCAPChicken <-  list(#dataMappingAtef1991IV,
    dataMappingAnadonIV,
    dataMappingAnadonIC50,
    dataMappingAkhtar100,
    dataMappingAnadonIC30,
    dataMappingAnadonIC50mult,
    # dataMappingAtefIC20,
    dataMappingAkhtar5)
  plotConfiguration$nrOfCols <- noOfCols
  plotConfiguration$outputName <- "CAP - profiles in chickens"

  plotMultiPanel(dataMappingsCAPChicken,
                 plotConfiguration = plotConfiguration )
  
  lapply(dataMappingsCAPChicken, function(x) {x$plotType <-  PlotTypes$PredictedVsObserved
  x$legendPosition <-  "topright"}
  )
  plotConfiguration$nrOfCols <- noOfCols
  plotConfiguration$outputName <- "CAP - PVO in chickens"
  plotMultiPanel(dataMappingsCAPChicken,
                 plotConfiguration = plotConfiguration )
  # duck
  dataMappingsCAPDuck <-  list(dataMappingDeinIV22,
                               dataMappingDeinIC55)
  dataMappingDeinIV22$legendPosition <- "topright"
  plotConfiguration$nrOfCols <- noOfCols
  plotConfiguration$outputName <- "CAP - profiles in ducks"
  plotMultiPanel(dataMappingsCAPDuck,
                 plotConfiguration = plotConfiguration )     
  lapply(dataMappingsCAPDuck, function(x) {x$plotType <-  PlotTypes$PredictedVsObserved
  x$legendPosition <-  "topright"}
  )      
  dataMappingsCAPDuck <-  list(dataMappingDeinIV22,
                               dataMappingDeinIC55)
  plotConfiguration$nrOfCols <- noOfCols
  plotConfiguration$outputName <- "CAP - PVO in ducks"
  plotMultiPanel(dataMappingsCAPDuck,
                 plotConfiguration = plotConfiguration )
  
  # ---- plotting PVO plots with ggplot ----
  ggdata <- dataMapping2DT(dataMapping = dataMappings, molecule = "CAP")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDT.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "CAP - PvO all", legPos = "bottom")
  } else{
    pvo_all <- ggPVO(ggdata, legPos = "bottom", "CAP - PvO all")
    ggsave(plot = pvo_all,
           filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
           device = 'png',
           width = 15,
           height = 22, units = "cm", dpi = 300
    )
  }
  # pvo chickens
  ggdata <- dataMapping2DT(dataMapping = dataMappingsCAPChicken, molecule = "CAP")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsChickenAsDT.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "CAP - PvO chicken", fileName = 'dataMappingsChickenAsDT.csv', legPos = "bottom")
  } else{
    pvo_all <- ggPVO(ggdata, legPos = "bottom", "CAP - PvO chicken",limits = c(1e-2, 1e3))
    ggsave(plot = pvo_all,
           filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
           device = 'png',
           width = 15,
           height = 22, units = "cm", dpi = 300
    )
  }
  
  # pvo ducks
  ggdata <- dataMapping2DT(dataMapping = dataMappingsCAPDuck, molecule = "CAP")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsDuckAsDT.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "CAP - PvO duck", fileName = 'dataMappingsDuckAsDT.csv', legPos = "bottom")
  } else{
    pvo_all <- ggPVO(ggdata,legPos = "bottom", "CAP - PvO duck")
    ggsave(plot = pvo_all,
           filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
           device = 'png',
           width = 13,
           height = 18, units = "cm", dpi = 300
    )
  }
  
  # ---- plotting dose normalised plots ----
  
  # ---- calculate GOF measures ----
  calculateGOFMeasures(c, m, dataMappings)
  calculateGOFMeasures(c, m, dataMappingsCAPChicken, devFile = file.path('..', 'Results', "chicken.txt"))
  calculateGOFMeasures(c, m, dataMappingsCAPChickenBlood, devFile = file.path('..', 'Results', "chicken_ven.txt"))
  calculateGOFMeasures(c, m, dataMappingsCAPDuck, devFile = file.path('..', 'Results', "duck.txt"))
  # ---- generate egg plot ----
  dataMappingEggsCAP <- DataMapping$new()
  currDataMapping <- dataMappingEggsCAP
  addEggsToDataMapping(observedPath = observedData$CAP_Akhtar1996_Yolk$LayingHen$f$mean$`5mg`$IC$Chloramphenicol,
                       eggPart = "Yolk",
                       simulatedScenario = simulatedScenarios$CAP_Akhtar1996_5mgkg_5d,
                       group = "Akhtar 1996 Yolk",
                       dataMapping = currDataMapping)
  addEggsToDataMapping(observedPath = observedData$CAP_Akhtar1996_albumen$LayingHen$f$mean$`5mg`$IC$Chloramphenicol,
                       eggPart = "Albumen",
                       simulatedScenario = simulatedScenarios$CAP_Akhtar1996_5mgkg_5d,
                       group = "Akhtar 1996 Albumen",
                       dataMapping = currDataMapping)
  lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -9)
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "CAP eggs (chicken)"
  # currDataMapping$xLim <- c(0, 24)
  currDataMapping$setConfiguration(dataMappingConfiguration())
  currDataMapping$log = "y"
  currDataMapping$plot()
  return(currDataMapping)
}

plotDTM <- function(plotConfiguration, simulatedScenarios, observedData, compound, c, m, simulationNames){
  
  # ---- 0) dose normalised observed data ----
  cmpout <-  plotConfiguration$outputFolder
  plotConfiguration$outputFolder <- file.path(plotConfiguration$outputFolder, '../../doseNormPlots')
  dataMappingNorm <- DataMapping$new()
  currDataMapping <- dataMappingNorm
  currDataMapping$addXYData(
    XYData = list(
      observedData$DTM_Hueyuek2017$m$mean$`0.75mgkg`$IV$Deltamethrin$VenousBlood$Plasma
    ),
    groups = list("Hueyuek2017IV_0.75mgkg")
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$DTM_Hueyuek2017$m$mean$`0.75mgkg`$IC$Deltamethrin$VenousBlood$Plasma
    ),
    groups = list("Hueyuek2017IC_0.75mgkg")
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$`DTM_M-132448-01-1`$`Laying Hen`$f$mean$`0.4mgkg`$IV$Deltamethrin$Blood$Tissue
    ),
    groups = list("M-132448-01-1_0.4mgkg")
  )
  
 
  
  currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("b", currDataMapping$xySeriesCount))
  currDataMapping$yLim <- currDataMapping$yLim/ c(100, 0.1)
  currDataMapping$yLim <- currDataMapping$yLim/ c(100, 0.1)
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "normalised concentration"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  yfactors <-  c(1/0.75, 1/.4, 1/.75 )/1.98 # dose normalisation
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){
    currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors[i])
  })
  currDataMapping$title <- "DTM dose normalised"
  currDataMapping$log <-  "y"
  currDataMapping$legendPosition <- "topright"
  currDataMapping$plot()
  
  # plotConfiguration$width <-  10
  # plotConfiguration$height <-  10
  plotConfiguration$outputName <- "DTM dose normalised"
  plotMultiPanel(dataMappingNorm, plotConfiguration)
  
  # ---- 1) plot Hueyuek IV ----
  plotConfiguration$outputFolder <- cmpout
  
  
  dataMappingHueyuekIV <- DataMapping$new()
  currDataMapping <- dataMappingHueyuekIV
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$DTM_Hueyuek2017_IV_075mgkg$results,
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  currDataMapping$addXYData(
    XYData = list(observedData$DTM_Hueyuek2017$m$mean$`0.75mgkg`$IV$Deltamethrin$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Hueyuek 2017 IV 0.75mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
  # ---- 2) plot Hueyuek IC ----
  dataMappingHueyuekIC <- DataMapping$new()
  currDataMapping <- dataMappingHueyuekIC
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$DTM_Hueyuek2017_IC_075mgkg$results,
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$DTM_Hueyuek2017$m$mean$`0.75mgkg`$IC$Deltamethrin$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Hueyuek 2017 IC 0.75mg/kg"
  currDataMapping$log = "y"
  currDataMapping$legendPosition <- "bottomright"
  
  currDataMapping$plot()
  
  # ---- 3) plot MacLachlan ----
  dataMappingLachlan <- DataMapping$new()
  currDataMapping <- dataMappingLachlan
  addEggsToDataMapping(observedPath = observedData$DTM_McLachlan2008_Fig2$`Laying Hen`$f$mean$`20mg/kg`$Feed$Deltamethrin,
                       eggPart = "WholeEgg",
                       label = "DTM_MacLachlan2008_2mg_28d",
                       simulatedScenario = simulatedScenarios$DTM_MacLachlan2008_2mg_28d,
                       dataMapping = currDataMapping)
  
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$legendPosition <- "right"
  
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "MacLachlan 2008 Feed 20mg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
  # ---- 4) plot M_132448_01 ----
  dataMappingM_132448_01 <- DataMapping$new()
  currDataMapping <- dataMappingM_132448_01
  currDataMapping$addModelOutputs(
    paths = list(
      OutputPaths$WholeBlood,
      OutputPaths$Brain,
      OutputPaths$Fat,
      OutputPaths$Heart,
      OutputPaths$Kidney,
      OutputPaths$Liver,
      OutputPaths$Muscle,
      OutputPaths$Skin
    ),
    simulationResults = simulatedScenarios$DTM_M132448_01_IV_04mgkg$results,
    labels = list(
      OutputGroups$WholeBlood,
      "Brain",
      "Fat",
      "Heart",
      "Kidney",
      "Liver",
      "Muscle",
      "Skin"
    ),
    groups = list(
      OutputGroups$WholeBlood,
      "Brain",
      "Fat",
      "Heart",
      "Kidney",
      "Liver",
      "Muscle",
      "Skin"
    )
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$`DTM_M-132448-01-1`$`Laying Hen`$f$mean$`0.4mgkg`$IV$Deltamethrin$Blood$Tissue,
      observedData$`DTM_M-132448-01-1`$`Laying Hen`$f$mean$`0.4mgkg`$IV$Deltamethrin$Brain$Tissue,
      observedData$`DTM_M-132448-01-1`$`Laying Hen`$f$mean$`0.4mgkg`$IV$Deltamethrin$Fat$Tissue,
      observedData$`DTM_M-132448-01-1`$`Laying Hen`$f$mean$`0.4mgkg`$IV$Deltamethrin$Heart$Tissue,
      observedData$`DTM_M-132448-01-1`$`Laying Hen`$f$mean$`0.4mgkg`$IV$Deltamethrin$Kidney$Tissue,
      observedData$`DTM_M-132448-01-1`$`Laying Hen`$f$mean$`0.4mgkg`$IV$Deltamethrin$Liver$Tissue,
      observedData$`DTM_M-132448-01-1`$`Laying Hen`$f$mean$`0.4mgkg`$IV$Deltamethrin$Muscle$Tissue,
      observedData$`DTM_M-132448-01-1`$`Laying Hen`$f$mean$`0.4mgkg`$IV$Deltamethrin$Skin$Tissue
    ),
    groups = list(
      OutputGroups$WholeBlood,
      "Brain",
      "Fat",
      "Heart",
      "Kidney",
      "Liver",
      "Muscle",
      "Skin"
    )
  )
  # ---- 4) plot M_132448_01 only blood----
  dataMappingM_132448_01bl <- DataMapping$new()
  currDataMapping <- dataMappingM_132448_01bl
  currDataMapping$addModelOutputs(
    paths = list(
      OutputPaths$WholeBlood
    ),
    simulationResults = simulatedScenarios$DTM_M132448_01_IV_04mgkg$results,
    labels = list(
      OutputGroups$WholeBlood
    ),
    groups = list(
      OutputGroups$WholeBlood
    )
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$`DTM_M-132448-01-1`$`Laying Hen`$f$mean$`0.4mgkg`$IV$Deltamethrin$Blood$Tissue
    ),
    groups = list(
      OutputGroups$WholeBlood
    )
  )
  

  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "M_132448_01 IV 0.4mgkg"
  currDataMapping$log = "y"
  currDataMapping$legendPosition <- "bottomright"
  currDataMapping$xLim <- c(currDataMapping$xLim[[1]],currDataMapping$xLim[[2]]+80)
  currDataMapping$plot()
  # currDataMapping$addLegend <-  0
  
  # ---- summary plotting ----
  dataMappings <- c(dataMappingHueyuekIV,
                    dataMappingHueyuekIC,
                    dataMappingM_132448_01,
                    dataMappingLachlan)
  dataMappingsBl <- c(dataMappingHueyuekIV,
                    dataMappingHueyuekIC,
                    dataMappingM_132448_01bl)
  saveNameList <-  simulationNames[c(1,3,2,4)]
  dataMappingsForAUC <- c(dataMappingHueyuekIV,
                          dataMappingHueyuekIC,
                          dataMappingM_132448_01)
  
  plotConfiguration$outputName <- "DTM profiles"
  plotConfiguration$nrOfCols <- 2
  lapply(dataMappings, function(x) {x$plotType <-  PlotTypes$IndividualProfile      
  # x$legendPosition <-  "topright"
  }
  )
  plotMultiPanel(dataMappings, plotConfiguration = plotConfiguration)
  # build big mapping with all data
  dataMappingAll <- collapseDataMappings(dataMappings)
  dataMappingAll$log = "y"
  dataMappingAll$legendPosition <- "topleft"
  dataMappingAll$addLegend <-  TRUE
  dataMappingAll$plotType <-  PlotTypes$PredictedVsObserved
  
  generatePlots(
    dataMappings = dataMappings,
    dataMappingAll = dataMappingAll,
    plotConfiguration = plotConfiguration,
    simulationNames = saveNameList
  )
  

  
  
  # ---- plotting PVO with ggplot ----
  ggdata <- dataMapping2DT(dataMapping = dataMappings, molecule = "DTM")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDT.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, legPos = "bottom", m, "DTM - PvO chicken")
  } else{pvo_all <- ggPVO(ggdata,legPos = "bottom", "DTM - PvO chicken")
  ggsave(plot = pvo_all,
         filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
         device = 'png',
         width = 15,
         height = 20, units = "cm", dpi = 300
  )
  }
  
  # ---- calculate GOF measures ----
  calculateGOFMeasures(c, m, dataMappings)
  calculateGOFMeasures(c, m, dataMappings, devFile = file.path('..', 'Results', "chicken.txt"))
  calculateGOFMeasures(c, m, dataMappingsBl, devFile = file.path('..', 'Results', "chicken_ven.txt"))


# ---- define egg plot ----
dataMappingEggsDTM <- DataMapping$new()
currDataMapping <- dataMappingEggsDTM
addEggsToDataMapping(observedPath = observedData$DTM_McLachlan2008_Fig2$`Laying Hen`$f$mean$`20mg/kg`$Feed$Deltamethrin,
                     eggPart = "WholeEgg",
                     label = "DTM_MacLachlan2008_2mg_28d",
                     simulatedScenario = simulatedScenarios$DTM_MacLachlan2008_2mg_28d,
                     dataMapping = currDataMapping,
                     group = "MacLachlan 2008")

currDataMapping$xLab <- "Time [d]"
currDataMapping$yLab <- "Concentration [µmol/l]"
currDataMapping$yUnit <- "µmol/l"
currDataMapping$xUnit <- "day(s)"
currDataMapping$title <- "DTM eggs (chicken)"
currDataMapping$yLim <- c(0.01,10)
currDataMapping$log = "y"
currDataMapping$plot()
return(currDataMapping)
}

plotFFC <- function(plotConfiguration, simulatedScenarios, observedData, compound, c, m, simulationNames){
  
  cmpout <-  plotConfiguration$outputFolder
  plotConfiguration$outputFolder <- file.path(plotConfiguration$outputFolder, '../../doseNormPlots')
 # ---- IV dose normalised observed data ----
  dataMappingIV <- DataMapping$new()
  currDataMapping <- dataMappingIV
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Liu2018_Fig5$`8 weeks`$unknown$mean$`10mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
    groups = list("Liu 10")
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Anadon2008_corrected$male$mean$`20mgkg`$IV$Florfenicol$`Venous Blood`$Plasma
    ),
    groups = list("Anadon 20")
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Shen2003$male$mean$`15mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
    groups = list("Shen 15")
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Shen2003$male$mean$`30mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
    groups = list("Shen 30")
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Ismail2009$Chicken$mean$`30mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
    groups = list("Ismail 30")
  )


  currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("b", currDataMapping$xySeriesCount))
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "normalised concentration"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "FFC IV dose normalised chicken"
  yfactors <-  c(1/20, 1/30, 1/10, 1/30, 1/15)/2.79
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){
    currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors[i])
  })
  currDataMapping$log <-  "y"
  currDataMapping$legendPosition <- "topright"
  currDataMapping$plot()
  
  plotConfiguration$outputName <- currDataMapping$title
  plotConfiguration$nrOfCols <- 1
  plotMultiPanel(currDataMapping,plotConfiguration)
  
  # ---- dose normalesd FFC duck ---- 
  
  dataMappingIVduck <- DataMapping$new()
  currDataMapping <- dataMappingIVduck
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_ElBanna1998$healthy$unknown$mean$`30mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
    groups = list("ElBanna 30 (duck)")
  )      
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Tikhomirov2019$male$mean$`30mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
    groups = list("Tikhomirov 30 (duck)")
  )  
  
  
  currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("b", currDataMapping$xySeriesCount))
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "normalised concentration"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "FFC IV dose normalised duck"
  yfactors <-  c( 1/30, 1/30)/2.79
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){
    currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors[i])
  })
  currDataMapping$log <-  "y"
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$plot()
  
  # plotConfiguration$width <-  10
  # plotConfiguration$height <-  10
  plotConfiguration$outputName <- currDataMapping$title
  plotConfiguration$nrOfCols <- 1
  plotMultiPanel(currDataMapping,plotConfiguration)
  
  #---- dose normalesd FFC quail ----
  dataMappingIVquail <- DataMapping$new()
  currDataMapping <- dataMappingIVquail
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Ismail2009$Quail$mean$`30mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
    groups = list("Ismail 30 (quail)")
  )
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Koc2009$mixed$mean$`30mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
    groups = list("Koc 30 (quail)")
  )
  
  currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("b", currDataMapping$xySeriesCount))
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "normalised concentration"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "FFC IV dose normalised quail"
  yfactors <-  c( 1/30, 1/30)/2.79
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){
    currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors[i])
  })
  currDataMapping$log <-  "y"
  currDataMapping$legendPosition <- "topright"
  currDataMapping$plot()
  
  # plotConfiguration$width <-  10
  # plotConfiguration$height <-  10
  plotConfiguration$outputName <- currDataMapping$title
  plotConfiguration$nrOfCols <- 1
  plotMultiPanel(currDataMapping,plotConfiguration)
  
  
  #---- dose normalesd FFC chicken PO ----
  dataMappingPOchicken <- DataMapping$new()
  currDataMapping <- dataMappingPOchicken
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Shen2003$male$mean$`30mgkg`$PO$Florfenicol$VenousBlood$Plasma
    ),
    groups = list("Shen 30mgkg")
  )
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Liu2018_Fig5$`8 weeks`$unknown$mean$`20mgkg`$PO$Florfenicol$VenousBlood$Plasma
    ),
    groups = list("Liu 20mgkg")
  )
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Shen2003$male$mean$`15mgkg`$PO$Florfenicol$VenousBlood$Plasma
    ),
    groups = list("Shen 15mgkg")
  )
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Afifi1997$unknown$mean$`30mgkg`$PO$Florfenicol$VenousBlood
      
    ),
    groups = list("Afifi 30mgkg"
    )
  )
  
  # currDataMapping$addXYData(
  #   XYData = list(
  #     observedData$FFC_Anadon2008_corrected$male$mean$`20mgkg`$PO$Florfenicol$`Venous Blood`$Plasma
  #   ),
  #   groups = list("Anadon 20")
  # )
  
  currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("b", currDataMapping$xySeriesCount))
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "normalised concentration"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "FFC PO dose normalised chicken"
  
  yfactors <-  c( 1/30, 1/20, 1/15, 1/30#, 1/20
                  )/2.79
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){
    currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors[i])
  })
  currDataMapping$setXOffsets(currDataMapping$xySeries[[2]]$label, -96)
  currDataMapping$xLim <-  c(0,40)
  currDataMapping$log <-  "y"
  currDataMapping$legendPosition <- "topright"
  currDataMapping$plot()
  
  # plotConfiguration$width <-  10
  # plotConfiguration$height <-  10
  plotConfiguration$outputName <- currDataMapping$title
  plotConfiguration$nrOfCols <- 1
  plotMultiPanel(currDataMapping,plotConfiguration)
  
  plotConfiguration$nrOfCols <- 2
  plotConfiguration$outputName <- "FFC - dose normalisation"
  
  plotMultiPanel(list(dataMappingIV, dataMappingPOchicken, dataMappingIVduck, dataMappingIVquail), plotConfiguration)
  
  
 # ---- 1) FFC_Liu2018_IV_10mgkg ----
  plotConfiguration$outputFolder <- cmpout
  
  dataMappingLiuIV <- DataMapping$new()
  currDataMapping <- dataMappingLiuIV
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$FFC_Liu2018_IV_10mgkg$results,
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.1, unit = 'µg/ml', MW =  358.21)
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Liu2018_Fig5$`8 weeks`$unknown$mean$`10mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Liu 2018 IV 10mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 2) FFC_Anadon2008_IV_20mgkg ----
  dataMappingAnadonIV <- DataMapping$new()
  currDataMapping <- dataMappingAnadonIV
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$FFC_Anadon2008_IV_20mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  # addLLOQLine(lloq = 20, unit = 'ng/ml', label = "lloq_tis", group = "LLOQ tissues", MW =  358.21)
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Anadon2008_corrected$male$mean$`20mgkg`$IV$Florfenicol$`Venous Blood`$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.02, unit = 'µg/ml', MW =  358.21)
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Anadon 2008 IV 20mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 3) FFC_Shen2003_IV_15mgkg ----
  dataMappingShenIV <- DataMapping$new()
  currDataMapping <- dataMappingShenIV
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$FFC_Shen2003_IV_15mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Shen2003$male$mean$`15mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.025, unit = 'µg/l', MW =  358.21)
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Shen 2003 IV 15mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 4) FFC_Shen2003_IV_30mgkg ----
  dataMappingShenIV30 <- DataMapping$new()
  currDataMapping <- dataMappingShenIV30
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$FFC_Shen2003_IV_30mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Shen2003$male$mean$`30mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.025, unit = 'µg/l', MW =  358.21)
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Shen 2003 IV 30mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 5) FFC_Shen2003_IC_30mgkg ----
  dataMappingShenIC30 <- DataMapping$new()
  currDataMapping <- dataMappingShenIC30
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$FFC_Shen2003_IC_15mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Shen2003$male$mean$`30mgkg`$PO$Florfenicol$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.025, unit = 'µg/l', MW =  358.21)
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Shen 2003 IC 30mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 6) FFC_Liu2018_IC_20mgkg ----
  dataMappingLiuIC20 <- DataMapping$new()
  currDataMapping <- dataMappingLiuIC20
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$FFC_Liu2018_IC_20mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Liu2018_Fig5$`8 weeks`$unknown$mean$`20mgkg`$PO$Florfenicol$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.1, unit = 'µg/ml', MW =  358.21)
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Liu 2018 IC 20mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 7) FFC_Shen2003_IC_15mgkg ----
  dataMappingShenIC15 <- DataMapping$new()
  currDataMapping <- dataMappingShenIC15
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$FFC_Shen2003_IC_15mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.025, unit = 'µg/l', MW =  358.21)
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Shen2003$male$mean$`15mgkg`$PO$Florfenicol$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Shen 2003 IC 15mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
 # ---- 8) FFC_Chang2010_IC_30mgkg ----
  dataMappingChang <- DataMapping$new()
  currDataMapping <- dataMappingChang
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$FFC_Chang2010_IC_30mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Chang2010$unknown$mean$`30mgkg`$PO$Florfenicol$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.05, unit = 'µg/ml', MW =  358.21)
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Chang 2010 IC 30mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 9) FFC_Filazi2014_20mgkg_5d ----
  dataMappingFilaziICmult5d <- DataMapping$new()
  currDataMapping <- dataMappingFilaziICmult5d
  addEggsToDataMapping(
    observedPath = observedData$FFC_Filazi2014_WholeEgg$`5dPO`$f$mean$`20mgkg`$PO$Florfenicol ,
    eggPart = "WholeEgg" ,
    simulatedScenario = simulatedScenarios$FFC_Filazi2014_20mgkg_5d,
    dataMapping = currDataMapping
  )
  # addLLOQLine(lloq = 06.45, unit = 'ng/ml', label = "lloq", group = OutputGroups$WholeEgg, datamapping = currDataMapping, MW =  358.21)
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Filazi 2014 IC 5d 20mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 10) FFC_Filazi2014_20mgkg_3d ----
  dataMappingFilaziICmult3d <- DataMapping$new()
  currDataMapping <- dataMappingFilaziICmult3d
  addEggsToDataMapping(
    observedPath = observedData$FFC_Filazi2014_WholeEgg$`3dPO`$f$mean$`20mgkg`$PO$Florfenicol ,
    eggPart = "WholeEgg" ,
    simulatedScenario = simulatedScenarios$FFC_Filazi2014_20mgkg_3d,
    dataMapping = currDataMapping
  )
  # addLLOQLine(lloq = 06.45, unit = 'ng/ml', label = "lloq", group = OutputGroups$WholeEgg, datamapping = currDataMapping, MW =  358.21)
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Filazi 2014 IC 3d 20mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 11) FFC_Afifi1997_30mgkg_5d tissue could be added ----
  dataMappingAfifi30 <- DataMapping$new()
  currDataMapping <- dataMappingAfifi30
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood,
                 OutputPaths$Fat,
                 OutputPaths$Heart,
                 OutputPaths$Kidney,
                 OutputPaths$Liver,
                 OutputPaths$Muscle,
                 OutputPaths$Brain
    ),
    simulationResults = simulatedScenarios$FFC_Afifi1997_30mgkg_5d$results,
    
    labels = list(OutputGroups$VenousBlood,
                  "Fat",
                  "Heart",
                  "Kidney",
                  "Liver",
                  "Muscle",
                  "Brain"
    ),
    groups = list(OutputGroups$VenousBlood,
                  "Fat",
                  "Heart",
                  "Kidney",
                  "Liver",
                  "Muscle",
                  "Brain"
    )
  )
  
  # addLLOQLine(lloq = 0.01, unit = 'µg/ml', label = "lloq", group = "LLOQ", datamapping = currDataMapping, MW =  358.21)
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Afifi1997$unknown$mean$`30mgkg`$PO$Florfenicol$VenousBlood,
      observedData$FFC_Afifi1997$unknown$mean$`30mgkg`$PO$Florfenicol$Fat,
      observedData$FFC_Afifi1997$unknown$mean$`30mgkg`$PO$Florfenicol$Heart,
      observedData$FFC_Afifi1997$unknown$mean$`30mgkg`$PO$Florfenicol$Kidney,
      observedData$FFC_Afifi1997$unknown$mean$`30mgkg`$PO$Florfenicol$Liver,
      observedData$FFC_Afifi1997$unknown$mean$`30mgkg`$PO$Florfenicol$Muscle,
      observedData$FFC_Afifi1997$unknown$mean$`30mgkg`$PO$Florfenicol$Brain
      
    ),
    groups = list(OutputGroups$VenousBlood,
                  "Fat",
                  "Heart",
                  "Kidney",
                  "Liver",
                  "Muscle",
                  "Brain"
    )
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Afifi 1997 IC 5d 30mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  # ---- 11) FFC_Afifi1997_30mgkg_5d blood ----
  dataMappingAfifi30bl <- DataMapping$new()
  currDataMapping <- dataMappingAfifi30bl
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood
    ),
    simulationResults = simulatedScenarios$FFC_Afifi1997_30mgkg_5d$results,
    
    labels = list(OutputGroups$VenousBlood
    ),
    groups = list(OutputGroups$VenousBlood
    )
  )
  
  # addLLOQLine(lloq = 0.01, unit = 'µg/ml', label = "lloq", group = "LLOQ", datamapping = currDataMapping, MW =  358.21)
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Afifi1997$unknown$mean$`30mgkg`$PO$Florfenicol$VenousBlood
      
    ),
    groups = list(OutputGroups$VenousBlood
    )
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Afifi 1997 IC 5d 30mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 12) chicken FFC_Ismail2009_IV_30mgkg ----
  dataMappingIsmail <- DataMapping$new()
  currDataMapping <- dataMappingIsmail
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$FFC_Ismail2009_IV_30mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.01, unit = 'µg/ml', label = "lloq", MW =  358.21)
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Ismail2009$Chicken$mean$`30mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Ismail 2009 IV 30mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 13) quail FFC_Ismail2009_IV_30mgkg ----
  dataMappingIsmailquail <- DataMapping$new()
  currDataMapping <- dataMappingIsmailquail
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$FFC_Ismail2009_IV_30mgkg_quail$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.01, unit = 'µg/ml', label = "lloq", MW =  358.21)
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Ismail2009$Quail$mean$`30mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Ismail 2009 IV 30mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 14) duck FFC_ElBanna1998_IV_30mgkg ----
  dataMappingElBanna <- DataMapping$new()
  currDataMapping <- dataMappingElBanna
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$FFC_ElBanna1998_IV_30mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_ElBanna1998$healthy$unknown$mean$`30mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.05, unit = 'µg/ml', label = "llod", MW =  358.21)
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "ElBanna 1998 IV 30mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 15) quail FFC_Koc2009_IV_30mgkg ----
  dataMappingKocIV <- DataMapping$new()
  currDataMapping <- dataMappingKocIV
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$FFC_Koc2009_IV_30mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.1, unit = 'µg/ml')
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Koc2009$mixed$mean$`30mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Koc 2009 IV 30mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 16) duck FFC_Tikhomirov2019_IV_30mgkg ----
  dataMappingTikhomirovIV <- DataMapping$new()
  currDataMapping <- dataMappingTikhomirovIV
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$FFC_Tikhomirov2019_IV_30mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.011, unit = 'µg/ml', MW =  358.21)
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Tikhomirov2019$male$mean$`30mgkg`$IV$Florfenicol$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Tikhomirov 2019 IV 30mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 17) duck FFC_Tikhomirov2019_IV_30mgkg ----
  dataMappingTikhomirovIC <- DataMapping$new()
  currDataMapping <- dataMappingTikhomirovIC
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$FFC_Tikhomirov2019_IC_30mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.011, unit = 'µg/ml', MW =  358.21)
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Tikhomirov2019$male$mean$`30mgkg`$PO$Florfenicol$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Tikhomirov 2019 IC 30mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 18) FFC_Anadon2008_IC_20mgkg ----
  dataMappingAnadonIC <- DataMapping$new()
  currDataMapping <- dataMappingAnadonIC
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$FFC_Anadon2008_IC_20mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
    groups = list(OutputGroups$VenousBlood)
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.02, unit = 'µg/ml', MW =  358.21)
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Anadon2008_corrected$male$mean$`20mgkg`$PO$Florfenicol$`Venous Blood`$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Anadon 2008 IC 20mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
  
 # ---- 19) FFC_Anadon2008_40mgkg_3d ----
  dataMappingAnadonICmult <- DataMapping$new()
  currDataMapping <- dataMappingAnadonICmult
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$Fat,
                 OutputPaths$Kidney,
                 OutputPaths$Liver,
                 OutputPaths$Muscle
    ),
    simulationResults = simulatedScenarios$FFC_Anadon2008_40mgkg_3d$results,
    
    labels = list("Fat",
                  "Kidney",
                  "Liver",
                  "Muscle"
    ),
    groups = list("Fat",
                  "Kidney",
                  "Liver",
                  "Muscle"
    )
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 20, unit = 'µg/l', label = "lloq", group = "LLOQ", MW =  358.21)
  currDataMapping$addXYData(
    XYData = list(
      observedData$FFC_Anadon_multPO$male$mean$`40mgkg`$IC$Florfenicol$Fat$Tissue,
      observedData$FFC_Anadon_multPO$male$mean$`40mgkg`$IC$Florfenicol$Kidney$Tissue,
      observedData$FFC_Anadon_multPO$male$mean$`40mgkg`$IC$Florfenicol$Liver$Tissue,
      observedData$FFC_Anadon_multPO$male$mean$`40mgkg`$IC$Florfenicol$Muscle$Tissue
    ),
    groups = list("Fat",
                  "Kidney",
                  "Liver",
                  "Muscle"
    )
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Anadon 2008 IC 3d 40mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
  
 # ---- summary ----
  dataMappings <- c(
    dataMappingLiuIV,
    dataMappingLiuIC20,
    dataMappingAnadonIV,
    dataMappingShenIV,
    dataMappingShenIV30,
    dataMappingShenIC15,
    dataMappingShenIC30,
    dataMappingChang,
    dataMappingFilaziICmult5d,
    dataMappingFilaziICmult3d,
    dataMappingAfifi30,
    dataMappingIsmail,
    dataMappingIsmailquail,
    dataMappingElBanna,
    dataMappingKocIV,
    dataMappingTikhomirovIV,
    dataMappingTikhomirovIC,
    dataMappingAnadonIC,
    dataMappingAnadonICmult 
  )
  dataMappingsChicken <- c(
    dataMappingLiuIV,
    dataMappingLiuIC20,
    dataMappingAnadonIV,
    dataMappingShenIV,
    dataMappingShenIV30,
    dataMappingShenIC15,
    dataMappingShenIC30,
    dataMappingChang,
    dataMappingFilaziICmult5d,
    dataMappingFilaziICmult3d,
    dataMappingAfifi30,
    dataMappingIsmail,
    dataMappingAnadonIC,
    dataMappingAnadonICmult 
  )  
  dataMappingsChickenBl <- c(
    dataMappingLiuIV,
    dataMappingLiuIC20,
    dataMappingAnadonIV,
    dataMappingShenIV,
    dataMappingShenIV30,
    dataMappingShenIC15,
    dataMappingShenIC30,
    dataMappingChang,
    # dataMappingFilaziICmult5d,
    # dataMappingFilaziICmult3d,
    dataMappingAfifi30bl,
    dataMappingIsmail,
    dataMappingAnadonIC
  )
  dataMappingsChickenIV <- c(
    dataMappingLiuIV,
    dataMappingAnadonIV,
    dataMappingShenIV,
    dataMappingShenIV30,
    dataMappingIsmail 
  )
  dataMappingsChickenIC <- c(
    dataMappingLiuIC20,
    dataMappingAnadonIC,
    dataMappingShenIC15,
    dataMappingShenIC30,
    dataMappingChang,
    dataMappingFilaziICmult5d,
    dataMappingFilaziICmult3d,
    dataMappingAfifi30,
    dataMappingAnadonICmult
  )
  
  dataMappingsOthers <- c(
    dataMappingIsmailquail,
    dataMappingElBanna,
    dataMappingKocIV,
    dataMappingTikhomirovIV,
    dataMappingTikhomirovIC
  )     
  dataMappingsDucks <- c(
    dataMappingElBanna,
    dataMappingTikhomirovIV,
    dataMappingTikhomirovIC
  )      
  dataMappingsQuails <- c(
    dataMappingIsmailquail,
    dataMappingKocIV
    
  )
  saveNameList <-  simulationNames[c(1,7,2,3,6,9,8,11,12,13,4,5,17,16,14,15,18,19)]
  saveNameListChicken <-  simulationNames[c(1,7,2,3,6,9,8,10,11,12,13,4,18,19)]
  saveNameListChickenIV <-  simulationNames[c(1,2,3,6,4)]
  saveNameListChickenIC <-  simulationNames[c(7,18,9,8,10,11,12,13,19)]
  saveNameListOthers <-  simulationNames[c(5,17,16,14,15)]
  saveNameListDucks <-  simulationNames[c(17,14,15)]
  saveNameListQuails <-  simulationNames[c(5,16)]
  
  # build big mapping with all data
  dataMappingAll <- collapseDataMappings(dataMappings)
  dataMappingAll$log = "y"
  dataMappingAll$legendPosition <- "topright"
  dataMappingAll$addLegend <-  TRUE
  # plotConfiguration$addTitle <- FALSE
  dataMappingAll$plotType <-  PlotTypes$PredictedVsObserved

 # ---- summary plotting ----

  generatePlots(
    dataMappings = dataMappings,
    dataMappingAll = dataMappingAll,
    plotConfiguration = plotConfiguration,
    simulationNames = saveNameList
  )
  plotConfiguration$nrOfCols <- 3
  dataMappingsIV <- list( dataMappingShenIV,
                          dataMappingLiuIV,
                          dataMappingIsmail,
                          dataMappingShenIV30,
                          dataMappingAnadonIV,
                          dataMappingIsmailquail,
                          dataMappingKocIV,
                          dataMappingElBanna,
                          dataMappingTikhomirovIV)
  dataMappingsPO <- list(
    dataMappingShenIC15,
    dataMappingShenIC30,
    dataMappingLiuIC20,
    dataMappingChang,
    dataMappingAnadonIC,
    dataMappingFilaziICmult5d,
    dataMappingFilaziICmult3d,
    dataMappingAfifi30,
    dataMappingTikhomirovIC,
    dataMappingAnadonICmult)
  
  lapply(dataMappingsIV, function(x) {x$plotType <-  PlotTypes$IndividualProfile
  x$legendPosition <-"topright"})
  lapply(dataMappingsPO, function(x) {x$plotType <-  PlotTypes$IndividualProfile
  x$legendPosition <-"bottomright"})
  plotConfiguration$outputName <- "FFC - IV profiles"
  plotMultiPanel(dataMappingsIV,
                 plotConfiguration = plotConfiguration)
  plotConfiguration$outputName <- "FFC - PO profiles"
  plotMultiPanel(dataMappingsPO,
                 plotConfiguration = plotConfiguration)
  
 # ----species-wise plotting ----
  plotConfiguration$outputName <- "FFC - profiles in Chickens"
  plotMultiPanel(dataMappingsChicken,
                 plotConfiguration = plotConfiguration) 
  plotConfiguration$outputName <- "FFC - IV profiles in Chickens"
  plotMultiPanel(dataMappingsChickenIV,
                 plotConfiguration = plotConfiguration) 
  plotConfiguration$outputName <- "FFC - IC profiles in Chickens"
  plotMultiPanel(dataMappingsChickenIC,
                 plotConfiguration = plotConfiguration)     
  plotConfiguration$outputName <- "FFC - profiles in Ducks"
  plotMultiPanel(dataMappingsDucks,
                 plotConfiguration = plotConfiguration)     
  plotConfiguration$outputName <- "FFC - profiles in Quails"
  plotMultiPanel(dataMappingsQuails,
                 plotConfiguration = plotConfiguration)
  
 # ----plotting with ggplot ----
  ggdata <- dataMapping2DT(dataMapping = dataMappings, molecule = "FFC")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDT.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "FFC - PvO all", legPos = "bottom")
  } else{pvo_all <- ggPVO(ggdata, "FFC - PvO all", legPos = "bottom")
  ggsave(plot = pvo_all,
         filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
         device = 'png',
         width = 15,
         height = 20, units = "cm", dpi = 300
  )}
  
  ggdata <- dataMapping2DT(dataMapping = dataMappingsChicken, molecule = "FFC")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDTChicken.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "FFC - PvO chicken", fileName = 'dataMappingsAsDTChicken.csv', legPos = "bottom")
  } else{pvo_all <- ggPVO(ggdata, "FFC - PvO chicken", legPos = "bottom")
  ggsave(plot = pvo_all,
         filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
         device = 'png',
         width = 15,
         height = 22, units = "cm", dpi = 300
  )}     
  ggdata <- dataMapping2DT(dataMapping = dataMappingsOthers, molecule = "FFC")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDTOthers.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "FFC - PvO others", fileName = 'dataMappingsAsDTOthers.csv')
  } else{pvo_all <- ggPVO(ggdata, "FFC - others")
  ggsave(plot = pvo_all,
         filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
         device = 'png',
         width = 15,
         height = 20, units = "cm", dpi = 300
  )}
  ggdata <- dataMapping2DT(dataMapping = dataMappingsDucks, molecule = "FFC")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDTDuck.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "FFC - PvO duck", fileName = 'dataMappingsAsDTDuck.csv', legPos = "bottom")
  } else{pvo_all <- ggPVO(ggdata, "FFC - PVO duck", legPos = "bottom")
  ggsave(plot = pvo_all,
         filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
         device = 'png',
         width = 15,
         height = 20, units = "cm", dpi = 300
  )}
  ggdata <- dataMapping2DT(dataMapping = dataMappingsQuails, molecule = "FFC")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDTQuail.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "FFC - PvO quail", fileName = 'dataMappingsAsDTQuail.csv', legPos = "bottom")
  } else{pvo_all <- ggPVO(ggdata, "FFC - PVO quail", legPos = "bottom")
  ggsave(plot = pvo_all,
         filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
         device = 'png',
         width = 15,
         height = 20, units = "cm", dpi = 300
  )}
  
 # ----calculate GOF measures ----
  calculateGOFMeasures(c, m, dataMappings)
  calculateGOFMeasures(c, m, dataMappingsChickenBl, devFile = file.path('..', 'Results', "chicken_ven.txt"))
  calculateGOFMeasures(c, m, dataMappingsChicken, devFile = file.path('..', 'Results', "chicken.txt"))
  calculateGOFMeasures(c, m, dataMappingsDucks, devFile = file.path('..', 'Results', "duck.txt"))
  calculateGOFMeasures(c, m, dataMappingsQuails, devFile = file.path('..', 'Results', "quail.txt"))
# ---- define egg plot ----
dataMappingEggsFFC <- DataMapping$new()
currDataMapping <- dataMappingEggsFFC
addEggsToDataMapping(
  observedPath = observedData$FFC_Filazi2014_WholeEgg$`5dPO`$f$mean$`20mgkg`$PO$Florfenicol ,
  eggPart = "WholeEgg" ,
  simulatedScenario = simulatedScenarios$FFC_Filazi2014_20mgkg_5d,
  dataMapping = currDataMapping,
  label = "Filazi Eggs 20mgkg 5d",
  group = "Filazi Eggs 20mgkg 5d"
)
addEggsToDataMapping(
  observedPath = observedData$FFC_Filazi2014_WholeEgg$`3dPO`$f$mean$`20mgkg`$PO$Florfenicol ,
  eggPart = "WholeEgg" ,
  simulatedScenario = simulatedScenarios$FFC_Filazi2014_20mgkg_3d,
  dataMapping = currDataMapping,
  label = "Filazi Eggs 20mgkg 3d",
  group = "Filazi Eggs 20mgkg 3d"
  
)

currDataMapping$yLim <- c(1e-3,3)
currDataMapping$xLab <- "Time [d]"
currDataMapping$yLab <- "Concentration [µmol/l]"
currDataMapping$yUnit <- "µmol/l"
currDataMapping$xUnit <- "day(s)"
currDataMapping$title <- "FFC eggs (chicken)"
currDataMapping$log = "y"
currDataMapping$plot()
return(currDataMapping)   
}

plotIVM <- function(plotConfiguration, simulatedScenarios, observedData, compound, c, m, simulationNames){
  # ---- dose normalised ----
  cmpout <-  plotConfiguration$outputFolder
  plotConfiguration$outputFolder <- file.path(plotConfiguration$outputFolder, '../../doseNormPlots')
  
  dataMappingIVChicken <- DataMapping$new()
  currDataMapping <- dataMappingIVChicken
  currDataMapping$addXYData(
    XYData = list(
      observedData$IVM_Cirak2018$`laying hen`$f$mean$`0.2mgkg`$IV$Ivermectin$VenousBlood$Plasma
    ),
    groups = list("Cirak 0.2"
  )) 
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$IVM_Moreno2015_Fig1_import$f$mean$`0.4mgkg`$IV$Ivermectin$VenousBlood$Plasma
    ),
    groups = list("Moreno 0.4")
  )
  
  currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("b", currDataMapping$xySeriesCount))
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "normalised concentration"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "IVM dose normalised"
  yfactors <-  c( 1/0.4, 1/0.2)/1.14
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){
    currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors[i])
  })
  currDataMapping$setXOffsets(currDataMapping$xySeries[[2]]$label, -7)
  currDataMapping$log <-  "y"
  currDataMapping$legendPosition <- "topright"
  currDataMapping$plot()
  
  # plotConfiguration$width <-  10
  # plotConfiguration$height <-  10
  plotConfiguration$outputName <- currDataMapping$title
  plotConfiguration$nrOfCols <- 1
  plotMultiPanel(currDataMapping,plotConfiguration)
  
  # ---- 0) dataMapping eggs ----
  plotConfiguration$outputFolder <- cmpout
  
  dataMappingEggsIVM <- DataMapping$new()
  currDataMapping <- dataMappingEggsIVM
  addEggsToDataMapping(
    observedPath = observedData$IVM_Cirak2018$`laying hen`$f$mean$`0.2mgkg`$IV$Ivermectin,
    eggPart = "WholeEgg",
    simulatedScenario = simulatedScenarios$IVM_Cirak2018_IV_02mgkg,
    dataMapping = currDataMapping,
    numberedEggs = 1
  )
  lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -168/24)
  
  
  addEggsToDataMapping(
    observedPath = observedData$IVM_Cirak2018$`laying hen`$f$mean$`0.2mgkg`$PO$Ivermectin,
    eggPart = "WholeEgg",
    simulatedScenario = simulatedScenarios$IVM_Cirak2018_IC_02mgkg,
    dataMapping = currDataMapping,
    numberedEggs = 1
  )
  lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -168/24)
  
  addEggsToDataMapping(
    observedPath = observedData$IVM_Moreno2015_Fig2_Egg$f$mean$`0.4mgkg`$PO$Ivermectin,
    eggPart = 'Yolk',
    simulatedScenario = simulatedScenarios$IVM_Moreno2015_04mgkg_5d,
    dataMapping = currDataMapping,
    numberedEggs = 1
  )
  lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -7)
  
  tmp <- currDataMapping$xySeries$Yolk
  tmp$xOffset <- -5*24*60
  currDataMapping$xLab <- "Time [d]"
  # currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Cirak 2018 IV 0.2mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
# ---- 1) IVM_Cirak2018_IV_02mgkg----
  dataMappingCirakIV <- DataMapping$new()
  currDataMapping <- dataMappingCirakIV
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$IVM_Cirak2018_IV_02mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(lloq = 0.25, unit = 'ng/ml')
  # addLLOQLine(lloq = 0.025, unit = 'µg/l', datamapping = currDataMapping, label = "lloq_egg", group = OutputGroups$WholeEgg)
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$IVM_Cirak2018$`laying hen`$f$mean$`0.2mgkg`$IV$Ivermectin$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  addEggsToDataMapping(
    observedPath = observedData$IVM_Cirak2018$`laying hen`$f$mean$`0.2mgkg`$IV$Ivermectin,
    eggPart = "WholeEgg",
    simulatedScenario = simulatedScenarios$IVM_Cirak2018_IV_02mgkg,
    dataMapping = currDataMapping,
    numberedEggs = 1
  )
  lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -168/24)
  currDataMapping$xLab <- "Time d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Cirak 2018 IV 0.2mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
# ---- 2) IVM_Cirak2018_IC_02mgkg----
  dataMappingCirakIC <- DataMapping$new()
  currDataMapping <- dataMappingCirakIC
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$IVM_Cirak2018_IC_02mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(lloq = 0.25, unit = 'ng/ml', datamapping = currDataMapping)
  # addLLOQLine(lloq = 0.025, unit = 'µg/l', datamapping = currDataMapping, label = "lloq_egg", group = OutputGroups$WholeEgg)
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$IVM_Cirak2018$`laying hen`$f$mean$`0.2mgkg`$PO$Ivermectin$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  addEggsToDataMapping(
    observedPath = observedData$IVM_Cirak2018$`laying hen`$f$mean$`0.2mgkg`$PO$Ivermectin,
    eggPart = "WholeEgg",
    simulatedScenario = simulatedScenarios$IVM_Cirak2018_IC_02mgkg,
    dataMapping = currDataMapping,
    numberedEggs = 1
  )
  lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -168/24)
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Cirak 2018 IC 0.2mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
# ---- 2) IVM_Moreno2015_IV_04mgkg----
  dataMappingMorenoIV <- DataMapping$new()
  currDataMapping <- dataMappingMorenoIV
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$IVM_Moreno2015_IV_04mgkg$results,
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(lloq = 0.2, unit = 'ng/ml')
  # addLLOQLine(lloq = 0.5, unit = 'ng/ml', datamapping = , label = "lloq_kid", group = OutputGroups$Kidney)
  
  #Add observed data, usually created by reading from excel.
  currDataMapping$addXYData(
    XYData = list(
      observedData$IVM_Moreno2015_Fig1_import$f$mean$`0.4mgkg`$IV$Ivermectin$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Moreno 2015 IV 0.4mg/kg"
  currDataMapping$log = "y"

# ---- 3) IVM_Moreno2015_04mgkg_5d----
  dataMappingMorenoPO <- DataMapping$new()
  currDataMapping <- dataMappingMorenoPO
  currDataMapping$addModelOutputs(
    paths = list(
      OutputPaths$VenousBlood,
      OutputPaths$Fat,
      OutputPaths$Kidney,
      OutputPaths$Liver,
      OutputPaths$Muscle
    ),
    simulationResults = simulatedScenarios$IVM_Moreno2015_04mgkg_5d$results,
    
    labels = list(OutputGroups$VenousBlood,
                  "Fat",
                  "Kidney",
                  "Liver",
                  "Muscle"),
    groups = list(OutputGroups$VenousBlood,
                  "Fat",
                  "Kidney",
                  "Liver",
                  "Muscle")
  )
  # addLLOQLine(lloq = 0.2, unit = 'ng/ml', group = "LLOQ plasma & muscle")
  # addLLOQLine(lloq = 0.5, unit = 'ng/ml', datamapping = , label = "lloq_kid", group = OutputGroups$Kidney)
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$IVM_Moreno2015_TableTissue$f$mean$`0.4mgkgk oid`$PO$Ivermectin$VenousBlood$Plasma,
      observedData$IVM_Moreno2015_TableTissue$f$mean$`0.4mgkgk oid`$PO$Ivermectin$Fat$Tissue,
      observedData$IVM_Moreno2015_TableTissue$f$mean$`0.4mgkgk oid`$PO$Ivermectin$Kidney$Tissue,
      observedData$IVM_Moreno2015_TableTissue$f$mean$`0.4mgkgk oid`$PO$Ivermectin$Liver$Tissue,
      observedData$IVM_Moreno2015_TableTissue$f$mean$`0.4mgkgk oid`$PO$Ivermectin$Muscle$Tissue
    ),
    groups = list(OutputGroups$VenousBlood,
                  "Fat",
                  "Kidney",
                  "Liver",
                  "Muscle")
  )
  
  addEggsToDataMapping(
    observedPath = observedData$IVM_Moreno2015_Fig2_Egg$f$mean$`0.4mgkg`$PO$Ivermectin,
    eggPart = 'Yolk',
    simulatedScenario = simulatedScenarios$IVM_Moreno2015_04mgkg_5d,
    dataMapping = currDataMapping,
    numberedEggs = 1
  )
  lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -7)
  tmp <- currDataMapping$xySeries$Yolk
  tmp$xOffset <- -5*24*60
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Moreno 2015 IC 0.4mg/kg"
  currDataMapping$yLim <- c(1e-5, 1)
  currDataMapping$log = "y"
  currDataMapping$plotType <- PlotTypes$IndividualProfile
  currDataMapping$plot()
  # ---- 3) IVM_Moreno2015_04mgkg_5d blood ----
  dataMappingMorenoPObl <- DataMapping$new()
  currDataMapping <- dataMappingMorenoPObl
  currDataMapping$addModelOutputs(
    paths = list(
      OutputPaths$VenousBlood
    ),
    simulationResults = simulatedScenarios$IVM_Moreno2015_04mgkg_5d$results,
    
    labels = list(OutputGroups$VenousBlood),
    groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(lloq = 0.2, unit = 'ng/ml', group = "LLOQ plasma & muscle")
  # addLLOQLine(lloq = 0.5, unit = 'ng/ml', datamapping = , label = "lloq_kid", group = OutputGroups$Kidney)
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$IVM_Moreno2015_TableTissue$f$mean$`0.4mgkgk oid`$PO$Ivermectin$VenousBlood$Plasma
    ),
    groups = list(OutputGroups$VenousBlood)
  )
  
  addEggsToDataMapping(
    observedPath = observedData$IVM_Moreno2015_Fig2_Egg$f$mean$`0.4mgkg`$PO$Ivermectin,
    eggPart = 'Yolk',
    simulatedScenario = simulatedScenarios$IVM_Moreno2015_04mgkg_5d,
    dataMapping = currDataMapping,
    numberedEggs = 1
  )
  lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -7)
  tmp <- currDataMapping$xySeries$Yolk
  tmp$xOffset <- -5*24*60
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Moreno 2015 IC 0.4mg/kg"
  currDataMapping$yLim <- c(1e-5, 1)
  currDataMapping$log = "y"
  currDataMapping$plotType <- PlotTypes$IndividualProfile
  currDataMapping$plot()
  
# ---- plotting----
  dataMappings <- c(dataMappingCirakIV,
                    dataMappingMorenoIV,
                    dataMappingCirakIC,
                    dataMappingMorenoPO)
  dataMappingsBL <- c(dataMappingCirakIV,
                    dataMappingMorenoIV,
                    dataMappingCirakIC,
                    dataMappingMorenoPObl)
  saveNameList <- simulationNames[c(1,3,2,4)]
  
  dataMappingAll <- collapseDataMappings(dataMappings)
  dataMappingAll$log = "y"
  dataMappingAll$legendPosition <- "bottomleft"
  dataMappingAll$addLegend <-  TRUE
  dataMappingAll$plotType <-  PlotTypes$PredictedVsObserved

# ---- summary plotting----

  generatePlots(
    dataMappings = dataMappings,
    dataMappingAll = dataMappingAll,
    plotConfiguration = plotConfiguration,
    simulationNames = simulationNames
  )
  # plot multipanel for report
  plotConfiguration$nrOfCols <- 2
  plotConfiguration$outputName <- "IVM profiles"
  dataMappingsMultiPanel <- list( dataMappingCirakIV,
                                  dataMappingMorenoIV,
                                  dataMappingCirakIC,
                                  dataMappingMorenoPO)
  lapply(dataMappingsMultiPanel, function(x) {x$plotType <-  PlotTypes$IndividualProfile      
  x$legendPosition <-  "topright"}
  )
  
  plotMultiPanel(dataMappingsMultiPanel, plotConfiguration = plotConfiguration)
# ---- plotting with ggplot----
  ggdata <- dataMapping2DT(dataMapping = dataMappings, molecule = "IVM")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDT.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "IVM - PvO chicken", legPos = "bottom")
  } else{pvo_all <- ggPVO(ggdata, "IVM - PvO chicken", legPos = "bottom")
  ggsave(plot = pvo_all,
         filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
         device = 'png',
         width = 15,
         height = 20, units = "cm", dpi = 300
  )
  }
# ---- calculate GOF measures----
  calculateGOFMeasures(c, m, dataMappings)
  calculateGOFMeasures(c, m, dataMappings, devFile = file.path('..', 'Results', "chicken.txt"))
  calculateGOFMeasures(c, m, dataMappingsBL, devFile = file.path('..', 'Results', "chicken_ven.txt"))
  
# ---- egg plot ----
dataMappingEggsIVM <- DataMapping$new()
currDataMapping <- dataMappingEggsIVM
addEggsToDataMapping(
  observedPath = observedData$IVM_Cirak2018$`laying hen`$f$mean$`0.2mgkg`$IV$Ivermectin,
  eggPart = "WholeEgg",
  simulatedScenario = simulatedScenarios$IVM_Cirak2018_IV_02mgkg,
  dataMapping = currDataMapping,
  group = "Cirak 2018 IV",
  numberedEggs = 1
)
lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -168/24)
addEggsToDataMapping(
  observedPath = observedData$IVM_Cirak2018$`laying hen`$f$mean$`0.2mgkg`$PO$Ivermectin,
  eggPart = "WholeEgg",
  simulatedScenario = simulatedScenarios$IVM_Cirak2018_IC_02mgkg,
  dataMapping = currDataMapping,
  group = "Cirak 2018 IC",
  numberedEggs = 1
)
lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -168/24)
addEggsToDataMapping(
  observedPath = observedData$IVM_Moreno2015_Fig2_Egg$f$mean$`0.4mgkg`$PO$Ivermectin,
  eggPart = 'Yolk',
  simulatedScenario = simulatedScenarios$IVM_Moreno2015_04mgkg_5d,
  dataMapping = currDataMapping,
  group = "Moreno 2015",
  numberedEggs = 1
)
currDataMapping$xLab <- "Time [d]"
currDataMapping$yLab <- "Concentration [µmol/l]"
currDataMapping$yUnit <- "µmol/l"
currDataMapping$xUnit <- "day(s)"
currDataMapping$title <- "IVM eggs (chicken)"
currDataMapping$yLim <- c(currDataMapping$yLim[1]*-.01, currDataMapping$yLim[2])
currDataMapping$log = "y"
currDataMapping$plot()
return(currDataMapping)
  
}

plotMDZ <- function(plotConfiguration, simulatedScenarios, observedData, compound, c, m, simulationNames){
  
  # ---- 1) Cortright chicken ----
  dataMappingCortrightChicken <- DataMapping$new()
  currDataMapping <- dataMappingCortrightChicken
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$MDZ_Cortright2007_IV_05mgkg_chicken$results,
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$MDZ_Cortright2007_Fig1$Chicken$m$mean$`5mgkg`$IV$Midazolam$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(datamapping = currDataMapping, lloq = 30.6, unit = 'ng/ml')
  
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Cortright 2007 IV 5mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
  # ---- 2) Cortright quail ----
  dataMappingCortrightQuail <- DataMapping$new()
  currDataMapping <- dataMappingCortrightQuail
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$MDZ_Cortright2007_IV_05mgkg_quail$results,
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$MDZ_Cortright2007_Fig1$`Bobwhite Quail`$mix$mean$`5mgkg`$IV$Midazolam$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(datamapping = currDataMapping, lloq = 30.6, unit = 'ng/ml', label = "lloq", group = "LLOQ (chicken)")
  
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Cortright 2007 IV 5mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
  # ---- summary plotting ----
  dataMappings <- c(dataMappingCortrightChicken,
                    dataMappingCortrightQuail)
  dataMappingsChicken <- list(dataMappingCortrightChicken)
  dataMappingsQuail <- list(dataMappingCortrightQuail)
  # build big mapping with all data
  dataMappingAll <- collapseDataMappings(dataMappings)
  dataMappingAll$log = "y"
  dataMappingAll$legendPosition <- "topleft"
  dataMappingAll$addLegend <-  TRUE
  dataMappingAll$plotType <-  PlotTypes$PredictedVsObserved
  generatePlots(
    dataMappings = dataMappings,
    # dataMappingsAUC = dataMappingsAUC,
    dataMappingAll = dataMappingAll,
    plotConfiguration = plotConfiguration,
    simulationNames = simulationNames
  )
  ## plot multipanel for report
  plotConfiguration$outputName <- "MDZ profiles"
  plotConfiguration$nrOfCols <- 2
  plotMultiPanel(dataMappings, plotConfiguration = plotConfiguration)
  
  # ---- plotting PVO with ggplot ----
  ggdata <- dataMapping2DT(dataMapping = dataMappings, molecule = "MDZ")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDT.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "MDZ - PvO all")
  } else{pvo_all <- ggPVO(ggdata, "MDZ - PvO all")
  ggsave(plot = pvo_all,
         filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
         device = 'png',
         width = 15,
         height = 20, units = "cm", dpi = 300
  )
  }
  ggdata <- dataMapping2DT(dataMapping = dataMappingsChicken, molecule = "MDZ")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDTchicken.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "MDZ - PvO chicken", legPos = "bottom")
  } else{pvo_all <- ggPVO(ggdata, "MDZ - PvO chicken", legPos = "bottom")
  ggsave(plot = pvo_all,
         filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
         device = 'png',
         width = 15,
         height = 20, units = "cm", dpi = 300
  )
  }
  ggdata <- dataMapping2DT(dataMapping = dataMappingsQuail, molecule = "MDZ")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDTquail.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "MDZ - PvO quail", legPos = "bottom")
  } else{pvo_all <- ggPVO(ggdata, "MDZ - PvO quail", legPos = "bottom")
  ggsave(plot = pvo_all,
         filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
         device = 'png',
         width = 15,
         height = 20, units = "cm", dpi = 300
  )
  }
  # ---- calculate GOF measures ----
  calculateGOFMeasures(c, m, dataMappings)
  calculateGOFMeasures(c, m, dataMappingsChicken, devFile = file.path('..', 'Results', "chicken.txt"))
  calculateGOFMeasures(c, m, dataMappingsQuail, devFile = file.path('..', 'Results', "quail.txt"))

  
}

plotMEL <- function(plotConfiguration, simulatedScenarios, observedData, compound, c, m, simulationNames){
  cmpout <-  plotConfiguration$outputFolder
  plotConfiguration$outputFolder <- file.path(plotConfiguration$outputFolder, '../../doseNormPlots')
  
  # ---- dose normalised observed data ----
  dataMappingDoseNorm <- DataMapping$new()
  currDataMapping <- dataMappingDoseNorm
  # currDataMapping$addXYData(
  #   XYData = list(observedData$MEL_Poapolathep2015$male$mean$`5mgkg`$IV$Melamine$VenousBlood$Plasma),
  #   groups = list("Poapolathep 5mgkg IV")
  # )
  # 
  # currDataMapping$addXYData(
  #   XYData = list(observedData$MEL_Poapolathep2015$male$mean$`5mgkg`$PO$Melamine$VenousBlood$Plasma),
  #   groups = list("Poapolathep 5mgkg IC")
  # )
  currDataMapping$addXYData(XYData = list(observedData$MEL_Bai2010_Tissues$f$mean$`140.9mgkg`$Feed$Melamine$VenousBlood$Plasma
  ),
  groups = list("Bai 140.9mgkg feed")
  )    
  
  currDataMapping$addXYData(XYData = list(observedData$MEL_Bai2010_Tissues$f$mean$`62.6mgkg`$Feed$Melamine$VenousBlood$Plasma
  ),
  groups = list("Bai 62.6mgkg feed"
  )            
  )
  
  currDataMapping$addXYData(XYData = list(observedData$MEL_Bai2010_Tissues$f$mean$`33.6mgkg`$Feed$Melamine$VenousBlood$Plasma
  ),
  groups = list("Bai 33.6mgkg feed"
  ))
  
  currDataMapping$addXYData(XYData = list(observedData$MEL_Bai2010_Tissues$f$mean$`17.4mgkg`$Feed$Melamine$VenousBlood$Plasma
  ),
  groups = list("Bai 17.4mgkg feed"
  )
  )  
  
  currDataMapping$addXYData(XYData = list(observedData$MEL_Bai2010_Tissues$f$mean$`8.6mgkg`$Feed$Melamine$VenousBlood$Plasma
  ),
  groups = list("Bai 8.6mgkg feed"
  )
  )  
  
  
  currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("b", currDataMapping$xySeriesCount))
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "normalised concentration"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "MEL dose normalised"
  yfactors <-  c(# 1/5, 1/5, 
    1/17.4,
    1/62.6, 
    1/140.9, 
    1/8.6,
    1/33.6
    )/7.93
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){
    currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors[i])
  })
  currDataMapping$xLim <-  c(currDataMapping$xLim[1], currDataMapping$xLim[2]*2)
  currDataMapping$log <-  "y"
  currDataMapping$legendPosition <- "topright"
  currDataMapping$plot()
  
  # plotConfiguration$width <-  10
  # plotConfiguration$height <-  10
  plotConfiguration$outputName <- currDataMapping$title
  plotConfiguration$nrOfCols <- 1
  plotMultiPanel(currDataMapping,plotConfiguration)
 # ---- 1) MEL_Poapolathep2015IV_5o5mgkg----
  plotConfiguration$outputFolder <-  cmpout
  
  dataMappingPoapIV <- DataMapping$new()
  currDataMapping <- dataMappingPoapIV
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$MEL_Poapolathep2015_IV_5o5mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$addXYData(
    XYData = list(observedData$MEL_Poapolathep2015$male$mean$`5mgkg`$IV$Melamine$VenousBlood$Plasma),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.1, unit = 'µg/ml')
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Poapolathep 2015 IV 5.5mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot() 
  
 # ---- 2) MEL_Suknikom2016IV_5o5mgkg----
  dataMappingSuknikomIV <- DataMapping$new()
  currDataMapping <- dataMappingSuknikomIV
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$MEL_Suknikom2016_IV_5o5mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$addXYData(
    XYData = list(observedData$MEL_Suknikom2016$male$mean$`5mgkg`$IV$Melamine$VenousBlood$Plasma),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.1, unit = 'µg/ml')
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Suknikom 2016 IV 5mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot() 
  
 # ---- 3) MEL_Poapolathep2015IC_5o5mgkg----
  dataMappingPoapPO <- DataMapping$new()
  currDataMapping <- dataMappingPoapPO
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$MEL_Poapolathep2015_IC_5o5mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$addXYData(
    XYData = list(observedData$MEL_Poapolathep2015$male$mean$`5mgkg`$PO$Melamine$VenousBlood$Plasma),
        groups = list(OutputGroups$VenousBlood)
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.1, unit = 'µg/ml')
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Poapolathep 2015 IC 5.5mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot() 
  
 # ---- 4) MEL_Suknikom2016IC_5o5mgkg----
  dataMappingSuknikomPO <- DataMapping$new()
  currDataMapping <- dataMappingSuknikomPO
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood,
                 OutputPaths$Heart,
                 OutputPaths$Kidney,
                 OutputPaths$Liver,
                 OutputPaths$Muscle),
    simulationResults = simulatedScenarios$MEL_Suknikom2016_IC_5o5mgkg$results,
    
    labels = list(OutputGroups$VenousBlood,
                  OutputGroups$Heart,
                  OutputGroups$Kidney,
                  OutputGroups$Liver,
                  OutputGroups$Muscle),
    groups = list(OutputGroups$VenousBlood,
                  OutputGroups$Heart,
                  OutputGroups$Kidney,
                  OutputGroups$Liver,
                  OutputGroups$Muscle)
  )
  
  
  currDataMapping$addXYData(
    XYData = list(observedData$MEL_Suknikom2016$male$mean$`5mgkg`$PO$Melamine$VenousBlood$Plasma,
                          observedData$MEL_Suknikom2016$male$mean$`5mgkg`$PO$Melamine$Heart$Tissue,
                          observedData$MEL_Suknikom2016$male$mean$`5mgkg`$PO$Melamine$Kidney$Tissue,
                          observedData$MEL_Suknikom2016$male$mean$`5mgkg`$PO$Melamine$Liver$Tissue,
                          observedData$MEL_Suknikom2016$male$mean$`5mgkg`$PO$Melamine$Muscle$Tissue),
    groups = list(OutputGroups$VenousBlood,
                  OutputGroups$Heart,
                  OutputGroups$Kidney,
                  OutputGroups$Liver,
                  OutputGroups$Muscle)
  )
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.1, unit = 'µg/ml')
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.25, unit = 'µg/ml', label = "LLOQ_tis", group = "LLOQ tissue")
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Suknikom 2016 IC 5.5mg/kg"
  currDataMapping$log = "y"
  currDataMapping$legendPosition <- "bottomright"
  currDataMapping$plot()  
  # ---- 4) MEL_Suknikom2016IC_5o5mgkg blood----
  dataMappingSuknikomPObl <- DataMapping$new()
  currDataMapping <- dataMappingSuknikomPObl
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$MEL_Suknikom2016_IC_5o5mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
    groups = list(OutputGroups$VenousBlood)
  )
  
  
  currDataMapping$addXYData(
    XYData = list(observedData$MEL_Suknikom2016$male$mean$`5mgkg`$PO$Melamine$VenousBlood$Plasma),
    groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.1, unit = 'µg/ml')
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.25, unit = 'µg/ml', label = "LLOQ_tis", group = "LLOQ tissue")
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Suknikom 2016 IC 5.5mg/kg"
  currDataMapping$log = "y"
  currDataMapping$legendPosition <- "bottomright"
  currDataMapping$plot() 
  
 # ---- 5) MEL_Zhang2012_08mg_30d ----
  dataMappingZhang8 <- DataMapping$new()
  currDataMapping <- dataMappingZhang8
  addEggsToDataMapping(observedPath = observedData$MEL_Zhang2012$female$mean$`50mgkg`$feed$Melamine,
                       eggPart = "WholeEgg",
                       simulatedScenario = simulatedScenarios$MEL_Zhang2012_08mg_30d,
                       label = 'MEL_Zhang2012_08mg_30d',
                       dataMapping = currDataMapping,
                       numberedEggs = TRUE)

  # addLLOQLine(datamapping = currDataMapping, lloq = 20, unit = 'ng/l', label = "LLOQ_egg", group = OutputGroups$WholeEgg)
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Zhang 2012 Feed 0.8mg 30d"
  currDataMapping$log = "y"
  currDataMapping$legendPosition <- "bottomright"
  currDataMapping$plot()
  
 # ---- 6) MEL_Zhang2012_1o6mg_30d ----
  dataMappingZhang16 <- DataMapping$new()
  currDataMapping <- dataMappingZhang16
  addEggsToDataMapping(observedPath = observedData$MEL_Zhang2012$female$mean$`100mgkg`$feed$Melamine,
                       eggPart = "WholeEgg",
                       simulatedScenario = simulatedScenarios$MEL_Zhang2012_1o6mg_30d,
                       label = 'MEL_Zhang2012_1o6mg_30d',
                       dataMapping = currDataMapping,
                       numberedEggs = TRUE)
  # addLLOQLine(datamapping = currDataMapping, lloq = 20, unit = 'ng/l', label = "LLOQ_egg", group = OutputGroups$WholeEgg)
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Zhang 2012 Feed 1.6mg 30d"
  currDataMapping$log = "y"
  currDataMapping$legendPosition <- "bottomright"
  currDataMapping$plot()
  
 # ---- 7) MEL_Dong2010_100mg_21d ----
  dataMappingDong100 <- DataMapping$new()
  currDataMapping <- dataMappingDong100
  currDataMapping$addModelOutputs(paths = list(OutputPaths$VenousBlood,
                                               OutputPaths$Muscle,
                                               OutputPaths$Liver,
                                               OutputPaths$Kidney),
                                  simulationResults = simulatedScenarios$MEL_Dong2010_100mg_21d$results,
                                  
                                  labels = list(OutputGroups$VenousBlood,
                                                "Muscle",
                                                "Liver",
                                                "Kidney"),
                                  groups =  list(OutputGroups$VenousBlood,
                                                 "Muscle",
                                                 "Liver",
                                                 "Kidney")
  )
  
  
  currDataMapping$addXYData(XYData = list(observedData$MEL_Dong2010_Fig2$`Laying Hen`$female$mean$`96.19mg/kg`$Feed$Melamine$VenousBlood$Plasma,
                                                          observedData$MEL_Dong2010_Fig2$`Laying Hen`$female$mean$`96.19mg/kg`$Feed$Melamine$Muscle$Tissue,
                                                          observedData$MEL_Dong2010_Fig2$`Laying Hen`$female$mean$`96.19mg/kg`$Feed$Melamine$Liver$Tissue,
                                                          observedData$MEL_Dong2010_Fig2$`Laying Hen`$female$mean$`96.19mg/kg`$Feed$Melamine$Kidney$Tissue),
                                    groups = list(OutputGroups$VenousBlood,
                                                  "Muscle",
                                                  "Liver",
                                                  "Kidney")
  )                                
  addEggsToDataMapping(observedPath = observedData$MEL_Dong2010_eggs$LayingHen$female$mean$`102.16mg/kg`$PO$Melamine,
                       eggPart = "WholeEgg",
                       simulatedScenario = simulatedScenarios$MEL_Dong2010_100mg_21d,
                       label = 'MEL_Dong2010_100mg_21d',
                       dataMapping = currDataMapping)
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.05, unit = 'mg/l')
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.1, unit = 'mg/l', label = "LLOQ_tis", group = "LLOQ tissues")
  
  
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Dong 2010 Feed 10mg 30d"
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$xLim <- c(currDataMapping$xLim[2], 55)
  currDataMapping$yLim <- c(1e-2, currDataMapping$yLim[2])
  currDataMapping$xLim <- c(9, 45)
  currDataMapping$log = "y"
  currDataMapping$plot()
 # ---- 8) MEL_Dong2010_50mg_21d ----
  dataMappingDong50 <- DataMapping$new()
  currDataMapping <- dataMappingDong50
  currDataMapping$addModelOutputs(paths = list(OutputPaths$VenousBlood,
                                               OutputPaths$Muscle,
                                               OutputPaths$Liver,
                                               OutputPaths$Kidney),
                                  simulationResults = simulatedScenarios$MEL_Dong2010_50mg_21d$results,
                                  
                                  labels = list(OutputGroups$VenousBlood,
                                                "Muscle",
                                                "Liver",
                                                "Kidney"),
                                  groups =  list(OutputGroups$VenousBlood,
                                                 "Muscle",
                                                 "Liver",
                                                 "Kidney")
  )
  
  currDataMapping$addXYData(XYData = list(observedData$MEL_Dong2010_Fig2$`Laying Hen`$female$mean$`48.37mg/kg`$Feed$Melamine$VenousBlood$Plasma,
                                                          observedData$MEL_Dong2010_Fig2$`Laying Hen`$female$mean$`48.37mg/kg`$Feed$Melamine$Muscle$Tissue,
                                                          observedData$MEL_Dong2010_Fig2$`Laying Hen`$female$mean$`48.37mg/kg`$Feed$Melamine$Liver$Tissue,
                                                          observedData$MEL_Dong2010_Fig2$`Laying Hen`$female$mean$`48.37mg/kg`$Feed$Melamine$Kidney$Tissue),
                                    groups = list(OutputGroups$VenousBlood,
                                                  "Muscle",
                                                  "Liver",
                                                  "Kidney")
  )                                
  addEggsToDataMapping(observedPath = observedData$MEL_Dong2010_eggs$LayingHen$female$mean$`47.14mg/kg`$PO$Melamine,
                       eggPart = "WholeEgg",
                       simulatedScenario = simulatedScenarios$MEL_Dong2010_50mg_21d,
                       label = 'MEL_Dong2010_50mg_21d',
                       dataMapping = currDataMapping)
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.05, unit = 'mg/l')
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.1, unit = 'mg/l', label = "LLOQ_tis", group = "LLOQ tissues")
  
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Dong 2010 Feed 5mg 30d"
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$xLim <- c(currDataMapping$xLim[2], 55)
  currDataMapping$yLim <- c(1e-2, currDataMapping$yLim[2])
  currDataMapping$xLim <- c(9, 45)
  currDataMapping$log = "y"
  currDataMapping$plot()
  
  # ---- 7) MEL_Dong2010_100mg_21d blood ----
  dataMappingDong100bl <- DataMapping$new()
  currDataMapping <- dataMappingDong100bl
  currDataMapping$addModelOutputs(paths = list(OutputPaths$VenousBlood),
                                  simulationResults = simulatedScenarios$MEL_Dong2010_100mg_21d$results,
                                  
                                  labels = list(OutputGroups$VenousBlood),
                                  groups =  list(OutputGroups$VenousBlood)
  )
  
  
  currDataMapping$addXYData(XYData = list(observedData$MEL_Dong2010_Fig2$`Laying Hen`$female$mean$`96.19mg/kg`$Feed$Melamine$VenousBlood$Plasma),
                                    groups = list(OutputGroups$VenousBlood)
  )                                
  
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Dong 2010 Feed 10mg 30d"
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$xLim <- c(currDataMapping$xLim[2], 55)
  currDataMapping$yLim <- c(1e-2, currDataMapping$yLim[2])
  currDataMapping$xLim <- c(9, 45)
  currDataMapping$log = "y"
  currDataMapping$plot()
 # ---- 8) MEL_Dong2010_50mg_21d blood ----
  dataMappingDong50bl <- DataMapping$new()
  currDataMapping <- dataMappingDong50bl
  currDataMapping$addModelOutputs(paths = list(OutputPaths$VenousBlood),
                                  simulationResults = simulatedScenarios$MEL_Dong2010_50mg_21d$results,
                                  
                                  labels = list(OutputGroups$VenousBlood),
                                  groups =  list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$addXYData(XYData = list(observedData$MEL_Dong2010_Fig2$`Laying Hen`$female$mean$`48.37mg/kg`$Feed$Melamine$VenousBlood$Plasma),
                                    groups = list(OutputGroups$VenousBlood)
  )                                
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Dong 2010 Feed 5mg 30d"
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$xLim <- c(currDataMapping$xLim[2], 55)
  currDataMapping$yLim <- c(1e-2, currDataMapping$yLim[2])
  currDataMapping$xLim <- c(9, 45)
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 9) MEL_Gao2010_18mg_21d ----
  dataMappingGao18 <- DataMapping$new()
  currDataMapping <- dataMappingGao18
  addEggsToDataMapping(observedPath = observedData$MEL_Gao2010$female$mean$`100mgkg`$Feed$Melamine,
                       eggPart = "WholeEgg",
                       simulatedScenario = simulatedScenarios$MEL_Gao2010_18mg_21d,
                       label = 'MEL_Gao2010_18mg_21d',
                       dataMapping = currDataMapping)
  # addLLOQLine(datamapping = currDataMapping, lloq = 20, unit = 'ng/ml', label = "LLOQ_egg", group = OutputGroups$WholeEgg)
  
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Gao 2010 Feed 18mg 21d"
  currDataMapping$yLim <- c(1e-2, currDataMapping$yLim[2])
  currDataMapping$legendPosition <- "bottomright"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 10) MEL_Gao2010_9mg_21d ----
  dataMappingGao9 <- DataMapping$new()
  currDataMapping <- dataMappingGao9
  addEggsToDataMapping(observedPath = observedData$MEL_Gao2010$female$mean$`50mgkg`$Feed$Melamine,
                       eggPart = "WholeEgg",
                       simulatedScenario = simulatedScenarios$MEL_Gao2010_9mg_21d,
                       label = 'MEL_Gao2010_9mg_21d',
                       dataMapping = currDataMapping)
  # addLLOQLine(datamapping = currDataMapping, lloq = 20, unit = 'ng/ml', label = "LLOQ_egg", group = OutputGroups$WholeEgg)
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Gao 2010 Feed 9mg 21d"
  currDataMapping$yLim <- c(1e-2, currDataMapping$yLim[2])
  currDataMapping$legendPosition <- "bottomright"
  currDataMapping$log = "y"
  currDataMapping$plot()
 # ---- 11) MEL_Bai140_34d ----
  dataMappingBai140 <- DataMapping$new()
  currDataMapping <- dataMappingBai140
  currDataMapping$addModelOutputs(paths = list(OutputPaths$VenousBlood,
                                               OutputPaths$Muscle,
                                               OutputPaths$Liver,
                                               OutputPaths$Kidney,
                                               OutputPaths$Ovary
  ),
  simulationResults = simulatedScenarios$MEL_Bai_140mg_34d$results,
  
  labels = list(OutputGroups$VenousBlood,
                "Muscle",
                "Liver",
                "Kidney",
                OutputGroups$Ovary
  ),
  groups =  list(OutputGroups$VenousBlood,
                 "Muscle",
                 "Liver",
                 "Kidney",
                 OutputGroups$Ovary
  )
  )
  
  currDataMapping$addXYData(XYData = list(observedData$MEL_Bai2010_Tissues$f$mean$`140.9mgkg`$Feed$Melamine$VenousBlood$Plasma,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`140.9mgkg`$Feed$Melamine$Liver$Tissue,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`140.9mgkg`$Feed$Melamine$Kidney$Tissue,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`140.9mgkg`$Feed$Melamine$Muscle$Tissue,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`140.9mgkg`$Feed$Melamine$Ovary$Tissue
  ),
  groups = list(OutputGroups$VenousBlood,
                "Liver",
                "Kidney",
                "Muscle",
                "Ovary")
  )                                
  addEggsToDataMapping(observedPath = observedData$MEL_Bai2010_Eggs$f$mean$`140.9mgkg`$Feed$Melamine,
                       eggPart = "WholeEgg",
                       simulatedScenario = simulatedScenarios$MEL_Bai_140mg_34d,
                       label = 'MEL_Bai_140mg_34d',
                       dataMapping = currDataMapping)
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml')
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml', label = "LLOQ_tis", group = "LLOQ")
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Bai 2010 Feed 140mg/kg 34d"
  currDataMapping$log = "y"
  currDataMapping$yLim <- c(1e-1, currDataMapping$yLim[2])
  currDataMapping$xLim <- c(9, 50)
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$plot()
  
 # ---- 12) MEL_Bai66_34d ----
  dataMappingBai62 <- DataMapping$new()
  currDataMapping <- dataMappingBai62
  currDataMapping$addModelOutputs(paths = list(OutputPaths$VenousBlood,
                                               OutputPaths$Muscle,
                                               OutputPaths$Liver,
                                               OutputPaths$Kidney,
                                               OutputPaths$Ovary
  ),
  simulationResults = simulatedScenarios$MEL_Bai_62mg_34d$results,
  
  labels = list(OutputGroups$VenousBlood,
                "Muscle",
                "Liver",
                "Kidney",
                OutputGroups$Ovary
  ),
  groups =  list(OutputGroups$VenousBlood,
                 "Muscle",
                 "Liver",
                 "Kidney",
                 OutputGroups$Ovary
  )
  )
  currDataMapping$addXYData(XYData = list(observedData$MEL_Bai2010_Tissues$f$mean$`62.6mgkg`$Feed$Melamine$VenousBlood$Plasma,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`62.6mgkg`$Feed$Melamine$Liver$Tissue,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`62.6mgkg`$Feed$Melamine$Kidney$Tissue,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`62.6mgkg`$Feed$Melamine$Muscle$Tissue,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`62.6mgkg`$Feed$Melamine$Ovary$Tissue
  ),
  groups = list(OutputGroups$VenousBlood,
                "Liver",
                "Kidney",
                "Muscle",
                "Ovary"
  )
  )                                
  addEggsToDataMapping(observedPath = observedData$MEL_Bai2010_Eggs$f$mean$`62.6mgkg`$Feed$Melamine,
                       eggPart = "WholeEgg",
                       simulatedScenario = simulatedScenarios$MEL_Bai_62mg_34d,
                       label = 'MEL_Bai_62mg_34d',
                       dataMapping = currDataMapping)
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml')
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml', label = "LLOQ_tis", group = "LLOQ")
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Bai 2010 Feed 62.6mg/kg 34d"
  currDataMapping$yLim <- c(1e-1, currDataMapping$yLim[2])
  currDataMapping$log  <-  "y"
  currDataMapping$xLim <- c(9, 50)
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$plot()
  
 # ---- 13) MEL_Bai33_34d ----
  dataMappingBai33 <- DataMapping$new()
  currDataMapping <- dataMappingBai33
  currDataMapping$addModelOutputs(paths = list(OutputPaths$VenousBlood,
                                               OutputPaths$Muscle,
                                               OutputPaths$Liver,
                                               OutputPaths$Kidney,
                                               OutputPaths$Ovary
  ),
  simulationResults = simulatedScenarios$MEL_Bai_33mg_34d$results,
  
  labels = list(OutputGroups$VenousBlood,
                "Muscle",
                "Liver",
                "Kidney",
                OutputGroups$Ovary
  ),
  groups =  list(OutputGroups$VenousBlood,
                 "Muscle",
                 "Liver",
                 "Kidney",
                 OutputGroups$Ovary
  )
  )
  simulatedScenarios$MEL_Bai_33mg_34d$simulation$molWeightFor(OutputPaths$VenousBlood)
  simulatedScenarios$MEL_Poapolathep2015_IV_5o5mgkg$simulation$molWeightFor(OutputPaths$VenousBlood)
  currDataMapping$addXYData(XYData = list(observedData$MEL_Bai2010_Tissues$f$mean$`33.6mgkg`$Feed$Melamine$VenousBlood$Plasma,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`33.6mgkg`$Feed$Melamine$Liver$Tissue,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`33.6mgkg`$Feed$Melamine$Kidney$Tissue,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`33.6mgkg`$Feed$Melamine$Muscle$Tissue,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`33.6mgkg`$Feed$Melamine$Ovary$Tissue
  ),
  groups = list(OutputGroups$VenousBlood,
                "Liver",
                "Kidney",
                "Muscle",
                "Ovary"
  )
  )                                
  addEggsToDataMapping(observedPath = observedData$MEL_Bai2010_Eggs$f$mean$`33.6mgkg`$Feed$Melamine,
                       eggPart = "WholeEgg",
                       simulatedScenario = simulatedScenarios$MEL_Bai_33mg_34d,                         
                       label = 'MEL_Bai_33mg_34d',
                       dataMapping = currDataMapping)
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml')
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml', label = "LLOQ_tis", group = "LLOQ")
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Bai 2010 Feed 33.6mg/kg 34d"
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$yLim <- c(1e-1, currDataMapping$yLim[2])
  currDataMapping$xLim <- c(9, 50)
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 14) MEL_Bai17_34d ----
  dataMappingBai17 <- DataMapping$new()
  currDataMapping <- dataMappingBai17
  currDataMapping$addModelOutputs(paths = list(OutputPaths$VenousBlood,
                                               OutputPaths$Muscle,
                                               OutputPaths$Liver,
                                               OutputPaths$Kidney,
                                               OutputPaths$Ovary
  ),
  simulationResults = simulatedScenarios$MEL_Bai_17mg_34d$results,
  
  labels = list(OutputGroups$VenousBlood,
                "Muscle",
                "Liver",
                "Kidney",
                OutputGroups$Ovary
  ),
  groups =  list(OutputGroups$VenousBlood,
                 "Muscle",
                 "Liver",
                 "Kidney",
                 OutputGroups$Ovary
  )
  )
  currDataMapping$addXYData(XYData = list(observedData$MEL_Bai2010_Tissues$f$mean$`17.4mgkg`$Feed$Melamine$VenousBlood$Plasma,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`17.4mgkg`$Feed$Melamine$Liver$Tissue,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`17.4mgkg`$Feed$Melamine$Kidney$Tissue,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`17.4mgkg`$Feed$Melamine$Muscle$Tissue,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`17.4mgkg`$Feed$Melamine$Ovary$Tissue
  ),
  groups = list(OutputGroups$VenousBlood,
                "Liver",
                "Kidney",
                "Muscle",
                "Ovary"
  )
  )                                
  addEggsToDataMapping(observedPath = observedData$MEL_Bai2010_Eggs$f$mean$`17.4mgkg`$Feed$Melamine,
                       eggPart = "WholeEgg",
                       label = 'MEL_Bai_17mg_34d',
                       simulatedScenario = simulatedScenarios$MEL_Bai_17mg_34d,
                       dataMapping = currDataMapping)
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml')
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml', label = "LLOQ_tis", group = "LLOQ")
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Bai 2010 Feed 17.4mg/kg 34d"
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$yLim <- c(1e-1, currDataMapping$yLim[2])
  currDataMapping$xLim <- c(9, 50)
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 15) MEL_Bai8_34d ----
  dataMappingBai8 <- DataMapping$new()
  currDataMapping <- dataMappingBai8
  currDataMapping$addModelOutputs(paths = list(OutputPaths$VenousBlood,
                                               OutputPaths$Muscle,
                                               OutputPaths$Liver,
                                               OutputPaths$Kidney,
                                               OutputPaths$Ovary),
                                  simulationResults = simulatedScenarios$MEL_Bai_8mg_34d$results,
                                  
                                  labels = list(OutputGroups$VenousBlood,
                                                "Muscle",
                                                "Liver",
                                                "Kidney",
                                                OutputGroups$Ovary
                                  ),
                                  groups =  list(OutputGroups$VenousBlood,
                                                 "Muscle",
                                                 "Liver",
                                                 "Kidney",
                                                 OutputGroups$Ovary
                                  )
  )
  currDataMapping$addXYData(XYData = list(observedData$MEL_Bai2010_Tissues$f$mean$`8.6mgkg`$Feed$Melamine$VenousBlood$Plasma,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`8.6mgkg`$Feed$Melamine$Liver$Tissue,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`8.6mgkg`$Feed$Melamine$Kidney$Tissue,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`8.6mgkg`$Feed$Melamine$Muscle$Tissue,
                                                          observedData$MEL_Bai2010_Tissues$f$mean$`8.6mgkg`$Feed$Melamine$Ovary$Tissue
  ),
  groups = list(OutputGroups$VenousBlood,
                "Liver",
                "Kidney",
                "Muscle",
                "Ovary"
  )
  )                                
  addEggsToDataMapping(observedPath = observedData$MEL_Bai2010_Eggs$f$mean$`8.6mgkg`$Feed$Melamine,
                       eggPart = "WholeEgg",
                       simulatedScenario = simulatedScenarios$MEL_Bai_8mg_34d,
                       label = 'MEL_Bai_8mg_34d',
                       dataMapping = currDataMapping)
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml')
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml', label = "LLOQ_tis", group = "LLOQ")
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Bai 2010 Feed 8.6mgkg 34days"
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$yLim <- c(1e-1, currDataMapping$yLim[2])
  currDataMapping$xLim <- c(9, 50)
  currDataMapping$log = "y"
  currDataMapping$plot()
  
   # ---- 11) MEL_Bai140_34d blood----
  dataMappingBai140bl <- DataMapping$new()
  currDataMapping <- dataMappingBai140bl
  currDataMapping$addModelOutputs(paths = list(OutputPaths$VenousBlood),
  simulationResults = simulatedScenarios$MEL_Bai_140mg_34d$results,
  
  labels = list(OutputGroups$VenousBlood),
  groups =  list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$addXYData(XYData = list(observedData$MEL_Bai2010_Tissues$f$mean$`140.9mgkg`$Feed$Melamine$VenousBlood$Plasma),
  groups = list(OutputGroups$VenousBlood)
  )                                
  # addEggsToDataMapping(observedPath = observedData$MEL_Bai2010_Eggs$f$mean$`140.9mgkg`$Feed$Melamine,
  #                      eggPart = "WholeEgg",
  #                      simulatedScenario = simulatedScenarios$MEL_Bai_140mg_34d,
  #                      label = 'MEL_Bai_140mg_34d',
  #                      dataMapping = currDataMapping)
  # # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml')
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml', label = "LLOQ_tis", group = "LLOQ")
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Bai 2010 Feed 140mg/kg 34d"
  currDataMapping$log = "y"
  currDataMapping$yLim <- c(1e-1, currDataMapping$yLim[2])
  currDataMapping$xLim <- c(9, 50)
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$plot()
  
 # ---- 12) MEL_Bai66_34d blood----
  dataMappingBai62bl <- DataMapping$new()
  currDataMapping <- dataMappingBai62bl
  currDataMapping$addModelOutputs(paths = list(OutputPaths$VenousBlood),
  simulationResults = simulatedScenarios$MEL_Bai_62mg_34d$results,
  
  labels = list(OutputGroups$VenousBlood),
  groups =  list(OutputGroups$VenousBlood)
  )
  currDataMapping$addXYData(XYData = list(observedData$MEL_Bai2010_Tissues$f$mean$`62.6mgkg`$Feed$Melamine$VenousBlood$Plasma),
  groups = list(OutputGroups$VenousBlood)
  )                                
  # addEggsToDataMapping(observedPath = observedData$MEL_Bai2010_Eggs$f$mean$`62.6mgkg`$Feed$Melamine,
  #                      eggPart = "WholeEgg",
  #                      simulatedScenario = simulatedScenarios$MEL_Bai_62mg_34d,
  #                      label = 'MEL_Bai_62mg_34d',
  #                      dataMapping = currDataMapping)
  # # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml')
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml', label = "LLOQ_tis", group = "LLOQ")
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Bai 2010 Feed 62.6mg/kg 34d"
  currDataMapping$yLim <- c(1e-1, currDataMapping$yLim[2])
  currDataMapping$log  <-  "y"
  currDataMapping$xLim <- c(9, 50)
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$plot()
  
 # ---- 13) MEL_Bai33_34d blood----
  dataMappingBai33bl <- DataMapping$new()
  currDataMapping <- dataMappingBai33bl
  currDataMapping$addModelOutputs(paths = list(OutputPaths$VenousBlood),
  simulationResults = simulatedScenarios$MEL_Bai_33mg_34d$results,
  
  labels = list(OutputGroups$VenousBlood),
  groups =  list(OutputGroups$VenousBlood)
  )
  simulatedScenarios$MEL_Bai_33mg_34d$simulation$molWeightFor(OutputPaths$VenousBlood)
  simulatedScenarios$MEL_Poapolathep2015_IV_5o5mgkg$simulation$molWeightFor(OutputPaths$VenousBlood)
  currDataMapping$addXYData(XYData = list(observedData$MEL_Bai2010_Tissues$f$mean$`33.6mgkg`$Feed$Melamine$VenousBlood$Plasma),
  groups = list(OutputGroups$VenousBlood)
  )                                
  # addEggsToDataMapping(observedPath = observedData$MEL_Bai2010_Eggs$f$mean$`33.6mgkg`$Feed$Melamine,
  #                      eggPart = "WholeEgg",
  #                      simulatedScenario = simulatedScenarios$MEL_Bai_33mg_34d,                         
  #                      label = 'MEL_Bai_33mg_34d',
  #                      dataMapping = currDataMapping)
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml')
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml', label = "LLOQ_tis", group = "LLOQ")
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Bai 2010 Feed 33.6mg/kg 34d"
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$yLim <- c(1e-1, currDataMapping$yLim[2])
  currDataMapping$xLim <- c(9, 50)
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 14) MEL_Bai17_34d blood----
  dataMappingBai17bl <- DataMapping$new()
  currDataMapping <- dataMappingBai17bl
  currDataMapping$addModelOutputs(paths = list(OutputPaths$VenousBlood),
  simulationResults = simulatedScenarios$MEL_Bai_17mg_34d$results,
  
  labels = list(OutputGroups$VenousBlood),
  groups =  list(OutputGroups$VenousBlood)
  )
  currDataMapping$addXYData(XYData = list(observedData$MEL_Bai2010_Tissues$f$mean$`17.4mgkg`$Feed$Melamine$VenousBlood$Plasma),
  groups = list(OutputGroups$VenousBlood)
  )                                
  # addEggsToDataMapping(observedPath = observedData$MEL_Bai2010_Eggs$f$mean$`17.4mgkg`$Feed$Melamine,
  #                      eggPart = "WholeEgg",
  #                      label = 'MEL_Bai_17mg_34d',
  #                      simulatedScenario = simulatedScenarios$MEL_Bai_17mg_34d,
  #                      dataMapping = currDataMapping)
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml')
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml', label = "LLOQ_tis", group = "LLOQ")
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Bai 2010 Feed 17.4mg/kg 34d"
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$yLim <- c(1e-1, currDataMapping$yLim[2])
  currDataMapping$xLim <- c(9, 50)
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 15) MEL_Bai8_34d ----
  dataMappingBai8bl <- DataMapping$new()
  currDataMapping <- dataMappingBai8bl
  currDataMapping$addModelOutputs(paths = list(OutputPaths$VenousBlood),
                                  simulationResults = simulatedScenarios$MEL_Bai_8mg_34d$results,
                                  
                                  labels = list(OutputGroups$VenousBlood),
                                  groups =  list(OutputGroups$VenousBlood)
  )
  currDataMapping$addXYData(XYData = list(observedData$MEL_Bai2010_Tissues$f$mean$`8.6mgkg`$Feed$Melamine$VenousBlood$Plasma),
  groups = list(OutputGroups$VenousBlood)
  )                                
  # addEggsToDataMapping(observedPath = observedData$MEL_Bai2010_Eggs$f$mean$`8.6mgkg`$Feed$Melamine,
  #                      eggPart = "WholeEgg",
  #                      simulatedScenario = simulatedScenarios$MEL_Bai_8mg_34d,
  #                      label = 'MEL_Bai_8mg_34d',
  #                      dataMapping = currDataMapping)
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml')
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'µg/ml', label = "LLOQ_tis", group = "LLOQ")
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Bai 2010 Feed 8.6mgkg 34days"
  currDataMapping$legendPosition <- "bottomleft"
  currDataMapping$yLim <- c(1e-1, currDataMapping$yLim[2])
  currDataMapping$xLim <- c(9, 50)
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- summary plotting ----
  dataMappings <- c(dataMappingPoapIV,
                    dataMappingSuknikomIV,
                    dataMappingPoapPO,
                    dataMappingSuknikomPO,
                    dataMappingZhang8,
                    dataMappingZhang16,
                    dataMappingDong100,
                    dataMappingDong50,
                    dataMappingGao18,
                    dataMappingGao9,
                    dataMappingBai140,
                    dataMappingBai62,
                    dataMappingBai33,
                    dataMappingBai17,
                    dataMappingBai8
  )
  names(dataMappings) <- simulationNames
  saveNameList <- simulationNames
  
  dataMappingsChicken <- c(dataMappingPoapIV,
                           dataMappingPoapPO,
                           dataMappingDong100,
                           dataMappingDong50,
                           dataMappingBai140,
                           dataMappingBai62,
                           dataMappingBai33,
                           dataMappingBai17,
                           dataMappingBai8)  
  dataMappingsChickenBl <- c(dataMappingPoapIV,
                           dataMappingPoapPO,
                           dataMappingDong100bl,
                           dataMappingDong50bl,
                           dataMappingBai140bl,
                           dataMappingBai62bl,
                           dataMappingBai33bl,
                           dataMappingBai17bl,
                           dataMappingBai8bl)
  
  dataMappingsDuck <- c(dataMappingSuknikomIV,
                        dataMappingSuknikomPO,
                        dataMappingGao18,
                        dataMappingGao9)
  dataMappingsDuckBl <- c(dataMappingSuknikomIV,
                        dataMappingSuknikomPObl)
  
  dataMappingsQuail <- c(dataMappingZhang8,
                         dataMappingZhang16
  )
  
  
  # build big mapping with all data

  ## plot multipanel for report
  lapply(dataMappingsChicken, function(x) {x$plotType <-  PlotTypes$IndividualProfile      
  # x$legendPosition <-  "bottomleft"
  }
  )
  plotConfiguration$outputName <- "MEL - profiles chicken"
  plotConfiguration$nrOfCols <- 3
  plotMultiPanel(dataMappingsChicken, plotConfiguration = plotConfiguration)
  lapply(dataMappingsDuck, function(x) {x$plotType <-  PlotTypes$IndividualProfile      
  # x$legendPosition <-  "topright"
  }
  )
  plotConfiguration$outputName <- "MEL - profiles duck"
  plotConfiguration$nrOfCols <- 2
  plotMultiPanel(dataMappingsDuck, plotConfiguration = plotConfiguration)
  lapply(dataMappingsQuail, function(x) {x$plotType <-  PlotTypes$IndividualProfile      
  # x$legendPosition <-  "topright"
  }
  )
  plotConfiguration$outputName <- "MEL - profiles quail"
  plotConfiguration$nrOfCols <- 2
  plotMultiPanel(dataMappingsQuail, plotConfiguration = plotConfiguration)
  dataMappingAll <- collapseDataMappings(dataMappings)
  dataMappingAll$log = "y"
  # dataMappingAll$legendPosition <- "topleft"
  dataMappingAll$addLegend <-  TRUE
  dataMappingAll$plotType <-  PlotTypes$PredictedVsObserved
  
  # ---- summary plotting ----
  noOfCols <- 3
  dataMappingAll$plotType <- PlotTypes$PredictedVsObserved
  plotConfiguration$outputName <- "PVO_all"
  
  generatePlots(dataMappings = dataMappings,
                dataMappingAll = dataMappingAll,
                plotConfiguration = plotConfiguration,
                simulationNames = saveNameList)# ---- plotting with ggplot ----
  ggdata <- dataMapping2DT(dataMapping = dataMappingsChicken, molecule = "MEL")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDT1.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "MEL - PvO chicken", fileName = 'dataMappingsAsDT1.csv', legPos = "bottom")
  } else{pvo_all <- ggPVO(ggdata, legPos = "bottom", "MEL - PvO chicken")
  ggsave(plot = pvo_all,
         filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
         device = 'png',
         width = 15,
         height = 20, units = "cm", dpi = 300
  )}
 # ---- plotting with ggplot ----
  ggdata <- dataMapping2DT(dataMapping = dataMappingsDuck, molecule = "MEL")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDT2.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "MEL - PvO duck", fileName = 'dataMappingsAsDT2.csv', legPos = "bottom")
  } else{ pvo_all <- ggPVO(ggdata, "MEL - PvO duck", legPos = "bottom")
  ggsave(plot = pvo_all,
         filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
         device = 'png',
         width = 15,
         height = 20, units = "cm", dpi = 300
  )}
 # ---- plotting with ggplot ----
  ggdata <- dataMapping2DT(dataMapping = dataMappingsQuail, molecule = "MEL")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDT3.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "MEL - PvO quail", fileName = 'dataMappingsAsDT3.csv', legPos = "bottom")
  } else{
    pvo_all <- ggPVO(ggdata, "MEL - PvO quail", legPos = "bottom")
    ggsave(plot = pvo_all,
           filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
           device = 'png',
           width = 15,
           height = 20, units = "cm", dpi = 300
    )
  }
 # ---- calculate GOF measures ----
  calculateGOFMeasures(compound, m, dataMappings)
  calculateGOFMeasures(compound, m, dataMappingsChicken,devFile = file.path('..', 'Results', "chicken.txt"))
  calculateGOFMeasures(compound, m, dataMappingsChickenBl, devFile = file.path('..', 'Results', "chicken_ven.txt"))
  calculateGOFMeasures(compound, m, dataMappingsDuck,devFile = file.path('..', 'Results', "duck.txt"))
  calculateGOFMeasures(compound, m, dataMappingsDuckBl,devFile = file.path('..', 'Results', "duck_ven.txt"))
  calculateGOFMeasures(compound, m, dataMappingsQuail,devFile = file.path('..', 'Results', "quail.txt"))


 # ---- egg plots ----
 # ---- dataMappings for eggs ----
dataMappingEggsMELch <- DataMapping$new()
currDataMapping <- dataMappingEggsMELch
addEggsToDataMapping(observedPath = observedData$MEL_Bai2010_Eggs$f$mean$`140.9mgkg`$Feed$Melamine,
                     eggPart = "WholeEgg",
                     simulatedScenario = simulatedScenarios$MEL_Bai_140mg_34d,
                     label = 'MEL_Bai_140mg_34d',
                     group = "Bai 140mg",
                     dataMapping = currDataMapping)
addEggsToDataMapping(observedPath = observedData$MEL_Bai2010_Eggs$f$mean$`62.6mgkg`$Feed$Melamine,
                     eggPart = "WholeEgg",
                     simulatedScenario = simulatedScenarios$MEL_Bai_62mg_34d,
                     label = 'MEL_Bai_62o2mg_34d',
                     group = "Bai 62mg",
                     dataMapping = currDataMapping)
addEggsToDataMapping(observedPath = observedData$MEL_Bai2010_Eggs$f$mean$`33.6mgkg`$Feed$Melamine,
                     eggPart = "WholeEgg",
                     simulatedScenario = simulatedScenarios$MEL_Bai_33mg_34d,
                     group = "Bai 33mg",
                     label = 'MEL_Bai_33o6mg_34d',
                     dataMapping = currDataMapping)
addEggsToDataMapping(observedPath = observedData$MEL_Bai2010_Eggs$f$mean$`17.4mgkg`$Feed$Melamine,
                     eggPart = "WholeEgg",
                     simulatedScenario = simulatedScenarios$MEL_Bai_17mg_34d,
                     group = "Bai 17mg",
                     label = 'MEL_Bai_17o4mg_34d',
                     dataMapping = currDataMapping)
addEggsToDataMapping(observedPath = observedData$MEL_Bai2010_Eggs$f$mean$`8.6mgkg`$Feed$Melamine,
                     eggPart = "WholeEgg",
                     simulatedScenario = simulatedScenarios$MEL_Bai_8mg_34d,
                     label = 'MEL_Bai_8o6mg_34d',
                     group = "Bai 8mg",
                     dataMapping = currDataMapping)

currDataMapping$xLab <- "Time [d]"
currDataMapping$yLab <- "Concentration [µmol/l]"
currDataMapping$yUnit <- "µmol/l"
currDataMapping$xUnit <- "day(s)"
currDataMapping$title <- "MEL eggs (chicken)"
currDataMapping$yLim <- c(0.1, currDataMapping$yLim[2])

currDataMapping$log  <-  "y"
currDataMapping$legendPosition <- "bottomleft"
currDataMapping$plot()

dataMappingEggsMELdu <- DataMapping$new()
currDataMapping <- dataMappingEggsMELdu
addEggsToDataMapping(observedPath = observedData$MEL_Gao2010$female$mean$`100mgkg`$Feed$Melamine,
                     eggPart = "WholeEgg",
                     simulatedScenario = simulatedScenarios$MEL_Gao2010_18mg_21d,
                     label = 'MEL_Gao2010_18mg_21d',
                     group = "Gao 18mg",
                     dataMapping = currDataMapping)
addEggsToDataMapping(observedPath = observedData$MEL_Gao2010$female$mean$`50mgkg`$Feed$Melamine,
                     eggPart = "WholeEgg",
                     simulatedScenario = simulatedScenarios$MEL_Gao2010_9mg_21d,
                     label = 'MEL_Gao2010_9mg_21d',
                     group = "Gao 9mg",
                     dataMapping = currDataMapping)
currDataMapping$xLab <- "Time [d]"
currDataMapping$yLab <- "Concentration [µmol/l]"
currDataMapping$yUnit <- "µmol/l"
currDataMapping$xUnit <- "day(s)"
currDataMapping$title <- "MEL eggs (duck)"
currDataMapping$yLim <- c(1e-3, currDataMapping$yLim[2])

currDataMapping$log = "y"
currDataMapping$plot()


dataMappingEggsMELqu <- DataMapping$new()
currDataMapping <- dataMappingEggsMELqu
addEggsToDataMapping(observedPath = observedData$MEL_Zhang2012$female$mean$`100mgkg`$feed$Melamine,
                     eggPart = "WholeEgg",
                     simulatedScenario = simulatedScenarios$MEL_Zhang2012_1o6mg_30d,
                     label = 'MEL_Zhang2012_1o6mg_30d',
                     group = "Zhang 1.6mg",
                     numberedEggs = TRUE,
                     dataMapping = currDataMapping)
addEggsToDataMapping(observedPath = observedData$MEL_Zhang2012$female$mean$`50mgkg`$feed$Melamine,
                     eggPart = "WholeEgg",
                     simulatedScenario = simulatedScenarios$MEL_Zhang2012_08mg_30d,
                     label = 'MEL_Zhang2012_08mg_30d',
                     group = "Zhang 0.8mg",
                     numberedEggs = TRUE,
                     dataMapping = currDataMapping)

currDataMapping$xLab <- "Time [d]"
currDataMapping$yLab <- "Concentration [µmol/l]"
currDataMapping$yUnit <- "µmol/l"
currDataMapping$xUnit <- "day(s)"
currDataMapping$title <- "MEL eggs (quail)"
currDataMapping$yLim <- c(currDataMapping$yLim[1]*.1, currDataMapping$yLim[2])

currDataMapping$log = "y"
currDataMapping$legendPosition <-  "bottomleft"
currDataMapping$plot()

return(list(chicken = dataMappingEggsMELch, quail = dataMappingEggsMELqu , duck = dataMappingEggsMELdu))
  
}

plotMON <- function(plotConfiguration, simulatedScenarios, observedData, compound, c, m, simulationNames){
  cmpout <-  plotConfiguration$outputFolder
  plotConfiguration$outputFolder <- file.path(plotConfiguration$outputFolder, '../../doseNormPlots')
  # ---- dose normalised observed data ----
  dataMappingDoseNormIV <- DataMapping$new()
  currDataMapping <- dataMappingDoseNormIV
  currDataMapping$addXYData(
    XYData = list(observedData$MON_Henri2009_import$mixed$mean$`0.46 mgkg`$IV$Monensin$VenousBlood$Plasma),
    groups = list("Henri 0.46mgkg IV")
  )
  
  currDataMapping$addXYData(
    XYData = list(observedData$MON_Atef1993_Fig$female$mean$`40mgkg`$IV$Monensin$VenousBlood$Plasma),
    groups = list("Atef 40mgkg IV")
  )

  currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("b", currDataMapping$xySeriesCount))
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "normalised concentration"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "MON IV dose normalised"
  yfactors <-  c( 1/40, 1/0.46)/1.49
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){
    currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors[i])
  })
  currDataMapping$log <-  "y"
  currDataMapping$legendPosition <- "right"
  currDataMapping$plot()
  
  # plotConfiguration$width <-  10
  # plotConfiguration$height <-  10
  plotConfiguration$outputName <- currDataMapping$title
  plotConfiguration$nrOfCols <- 1
  plotMultiPanel(currDataMapping,plotConfiguration)
  
  dataMappingDoseNormPO <- DataMapping$new()
  currDataMapping <- dataMappingDoseNormPO
 
  currDataMapping$addXYData(
    XYData = list(observedData$MON_Henri2009_import$mixed$mean$`4mgkg`$PO$Monensin$VenousBlood$Plasma),
    groups = list("Henri 4mgkg IC")
  )
  
  currDataMapping$addXYData(
    XYData = list(observedData$MON_Atef1993_Fig$female$mean$`40mgkg`$IC$Monensin$VenousBlood$Plasma),
    groups = list("Atef 40mgkg IC")
  )
  
  # currDataMapping$addXYData(
  #   XYData = list(observedData$MON_Atef1993_Table6$female$mean$`120mgkg`$Feed$Monensin$VenousBlood$Plasma
  #   ),
  #   groups = list("Atef 24mg feed")
  # )
  
  # currDataMapping$addXYData(
  #   XYData = list(observedData$MON_Henri2009_Table3_import$residuals$unknown$mean$`125mgkg of feed`$PO$Monensin$VenousBlood$Plasma
  #   ),
  #   groups = list("Henri 25mg feed")
  # )
  
  currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("b", currDataMapping$xySeriesCount))
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "normalised concentration"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "MON PO dose normalised"
  yfactors <-  c(1/40, 
                 # 1/25/1.36, 
                 1/4#, 
                 # 1/24/1.36 
                 )/1.49
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){
    currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors[i])
  })
  currDataMapping$log <-  "y"
  currDataMapping$legendPosition <- "topright"
  currDataMapping$plot()
  
  # plotConfiguration$width <-  10
  # plotConfiguration$height <-  10
  plotConfiguration$outputName <- currDataMapping$title
  plotConfiguration$nrOfCols <- 1
  plotMultiPanel(currDataMapping,plotConfiguration)
  
  # ---- feed dose normalised ----
  dataMappingDoseNormFeed <- DataMapping$new()
  currDataMapping <- dataMappingDoseNormFeed
  
  currDataMapping$addXYData(
    XYData = list(observedData$MON_Atef1993_Table6$female$mean$`120mgkg`$Feed$Monensin$VenousBlood$Plasma
    ),
    groups = list("Atef 24mg feed")
  )
  
  currDataMapping$addXYData(
    XYData = list(observedData$MON_Henri2009_Table3_import$residuals$unknown$mean$`125mgkg of feed`$PO$Monensin$VenousBlood$Plasma
    ),
    groups = list("Henri 25mg feed")
  )
  
  currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("b", currDataMapping$xySeriesCount))
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "normalised concentration"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "MON feed dose normalised"
  yfactors <-  c( 1/25/1.36, 1/24/1.36)/1.49
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){
    currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors[i])
  })
  currDataMapping$log <-  "y"
  currDataMapping$legendPosition <- "topright"
  currDataMapping$plot()
  
  # plotConfiguration$width <-  10
  # plotConfiguration$height <-  10
  plotConfiguration$outputName <- currDataMapping$title
  plotConfiguration$nrOfCols <- 1
  plotMultiPanel(currDataMapping,plotConfiguration)
  plotConfiguration$nrOfCols <- 2
  
  plotConfiguration$outputName <- "MON - dose normalisation"
  
  plotMultiPanel(list(dataMappingDoseNormIV,dataMappingDoseNormPO,dataMappingDoseNormFeed),plotConfiguration)
  
 # ---- 1) MON_Henri2012_IV_046mgkg ----
  plotConfiguration$outputFolder <-  cmpout
  
  dataMappingHenriIV <- DataMapping$new()
  currDataMapping <- dataMappingHenriIV
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$MON_Henri2012_IV_046mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 2.5, unit = 'µg/ml')
  currDataMapping$addXYData(
    XYData = list(observedData$MON_Henri2009_import$mixed$mean$`0.46 mgkg`$IV$Monensin$VenousBlood$Plasma),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Henri 2009 IV 0.46mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot() 
  
 # ---- 2) MON_Atef1993_IV_40mgkg ----
  dataMappingAtefIV <- DataMapping$new()
  currDataMapping <- dataMappingAtefIV
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$MON_Atef1993_IV_40mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.1, unit = 'µg/ml', label = "LLOD", group = "LLOD")
  currDataMapping$addXYData(
    XYData = list(observedData$MON_Atef1993_Fig$female$mean$`40mgkg`$IV$Monensin$VenousBlood$Plasma),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Atef 1993 IV 40mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot() 
  
 # ---- 3) MON_Henri2012_IC_4mgkg ----
  dataMappingHenriPO <- DataMapping$new()
  currDataMapping <- dataMappingHenriPO
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$MON_Henri2012_IC_4mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 2.5, unit = 'µg/ml')
  currDataMapping$addXYData(
    XYData = list(observedData$MON_Henri2009_import$mixed$mean$`4mgkg`$PO$Monensin$VenousBlood$Plasma),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Henri 2009 IC 4mg/kg"

  currDataMapping$log = "y"
  currDataMapping$plot() 
  
 # ---- 4) MON_Atef1993_IC_40mgkg ----
  dataMappingAtefPO <- DataMapping$new()
  currDataMapping <- dataMappingAtefPO
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$MON_Atef1993_IC_40mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.1, unit = 'µg/ml', label = "LLOD", group = "LLOD")
  currDataMapping$addXYData(
    XYData = list(observedData$MON_Atef1993_Fig$female$mean$`40mgkg`$IC$Monensin$VenousBlood$Plasma),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Atef 1993 IC 40mg/kg"

  currDataMapping$log = "y"
  currDataMapping$plot() 
  
 # ---- 5) MON_Atef1993_24mg_14d - tissue could be added ----
  dataMappingAtefmult <- DataMapping$new()
  currDataMapping <- dataMappingAtefmult
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood,
                 OutputPaths$Fat,
                 OutputPaths$Kidney,
                 OutputPaths$Liver,
                 OutputPaths$Muscle,
                 OutputPaths$Heart
                 
    ),
    simulationResults = simulatedScenarios$MON_Atef1993_24mg_14d$results,
    
    labels = list(OutputGroups$VenousBlood,
                  "Fat",
                  "Kidney",
                  "Liver",
                  "Muscle",
                  "Heart") ,       
    groups = list(OutputGroups$VenousBlood,
                  "Fat",
                  "Kidney",
                  "Liver",
                  "Muscle",
                  "Heart")      
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.1, unit = 'µg/ml', label = "LLOD", group = "LLOD")
  currDataMapping$addXYData(
    XYData = list(observedData$MON_Atef1993_Table6$female$mean$`120mgkg`$Feed$Monensin$VenousBlood$Plasma,
                          observedData$MON_Atef1993_Table6$female$mean$`120mgkg`$Feed$Monensin$Fat$Tissue,
                          observedData$MON_Atef1993_Table6$female$mean$`120mgkg`$Feed$Monensin$Kidney$Tissue,
                          observedData$MON_Atef1993_Table6$female$mean$`120mgkg`$Feed$Monensin$Liver$Tissue,
                          observedData$MON_Atef1993_Table6$female$mean$`120mgkg`$Feed$Monensin$Muscle$Tissue,
                          observedData$MON_Atef1993_Table6$female$mean$`120mgkg`$Feed$Monensin$Heart$Tissue
                          
    ),
    groups = list(OutputGroups$VenousBlood,
                  "Fat",
                  "Kidney",
                  "Liver",
                  "Muscle",
                  "Heart")
  )
  lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -14)
  currDataMapping$xLim <- c(0, 24)
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Atef 1993 Feed 24mg 14d"
  currDataMapping$log = "y"
  currDataMapping$plot() 
  
  # ---- 5) MON_Atef1993_24mg_14d - blood ----
  dataMappingAtefmultbl <- DataMapping$new()
  currDataMapping <- dataMappingAtefmultbl
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$MON_Atef1993_24mg_14d$results,
    
    labels = list(OutputGroups$VenousBlood) ,       
    groups = list(OutputGroups$VenousBlood)      
  )
  
  currDataMapping$addXYData(
    XYData = list(observedData$MON_Atef1993_Table6$female$mean$`120mgkg`$Feed$Monensin$VenousBlood$Plasma),
    groups = list(OutputGroups$VenousBlood)
  )
  lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -14)
  currDataMapping$xLim <- c(0, 24)
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Atef 1993 Feed 24mg 14d"
  currDataMapping$log = "y"
  currDataMapping$plot() 
  
 # ---- 6) MON_Henri2012_25mg_33d ----
  dataMappingHenrimult <- DataMapping$new()
  currDataMapping <- dataMappingHenrimult
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood,
                 OutputPaths$Fat,
                 OutputPaths$Liver,
                 OutputPaths$Muscle
    ),
    simulationResults = simulatedScenarios$MON_Henri2012_25mg_33d$results,
    
    labels = list(OutputGroups$VenousBlood,
                  "Fat",
                  "Liver",
                  "Muscle"),
    groups = list(OutputGroups$VenousBlood,
                  "Fat",
                  "Liver",
                  "Muscle")
  )
  
  # addLLOQLine(datamapping = currDataMapping, lloq = 2.5, unit = 'µg/ml')
  # addLLOQLine(datamapping = currDataMapping, lloq = 2.5, unit = 'ng/ml', label = "lloq", group = OutputGroups$Fat)
  # addLLOQLine(datamapping = currDataMapping, lloq = 2.5, unit = 'ng/ml', label = "lloq", group = OutputGroups$Muscle)
  # addLLOQLine(datamapping = currDataMapping, lloq = 1, unit = 'ng/ml', label = "lloq", group = OutputGroups$Liver)
  currDataMapping$addXYData(
    XYData = list(observedData$MON_Henri2009_Table3_import$residuals$unknown$mean$`125mgkg of feed`$PO$Monensin$VenousBlood$Plasma,
                          observedData$MON_Henri2009_Table3_import$residuals$unknown$mean$`125mgkg of feed`$PO$Monensin$Fat$Tissue,
                          observedData$MON_Henri2009_Table3_import$residuals$unknown$mean$`125mgkg of feed`$PO$Monensin$Liver$Tissue,
                          observedData$MON_Henri2009_Table3_import$residuals$unknown$mean$`125mgkg of feed`$PO$Monensin$Muscle$Tissue
    ),
    groups = list(OutputGroups$VenousBlood,
                  "Fat",
                  "Liver",
                  "Muscle")
  )
  lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -33)
  
  currDataMapping$xLim <- c(0, 24)
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Henri 2009 Feed 25mg 33d"
  currDataMapping$log = "y"
  currDataMapping$plotType <- PlotTypes$IndividualProfile
  currDataMapping$plot() 
  
  # ---- 6) MON_Henri2012_25mg_33d blood----
  dataMappingHenrimultbl <- DataMapping$new()
  currDataMapping <- dataMappingHenrimultbl
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood
    ),
    simulationResults = simulatedScenarios$MON_Henri2012_25mg_33d$results,
    
    labels = list(OutputGroups$VenousBlood),
    groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$addXYData(
    XYData = list(observedData$MON_Henri2009_Table3_import$residuals$unknown$mean$`125mgkg of feed`$PO$Monensin$VenousBlood$Plasma    ),
    groups = list(OutputGroups$VenousBlood)
  )
  lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -33)
  
  currDataMapping$xLim <- c(0, 24)
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Henri 2009 Feed 25mg 33d"
  currDataMapping$log = "y"
  currDataMapping$plotType <- PlotTypes$IndividualProfile
  currDataMapping$plot() 
  
 # ---- 7) MON_Vandenberge2012_1o75mg_14d ----
  dataMappingVand <- DataMapping$new()
  currDataMapping <- dataMappingVand
  addEggsToDataMapping(observedPath = observedData$MON_Vandenberge2012_Fig3$`Laying hen`$f$mean$`0.1`$Feed$Monensin,
                       eggPart = "WholeEgg",
                       simulatedScenario = simulatedScenarios$MON_Vandenberge2012_1o75mg_14d,
                       dataMapping = currDataMapping
  )
  lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -9)
  # addLLOQLine(datamapping = currDataMapping, lloq = 0.5, unit = 'mg/l', label = "LLOQ", group = OutputGroups$WholeEgg, MW = 670.871)
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Vandenberge 2012 \n Feed 1.75mg 14d"
  currDataMapping$xLim <- c(0, 24)
  currDataMapping$yLim <- c(1e-5, 1e-1)
  
  currDataMapping$log = "y"
  currDataMapping$plot() 


 # ---- generate Plots ----
  dataMappings <- c(dataMappingHenriIV,
                    dataMappingAtefIV,
                    dataMappingHenriPO,
                    dataMappingAtefPO,
                    dataMappingHenrimult,
                    dataMappingAtefmult,
                    dataMappingVand
  )
  dataMappingsBl <- c(dataMappingHenriIV,
                    dataMappingAtefIV,
                    dataMappingHenriPO,
                    dataMappingAtefPO,
                    dataMappingHenrimultbl,
                    dataMappingAtefmultbl
  )
  saveNameList <-  simulationNames[c(1,2,3,4,5,6,7)]
  
  # build big mapping with all data
  dataMappingAll <- collapseDataMappings(dataMappings)
  dataMappingAll$log = "y"
  dataMappingAll$legendPosition <- "topleft"
  dataMappingAll$addLegend <-  TRUE
  dataMappingAll$plotType <-  PlotTypes$PredictedVsObserved

 # ---- summary plotting ----

  generatePlots(dataMappings = dataMappings, 
                dataMappingAll = dataMappingAll, 
                plotConfiguration = plotConfiguration, 
                simulationNames = saveNameList)    
  plotConfiguration$outputName <- "MON profiles"
  plotConfiguration$nrOfCols <- 3
  lapply(dataMappings, function(x) {x$plotType <-  PlotTypes$IndividualProfile      
  x$legendPosition <-  "topright"}
  )
  plotMultiPanel(dataMappings, plotConfiguration = plotConfiguration)
  
 # ---- plotting with ggplot ----
  ggdata <- dataMapping2DT(dataMapping = dataMappings, molecule = "MON")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDT.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "MON - PvO chicken", legPos = "bottom")
  } else{pvo_all <- ggPVO(ggdata, "MON - PvO chicken", legPos = "bottom", limits = c(1e-3, 1e1))
  ggsave(plot = pvo_all,
         filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
         device = 'png',
         width = 15,
         height = 20, units = "cm", dpi = 300
  )
  }
 # ---- calculate GOF measures ----
  calculateGOFMeasures(c, m, dataMappings)
  calculateGOFMeasures(c, m, dataMappings, devFile = file.path('..', 'Results', "chicken.txt"))
  calculateGOFMeasures(c, m, dataMappingsBl, devFile = file.path('..', 'Results', "chicken_ven.txt"))
  


 # ---- egg plot ----
dataMappingEggsMON <- DataMapping$new()
currDataMapping <- dataMappingEggsMON

addEggsToDataMapping(observedPath = observedData$MON_Vandenberge2012_Fig3$`Laying hen`$f$mean$`0.1`$Feed$Monensin,
                     eggPart = "WholeEgg",
                     simulatedScenario = simulatedScenarios$MON_Vandenberge2012_1o75mg_14d,
                     dataMapping = currDataMapping,
                     group = "Vandenberge Feed 1.75mg"
                     
)
lapply(currDataMapping$xySeries, shiftDataMapping, timeshift = -9)
currDataMapping$xLab <- "Time [d]"
currDataMapping$yLab <- "Concentration [µmol/l]"
currDataMapping$yUnit <- "µmol/l"
currDataMapping$xUnit <- "day(s)"
currDataMapping$title <- "MON eggs (chicken)"
currDataMapping$xLim <- c(0, 24)
currDataMapping$yLim <- c(1e-5, 1e-1)

currDataMapping$log = "y"
currDataMapping$plot() 
return(currDataMapping)  
  
}

plotSAL <- function(plotConfiguration, simulatedScenarios, observedData, compound, c, m, simulationNames){
  
 # ---- dose normalised observed data ----
  cmpout <-  plotConfiguration$outputFolder
  plotConfiguration$outputFolder <- file.path(plotConfiguration$outputFolder, '../../doseNormPlots')
  
  dataMappingNormIV <- DataMapping$new()
  currDataMapping <- dataMappingNormIV

  currDataMapping$addXYData(
    XYData = list(
      observedData$SAL_Henri2012$mixed$mean$`0.25mgkg`$IV$Salinomycin$VenousBlood$Plasma
    ),
    groups = list("Henri IV")
  )      
  currDataMapping$addXYData(
    XYData = list(
      observedData$SAL_Atef1993$IV$unknown$mean$`20mgkg`$IV$Salinomycin$VenousBlood$Plasma
    ),
    groups = list("Atef IV")
  )

  
  currDataMapping$setXOffsets(labels = "SAL_Henri2012_Feed.mixed.mean.10mg.Feed.Salinomycin.VenousBlood.Plasma", xOffsets = -35*24)
  currDataMapping$setXOffsets(labels = "SAL_Atef1993.Feed_residuals.unknown.mean.6mg.Feed.Salinomycin.VenousBlood.Plasma", xOffsets = -14*24)
  
  currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("b", currDataMapping$xySeriesCount))
  currDataMapping$xLim <- c(0,12)
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "normalised concentration"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "SAL IV dose normalised"
  
  yfactors <-  c(1/0.25, 1/20)/1.33
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){
    currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors[i])
  })
  currDataMapping$log <-  "y"
  currDataMapping$legendPosition <- "topright"
  currDataMapping$plot()
  
  # plotConfiguration$width <-  10
  # plotConfiguration$height <-  10

  plotConfiguration$outputName <- currDataMapping$title
  plotConfiguration$nrOfCols <- NULL
  plotMultiPanel(currDataMapping, plotConfiguration)
  
  # ---- dose normalised observed data ----
 
  dataMappingNormPO <- DataMapping$new()
  currDataMapping <- dataMappingNormPO
  

  currDataMapping$addXYData(
    XYData = list(
      observedData$SAL_Henri2012$mixed$mean$`2.5mgkg`$IC$Salinomycin$VenousBlood$Plasma
    ),
    groups = list("Henri PO")
  )
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$SAL_Atef1993$IC$unknown$mean$`20mgkg`$IC$Salinomycin$VenousBlood$Plasma
    ),
    groups = list("Atef PO")
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$SAL_Henri2012_Feed$mixed$mean$`10mg`$Feed$Salinomycin$VenousBlood$Plasma
    ),
    groups = list("Henri feed")
  )
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$SAL_Atef1993$Feed_residuals$unknown$mean$`6mg`$Feed$Salinomycin$VenousBlood$Plasma
    ),
    groups = list("Atef feed")
  )
  
  currDataMapping$setXOffsets(labels = "SAL_Henri2012_Feed.mixed.mean.10mg.Feed.Salinomycin.VenousBlood.Plasma", xOffsets = -35*24)
  currDataMapping$setXOffsets(labels = "SAL_Atef1993.Feed_residuals.unknown.mean.6mg.Feed.Salinomycin.VenousBlood.Plasma", xOffsets = -14*24)
  
  
  currDataMapping$setLinetypes(labels = names(currDataMapping$xySeries), rep("solid", currDataMapping$xySeriesCount))
  currDataMapping$setTypes(labels = names(currDataMapping$xySeries), rep("b", currDataMapping$xySeriesCount))
  currDataMapping$xLim <- c(0,12)
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "normalised concentration"
  currDataMapping$yDimension <- "Concentration (molar)"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "SAL PO dose normalised"
  yfactors <-  c( 1/(10/1.3), 1/2.5, 1/(6/1.3), 1/20)/1.33
  lapply(seq(1,currDataMapping$xySeriesCount), function(i){
    currDataMapping$setYFactors(labels = currDataMapping$xySeries[[i]]$label, yFactors = yfactors[i])
  })
  
  currDataMapping$log <-  "y"
  currDataMapping$legendPosition <- "topright"
  currDataMapping$plot()
  
  # plotConfiguration$width <-  10
  # plotConfiguration$height <-  10
  
  plotConfiguration$outputName <- currDataMapping$title
  plotConfiguration$nrOfCols <- 1
  plotMultiPanel(currDataMapping, plotConfiguration)
  
  
  plotConfiguration$nrOfCols <- 2
  plotConfiguration$outputName <- "SAL - dose normalisation"
  
  plotMultiPanel(list(dataMappingNormIV,dataMappingNormPO),plotConfiguration)
 # ---- 1) Henri IV 0.25mgkg ----
 plotConfiguration$outputFolder <-  cmpout
  dataMappingHenriIV <- DataMapping$new()
  currDataMapping <- dataMappingHenriIV
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$SAL_Henri2012_IV_025mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  currDataMapping$addXYData(
    XYData = list(
      observedData$SAL_Henri2012$mixed$mean$`0.25mgkg`$IV$Salinomycin$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(lloq = 2.5, unit = "µg/l", datamapping = currDataMapping)
  
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Henri 2012 IV 0.25mg/kg"
  
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 2) Henri IC 25mgkg ----
  dataMappingHenriIC <- DataMapping$new()
  currDataMapping <- dataMappingHenriIC
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$SAL_Henri2012_IC_25mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(lloq = 2.5, unit = "µg/l", datamapping = currDataMapping)
  currDataMapping$addXYData(
    XYData = list(
      observedData$SAL_Henri2012$mixed$mean$`2.5mgkg`$IC$Salinomycin$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Henri 2012 IC 2.5mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
  
 # ---- 3) Henri feed 10mg 35 d ----
  dataMappingHenriFeed <- DataMapping$new()
  currDataMapping <- dataMappingHenriFeed
  currDataMapping$addModelOutputs(
    paths = list(
      OutputPaths$VenousBlood,
      OutputPaths$Muscle,
      OutputPaths$Liver,
      OutputPaths$Fat
    ),
    simulationResults = simulatedScenarios$SAL_Henri2012_10mg_35d$results,
    
    labels = list(OutputGroups$VenousBlood,
                  "Muscle",
                  "Liver",
                  "Fat"),
    groups = list(OutputGroups$VenousBlood,
                  "Muscle",
                  "Liver",
                  "Fat")
  )
  
  # addLLOQLine(lloq = 2.5, unit = "µg/l", datamapping = currDataMapping)
  # addLLOQLine(lloq = 1, unit = "µg/l", datamapping = currDataMapping, label = "lloq_liv", group = OutputGroups$Liver)
  # addLLOQLine(lloq = 2.5, unit = "µg/l", datamapping = currDataMapping, label = "lloq_fat", group = OutputGroups$Fat)
  # addLLOQLine(lloq = 2.5, unit = "µg/l", datamapping = currDataMapping, label = "lloq_muscle", group = OutputGroups$Muscle)
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$SAL_Henri2012_Feed$mixed$mean$`10mg`$Feed$Salinomycin$VenousBlood$Plasma,
      observedData$SAL_Henri2012_Feed$mixed$mean$`10mg`$Feed$Salinomycin$Muscle$Tissue,
      observedData$SAL_Henri2012_Feed$mixed$mean$`10mg`$Feed$Salinomycin$Liver$Tissue,
      observedData$SAL_Henri2012_Feed$mixed$mean$`10mg`$Feed$Salinomycin$Fat$Tissue
      
      
    ),
    groups = list(OutputGroups$VenousBlood,
                  "Muscle",
                  "Liver",
                  "Fat")
  )
  
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Henri 2012 Feed 10mg 35d"
  currDataMapping$log = "y"
  currDataMapping$xLim <- c(33,39)
  currDataMapping$legendPosition <-  "topright"
  
  currDataMapping$plot()
  # ---- 3) Henri feed 10mg 35 d blood----
  
  dataMappingHenriFeedbl <- DataMapping$new()
  currDataMapping <- dataMappingHenriFeedbl
  currDataMapping$addModelOutputs(
    paths = list(
      OutputPaths$VenousBlood
    ),
    simulationResults = simulatedScenarios$SAL_Henri2012_10mg_35d$results,
    
    labels = list(OutputGroups$VenousBlood),
    groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$SAL_Henri2012_Feed$mixed$mean$`10mg`$Feed$Salinomycin$VenousBlood$Plasma),
    groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Henri 2012 Feed 10mg 35d"
  currDataMapping$log = "y"
  currDataMapping$xLim <- c(33,39)
  currDataMapping$legendPosition <-  "topright"
  
  currDataMapping$plot()
 # ---- 4) Atef IV 20mgkg ----
  dataMappingAtefIV <- DataMapping$new()
  currDataMapping <- dataMappingAtefIV
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$SAL_Atef1993_IV_20mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(lloq = 0.1, unit = "µg/ml", datamapping = currDataMapping)
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$SAL_Atef1993$IV$unknown$mean$`20mgkg`$IV$Salinomycin$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$xLab <- "Time [h]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "h"
  currDataMapping$title <- "Atef 1993 IV 20mg/kg"
  currDataMapping$log = "y"
  currDataMapping$plot()
 # ---- 5) Atef IC 20mgkg ----
  dataMappingAtefIC <- DataMapping$new()
  currDataMapping <- dataMappingAtefIC
  currDataMapping$addModelOutputs(
    paths = list(OutputPaths$VenousBlood),
    simulationResults = simulatedScenarios$SAL_Atef1993_IC_20mgkg$results,
    
    labels = list(OutputGroups$VenousBlood),
        groups = list(OutputGroups$VenousBlood)
  )
  # addLLOQLine(lloq = 0.1, unit = "µg/ml", datamapping = currDataMapping)
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$SAL_Atef1993$IC$unknown$mean$`20mgkg`$IC$Salinomycin$VenousBlood$Plasma
    ),
        groups = list(OutputGroups$VenousBlood)
  )
  
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Atef 1993 IC 20mg/kg"
  currDataMapping$log = "y"
  currDataMapping$legendPosition <- "bottomright"
  currDataMapping$plot()
  
 # ---- 6)Atef IC 20mgkg residuals ----
  dataMappingAtefICres <- DataMapping$new()
  currDataMapping <- dataMappingAtefICres
  currDataMapping$addModelOutputs(
    paths = list(
      OutputPaths$VenousBlood,
      OutputPaths$Muscle,
      OutputPaths$Liver,
      OutputPaths$Fat,
      OutputPaths$Kidney,
      OutputPaths$Skin,
      OutputPaths$Heart
    ),
    simulationResults = simulatedScenarios$SAL_Atef1993_IC_20mgkg$results,
    
    labels = list(
      OutputGroups$VenousBlood,
      "Muscle",
      "Liver",
      "Fat",
      "Kidney",
      "Skin",
      "Heart"
    ),
    groups = list(
      OutputGroups$VenousBlood,
      "Muscle",
      "Liver",
      "Fat",
      "Kidney",
      "Skin",
      "Heart"
    )
  )
  # addLLOQLine(lloq = 0.1, unit = "µg/ml", datamapping = currDataMapping)
  
  currDataMapping$addXYData(
    XYData = list(
      observedData$SAL_Atef1993$IC_residuals$unknown$mean$`20mgkg`$IC$Salinomycin$VenousBlood$Plasma,
      observedData$SAL_Atef1993$IC_residuals$unknown$mean$`20mgkg`$IC$Salinomycin$Muscle$Tissue,
      observedData$SAL_Atef1993$IC_residuals$unknown$mean$`20mgkg`$IC$Salinomycin$Liver$Tissue,
      observedData$SAL_Atef1993$IC_residuals$unknown$mean$`20mgkg`$IC$Salinomycin$Fat$Tissue,
      observedData$SAL_Atef1993$IC_residuals$unknown$mean$`20mgkg`$IC$Salinomycin$Kidney$Tissue,
      observedData$SAL_Atef1993$IC_residuals$unknown$mean$`20mgkg`$IC$Salinomycin$Skin$Tissue,
      observedData$SAL_Atef1993$IC_residuals$unknown$mean$`20mgkg`$IC$Salinomycin$Heart$Tissue
    ),
    groups = list(
      OutputGroups$VenousBlood,
      "Muscle",
      "Liver",
      "Fat",
      "Kidney",
      "Skin",
      "Heart"
    )
  )
  
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Atef 1993 IC 20mgkg residues "
  currDataMapping$log = "y"
  currDataMapping$legendPosition <- "bottomright"
  currDataMapping$plot()
  
  # ---- 7)Atef feed 6mg ----
  dataMappingAtefFeed <- DataMapping$new()
  currDataMapping <- dataMappingAtefFeed
  
  currDataMapping$addModelOutputs(
    paths = list(
      OutputPaths$VenousBlood,
      OutputPaths$Muscle,
      OutputPaths$Liver,
      OutputPaths$Fat,
      OutputPaths$Kidney,
      OutputPaths$Skin,
      OutputPaths$Heart
    ),
    simulationResults = simulatedScenarios$SAL_Atef1993_6mg_14d$results,
    
    labels = list(
      OutputGroups$VenousBlood,
      "Muscle",
      "Liver",
      "Fat",
      "Kidney",
      "Skin",
      "Heart"
    ),
    groups = list(
      OutputGroups$VenousBlood,
      "Muscle",
      "Liver",
      "Fat",
      "Kidney",
      "Skin",
      "Heart"
    )
  )
  # addLLOQLine(lloq = 0.1, unit = "µg/ml", datamapping = currDataMapping)
  currDataMapping$addXYData(
    XYData = list(
      observedData$SAL_Atef1993$Feed_residuals$unknown$mean$`6mg`$Feed$Salinomycin$VenousBlood$Plasma,
      observedData$SAL_Atef1993$Feed_residuals$unknown$mean$`6mg`$Feed$Salinomycin$Muscle$Tissue,
      observedData$SAL_Atef1993$Feed_residuals$unknown$mean$`6mg`$Feed$Salinomycin$Liver$Tissue,
      observedData$SAL_Atef1993$Feed_residuals$unknown$mean$`6mg`$Feed$Salinomycin$Fat$Tissue,
      observedData$SAL_Atef1993$Feed_residuals$unknown$mean$`6mg`$Feed$Salinomycin$Kidney$Tissue,
      observedData$SAL_Atef1993$Feed_residuals$unknown$mean$`6mg`$Feed$Salinomycin$Skin$Tissue,
      observedData$SAL_Atef1993$Feed_residuals$unknown$mean$`6mg`$Feed$Salinomycin$Heart$Tissue
    ),
    groups = list(
      OutputGroups$VenousBlood,
      "Muscle",
      "Liver",
      "Fat",
      "Kidney",
      "Skin",
      "Heart"
    )
  )
  currDataMapping$xLim <- c(13, 16)
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Atef 1993 Feed 6mg 14d"
  currDataMapping$log = "y"
  currDataMapping$legendPosition <- "bottomright"
  
  currDataMapping$plot()
  
  # ---- 7)Atef feed 6mg blood----
  dataMappingAtefFeedbl <- DataMapping$new()
  currDataMapping <- dataMappingAtefFeedbl
  
  currDataMapping$addModelOutputs(
    paths = list(
      OutputPaths$VenousBlood
    ),
    simulationResults = simulatedScenarios$SAL_Atef1993_6mg_14d$results,
    
    labels = list(
      OutputGroups$VenousBlood
    ),
    groups = list(
      OutputGroups$VenousBlood
    )
  )
  # addLLOQLine(lloq = 0.1, unit = "µg/ml", datamapping = currDataMapping)
  currDataMapping$addXYData(
    XYData = list(
      observedData$SAL_Atef1993$Feed_residuals$unknown$mean$`6mg`$Feed$Salinomycin$VenousBlood$Plasma
    ),
    groups = list(
      OutputGroups$VenousBlood
    )
  )
  currDataMapping$xLim <- c(13, 16)
  currDataMapping$yLim
  currDataMapping$xLab <- "Time [d]"
  currDataMapping$yLab <- "Concentration [µmol/l]"
  currDataMapping$yUnit <- "µmol/l"
  currDataMapping$xUnit <- "day(s)"
  currDataMapping$title <- "Atef 1993 Feed 6mg 14d"
  currDataMapping$log = "y"
  currDataMapping$legendPosition <- "bottomright"
  
  currDataMapping$plot()
  
 # ---- summary ----
  dataMappings <- c(
    dataMappingHenriIV,
    dataMappingAtefIV,
    dataMappingHenriIC,
    dataMappingAtefICres,
    dataMappingHenriFeed,
    # dataMappingAtefIC,
    dataMappingAtefFeed
  )
  dataMappingsBl <- c(
    dataMappingHenriIV,
    dataMappingAtefIV,
    dataMappingHenriIC,
    dataMappingAtefIC,
    dataMappingHenriFeedbl,
    # dataMappingAtefIC,
    dataMappingAtefFeedbl
  )
  saveNameList <- simulationNames[c(1,2,3,4,5,6)]
  ## plot multipanel for report
  plotConfiguration$outputName <- "SAL profiles"
  plotConfiguration$nrOfCols <- 3
  lapply(dataMappings, function(x) {x$plotType <-  PlotTypes$IndividualProfile      
  # x$legendPosition <-  "topright"
  }
  )
  mw <-  751
  lloqs <-  list(henriIV =1/mw,
                 atefIV =0.1/mw*1000,
                 henriPO =1/mw,
                 atefPO =0.1/mw*1000,
                 henriFeed =1/mw,
                 atefFeed =0.1/mw*1000
  )
  plotMultiPanel(dataMappings, 
                 plotConfiguration = plotConfiguration, 
                 lloqs= lloqs)
  # build big mapping with all data
  dataMappingAll <- collapseDataMappings(dataMappings)
  dataMappingAll$log = "y"
  dataMappingAll$legendPosition <- "topleft"
  dataMappingAll$addLegend <-  TRUE
  dataMappingAll$plotType <-  PlotTypes$PredictedVsObserved


 # ---- summary plotting ----

  generatePlots(
    dataMappings = dataMappings,
    # dataMappingsAUC = dataMappingsAUC,
    dataMappingAll = dataMappingAll,
    plotConfiguration = plotConfiguration,
    simulationNames = saveNameList
  )
  
 # ---- plotting with ggplot ----
  ggdata <- dataMapping2DT(dataMapping = dataMappings, molecule = "SAL")
  write.table(ggdata, file = file.path(plotConfiguration$outputFolder, 'dataMappingsAsDT.csv'))
  if(length(compound) == 2 && c == compound[[2]]){
    plotPVOwithSameLimits(compound, m, "SAL - PvO chicken", legPos = "bottom")
  } else{pvo_all <- ggPVO(ggdata, "SAL - PvO chicken", legPos = "bottom")
  ggsave(plot = pvo_all,
         filename = file.path(plotConfiguration$outputFolder, paste0(pvo_all$labels$title, '.png')),
         device = 'png',
         width = 15,
         height = 20, units = "cm", dpi = 300
  )
  }
 # ---- calculate GOF measures ----
  calculateGOFMeasures(c, m, dataMappings)
  calculateGOFMeasures(c, m, dataMappings, devFile = file.path('..', 'Results', "chicken.txt"))
  calculateGOFMeasures(c, m, dataMappingsBl, devFile = file.path('..', 'Results', "chicken_ven.txt"))
  
}

