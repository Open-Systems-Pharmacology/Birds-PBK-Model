# ---- functions for plotting ----
addLLOQLine <-  function(lloq, unit, datamapping = currDataMapping, label = "lloq_venbl", group= OutputGroups$VenousBlood, MW = NULL){
  
  xMin <- datamapping$xLim[[1]]
  xMax <- datamapping$xLim[[2]]
  lloqxy <- XYData$new(xVals = c(xMin, xMax) + abs(c(xMin, xMax)) * c(-0.1, 0.1) , 
                       yVals = rep(lloq,2), label = label)
  
  
  lloqxy$dataType <- XYDataTypes$Simulated
  lloqxy$xDimension <- "Time"
  lloqxy$yDimension <- "Concentration (mass)"
  lloqxy$xUnit <- datamapping$xUnit
  lloqxy$yUnit <- unit
  
  if(!is.null(MW)){
    lloqxy$MW <-  MW
  }else{
    lloqxy$MW <- datamapping$xySeries[[1]]$MW
  }
  lloqxy$lty <- 2
  lloqxy$type <- 'l'
  
  if(!group %in% OutputGroups){
    lloqxy$color <-  'black'
  }
  datamapping$addXYData(XYData = lloqxy, 
                        groups = group)
}

getIndexClosestToValue <- function(value, array) {
  idx <- which(abs(array - value) == min(abs(array - value)))
  return(idx)
}

nearestNeighbour <- function(x) {
  x <- data.table(x[[1]], x[[2]], x[[3]])
  names(x) <- c('xValues', 'dataType', 'yValues')
  sim <- x[dataType == "Simulated"]
  idx <-
    unlist(lapply(x[dataType == "Observed", xValues - 10], getIndexClosestToValue,  sim[, xValues]))
  simTime <- sim[idx, xValues]
  conc <- sim[idx, yValues]
  expTime <- x[dataType == "Observed", xValues]
  result <- data.table(simTime, expTime, conc)
  return(result)
}

preparePVO <-  function(plotdata){
  pred <-  plotdata[dataType == "Simulated"]
  obs <- plotdata[dataType == "Observed"]
  setkey(pred, 'title', 'molecule', 'grouping', 'xValues')
  setkey(obs, 'title', 'molecule', 'grouping', 'xValues')
  predvsobs <-  pred[obs, roll = 'nearest']
  minValPvO <-
    min(c(predvsobs[yValues > 1e-5, yValues], predvsobs[i.yValues > 1e-5, i.yValues]))
  maxValPvO <-
    max(c(predvsobs[yValues > 1e-5, yValues], predvsobs[i.yValues > 1e-5, i.yValues]))
  limits <- c(minValPvO, maxValPvO)
  return(list(predvsobs, limits))
}

ggPVO <- function(plotdata, name, legPos = 'right', limits = NULL) {
  output_levels <- names(OutputColours)
  plotdata[, ('grouping') := factor(get('grouping'), 
                                    levels = output_levels)]
  preproc <- preparePVO(plotdata)
  predvsobs <- preproc[[1]]
  
  if(is.null(limits)){
    minVal <-
      floor(log10(min(plotdata[
        yValues >1e-8, yValues]))) -1
    maxVal <-
      round(log10(max(plotdata[
        yValues >1e-8, yValues]))) +1
    minValPvO <-preproc[[2]][[1]]
    maxValPvO <-preproc[[2]][[2]]
    
  } else {
    minVal <-  floor(log10(limits[[1]]))-3
    maxVal <-  round(log10(limits[[2]]))+3
    minValPvO <-  limits[[1]]
    maxValPvO <-  limits[[2]]
  }
  
  numShapes <- length(unique(plotdata[,title]) )
  startShape <-  3
  shapeList <-  c(15,17,18,19,3,7,8,9,10,11,4,12,14,6)
  
  colScale <- scale_colour_manual(name = "Measurement",
                                  values = lapply(levels(droplevels(predvsobs$grouping)), 
                                                  function(x) OutputColours[[x]]))
  
  unitline <- 10^(seq(-12,maxVal+1,1))
  ul <- data.table(unitline)
  y_pvo_label <- 'Predicted Values [µmol/l]'
  x_pvo_label <- 'Observed Values [µmol/l]'
  
  pvoPlot <- ggplot() +
    geom_point(
      data = predvsobs[yValues > 1e-8 & i.yValues > 1e-8],
      size = 4,
      aes(
        x = i.yValues,
        y = yValues,
        colour = interaction(molecule, grouping, sep = ' in '),
        shape = interaction(title, sep = ', ')
      )
    ) +
    colScale +
    scale_shape_manual(values= shapeList[1:numShapes]) +
    labs(colour = "Measurement") +
    labs(shape = "Scenario") +
    labs(title = name) +
    geom_line(data = ul ,
              aes(x = unitline, y = unitline),
              linetype = 1,
    ) +
    geom_line(data = ul,
              aes(x = unitline, y = 3 * unitline),
              linetype = 2) +
    geom_line(data = ul,
              aes(x = unitline, y = 1/3 * unitline),
              linetype = 2) +    
    geom_line(data = ul,
              aes(x = unitline, y = 10 * unitline),
              linetype = 3) +
    geom_line(data = ul,
              aes(x = unitline, y = 1/10 * unitline),
              linetype = 3) +
    xlab(x_pvo_label) +
    ylab(y_pvo_label) +
    scale_y_log10(
      limits = c(10 ^ minVal, 10 ^ maxVal),
      breaks = 10 ^ (minVal:maxVal),
      labels = trans_format("log10", math_format(10 ^ .x)),
    ) +
    scale_x_log10(
      limits = c(10 ^ minVal, 10 ^ maxVal),
      breaks = 10 ^ (minVal:maxVal),
      labels = trans_format("log10", math_format(10 ^ .x)),
    ) +
    coord_cartesian(xlim=c(minValPvO*0.9,maxValPvO*1.1), ylim=c(minValPvO*0.9,maxValPvO*1.1))+
    theme_bw() +
    theme(
      panel.grid.minor.y = element_blank(),
      legend.position = legPos,
      legend.direction="vertical",
      legend.text = element_text(size = 10),
      text = element_text(size = 10),
      axis.text = element_text(size = 10, color = 'black'),
      plot.title = element_text(hjust = 0.5, face = 'bold'),
      aspect.ratio = 1
    )
  print(pvoPlot)
  return(pvoPlot)
}

plotPVOwithSameLimits <- function(compound, m, title, fileName = "dataMappingsAsDT.csv", legPos = "bottom"){
  pvodata1 <- as.data.table(read.table(file = file.path(getwd(),'..','Results', compound[[1]] , m, fileName)))
  pvodata2 <- as.data.table(read.table(file = file.path(getwd(),'..','Results', compound[[2]] , m, fileName)))
  limits <- c(
    min(c(preparePVO(pvodata1)[[2]][[1]], preparePVO(pvodata2)[[2]][[1]])),
    max(c(preparePVO(pvodata1)[[2]][[2]], preparePVO(pvodata2)[[2]][[2]]))
  )
  pvo_all1 <- ggPVO(pvodata1, paste0(title, ' (', m, ')'), legPos = legPos, limits = limits)
  pvo_all2 <- ggPVO(pvodata2, paste0(title, ' (', m, ')'), legPos = legPos, limits = limits)
  ggsave(plot = pvo_all1,
         filename = file.path(getwd(),'..','Results', compound[[1]] , m, paste0(pvo_all1$labels$title, '_sameLimits.png')),
         device = 'png',
         width = 15,
         height = 20, units = "cm", dpi = 300
  )
  ggsave(plot = pvo_all2,
         filename = file.path(getwd(),'..','Results', compound[[2]] , m, paste0(pvo_all2$labels$title, '_sameLimits.png')),
         device = 'png',
         width = 15,
         height = 20, units = "cm", dpi = 300
  )}

plotAll <- function(dataMappings, compoundName, simulationNames){
  plotConfiguration <- PlotConfiguration$new()
  plotConfiguration$outputDevice <- runConfiguration$outputDevice
  plotConfiguration$outputName <- "Chicken"
  plotConfiguration$outputFolder <- file.path(runConfiguration$outputFolder, compoundName)
  plotConfiguration$addTitle <- FALSE
  plotConfiguration$nrOfCols <- NULL
  plotConfiguration$outputDevice <- "PNG"
  
  #Example to plot each data mapping separately
  tmp <- lapply(dataMappings, function(x){x$plot()})
  
  # time profile plots
  lapply(dataMappings, function(x) {x$addLegend <- TRUE} )
  lapply(dataMappings, function(x) {x$plotType <- PlotTypes$IndividualProfile} )
  lapply(seq_along(dataMappings), 
         function(i) {plotConfiguration$outputName <- simulationNames[i] 
         plotMultiPanel(dataMappings[i], plotConfiguration)})
  
  # PVO plots
  lapply(dataMappings, function(x) {x$plotType <- PlotTypes$PredictedVsObserved} )
  lapply(seq_along(dataMappings), 
         function(i) {plotConfiguration$outputName <- paste0("PVO_",simulationNames[i]) 
         plotMultiPanel(dataMappings[i], plotConfiguration)})  
}

generatePlots <-  function(dataMappings, dataMappingsAUC = NULL, dataMappingAll = NULL, plotConfiguration, simulationNames) {
  
  plotConfiguration$nrOfCols <- 1
  # ---- conc profiles ----
  lapply(dataMappings, function(x) {
    x$plotType <- PlotTypes$IndividualProfile
    x$legendPosition <- "topright"
    x$addLegend <- TRUE
  })
  lapply(seq_along(dataMappings),
         function(i) {
           plotConfiguration$outputName <- simulationNames[i]
           plotMultiPanel(dataMappings[i], plotConfiguration)
         })
  # ---- PVO plots ---- 
  lapply(dataMappings, function(x) {
    x$plotType <- PlotTypes$PredictedVsObserved
    x$legendPosition <- "topleft"
  })
  lapply(seq_along(dataMappings),
         function(i) {
           plotConfiguration$outputName <- paste0("PVO_", simulationNames[i])
           plotMultiPanel(dataMappings[i], plotConfiguration, foldDistance = c(3, 10))
         })
  
  # ---- PVO AUC ---- 
  if (!is.null(dataMappingsAUC)) {
    dataMappingsAUC$legendPosition <- "topleft"
    plotConfiguration$pointsize <-  7
    plotConfiguration$outputName <- "PVO_AUC"
    plotMultiPanel(dataMappingsAUC, plotConfiguration, foldDistance = c(3, 10))
  }
  # ---- PVO all ---- 
  if (!is.null(dataMappingAll)) {
    dataMappingAll$log = "y"
    dataMappingAll$legendPosition <- "topleft"
    dataMappingAll$plotType <- PlotTypes$PredictedVsObserved
    plotConfiguration$outputName <- "PVO_all"
    plotMultiPanel(dataMappingAll, plotConfiguration, foldDistance = c(3, 10))
  }
}

# ---- functions to prepare dataMappings for ggplot ----

dataMapping2Df <- function(dataMapping, molecule = 'undefined') {
  
  variabs <- c(
    'xValues',
    'yValues',
    'yDimension',
    'xDimension',
    'xUnit',
    'yUnit',
    'yError',
    'yErrorUnit',
    'dataType',
    'Compartment',
    'molecule',
    'Organ',
    'Species',
    'MW'
  )
  dataMappingAsDf <- NULL
  tmp2 <-  NULL
  for (m in dataMapping) {
    currDM <- m
    for (x in currDM$xySeries) {
      dm <-  list()
      dm <-  lapply(variabs, function(v) { x[[v]] })
      names(dm) <- variabs
      if(x$dataType == "Observed"){
        dm$xValues <- toUnit(quantityOrDimension = x$xDimension,
                             values = x$xValues,
                             targetUnit = "min",
                             molWeight = x$MW,
                             sourceUnit = x$xUnit,
                             molWeightUnit = "g/mol"
        )
        
        dm$yValues <-  toUnit(quantityOrDimension = x$yDimension,
                              values = x$yValues,
                              targetUnit = "µmol/l",
                              molWeight = x$MW,
                              sourceUnit = x$yUnit,
                              molWeightUnit = "g/mol"
        )
        
        if(!is.null(x$yErrorDimension)){
          dm$yError <-  toUnit(quantityOrDimension = x$yErrorDimension,
                               values = x$yError,
                               targetUnit = "µmol/l",
                               molWeight = x$MW,
                               sourceUnit = x$yErrorUnit,
                               molWeightUnit = "g/mol"
          )
        }
        dm$xDimension <- "Time"
        dm$xUnit <- "min"
        dm$yUnit <- "µmol/l"
        dm$yDimension <- "Concentration (molar)"
        dm$yErrorUnit <- "µmol/l"
      }
      
      tmp <- data.frame(cbind(dm[[variabs[[1]]]], dm[[variabs[[2]]]]))
      names(tmp) <- c(variabs[[1]], variabs[[2]])
      for (v in variabs[3:length(variabs)]) {
        if( is.null(dm[[v]])){
          if( v == 'molecule'){
            tmp[[v]] <- molecule
            
          }else{
            tmp[[v]] <- NA
          }
        } else{
          tmp[[v]] <- dm[[v]]
        }
      }
    
      tmp$grouping <- m$getXYSeriesGroupMap()[[(x$label)]]
      tmp$title <- m$title
      if(is.null(tmp2)){
        tmp2 <- tmp
      } else{
        tmp2 <- rbind(tmp2, tmp)
      }
    }
    if(is.null(dataMappingAsDf)){
      dataMappingAsDf <- tmp2
    } else{
      dataMappingAsDf <- rbind(dataMappingAsDf, tmp2)
    }
  }
  return(dataMappingAsDf)
}

dataMapping2DT <-  function(dataMapping, molecule = "undefined"){
  
  return(as.data.table(dataMapping2Df(dataMapping, molecule)))
}

collapseDataMappings <-  function(dataMappingList) {
  dataMappingList <-  dataMappingList
  dataMappingAll <- DataMapping$new()
  counter <-  0
  for (m in seq_along(dataMappingList)) {
    currDatMap <- dataMappingList[[m]]
    for (g in seq_along(currDatMap$getXYSeriesGroupMap())) {
      counter <- counter + 1
      currXY <- currDatMap$xySeries[[names(currDatMap$getXYSeriesGroupMap()[g])]]
      copiedxy <- XYData$new(xVals = toUnit(quantityOrDimension = currXY$xDimension,
                                            values = currXY$xValues,
                                            targetUnit = "min",
                                            molWeight = currXY$MW,
                                            sourceUnit = currXY$xUnit,
                                            molWeightUnit = "g/mol"
      ),
      yVals = toUnit(quantityOrDimension = currXY$yDimension,
                     values = currXY$yValues,
                     targetUnit = "µmol/l",
                     molWeight = currXY$MW,
                     sourceUnit = currXY$yUnit,
                     molWeightUnit = "g/mol"
      ),
      label = paste(currXY$label, as.character(m), sep = '_')
      )
      
      copiedxy$dataType <- currXY$dataType
      copiedxy$xDimension <- "Time"
      copiedxy$yDimension <- "Concentration (molar)"
      copiedxy$xUnit <- "min"
      copiedxy$yUnit <- "µmol/l"
      copiedxy$pch <- 21
      dataMappingAll$addXYData(XYData = (copiedxy),
                               groups = paste(currDatMap$getXYSeriesGroupMap()[g], as.character(m), sep = '_')
      )
    }
  }
  return(dataMappingAll)
}

# ---- functions for GOF criteria ----

pointsWithinFoldChange <- function (dataMappingList, foldChange) {
  if (foldChange <= 0) {
    error(messages$errorMustBePositive(foldChange))
  }
  valuePairs <- retrieveValuePairs(dataMappingList)
  devs <- valuePairs$yhat/valuePairs$y
  n <- length(devs)
  pif <- sum(devs <= foldChange & devs >= 1/foldChange, na.rm = TRUE)
  return(list(n = n, foldChange = foldChange, pif = pif, perc = round((pif)/n * 
                                                                        100, digits = 2)))
}

retrieveValuePairs <- function (dataMappingList) {
  dataMappingList <- toList(dataMappingList)
  y <- c()
  yhat <- c()
  for (dataMapping in dataMappingList) {
    for (grouping in dataMapping$groupings) {
      dataPointsX <- c()
      dataPointsY <- c()
      simulatedResults <- list()
      for (dataName in grouping) {
        xySeries <- dataMapping$xySeries[[dataName]]
        if (xySeries$dataType == XYDataTypes$Simulated) {
          simulatedResults <- append(simulatedResults, 
                                     xySeries)
          next
        }
        dataPointsX <- c(dataPointsX, xySeries$xValuesProcessed(dataMapping$xUnit))
        dataPointsY <- c(dataPointsY, xySeries$yValuesProcessed(dataMapping$yUnit))
      }
      for (simulatedResult in simulatedResults) {
        simulatedPointsX <- simulatedResult$xValuesProcessed(dataMapping$xUnit)
        simulatedPointsY <- simulatedResult$yValuesProcessed(dataMapping$yUnit)
        for (i in seq_along(dataPointsX)) {
          idx <- getIndexClosestToValue(dataPointsX[[i]], 
                                        (simulatedPointsX))
          for (pointIdx in idx) {
            y <- c(y, dataPointsY[[i]])
            yhat <- c(yhat, simulatedPointsY[[pointIdx]])
          }
        }
      }
    }
  }
  return(list(y = y, yhat = yhat))
}

calculateGOFMeasures <-  function(c, m, dataMappings, f1 = 3, f2 = 10, devFile = file.path('..', 'Results',"dev.txt"), 
                                  rmsefile = file.path('..', 'Results',"RMSE.txt")){
  rmse <- calculateRMSE(dataMappings, 20)
  dev10 <- pointsWithinFoldChange(dataMappings, 10) 
  dev3 <- pointsWithinFoldChange(dataMappings, 3)
  write(x = paste0(c, " (", m, "): ", rmse), file = rmsefile, append=TRUE)
  write(x = paste0(c, " (", m, "):\n",
                   '\t Deviation < ', dev3$foldChange, '-fold: ', dev3$pif, " out of total ", dev3$n, " points.(", dev3$perc, "%)\n",
                   '\t Deviation < ', dev10$foldChange, '-fold: ', dev10$pif, " out of total ", dev10$n, " points.(", dev10$perc, "%)\n"),
        file = devFile,
        append=TRUE)
}

# ---- functions for egg plotting ----

addEggsToDataMapping <-  function(observedPath, eggPart = "WholeEgg", label = "", group = "", simulatedScenario, dataMapping = NULL, numberedEggs = FALSE) {
  
  if(eggPart == "WholeEgg"){
    eggPath = "DefaultCompound|Whole Egg"
    eggGroup = "WholeEgg"
  } else if (eggPart == "Yolk"){
    eggPath = "Yolk|Intracellular|DefaultCompound|Concentration in container"
    eggGroup = "Yolk"
    
  } else if (eggPart == "Albumen"){
    eggPath = "Albumen|Intracellular|DefaultCompound|Concentration in container"
    eggGroup = "Albumen"
    
  } else {
    stop("Enter valid egg component: WholeEgg (default), Albumen, or Yolk" )
  }
  
  eggs <- list()
  teggsnum <- list()
  
  for (i in 1:9) {
    currEgg <- observedPath[[paste0('Egg ', as.character(i))]][[eggPart]]
    
    if(is.null(currEgg)){
      if(i>1){
        eggs[[i]] <- observedPath[[paste0('Egg ', as.character(i-1))]][[eggPart]]
      }else{
        eggs[[i]] <- observedPath[[paste0('Egg ', as.character(i+1))]][[eggPart]]
      }
    }else{
      eggs[[i]] <- currEgg
    }
  }
  teggsnum <-  lapply(eggs, function(x) length(x$xValues))
  teggs <-  lapply(eggs, function(x) (x$xValues))
  valeggs <-  lapply(eggs, function(x) (x$yValues))
  
  allTimeIndices <- list()
  allEggValues <- list()
  endtime <- simulatedScenario$outputValues$data$Time[[length(simulatedScenario$outputValues$data$Time)]]/60/24
  
  for (i in seq_along(eggs)) {
    if(!is.null(eggs[[i]])){
      teggsmin <- lapply(teggs[i],function(x) x*calcTimeFactor(eggs[[i]]$xUnit)-10)
      if(numberedEggs){
        idx_egg <- lapply(unlist(teggsmin), FUN = function(x){getIndexClosestToValue(x, array = simulatedScenario$outputValues$data$Time )}) 
      }else{
        idx_egg <- lapply(seq(9+(i-1) - 0.1, endtime, 9) * 60 * 24, getIndexClosestToValue, array = simulatedScenario$outputValues$data$Time)
        
      }
      veggs <- simulatedScenario$outputValues$data[[paste0("Organism|Egg ", as.character(i), '|' , eggPath)]] [unlist(idx_egg)]
      allTimeIndices[[i]] <- simulatedScenario$outputValues$data$Time[unlist(idx_egg)]
      allEggValues[[i]] <- veggs
    }
  }
  sortedEggs <- sort.int(unlist(allTimeIndices),index.return = TRUE)
  simulatedEggs <- XYData$new(xVals = sortedEggs$x, 
                              yVals = (unlist(allEggValues))[(sortedEggs$ix)],
                              label = paste0(label, eggPart )
  )
  simulatedEggs$dataType <- XYDataTypes$Simulated
  simulatedEggs$xDimension <- "Time"
  simulatedEggs$yDimension <- "Concentration (molar)"
  simulatedEggs$xUnit <- "min"
  simulatedEggs$yUnit <- "µmol/l"
  simulatedEggs$type <-  "S"
  simulatedEggs$lty <- 1
  simulatedEggs$MW <- unlist(observedPath[[1]])[[1]]$MW
  if(!is.null(dataMapping)) {
    if(group == ""){
      group <- eggGroup
    }
    dataMapping$addXYData(XYData = simulatedEggs,
                          groups = group)
    dataMapping$addXYData(
      XYData = unlist(eggs) ,
      groups = rep(group,length(unlist(eggs)))
    )
    for(e in eggs){
      if(!is.null(e)){
        x <- dataMapping$xySeries[[e$label]]
        x$pch <-  16
        x$lty <-  1
        
      }
    }
    
  }
  return(simulatedEggs)
}

eggs2points <-  function(observedPath, eggPart = "WholeEgg", simulatedScenario) {
  
  if(eggPart == "WholeEgg"){
    eggPath = "DefaultCompound|Whole Egg"
  } else if (eggPart == "Yolk"){
    eggPath = "Yolk|Intracellular|DefaultCompound|Concentration in container"
  } else if (eggPart == "Albumen"){
    eggPath = "Albumen|Intracellular|DefaultCompound|Concentration in container"
  } else {
    stop("Enter valid egg component: WholeEgg (default), Albumen, or Yolk" )
  }
  
  e1 <- observedPath$`Egg 1`[[eggPart]]
  e2 <- observedPath$`Egg 2`[[eggPart]]
  e3 <- observedPath$`Egg 3`[[eggPart]]
  e4 <- observedPath$`Egg 4`[[eggPart]]
  e5 <- observedPath$`Egg 5`[[eggPart]]
  e6 <- observedPath$`Egg 6`[[eggPart]]
  e7 <- observedPath$`Egg 7`[[eggPart]]
  e8 <- observedPath$`Egg 8`[[eggPart]]
  e9 <- observedPath$`Egg 9`[[eggPart]]
  
  teggsnum <- c(length(e1$xValues),
                length(e2$xValues),
                length(e3$xValues),
                length(e4$xValues),
                length(e5$xValues),
                length(e6$xValues),
                length(e7$xValues),
                length(e8$xValues),
                length(e9$xValues))
  
  teggs <- c(e1$WholeEggWholeEgg$xValues, e2$xValues, e3$xValues, e4$xValues, e5$xValues, e6$xValues, e7$xValues, e8$xValues, e9$xValues)
  cs_teggs <- cumsum(teggsnum)
  idx_egg <- lapply(X = teggs*calcTimeFactor(e1$xUnit), FUN = function(x){getIndexClosestToValue(x, array = simulatedScenario$outputValues$data$Time )})
  veggs <- c(simulatedScenario$outputValues$data[[paste0("Organism|Egg 1|" , eggPath)]] [unlist(idx_egg[seq(1, cs_teggs[1], 1)])],
             simulatedScenario$outputValues$data[[paste0("Organism|Egg 2|" , eggPath)]] [unlist(idx_egg[seq(cs_teggs[1]+1, cs_teggs[2], 1)])],
             simulatedScenario$outputValues$data[[paste0("Organism|Egg 3|" , eggPath)]] [unlist(idx_egg[seq(cs_teggs[2]+1, cs_teggs[3], 1)])],
             simulatedScenario$outputValues$data[[paste0("Organism|Egg 4|" , eggPath)]] [unlist(idx_egg[seq(cs_teggs[3]+1, cs_teggs[4], 1)])],
             simulatedScenario$outputValues$data[[paste0("Organism|Egg 5|" , eggPath)]] [unlist(idx_egg[seq(cs_teggs[4]+1, cs_teggs[5], 1)])],
             simulatedScenario$outputValues$data[[paste0("Organism|Egg 6|" , eggPath)]] [unlist(idx_egg[seq(cs_teggs[5]+1, cs_teggs[6], 1)])],
             simulatedScenario$outputValues$data[[paste0("Organism|Egg 7|" , eggPath)]] [unlist(idx_egg[seq(cs_teggs[6]+1, cs_teggs[7], 1)])],
             simulatedScenario$outputValues$data[[paste0("Organism|Egg 8|" , eggPath)]] [unlist(idx_egg[seq(cs_teggs[7]+1, cs_teggs[8], 1)])],
             simulatedScenario$outputValues$data[[paste0("Organism|Egg 9|" , eggPath)]] [unlist(idx_egg[seq(cs_teggs[8]+1, cs_teggs[9], 1)])])
  simulatedEggs <- XYData$new(xVals = simulatedScenario$outputValues$data$Time[unlist(idx_egg)], 
                              yVals = veggs, label = "eggs" )
  simulatedEggs$dataType <- XYDataTypes$Simulated
  simulatedEggs$xDimension <- "Time"
  simulatedEggs$yDimension <- "Concentration (molar)"
  simulatedEggs$xUnit <- "min"
  simulatedEggs$yUnit <- "µmol/l"
  return(simulatedEggs)
}

calcTimeFactor <-  function(xunit){
  switch(xunit, 
         "h" = {
           timefac <- 60
         },
         "day(s)" = {
           timefac <- 24*60
         },
         "min" = {
           timefac <- 1
         },
         {
           xunit
           stop("unknown time unit")
         }
  )
  return(timefac)
}

shiftDataMapping <-  function(x, timeshift){
  if( x$xUnit == "day(s)"){
    x$xOffset <- timeshift
  }else if(x$xUnit == "min"){
    if(x$dataType == "Simulated"){
      x$xValues[[1]] <- -timeshift*24*60
    }
    x$xOffset <- timeshift*24*60
    
  }else if(x$xUnit == "h"){
    x$xOffset <- timeshift*24
  }
}
