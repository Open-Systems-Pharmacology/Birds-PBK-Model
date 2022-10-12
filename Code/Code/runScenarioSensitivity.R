library(readxl)
library(dplyr)
library(ggplot2)
library(forcats)

### filters and plots the exportes SA
filterSAForPlot <-  function(dfSA, output, pkParam, sortColumn, sortValue, threshold = 0.1, saveName = "SA"){
  
  dfSort <- dfSA %>%
    filter(.[[sortColumn]] %in% sortValue) %>% #View()
     filter(`PK-Parameter` %in% pkParam) %>%
    select(`PK-Parameter`, absoluteValue, Parameter, Output)
  
  dfSA_n <- merge(dfSA, dfSort, by = c('PK-Parameter', 'Parameter', 'Output'), all.y = TRUE)
  names(dfSA_n)[names(dfSA_n) == 'absoluteValue.y'] <- 'sortedAbsValue'
  names(dfSA_n)[names(dfSA_n) == 'absoluteValue.x'] <- 'absoluteValue'
  
  dfSA_n <- dfSA_n %>% 
    filter(Output %in% output)
 
  sensitivityAnalysis_pkParam <- dfSA_n %>% 
    filter(Output %in% output) %>%
    # filter(`PK-Parameter` %in% pkParam) %>% 
    filter(absoluteValue > threshold) #%>%
    # mutate(Value =, Value < 0.1, 0)
  params <- unique(sensitivityAnalysis_pkParam$Parameter)
  
  dfSA_n$include <-FALSE 
  dfSA_n$include[dfSA_n$Parameter %in% params] <-TRUE 
  dfSA_n <- dfSA_n[order(dfSA_n$sortedAbsValue,decreasing = FALSE),]
  dfSA_n$Parameter <- factor(dfSA_n$Parameter, levels = unique(dfSA_n$Parameter))

  
  gplot <- dfSA_n %>% 
    filter(include) %>%
    filter(!is.na(Parameter)) %>%
    filter(!(Parameter %in% c("Egg 1-t_lag", "Egg 1-t_maxGrowth"))) %>%
    filter(!is.na(`Parameter Path`)) %>%
    ggplot(aes(x = Parameter, y=Value, fill = species)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_hue(direction = 1) +
    coord_flip() +
    theme_gray() +
    theme(text = element_text(size = 18))
  ggsave(filename = paste0("../Results/SA/",saveName, output, pkParam, ".png"), width = 44, height = 29, units = "cm")
  return(gplot)
}


### read the exported SAs; add the species column and merge them

#MEL
# sensitivity_AnalysisChicken <- read_xlsx("../Results/SA/SA_chicken_MEL_IV.xlsx")
# sensitivity_AnalysisDuck <- read_xlsx("../Results/SA/SA_duck_MEL_IV.xlsx")
# sensitivity_AnalysisQuail <- read_xlsx("../Results/SA/SA_quail_MEL_IV.xlsx")
# saveName <- "SA_MEL"



#FFC
sensitivity_AnalysisChicken <- read_xlsx("../Results/SA/SA_chicken_FFC.xlsx")
sensitivity_AnalysisDuck <- read_xlsx("../Results/SA/SA_duck_FFC.xlsx")
sensitivity_AnalysisQuail <- read_xlsx("../Results/SA/SA_quail_FFC.xlsx")
saveName <- "SA_FFC_"

# for all

sensitivity_AnalysisChicken$absoluteValue <- abs(sensitivity_AnalysisChicken$Value)
sensitivity_AnalysisDuck$absoluteValue <- abs(sensitivity_AnalysisDuck$Value)
sensitivity_AnalysisQuail$absoluteValue <- abs(sensitivity_AnalysisQuail$Value)
sensitivity_AnalysisChicken$species <- 'chicken'
sensitivity_AnalysisDuck$species <- "duck"
sensitivity_AnalysisQuail$species <- "quail"
sensitivityAnalysis <-  rbind(sensitivity_AnalysisChicken,sensitivity_AnalysisDuck)
sensitivityAnalysis <-  rbind(sensitivityAnalysis,sensitivity_AnalysisQuail)

### replace the output name for readability
sensitivityAnalysis <- sensitivityAnalysis %>%
  mutate(Output = replace(Output, Output == "Organism|VenousBlood|Plasma|DefaultCompound|Concentration in container", "Venous Blood Plasma"))


### plot SA
threshold <- 0.1

sortColumn <- "species"
sortValue <- "chicken"
sensitivityAnalysis_AUCVB_plot <- filterSAForPlot(dfSA = sensitivityAnalysis,
                                                  output = "Venous Blood Plasma",
                                                  pkParam = "AUC_tEnd",
                                                  sortColumn = sortColumn,
                                                  sortValue =  sortValue,
                                                  threshold = threshold,
                                                  saveName = paste0(saveName,threshold*100, 'perc_'))
sensitivityAnalysis_cmaxVB_plot <- filterSAForPlot(dfSA = sensitivityAnalysis, 
                                                   output = "Venous Blood Plasma",
                                                   pkParam = "C_max",
                                                   sortColumn = sortColumn,
                                                   sortValue =  sortValue,
                                                   threshold = threshold, 
                                                   saveName = paste0(saveName,threshold*100, 'perc_'))

