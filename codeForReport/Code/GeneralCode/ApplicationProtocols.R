#' Disable all events listed in the enum \code{Events}.
#'
#' @param simulation Simulation for which the events will be disabled.
#'
#' @return
#' @export
disableEvents <- function(simulation) {
  if (length(enumValues(Events)) != 0) {
    setParameterValuesByPath(parameterPaths = paste(enumValues(Events), "isApplied", sep = "|"), values = rep(0, len = length(enumValues(Events))),
                             simulation = simulation)
  }
}

#For each simulated scenario, create an entry with the name and the code that sets the corresponding parameters of the simulation.
#The parameters can be Active-parameters of the administration protocols, dose specifications, but also changing some model parameters that might be fitted to the specific scenario.
setApplications <- function(simulation, scenario,
                            dose = NULL, units = NULL, runConfiguration = NULL, simulationName = NULL, model = NULL, compound = NULL) {
  cname <-  strsplit(compound, " ")[[1]][2]
  if(is.na(cname)){
    cname <- "unfitted"
  }
  allScenarios <- enum(list(
    #### Itraconazole ####
    ITZ_Tell2005_IC_20mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 20, simulation = simulation, units = "mg/kg")
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("ITZ_Tell2005_IC_20mgkg", "_", model, "_", cname, ".pkml")))
      
    },
    #### Midazolam ####
    MDZ_Cortright2007_IV_05mgkg_chicken = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath("DefaultCompound-CYP3A4-Default CYP|Km", values = 2.1, simulation = simulation)
      setParameterValuesByPath("DefaultCompound-CYP3A4-Default CYP|kcat", values = 0.45, simulation = simulation)
      if(length(grep("unfitted", cname)) == 0){
        setParameterValuesByPath("DefaultCompound-CYP3A4-Default CYP|kcat", values = 1.59, simulation = simulation) # fitted
        setParameterValuesByPath("DefaultCompound|Fraction unbound (plasma, reference value)", values = 0.08, simulation = simulation) # fitted
      }

      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 5, simulation = simulation, units = "mg/kg")
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MDZ_Cortright2007_IV_05mgkg_chicken", "_", model, "_", cname, ".pkml")))
    },
    MDZ_Cortright2007_IV_05mgkg_quail = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath("DefaultCompound-CYP3A4-Default CYP|Km", values = 3.2, simulation = simulation)
      setParameterValuesByPath("DefaultCompound-CYP3A4-Default CYP|kcat", values = 0.39, simulation = simulation)# unfitted
      if(length(grep("unfitted", cname)) == 0){
      setParameterValuesByPath("DefaultCompound-CYP3A4-Default CYP|kcat", values = 4.41, simulation = simulation)# fitted
      setParameterValuesByPath("DefaultCompound|Fraction unbound (plasma, reference value)", values = 0.16, simulation = simulation) # fitted
      }
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 5, simulation = simulation, units = "mg/kg")
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MDZ_Cortright2007_IV_05mgkg_quail", "_", model, "_", cname, ".pkml")))
    },
    MDZ_Cortright2012_5mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 5, simulation = simulation, units = "mg")
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MDZ_Cortright2012_5mgkg", "_", model, "_", cname, ".pkml")))
    },
    #### Salinomycin ####
    SAL_Henri2012_IV_025mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0.25, simulation = simulation, units = "mg/kg")
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("SAL_Henri2012_IV_025mgkg", "_", model, "_", cname, ".pkml")))
    },
    SAL_Henri2012_IC_25mgkg = function(){
      eventName <- Events$IC_bolus
      # setParameterValuesByPath("DefaultCompound|Solubility at reference pH", values = 17, simulation = simulation, units = "mg/l")
      
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 2.5, simulation = simulation, units = "mg/kg")
      setParameterValuesByPath(list("Organism|Lumen|Crop|Gastric emptying time",
                                            "Organism|Lumen|Stomach|Gastric emptying time",
                                            "Organism|Lumen|Crop|Inverse rate of inflow of liquid into stomach",
                                            "Organism|Lumen|Stomach|Inverse rate of inflow of liquid into stomach"),
                                       values = rep(5, 4), units = rep("min", 4),simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("SAL_Henri2012_IC_25mgkg", "_", model, "_", cname, ".pkml")))
    },
    SAL_Henri2012_10mg_35d = function(){
      eventName <- Events$IC_mult_35
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 35, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 10, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 0+i-1, simulation = simulation, units = "day(s)")
      }
      setOutputInterval(startTime = 33*24*60, endTime = 36*24*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-11
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("SAL_Henri2012_10mg_35d", "_", model, "_", cname, ".pkml")))
      
    },
    SAL_Atef1993_IV_20mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 20, simulation = simulation, units = "mg/kg")
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("SAL_Atef1993_IV_20mgkg", "_", model, "_", cname, ".pkml")))
    },
    SAL_Atef1993_IC_20mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 20, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0*24*60, endTime = 2*24*60, resolution = 1, simulation = simulation)
      setParameterValuesByPath(list("Organism|Lumen|Crop|Gastric emptying time",
                                            "Organism|Lumen|Stomach|Gastric emptying time",
                                            "Organism|Lumen|Crop|Inverse rate of inflow of liquid into stomach",
                                            "Organism|Lumen|Stomach|Inverse rate of inflow of liquid into stomach"),
                                       values = rep(5, 4), units = rep("min", 4),simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("SAL_Atef1993_IC_20mgkg", "_", model, "_", cname, ".pkml")))
    },
    SAL_Atef1993_6mg_14d = function(){
      eventName <- Events$IC_mult_14
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      # setParameterValuesByPath("DefaultCompound|Solubility at reference pH", values = 1000, simulation = simulation, units = "mg/l")
      
      for (i in seq(1, 14, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 6, simulation = simulation, units = "mg")
        # setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 0+i-1, simulation = simulation, units = "day(s)")
      }
      setParameterValuesByPath(list("Organism|Lumen|Crop|Gastric emptying time",
                                            "Organism|Lumen|Stomach|Gastric emptying time",
                                            "Organism|Lumen|Crop|Inverse rate of inflow of liquid into stomach",
                                            "Organism|Lumen|Stomach|Inverse rate of inflow of liquid into stomach"),
                                       values = rep(5, 4), units = rep("min", 4),simulation = simulation)
      setOutputInterval(startTime = 13*24*60, endTime = 15*24*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-11
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("SAL_Atef1993_6mg_14d", "_", model, "_", cname, ".pkml")))
      
    },
    #### Chloramphenicol ####
    CAP_Atef1991_IV_20mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 20, simulation = simulation, units = "mg/kg")
      # saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("CAP_Atef1991_IV_20mgkg", "_", model, "_", cname, ".pkml")))
    },
    CAP_Dein1980_IV_22mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 22, simulation = simulation, units = "mg/kg")
      # saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("CAP_Dein1980_IV_22mgkg", "_", model, "_", cname, ".pkml")))
    },
    CAP_Anadon1994_IV_30mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 30, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 24*60, resolution = 1, simulation = simulation)
      # saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("CAP_Anadon1994_IV_30mgkg", "_", model, "_", cname, ".pkml")))
    },
    CAP_Atef1991_IC_20mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 20, simulation = simulation, units = "mg/kg")
      # saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("CAP_Atef1991_IC_20mgkg", "_", model, "_", cname, ".pkml")))
    },
    CAP_Dein1980_IC_55mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 55, simulation = simulation, units = "mg/kg")
      # saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("CAP_Anadon1994_IC_30mgkg", "_", model, "_", cname, ".pkml")))
    },
    CAP_Anadon1994_IC_30mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 30, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 25*60, resolution = 1, simulation = simulation)
      # saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("CAP_Anadon1994_IC_30mgkg", "_", model, "_", cname, ".pkml")))
    },
    CAP_Anadon1994_IC_50mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 50, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 25*60, resolution = 1, simulation = simulation)
      # saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("CAP_Anadon1994_IC_50mgkg", "_", model, "_", cname, ".pkml")))
    },
    CAP_Akhtar_IC_100mg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0, simulation = simulation, units = "mg/kg")
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Dose", sep = "|"), values = 100, simulation = simulation, units = "mg")
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      # saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("CAP_Akhtar_IC_100mg", "_", model, "_", cname, ".pkml")))
    },
    CAP_Akhtar1996_5mgkg_5d = function(){
      eventName <- Events$IC_mult_5
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 5, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 5, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 9+i-1, simulation = simulation, units = "day(s)")
      }
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-11
      setOutputInterval(startTime = 0*24*60, endTime = 22*24*60, resolution = 1, simulation = simulation)
      # saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("CAP_Akhtar1996_5mgkg_5d", "_", model, "_", cname, ".pkml")))
    },
    CAP_Anadon1994_50mgkg_4d = function(){
      eventName <- Events$IC_mult_4
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 4, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 50, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 0+i-1, simulation = simulation, units = "day(s)")
      }
      setOutputInterval(startTime = 0, endTime = 216*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-11
      # saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("CAP_Anadon1994_50mgkg_4d", "_", model, "_", cname, ".pkml")))
    },
    
    #### Deltamethrin ####
    DTM_Hueyuek2017_IV_075mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0.75, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 24*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("DTM_Hueyuek2017_IV_075mgkg", "_", model, "_", cname, ".pkml")))
    },
    DTM_M132448_01_IV_04mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName, "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default","Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0.4, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 120*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("DTM_M132448_01_IV_04mgkg", "_", model, "_", cname, ".pkml")))
    },
    DTM_Hueyuek2017_IC_075mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0.75, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 13*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("DTM_Hueyuek2017_IC_075mgkg", "_", model, "_", cname, ".pkml")))
    },
    DTM_MacLachlan2008_2mg_28d = function(){
      eventName <- Events$IC_mult_28
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      if(model == "pplfer"){
      setParameterValuesByPath("DefaultCompound|Lipophilicity", values = 5.2, simulation = simulation)
      }
      for (i in seq(1, 28, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 2, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 9+i-1, simulation = simulation, units = "day(s)")
      }
      setOutputInterval(startTime = 0*24*60, endTime = 36*24*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-9
      simulation$solver$relTol <- 1e-11
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("DTM_MacLachlan2008_2mg_28d", "_", model, "_", cname, ".pkml")))
    },
    
    #### Florfenicol ####
    FFC_Liu2018_IV_10mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 10, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 4*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Liu2018_IV_10mgkg", "_", model, "_", cname, ".pkml")))
    },
    FFC_Anadon2008_IV_20mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 20, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 24*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Anadon2008_IV_20mgkg", "_", model, "_", cname, ".pkml")))
    },    
    FFC_Shen2003_IV_15mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 15, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 10*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Shen2003_IV_15mgkg", "_", model, "_", cname, ".pkml")))
    },
    FFC_Ismail2009_IV_30mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 30, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 12*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Ismail2009_IV_30mgkg", "_", model, "_", cname, ".pkml")))
    },    
    FFC_Ismail2009_IV_30mgkg_quail = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 30, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 8*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Ismail2009_IV_30mgkg_quail", "_", model, "_", cname, ".pkml")))
    },
    FFC_Shen2003_IV_30mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 30, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 10*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Shen2003_IV_30mgkg", "_", model, "_", cname, ".pkml")))
    },
    FFC_Koc2009_IV_30mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 30, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 20*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Koc2009_IV_30mgkg", "_", model, "_", cname, ".pkml")))
    },
    FFC_Tikhomirov2019_IV_30mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 30, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 25*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Tikhomirov2019_IV_30mgkg", "_", model, "_", cname, ".pkml")))
    }, 
    FFC_Liu2018_IC_20mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 20, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 9*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Liu2018_IC_20mgkg", "_", model, "_", cname, ".pkml")))
    },    
    FFC_Anadon2008_IC_20mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 20, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 24*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Anadon2008_IC_20mgkg", "_", model, "_", cname, ".pkml")))
    },
    FFC_Shen2003_IC_15mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 15, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 11*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Shen2003_IC_15mgkg", "_", model, "_", cname, ".pkml")))
    },    
    FFC_Shen2003_IC_30mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 30, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 11*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Shen2003_IC_30mgkg", "_", model, "_", cname, ".pkml")))
    },
    FFC_Tikhomirov2019_IC_30mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 30, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 25*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Tikhomirov2019_IC_30mgkg", "_", model, "_", cname, ".pkml")))
    },
    FFC_ElBanna1998_IV_30mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 30, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 20*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_ElBanna1998_IV_30mgkg", "_", model, "_", cname, ".pkml")))
    },
    FFC_Chang2010_IC_30mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 30, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 24*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Chang2010_IC_30mgkg", "_", model, "_", cname, ".pkml")))
    },
    FFC_Filazi2014_20mgkg_5d = function(){
      eventName <- Events$IC_mult_5
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 5, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 20, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 9+i-1, simulation = simulation, units = "day(s)")
      }
      setOutputInterval(startTime = 0*24*60, endTime = 24*24*60, resolution = 1, simulation = simulation)
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      # simulation$solver$absTol <- 1e-14
      simulation$solver$absTol <- 1e-11#RR
      simulation$solver$absTol <- 1e-12#RR
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Filazi2014_20mgkg_5d", "_", model, "_", cname, ".pkml")))
    },
    FFC_Filazi2014_20mgkg_3d = function(){
      eventName <- Events$IC_mult_3
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 3, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 20, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 9+i-1, simulation = simulation, units = "day(s)")
      }
      setOutputInterval(startTime = 0*24*60, endTime = 24*24*60, resolution = 1, simulation = simulation)
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-14
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Filazi2014_20mgkg_3d", "_", model, "_", cname, ".pkml")))
    },
    FFC_Filazi2014_40mgkg_3d = function(){
      eventName <- Events$IC_mult_3
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 3, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 40, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 9+i-1, simulation = simulation, units = "day(s)")
      }
      setOutputInterval(startTime = 0, endTime = 24*24*60, resolution = 1, simulation = simulation)
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-15 #RR
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Filazi2014_40mgkg_3d", "_", model, "_", cname, ".pkml")))
    },
    FFC_Afifi1997_30mgkg_5d = function(){
      eventName <- Events$IC_mult_5
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 5, 1)) {
        appnum <- paste0('Application_', as.character(i))
        
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 30, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 0+i-1, simulation = simulation, units = "day(s)")
      }
      setOutputInterval(startTime = 0, endTime = 168*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-11
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Afifi1997_30mgkg_5d", "_", model, "_", cname, ".pkml")))
    },
    FFC_Anadon2008_40mgkg_3d = function(){
      eventName <- Events$IC_mult_3
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 3, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        # setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        # 
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 40, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = i-1, simulation = simulation, units = "day(s)")
      }
      setOutputInterval(startTime = 0*24*60, endTime = 10*24*60, resolution = 1, simulation = simulation)
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-10
      simulation$solver$relTol <- 1e-9
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("FFC_Anadon2008_40mgkg_3d", "_", model, "_", cname, ".pkml")))
    },
    
    #### Ivermectin ####
    IVM_Cirak2018_IV_02mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 168*60, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0.2, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 168*60, endTime = 15*24*60, resolution = 1, simulation = simulation)
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("IVM_Cirak2018_IV_02mgkg", "_", model, "_", cname, ".pkml")))
    },
    IVM_Cirak2018_IC_02mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath("DefaultCompound|Solubility at reference pH", values = 0.7, simulation = simulation, units = "mg/l")
      
      q <-  getQuantity(path = paste(eventName, "solution",  "Application_1", 'DefaultCompound', sep = "|"), container = simulation)
      q$value <- 100000
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 168*60, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0.2, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 168*60, endTime = 15*24*60, resolution = 1, simulation = simulation)
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("IVM_Cirak2018_IC_02mgkg", "_", model, "_", cname, ".pkml")))
    },
    IVM_Moreno2015_IV_04mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0.4, simulation = simulation, units = "mg/kg")
      setOutputInterval(startTime = 0, endTime = 240*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("IVM_Moreno2015_IV_04mgkg", "_", model, "_", cname, ".pkml")))
    },
    IVM_Moreno2015_04mgkg_5d = function(){
      eventName <- Events$IC_mult_5
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 5, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0.4, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 7+i-1, simulation = simulation, units = "day(s)")
      }
      setOutputInterval(startTime = 7*24*60, endTime = 28*24*60, resolution = 1, simulation = simulation)
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("IVM_Moreno2015_04mgkg_5d", "_", model, "_", cname, ".pkml")))
    },
    
    #### Melamine ####
    MEL_Poapolathep2015_IV_5o5mgkg = function(){
      setParameterValuesByPath("Organism|isLayingHen", values = 0, simulation = simulation)
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 5.5, simulation = simulation, units = "mg/kg")
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MEL_Poapolathep2015_IV_5o5mgkg", "_", model, "_", cname, ".pkml")))
    },
    MEL_Suknikom2016_IV_5o5mgkg = function(){
      setParameterValuesByPath("Organism|isLayingHen", values = 0, simulation = simulation)
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 5.5, simulation = simulation, units = "mg/kg")
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MEL_Suknikom2016_IV_5o5mgkg", "_", model, "_", cname, ".pkml")))
    },
    MEL_Poapolathep2015_IC_5o5mgkg = function(){
      setParameterValuesByPath("Organism|isLayingHen", values = 0, simulation = simulation)
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 5.5, simulation = simulation, units = "mg/kg")
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MEL_Poapolathep2015_IC_5o5mgkg", "_", model, "_", cname, ".pkml")))
    },
    MEL_Suknikom2016_IC_5o5mgkg = function(){
      setParameterValuesByPath("Organism|isLayingHen", values = 0, simulation = simulation)
      eventName <- Events$IC_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste("CYP3A4|Reference concentration" ), values = 29, simulation = simulation)
      # setParameterValuesByPath(paste("DefaultCompound-CYP3A4-Default CYP|kcat" ), values = 0.4, simulation = simulation)
      # setParameterValuesByPath(paste("DefaultCompound-CYP3A4-Default CYP|Km" ), values = 100, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 5.5, simulation = simulation, units = "mg/kg")
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MEL_Suknikom2016_IC_5o5mgkg", "_", model, "_", cname, ".pkml")))
    },
    MEL_Zhang2012_08mg_30d = function(){
      eventName <- Events$IC_mult_30
      setParameterValuesByPath(paste(eventName, "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 30, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 0.8, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 9+i-1, simulation = simulation, units = "day(s)")
      }  
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      setOutputInterval(startTime = 15*24*60, endTime = 42*24*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-12
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MEL_Zhang2012_08mg_30d", "_", model, "_", cname, ".pkml")))
    },
    MEL_Zhang2012_1o6mg_30d = function(){
      eventName <- Events$IC_mult_30
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 30, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 1.6, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 9+i-1, simulation = simulation, units = "day(s)")
      }
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      setOutputInterval(startTime = 15*24*60, endTime = 42*24*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-14
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MEL_Zhang2012_1o6mg_30d", "_", model, "_", cname, ".pkml")))
    },
    MEL_Dong2010_100mg_21d = function(){
      eventName <- Events$IC_mult_21
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 21, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 10, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 8+i-1, simulation = simulation, units = "day(s)")
      }
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      setOutputInterval(startTime = 9*24*60, endTime = 42*24*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-11
      # simulation$solver$relTol <- 1e-5
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MEL_Dong2010_100mg_21d", "_", model, "_", cname, ".pkml")))
    },
    MEL_Dong2010_50mg_21d = function(){
      eventName <- Events$IC_mult_21
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 21, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 5, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 8+i-1, simulation = simulation, units = "day(s)")
      }
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      setOutputInterval(startTime = 9*24*60, endTime = 42*24*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-13
      # simulation$solver$relTol <- 1e-5
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MEL_Dong2010_50mg_21d", "_", model, "_", cname, ".pkml")))
    },
    MEL_Gao2010_18mg_21d = function(){
      eventName <- Events$IC_mult_21
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 21, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 18, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 10+i-1, simulation = simulation, units = "day(s)")
      }
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      setOutputInterval(startTime = 9*24*60, endTime = 51*24*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-15
      # simulation$solver$relTol <- 1e-5
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MEL_Gao2010_18mg_21d", "_", model, "_", cname, ".pkml")))
    },
    MEL_Gao2010_9mg_21d = function(){
      eventName <- Events$IC_mult_21
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 21, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 9, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 10+i-1, simulation = simulation, units = "day(s)")
      }
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      setOutputInterval(startTime = 9*24*60, endTime = 42*24*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-14
      # simulation$solver$relTol <- 1e-5
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MEL_Gao2010_9mg_21d", "_", model, "_", cname, ".pkml")))
    },
    MEL_Bai_140mg_34d = function(){
      eventName <- Events$IC_mult_34
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 34, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        # setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 0, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 140.9, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 8+i-1, simulation = simulation, units = "day(s)")
      }
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      setOutputInterval(startTime = 9*24*60, endTime = 49*24*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-11
      # simulation$solver$relTol <- 1e-5
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MEL_Bai_140mg_34d", "_", model, "_", cname, ".pkml")))
    },
    MEL_Bai_62mg_34d = function(){
      eventName <- Events$IC_mult_34
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 34, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        # setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 0, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 62.6, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 8+i-1, simulation = simulation, units = "day(s)")
      }
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      setOutputInterval(startTime = 9*24*60, endTime = 49*24*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-11
      # simulation$solver$relTol <- 1e-5
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MEL_Bai_62mg_34d", "_", model, "_", cname, ".pkml")))
    },
    MEL_Bai_33mg_34d = function(){
      eventName <- Events$IC_mult_34
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 34, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        # setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 0, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 33.6, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 8+i-1, simulation = simulation, units = "day(s)")
      }
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      setOutputInterval(startTime = 9*24*60, endTime = 49*24*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-11
      # simulation$solver$relTol <- 1e-5
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MEL_Bai_33mg_34d", "_", model, "_", cname, ".pkml")))
    },
    MEL_Bai_17mg_34d = function(){
      eventName <- Events$IC_mult_34
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 34, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        # setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 0, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 17.4, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 8+i-1, simulation = simulation, units = "day(s)")
      }
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      setOutputInterval(startTime = 9*24*60, endTime = 49*24*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-11
      # simulation$solver$relTol <- 1e-5
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MEL_Bai_17mg_34d", "_", model, "_", cname, ".pkml")))
    },
    MEL_Bai_8mg_34d = function(){
      eventName <- Events$IC_mult_34
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      for (i in seq(1, 34, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        # setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 0, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 8.6, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 8+i-1, simulation = simulation, units = "day(s)")
      }
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      setOutputInterval(startTime = 9*24*60, endTime = 49*24*60, resolution = 1, simulation = simulation)
      simulation$solver$absTol <- 1e-11
      # simulation$solver$relTol <- 1e-5
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MEL_Bai_8mg_34d", "_", model, "_", cname, ".pkml")))
    },
    

    
    
    #### Monensin ####
    MON_Henri2012_IV_046mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName,  "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0.46, simulation = simulation, units = "mg/kg")
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MON_Henri2012_IV_046mgkg", "_", model, "_", cname, ".pkml")))
    },
    MON_Atef1993_IV_40mgkg = function(){
      eventName <- Events$IV_bolus
      setParameterValuesByPath(paste(eventName, "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "Default", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 40, simulation = simulation, units = "mg/kg")
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MON_Atef1993_IV_40mgkg", "_", model, "_", cname, ".pkml")))
    },
    MON_Henri2012_IC_4mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath("Organism|Lumen|Crop|Gastric emptying time", values = 5, simulation = simulation)
      setParameterValuesByPath("Organism|Lumen|Stomach|Gastric emptying time",values = 5, simulation = simulation)
      setParameterValuesByPath("Organism|Lumen|Crop|Inverse rate of inflow of liquid into stomach", values = 5, simulation = simulation)
      setParameterValuesByPath("Organism|Lumen|Stomach|Inverse rate of inflow of liquid into stomach", values = 5, simulation = simulation)
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath("DefaultCompound|Solubility at reference pH", values = 0.1, simulation = simulation, units = "mg/l")
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 4, simulation = simulation, units = "mg/kg")
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MON_Henri2012_IC_4mgkg", "_", model, "_", cname, ".pkml")))
    },
    MON_Atef1993_IC_40mgkg = function(){
      eventName <- Events$IC_bolus
      setParameterValuesByPath("Organism|Lumen|Crop|Gastric emptying time", values = 5, simulation = simulation)
      setParameterValuesByPath("Organism|Lumen|Stomach|Gastric emptying time",values = 5, simulation = simulation)
      setParameterValuesByPath("Organism|Lumen|Crop|Inverse rate of inflow of liquid into stomach", values = 5, simulation = simulation)
      setParameterValuesByPath("Organism|Lumen|Stomach|Inverse rate of inflow of liquid into stomach", values = 5, simulation = simulation)
      setParameterValuesByPath("DefaultCompound|Solubility at reference pH", values = 25, simulation = simulation, units = "mg/l")
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "Start time", sep = "|"), values = 0, simulation = simulation)
      setParameterValuesByPath(paste(eventName, "solution", "Application_1", "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 40, simulation = simulation, units = "mg/kg")
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MON_Atef1993_IC_40mgkg", "_", model, "_", cname, ".pkml")))
    },
    MON_Atef1993_24mg_14d = function(){
      eventName <- Events$IC_mult_14
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath("DefaultCompound|Solubility at reference pH", values = 200, simulation = simulation, units = "mg/l")
      setParameterValuesByPath("Organism|Lumen|Crop|Gastric emptying time", values = 5, simulation = simulation)
      setParameterValuesByPath("Organism|Lumen|Stomach|Gastric emptying time",values = 5, simulation = simulation)
      setParameterValuesByPath("Organism|Lumen|Crop|Inverse rate of inflow of liquid into stomach", values = 5, simulation = simulation)
      setParameterValuesByPath("Organism|Lumen|Stomach|Inverse rate of inflow of liquid into stomach", values = 5, simulation = simulation)
      for (i in seq(1, 14, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 24, simulation = simulation, units = "h")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 24, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 0+i-1, simulation = simulation, units = "day(s)")
      }
      setOutputInterval(startTime = 13*24*60, endTime = 15*24*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MON_Atef1993_24mg_14d", "_", model, "_", cname, ".pkml")))
    },
    MON_Henri2012_25mg_33d = function(){ #actually Henri2009!
      eventName <- Events$IC_mult_33
      setParameterValuesByPath("DefaultCompound|Solubility at reference pH", values = 0.5, simulation = simulation, units = "mg/l")
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      # for (i in seq(1, 32, 1)) {
      for (i in seq(1, 33, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 24, simulation = simulation, units = "h")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 25, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 0+i-1, simulation = simulation, units = "day(s)")
      }
      setParameterValuesByPath("Organism|Lumen|Crop|Gastric emptying time", values = 5, simulation = simulation)
      setParameterValuesByPath("Organism|Lumen|Stomach|Gastric emptying time",values = 5, simulation = simulation)
      setParameterValuesByPath("Organism|Lumen|Crop|Inverse rate of inflow of liquid into stomach", values = 5, simulation = simulation)
      setParameterValuesByPath("Organism|Lumen|Stomach|Inverse rate of inflow of liquid into stomach", values = 5, simulation = simulation)
      setOutputInterval(startTime = 30*24*60, endTime = 810*60, resolution = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MON_Henri2012_25mg_33d", "_", model, "_", cname, ".pkml")))
    },
    MON_Vandenberge2012_1o75mg_14d = function(){
      eventName <- Events$IC_mult_14
      setParameterValuesByPath(paste(eventName,  "isApplied", sep = "|"), values = 1, simulation = simulation)
      setParameterValuesByPath("DefaultCompound|Solubility at reference pH", values = 16.46, simulation = simulation, units = "mg/l")
      for (i in seq(1, 14, 1)) {
        appnum <- paste0('Application_', as.character(i))
        q <-  getQuantity(path = paste(eventName, "solution", appnum, 'DefaultCompound', sep = "|"), container = simulation)
        q$value <- 100000
        setParameterValuesByPath(paste(eventName, "solution", appnum, "End time", sep = "|"), values = 16, simulation = simulation, units = "h")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Dose", sep = "|"), values = 1.75, simulation = simulation, units = "mg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), values = 0, simulation = simulation, units = "mg/kg")
        setParameterValuesByPath(paste(eventName, "solution", appnum, "ProtocolSchemaItem", "Start time", sep = "|"), values = 9+i-1, simulation = simulation, units = "day(s)")
      }
      setOutputInterval(startTime = 9*24*60, endTime = 40*24*60, resolution = 1, simulation = simulation)      
      setParameterValuesByPath("Organism|isLayingHen", values = 1, simulation = simulation)
      saveSimulation(simulation = simulation,  filePath = file.path(getwd(), "../Models/Simulations", paste0("MON_Vandenberge2012_1o75mg_14d", "_", model, "_", cname, ".pkml")))
    }
  ))
  
  allScenarios[[scenario]]
}
