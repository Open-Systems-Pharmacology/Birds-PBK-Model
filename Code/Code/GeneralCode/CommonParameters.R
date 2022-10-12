getTestParameters <- function(params) {
  paramVals <- enum(list(
    "Organism|Lumen|Crop|Gastric emptying time" = 15,
    "Organism|Lumen|Stomach|Gastric emptying time" = 15,    
    "Organism|Lumen|Crop|Inverse rate of inflow of liquid into stomach" = 15,
    "Organism|Lumen|Stomach|Inverse rate of inflow of liquid into stomach" = 15,
    "DefaultCompound|useppLFER" = 0,
    "Organism|SmallIntestine|Small intestinal transit time factor" = 344.91,
    "Organism|LargeIntestine|Large intestinal transit time factor" = 75,
    "Neighborhoods|Oviduct_int_Albumen1|DefaultCompound|P_int_cell_factor" = 1,
    "Neighborhoods|Oviduct_int_Albumen2|DefaultCompound|P_int_cell_factor" = 1,
    "Neighborhoods|Oviduct_int_Albumen3|DefaultCompound|P_int_cell_factor" = 1,
    "Neighborhoods|Oviduct_int_Albumen4|DefaultCompound|P_int_cell_factor" = 1,
    "Neighborhoods|Oviduct_int_Albumen5|DefaultCompound|P_int_cell_factor" = 1,
    "Neighborhoods|Oviduct_int_Albumen6|DefaultCompound|P_int_cell_factor" = 1,
    "Neighborhoods|Oviduct_int_Albumen7|DefaultCompound|P_int_cell_factor" = 1,
    "Neighborhoods|Oviduct_int_Albumen8|DefaultCompound|P_int_cell_factor" = 1,
    "Neighborhoods|Oviduct_int_Albumen9|DefaultCompound|P_int_cell_factor" = 1
    
  ))
  
  paramUnits <- enum(list(
    "Organism|Lumen|Crop|Gastric emptying time" = "min",
    "Organism|Lumen|Stomach|Gastric emptying time" = "min",
    "Organism|Lumen|Crop|Inverse rate of inflow of liquid into stomach" = "min",
    "Organism|Lumen|Stomach|Inverse rate of inflow of liquid into stomach" = "min",
    "Organism|SmallIntestine|Small intestinal transit time factor" = "",
    "Organism|LargeIntestine|Large intestinal transit time factor" = "",
    "DefaultCompound|useppLFER" = "",
    "Neighborhoods|Oviduct_int_Albumen1|DefaultCompound|P_int_cell_factor" = "",
    "Neighborhoods|Oviduct_int_Albumen2|DefaultCompound|P_int_cell_factor" = "",
    "Neighborhoods|Oviduct_int_Albumen3|DefaultCompound|P_int_cell_factor" = "",
    "Neighborhoods|Oviduct_int_Albumen4|DefaultCompound|P_int_cell_factor" = "",
    "Neighborhoods|Oviduct_int_Albumen5|DefaultCompound|P_int_cell_factor" = "",
    "Neighborhoods|Oviduct_int_Albumen6|DefaultCompound|P_int_cell_factor" = "",
    "Neighborhoods|Oviduct_int_Albumen7|DefaultCompound|P_int_cell_factor" = "",
    "Neighborhoods|Oviduct_int_Albumen8|DefaultCompound|P_int_cell_factor" = "",
    "Neighborhoods|Oviduct_int_Albumen9|DefaultCompound|P_int_cell_factor" = ""
    
  ))

  idx <- match(enumKeys(paramVals), params$paths)

  # Add entries if the parameter paths are not present in the original params
  notPresent <- which(is.na(idx))
  for (i in notPresent){
    path <- enumKeys(paramVals)[[i]]
    value <- paramVals[[i]]
    unit <- enumGetValue(key = path, enum = paramUnits)
    
    params$paths <- append(params$paths, path)
    params$values <- append(params$values, value)
    params$units <- append(params$units, unit)
    
    idx[[i]] <- length(params$paths)
  }
  
  params$values[idx] <- enumValues(paramVals)
  params$units[idx] <- lapply(enumKeys(paramVals), function(x){enumGetValue(enum = paramUnits, key = x)})
  
  return(params)
}
