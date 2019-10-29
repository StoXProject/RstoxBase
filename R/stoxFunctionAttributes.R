# A list of the attributes of the exported StoX functions:
#' 
#' @export
#' 
stoxFunctionAttributes <- list(
  
  # Read input biotic data:
  ReadBiotic = list(
    functionCategory = "Baseline", 
    functionOutputDataType = "BioticData", 
    functionParameterHierarchy = list(
      FileNames = list()
    ), 
    parameterDataType = list(
      FileNames = "filePath"
    )
  ), 
  
  # Read strata polygons:
  DefineStrata = list(
    functionCategory = "Baseline", 
    functionOutputDataType = "StratumPolygon", 
    functionParameterHierarchy = list(
      FileName = list(
        UseProcessData = FALSE
      ), 
      UseProcessData = list()
    ), 
    parameterDataType = list(
      FileName = "filePath", 
      UseProcessData = "logical"
    )
  ), 
  
  # Calculate areas of strata polygons:
  StratumArea = list(
    functionCategory = "Baseline", 
    functionOutputDataType = "StratumArea", 
    functionParameterHierarchy = list(
      StratumPolygon = list(), 
      AreaMethod = list()
    ), 
    parameterDataType = list(
      AreaMethod = "character"
    )
  )
  
)




# functionAlias
# parameterAlias
# parameterValueAilas
