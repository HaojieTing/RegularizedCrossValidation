uci_classification.readDataAsPopulation <- function(id) {
  data_dirpath = Sys.getenv("SXU_SWL_DATASETS_PATH")
  pathData <- file.path(data_dirpath, "UCI_ML_DataFolders")
  pathDescription <- file.path(data_dirpath, "UCI_ML_DataDescription")  
  dsList <- prepareDSList(pathData, pathDescription)  
  data <- dsRead(dsList, id, keepContents = TRUE)
  responseIdx <- dsList[which(dsList['identification'] == id),]['responsePos']
  responseIdx <- responseIdx[1,1]  
  info <- analyzeData(data)
  for(i in 1:nrow(info)) {
    if(info[i,1] == 'character') {
       data[, i] <- factor(data[,i])
    }
  }
  if(responseIdx != ncol(data)) {
    nms <- colnames(data)
    class_name <- nms[responseIdx]
    nms <- c(nms[-responseIdx], class_name)
    data <- data[, nms]    
  }
  if(nrow(data) > 5000) data<- data[1:5000,]
  return(data)
}


uci_classification.DataGenerator <- function(dataConf) {
  name <- dataConf$name
  mode <- dataConf$mode  
  data <- get(paste(dataConf$name, 'readDataAsPopulation', sep="."))(name)
  if(mode == 1){
    return(data)  
  } 
  else if(mode == 2)
  {    
    size <- dataConf$size
    y_balance <- dataConf$y_balance
    by_portion <- dataConf$by_portion
    sample <- GenerateObservantsWithReplace(data, size, y_balance, by_portion)
    return(sample)
  }  
}

uci_classification.Prepackages <- c("readMLData", "XML")


uci_classification.validate.name <- function(name, value){
  return(TRUE)
}

uci_classification.validate.mode <- function(name, value) {
  if(value == 1 || value == 2) return(TRUE)
  else return(FALSE)
}

uci_classification.conf.list <- list(
  list(name = "name", tip =  "data name", type="", valid_fun = uci_classification.validate.name),
  list(name = "mode", tip =  "load mode", type="", valid_fun = uci_classification.validate.mode),
  list(name = "size", tip = "resample size", type="", valid_fun = function(name, value) {return(TRUE)}),
  list(name = "y_balance", tip = "keep y balance", type = "", valid_fun = function(name, value) {return(TRUE)}),
  list(name = "by_portion", tip="balance mode", type = "", valid_fun = function(name, value) {return(TRUE)})
  )

uci_classification.conf.shortcuts <- list(
  adult = list(name="adult", mode=1),
  "balance-scale" = list(name="balance-scale", mode=1)
  )

uci_classification.validation <- function(dataConf) {
  conf_list <- get(paste(dataConf$name, "conf.list", sep="."))
  conf_shortcuts <- get(paste(dataConf$name , "conf.shortcuts", sep="."))
  valid <- config.validation( conf_impl = dataConf, 
                              conf_def = conf_list, 
                              shortcut_list =  conf_shortcuts  )
  return(valid)
}


uci_classification.replaceVarNames <- function(dataname) {
  assign(paste(dataname, "readDataAsPopulation", sep="."), uci_classification.readDataAsPopulation,envir = globalenv())
  remove(uci_classification.readDataAsPopulation, envir = globalenv())
  assign(paste(dataname, "DataGenerator", sep="."), uci_classification.DataGenerator, envir = globalenv())
  remove(uci_classification.DataGenerator, envir = globalenv())
  assign(paste(dataname, "Prepackages", sep="."), uci_classification.Prepackages, envir = globalenv())
  remove(uci_classification.Prepackages, envir = globalenv())
  assign(paste(dataname, "conf.list", sep="."), uci_classification.conf.list, envir = globalenv())
  remove(uci_classification.conf.list, envir = globalenv())
  assign(paste(dataname,"conf.shortcuts", sep="."), uci_classification.conf.shortcuts, envir = globalenv())
  remove(uci_classification.conf.shortcuts, envir = globalenv())
  assign(paste(dataname,"validation", sep="."), uci_classification.validation, envir = globalenv())
  remove(uci_classification.validation, envir = globalenv())
}
