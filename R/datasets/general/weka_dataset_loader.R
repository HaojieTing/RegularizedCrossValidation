weka_dataset_loader.DataGenerator <- function(dataConf) {
  data_dirpath = "/public/swl/datasets"
  if ( "" != Sys.getenv("SXU_SWL_DATASETS_PATH") )
    data_dirpath = Sys.getenv("SXU_SWL_DATASETS_PATH")
  name <- dataConf$name
  type <- dataConf$type
  mode <- dataConf$mode  
  file_path_df <- paste(data_dirpath, "weka_datasets", type, paste(name, "arff", "df", sep="."), sep=.Platform$file.sep)
  if(!file.exists(file_path_df))  stop("The requested dataset not exists")
  data <- readRDS(file=file_path_df)

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
  stop("Code cannot touch here");
}

weka_dataset_loader.Prepackages <- c()


weka_dataset_loader.validation  <- function(dataConf) {return(TRUE);}


weka_dataset_loader.replaceVarNames <- function(dataname) {  
  assign(paste(dataname, "DataGenerator", sep="."), weka_dataset_loader.DataGenerator, envir = globalenv())
  remove(weka_dataset_loader.DataGenerator, envir = globalenv())
  assign(paste(dataname, "Prepackages", sep="."), weka_dataset_loader.Prepackages, envir = globalenv())
  remove(weka_dataset_loader.Prepackages, envir = globalenv())  
  assign(paste(dataname,"validation", sep="."), weka_dataset_loader.validation, envir = globalenv())
  remove(weka_dataset_loader.validation, envir = globalenv())
}
