if( !exists("loadDatasetsFromExternalFile.load_count")) {
	loadDatasetsFromExternalFile.load_count <- 0;
}

loadDatasetsFromExternalFile.DataGenerator <- function(dataConf) {
	if( exists("loadDatasetsFromExternalFile.load_count") ) {
		loadDatasetsFromExternalFile.load_count <<- loadDatasetsFromExternalFile.load_count + 1
	} else {
		stop("workspace:loadDatasetsFromExternalFile.load_count")
	}
	data_file_name <- dataConf$file_path
	if( FALSE == file.exists(data_file_name) ) {
		stop("data not exists.")
	}
	datasets <- readRDS(data_file_name) 	
	curdataset <- datasets[[loadDatasetsFromExternalFile.load_count]]
	names(curdataset) <- c("x", "y")
	return(curdataset)
}


loadDatasetsFromExternalFile.validate.file_path <- function(name, value) {
	return(TRUE)
}

loadDatasetsFromExternalFile.conf.list <- list(
	list(name="file_path", tip=" ", type="str", valid_fun = loadDatasetsFromExternalFile.validate.file_path )
)


loadDatasetsFromExternalFile.conf.shortcuts <- list ()


loadDatasetsFromExternalFile.validation <- function(dataConf) {
  valid <- config.validation( conf_impl = dataConf, 
                     conf_def = loadDatasetsFromExternalFile.conf.list, 
                     shortcut_list =  loadDatasetsFromExternalFile.conf.shortcuts  )
  return(valid)
}


loadDatasetsFromExternalFile.comment <- list()