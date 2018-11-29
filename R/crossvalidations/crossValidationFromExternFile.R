if(	!exists('crossValidationFromExternFile.load_count') ) {
	crossValidationFromExternFile.load_count <- 0
}


crossValidationFromExternFile.Generator <- function(cvConf) {
	if( exists('crossValidationFromExternFile.load_count') ) {
		crossValidationFromExternFile.load_count <<- crossValidationFromExternFile.load_count + 1
	} else {
		stop("variable not in workspace: crossValidationFromExternFile.load_count")
	}
	cv_data_file_name <- cvConf$file_path
	if( FALSE == file.exists(cv_data_file_name) ) {
		stop("file not exists.")
	}	
	cvlist <- readRDS(cv_data_file_name)
	curlist <- cvlist[[crossValidationFromExternFile.load_count]]
	return(curlist)
}

crossValidationFromExternFile.validate.file_path <- function(name, value) {
	return(TRUE)
}

crossValidationFromExternFile.conf.list <- list(
	list(name = "file_path", tip = "abs path of rds file for cross validation splits.", type = "字符串", valid_fun = crossValidationFromExternFile.validate.file_path)
)

crossValidationFromExternFile.conf.shortcuts <- list ()

crossValidationFromExternFile.validation <- function(cvConf) {
	valid <- config.validation( conf_impl = cvConf, 
                              conf_def = crossValidationFromExternFile.conf.list, 
                              shortcut_list =  crossValidationFromExternFile.conf.shortcuts  )
	return(valid)
}