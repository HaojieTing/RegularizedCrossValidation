loadSignTestFunction <- function(stname) {
  path = getwd()
  if('signif_Test' %in% list.files(path) ) {
    path <- file.path(path, 'signif_Test', paste(stname, ".R", sep = ""),
                      fsep=.Platform$file.sep)
  } else {
    path <- file.path(path, '..', 'signif_Test', paste(stname, ".R", sep = ""),
                      fsep=.Platform$file.sep)
  }  
  if ( file.exists(path) ) {
    source(path, encoding = "UTF8")
    st.generator <- get(paste(stname, "process", sep="."))    
    return(st.generator)
  } else {
    stop("")
  }
}


validateSignTestInfo <- function(testConf) {
  
  return_value <- TRUE
  if( is.null(testConf$test_type) ) {
    warning("The type property of significant test must be specified.[ST]")
    return( FALSE )
  }
  if(is.null(testConf$algor1_path)) {
    warning("The algor1_path property of significant test must be specified.[ST]")
    return(FALSE)
  }
  if(is.null(testConf$algor2_path)) {
    warning("The algor2_path property of significant test must be specified.[ST]")
    return(FALSE)
  }
  loadSignTestFunction(testConf$test_type)
  if( exists(paste(testConf$test_type, "validate", sep=".")) ) {
    st.validate <- get(paste(testConf$test_type, "validate", sep="."))
    return_value <- st.validate(testConf)
  } else {
    return_value <- FALSE
  }
  return(return_value)
}


normalized_distance_error <- function(mu_algor1, mu_algor2) {
  errV1 <- mu_algor1[, 1:(ncol(mu_algor1)-1)]
  errV2 <- mu_algor2[, 1:(ncol(mu_algor2)-1)]
  z <- 2 * (mean(errV1) - mean(errV2))/(mean(apply(errV1, 1, sd)) + mean(apply(errV2, 1, sd)))
  return(z)
}
