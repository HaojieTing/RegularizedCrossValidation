loadVarEstInfo <- function(vename ) {
  path <- getwd()
  if( "varianceEstimator" %in% list.files(path) ) {
    path <- file.path(path, 'varianceEstimator', paste(vename, ".R", sep = ""),
                      fsep=.Platform$file.sep)
  } else {
    path <- file.path(path, "..", 'varianceEstimator', paste(vename, ".R", sep = ""),
                      fsep=.Platform$file.sep)
  }
  if( file.exists(path) ) {
    print(path)
    source(path, encoding = "UTF-8")
    ve.estimator <- get(paste(vename, "Estimator", sep = "."))
    return(ve.estimator)
  } else {
    stop("Cannot find the given variance estimator.")
  }
}

loadVarEstForOneExprInfo <- function(vename) {
  path <- getwd()
  if("R" %in% list.files(path) && "LICENSE" %in% list.files(path))
    path <- file.path(path, "R", fsep = .Platform$file.sep)
  if( "varianceEstimator" %in% list.files(path) ) {
    path <- file.path(path, 'varianceEstimator', paste(vename, ".R", sep = ""),
                      fsep=.Platform$file.sep)
  } else {
    path <- file.path(path, "..", 'varianceEstimator', paste(vename, ".R", sep = ""),
                      fsep=.Platform$file.sep)
  }
  if( file.exists(path) ) {
    source(path, encoding = "UTF-8")
    ve.estimator <- get(paste(vename, "EstForOneExpr", sep = "."))
    return(ve.estimator)
  } else {
    stop(paste("Cannot find the given variance estimator EstForOneExpr:",path))
  }
}


loadFreeDegreeWrtVE <- function(vename) {
  path <- getwd()
  if( "varianceEstimator" %in% list.files(path) ) {
    path <- file.path(path, 'varianceEstimator', paste(vename, ".R", sep = ""),
                      fsep=.Platform$file.sep)
  } else {
    path <- file.path(path, "..", 'varianceEstimator', paste(vename, ".R", sep = ""),
                      fsep=.Platform$file.sep)
  }
  if( file.exists(path) ) {
    source(path, encoding = "UTF-8")
    ve.freedegree <- get(paste(vename, "getfreedegree", sep = "."))
    return(ve.freedegree)
  } else {
    stop("Cannot find the given variance estimator freedegree.")
  }
}

validateVarEstConf <- function( veConf ) {
  return_value <- TRUE
  if(is.null(veConf$name)) {
    warning("the name of variance estimator is not given.[VarEst]")
    return(FALSE)
  }
  if(is.null(veConf$data_file)) {
    warning("the data file for variance estimator is not given.[VarEst]")
    return(FALSE)
  }
  loadVarEstInfo(veConf$name)
  if( exists(paste(veConf$name, "validation", sep=".")) ) {
    ve.validate <- get(paste(veConf$name, "validation", sep="."))
    return_value <- ve.validate(veConf)
  } else {
    return_value <- FALSE
  }
  return(return_value)
}