#' @title Parse input from python
#'
#' @description When calling R from python using subprocess, one can pass argument. A standard have been defined as sending a list
#' of argument with the format arg_name=arg_value. This function parse it and create object in R RAM.
#' @param myArgs list of parameters obtained using \code{commandArgs(trailingOnly = TRUE)}
#' @param wanted_inputs is a list of character containing the name of expected inputs. (default to \code{list()})
#' @param verbose should the function talk (logical, default to TRUE).
#' @details
#' To simulate inputs from python, one can feed a vector respecting the expected format for myArgs (see examples). \cr
#' If you ask for some unprovided inputs, function will stop and raise un error.
#' @return list of asked inputs
#' @export
#' @examples
#' myArgs = c("code_path=E:/CA-SA/DADGP/scripts/",
#'            "raw_data_path=E:/CA-SA/DADGP/data/raw_data/",
#'            "reports_path=E:/CA-SA/DADGP/data/reports/",
#'            "list_cr=['810', '847']")
#'
#' get_inputs(myArgs, wanted_inputs = list("list_cr", "raw_data_path"), verbose = FALSE)
get_inputs <- function(myArgs, wanted_inputs = list(), verbose = TRUE){
  ## Working environment
  function_name <- "get_inputs"

  ## Initialization # Make inputs as vector
  wanted_inputs <- unlist(wanted_inputs)
  myArgs <- unlist(myArgs)
  myArgs <- sapply(myArgs, URLdecode)
  ## Identify unprovided inputs
  provided_inputs <- sapply(strsplit(myArgs, '='), function(x)x[1])
  unprovided_inputs <- unlist(setdiff(wanted_inputs, provided_inputs))
  if (length(unprovided_inputs) > 0){
    stop(paste0(function_name, ": you asked for some inputs that weren't provided. Here is the list: ",
                paste(unprovided_inputs, collapse = ", ")))
  }

  ## Edit those which where fed in my Args #To-do change code with get
  for (arg in myArgs){
    argDecode <- unlist(strsplit(arg,'='))
    if (argDecode[[1]] %in% wanted_inputs){
		if ((length(argDecode) == 3 & argDecode[3] == "list") || grepl('\\[*\\]', argDecode[2])){ # Parse for list
		# Remove quotes in char (because python might throw too much quotes)
        argDecode[2] <- gsub("\"", "",gsub("\'", "", argDecode[2]))
        # remove braces
        sub_decode <- substr(argDecode[2], 2, nchar(argDecode[2]) -1)
        # split
        argDecode[2] <- list(strsplit(sub_decode, ", ")[[1]])

      }
      if (! is.na(argDecode[3]) & ! is.null(argDecode[3]) & exists(paste0("as.", argDecode[3]))){
        assign(argDecode[[1]], get(paste0("as.", argDecode[3]))(argDecode[2]))
      }
      else{
        assign(argDecode[[1]], argDecode[2])
      }
    }
  }

  ## Return list of args
  result <- list()
  for (arg in wanted_inputs){
    result <- c(result, get(arg))
  }
  names(result) <- wanted_inputs

  ## Wrapp-up
  if (verbose){
    print(paste0(function_name, ": I will use the following inputs"))
    print(result)
  }
  return(result)
}
