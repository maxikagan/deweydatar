#' @exportPattern "^[[:alpha:]]+"
#'
#' @import jsonlite
library(jsonlite)

json_to_df1 = function(x, keys=NULL, asis=F) {
  ret_val = NULL;

  if(asis) {
    ret_val = unlist(fromJSON(x))
  } else {
    tryCatch(
      {
        ret_val = unlist(fromJSON(x))
        if(!is.null(keys)) {
          names1 = names(ret_val)
          # names1 selected by keys
          selected = names1 %in% keys
          # selected by keys
          ret_val = ret_val[names1[selected]]

          # not in names1
          not_exist = !(keys %in% names1)
          # Set to NA
          if(sum(not_exist) >0) {
            ret_val[keys[not_exist]] = NA
          }

          # select in keys order
          ret_val= ret_val[keys]
        }
      },
      error = function(e) {
        #message("Error in json_func.")
        #message(e)
      },
      warning = function(e) {
        #message("Warning in json_func.")
        #message(e)
      }, finally = {
      }
      )

    if(is.null(ret_val)) {
      if(is.null(keys)) {
        message("JSON text error with the JSON text: ")
        message(paste0("\"", x, "\""))
        message("If you want to force out NA data.frame, provide keys.")
        stop("Parsing stopped.")
      } else {
        ret_val = rep(NA, length(keys))
        names(ret_val) = keys
      }
    }
  }

  return(ret_val)
}

to_json_pass_by = function(x) {
  # x example
  # '{{"AssocDegree",7.27},{"BachDegree",15.23},{"DoctDegree",0.94},{"HighSchGrad",50.97},{"MastDegree",5.93},{"NoDegree",18.14},{"ProfSchDegree",1.52}}'
  x = substr(x, 2, nchar(x)-1) # remove first { and last }
  x = gsub('\",', '\":', x)
  x = gsub('\\},\\{',',', x)
  # x

  return (x)
}

#' @title
#' Convert JSON data rows to data.frame
#' @description
#' Convert JSON data rows to data.frame.
#' @param keyval (character) key-value pairs text (JSON, etc.).
#' @param keys (character) keys (vector) to populate.
#' @param type (character) c("json", "pass_by").
#' @param asis (boolean) Populate all keys in the data without error checking.
#' @details
#' None
#' @return A data.frame object contains key-values.
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' @examples
#' # not run
#' aa = keyval_to_df(sample_data$POPULARITY_BY_DAY,
#'                   c("Friday", "Monday", "Whatday"), asis = F)
#'
#' @export
keyval_to_df = function(keyval, keys=NULL, type="json", asis=F) {
  if(asis & !is.null(keys)) {
    message("Wtih asis=TRUE, all key-value pair will be populated.")
    message("Keys parameter will be ignored.")
  }

  type = tolower(type)
  populated_df = NULL;

  if(type == "json") {
    # do nothing
  } else if(type %in% c("pass_by", "passby")) {
    # convert to json
    keyval = to_json_pass_by((keyval))
  } else {
    return (NULL)
  }

  populated_df = do.call(rbind,
                        lapply(keyval, json_to_df1, keys = keys, asis))
  return (as.data.frame(populated_df))
}


