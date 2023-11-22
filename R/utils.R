#' @exportPattern "^[[:alpha:]]+"
#'
#' @import data.table
library(data.table)

#' @title
#' View data in Excel
#' @description
#' View data in Excel
#' @param data data.frame.
#' @details
#' !! WARNING!!
#' This will write the data to a temp file and the file will not be removed
#' and it is vulnerable for security threats.
#' Thus, do not use this function for any proprietary data.
#' Also, only one Excel view can be opened at one time.
#' @return None.
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' None
#' @examples
#' # not run
#' xlview(data.frame(a = 3, b = 4))
#'
#' @export
xlview = function(data) {
  data = as.data.frame(data)
  temp_file_path = paste0(tempdir(),
                          "\\kek_glkdj_flksdj_abcd_ef02_0_30.csv")
  if(file.exists(temp_file_path)) {
    file.remove(temp_file_path)
  }
  fwrite(data, temp_file_path, row.names = F)
  shell.exec(temp_file_path)
}
