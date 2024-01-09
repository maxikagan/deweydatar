#' @exportPattern "^[[:alpha:]]+"
#'
#' @import httr2
library(httr2)

# make api endpoint from product ID
make_api_endpoint = function(path) {
  if(!startsWith(path, "https://")) {
    api_endpoint = paste0("https://app.deweydata.io/external-api/v3/products/", path, "/files")
    return(api_endpoint)
  } else {
    return (path)
  }
}

#' @title
#' Collects file list from server
#' @description
#' Collects the file list information from data server.
#' @param apikey (character) API Key.
#' @param product_path (character) API endpoint or Product ID.
#' @param start_page (integer) Start page of file list. Default is 1.
#' @param end_page (integer) End page of file list. Default is Inf.
#' @param partition_key_after (character) Start date character for files in the form of "20021-07-01".
#' Default is NA ("1000-01-01"), which indicates no limit.
#' @param partition_key_before (character) End date character for files in the form of "2023-08-21".
#' Default is NA ("9999-12-31"), which indicates no limit.
#' @param print_info (logical) Print file list information. Default is TRUE.
#' @details
#' None
#' @return A data.frame object contains files information.
#'   \item{index}{file index starts from 1.}
#'   \item{page}{page of the file list.}
#'   \item{link}{download link.}
#'   \item{partition_key}{date field to partiton files.}
#'   \item{file_name}{file name.}
#'   \item{file_extension}{file extension (e.g. .csv.gz.)}
#'   \item{download_lik}{same as \code{link}.}
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' \code{\link{slice_files_df}}, \code{\link{read_sample_data}}
#' @examples
#' # not run
#' files_df = get_file_list(apikey_, product_path_,
#'            start_page = 1, end_page = 3, print_info = T)
#'
#' @export
get_file_list = function(apikey, product_path,
                         start_page = 1, end_page = Inf,
                         partition_key_after = NA, partition_key_before = NA,
                         print_info = T) {
  product_path = make_api_endpoint(product_path)
  data_meta = NULL
  page_meta = NULL
  files_df = NULL

  if(is.na(partition_key_after)) {
    partition_key_after = "1000-01-01"
  }
  if(is.na(partition_key_before)) {
    partition_key_before = "9999-12-31"
  }

  page = start_page
  while(T) {
    req = request(product_path) %>%
      req_headers("X-API-KEY" = apikey) %>%
      req_headers("accept" = "application/json") %>%
      req_url_query ("page" = page) %>%
      req_url_query ("partition_key_after" = partition_key_after) %>%
      req_url_query ("partition_key_before" = partition_key_before)

    response = tryCatch(
      {
        req_perform(req)
      }, warning = function(cond) {
        message("Warning during httr2::req_perform.")
        message(cond)
        message("")
      }, error = function(cond) {
        message("Error during httr2::req_perform.")
        message(cond)
        message("")
      }
    )

    if(is.null(response)) {
      return(NULL)
    } else if(response$status_code == 401) {
      print(response);
      return(NULL);
    }

    # resp_content_type(response)
    # resp_status_desc(response)

    res_json = resp_body_json(response)

    # initialize
    if(res_json$page == start_page) {
      data_meta = data.frame(
        total_files = res_json$total_files,
        total_pages = res_json$total_pages,
        total_size = res_json$total_size/1000000,
        expires_at = res_json$expires_at
        )

    }

    message(paste0("Collecting files information for page ", res_json$page, "/",
                   res_json$total_pages, "..."))

    if(!is.null(res_json$partition_column)) {
      partition_column = res_json$partition_column
    } else {
      partition_column = NA
    }

    page_meta = rbind(page_meta,
                      data.frame(page = res_json$page,
                                 number_of_files_for_page = res_json$number_of_files_for_page,
                                 avg_file_size_for_page = res_json$avg_file_size_for_page/1000000,
                                 partition_column = partition_column)
                      )

    col_names_str = names(res_json$download_links[[1]])
    dn_link_unlist = unlist(res_json$download_links)

    # When partition column (key) is null
    if(!is.null(res_json$partition_column)) {
      dn_link_df = data.frame(matrix(dn_link_unlist, ncol = length(col_names_str), byrow = T))
    } else {
      dn_link_df = data.frame(matrix(dn_link_unlist, ncol = length(col_names_str)-1, byrow = T))
      dn_link_df = data.frame(dn_link_df[, 1], NA, dn_link_df[2:ncol(dn_link_df)])
    }
    colnames(dn_link_df) = col_names_str

    page_files_df = data.frame(page = res_json$page, dn_link_df)

    files_df = rbind(files_df, page_files_df)

    page = res_json$page + 1

    if((page > res_json$total_pages) | (page > end_page)) {
      message("Files information collection completed.")
      break
    }
  }

  # To date type
  files_df$partition_key = as.Date(files_df$partition_key)
  # Attach index
  files_df = data.frame(index = 1:nrow(files_df), files_df)
  # Backward compatibility
  files_df$download_link = files_df$link

  if(print_info == T) {
    message(" ")
    message("Files information summary ---------------------------------------")
    message(paste0("Total number of pages: ", data_meta$total_pages))
    message(paste0("Total number of files: ", data_meta$total_files))
    message(paste0("Total files size (MB): ", round(data_meta$total_size, digits = 2)))
    message(paste0("Average single file size (MB): ",
                   round(mean(page_meta$avg_file_size_for_page), digits = 2)))
    message(paste0("Date partition column: ", page_meta$partition_column[1]))
    message(paste0("Expires at: ", data_meta$expires_at))
    message("-----------------------------------------------------------------")
    message(" ")
  }

  #return(list(files_df = files_df,
  #            data_meta = data_meta, page_meta = page_meta));
  return(files_df)
}

#' @title
#' Read sample data into memory from a URL
#' @description
#' Read sample data into memory from a URL.
#' @param url (character) A file URL.
#' @param nrows (integer) Number of rows to read. Default is 100.
#' @details
#' None
#' @return A data.frame object contains data.
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' \code{\link{get_file_list}}, \code{\link{read_sample_data0}}
#' @examples
#' # not run
#' sample_data = read_sample_data(files_df$link[1], nrows = 100)
#'
#' @export
read_sample_data = function(url, nrows = 100) {
  # if(nrows > 1000) {
  #   message("Warning: set nrows no greater than 1000.");
  #   nrows = 1000;
  # }

  url_con = gzcon(url(url), text = T);
  df = read.csv(url_con, nrows = nrows, skipNul = TRUE, encoding = "UTF-8");

  return(df);
}

#' @title
#' Read sample data into memory from a URL
#' @description
#' Read first file data into memory using API key and product path.
#' @param apikey (character) API key.
#' @param product_path (character) API endpoint.
#' @param nrows (integer) Number of rows to read. Default is 100.
#' @details
#' None
#' @return A data.frame object contains data.
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' \code{\link{get_file_list}}, \code{\link{read_sample_data}}
#' @examples
#' # not run
#' sample_data0 = read_sample_data0(apikey_, pp_advan_wp, 100)
#'
#' @export
read_sample_data0 = function(apikey, product_path, nrows = 100) {
  files_df = get_file_list(apikey, product_path,
                           start_page = 1, end_page =1, print_info = T);
  message("    ");

  if(!is.null(files_df) & (nrow(files_df) > 0)) {
    return(read_sample_data(files_df$link[1], nrows));
  }
}

#' @title
#' Read local data into memory from a path
#' @description
#' Read local data into memory from a path
#' @param apikey (character) Path to a .csv.gz file.
#' @param nrows (integer) Number of rows to read. Default is -1 (all).
#' @details
#' This can be slow. Recommend to use fread function at data.table package.
#' @return A data.frame object contains data.
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' None
#' @examples
#' # not run
#' sample_data = read_sample_data(files_df$link[1], nrows = 100)
#'
#' @export
read_local_data = function(path, nrows = -1) {
  df = read.csv(gzfile(path), nrows = nrows);

  return(df);
}

#' @title
#' Download files from file list to a destination folder
#' @description
#' Download files from file list collected from \code{\link{get_file_list}}
#' to a destination folder
#' @param files_df (data.frame) File list collected from \code{\link{get_file_list}}.
#' @param dest_folder (character) Destination local folder to save files.
#' @param filename_prefix (character) Prefix for file names.
#' @param skip_exists (boolean) Skips downloading if the file
#' @details
#' The file links from  \code{\link{get_file_list}} are valid for 24 hours.
#' If download process passes the 24 hours period,
#' the \code{files_df} will be no longer valid
#' and \code{\link{get_file_list}} must be called again to collect new
#' \code{files_df}. Then run this function again with \code{skip_exists = TRUE} option.
#' @return None.
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' \code{\link{download_files0}}
#' @examples
#' # not run
#' download_files(files_df, "C:/temp", "advan_wp_", skip_exists = TRUE)
#'
#' @export
download_files = function(files_df, dest_folder, filename_prefix = NULL, skip_exists = TRUE) {
  dest_folder = gsub("\\", "/", dest_folder, fixed = T);

  if(!endsWith(dest_folder, "/")) {
    dest_folder = paste0(dest_folder, "/");
  }

  # number of files
  num_files = nrow(files_df);
  for (i in 1:num_files) {
    message(paste0("Downloading ", i, "/", num_files,
                   " (file index = ", files_df$index[i], ")"))

    file_name = paste0(filename_prefix, files_df$file_name[i]);

    dest_path = paste0(dest_folder, file_name)
    if (file.exists(dest_path) && skip_exists) {
      print(paste0("File ", dest_path, " already exists. Skipping..."))
      next
    }

    message(paste0("Writing ", dest_path))
    message("Please be patient. It may take a while...")

    req = request(files_df$link[i])
    response = req_perform(req)

    file_con = file(dest_path, "wb")
    writeBin(response$body, file_con)
    close(file_con)

    message("   ")
  }
}

#' @title
#' Download files with API key and product path to a destination folder
#' @description
#' Download files with API key and product path to a destination folder.
#' @param apikey (character) API Key.
#' @param product_path (character) API endpoint or Product ID.
#' @param dest_folder (character) Destination local folder to save files.
#' @param filename_prefix (character) Prefix for file names.
#' @param skip_exists (boolean) Skips downloading if the file
#' @details
#' The file links inside this function are valid for 24 hours.
#' If download process passes the 24 hours period,
#' this function must be called again with \code{skip_exists = TRUE} option.
#' @return None.
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' \code{\link{download_files}}
#' @examples
#' # not run
#' download_files0(apikey_, product_path, "C:/temp")
#'
#' @export
download_files0 = function(apikey, product_path, dest_folder,
                           filename_prefix = NULL, skip_exists = TRUE) {
  files_df = get_file_list(apikey, product_path, print_info = T);
  message("   ");

  download_files(files_df, dest_folder, filename_prefix, skip_exists);
}

#' @title
#' Slice files_df from \code{get_file_list}
#' @description
#' Slice \code{files_df} from \code{get_file_list} for specific data range
#' of from start_date to end_date.
#' For example, start_date = "2023-08-14", end_date = "2023-08-21".
#' @param files_df (data.frame) \code{files_df} from \code{get_file_list}.
#' @param start_date (character) Start date character for files in the form of "2023-08-21".
#' @param end_date (character) End date character for files in the form of "2023-08-21".
#' Default is \code{NULL}, which indicates slice to the last date.
#' @details
#' None
#' @return None.
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' \code{\link{get_file_list}}
#' @examples
#' # not run
#' sliced_files_df = slice_files_df(files_df, "2023-09-01", "2023-09-10")
#'
#' @export
slice_files_df = function(files_df, start_date, end_date = NULL) {
  start_date = as.Date(start_date)
  if(is.null(end_date)) {
    end_date = Inf
  }
  end_date = as.Date(end_date)

  sliced_df = files_df[(start_date <= files_df$partition_key) &
                         (files_df$partition_key <= end_date), ]
  return (sliced_df)
}
