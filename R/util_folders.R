#' This function gets the right folder for results
#' @param ... Second level and beyond
#' @param create_dir create directory if it does not exist
#' @param trailing_slash do you want a trailing /?
#' @param auto Is this running in auto (then the base directory is used) or interactive ("/interactive")?
#' @export
path <- function(..., create_dir = FALSE, trailing_slash = FALSE, auto = cs9::config$is_auto) {
  end_location <- glue::glue(fs::path(...), .envir = parent.frame(n = 1))
  end_location <- stringr::str_split(end_location, "/")[[1]]
  end_location <- end_location[end_location != ""]
  if (!auto) {
    if (length(end_location) == 1) {
      end_location <- c(end_location[1], "_interactive")
    } else if (length(end_location) >= 2) {
      end_location <- c(end_location[1], "_interactive", end_location[2:length(end_location)])
    }
  }

  retval <- paste0(c(config$path, end_location), collapse = "/")
  if (create_dir) {
    if (!fs::dir_exists(retval)) dir.create(retval, showWarnings = FALSE, recursive = TRUE)
  }
  if (trailing_slash) retval <- paste0(retval, "/")
  return(retval)
}

#' Creates folder if it doesn't exist
#' @param path The path
#' @export
create_folder_if_doesnt_exist <- function(path) {
  retval <- glue::glue(path, .envir = parent.frame(n = 1))
  if (!fs::dir_exists(retval)) dir.create(retval, showWarnings = FALSE, recursive = TRUE)
  return(retval)
}

#' Create latest folder
#'
#' This function copies results_folder/date til results_folder/latest
#' @param results_folder_name name of the results folder
#' @param date the date of extraction
#' @export
create_latest_folder <- function(results_folder_name, date) {
  from_folder <- path("output", results_folder_name, date)
  to_folder <- path("output", results_folder_name, "latest")
  processx::run("cp", c("-rT", from_folder, to_folder))
}
