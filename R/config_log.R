#' Update Configuration Log
#'
#' Logs configuration updates with relevant metadata such as timestamp,
#' session state, task name, and a custom message. The function inserts
#' the log entry into the `config_log` table in the current configuration.
#'
#' @param ss Character. Surveillance system identifier. Defaults to `"unspecified"`.
#' @param task Character. Name of the task being logged. Defaults to `"unspecified"`.
#' @param message Character. Custom message describing the log entry. Must not be `NULL`.
#'
#' @details
#' The function records the type of interaction (automatic or interactive),
#' session state, task description, and a user-provided message in the configuration log.
#' It throws an error if the `message` argument is `NULL`.
#'
#' @return No return value; this function is called for its side effect of inserting
#' a log entry into the `config_log` table.
#'
#' @examples
#' \dontrun{
#' update_config_log(ss = "weather", task = "data_import", message = "Imported dataset successfully.")
#' }
#'
#' @export
update_config_log <- function(
    ss = "unspecified",
    task = "unspecified",
    message = NULL
) {
  stopifnot(!is.null(message))

  datetime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  date <- stringr::str_sub(start_datetime, 1, 10)

  to_upload <- data.table(
    auto_interactive = ifelse(config$is_auto, "auto", "interactive"),
    ss = ss,
    task = task,
    message = message
  )
  config$tables$config_log$insert_data(to_upload)
}
