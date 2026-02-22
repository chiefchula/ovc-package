reg_sub <- function(reglist,
                    status = c("ACTIVE", "EXITED", "CALHIV", "HEI"),
                    fy = NULL) {

  status <- match.arg(status)
  status <- toupper(status)

  if (status == "EXITED" && is.null(fy)) {
    stop("You must supply a financial year (fy) when requesting EXITED records.")
  }

  data <- janitor::clean_names(reglist) |>
    dplyr::mutate(
      exit_date = lubridate::ymd(exit_date),
      fin_year = dplyr::if_else(
        lubridate::month(exit_date) >= 10,
        paste0("FY", lubridate::year(exit_date) + 1),
        paste0("FY", lubridate::year(exit_date))
      )
    ) |>
    dplyr::distinct(cpims_ovc_id, .keep_all = TRUE)

  # Base ACTIVE subset
  active_data <- data |>
    dplyr::filter(exit_status == "ACTIVE")

  if (status == "ACTIVE") {
    return(active_data)
  }

  if (status == "CALHIV") {
    return(
      active_data |>
        dplyr::filter(ovchivstatus == "POSITIVE")
    )
  }

  if (status == "HEI") {
    return(
      active_data |>
        dplyr::filter(stringr::str_detect(ovchivstatus, "HEI"))
    )
  }

  if (status == "EXITED") {
    return(
      data |>
        dplyr::filter(exit_status == "EXITED", !is.na(exit_date), fin_year == fy)
    )
  }
}
