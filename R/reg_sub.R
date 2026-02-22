reg_sub <- function(reglist, status = 'ACTIVE', fy = NULL) {

  data <- janitor::clean_names(reglist) |>
    mutate(
      exit_date = ymd(exit_date),  # ensure it's a Date
      fin_year = case_when(
        between(exit_date, ymd("2025-10-01"), ymd("2026-09-30")) ~ "FY2026",
        between(exit_date, ymd("2024-10-01"), ymd("2025-09-30")) ~ "FY2025",
        between(exit_date, ymd("2023-10-01"), ymd("2024-09-30")) ~ "FY2024",
        between(exit_date, ymd("2022-10-01"), ymd("2023-09-30")) ~ "FY2023",
        between(exit_date, ymd("2021-10-01"), ymd("2022-09-30")) ~ "FY2022",
        between(exit_date, ymd("2020-10-01"), ymd("2021-09-30")) ~ "FY2021",
        between(exit_date, ymd("2019-10-01"), ymd("2020-09-30")) ~ "FY2020",
        TRUE ~ "Old FY"
      )
    )

  active <- data |>
    dplyr::filter(exit_status == 'ACTIVE') |>
    dplyr::distinct(cpims_ovc_id, .keep_all = TRUE)

  exited <- data |>
    dplyr::filter(exit_status == 'EXITED', fin_year == fy) |>
    dplyr::distinct(cpims_ovc_id, .keep_all = TRUE)

  calhiv <- data |>
    dplyr::filter(exit_status == 'ACTIVE', ovchivstatus == 'POSITIVE') |>
    dplyr::distinct(cpims_ovc_id, .keep_all = TRUE)

  hei <- data |>
    dplyr::filter(exit_status == 'ACTIVE', stringr::str_detect(ovchivstatus, 'HEI')) |>
    dplyr::distinct(cpims_ovc_id, .keep_all = TRUE)

  if(status == 'ACTIVE') {
    return (active)
  } else if(status == 'EXITED') {
    return (exited)
  } else {
    stop('Only active or exited are required')
  }

}
