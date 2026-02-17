reg_sub <- function(data, cohort) {
  active <- data |>
    dplyr::filter(exit_status == 'ACTIVE') |>
    dplyr::distinct(cpims_ovc_id, .keep_all = TRUE)

  exited <- data |>
    dplyr::filter(exit_status == 'EXITED') |>
    dplyr::distinct(cpims_ovc_id, .keep_all = TRUE)

  if(cohort == 'ACTIVE') {
    return (active)
  } else if(cohort == 'EXITED') {
    return (exited)
  } else {
    stop('Only active or exited are required')
  }

}
