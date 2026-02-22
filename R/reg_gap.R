reg_gaps <- function(data, cohort = "ACTIVE", type) {

  if (!is.data.frame(data)) {
    stop("The object is not a dataframe")
  }

  cohort <- toupper(cohort)

  if (!cohort %in% c("ACTIVE", "EXITED")) {
    stop("Cohort must be 'ACTIVE' or 'EXITED'")
  }

  # Filter dataset by cohort first
  data <- data |>
    filter(toupper(exit_status) == cohort)


  if (cohort == "ACTIVE") {

    overaged <- data |>
      mutate(check = age >= 21) |>
      distinct(cpims_ovc_id, .keep_all = TRUE) |>
      filter(check)

    btn18_20_nis <- data |>
      mutate(check = age >= 18 & age <= 20 &
               schoollevel == "Not in School") |>
      distinct(cpims_ovc_id, .keep_all = TRUE) |>
      filter(check)

    if (type == "overage") {
      return(overaged)
    } else if (type == "not in school") {
      return(btn18_20_nis)
    } else {
      stop("For ACTIVE cohort, type must be 'overage' or 'not in school'")
    }


  } else if (cohort == "EXITED") {

    wrong_exit_age <- data |>
      mutate(age_at_exit = as.numeric(dob %--% exit_date) / 365.25,
             wrong_flag = (
               (age_at_exit < 20 & exit_reason == "Over 20 Years") |
                 (age_at_exit < 18 & exit_reason == "Over 18 yrs and out of School")
             )) |>
      distinct(cpims_ovc_id, .keep_all = TRUE) |>
      filter(wrong_flag)

    if (type == "wrong exit reason") {
      return(wrong_exit_age)
    } else {
      stop("For EXITED cohort, type must be 'wrong exit reason'")
    }
  }
}
