# reg_sub <- function(reglist,
#                     status = c("ACTIVE", "EXITED", "CALHIV", "HEI"),
#                     fy = NULL) {
#
#   status <- match.arg(status)
#   status <- toupper(status)
#
#   if (status == "EXITED" && is.null(fy)) {
#     stop("You must supply a financial year (fy) when requesting EXITED records.")
#   }
#
#   data <- janitor::clean_names(reglist) |>
#     dplyr::mutate(exit_date = as.Date(exit_date, format = "%d/%m/%Y")) |>
#     dplyr::mutate(
#       exit_fin_year = dplyr::if_else(
#         lubridate::month(exit_date) >= 10,
#         paste0("FY", lubridate::year(exit_date) + 1),
#         paste0("FY", lubridate::year(exit_date))
#       )
#     ) |>
#     dplyr::relocate(exit_fin_year, .after = exit_date) |>
#     dplyr::distinct(cpims_ovc_id, .keep_all = TRUE)
#
#   # Base ACTIVE subset
#   active_data <- data |>
#     dplyr::filter(exit_status == "ACTIVE")
#
#   if (status == "ACTIVE") {
#     return(active_data)
#   }
#
#   if (status == "CALHIV") {
#     return(
#       active_data |>
#         dplyr::filter(ovchivstatus == "POSITIVE")
#     )
#   }
#
#   if (status == "HEI") {
#     return(
#       active_data |>
#         dplyr::filter(stringr::str_detect(ovchivstatus, "HEI"))
#     )
#   }
#
#   # Base EXITED subset
#
#   exited_data <- data |>
#     dplyr::filter(exit_status == "EXITED", !is.na(exit_fin_year)) |>
#     dplyr::distinct(cpims_ovc_id, .keep_all = TRUE)
#
# if (status == 'EXITED') {
#   return(
#     exited_data[exited_data$exit_fin_year == fy, ]
#   )
# }
# }


reg_sub <- function(
    reglist,
    exit_status = c('ACTIVE', 'EXITED'),
    cohort,
    fy = NULL) {



  exit_status <- toupper(match.arg(exit_status))

  # Clean data and compute financial year
  data <- reglist |>
    janitor::clean_names() |>
    dplyr::mutate(
      exit_date = as.Date(exit_date, format = "%d/%m/%Y"),
      exit_fin_year = dplyr::if_else(
        is.na(exit_date),
        NA_character_,
        dplyr::if_else(
          lubridate::month(exit_date) >= 10,
          paste0("FY", lubridate::year(exit_date) + 1),
          paste0("FY", lubridate::year(exit_date))
        )
      )
    ) |>
    dplyr::relocate(exit_fin_year, .after = exit_date) |>
    dplyr::distinct(cpims_ovc_id, .keep_all = TRUE)



  # =============== ACTIVE ================
  if (exit_status == "ACTIVE") {
    # Get active records
    active_data <- data |>
      dplyr::filter(exit_status == "ACTIVE")

    # Display cohort distribution
    cat("\n=== ACTIVE Cohort Processing ===\n")
    cat("Total ACTIVE records:", nrow(active_data), "\n")
    cat("Cohort distribution:\n")
    active_data |>
      dplyr::count(ovchivstatus, sort = TRUE) |>
      dplyr::filter(!is.na(ovchivstatus)) |>
      print()

    # Apply cohort filters
    if(is.null(cohort) || cohort == "ALL") {
      result <- active_data
      cat("ALL cohort:", nrow(result), "records\n")
    } else if(toupper(cohort) == "CALHIV") {
      result <- active_data |> dplyr::filter(ovchivstatus == "POSITIVE")
      cat("CALHIV cohort:", nrow(result), "records\n")
    } else if(toupper(cohort) == "HEI") {
      result <- active_data |> dplyr::filter(stringr::str_detect(ovchivstatus, 'HEI'))
      cat("HEI cohort:", nrow(result), "records\n")
    } else {
      stop("Unknown cohort for ACTIVE. Choose 'ALL', 'CALHIV', or 'HEI'.")
    }

    cat("===============================\n\n")
    return(result)
  }

  # ================= EXITED =======================
  if(exit_status == "EXITED") {

    # Set default FY if NULL
    if (is.null(fy)) {
      # Default to current FY based on today's date
      today <- Sys.Date()
      fy <- if (lubridate::month(today) >= 10) {
        paste0("FY", lubridate::year(today) + 1)
      } else {
        paste0("FY", lubridate::year(today))
      }
      message("Using current financial year: ", fy)
    }

    # First, filter for EXITED records and remove duplicates
    exited_base <- data |>
      dplyr::filter(exit_status == "EXITED") |>
      dplyr::filter(!is.na(exit_fin_year)) |>
      dplyr::filter(exit_reason != 'Duplicated') |>
      dplyr::distinct(cpims_ovc_id, .keep_all = TRUE)

    # Debug: Show what we're working with
    cat("\n=== Processing request for fy =", fy, "===\n")

    # Create a copy of fy for filtering to avoid any scoping issues
    target_fy <- fy

    # Apply cohort filters based on the target_fy
    if (is.null(cohort) || toupper(cohort) == "ALL") {
      result <- dplyr::filter(exited_base, exit_fin_year == target_fy)

      cat("ALL cohort in", target_fy, ":", nrow(result), "records\n")
      cat("First few exit_fin_year values in result:",
          paste(head(result$exit_fin_year), collapse = ", "), "\n")

      return(result)
    }

    if(toupper(cohort) == "CPA") {
      result <- dplyr::filter(exited_base,
                              exit_reason == "Case Plan Achievement",
                              exit_fin_year == target_fy)

      cat("CPA cohort in", target_fy, ":", nrow(result), "records\n")
      return(result)
    }

    if (toupper(cohort) == "EWG") {
      result <- dplyr::filter(exited_base,
                              exit_reason != "Case Plan Achievement",
                              exit_fin_year == target_fy)

      cat("EWG cohort in", target_fy, ":", nrow(result), "records\n")
      return(result)
    }

    stop("Unknown cohort for Exited. Choose 'ALL', 'CPA', or 'EWG'")
  }
}
