## creates a package specific env
the <- new.env(parent = emptyenv())
the$latest_version <- ""
the$working_version <- ""
the$all_versions <- ""
the$summary_df <- data.frame()
