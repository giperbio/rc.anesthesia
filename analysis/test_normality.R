test_normality = function(data, col) {
    classes <- c("numeric", "Duration", "difftime", "hms", "POSIXct",
                 "POSIXlt", "Interval")

    checkmate::assert_tibble(data)
    checkmate::assert_choice(col, names(data))
    checkmate::assert_multi_class(data[[col]], classes)

    n <- length(gutils:::rm_na(data[[col]]))

    if (gutils:::test_temporal(data[[col]], rm = "Period")) {
        data[[col]] <- mctq:::extract_seconds(data[[col]])
    }

    if (n >= 3 && n <= 3000) {
        test_shapiro <- data[[col]] %>%
            gutils:::rm_na() %>%
            stats::shapiro.test()
    } else if(n >= 50) {
        test_lcks <- data[[col]] %>%
            gutils:::rm_na() %>%
            KScorrect::LcKS("pnorm")
    }

    if (exists("test_shapiro") && exists("test_lcks")) {
        list(test_shapiro = test_shapiro, test_lcks = test_lcks)
    } else if (exists("test_shapiro") && !(exists("test_lcks"))) {
        list(test_shapiro = test_shapiro)
    } else if (!(exists("test_shapiro")) && exists("test_lcks")) {
        list(test_lcks = test_lcks)
    }
}
