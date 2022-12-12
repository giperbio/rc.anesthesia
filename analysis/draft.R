# library(dplyr)
# library(lubridate)
# library(googlesheets4)
# library(magrittr)
# library(stats)

# mean(data$vd[data$period == 1])
# mean(data$vd[data$period == 2])
#
# data <- dplyr::tibble(subject = c(rep(1, 5), rep(1, 5)), vd = rnorm(10),
#                       period = c(rep(1, 5), rep(2, 5)))

data <- googlesheets4::read_sheet(
    "1ftCXiFtrx4a7qoszIVMCfQtejEjcIx8tIwBKgdKPcJU", "Dataset") %>%
    dplyr::mutate(
        sex = factor(sex, levels = c("Male", "Female")),
        ventral_plane_return_light =
            lubridate::dminutes(ventral_plane_return_light),
        ventral_plane_return_dark =
            lubridate::dminutes(ventral_plane_return_dark)
    )

# KScorrect::LcKS(rnorm(5000), "pnorm")
# stats::shapiro.test(rnorm(5000))
# ggplot2::qplot(rnorm(5000))
# test_normality(data, "ventral_plane_return_light")
# test_normality(data, "ventral_plane_return_dark")


# VD x period
stats::shapiro.test(data$ventral_plane_return_light)
stats::shapiro.test(data$ventral_plane_return_dark)

lubridate::as.duration(mean(data$ventral_plane_return_light))
lubridate::as.duration(mean(data$ventral_plane_return_dark))

lubridate::as.duration(mean(data$ventral_plane_return_light)) %>%
    as.numeric(.) / 60 / 60

lubridate::as.duration(mean(data$ventral_plane_return_dark)) %>%
    as.numeric(.) / 60 / 60

stats::t.test(data$ventral_plane_return_light,
              data$ventral_plane_return_dark,
              paired = TRUE)

# VD x period x sex

vd_light_male <- data$ventral_plane_return_light[data$sex == "Male"]
vd_light_female <- data$ventral_plane_return_light[data$sex == "Female"]
vd_dark_male <- data$ventral_plane_return_dark[data$sex == "Male"]
vd_dark_female <- data$ventral_plane_return_dark[data$sex == "Female"]

stats::shapiro.test(vd_light_male)
stats::shapiro.test(vd_light_female)
stats::shapiro.test(vd_dark_male)
stats::shapiro.test(vd_dark_female)

lubridate::as.duration(mean(vd_light_male))
lubridate::as.duration(mean(vd_light_female))
lubridate::as.duration(mean(vd_dark_male))
lubridate::as.duration(mean(vd_dark_female))

lubridate::as.duration(mean(vd_light_male)) %>% as.numeric(.) / 60 / 60
lubridate::as.duration(mean(vd_light_female)) %>% as.numeric(.) / 60 / 60
lubridate::as.duration(mean(vd_dark_male)) %>% as.numeric(.) / 60 / 60
lubridate::as.duration(mean(vd_dark_female)) %>% as.numeric(.) / 60 / 60

stats::t.test(vd_male_light, vd_male_dark, paired = TRUE)
stats::t.test(vd_female_light, vd_female_dark, paired = TRUE)

# VD x sex
vd_male <- c(
    data$ventral_plane_return_light[data$sex == "Male"],
    data$ventral_plane_return_dark[data$sex == "Male"]
)

vd_female <- c(
  data$ventral_plane_return_light[data$sex == "Female"],
  data$ventral_plane_return_dark[data$sex == "Female"]
)

stats::shapiro.test(vd_male)
stats::shapiro.test(vd_female)

lubridate::as.duration(mean(vd_male))
lubridate::as.duration(mean(vd_female))

lubridate::as.duration(mean(vd_male)) %>%
    as.numeric(.) / 60 / 60

lubridate::as.duration(mean(vd_female)) %>%
    as.numeric(.) / 60 / 60

stats::t.test(vd_male, vd_female, paired = FALSE)

# VD x sex x period

vd_male_light <- data$ventral_plane_return_light[data$sex == "Male"]
vd_female_light <- data$ventral_plane_return_light[data$sex == "Female"]
vd_male_dark <- data$ventral_plane_return_dark[data$sex == "Male"]
vd_female_dark <- data$ventral_plane_return_dark[data$sex == "Female"]

stats::shapiro.test(vd_male_light)
stats::shapiro.test(vd_female_light)
stats::shapiro.test(vd_male_dark)
stats::shapiro.test(vd_female_dark)

lubridate::as.duration(mean(vd_male_light))
lubridate::as.duration(mean(vd_female_light))
lubridate::as.duration(mean(vd_male_dark))
lubridate::as.duration(mean(vd_female_dark))

lubridate::as.duration(mean(vd_male_light)) %>% as.numeric(.) / 60 / 60
lubridate::as.duration(mean(vd_female_light)) %>% as.numeric(.) / 60 / 60
lubridate::as.duration(mean(vd_male_dark)) %>% as.numeric(.) / 60 / 60
lubridate::as.duration(mean(vd_female_dark)) %>% as.numeric(.) / 60 / 60

stats::t.test(vd_male_light, vd_female_light, paired = FALSE)
stats::t.test(vd_male_dark, vd_female_dark, paired = FALSE)

