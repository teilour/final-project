# Install and load packages
install.packages("here")
install.packages("gtsummary")
install.packages("renv")
install.packages("yaml")
install.packages("usethis")

library(tidyverse)
library(gtsummary)
library(broom)
usethis::use_readme_rnd



# Renv
renv::init()
renv::snapshot()
source("renv/activate.R")



# Read in data to R
sauce_data <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/sauces.csv")
view(sauce_data)



# column names
sauces_col <- c("season", "sauce_number", "sauce_name", "scoville")



# read in raw data
sauces <- read_csv(here::here("data/raw/sauces.csv"),
									 skip = 1, col_names = sauces_col) |>
	mutate(sauce_number = factor(sauce_number, labels = (1:10)),
				 season = factor(season, labels = c("Season 1", "Season 2", "Season 3", "Season 4", "Season 5",
				 																	 "Season 6", "Season 7", "Season 8", "Season 9", "Season 10",
				 																	 "Season 11", "Season 12", "Season 13", "Season 14", "Season 15",
				 																	 "Season 16", "Season 17", "Season 18", "Season 19", "Season 20",
				 																	 "Season 21")))

here::here("data", "raw", "sauces.csv")



# Customization of `tbl_summary()`
tbl_summary(
	sauces,
	by = season,
	include = c(season, sauce_number, sauce_name, scoville))


tbl_summary(
	sauces,
	by = season,
	include = c(season, sauce_number, sauce_name, scoville),
	label = list(
		sauce_number ~ "Sauce Number",
		sauce_name ~ "Sauce Name",
		scoville ~ "Scoville"
	),
	missing_text = "Missing")


tbl_summary(
	sauces,
	by = season,
	include = c(season, sauce_number, sauce_name, scoville),
	label = list(
		sauce_number ~ "Sauce Number",
		sauce_name ~ "Sauce Name",
		scoville ~ "Scoville"
	),
	missing_text = "Missing") |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**")


# Univariate regression
tbl_uvregression(
	sauces,
	y = scoville,
	include = c(season, sauce_number, sauce_name, scoville),
	method = lm)


# Multivariable regressions
linear_model <- lm(scoville ~ season + saucenum + sauce_name,
									 data = sauces)

linear_model_int <- lm(scoville ~ season*saucenum + sauce_name,
											 data = sauces)

# Regression
tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		season ~ "Season",
		saucenum ~ "Sauce Number",
		sauce_name ~ "Sauce Name"
	))


tbl_no_int <- tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		season ~ "Season",
		saucenum ~ "Sauce Number",
		sauce_name ~ "Sauce Name"
	))

> tbl_int <- tbl_regression(
	linear_model_int,
	intercept = TRUE,
	label = list(
		season ~ "Season",
		saucenum ~ "Sauce Number",
		sauce_name ~ "Sauce Name",
		`season:sauce_name` ~ "Season/Sauce Name Interaction"
	))

> tbl_merge(list(tbl_no_int, tbl_int),
						tab_spanner = c("**Model 1**", "**Model 2**"))


# Series of univariate regressions
scoville_table <- tbl_uvregression(
	sauces,
	y = scoville,
	include = c(
		season, saucenum, sauce_name
	),
	method = lm
)
scoville_table


> inline_text(scoville_table, variable = "saucenum")


> logistic_model <- glm(sauce_name ~ season + saucenum + scoville,
												data = sauces, family = binomial())
> tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE)

# Using broom to combine regressions
mod_szn_cat <- lm(scoville ~ season, data = sauces)
summary(mod_szn_cat)
mod_saucenum_cat <- lm(scoville ~ saucenum, data = sauces)
mod_sauce_name_cat <- lm(scoville ~ sauce_name, data = sauces)

tidy_szn_cat <- tidy(mod_szn_cat, conf.int = TRUE)
tidy_saucenum_cat <- tidy(mod_saucenum_cat, conf.int = TRUE)
tidy_sauce_name_cat <- tidy(mod_sauce_name_cat, conf.int = TRUE)

dplyr::bind_rows(
	season = tidy_szn_cat,
	saucenum = tidy_saucenum_cat,
	sauce_name = tidy_sauce_name_cat, .id = "model") |>
	dplyr::mutate(
		term = stringr::str_remove(term, model),
		term = ifelse(term == "", model, term))

> tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE) |>
	tidycat::tidy_categorical(logistic_model, exponentiate = TRUE)
dplyr::select(-c(3:5))


# Figure
hist(sauces$scoville)


# Function
x <- c(450, 550, 600, 747, 1600)
new_mean <- function(x) {
	n <- length(x)
	mean_val <- sum(x) / n
	return(mean_val)
}

new_mean(x = x)
new_mean(x = c(450, 600, 1600))
