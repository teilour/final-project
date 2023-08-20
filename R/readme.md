# Install and load packages
library(tidyverse)
library(gtsummary)

install.packages("here")


# Read in data to R
sauce_data <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/sauces.csv")
view(sauce_data)


sauces_col <- c("season", "sauce_number", "sauce_name", "scoville")
							 
sauces <- read_csv(here::here("data", "raw", "sauces.csv"),
								 skip = 1, col_names = sauces_col) |>
	mutate(saucenum_cat = factor(sauce_number, labels = (1:10)),
				 season = factor(season, labels = c("Season 1", "Season 2", "Season 3", "Season 4", "Season 5",
																					"Season 6", "Season 7", "Season 8", "Season 9", "Season 10",
																					"Season 11", "Season 12", "Season 13", "Season 14", "Season 15",
																					"Season 16", "Season 17", "Season 18", "Season 19", "Season 20",
																					"Season 21")))


# Customization of `tbl_summary()`

tbl_summary(
	sauces,
	by = season,
	include = c(season, saucenum_cat, sauce_name, scoville))


tbl_summary(
	sauces,
	by = season,
	include = c(season, saucenum_cat, sauce_name, scoville),
	label = list(
		seasonnum_cat ~ "Number of Season",
		sauce_name ~ "Sauce Name",
		scoville ~ "Scoville"
		),
		missing_text = "Missing")


tbl_summary(
	sauces,
	by = season,
	include = c(season, saucenum_cat, sauce_name, scoville),
	label = list(
	  season ~ "Season",
		seasonnum_cat ~ "Number of Season",
		sauce_name ~ "Sauce Name",
		scoville ~ "Scoville"), 
		missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")
