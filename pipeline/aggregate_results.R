################################################################################
#
# Aggregate Financial Impact Summaries
#
# Reads financial_impact_summary_*.csv files for all cities and produces
# a combined summary table.
#
# Output: results_csv/financial_impact_total.csv
#
################################################################################

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

cat("Aggregating financial impact summaries across cities...\n")

files <- list.files("results_csv",
                    pattern = "^financial_impact_summary_.*\\.csv$",
                    full.names = TRUE)

if (length(files) == 0) {
	warning("No financial_impact_summary_*.csv files found in results_csv/")
} else {
	total_list <- list()

	for (file in files) {
		city <- str_match(basename(file), "financial_impact_summary_(.*)\\.csv")[, 2]
		df <- read_csv(file, show_col_types = FALSE)
		df0 <- df %>% filter(adaptation == "0%")

		annuity_wide <- df0 %>%
			select(rcp, pct_delta_annuity) %>%
			pivot_wider(names_from = rcp, values_from = pct_delta_annuity) %>%
			mutate(Product = "Annuity", City = city) %>%
			select(Product, City, everything())

		insurance_wide <- df0 %>%
			select(rcp, pct_delta_insurance) %>%
			pivot_wider(names_from = rcp, values_from = pct_delta_insurance) %>%
			mutate(Product = "Insurance", City = city) %>%
			select(Product, City, everything())

		total_list[[length(total_list) + 1]] <- annuity_wide
		total_list[[length(total_list) + 1]] <- insurance_wide
	}

	final_df <- bind_rows(total_list)
	rcp_cols <- sort(setdiff(names(final_df), c("Product", "City")))
	final_df <- final_df %>% select(Product, City, all_of(rcp_cols))
	final_df$City <- str_to_title(final_df$City)

	for (col in rcp_cols) {
		if (is.numeric(final_df[[col]])) {
			final_df[[col]] <- round(final_df[[col]], 3)
		}
	}

	final_df <- final_df %>% arrange(Product, City)
	write_csv(final_df, "results_csv/financial_impact_total.csv")
	cat(sprintf("Saved: results_csv/financial_impact_total.csv (%d rows)\n", nrow(final_df)))
}
