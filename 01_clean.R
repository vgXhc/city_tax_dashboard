library(readxl)
library(here)
library(tidyverse)
library(arrow)

read_tax <- function(year, skip = 0) {
  path <- here("data", paste0("TaxRoll", year, ".xlsx")) 
  read_excel(path, skip = skip)
}

# some years have junk rows on top; some don't

taxes_2023 <- read_tax(2023, 3)
taxes_2022 <- read_tax(2022, 3)
taxes_2021 <- read_tax(2021, skip = 0)
taxes_2020 <- read_tax(2020, skip = 0)
taxes_2019 <- read_tax(2019, skip = 0)
taxes_2018 <- read_tax(2018, skip = 0)
taxes_2017 <- read_tax(2017, skip = 0)
taxes_2016 <- read_tax(2016, skip = 0)




# identify common columns between 2022 and 2023
c_23 <- colnames(taxes_2023)
c_22 <- colnames(taxes_2022)

taxes_22_23 <- taxes_2022 |> select(intersect(c_23, c_22)) |> 
  rbind(taxes_2023 |> select(intersect(c_23, c_22))) |> 
  janitor::clean_names()


select_common_columns <- function(tax_df){
  cols_common <- intersect(colnames(taxes_2016), colnames(taxes_2021))
  tax_df[, cols_common]
}

taxes_16_21 <- map_dfr(list(taxes_2016,
             taxes_2017,
             taxes_2018,
             taxes_2019,
             taxes_2020,
             taxes_2021), select_common_columns) |> 
  janitor::clean_names()


# 2022/23 column names don't match the older ones
taxes_22_23 <- taxes_22_23 |> 
  rename(
    parcel = prop_id,
    location = prop_location,
    assessed_value_land = land_assessment,
    assessed_value_improvement = impr_assessment,
    total_assessed_value = total_assessment,
    est_fair_mkt_land = est_fmv_land,
    est_fair_mkt_improvement = est_fmv_impr,
    total_estimated_fair_market = est_fmv_total,
    current_county_net_tax = county_tax,
    current_matc_net_tax = matc_tax,
    current_school_net_tax = net_school_tax,
    current_city_net_tax = city_tax,
    total_current_net_tax = net_tax,
    total_current_tax = gross_tax
  ) 
  #mutate(across(where(is.character), str_to_title)) #values are all caps

# character vectors in 22/23 are all caps. Can't use string conversion to fix because of names like O'Brien or McKenna
# create list of all parcels and their names from 2016-21 files
parcels_16_21  <-  
  taxes_16_21 |> 
    select(parcel, location) |> 
    distinct()
# update street names of 2022/23 file based on 16-21 names
taxes_22_23 <- taxes_22_23 |> 
  left_join(parcels_16_21, by = "parcel") |> 
  mutate(location = location.y)

cols_common <- intersect(colnames(taxes_16_21), colnames(taxes_22_23))

# data frame for all years
taxes <- rbind(taxes_16_21[, cols_common], taxes_22_23[, cols_common])





taxes <- taxes |> 
  group_by(parcel) |> 
  mutate(city_net_tax_change = current_city_net_tax/lag(current_city_net_tax, order_by = tax_year) - 1,
         current_county_net_tax_change = current_county_net_tax / lag(current_county_net_tax, order_by = tax_year) - 1,
         current_matc_net_tax_change = current_matc_net_tax / lag(current_matc_net_tax, order_by = tax_year) - 1,
         current_school_net_tax_change = current_school_net_tax / lag(current_school_net_tax, order_by = tax_year) - 1,
         total_net_tax_change = total_current_net_tax / lag(total_current_net_tax, order_by = tax_year) - 1,
         total_assessed_value_change = total_assessed_value / lag(total_assessed_value, order_by = tax_year) - 1) |> 
  select(tax_year, parcel, location, total_assessed_value, current_county_net_tax, current_city_net_tax, current_matc_net_tax, current_school_net_tax, total_current_net_tax, ends_with("_change"))

# remove "current_" from column names
names(taxes) <- str_remove(names(taxes), "current_")

# function for indexing: 
index_function <- function(x){
  # input: x, a vector
  # output: x_indexed, a vector 
  initial <- x[1]
  map_dbl(x, function(x){x/initial * 100})
}

# add indexed values
taxes <- taxes |> 
  group_by(parcel) |> 
  mutate(across(ends_with(c("net_tax", "assessed_value")), index_function, .names = "{.col}_indexed"))

# read CPI data (from data.bls.gov) and index to 2016
cpi <- read_excel("~/Downloads/SeriesReport-20241005093602_290b8b.xlsx", skip = 10)

cpi <- cpi |> 
  rename(tax_year = Year) |> 
  mutate(cpi_indexed = index_function(Annual)) |> 
  select(tax_year, cpi_indexed)

# load annual inflation rate data
inflation <- read_csv("data/fredgraph.csv") |> 
  mutate(tax_year = year(DATE)) |> 
  rename(inflation_rate = CPIAUCSL_PC1) |> 
  select(tax_year, inflation_rate)

# add inflation to tax parcels
taxes <- taxes |> 
  left_join(y = inflation) |> 
  left_join(y = cpi)

# save files in various formats
# write_csv(taxes, "data/taxes_all.csv")
# write_rds(taxes, "data/taxes_all.RDS", compress = "gz")
arrow::write_parquet(taxes, sink = "madison_tax_dashboard/data/taxes_all.parquet")

# long version of data set for creating plot
## set up pivot columns
pivot_cols <- c("total_assessed_value", 
                "county_net_tax", 
                "city_net_tax", 
                "school_net_tax", 
                "total_net_tax")

pivot_cols <- c(pivot_cols, paste0(pivot_cols, "_indexed"))

taxes_long <- taxes |>
  pivot_longer(cols = all_of(pivot_cols) ,names_to = "variable", values_to = "value") |>
  group_by(parcel, variable) |>
  mutate(change = value/lag(value, order_by = tax_year) - 1)

# save long data as parquet file
arrow::write_parquet(taxes_long, "madison_tax_dashboard/data/taxes_long.parquet")

library(ntdr)
x <- get_ntd()

madison <- x |> filter(agency == "City of Madison" & modes == "MB" & tos == "DO" & year(month) >2018)

max_month <-  madison |>  filter(year(month) == max(year(month))) |> 
  summarize(max(month(month, label = T, abbr = F))) |> pull()
madison |> 
  mutate(month_label = month(month, label = T, abbr = T)) |> 
  ggplot(aes(month, value)) +
  geom_col(aes(fill = month_label)) +
  geom_line() +
  theme_minimal() +
  #geom_point(data = x |> filter(agency == "City of Madison" & modes == "MB" & tos == "DO" & year(month) > 2016 & month(month) == 11)) +
  scale_x_date() +
  ylab("Unlinked passenger trips") +
  ylim(0,1500000) +
  labs(title = "Madison Metro monthly ridership", 
       subtitle = paste0("Latest month shown: ", max_month, " 2024"),
       caption = "Data: National Transit Database")


madison_line <- madison |> 
  mutate(month_label = month(month, label = T, abbr = T),
         year = as.factor(year(month))) |> 
  select(year, value, month_label)
  # group_by(year, month_label) |>
madison_line |> 
  ggplot(aes(month_label, value, color = year, group = year)) +
  geom_line() +
  geom_point() +
  gghighlight::gghighlight(year == 2024,
  unhighlighted_params = list(colour = NULL, alpha = 0.3)) +
  geom_text(data = madison_line |> filter(month_label == "Dec"), aes(label = year)) +
  ylab("Unlinked passenger trips") +
  xlab("Month") +
  theme_minimal() +
  ylim(0,1500000) +
  labs(title = "Madison Metro monthly ridership", 
       subtitle = paste0("Latest month shown: ", max_month, " 2024"),
       caption = "Data: National Transit Database")
