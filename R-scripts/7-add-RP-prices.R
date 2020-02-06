#--------Background------------------------------------------------------------ 
# This script takes the RP Data frames, summarises the housing data and
# adds to the HILDA exposed data.
# More commentary is provided in the code on how it does this

# Load the packages
library(tidyverse)

out_path <- "../output/"
rp_path <- "~/Data/RP Data/"

#--------Read in RP Sales Data and prepare ------------------------------------
# first the sales data
sales_names <- c(
  "Sirca_id", "suburb", "state", "postcode", "lga_code", "lga_name",
  "property_type", "contract_price", "contract_year", "contract_month",
  "settlement_year", "settlement_month", "sale_type_description",
  "rp_vg_land_area", "rp_est_land_area", "rp_bedrooms",
  "rp_bedrooms_derived", "rp_bathrooms", "rp_bathrooms_derived",
  "rp_car_spaces", "rp_lockup_garage"
)

# set up the column specification
spec_col <- cols(
  .default = col_integer(),
  suburb = col_character(),
  postcode = col_character(),
  state = col_character(),
  lga_code = col_character(),
  lga_name = col_character(),
  property_type = col_character(),
  sale_type_description = col_character()
)

# first read in the sales data
RpSales <- read_delim(
  file = paste0(rp_path, "Sales.csv"), delim = "^",
  col_names = sales_names, col_types = spec_col
)

# restrict to a reasonable price range as some items look in error
RpSales <- RpSales %>% filter(contract_price >= 1e5 & contract_price <= 5e6)

# create the year category based on contract year except if this is NA
# then use settlement year. Note some states (NT and SA) only provided the
# settlment year
RpSales$def_year <- RpSales$contract_year
RpSales$def_month <- RpSales$contract_month
missing_contract_date <- is.na(RpSales$contract_year)
RpSales$def_year[missing_contract_date] <- RpSales$settlement_year[missing_contract_date]
RpSales$def_month[missing_contract_date] <- RpSales$settlement_month[missing_contract_date]

# filter any data before 2000 and after 2017
RpSales <- RpSales %>% filter(def_year >= 2000 & def_year <= 2017)

## ----postcode and SGCC-------------------------------------------------------
# read in the state to postcode mapping
Postcode_hhsgcc <- read_csv(
  file = paste0(out_path, "postcode_SGCC.csv"),
  col_types =
    cols(
      postcode = col_character(),
      hhsgcc = col_integer(),
      hhsgcc_desc = col_character()
    )
)

# take off the description
Postcode_hhsgcc$hhsgcc_desc <- NULL

# convert post code to character for joining
RpSales$postcode <- as.character(RpSales$postcode)

# join the data
RpSales <- left_join(x = RpSales, y = Postcode_hhsgcc, by = "postcode")

#----NA post codes-----------------------------------------------------------
# for the NA postcodes map to the SGCC for regional centre implied by
# the state variable

state_hhsgcc <- data.frame(
  state = c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT"),
  hhsgcc = c(19, 29, 39, 49, 59, 61, 71, 81),
  stringsAsFactors = FALSE
)

# convert to character for joining
RpSales$state <- as.character(RpSales$state)

# get a listing of missing sgcc data
missing_hhsgcc <- is.na(RpSales$hhsgcc)

# create a data frame with all the impacted states
missing_states <- data.frame(
  state = RpSales$state[missing_hhsgcc],
  stringsAsFactors = FALSE
)

# inner join the hhsgcc onto this frame
missing_states <- left_join(
  x = missing_states, y = state_hhsgcc,
  by = "state"
)

# the overwrite the updated hhsgcc
RpSales$hhsgcc[missing_hhsgcc] <- missing_states$hhsgcc

#----Create summary data for linking to combined by hhsgcc -------------------
# note that postcode can be substituted for hhsgcc when working with
# confidential data set.

# To get the median of the lower quartile this is simply the quantile 0.125

sumSales <- RpSales %>%
  group_by(hhsgcc, def_year) %>%
  summarise(
    RP_HousePrice_count = n(),
    RP_HousePrice_mean = mean(contract_price),
    RP_HousePrice_median = median(contract_price),
    RP_HousePrice_sd = sd(contract_price),
    RP_HousePrice_LQmed = quantile(contract_price, 0.125)
  )



#--------Read in RP listings data and prepare ---------------------------------
# now the listings data
listNames <- c(
  "Sirca_id", "suburb", "state", "postcode", "lga_code", "lga_name",
  "listing_category", "most_recent_rental_period",
  "most_recent_listing_price_desc", "most_recent_listing_price_from",
  "most_recent_listing_price_to",
  "most_recent_listing_year", "most_recent_listing_month",
  "rp_listing_area", "rp_bedrooms",
  "rp_bathrooms", "rp_car_spaces", "rp_lockup_garage"
)

# set up the column specification
specCol <- cols(
  .default = col_integer(),
  suburb = col_character(),
  state = col_character(),
  postcode = col_character(),
  lga_code = col_character(),
  lga_name = col_character(),
  listing_category = col_character(),
  most_recent_rental_period = col_character(),
  most_recent_listing_price_desc = col_character()
)

# first read in the listings data
RpRental <- read_delim(
  file = paste0(rp_path, "Listings.csv"), delim = "^",
  col_names = listNames, col_types = specCol
)

# now filter to retain only rentals
RpRental <- filter(RpRental, listing_category == "Rental")

# and prices in a sensible range
RpRental <- filter(RpRental, (most_recent_listing_price_from >= 100) &
  (most_recent_listing_price_from <= 10000))

# shorten some of the names
names(RpRental)[names(RpRental) == "most_recent_listing_year"] <- "def_year"
names(RpRental)[names(RpRental) == "most_recent_listing_month"] <- "def_month"
names(RpRental)[names(RpRental) == "most_recent_listing_price_from"] <- "weekly_rent"

# similar to RpSales add sgcc data
# convert post code to character for joining
RpRental$postcode <- as.character(RpRental$postcode)

# join the data
RpRental <- left_join(x = RpRental, y = Postcode_hhsgcc, by = "postcode")

# convert to character for joining
RpRental$state <- as.character(RpRental$state)

# get a listing of missing sgcc data
missing_hhsgcc <- is.na(RpRental$hhsgcc)

# create a data frame with all the impacted states
missing_states <- data.frame(
  state = RpRental$state[missing_hhsgcc],
  stringsAsFactors = FALSE
)

# inner join the hhsgcc onto this frame
missing_states <- left_join(
  x = missing_states, y = state_hhsgcc,
  by = "state"
)

# the overwrite the updated hhsgcc
RpRental$hhsgcc[missing_hhsgcc] <- missing_states$hhsgcc

#----Create summary data for linking to combined by hhsgcc -------------------
# note that postcode can be substituted for hhsgcc when working with
# confidential data set.
sumRents <- RpRental %>%
  group_by(hhsgcc, def_year) %>%
  summarise(
    RP_Rent_count = n(),
    RP_Rent_mean = mean(weekly_rent),
    RP_Rent_median = median(weekly_rent),
    RP_Rent_sd = sd(weekly_rent)
  )

# note we have missing data in sumRents that needs to be filled in and some
# sparse data

#----Load the HILDA exposure data----------------------------------------------
exposed <- read_rds(path = paste0(out_path, "exposure-03.rds"))

#----join the sales data-------------------------------------------------------
exposed <- exposed %>% left_join(sumSales, by = c(
  "hhsgcc" = "hhsgcc",
  "Year" = "def_year"
))

exposed <- exposed %>% left_join(sumRents, by = c(
  "hhsgcc" = "hhsgcc",
  "Year" = "def_year"
))

#----save the updated file-----------------------------------------------------
write_rds(x = exposed, path = paste0(out_path, "exposure-04.rds"))
