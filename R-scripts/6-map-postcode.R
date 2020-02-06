#------------------------------------------------------------------------------
# this script loads ABS data to map the RpData Local Government Areas
# (provided by each state's Valuer General) to the SGCC used in HILDA
# Note the 2011 definitions are used from the ABS as this is the HILDA approach
#------------------------------------------------------------------------------

# standard libaries
library(tidyverse)
library(openxlsx)

out_path <- "../output/"
rp_path <- "~/Data/RP Data/"
abs_path <- "~/Documents/MasterEconAnalysis/Thesis/ABS Data/"

#------------------------------------------------------------------------------
# first read in the RpData information so we know which postcode are needed
#------------------------------------------------------------------------------

# only keep the following columns from the Rp Data
colNames <- c("postcode", "state", "suburb")

# first the sales data
salesNames <- c(
  "Sirca_id", "suburb", "state", "postcode", "lga_code", "lga_name",
  "property_type", "contract_price", "contract_year", "contract_month",
  "settlement_year", "settlement_month", "sale_type_description",
  "rp_vg_land_area", "rp_est_land_area", "rp_bedrooms",
  "rp_bedrooms_derived", "rp_bathrooms", "rp_bathrooms_derived",
  "rp_car_spaces", "rp_lockup_garage"
)

# set up the column specification
specCol <- cols(
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
  col_names = salesNames, col_types = specCol
)

# selects columns and removes duplicates
RpSales <- RpSales %>%
  select(colNames) %>%
  distinct()
# convert all to character for joining
RpSales[, colNames] <- lapply(RpSales[, colNames], as.character)

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
RpList <- read_delim(
  file = paste0(rp_path, "Listings.csv"), delim = "^",
  col_names = listNames, col_types = specCol
)

# only keep the columns of interest and remove duplicates
RpList <- RpList %>%
  select(colNames) %>%
  distinct()
# convert all to character for later use
RpList[, colNames] <- lapply(RpList[, colNames], as.character)

# row bind, dedup and remove NAs
RpBoth <- RpSales %>%
  bind_rows(RpList) %>%
  distinct() %>%
  filter(!is.na(postcode))

# clean up the dataframes
rm(RpList, RpSales, colNames, specCol)

#------------------------------------------------------------------------------
# This part reads in the geo data from the ABS
#------------------------------------------------------------------------------

# Read in the data that maps postcode to the SA4 Blocks in the census
# the link to this file for future reference is
# http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.006July%202011?OpenDocument

ABS_postcode_SA4 <-
  read.xlsx(
    xlsxFile = paste0(abs_path, "ABS_postcode_SA4_2011.xlsx"),
    sheet = "Table 3", startRow = 6, colNames = TRUE,
    rowNames = FALSE, detectDates = TRUE
  )

# rename the seemingly duplicate post code column (and make it lower case)
names(ABS_postcode_SA4)[1] <- "postcode"
names(ABS_postcode_SA4)[2] <- "postcode2"

# see are any different - the answer is no
all(ABS_postcode_SA4$postcode == ABS_postcode_SA4$postcode2, na.rm = TRUE)
# lost the second one for redundancy
ABS_postcode_SA4$postcode2 <- NULL

# drop the last row which is a copyright statement do this by removing NAs
ABS_postcode_SA4 <- filter(ABS_postcode_SA4, !is.na(SA4_CODE_2011))

# make percentage numeric and lose ratio for redundancy
ABS_postcode_SA4$RATIO <- NULL
ABS_postcode_SA4$PERCENTAGE <- as.numeric(ABS_postcode_SA4$PERCENTAGE)

# Next read in the data that maps the SA4 Blocks to SGCC
# http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202011?OpenDocument

ABS_SA4_SGCC <- read_csv(file = paste0(abs_path, "SA4_2011_AUST.csv"))

#------------------------------------------------------------------------------
# Inner join the two ABS data sources and then see the resulting mapping
#------------------------------------------------------------------------------

# convert joining column to same type
ABS_SA4_SGCC$SA4_CODE_2011 <- as.character(ABS_SA4_SGCC$SA4_CODE_2011)
ABS_postcode_SGCC <- inner_join(ABS_postcode_SA4, ABS_SA4_SGCC,
  by = "SA4_CODE_2011"
)

# only keep the columns needed
ABS_postcode_SGCC <- ABS_postcode_SGCC %>% select(
  postcode,
  GCCSA_CODE_2011, GCCSA_NAME_2011, PERCENTAGE
)

# group by postcode leaving only true duplicates
ABS_postcode_SGCC <- ABS_postcode_SGCC %>%
  group_by(
    postcode,
    GCCSA_CODE_2011, GCCSA_NAME_2011
  ) %>%
  summarise(PERC = sum(PERCENTAGE))

# store the duplicates
dup <- ABS_postcode_SGCC$postcode[duplicated(ABS_postcode_SGCC$postcode)]
dup_postcode <- ABS_postcode_SGCC %>% filter(postcode %in% dup)

# data has been checked that no sum to more than 100%
# (that is no true duplicate)
# with following code
true_dups <- dup_postcode %>%
  group_by(postcode) %>%
  summarise(sumPerc = sum(PERC)) %>%
  filter(abs(sumPerc - 100) > 0.00001)
# there are a few but clearly all rounding issues.

# there is only one triple (all the others are doubles).
# As this triple the greatest entry is more than 50
# the following code can be used to delete the less than 50% entries
dup_delete <- dup_postcode %>% filter(PERC < 50)
dup_uid <- paste(dup_delete$postcode, dup_delete$GCCSA_CODE_2011, sep = "_")
ABS_postcode_SGCC <- mutate(ABS_postcode_SGCC,
  uid = paste(postcode, GCCSA_CODE_2011, sep = "_")
)

ABS_postcode_SGCC <- ABS_postcode_SGCC %>% filter(!(uid %in% dup_uid))

# now create a mapping to HILDA names for these SGCC areas
# noting that HILDA has fewer categories
mapSGCC <- data.frame(
  hhsgcc = c(
    11, 19,
    21, 29, 31, 39,
    41, 49, 51, 59,
    61, 61,
    71, 71,
    81, 81
  ),
  GCCSA_CODE_2011 = c(
    "1GSYD", "1RNSW",
    "2GMEL", "2RVIC", "3GBRI", "3RQLD",
    "4GADE", "4RSAU", "5GPER", "5RWAU",
    "6GHOB", "6RTAS",
    "7GDAR", "7RNTE",
    "8ACTE", "9OTER"
  ),
  stringsAsFactors = FALSE
)

# and HILDA descriptions
HildaSGCC <- data.frame(
  hhsgcc = c(
    11, 19, 21, 29, 31, 39, 41, 49,
    51, 59, 61, 71, 81
  ),
  hhsgcc_desc = c(
    "Greater Sydney", "Rest of NSW",
    "Greater Melbourne", "Rest of Vic",
    "Greater Brisbane", "Rest of Qld",
    "Greater Adelaide", "Rest of SA",
    "Greater Perth", "Rest of WA",
    "Tasmania", "Northern Territory", "Australian Capital Territory"
  ),
  stringsAsFactors = FALSE
)

# create the final mapping table
map_postcode_SGCC <- ABS_postcode_SGCC %>% inner_join(
  y = mapSGCC,
  by = "GCCSA_CODE_2011"
)
map_postcode_SGCC <- map_postcode_SGCC[, c("postcode", "hhsgcc")]

# note there are 16 missing post codes from the ABS being the following
mis_postcode <- setdiff(RpBoth$postcode, map_postcode_SGCC$postcode)

# the tabel below fills this in based on the Australia post website
mis_map_postcode_SGCC <- data.frame(
  postcode =
    c(
      "0834", "0839", "0873", "0874", "0875", "2817", "2818", "2822", "2826",
      "2838", "3213", "3234", "3336", "3358", "4672", "4892", "5611", "5701",
      "5719"
    ),
  hhsgcc = c(rep(71, 5), rep(19, 5), rep(29, 4), rep(39, 2), rep(49, 3)),
  stringsAsFactors = FALSE
)

# append to the existing mapping
map_postcode_SGCC <- map_postcode_SGCC %>% bind_rows(mis_map_postcode_SGCC)

# now add the descriptions
map_postcode_SGCC <- map_postcode_SGCC %>% inner_join(
  y = HildaSGCC,
  by = "hhsgcc"
)

# so now we can join to SGCC to RP_Data
# save the result for use in that logic (as short use csv for readability)
write_csv(map_postcode_SGCC, path = paste0(out_path, "postcode_SGCC.csv"))
