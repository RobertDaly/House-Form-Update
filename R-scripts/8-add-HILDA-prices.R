#--------Background------------------------------------------------------------ 
# This script takes the HILDA data, summarises the housing data and
# adds to the HILDA exposed data.
# More commentary is provided in the code on how it does this

#----load libraries -----------------------------------------------------------
library(tidyverse)

out_path <- "../00-Output/"

# read in the data file
HILDA <- read_rds(path = paste0(out_path, "combined-master.rds"))

## ----Extract house values----------------------------------------------------
# notes this uses hhsgcc but can use postcode when available
# set up the complete panel
# remove zero house values
housePanel <- HILDA %>%
  filter(hsvalue > 0) %>%
  group_by(hhsgcc, wave) %>%
  summarise(
    HILDA_HousePrice_count = n(),
    HILDA_HousePrice_mean = mean(hsvalue),
    HILDA_HousePrice_median = median(hsvalue),
    HILDA_HousePrice_sd = sd(hsvalue)
  )

## ----Rental Cost Model---------------------------------------------------
# remove zero rental cost
rentPanel <- HILDA %>%
  filter(hsrnti > 0) %>%
  group_by(hhsgcc, wave) %>%
  summarise(
    HILDA_Rent_count = n(),
    HILDA_Rent_mean = mean(hsrnti),
    HILDA_Rent_median = median(hsrnti),
    HILDA_Rent_sd = sd(hsrnti)
  )

## ----Mortgage Cost Model-------------------------------------------------
# remove zero mortgage cost and the 9 cases with hhsgcc == -7
mortPanel <- HILDA %>%
  filter(hsmgi > 0) %>%
  group_by(hhsgcc, wave) %>%
  summarise(
    HILDA_Mortg_count = n(),
    HILDA_Mortg_mean = mean(hsmgi),
    HILDA_Mortg_median = median(hsmgi),
    HILDA_Mortg_sd = sd(hsmgi)
  )

#---------Join the three together for review purposes -------------------------
total <- housePanel %>%
  full_join(rentPanel, by = c("hhsgcc", "wave")) %>%
  full_join(mortPanel, by = c("hhsgcc", "wave"))


#----Load the HILDA exposure data----------------------------------------------
exposed <- read_rds(path = paste0(out_path, "exposure-04.rds"))

#----join the  data-------------------------------------------------------
exposed <- exposed %>% left_join(total, by = c("hhsgcc", "wave"))

#------- Add the deflated housing cost measures
exposed <- exposed %>% mutate(
  RP_HousePrice_median_real =
    RP_HousePrice_median * deflator,
  HILDA_HousePrice_median_real =
    HILDA_HousePrice_median * deflator,
  HILDA_Mortg_median_real =
    HILDA_Mortg_median * deflator,
  HILDA_Rent_median_real =
    HILDA_Rent_median * deflator,
  RP_HousePrice_LQmed_real =
    RP_HousePrice_LQmed * deflator
)

#---------Also the deflated income measures
exposed <- exposed %>% mutate(
  famInc_real = famInc * deflator,
  hifdip_real = hifdip * deflator,
  tifdip_real = tifdip * deflator
)

#----save the updated file-----------------------------------------------------
write_rds(x = exposed, path = paste0(out_path, "exposure-05.rds"))
