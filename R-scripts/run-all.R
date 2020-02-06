# this is a convience script that runs all the scripts to do the analysis
# just comment out any you do not want to run
# Note the mark down scripts are not included. These are designed to be stand alone
# and should include no key coding only descriptions

# this script has been updated from runAll to work with the survivor model

# this script is being updated for HILDA 18.
# All scripts are in Scripts directory

#------------------------------------------------------------------------------
# this block reads in the HILDA combined data and then appends the master data
rm(list = ls())
cat("Read in combined data files and save as single RDS \n")
source("0-read-comb-data.R")
rm(list = ls())
cat("Read in the master data and add to combined data frame \n")
source("1-read-master-data.R")
# a quick script that pulls off the labels attributes which are lost in the
# read master script
rm(list = ls())
source("2-get-labels.R")

#------------------------------------------------------------------------------
# this block does survivor models
# this takes 10-15 minutes to run
rm(list = ls())
cat("Create survivor data \n")
source("3-surv-define.R")

rm(list = ls())
cat("Modify exposure \n")
source("4-modify-exposure.R")

#------------------------------------------------------------------------------
# this block adds on ABS data to the HILDA exposure data
rm(list = ls())
cat("Add ABS Data \n")
source("5-add-ABS.R")
rm(list = ls())

#------------------------------------------------------------------------------

# this file creates a mapping of post codes to sgcc
cat("Map post codes to sgcc \n")
source("6-map-postcode.R")
rm(list = ls())

cat("Join RP to HILDA \n")
source("7-add-RP-prices.R")
rm(list = ls())

# now add HILDA own housing data
cat("Join Housing to HILDA \n")
source("8-add-HILDA-prices.R")
rm(list = ls())

# now prepare data table for thesis


# and final models for thesis
cat("Final Thesis model results \n")
source("9-models.R")
rm(list = ls())
