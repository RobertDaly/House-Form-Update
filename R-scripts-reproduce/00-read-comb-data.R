# This script reads in the Hilda Data combined data, does preliminary processing and
# then saves a data file to which Master data is added by the next script.
# It is in a separate script as changes to the Master read in do not require this
# script to be rerun.
#

# Load the package that reads in stata files and to reshape data
library(haven)
library(tidyverse)
library(lubridate)

# Create a funtion that takes the first letter off a named variable.
# Many Hilda variables include the wave id as the first variable letter.
strip_first_char <- function(x) {
  substr(x, 2, nchar(x))
}

# ------------------------------------------------------------------------------------
# Create the function to read in a single HILDA wave file with only the variables
# required to save on storage.
# ------------------------------------------------------------------------------------
read_HILDA_wave <- function(wave_id, var_to_read, var_wave) {
  # first construct the file name
  file_name <- paste0(path_name, "Combined_", wave_id, "160c.dta")

  # now read the data into a dateframe using the standard stata function
  working_data <- read_stata(file = file_name)

  # take off labels etc
  working_data <- haven::zap_labels(working_data)

  # to prevent lots of warnings remove the following attributes
  for (col in 1:ncol(working_data)) {
    att_list <- c("label", "format.stata", "labels", "names")
    for (att in att_list) {
      attr(working_data[[col]], att) <- NULL
    }
  }

  # add the appropriate wave_id to variable names
  var_to_read_prefix <- var_to_read
  for (i in 1:length(var_to_read))
  {
    if (var_wave[i]) var_to_read_prefix[i] <- paste(wave_id, var_to_read[i], sep = "")
  }

  # subset the data we need only
  # the intersect is to check the name exists in the data frame
  working_data <- working_data[, intersect(var_to_read_prefix, names(working_data))]

  # now strip off the wave IDs
  for (i in 1:length(var_to_read_prefix))
  { # remember the order may have changed so first match it
    j <- match(var_to_read_prefix[i], names(working_data))
    if (var_wave[i]) names(working_data)[j] <- strip_first_char(names(working_data)[j])
  }

  # now add blank columns for the data not found so rbind works fine later
  working_data[, setdiff(var_to_read, names(working_data))] <- NA

  # add the wave id
  working_data$wave <- as.factor(wave_id)

  return(working_data)
}
# ------------------------------------------------------------------------------------
# end of function to read in a single HILDA wave file
# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------
# create the function to read in a all HILDA wave file
# ------------------------------------------------------------------------------------
# Now this function reads in all the HILDA wave files and combines them into one data frame
read_HILDA_waves <- function(wave_ids, ...) { # the variables from this function are mostly passed to the single function
  for (i in wave_ids)
  { # update progress also
    # check whether we have created the dataframe
    if (exists("working_data")) {
      working_data <- rbind(working_data, read_HILDA_wave(wave_id = i, ...))
      cat(paste("..", i, sep = ""))
    }
    else {
      working_data <- read_HILDA_wave(wave_id = i, ...)
      cat(i)
    }
  }
  return(working_data)
}
# ------------------------------------------------------------------------------------
# end of function to read in a all HILDA wave file
# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------
# this is the script to read in the Combined data so that individuals are already
# linked to combineds
# -----------------------------------------------------------------------------
# store the path name
path_name <- "~/Data/HILDA/STATA 160c/"
out_path <- "../output/"

# a list of all the waves
waves <- unlist(lapply(X = seq(1, 16) + 96, FUN = intToUtf8))
# waves <- "p"  # just one wave as a check

# This is the variable list to read in
comb_vars <- c(
  "xwaveid", "hhrhid", "hhrih", "hgage", "hgsex",
  "hstenr", "hstenur", "hhsgcc", "hhtup", "hhfxid", "hhmxid",
  "hhcomps", "capeft", "capept", "capj", "capune", "capnlf",
  "hsvalue", "fmhsib", "fmnsib", "ancob",
  "fmfcob", "fmmcob", "edhigh1", "tchad", "mrcurr", "hhsra",
  "hifdip", "tifdip", "hglth", "helth", "helthwk", "fmpdiv",
  "fmlwop", "fmnprea", "hsrnt", "hsrnti", "hsrntfg",
  "hsmg", "hsmgi", "hsmgfg", "hssl", "hssli", "hsslfg", "hifefp",
  "hsbedrm", "hgyob", "hhiage", "hgjlm", "hgjly",
  "edssenr", "edssl", "edrqenr", "edccenr", "edcqtyp",
  "edrqcmp", "esdtl", "edageln", "edcqen", "edagels", "edcqfpt"
)

# In the same order this denotes the variable names with a wave prefix
# xwaveid is likely the only one without a wave prefix
comb_var_wave <- c(FALSE, rep(TRUE, length(comb_vars) - 1))

combined <- read_HILDA_waves(
  wave_ids = waves, var_to_read = comb_vars,
  var_wave = comb_var_wave
)

# now create a unique cross wave combined ID
combined$xWaveUid <- paste(combined$wave, combined$xwaveid, sep = "")

# need to convert ghage from being labelled to numeric
class(combined$hgage) <- "numeric"

# and the date classes only one for now
combined$hhcomps <- dmy(combined$hhcomps)

# Save the final file as a rds
write_rds(x = combined, path = paste0(out_path, "combined.rds"))
