# This code reads in the master file and then combines it with the
# combined data to give a single file for analysis.
#
# the coding only keeps certain variables add more as needed in the location
# marked below

library(tidyverse)
library(haven)

#-------------------------------------------------------------------------------------
# In this section, we read in the Master file and combine with existing data.
#-------------------------------------------------------------------------------------
# store the path name
path_name <- "~/Data/HILDA/STATA 160c/"
out_path <- "../output/"

master_data <- read_stata(file = paste0(path_name, "Master_p160c.dta"))

# take off labels etc
master_data <- haven::zap_labels(master_data)

# to prevent lots of warnings remove the following attributes
for (col in 1:ncol(master_data)) {
  att_list <- c("label", "format.stata", "labels", "names")
  for (att in att_list) {
    attr(master_data[[col]], att) <- NULL
  }
}

# the Master file has a set of wave dependent variables
# Instead, we want to create a wave ID for these variables and melt the file
# not_melt_vars has the non-wave dependent ones
not_melt_vars <- c(
  "xwaveid", "xhhraid", "xhhstrat", "hhsm", "sex", "yob", "yodeath",
  "isdeath", "aadeath", "enumptn", "ivwptn", "ivwnum", "yrenter",
  "yrleft", "yrivwfst", "yrivwlst", "yrenlst"
)

# make a long data frame from master so we can take the wave id from the variable names
new_master_data <- gather(
  data = master_data, key = "waveKey", value = "value",
  -not_melt_vars
)

# separate out the wave character from the rest of measures
new_master_data <- separate(
  data = new_master_data, col = "waveKey", into = c("wave", "key"),
  sep = 1, remove = TRUE
)

# and put it back into a tidy wide format
new_master_data <- spread(data = new_master_data, key = "key", value = "value")

# remove the na rows as follows:
new_master_data <- new_master_data[new_master_data$fstatus != -2, ]

# --------------------
# This is the place to add more variable names if needed.
# --------------------
varToKeep <- c("xwaveid", "hhsm", "wave", "fstatus", "hhresp")
new_master_data <- new_master_data [, intersect(varToKeep, names(new_master_data))]

# Create a unique cross wave combined ID
new_master_data$xWaveUid <- paste0(new_master_data$wave, new_master_data$xwaveid)

#-------------------------------------------------------------------------------------
# End of the Master file section.
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# this section reads in the combined data file to add on the master file
# variables that are required.
#-------------------------------------------------------------------------------------
combined <- read_rds(paste0(out_path, "combined.rds"))

# the combined file is a subset of the master file as it only inlcudes those
# who responded in each survey. Hence, join from newMaster and
# remove the .y variables once joined
combined_master <- left_join(x = new_master_data, y = combined, by = "xWaveUid")
combined_master$xwaveid.y <- NULL
names(combined_master)[names(combined_master) == "xwaveid.x"] <- "xwaveid"
combined_master$wave.y <- NULL
names(combined_master)[names(combined_master) == "wave.x"] <- "wave"

# add a year variable then a state variable
yearFromWave <- function(x) {
  # first convert x to a character
  x <- as.character(x)
  # and lapply the utf8toInt function to the character vector
  y <- unlist(lapply(x, FUN = utf8ToInt))
  # adjust to get to the year
  y <- y - 96 + 2000
  # make y an integer variable for later efficiency
  y <- as.integer(y)
  # then return the vector version of the resulting list
  return(y)
}
combined_master <- combined_master %>% mutate(Year = yearFromWave(wave))

# also create a state variable
# first create the mapping data frame
map_state <- data_frame(
  hhsgcc = c(
    -7, 11, 19,
    21, 29, 31, 39,
    41, 49, 51, 59,
    61, 71, 81
  ),
  State = c(
    "NSW", "NSW", "NSW",
    "VIC", "VIC", "QLD", "QLD",
    "SA", "SA", "WA", "WA",
    "TAS", "NT", "ACT"
  )
)

# now left join this new data frame onto exposed
combined_master <- left_join(x = combined_master, y = map_state, by = "hhsgcc")

# save the new file for analysis programs
write_rds(x = combined_master, path = paste0(out_path, "combined-master.rds"))
