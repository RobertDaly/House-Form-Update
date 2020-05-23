# rountine to add survivor data (entry, exit, event) onto combMast

# load standard packages
library(lubridate)
library(scales)
library(survival)
library(tidyverse)

# store the path name
out_path <- "../output/"

#----This code prepares the data file -----------------------------------------
# load the data file
combMast <- read_rds(path = paste0(out_path, "combined-master.rds"))

# set up lag variables and remove the last wave.
# If data is needed from the last wave pull it to the previous wave here
combMast <- combMast %>%
  arrange(xwaveid, wave) %>%
  group_by(xwaveid) %>%
  mutate(hhrihNxt = lead(hhrih)) %>%
  mutate(fstatusNxt = lead(fstatus)) %>%
  mutate(hhrespNxt = lead(hhresp)) %>%
  mutate(hhrhidNxt = lead(hhrhid)) %>%
  mutate(hstenrNxt = lead(hstenr)) %>%
  mutate(mrcurrNxt = lead(mrcurr)) %>%
  mutate(hhsmNxt = lead(hhsm))
# this works by first ordering by person and wave. By grouping by wave the
# the lead function then just brings in for each person the next wave's value
combMast <- ungroup(combMast)

#----Collect parents household ids both now and later -------------------------
# first create a look up file
household <- combMast %>% select(wave, xwaveid, hhrhid, hhrhidNxt)

# now add mother's and father's household both now and later
combMast <- combMast %>% left_join(household,
  by = c("hhmxid" = "xwaveid", "wave" = "wave"),
  suffix = c("", ".m")
)
combMast <- combMast %>% left_join(household,
  by = c("hhfxid" = "xwaveid", "wave" = "wave"),
  suffix = c("", ".f")
)

# the file can be removed.
rm(household)

# some times parents id change. Hence, we need to pull in next household ids for parents also
combMast <- combMast %>%
  arrange(xwaveid, wave) %>%
  group_by(xwaveid) %>%
  mutate(hhrhid.mNxt = lead(hhrhid.m)) %>%
  mutate(hhrhid.fNxt = lead(hhrhid.f))
combMast <- ungroup(combMast)

# remove wave p at this point (if a variable is needed from wave p then
# pull it in as a lead variable)
combMast <- combMast[combMast$wave != "p", ]

#----Collect parents household ids both now and later -------------------------
# These are the variables used by the individual loop
inpVar <- c(
  "hgage", "hhrhid", "hhrhid.m", "hhrhid.f", "hhrhidNxt",
  "hhrhidNxt.m", "hhrhidNxt.f", "hhrhid.mNxt", "hhrhid.fNxt",
  "hhrih", "hgyob", "Year", "xwaveid", "wave"
)

# set up a output dataframe to join back at end
survDF <- data_frame(
  hgage = as.numeric(), hhrhid = as.character(),
  hhrhid.m = as.character(), hhrhid.f = as.character(),
  hhrhidNxt = as.character(),
  hhrhid.mNxt = as.character(), hhrhid.fNxt = as.character(),
  hhrhidNxt.m = as.character(), hhrhidNxt.f = as.character(),
  hhrih = as.character(), hgyob = as.numeric(),
  Year = as.numeric(),
  xwaveid = as.character(), wave = as.character(),
  entry = as.numeric(), exit = as.numeric(),
  event = as.numeric()
)

# Obtain a list of all individuals to loop through.
indVector <- (combMast %>% distinct(xwaveid))[[1]]

# select random entries of indVector to loop through
# for testing purposes
# indVector <- sample(indVector, size = 100)
# indVector <- indVector[1:1000]

clock <- proc.time()
# now loop through for now just the first 10 cases
for (ind in indVector) {
  # subset the data frame just for this individual ordered by year
  indDF <- combMast %>%
    filter(xwaveid == ind) %>%
    select(inpVar) %>%
    arrange(Year)

  # set up the three survivor variables being entry age, exit age and
  # whether an event occurs. Default these to NA which implies no entries
  indDF$entry <- NA
  indDF$exit <- NA
  indDF$event <- NA

  # now loop through waves until we have an entry point
  entered <- FALSE
  for (i in seq(1, nrow(indDF), 1)) {
    # first test for entry
    if (!entered) {
      # these are the first entry criteria
      entered <- ((indDF$hhrih[i] == 9) | (indDF$hhrih[i] == 10)) &
        (indDF$hgage[i] <= 25)
      # treat a NA outcome as false
      if (is.na(entered)) {
        entered <- FALSE
      }
      # if these are not met just go to next record
      if (!entered) next
    }

    # now test for exits and right censoring
    # first are they not enumerated in next period
    if (is.na(indDF$hhrhidNxt[i])) {
      # if missing in next period may still be an exit
      # first check is if you were living with mum and your mum is enumerated
      # then probably an exit
      if (!is.na(indDF$hhrhidNxt.m[i]) &
        (indDF$hhrhid[i] == indDF$hhrhid.m[i])) {
        exitStatus <- TRUE
      } else {
        # okay not mum - try dad
        if (!is.na(indDF$hhrhidNxt.f[i]) &
          (indDF$hhrhid[i] == indDF$hhrhid.f[i])) {
          exitStatus <- TRUE
        } else {
          # so neither mum nor dad are tracked. Hence right curtate
          # as status is not knowable
          # in this case do nothing (as nothing set up) and exit loop
          break
        }
      }
    } else {
      # so they are counted in the next period so then are their parents
      # and if so are they living with them
      # note this logic uses .fNxt while above uses Nxt.f. The above
      # works where the next entry is NA. The below is safer when
      # there is a following id for the individual
      if (is.na(indDF$hhrhid.fNxt[i]) & is.na(indDF$hhrhid.mNxt[i])) {
        # both parents are na - you are not this is an exit
        exitStatus <- TRUE
      } else {
        if ((indDF$hhrhidNxt[i] == indDF$hhrhid.fNxt[i]) |
          (indDF$hhrhidNxt[i] == indDF$hhrhid.mNxt[i])) {
          # you are living with your parents next period so no exit
          exitStatus <- FALSE
        } else {
          # you are living with neither parent so its an exit
          exitStatus <- TRUE
        }
      }
    }
    # not act on the exitStatus
    if (exitStatus) {
      # its an exit
      indDF$entry[i] <- (indDF$Year[i] - indDF$hgyob[i] + 1 + indDF$hgage[i]) / 2
      indDF$exit[i] <- indDF$entry[i] + 0.5
      indDF$event[i] <- 1
      # if it is an exit stop the loop
      break
    } else {
      # its an entry no exit
      indDF$entry[i] <- (indDF$Year[i] - indDF$hgyob[i] + 1 + indDF$hgage[i]) / 2
      indDF$exit[i] <- indDF$entry[i] + 1
      indDF$event[i] <- 0
    }
  } # this is the end of the loop

  # add the variables onto the storage DB
  survDF <- bind_rows(survDF, indDF %>% filter(!is.na(indDF$entry))
    %>% select(xwaveid, wave, entry, exit, event))
}
cat(proc.time() - clock)

# rejoin the data back adding new fields
combMast <- combMast %>% left_join(survDF %>%
  select(xwaveid, wave, entry, exit, event),
by = c("xwaveid", "wave")
)

#-----This section adds some useful response fields----------------------------

respType <- function(status) {
  return(status == 1 | status == 2)
}
combMast$resp <- respType(combMast$fstatus)
combMast$respNxt <- respType(combMast$fstatusNxt)

# the following breaks the response type down further by type of
# respondant - that is a permanent or temporary survey member
combMast$indType <- "Responding PSM" # default all to this category
combMast$indType[combMast$hhsm == 3] <- "Responding TSM"
# hhsm tells you if someone is a TSM with indicator 3
combMast$indType[!combMast$resp] <- "No Ind response"

combMast$indTypeNxt <- "Responding PSM" # default all to this category
combMast$indTypeNxt[combMast$hhsmNxt == 3] <- "Responding TSM"
# hhsm tells you if someone is a TSM with indicator 3
combMast$indTypeNxt[!combMast$respNxt] <- "No Ind response"

# and write the file for next use
write_rds(combMast, path = paste0(out_path, "comb-mast-surv.rds"))

#----Now Create the exposure file ---------------------------------------------

# first simple filter out the NA entry types
exposed <- combMast %>% filter(!is.na(entry))

# filter out the case with -7 as hhsgcc
exposed <- exposed %>% filter(hhsgcc != "-7")

# create an exposure indicated for documentation purposes
exposed$exposure <- 1

write_rds(exposed, path = paste0(out_path, "exposure-01.rds"))
