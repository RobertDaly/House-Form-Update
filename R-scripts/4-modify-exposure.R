# The purpose of this script is to create categories for the covariates
# in HILDA data or adjust the other covariates.

# load up packages
library(tidyverse)
library(openxlsx)

# store the path name
out_path <- "../00-Output/"

# first read in the combined master file for HILDA
exposed <- read_rds(path = paste0(out_path, "exposure-01.rds"))

# filter out the case with -7 as hhsgcc
exposed <- exposed %>% filter(hhsgcc != "-7")

# create first two quadractic terms for age
exposed$hgage2 <- exposed$hgage^2
exposed$hgage3 <- exposed$hgage^3

# create gender categories
exposed$gender <- "Female"
exposed$gender[exposed$hgsex == 1] <- "Male"
exposed$gender <- factor(exposed$gender)

#------------------------------------------------------------------------------
# Now create employment categories based on the capj, capune and capnlf fields.
# a) the did not responds (-10 most of them and also -4)
# b) the not asked the question need to find why later. These same people were
#    not asked about education also (-1)
# c) In a job for more than 90% of the time (capj >90)
# d) Not in the labour force for more than 90% of the time (capnlf > 90)
# e) Unemployed for more than 90% of the time
# f) Not above but mainly working
# g) Not above but mainly in labour force
# h) Not above but mainly unemployed
exposed$empCat <- "Not alloc"
exposed$empCat[exposed$capj == -10 ] <- "Non respondent"
exposed$empCat[exposed$capj == -4 ] <- "Not answered"
exposed$empCat[exposed$capj == -1 ] <- "Not answered"
exposed$empCat[exposed$capj > 90] <- "Working"
exposed$empCat[exposed$capune > 90] <- "Unemployed"
exposed$empCat[exposed$capnlf > 90] <- "Not in labour force"
exposed$empCat[(exposed$empCat == "Not alloc") &
  (exposed$capj > exposed$capune) &
  (exposed$capj > exposed$capnlf) ] <- "Mostly working"
exposed$empCat[(exposed$empCat == "Not alloc") &
  (exposed$capnlf > exposed$capune)] <- "Mostly NILF"
exposed$empCat[(exposed$empCat == "Not alloc")] <- "Mostly unemployed"
exposed$empCat <- as.factor(exposed$empCat)
# relevel to have working as first category
exposed$empCat <- relevel(exposed$empCat, "Working")


#------------------------------------------------------------------------------
# Now create education categories.
#------------------------------------------------------------------------------
# a) the did not responds (-10 most of them and also -4)
# b) the not asked the question need to find why later. These same people were
#    not asked about education also (-1)
# c) No education (part time and full time less than 10%)
# d) Some part time education
# e) Part time education
# e) Some full time education
# f) In full time education
exposed$eduCat <- "Not alloc"
exposed$eduCat[exposed$capeft == -10 ] <- "Non respondent"
exposed$eduCat[exposed$capeft == -4 ] <- "Not answered"
exposed$eduCat[exposed$capeft == -1 ] <- "Not answered"
exposed$eduCat[(exposed$capeft >= 0) & (exposed$capeft < 10) &
  (exposed$capept < 10)] <- "Not in education"
exposed$eduCat[(exposed$capeft >= 0) & (exposed$capeft < 10) &
  (exposed$capept >= 10)] <- "Some PT edu this year"
exposed$eduCat[(exposed$capeft >= 0) & (exposed$capeft < 10) &
  (exposed$capept >= 90)] <- "In part time edu"
exposed$eduCat[exposed$capeft >= 10 ] <- "Some FT edu this year"
exposed$eduCat[exposed$capeft >= 90 ] <- "In full time edu"
exposed$eduCat <- as.factor(exposed$eduCat)

#------------------------------------------------------------------------------
# Add combined education and employment categories using a fresh approach
#------------------------------------------------------------------------------

## ----Wave a education----------------------------------------------------

# set up new category variable default to NA
exposed$eduCat2 <- NA

# use the following to set the non response category
exposed$eduCat2[(exposed$esdtl == -10)] <- "No response"

# code up the cohort definition
cohort <- (exposed$esdtl != -10) & (exposed$wave == "a")

# so you are in school if its the first wave and you said that you are
exposed$eduCat2[cohort & (exposed$edagels == 2)] <- "In school"
exposed$eduCat2[cohort & (exposed$edcqfpt == 2)] <- "College full-time"
exposed$eduCat2[cohort & (exposed$edcqfpt == 3)] <- "College part-time"
exposed$eduCat2[cohort & (exposed$edcqfpt == 1)] <- "Not in education"
exposed$eduCat2[cohort & (exposed$edagels != 2) &
  (exposed$edcqfpt == -1)] <- "Not in education"


## ----First asked education-----------------------------------------------

# code up the first asked exception
cohort <- (exposed$esdtl != -10) & (exposed$wave != "a") &
  (exposed$edssenr == -1)

# so you are in school if its the first wave and you said that you are
exposed$eduCat2[cohort & (exposed$edageln == 2)] <- "In school"
exposed$eduCat2[cohort & (exposed$edcqen == 1) &
  (exposed$edcqtyp == 1)] <- "College full-time"
exposed$eduCat2[cohort & (exposed$edcqen == 1) &
  (exposed$edcqtyp == 2)] <- "College part-time"
exposed$eduCat2[cohort & (exposed$edcqen == 2)] <- "Not in education"
exposed$eduCat2[cohort & (exposed$edageln != 2) &
  (exposed$edcqen == -1)] <- "Not in education"

## ----Ongoing Education---------------------------------------------------

cohort <- (exposed$esdtl != -10) & (exposed$wave != "a") &
  (exposed$edssenr != -1)

# default is not in education
exposed$eduCat2[cohort] <- "Not in education"

# or its not your first response and you said so
exposed$eduCat2[cohort & (exposed$edrqenr == 1) & (exposed$edccenr == 1) &
  (exposed$edcqtyp == 1)] <- "College full-time"
exposed$eduCat2[cohort & (exposed$edrqenr == 1) & (exposed$edccenr == 1) &
  (exposed$edcqtyp == 2)] <- "College part-time"

exposed$eduCat2[cohort & (exposed$edssenr == 1) &
  (exposed$edssl == 2)] <- "In school"

## ----Employment and education--------------------------------------------

# now set up education and employment combos
exposed$eduEmpCat <- NA

# first at school and working then rest
exposed$eduEmpCat[exposed$esdtl == -10] <- "NoResp"
exposed$eduEmpCat[(exposed$eduCat2 == "In school") &
  (exposed$esdtl > 0)] <- "School"
exposed$eduEmpCat[(exposed$eduCat2 == "College full-time") &
  ((exposed$esdtl == 1) | (exposed$esdtl == 2))] <- "CollegeFTWork"
exposed$eduEmpCat[(exposed$eduCat2 == "College full-time") &
  (exposed$esdtl > 2)] <- "CollegeFTNoWork"
exposed$eduEmpCat[(exposed$eduCat2 == "College part-time") &
  (exposed$esdtl != -10)] <- "CollegePT"
exposed$eduEmpCat[(exposed$eduCat2 == "Not in education") &
  (exposed$esdtl == 1)] <- "WorkFT"
exposed$eduEmpCat[(exposed$eduCat2 == "Not in education") &
  (exposed$esdtl == 2)] <- "WorkPT"
exposed$eduEmpCat[(exposed$eduCat2 == "Not in education") &
  ((exposed$esdtl == 3) | (exposed$esdtl == 4))] <- "UNE"
exposed$eduEmpCat[(exposed$eduCat2 == "Not in education") &
  (exposed$esdtl > 4)] <- "NILF"

# make it a factor and relevel
exposed$eduEmpCat <- relevel(as.factor(exposed$eduEmpCat), "School")

# add further empl factor
exposed$esdtlFac <- factor(
  x = (exposed$esdtl),
  levels = (c(seq(1, 7, 1), -10)),
  labels = c(
    "Emp FT", "Emp PT", "Une", "Une",
    "NILF att", "NILF", "NoResp", "NoResp"
  )
)


#------------------------------------------------------------------------------
# Now create sibling categories.
# a) first the no response or not asked category
# b) then the only child category
# c) then 1, 2, 3 siblings
# then 4-5 and over five
exposed$sibCat <- "Not alloc"
exposed$sibCat[exposed$fmhsib == -10 ] <- "Non respondent"
exposed$sibCat[exposed$fmhsib == -4 ] <- "Not answered"
exposed$sibCat[exposed$fmhsib == -1 ] <- "Not answered"
exposed$sibCat[exposed$fmhsib == 2] <- "No siblings"
exposed$sibCat[exposed$fmhsib == 1 & (exposed$fmnsib < 0)] <- "Not answered"
exposed$sibCat[exposed$fmhsib == 1 & (exposed$fmnsib == 1)] <- "1 sibling"
exposed$sibCat[exposed$fmhsib == 1 & (exposed$fmnsib == 2)] <- "2 siblings"
exposed$sibCat[exposed$fmhsib == 1 & (exposed$fmnsib == 3)] <- "3 siblings"
exposed$sibCat[exposed$fmhsib == 1 & (exposed$fmnsib >= 4) &
  (exposed$fmnsib <= 5)] <- "4-5 siblings"
exposed$sibCat[exposed$fmhsib == 1 & (exposed$fmnsib > 5)] <- "6+ siblings"
exposed$sibCat <- as.factor(exposed$sibCat)

exposed$sibCat2 <- exposed$sibCat
levels(exposed$sibCat2) <- c(
  "1 sibling", rep("2+ siblings", 4),
  "No siblings", rep("No data", 3)
)


#------------------------------------------------------------------------------
# Now parent categories
# fmlwop tells you whether living with parents at age 14
exposed$livParCat <- "Not alloc"
exposed$livParCat[exposed$fmlwop == -10] <- "Non respondent"
exposed$livParCat[exposed$fmlwop == -4 | exposed$fmlwop == -3] <- "Not answered"
exposed$livParCat[(exposed$fmlwop == 1)] <- "Both parents"
# check whether parents later divorced
exposed$livParCat[(exposed$fmlwop == 1) & (exposed$fmpdiv == 1)] <-
  "Divorced later"
exposed$livParCat[exposed$fmlwop == 2 | exposed$fmlwop == 3
| exposed$fmlwop == 8] <- "Step-parent or other"
exposed$livParCat[exposed$fmlwop == 4] <- "Father only"
exposed$livParCat[exposed$fmlwop == 5] <- "Mother only"
exposed$livParCat <- as.factor(exposed$livParCat)

#------------------------------------------------------------------------------
# Now parent ownership categories
# hstenr tells you whether parents own their house or not in waves b on
# in wave a it is a different variable being hstenur

exposed$parOwnCat <- if_else(is.na(exposed$hstenr), exposed$hstenur,
  exposed$hstenr
)

# note 3 has a different meaning in wave a but is combined in any event

exposed$parOwnCat <- factor(
  x = exposed$parOwnCat, exclude = NULL,
  levels = c(1, 2, 3, 4, -4, -3),
  labels = c("Own", "Rent", rep("Own", 4))
)

#------------------------------------------------------------------------------
# Whether the person has children or not
exposed$hasKidsCat <- "Other"
exposed$hasKidsCat[exposed$tchad == 0] <- "No Children"
exposed$hasKidsCat[exposed$tchad > 0] <- "Has Children"
exposed$hasKidsCat[exposed$tchad == -10] <- "Non respondent"
exposed$hasKidsCat[exposed$tchad == -4] <- "No Children"
# above only has three entries so assume no children for simplicity
exposed$hasKidsCat <- as.factor(exposed$hasKidsCat)

#------------------------------------------------------------------------------
# marriage categories
#------------------------------------------------------------------------------
exposed$marriedCat <- "Other"
exposed$marriedCat[exposed$mrcurr == 1 | exposed$mrcurr == 2] <- "Married"
exposed$marriedCat[exposed$mrcurr == 3 | exposed$mrcurr == 4 |
  exposed$mrcurr == 5 ] <- "Was Married"
exposed$marriedCat[exposed$mrcurr == 6] <- "Unmarried"
exposed$marriedCat[exposed$mrcurr == -4] <- "Unmarried"
# now marked as married also if in a married category after exit
exposed$marriedCat[exposed$mrcurrNxt == 1 |
  exposed$mrcurrNxt == 2] <- "Married"
# preserve the non respondent status (although some of these may now be
# responding)
exposed$marriedCat[exposed$mrcurr == -10] <- "Non respondent"

exposed$marriedCat <- as.factor(exposed$marriedCat)

# further married categories
exposed$marriedCat2 <- factor(
  x = exposed$mrcurrNxt, exclude = NULL,
  levels = c(6, seq(1, 4, 1), -10, NA),
  labels = c(
    "NoPartner", rep("Partnered", 4),
    "Unknown", "Unknown"
  )
)


#------------------------------------------------------------------------------
# health categories
#------------------------------------------------------------------------------
exposed$healthCat <- "Other"
exposed$healthCat[exposed$helth == -10] <- "Non respondent"
exposed$healthCat[exposed$helth == 2 | exposed$helth == -4 |
  exposed$helth == -3] <- "Healthy"
# all the other have 1 as the entry so now allocated based on working health
exposed$healthCat[exposed$helthwk == 1 | exposed$helthwk == -3
| exposed$helthwk == 3] <- "Health limits work"
exposed$healthCat[exposed$helthwk == 2] <-
  "Health conditions do not limit work"

exposed$healthCat <- as.factor(exposed$healthCat)

#------------------------------------------------------------------------------
# parent country of origin categories
#------------------------------------------------------------------------------
# the coding uses the ABS's SACC coding system. This is four digits with the
# first digit indicating the following regions
# 1 Oceania and Antarctica
# 2 North-West Europe
# 3 Southern and Eastern Europe
# 4 North Africa and the Middle East
# 5 South-East Asia
# 6 North-East Asia
# 7 Southern and Central Asia
# 8 Americas
# 9 Sub-Saharan Africa

# code for the mother cob category and father's first
# use a single funciton to do each

setParCobCat <- function(cob) {
  # create a category vector of the right length by copying and then setting
  # to a default
  cat <- cob
  dig1 <- substr(cob, 1, 1)
  cat[cob == 1101] <- "Australia"
  cat[cob != 1101 & dig1 == 1] <- "Other Oceanic"
  cat[dig1 == 2] <- "North-West Europe"
  cat[dig1 == 3] <- "Southern and Eastern Europe"
  cat[dig1 == 4] <- "Other"
  cat[dig1 == 5] <- "Asia"
  cat[dig1 == 6] <- "Asia"
  cat[dig1 == 7] <- "Asia"
  cat[dig1 == 8] <- "Other"
  cat[dig1 == 9] <- "Other"
  cat[cob == -10] <- "Non respondent"
  cat[cob == -3 | cob == -4 | cob == -7 ] <- "Other"
  cat <- as.factor(cat)
}

# then set own and each parent's country of origin using this function
exposed$cobCat <- setParCobCat(exposed$ancob)
exposed$mumCobCat <- setParCobCat(exposed$fmmcob)
exposed$dadCobCat <- setParCobCat(exposed$fmfcob)

#---------- Now some income measures
exposed$famInc <- exposed$hifdip - exposed$tifdip
exposed$famInc[exposed$famInc <= 0] <- NA
exposed$famIncSq <- exposed$famInc^2

exposed$tifdip[exposed$tifdip == 0] <- NA

# ----------- save results
write_rds(exposed, path = paste0(out_path, "exposure-02.rds"))
