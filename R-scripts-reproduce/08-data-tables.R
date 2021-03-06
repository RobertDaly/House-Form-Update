# this file does all the data summaries that appear in the thesis

# load libraries, data and other preliminaries ----------------------------
library(tidyverse)
library(survival)
library(lubridate)
library(scales)
library(stargazer)
library(xtable)

out_path <- "../output/"

# read in the exposure data and HILDA data
exposed <- read_rds(paste0(out_path, "exposure-05.rds"))

combMast <- read_rds(paste0(out_path, "combined-master.rds"))

plotFit <- function(coxFit, surv = TRUE) {
  survf <- survfit(coxFit)
  plotdata <- tibble(
    age = survf[["time"]],
    surv = survf[["surv"]],
    low = survf[["lower"]],
    upp = survf[["upper"]]
  )
  plotdata <- plotdata %>% mutate(
    rate = 1 - surv / c(1, head(surv, -1))
  )
  if (surv) {
    fit.plot <- ggplot(
      data = plotdata,
      mapping = aes(x = age, y = surv)
    ) +
      #                  geom_ribbon(aes(ymin = low, ymax = upp))
      geom_point() + ylab("") + xlab("Age") + theme_minimal()
  } else {
    fit.plot <- ggplot(
      data = plotdata,
      mapping = aes(x = age, y = rate)
    ) +
      #                  geom_ribbon(aes(ymin = low, ymax = upp))
      geom_point() + ylab("") + xlab("Age") + theme_minimal() +
      scale_y_continuous(labels = scales::percent)
  }
  return(fit.plot)
}

XTSUM <- function(data, varName, placeVar) {
  # note enquo and !! are elements of rlang that enable flexible use of
  # variable names in functions
  varName <- enquo(varName)
  placeVar <- enquo(placeVar)
  ores <- data %>% summarise(
    ovr.mean = mean(!!varName, na.rm = TRUE),
    ovr.sd = sd(!!varName, na.rm = TRUE),
    ovr.min = min(!!varName, na.rm = TRUE),
    ovr.max = max(!!varName, na.rm = TRUE),
    ovr.N = sum(as.numeric((!is.na(!!varName))))
  )

  bmeans <- data %>%
    group_by(!!placeVar) %>%
    summarise(
      meanx = mean(!!varName, na.rm = T),
      t.count = sum(as.numeric(!is.na(!!varName)))
    )
  bres <- bmeans %>%
    ungroup() %>%
    summarise(
      between.sd = sd(meanx, na.rm = TRUE),
      between.min = min(meanx, na.rm = TRUE),
      between.max = max(meanx, na.rm = TRUE),
      Units = sum(as.numeric(!is.na(t.count))),
      t.bar = mean(t.count, na.rm = TRUE)
    )

  wdat <- data %>%
    group_by(!!placeVar) %>%
    mutate(W.x = scale(!!varName, scale = FALSE))
  wres <- wdat %>%
    ungroup() %>%
    summarise(
      within.sd = sd(W.x, na.rm = TRUE),
      within.min = min(W.x, na.rm = TRUE),
      within.max = max(W.x, na.rm = TRUE)
    )
  return(list(ores = ores, bres = bres, wres = wres))
}

# do a wrapper function that returns a vector of total SD, between SD and within SD.
calcVariation <- function(label, ...) {
  temp <- XTSUM(...)
  return(tibble(
    desc = label, mean = temp$ores$ovr.mean,
    total = temp$ores$ovr.sd,
    between = temp$bres$between.sd,
    within = temp$wres$within.sd
  ))
}

# this function returns the average age for 1 factor times base model
avgAgeFac <- function(multFac, coxFit) {
  survD <- survfit(coxFit)

  # derive the harard rates from the survivor curve
  hazRate <- 1 - survD$surv / c(1, head(survD$surv, -1))
  adjHazRate <- multFac * hazRate
  adjSurv <- rep(1, length(adjHazRate))

  # loop to recreate
  # first is different so do it outside of loop
  adjSurv[1] <- 1 - adjHazRate[1]
  for (i in seq(2, length(adjHazRate), 1)) {
    adjSurv[i] <- adjSurv[i - 1] * (1 - adjHazRate[i])
  }

  exits <- c(1, head(adjSurv, -1)) - adjSurv
  avg <- sum(exits * survD$time)
  # now adjust for those that have not exited at oldest age. Use oldest recorded age
  avg <- avg + (1 - sum(exits)) * tail(survD$time, 1)

  return(avg)
}

# neat percentage formating
percent_1 <- label_percent(accuracy = 0.1)

# create function to sanitize column names
neat_col_names <- function(col_names) {
  # first replace underscores with spaces
  adj_names <- gsub("_", " ", col_names, fixed = TRUE)
  # then capitalise each word
  adj_names <- tools::toTitleCase(adj_names)
  # do the standard santization in any event
  adj_names = xtable::sanitize(adj_names)
  return(adj_names)
}

# create function to sanitize column names
neat_col_names_1 <- function(col_names) {
    # this strips off the last char from each column name
    adj_names <- str_sub(col_names, 1, -2)
    # apply above function also for completeness
    return(neat_col_names(adj_names))
}

# create function to wrap and center column names
center_col_names <- function(col_names) {
  # pull out the width - it should be at the start surrounded by 
  # curly braces
  locations <- regexpr("}", col_names)
  # throw an error if any are -1
  if (any(locations == -1)) {
    stop("Column heading must have width at start as {width}")
  }
  # column heading then strips the width part
  adj_names <- substring(col_names, locations + 1)
  # now figure our the widths
  widths <- substring(col_names, 1, locations)
  # throw an error if any do not start with {
  if (any(substring(widths,1,1) != "{")) { 
    stop("Column heading must have width at start as {width}")
  }
  # now drop the brackets
  widths <- substring(widths, 2, locations -1)
  # next replace underscores with spaces
  adj_names <- gsub("_", " ", adj_names, fixed = TRUE)
  # then capitalise each word
  adj_names <- tools::toTitleCase(adj_names)
  
  adj_names = paste0("\\parbox[t]{",widths,"\\textwidth}{\\centering ", adj_names, "}")
  return(adj_names)
}
# this function cleans row names but does not use the width
# it is a companion of above function for matrices
no_width_row_names <- function(col_names) {
  # pull out the width - it should be at the start surrounded by 
  # curly braces
  locations <- regexpr("}", col_names)
  # column heading then strips the width part
  adj_names <- substring(col_names, locations + 1)
  # next replace underscores with spaces
  adj_names <- gsub("_", " ", adj_names, fixed = TRUE)
  # then capitalise each word
  adj_names <- tools::toTitleCase(adj_names)
  return(adj_names)
}

# convert waves into years
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

# Summary HILDA table -----------------------------------------------------

# number of households in latest wave
nrow(combMast %>% filter(wave == "p") %>% select(hhrhid) %>% distinct())

# add household count
combWave <- do(
  group_by(combMast, wave, Year),
  tibble(
    Households = length(unique(.$hhrhid)),
    Individuals = nrow(.)
  )
)

riskSet <- do(
  group_by(exposed, wave),
  tibble(
    Risk_Set = nrow(.), exits = sum(.$event == 1),
    Exit_Rate = exits / Risk_Set
  )
)

HILDAsum <- left_join(combWave, riskSet, by = "wave")

HILDAsum$Households <- comma(HILDAsum$Households)
HILDAsum$Individuals <- comma(HILDAsum$Individuals)
HILDAsum$Risk_Set <- comma(HILDAsum$Risk_Set)
HILDAsum$exits <- comma(HILDAsum$exits)
HILDAsum$Exit_Rate <- percent_1(HILDAsum$Exit_Rate)

HILDAsum <- xtable(HILDAsum,
  caption = "Summary of HILDA Data and Exit Rate",
  label = "HILDAsum", align = "lccrrrrr"
)

print(HILDAsum,
  type = "latex", file = paste0(out_path, "HILDAsum.tex"),
  include.rownames = FALSE, caption.placement = "top",
  sanitize.colnames.function = neat_col_names, 
  booktabs = TRUE, table.placement = "htpb"
)

# Avg Hazard Rates and Survival -------------------------------------------

# fit  a curve with no co-variates
modRaw <- coxph(Surv(entry, exit, event) ~ 1, data = exposed)

SurvRawPlot <- plotFit(modRaw, TRUE)

HazRawPlot <- plotFit(modRaw, FALSE)

plot(SurvRawPlot)
plot(HazRawPlot)

# now save the plots to be used by latex for thesis
ggsave("RawSurv.pdf",
  plot = SurvRawPlot, device = "pdf", width = 14, height = 9,
  units = "cm", path = out_path
)
ggsave("RawHazard.pdf",
  plot = HazRawPlot, device = "pdf", width = 14, height = 9,
  units = "cm", path = out_path
)


# Average age from data ---------------------------------------------------

survD <- survfit(modRaw)
avgAges <- list(
  data = mean(exposed$exit[exposed$event == 1]),
  unadj = sum((c(1, head(survD$surv, -1)) - survD$surv) * survD$time),
  model = avgAgeFac(1, modRaw)
)

# fit a curve with just wave covariates
modWave <- coxph(Surv(entry, exit, event) ~ wave, data = exposed)

# now calculate fitted average ages for each wave
WaveFac <- exp(c(wavea = 0, modWave$coefficients))
WaveAvgAge <- unlist(lapply(WaveFac, avgAgeFac, modWave))

# now create a table with the different avgAges
avgAgeWave <- tibble(wave = substr(names(WaveFac), 5, 5), modAvg = WaveAvgAge)

# convert to three columns
age_3_cols <- tibble(Year1 = yearFromWave(avgAgeWave$wave[1:5]), 
                     Age1 = comma(avgAgeWave$modAvg[1:5], accuracy = 0.1),
                     Year2 = yearFromWave(avgAgeWave$wave[6:10]), 
                     Age2 = comma(avgAgeWave$modAvg[6:10], accuracy = 0.1),
                     Year3 = yearFromWave(avgAgeWave$wave[11:15]), 
                     Age3 = comma(avgAgeWave$modAvg[11:15], accuracy = 0.1))

# now prepare for printing
age_3_cols <- xtable(age_3_cols,
                   caption = "Modelled Average Age for Leaving Home",
                   label = "age-3-cols", align = c("l", rep("p{1.3cm}", 6))
)

print(age_3_cols,
      type = "latex", file = paste0(out_path, "age-3-cols.tex"),
      include.rownames = FALSE, caption.placement = "top",
      sanitize.colnames.function = neat_col_names_1, 
      booktabs = TRUE, table.placement = "htpb"
)


# Exit Rate by wave ----------------
sumWave <- exposed %>%
  group_by(wave) %>%
  summarise(exp = sum(exposure), exits = sum(event), exitRate = mean(event), avgAge = sum(event * (entry + exit) / 2) / exits)
wavePlot <- ggplot(data = sumWave) + geom_point(mapping = aes(x = wave, y = exitRate))
# add axises and legends
wavePlot <- wavePlot + scale_x_discrete(
  name = "",
  breaks = c("a", "d", "g", "j", "m", "o"),
  labels = c(2001, 2004, 2007, 2010, 2013, 2015)
) +
  scale_y_continuous(
    name = "", labels = scales::percent,
    limits = c(0.1, 0.2)
  ) + theme_minimal()
plot(wavePlot)
ggsave("WaveRate.pdf",
  plot = wavePlot, device = "pdf", width = 14, height = 9,
  units = "cm", path = out_path
)


# tenure choice --------------------------
# parents' own
tenure <- list(
  parentOwn = sum(exposed$parOwnCat == "Own") / nrow(exposed),
  leaveOwn = nrow(exposed %>% filter(event == 1 & hstenrNxt != 2)) /
    nrow(exposed %>% filter(event == 1))
)


# Housing Variables -------------------------------------------------------

# list of relevant variables
houseVars <- c(
  "RP_HousePrice_median_real", "HILDA_HousePrice_median_real",
  "HILDA_Rent_median_real", "HILDA_Mortg_median_real"
)
housingData <- exposed %>%
  select(houseVars, Year, hhsgcc) %>%
  distinct()

houseVar <- bind_rows(
  calcVariation("RP House Prices", housingData, RP_HousePrice_median_real, hhsgcc),
  calcVariation("HILDA House Prices", housingData, HILDA_HousePrice_median_real, hhsgcc),
  calcVariation("HILDA Rents", housingData, HILDA_Rent_median_real, hhsgcc),
  calcVariation("HILDA Mortgages", housingData, HILDA_Mortg_median_real, hhsgcc)
)

# clean up the format before sending to print
houseVar$mean <- comma(houseVar$mean)
houseVar$total <- comma(houseVar$total)
houseVar$between <- comma(houseVar$between)
houseVar$within <- comma(houseVar$within)

# update names for printing purposes
names(houseVar) <- c("{0.25}Housing Measure", "{0.12}Mean of medians",
                     "{0.12}Variation in medians",
                     "{0.16}Across regions (between)", 
                     "{0.12}Over time (within)")


houseVar <- xtable(houseVar,
  caption = "Variation in Housing Cost Measures",
  label = "houseVar", align = c("l", "l", rep("r",4)), digits = 0
)

print(houseVar,
  type = "latex", file = paste0(out_path, "houseVar.tex"),
  include.rownames = FALSE, caption.placement = "top",
  sanitize.colnames.function = center_col_names, 
  booktabs = TRUE, table.placement = "htpb"
)

houseLabs <- c("{0.14}RP_House_Prices", "{0.14}HILDA_Prices", "{0.14}HILDA_Rents",
               "{0.14}HILDA_Mortgages")
houseCor <- cor(housingData %>% select(houseVars), method = "pearson")
# add labels
attr(houseCor, "dimnames") <- list(houseLabs, houseLabs)

# print(corMat, digits = 3)

houseCor <- xtable(houseCor,
  caption = "Correlation of Housing Cost Measures",
  label = "houseCor", align = "lrrrr", digits = 3
)

print(houseCor,
  type = "latex", file = paste0(out_path, "houseCor.tex"),
  include.rownames = TRUE, caption.placement = "top", 
  sanitize.colnames.function = center_col_names, 
  sanitize.rownames.function = no_width_row_names,
  booktabs = TRUE, table.placement = "htpb"
)


# Summary Gender, siblings, parent rent -----------------------------------
# list of relevant variables
genderTab <- exposed %>%
  group_by(gender) %>%
  summarise(Prop = n() / nrow(exposed))
sibTab <- exposed %>%
  group_by(sibCat2) %>%
  summarise(Prop = n() / nrow(exposed))
parOwnTab <- exposed %>%
  group_by(parOwnCat) %>%
  summarise(Prop = n() / nrow(exposed))

# now combined into one summary table
HILDATab <- bind_rows(
  tibble(
    Description = c(
      "Gender",
      rep("", nrow(genderTab))
    ),
    Category = c(as.character(genderTab$gender), "Total"),
    Percentage = c(unlist(genderTab[, 2]), sum(genderTab[, 2]))
  ),
  tibble(Description="", Category = "", Percentage = NA),
  tibble(
    Description = c(
      "Sibling Count",
      rep("", nrow(sibTab))
    ),
    Category = c(as.character(sibTab$sibCat2), "Total"),
    Percentage = c(unlist(sibTab[, 2]), sum(sibTab[, 2]))
  ),
  tibble(Description="", Category = "", Percentage = NA),
  tibble(
    Description = c(
      "Whether Parents Own",
      rep(" ", nrow(parOwnTab))
    ),
    Category = c(as.character(parOwnTab$parOwnCat), "Total"),
    Percentage = c(unlist(parOwnTab[, 2]), sum(parOwnTab[, 2]))
  )
)
HILDATab$Percentage <- percent_1(HILDATab$Percentage)

# update names for printing
names(HILDATab) = c("{0.3}Description", "{0.13}Category", 
                    "{0.15}Percentage_in_risk_set")

HILDATab <- xtable(HILDATab,
  caption = "Summary of Household and Individual Data",
  label = "HILDATab", align = "lllr"
)

print(HILDATab,
  type = "latex", file = paste0(out_path, "HILDATab.tex"),
  include.rownames = FALSE, caption.placement = "top",
  sanitize.colnames.function = center_col_names, 
  booktabs = TRUE, table.placement = "htpb"
)

# Employment and education category ---------

empEduTab <- exposed %>%
  group_by(eduEmpCat) %>%
  summarise(Percentage = n() / nrow(exposed))

# sort by percentage
empEduTab <- empEduTab[order(empEduTab$Percentage,decreasing = TRUE),]

# add a total
empEduTab <- bind_rows(
  empEduTab[order(empEduTab$Percentage,
    decreasing = TRUE
  ), ],
  tibble(
    eduEmpCat = "Total",
    Percentage = sum(empEduTab$Percentage)
  )
)

empEduTab$Percentage <- percent_1(empEduTab$Percentage)

# create some nice descriptions
emp_edu_desc = tibble(
  eduEmpCat = c("School", "CollegeFTNoWork", "CollegeFTWork",
                "CollegePT", "NILF", "NoResp", "UNE", "WorkFT", "WorkPT" ),
  Category = c("School", "College Not Working", "College and Working",
               "College Part Time", "Neither in Education nor Labour Force",
               "No Response", "Unemployed", "Working Full Time",
               "Working Part Time")
)

# map in the names
empEduTab <- left_join(empEduTab, emp_edu_desc, by = "eduEmpCat")
# and strip the first one
empEduTab <- empEduTab[,c(3,2)]

# update names for printing
names(empEduTab) = c("{0.3}Category", 
                    "{0.2}Percentage_in_risk_set")

empEduTab <- xtable(empEduTab,
  caption = "Employment and Education Categories",
  label = "empEduTab", align = "llr"
)

print(empEduTab,
  type = "latex", file = paste0(out_path, "empEduTab.tex"),
  include.rownames = FALSE, caption.placement = "top",
  sanitize.colnames.function = center_col_names, 
  booktabs = TRUE, table.placement = "htpb"
)

# Partner category ---------

marriedTab <- exposed %>%
  group_by(marriedCat2) %>%
  summarise(PercRiskSet = n() / nrow(exposed))

marriedTab <- left_join(marriedTab,
  exposed %>% filter(event == 1) %>%
    group_by(marriedCat2) %>%
    summarise(PercExit = n() /
      nrow(exposed %>% filter(event == 1))),
  by = "marriedCat2"
)

# add a total
marriedTab <- bind_rows(
  marriedTab[order(marriedTab$PercRiskSet,
    decreasing = TRUE
  ), ],
  tibble(
    marriedCat2 = "Total",
    PercRiskSet = sum(marriedTab$PercRiskSet),
    PercExit = sum(marriedTab$PercExit)
  )
)

marriedTab$PercRiskSet <- percent_1(marriedTab$PercRiskSet)
marriedTab$PercExit <- percent_1(marriedTab$PercExit)

# update names for printing
names(marriedTab) = c("{0.15}Status", 
                     "{0.15}Percentage_in_risk_set",
                     "{0.15}Percentage_in_Exits")

marriedTab <- xtable(marriedTab,
  caption = "Whether Partnered",
  label = "marriedTab", align = "llrr"
)

print(marriedTab,
  type = "latex", file = paste0(out_path, "marriedTab.tex"),
  include.rownames = FALSE, caption.placement = "top",
  sanitize.colnames.function = center_col_names, 
  booktabs = TRUE, table.placement = "htpb"
)
