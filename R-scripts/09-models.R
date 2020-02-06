# this file does all the models that appear in the thesis

# load libraries, data and other preliminaries ----------------------------
library(tidyverse)
library(survival)
library(lubridate)
library(scales)
library(stargazer)
library(xtable)
library(coxme)
library(lmtest)

# store the path name
out_path <- "../output/"

# read in the exposure data
exposed <- read_rds(paste0(out_path, "exposure-05.rds"))

# this function returns a plot of the survival function
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
      geom_point() + ylab("") + xlab("Age") + theme_minimal()
  }
  return(fit.plot)
}
# this function returns an adjusted survival function
adjSurv <- function(multFac, coxFit) {
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
  return(adjSurv)
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

# this function returns the average age for the model
avgAgeMod <- function(coxFit, fitdata = exposed) {
  exitRates <- predict(coxFit, type = "expected", newdata = fitdata)
  # average is based on entry age so needs 0.5 added as on average
  # lives are half a year older when they exit
  avg <- sum(exitRates * fitdata$entry) / sum(exitRates) + 0.5
  return(avg)
}

# First model results -----------------------------------------------------
mod01 <- coxph(Surv(entry, exit, event) ~ log(RP_HousePrice_median_real),
  data = exposed
)

mod02 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat
  + log(RP_HousePrice_median_real), data = exposed)

mod03 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat
  + uneRate + mortgRate + log(RP_HousePrice_median_real),
data = exposed, model = TRUE
)


covLabel1 <- c(
  "Males", "2+ siblings", "No siblings", "No data on siblings",
  "Parents rent", "Unemployment rate", "Mortgage rate",
  "Log house prices"
)

stargazer(mod01, mod02, mod03,
  type = "text", style = "aer",
  out = paste0(out_path, "mainRes.tex"),
  dep.var.labels = NULL,
  column.labels = c(
    "Basic model", "Plus HILDA covariates",
    "Plus Economic covariates"
  ),
  column.sep.width = "1pt",
  covariate.labels = covLabel1,
  keep.stat = c("n", "rsq", "wald"), df = FALSE, label = "mainRes",
  title = "Household formation models", notes.append = FALSE
)


# plot of survival function -------------------------------------------------------

SurvModPlot <- plotFit(mod03, TRUE)

# add a +10% chart
SurvModPlot <- SurvModPlot + geom_point(aes(y = adjSurv(1.1, mod03)), shape = 2) +
  geom_point(aes(y = adjSurv(0.9, mod03)), shape = 3)

plot(SurvModPlot)

# now save the plots to be used by latex for thesis
ggsave("ModSurv.pdf",
  plot = SurvModPlot, device = "pdf", width = 14, height = 9,
  units = "cm", path = out_path
)

# table of age adjustments ------------------------------------------------

ageAdj <- tibble(
  Factor = c(0.8, 0.9, 1.0, 1.1, 1.25),
  Average.age = sapply(Factor, avgAgeFac, mod03)
)
# add column for change in age
temp <- ageAdj$Average.age[ageAdj$Factor == 1]
ageAdj$Change.in.age <- (ageAdj$Average.age - temp) * 12

AvgAge <- xtable(ageAdj,
  caption = "Average ages by factors", label = "ageFactor",
  align = "llrr", digits = c(0, 2, 1, 0)
)

print(AvgAge,
  type = "latex", file = paste0(out_path, "AvgAge.tex"),
  include.rownames = FALSE, tabular.environment = "threeparttable"
)


# other measures of housing costs -----------------------------------------
mod04 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
  + mortgRate + log(HILDA_Mortg_median_real), data = exposed)

mod05 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
  + mortgRate + log(HILDA_Rent_median_real), data = exposed)

mod06 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
  + mortgRate + log(RP_HousePrice_median_real)
  + log(HILDA_Mortg_median_real) + log(HILDA_Rent_median_real), data = exposed)

covLabel2 <- c(covLabel1, c("Log mortgage costs", "Log rent costs"))

stargazer(mod03, mod04, mod05, mod06,
  type = "text", style = "aer",
  out = paste0(out_path, "HC_Table.tex"),
  column.labels = c(
    "House prices", "Mortgage costs", "Rental costs",
    "All housing costs"
  ),
  dep.var.labels = "",
  covariate.labels = covLabel2, column.sep.width = "1pt",
  keep.stat = c("n", "rsq", "wald"), df = FALSE, label = "HC_Table",
  title = "Household formation by housing cost measures",
  notes.append = FALSE
)


# Exits of one type -------------------------------------------------------

exitRental <- exposed$event == 1 & exposed$hstenrNxt == 2

exitOwned <- exposed$event == 1 & exposed$hstenrNxt == 1

exitUnk <- exposed$event == 1 & (is.na(exposed$hstenrNxt) |
  (exposed$hstenrNxt != 1 & exposed$hstenrNxt != 2))

exitProp <- list(
  rent = sum(exitRental, na.rm = TRUE) / nrow(exposed %>% filter(event == 1)),
  own = sum(exitOwned, na.rm = TRUE) / nrow(exposed %>% filter(event == 1)),
  unk = sum(exitUnk, na.rm = TRUE) / nrow(exposed %>% filter(event == 1))
)
print(exitProp)

# this is messy but copy the data, knock out other outcomes and retry regression
newExposed <- exposed
newExposed$event[!exitRental] <- 0
mod5.1 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
  + mortgRate + log(HILDA_Rent_median_real), data = newExposed)

newExposed <- exposed
newExposed$event[!exitOwned] <- 0
mod5.2 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
  + mortgRate + log(HILDA_Rent_median_real), data = newExposed)

newExposed <- exposed
newExposed$event[!exitUnk] <- 0
mod5.3 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
  + mortgRate + log(HILDA_Rent_median_real), data = newExposed)

stargazer(mod05, mod5.1, mod5.2, mod5.3,
  type = "text", style = "aer",
  out = paste0(out_path, "ModOwner.tex"),
  column.labels = c("", "Exit to Rental", "Exit to Owned", "Unknowns"),
  covariate.labels = c(head(covLabel1, -1), "Log Rent Costs"),
  column.sep.width = "1pt",
  keep.stat = c("n", "rsq", "wald"), df = FALSE, label = "ModOwner",
  title = "Home ownership sensitivities", notes.append = FALSE
)


# unobserved heterogeneity -----------------------------------------
# this adds a random effect for all
mod07 <- coxme(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
  + mortgRate + log(RP_HousePrice_median_real) +
  (1 | xwaveid), data = exposed, ties = "efron")

cat("the difference in house price coefficient is ", mod03$coefficients[8]
- mod07$coefficients[8], "\n")

# location fixed effects ------------------------------------
# this only includes the random effect on location
mod08 <- coxme(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
  + mortgRate + log(RP_HousePrice_median_real) + (1 | hhsgcc),
data = exposed, ties = "efron"
)
cat("the revised house price coefficient is ", mod08$coefficients[8], "\n")

# shown the table of random effects - stargazer cannot be used unfortunately
# first get the summary of each fit
sumMod03 <- summary(mod03)
# and then keep what is needed (coef, SE, p-value)
sumMod03 <- tibble(
  coef = sumMod03$coefficients[, 1],
  SE = sumMod03$coefficients[, 3],
  pVal = sumMod03$coefficients[, 5]
)

# complex and has to be generated
sumMod07 <- tibble(
  coef = mod07$coefficients,
  SE = diag(sqrt(vcov(mod07)))
) %>%
  mutate(pVal = pnorm(-abs(coef), 0, SE) * 2)

# complex and has to be generated
sumMod08 <- tibble(
  coef = mod08$coefficients,
  SE = diag(sqrt(vcov(mod08)))
) %>%
  mutate(pVal = pnorm(-abs(coef), 0, SE) * 2)

prettyCoef <- function(coef, pVal) {
  textCoef <- (paste("$", format(coef, digits = 3, nsmall = 3), "$", sep = ""))
  # now pVal cases
  if (pVal < 0.01) {
    textCoef <- paste(textCoef, "^***$", sep = "")
  } else {
    if (pVal < 0.05) {
      textCoef <- paste(textCoef, "^**$", sep = "")
    } else {
      if (pVal < 0.10) {
        textCoef <- paste(textCoef, "^*$", sep = "")
      } else {
        textCoef <- paste(textCoef, "$", sep = "")
      }
    }
  }
  return(textCoef)
}
prettySE <- function(SE) {
  return(paste("$(", format(SE, digits = 3, nsmall = 3), ")$", sep = ""))
}


# now armed with this loop to generate the table
rY <- length(covLabel1) * 2
random <- tibble(
  variable = rep("", rY), NoRand = rep("", rY),
  indRE = rep("", rY), locRE = rep("", rY)
)
for (i in seq(1, rY / 2, 1)) {
  rX <- 1 + (i - 1) * 2
  # do the first line
  random[rX, 1] <- covLabel1[i]
  # now for columns
  random[rX, 2] <- prettyCoef(sumMod03$coef[i], sumMod03$pVal[i])
  random[rX, 3] <- prettyCoef(sumMod07$coef[i], sumMod07$pVal[i])
  random[rX, 4] <- prettyCoef(sumMod08$coef[i], sumMod08$pVal[i])
  # and SE
  random[rX + 1, 2] <- prettySE(sumMod03$SE[i])
  random[rX + 1, 3] <- prettySE(sumMod07$SE[i])
  random[rX + 1, 4] <- prettySE(sumMod08$SE[i])
}

random <- xtable(random,
  caption = "Inclusion of Random Effects",
  label = "random", align = "llrrr"
)

print(random,
  type = "latex", file = paste0(out_path, "random.tex"),
  include.rownames = FALSE
)

# this uses location as a fixed effect
mod09 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
  + mortgRate + log(RP_HousePrice_median_real) +
  as.factor(hhsgcc), data = exposed, ties = "efron")

cat("Effect with a location fixed effect ", mod09$coefficients[8], "\n")

cat(" Run the WALD test for impact of fixed effects. \n")
waldtest(mod03, mod09)

# set up a function to iterate through each location returning its size and
# the house price coefficient.
retHPCoeff <- function(hhsgccX) {
  modTemp <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat
    + uneRate + mortgRate + log(RP_HousePrice_median_real),
  data = exposed %>% filter(hhsgcc == hhsgccX), ties = "efron"
  )
  temp <- summary(modTemp)
  return(list(
    hhsgccX, temp$nevent,
    temp$coefficients[8, 1], temp$coefficients[8, 3],
    temp$coefficients[8, 5]
  ))
}

temp <- sapply(sort(unique(exposed$hhsgcc)), retHPCoeff)

# put the results in a dataframe
ModLoc <- tibble(
  Location = unlist(temp[1, ]), Events = unlist(temp[2, ]),
  House.Price.Coeff = unlist(temp[3, ]),
  Std.Error = unlist(temp[4, ]),
  P.value = unlist(temp[5, ])
)

# update the location names
locName <- tibble(
  Location = c(11, 19, 21, 29, 31, 39, 41, 49, 51, 59, 61, 71, 81),
  names = c(
    "Sydney", "Rest of NSW", "Melbourne", "Rest of VIC",
    "Brisbane", "Rest of QLD", "Adelaide", "Rest of SA",
    "Perth", "Rest of WA", "Tasmania", "NT", "ACT"
  )
)
ModLoc <- full_join(ModLoc, locName, by = "Location")
ModLoc$Location <- NULL
ModLoc <- ModLoc[, c(5, 1, 2, 3, 4)]

ModLoc <- xtable(ModLoc,
  caption = "Results by location", label = "ModLoc",
  align = "llrrrr", digits = c(0, 0, 0, 3, 3, 3)
)

print(ModLoc,
  type = "latex", file = paste0(out_path, "ModLoc.tex"),
  include.rownames = FALSE
)

# employment status ----------------------

mod10 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat
  + uneRate + mortgRate + log(RP_HousePrice_median_real)
  + esdtlFac, data = exposed)

covEmp <- c("Employed PT", "Unemployed", "NILF attached", "NILF", "No Emp. data")

stargazer(mod03, mod10,
  type = "text", style = "aer", out = paste0(out_path, "ResTable3.tex"),
  column.labels = c("Main model", "Employment"),
  dep.var.labels = "Employment and Leaving Home",
  covariate.labels = c(covLabel1, covEmp),
  column.sep.width = "1pt",
  keep.stat = c("n", "rsq", "wald"), df = FALSE, label = "employed",
  title = "Household formation models", notes.append = FALSE
)

# employment and education status ----------------------
mod11 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat
  + uneRate + mortgRate + log(RP_HousePrice_median_real)
  + eduEmpCat, data = exposed)

covEduEmp <- c(
  "College not working", "College working", "Part time college",
  "NILF", "No Response", "Unemployed", "Working FT", "Working PT"
)

stargazer(mod03, mod11,
  type = "text", style = "aer",
  out = paste0(out_path, "empEdu.tex"),
  column.labels = c("Main model", "Employment"),
  dep.var.labels = "Employment and Leaving Home",
  covariate.labels = c(covLabel1, covEduEmp),
  column.sep.width = "1pt",
  keep.stat = c("n", "rsq", "wald"), df = FALSE, label = "empEdu",
  title = "Impact of Employment and Education Status", notes.append = FALSE
)

# marital status ----------------------

mod12 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
  + mortgRate + log(RP_HousePrice_median_real) + marriedCat2,
data = exposed
)

covMarried <- c("Has partner", "Status not known")

stargazer(mod03, mod12,
  type = "text", style = "aer",
  out = paste0(out_path, "married.tex"),
  column.labels = c("Main model", "Marital Status"),
  covariate.labels = c(covLabel1, covMarried),
  column.sep.width = "1pt",
  keep.stat = c("n", "rsq", "wald"), df = FALSE, label = "married",
  title = "Marital Status and Leaving Home", notes.append = FALSE
)

# now relationship transitions-----------------
# exitSingle <- exposed$event == 1 & exposed$mrcurr == 6 &
#   exposed$mrcurrNxt == 6
#
# exitPartner <- exposed$event == 1 & exposed$mrcurr == 6 &
#   (exposed$mrcurrNxt == 1 | exposed$mrcurrNxt == 2)
#
# exitOther <- exposed$event == 1 & (exposed$mrcurr !=6 |
#              (exposed$mrcurrNxt == -10 | is.na(exposed$mrcurrNxt)))

# logic is simplified below
exitSingle <- exposed$event == 1 & exposed$mrcurrNxt == 6

exitPartner <- exposed$event == 1 &
  (exposed$mrcurrNxt == 1 | exposed$mrcurrNxt == 2)

exitOther <- exposed$event == 1 & (exposed$mrcurrNxt == -10 |
  is.na(exposed$mrcurrNxt))

# this is messy but copy the data, knock out other outcomes and retry regression
newExposed <- exposed
newExposed$event[!exitSingle] <- 0
mod13 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
  + mortgRate + log(RP_HousePrice_median_real), data = newExposed)

newExposed <- exposed
newExposed$event[!exitPartner] <- 0
mod14 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
  + mortgRate + log(RP_HousePrice_median_real), data = newExposed)

newExposed <- exposed
newExposed$event[!exitOther] <- 0
mod15 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
  + mortgRate + log(RP_HousePrice_median_real), data = newExposed)


stargazer(mod03, mod13, mod14, mod15,
  type = "text", style = "aer",
  out = paste0(out_path, "sepMarry.tex"),
  column.labels = c("Main model", "Single Exits", "Married Exits", "Unknowns"),
  covariate.labels = c(covLabel1),
  column.sep.width = "1pt",
  keep.stat = c("n", "rsq", "wald"), df = FALSE, label = "sepMarry",
  title = "Marital Status and Leaving Home", notes.append = FALSE
)


# Income measures ---------------------------------------------------------

mod16 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
  + mortgRate + log(RP_HousePrice_median_real)
  + log(tifdip_real) + log(famInc_real), data = exposed)

stargazer(mod03, mod16,
  type = "text", style = "aer", out = paste0(out_path, "ResTable8.tex"),
  column.labels = c("Main model", "Income measures"),
  covariate.labels = covLabel1,
  column.sep.width = "1pt",
  keep.stat = c("n", "rsq", "wald"), df = FALSE, label = "employed",
  title = "Marital Status, Leaving Home and income", notes.append = FALSE
)


# Largest model -----------------------------------------------------------
mod17 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat
  + uneRate + mortgRate + log(RP_HousePrice_median_real)
  + eduEmpCat + marriedCat2, data = exposed, model = TRUE)

# plot the different survival curves
surv01 <- survfit(mod01)
surv02 <- survfit(mod02)
surv03 <- survfit(mod03)
surv11 <- survfit(mod11)
surv13 <- survfit(mod13)
surv17 <- survfit(mod17)
Survs <- bind_rows(
  tibble(group = "mod01", surv = surv01$surv, time = surv01$time),
  tibble(group = "mod02", surv = surv02$surv, time = surv02$time),
  tibble(group = "mod03", surv = surv03$surv, time = surv03$time),
  tibble(group = "mod11", surv = surv11$surv, time = surv11$time),
  tibble(group = "mod13", surv = surv13$surv, time = surv13$time),
  tibble(group = "mod17", surv = surv17$surv, time = surv17$time)
)

survPlot <- ggplot(data = Survs, mapping = aes(x = time)) +
  geom_point(aes(y = surv, color = group))
plot(survPlot)

stargazer(mod03, mod17,
  type = "text", style = "aer", out = paste0(out_path, "ResTable9.tex"),
  column.labels = c("Main model", "all in model"),
  covariate.labels = covLabel1,
  column.sep.width = "1pt",
  keep.stat = c("n", "rsq", "wald"), df = FALSE, label = "employed",
  title = "Model with everything in", notes.append = FALSE
)

# Low quartile  -----------------------------------------------------------

mod18 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
  + mortgRate + log(RP_HousePrice_LQmed_real), data = exposed)

stargazer(mod03, mod18,
  type = "text", style = "aer", out = paste0(out_path, "ResTable10.tex"),
  column.labels = c("All in model", "Lower Quartile"),
  covariate.labels = c(covLabel1),
  column.sep.width = "1pt",
  keep.stat = c("n", "rsq", "wald"), df = FALSE, label = "LwrQ",
  title = "Lower quartile house prices", notes.append = FALSE
)


# Simultation -------------------------------------------------------------

modSimulM <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat +
  hhsgcc,
data = exposed %>% filter(wave == "m")
)

modSimulN <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat +
  hhsgcc,
data = exposed %>% filter(wave == "n")
)


modSimulO <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat +
  hhsgcc,
data = exposed %>% filter(wave == "o")
)

# past house price growth on average
housePrice <- exposed %>%
  group_by(wave) %>%
  summarise(avgHouse = mean(RP_HousePrice_median_real)) %>%
  mutate(HPGrow = avgHouse / lag(avgHouse) - 1)

# this creates a five year projection
fiveYearProj <- function(assInc, coeff, baseMod) {
  # fit the survival curve
  survCurve <- survfit(baseMod)
  survCurveDF <- tibble(
    Time = survCurve[["time"]],
    Surv = survCurve[["surv"]],
    Year = "2015"
  )
  avgAges <- tibble(
    Year = "2015", avgAge = avgAgeFac(1, baseMod),
    HPGrow = 100 * housePrice$HPGrow[housePrice$wave == "o"]
  )

  # now project this only 1 year first
  for (Yr in c(1, 2, 3, 4, 5)) {
    # house price increase factor
    fac <- (1 + assInc)^(Yr * coeff)

    # how adj hazard rates for the factor and remake survival curve
    # derive the harard rates from the survivor curve
    hazRate <- 1 - survCurve$surv / c(1, head(survCurve$surv, -1))
    adjHazRate <- fac * hazRate

    # loop to recreate
    # first is different so do it outside of loop
    adjSurv <- rep(1, length(adjHazRate))
    adjSurv[1] <- 1 - adjHazRate[1]
    for (i in seq(2, length(adjHazRate), 1)) {
      adjSurv[i] <- adjSurv[i - 1] * (1 - adjHazRate[i])
    }

    # add to results
    survCurveDF <- bind_rows(
      survCurveDF,
      tibble(
        Time = survCurve[["time"]],
        Surv = adjSurv,
        Year = as.character(2015 + Yr)
      )
    )
    avgAges <- bind_rows(
      avgAges,
      tibble(
        Year = as.character(2015 + Yr),
        avgAge = avgAgeFac(fac, baseMod),
        HPGrow = 100 * assInc
      )
    )
  }
  # end of projeciton loop

  return(list(survCurveDF, avgAges))
}

projRes <- fiveYearProj(0.05, mod03$coefficients[8], modSimulO)

plotData <- bind_rows(
  tibble(
    Time = survfit(modSimulM)[["time"]],
    Surv = survfit(modSimulM)[["surv"]],
    Year = "2013"
  ),
  tibble(
    Time = survfit(modSimulN)[["time"]],
    Surv = survfit(modSimulN)[["surv"]],
    Year = "2014"
  ),
  projRes[[1]]
)


plotSurv <- ggplot(
  data = plotData,
  mapping = aes(x = Time)
) +
  geom_line(aes(y = Surv, linetype = Year)) + ylab("") + xlab("") + theme_minimal()

plot(plotSurv)

ggsave("plotSurv.pdf",
  plot = plotSurv, device = "pdf", width = 14, height = 9,
  units = "cm", path = out_path
)

ProgAvgAges <- bind_rows(
  tibble(
    Year = "2013", avgAge = avgAgeFac(1, modSimulM),
    HPGrow = 100 * housePrice$HPGrow[housePrice$wave == "m"]
  ),
  tibble(
    Year = "2014", avgAge = avgAgeFac(1, modSimulN),
    HPGrow = 100 * housePrice$HPGrow[housePrice$wave == "n"]
  ),
  projRes[[2]]
)

ProgAvgAges <- xtable(ProgAvgAges,
  caption = "Projected Average Age of Leaving Home",
  label = "ProgAvgAges",
  align = "llrr", digits = c(0, 0, 1, 1)
)

print(ProgAvgAges,
  type = "latex", file = paste0(out_path, "ProgAvgAges.tex"),
  include.rownames = FALSE
)
