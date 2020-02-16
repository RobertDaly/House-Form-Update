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

# this function converts the stargazer latex to update formating
adj_latex_table <- function(latex, column_labels, widths) {
  # first change to the three part table format
  # the insert points are after the table float starts and after the tabular ends.
  # Also may need to remove any existing notes
  begin_table <- grep("begin.table", latex)
  end_tabular <- grep("end.tabular", latex)
  adj_latex <- c(
    latex[1:begin_table],
    "  \\begin{threeparttable}",
    latex[(begin_table + 1):end_tabular],
    "  \\begin{tablenotes}[flushleft]",
    "  \\item $^{***}$Significant at 1\\%; $^{**}$Significant at 5\\%; $^{*}$Significant at 10\\%.",
    "  \\end{tablenotes}",
    "  \\end{threeparttable}",
    latex[(end_tabular + 1):length(latex)]
  )

  # find the notes and clear out if there
  begin_notes <- grep("Notes:", adj_latex)
  if (length(begin_notes) == 1) {
    adj_latex <- c(
      adj_latex[1:(begin_notes - 1)],
      adj_latex[(begin_notes + 3):length(adj_latex)]
    )
  }

  # now use new blank rows
  no_cols <- length(widths)
  blank_row <- paste(c(rep(" &", no_cols - 1), " \\\\"), sep = "", collapse = "")
  blank_rows <- grep(blank_row, adj_latex)
  adj_latex[blank_rows] <- "\\addlinespace[0.5em]"

  # now put in toprule, mid rule and end rule
  rules <- grep("hline", adj_latex)
  # only do this if there are four
  if (length(rules) == 4) {
    adj_latex[rules[1]] <- "\\toprule"
    adj_latex[rules[3]] <- "\\midrule"
    adj_latex[rules[4]] <- "\\bottomrule"

    # we have also located the header so this next gets dealt with
    # create the new header row
    headings <- paste0("\\parbox[t]{",widths,"\\textwidth}{\\centering ", column_labels, "}")
    headings <- paste(headings, sep = "", collapse = " & ")
    headings <- paste0(headings, " \\\\")
    adj_latex[rules[2]] <- headings
    
    # now clean out some lines
    adj_latex <- c(adj_latex[1:rules[2]], adj_latex[rules[3]:length(adj_latex)])
  }

  return(adj_latex)
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

# rountine to do star gazer type printing
prettyCoef <- function(coef, pVal) {
  textCoef <- paste0("$",format(round(coef, 3), nsmall = 3))
  # now pVal cases
  if (pVal < 0.01) {
    textCoef <- paste0(textCoef, "^{***}$")
  } else {
    if (pVal < 0.05) {
      textCoef <- paste0(textCoef, "^{**}$")
    } else {
      if (pVal < 0.10) {
        textCoef <- paste0(textCoef, "^{*}$")
      } else {
        textCoef <- paste0(textCoef, "$")
      }
    }
  }
  return(textCoef)
}

prettySE <- function(SE) {
  return(paste0("$(", format(round(SE, 3), nsmall = 3), ")$"))
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

mainRes <- stargazer(mod01, mod02, mod03,
  type = "latex", style = "aer",
  dep.var.labels = NULL,
  column.sep.width = "1pt",
  covariate.labels = covLabel1,
  keep.stat = c("n", "rsq", "wald"), df = FALSE, label = "main-res",
  title = "Household Formation Models", notes.append = FALSE
)

write_lines(adj_latex_table(latex = mainRes, column_labels = c(
  "", "Basic model", "Individual and Household Controls",
  "Economic Controls"
), widths = c(0.28, 0.18, 0.23, 0.18)),
path = paste0(out_path, "main-res.tex")
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

# update names for printing purposes
names(ageAdj) <- c("{0.2}Factor Applied to Hazard Rate", "{0.2}Average Age",
                     "{0.2}Change in Age (months)")

AvgAge <- xtable(ageAdj,
                 caption = "Change in Average Age", label = "ageFactor",
                 align = "lccc", digits = c(0, 2, 1, 0)
)

print(AvgAge,
  type = "latex", file = paste0(out_path, "AvgAge.tex"),
  include.rownames = FALSE, caption.placement = "top",
  sanitize.colnames.function = center_col_names, 
  booktabs = TRUE, table.placement = "htpb"
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

house_cost_res <- stargazer(mod03, mod04, mod05, mod06,
  type = "latex", style = "aer",
  dep.var.labels = NULL,
  covariate.labels = covLabel2, column.sep.width = "1pt",
  keep.stat = c("n", "rsq", "wald"), df = FALSE, label = "house-cost-res",
  title = "Household Formation by Housing Cost Measures",
  notes.append = FALSE
)

write_lines(adj_latex_table(latex = house_cost_res, column_labels = c(
  "" , "House Prices", "Mortgage Costs", "Rental Costs",
  "All Housing Costs")
                  , widths = c(0.24, 0.16, 0.16, 0.16, 0.16)),
path = paste0(out_path, "house-cost-res.tex")
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

exit_res <- stargazer(mod05, mod5.1, mod5.2, mod5.3,
                      type = "latex", style = "aer",
                      covariate.labels = c(head(covLabel1, -1), "Log Rent Costs"),
                      column.sep.width = "1pt",
                      keep.stat = c("n", "rsq", "wald"), df = FALSE, label = "exit_res",
                      title = "Home Ownership Sensitivities", notes.append = FALSE
)

write_lines(adj_latex_table(latex = exit_res, column_labels = c("", 
                "Rent Cost Model", "Exit to Rental", "Exit to Owned", "Unknowns"),
                    widths = c(0.22, rep(0.15,4))),
  path = paste0(out_path, "exit-res.tex")
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
# as it does not support coxme

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

# this uses location as a fixed effect
mod09 <- coxph(Surv(entry, exit, event) ~ gender + sibCat2 + parOwnCat + uneRate
               + mortgRate + log(RP_HousePrice_median_real) +
                 as.factor(hhsgcc), data = exposed, ties = "efron")

sumMod09 <- summary(mod09)
# and then keep what is needed (coef, SE, p-value)
sumMod09 <- tibble(
  coef = sumMod09$coefficients[, 1],
  SE = sumMod09$coefficients[, 3],
  pVal = sumMod09$coefficients[, 5]
)

cat("Effect with a location fixed effect ", mod09$coefficients[8], "\n")

cat(" Run the WALD test for impact of fixed effects. \n")
waldtest(mod03, mod09)

#---------------
# now generate the table as a vector of character lines in Latex type
# this could be set up as a function for reuse
# start with header data
effects_res <- c(
  "\\begin{table}[htpb] \\centering",
  "\\begin{threeparttable}",
  "\\caption{Inclusion of Random and Fixed Effects}",
  "\\label{effects_res}",
  "\\tabularnewline",
  "\\begin{tabular}{@{\\extracolsep{1pt}}lcccc}",
  "\\toprule")

# add the column headings
column_labels <- c("", "No Random Effects", "Individual Effects",
                  "Random Location Effects", "Fixed Location Effects")
widths <- c(0.25, rep(0.14, 4))
headings <- paste0("\\parbox[t]{",widths,"\\textwidth}{\\centering ", column_labels, "}")
headings <- paste(headings, sep = "", collapse = " & ")
headings <- paste0(headings, " \\\\")
effects_res <- c(effects_res, headings, "\\midrule")

# now add the rows
for (i in 1:length(covLabel1)) {
  # first add the covariates
  effects_res <- c(effects_res, paste(c(), sep = " & "))
  row_text <- c(covLabel1[i], 
                prettyCoef(sumMod03$coef[i], sumMod03$pVal[i]),
                prettyCoef(sumMod07$coef[i], sumMod07$pVal[i]),
                prettyCoef(sumMod08$coef[i], sumMod08$pVal[i]),
                prettyCoef(sumMod09$coef[i], sumMod09$pVal[i]))
  row_text <- paste(row_text, sep = "", collapse = " & ")
  row_text <- paste0(row_text, " \\\\ ")
  effects_res <- c(effects_res, row_text)
  
  # then the standard deviations
  row_text <- c("", 
                prettySE(sumMod03$SE[i]),
                prettySE(sumMod07$SE[i]),
                prettySE(sumMod08$SE[i]),
                prettySE(sumMod09$SE[i]))
  row_text <- paste(row_text, sep = "", collapse = " & ")
  row_text <- paste0(row_text, " \\\\ ")
  effects_res <- c(effects_res, row_text)
  
  # and finally the spacing row
  effects_res <- c(effects_res, "\\addlinespace[0.5em]")
}

# and finally close with bottom rule, end statements and notes
effects_res <- c(effects_res,
  "  \\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "  \\item $^{***}$Significant at 1\\%; $^{**}$Significant at 5\\%; $^{*}$Significant at 10\\%.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

# then write out the results
write_lines(effects_res,
            path = paste0(out_path, "effects-res.tex"))
            
# ---------------
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
ModLoc <- ModLoc[, c(6, 2:5)]

# update the names for printing
names(ModLoc) <- c("{0.15}Location", "{0.11}Events", "{0.15}House Price Coefficient",
                   "{0.11}Standard Error", "{0.11}P-value")

loc_tex <- xtable(ModLoc,
  caption = "Results by Location", label = "ModLoc",
  align = "llrrrr", digits = c(0, 0, 0, 3, 3, 3)
)

print(loc_tex,
  type = "latex", file = paste0(out_path, "ModLoc.tex"),
  include.rownames = FALSE, caption.placement = "top",
  sanitize.colnames.function = center_col_names, 
  booktabs = TRUE, table.placement = "htpb"
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
  "Neither working nor in labour force", "No Response", "Unemployed", "Working FT", "Working PT"
)

emp_edu_res <- stargazer(mod03, mod11,
                      type = "latex", style = "aer",
                      covariate.labels = c(covLabel1, covEduEmp),
                      column.sep.width = "1pt",
                      keep.stat = c("n", "rsq", "wald"), df = FALSE, label = "empEdu",
                      title = "Impact of Employment and Education Status", notes.append = FALSE
)

write_lines(adj_latex_table(latex = emp_edu_res, column_labels = c("", 
                            "Main Model", "Employment and Education"),
                            widths = c(0.24, rep(0.18, 2))),
            path = paste0(out_path, "empEdu.tex")
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
