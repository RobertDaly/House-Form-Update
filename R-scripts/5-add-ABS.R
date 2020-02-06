#------------------------------------------------------------------------------
# This code reads in the ABS data that is needed for the modelling and
# links it to the existed exposed file
# also used to read in interest rates from RBA data
#------------------------------------------------------------------------------

# first load libraries needed
library(openxlsx)
library(lubridate)
library(tidyverse)

# store the path name
out_path <- "../00-Output/"
abs_path <- "~/Documents/MasterEconAnalysis/Thesis/ABS Data/"
#------------------------------------------------------------------------------
# First read in employment data
#------------------------------------------------------------------------------
# The codes starts by loading the unemployment data
# The variables required are in a CSV file
# The main spreadsheet is then loaded each tab in turn
# The varables required are selected from that
# and then averaged for the years of interest
# Before the relevant rate is calculated to be merged back into the exposed
# file

# Start by reading in the spreadsheet data
ABSemployData <- data.frame(
  read.xlsx(
    xlsxFile = paste0(abs_path, "6202012.xlsx"),
    sheet = "Data1", startRow = 10, colNames = TRUE,
    rowNames = FALSE, detectDates = TRUE
  ),
  read.xlsx(
    xlsxFile = paste0(abs_path, "6202012.xlsx"),
    sheet = "Data2", startRow = 10, colNames = TRUE,
    rowNames = FALSE, detectDates = TRUE
  ),
  read.xlsx(
    xlsxFile = paste0(abs_path, "6202012.xlsx"),
    sheet = "Data3", startRow = 10, colNames = TRUE,
    rowNames = FALSE, detectDates = TRUE
  )
)

# Then read in the required columns and the new names for them
nameDF <- read.table(
  file = paste0(abs_path, "ABS Employ Data.csv"),
  header = TRUE, sep = ","
)

# add the date variable to this list
nameDF <- rbind(data.frame(
  Description = "Date", Series.ID = "Series.ID",
  Measure = "", State = "", VarName = "MthYr"
), nameDF)

# just get the variables needed from ABSemployData
# Note that the order is only obtained above if the Series.ID are strings
# not factors (as factors are integer types it does not keep the right vars)
nameDF$Series.ID <- as.character(nameDF$Series.ID)
ABSemployData <- ABSemployData[, nameDF$Series.ID]

# and give them sensible names
colnames(ABSemployData) <- nameDF$VarName

# next create month and year variables
ABSemployData$mth <- month(ABSemployData$MthYr)
ABSemployData$Year <- year(ABSemployData$MthYr)

# now average the variables over the year
ABSemployData <- ABSemployData %>%
  group_by(Year) %>%
  summarise_all(mean)
# and remove the unneeded ones
ABSemployData$mth <- NULL
ABSemployData$MthYr <- NULL

# now gather everything
ABSemployData <- gather(
  data = ABSemployData, key = "var", value = "value",
  -Year
)

# break up var into its components
ABSemployData <- separate(ABSemployData,
  col = "var", sep = "_",
  into = c("measure", "State")
)

# and now spread with the measures
ABSemployData <- spread(ABSemployData, key = "measure", value = "value")

# now (at last) we can calculate the unemployment rate scaled by 100
ABSemployData$uneRate <- ABSemployData$une / (ABSemployData$une +
  ABSemployData$emp) * 100

# do a graph to understand data
employPlot <- ggplot(
  data = ABSemployData[ABSemployData$Year >= 2001, ],
  aes(x = Year, y = uneRate)
)
employPlot <- employPlot + geom_line(aes(color = State, linetype = State))
plot(employPlot)

# the data can now be linked to the exposure data (wait until other data is
# formed)

#------------------------------------------------------------------------------
# Now read in State Final demand data
#------------------------------------------------------------------------------

# Start by reading in the spreadsheet data
ABSstateDemand <-
  read.xlsx(
    xlsxFile = paste0(abs_path, "5206025_sfd_summary.xlsx"),
    sheet = "Data1", startRow = 10, colNames = TRUE,
    rowNames = FALSE, detectDates = TRUE
  )

# Then read in the required columns and the new names for them
nameDF <- read.table(
  file = paste0(abs_path, "ABS State Demand.csv"),
  header = TRUE, sep = ","
)

# add the date variable to this list
nameDF <- rbind(
  data.frame(
    Data.Item.Description = "Date",
    Series.ID = "Series.ID",
    VarName = "MthYr"
  ),
  nameDF
)

# just get the variables needed from ABSemployData
# Note that the order is only obtained above if the Series.ID are strings
# not factors (as factors are integer types it does not keep the right vars)
nameDF$Series.ID <- as.character(nameDF$Series.ID)
ABSstateDemand <- ABSstateDemand[, nameDF$Series.ID]

# and give them sensible names
colnames(ABSstateDemand) <- nameDF$VarName

# calculate the year
ABSstateDemand <- ABSstateDemand %>% mutate(Year = year(MthYr))


# now only keep the december data
ABSstateYoY <- ABSstateDemand[month(ABSstateDemand$MthYr) == 12, ]
# Gather the data into states and group by state
ABSstateYoY <- gather(
  data = ABSstateYoY, key = "State",
  value = "Demand", -Year, -MthYr
)
# group by year and determine growth
ABSstateYoY <- ABSstateYoY %>%
  group_by(State) %>%
  mutate(
    incGrow =
      Demand / lag(Demand) - 1
  )

# now plot the data to review it
growthPlot <- ggplot(
  data = ABSstateYoY,
  aes(x = Year, y = incGrow)
)
growthPlot <- growthPlot + geom_line(aes(color = State, linetype = State))
plot(growthPlot)

# this calculation approach results in volatile annual growth particularly
# for Northern Territories.
#
# Hence try another method based on average demand over the year.

# Gather the data into states and group by state
ABSstateAvg <- gather(
  data = ABSstateDemand, key = "State",
  value = "Demand", -Year, -MthYr
)

# group by year, average income over the year and then calculate
# year on year growth
ABSstateAvg <- ABSstateAvg %>%
  group_by(State, Year) %>%
  summarise(avgDem = mean(Demand)) %>%
  mutate(incGrow = avgDem / lag(avgDem) - 1)

# now plot the data to review it
growthPlot <- ggplot(
  data = ABSstateAvg[ABSstateAvg$Year >= 2001, ],
  aes(x = Year, y = incGrow)
)
growthPlot <- growthPlot + geom_line(aes(color = State, linetype = State)) +
  scale_y_continuous(name = "Growth in Average Annual Demand")
plot(growthPlot)

# now compare the two growth approches
# first merge the two approaches
joinGrowth <- inner_join(ABSstateAvg, ABSstateYoY,
  by = c("Year", "State"),
  suffix = c("Avg", "YoY")
)
# remove the date column
joinGrowth$MthYr <- NULL

# now difference the growth
joinGrowth$incGrowDif <- joinGrowth$incGrowAvg - joinGrowth$incGrowYoY

# and graph the difference
growthDifPlot <- ggplot(
  data = joinGrowth[(joinGrowth$Year >= 2001) &
    (joinGrowth$State != "NT" &
      joinGrowth$State != "ACT"), ],
  aes(x = Year, y = incGrowDif)
)
growthDifPlot <- growthDifPlot + geom_line(aes(color = State, linetype = State)) +
  scale_y_continuous(name = "Growth Differentials")
plot(growthDifPlot)

# and show the originals side by side (split states into two groups first)
joinGrowth$sizeCat <- "Large"
joinGrowth$sizeCat[joinGrowth$State == "ACT" |
  joinGrowth$State == "NT" |
  joinGrowth$State == "SA" |
  joinGrowth$State == "TAS" ] <- "Small"
growthCompPlot <- ggplot(
  data = joinGrowth,
  aes(x = Year, y = incGrowAvg)
) +
  geom_line() + geom_line(aes(y = incGrowYoY, color = "Red")) +
  facet_wrap(facets = ~State, scales = "free_y")
plot(growthCompPlot)
# the conclusion is that the avg appraoch is smoother and adds a six month
# lag that may be more meaningful in making formation decisions.

#------------------------------------------------------------------------------
# Read in inflation data now
# Start by reading in the spreadsheet data
ABScpiData <-
  read.xlsx(
    xlsxFile = paste0(abs_path, "ABS_CPI_640106.xlsx"),
    sheet = "Data1", startRow = 10, colNames = TRUE,
    rowNames = FALSE, detectDates = TRUE
  )

# for the nominal to real factor use A2325846C
ABSdeflator <- ABScpiData %>%
  select(Date = Series.ID, cpiIndex = A2325846C) %>%
  filter(!is.na(cpiIndex))
# create mth and year and only keep 30 June
ABSdeflator <- ABSdeflator %>% mutate(mth = month(Date), Year = year(Date))
ABSdeflator <- ABSdeflator %>%
  filter(mth == 6) %>%
  select(cpiIndex, Year)
numeraire <- ABSdeflator$cpiIndex[ABSdeflator$Year == 2001]
ABSdeflator$deflator <- numeraire / ABSdeflator$cpiIndex

# the required column name is A2332686K
# which contains "Index Numbers ;  All groups CPI excluding Housing and
# Insurance and financial services ;  Australia"
ABScpiData <- ABScpiData %>%
  select(Date = Series.ID, cpiIndex = A2332686K) %>%
  filter(!is.na(cpiIndex))
# create mth and year and only keep year end
ABScpiData <- ABScpiData %>% mutate(mth = month(Date), Year = year(Date))
ABScpiData <- ABScpiData %>%
  filter(mth == 12) %>%
  select(cpiIndex, Year)
ABScpiData <- ABScpiData %>% mutate(cpiRate = 100 * (cpiIndex / lag(cpiIndex) - 1))

# do a graph to understand data
cpiPlot <- ggplot(
  data = ABScpiData %>% filter(Year > 2001),
  aes(x = Year, y = cpiRate)
) + geom_line()
plot(cpiPlot)

# the data can now be linked to the exposure data (wait until other data is
# formed)

#------------------------------------------------------------------------------
# Now read in the interest rate data
#------------------------------------------------------------------------------

# Start by reading in the spreadsheet data
RBAmortRate <-
  read.xlsx(
    xlsxFile = paste0(abs_path, "RBA_IntRates_f05hist.xlsx"),
    sheet = "Data", startRow = 11, colNames = TRUE,
    rowNames = FALSE, detectDates = TRUE
  )

# Then read in the required columns and the new names for them
nameDF <- tibble(
  Desc = c(
    "Publication date",
    "Lending rates; Housing loans; Banks; Variable; Standard; Owner-occupier"
  ),
  Series.ID = c("Series.ID", "FILRHLBVS"),
  VarName = c("MthYr", "MortgRate")
)

# just get the variables needed
RBAmortRate <- RBAmortRate[, nameDF$Series.ID]

# and give them sensible names
colnames(RBAmortRate) <- nameDF$VarName

# create averages over each year
# calculate the year
RBAmortRate <- RBAmortRate %>% mutate(Year = year(MthYr))

# and the average
RBAmortRateYear <- RBAmortRate %>%
  group_by(Year) %>%
  summarise(mortgRate = mean(MortgRate))

# now plot the data to review it
intRatePlot <- ggplot(
  data = RBAmortRateYear %>% filter(Year >= 2000),
  aes(x = Year, y = mortgRate)
)
intRatePlot <- intRatePlot + geom_point()
plot(intRatePlot)


#------------------------------------------------------------------------------
# Merge into exposure data
#------------------------------------------------------------------------------
# when merging into the exposure data include 2 lag years
# first group up and filter the ABS data ready for merger and add the lag years
ABSmergeDF <- inner_join(ABSemployData, ABSstateAvg, by = c("Year", "State"))

# scale incGrow by 100
ABSmergeDF$incGrow <- ABSmergeDF$incGrow * 100

# keep only the columns needed
ABSmergeDF <- ABSmergeDF[, c("State", "Year", "uneRate", "incGrow")]
# remove the NAs for 1985
ABSmergeDF <- ABSmergeDF[ABSmergeDF$Year > 1985, ]
# order by year then state
ABSmergeDF <- ABSmergeDF[order(ABSmergeDF$State, ABSmergeDF$Year), ]
# add the lags
ABSmergeDF <- ABSmergeDF %>%
  group_by(State) %>%
  mutate(
    uneRateL1 = lag(uneRate), uneRateL2 = lag(uneRateL1),
    incGrowL1 = lag(incGrow), incGrowL2 = lag(incGrowL1)
  ) %>%
  ungroup()
# make year an integer
ABSmergeDF$Year <- as.integer(ABSmergeDF$Year)
# reorder columns for neatness
ABSmergeDF <- ABSmergeDF[, c(1, 2, 3, 5, 6, 4, 7, 8)]
# and make state a factor for later mapping
ABSmergeDF$State <- as.factor(ABSmergeDF$State)

# now load the exposure data for merging
exposed <- read_rds(path = paste0(out_path, "exposure-02.rds"))

# now that State and Year exist we can merge in the ABS data
exposed <- left_join(x = exposed, y = ABSmergeDF, by = c(
  "State",
  "Year"
))

# now include the mortgage interest rates
exposed <- left_join(x = exposed, y = RBAmortRateYear, by = c("Year"))

# and the CPI Index and rates
exposed <- left_join(x = exposed, y = ABScpiData, by = c("Year"))
ABSdeflator$cpiIndex <- NULL
exposed <- left_join(x = exposed, y = ABSdeflator, by = c("Year"))

# all done so save the exposure data
write_rds(x = exposed, path = paste0(out_path, "exposure-03.rds"))
