# quick scripts to create a file to get variable labels from

library(haven)

# store the path name
path_name <- "~/Data/HILDA/STATA 160c/"
out_path <- "../output/"

strip_first_char <- function(x) {
  substr(x, 2, nchar(x))
}

# read in the last data set
labels <- read_stata(paste0(path_name, "Combined_p160c.dta"))

# keep one random row
labels <- labels[55, ]

# get rid of xwaveid
labels$xwaveid <- NULL

# strip off the first letter of the name if it is 'p'
for (i in seq(from = 1, to = length(names(labels)), by = 1))
{
  if (substr(names(labels)[i], 1, 1) == "p") {
    names(labels)[i] <- strip_first_char(names(labels)[i])
  }
}

# now save the data for later use
save(labels, file = paste0(out_path, "labels.RData"))
