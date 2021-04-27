nrow(filter(df.bnp, praktijkcode == 1))

# create a variable indicating the number of total months passed in the DMP evaluation period
startdate <- ymd("2020-01-01")
enddate <- ymd("2021-03_30")
timespan <- interval(start = startdate, end = enddate)
time_length(timespan, unit ="month")
# and a second one for the duration from beginning until the current date
timespan2 <- interval(start = startdate, end = Sys.Date())
interval.m <- time_length(timespan2, unit ="month")
interval.y <- time_length(timespan2, unit ="year")


# calculate the average number of monthly tests for the practice
bnp1 <- df.bnp%>%
  filter(praktijkcode == 1)
round(nrow(bnp1)/interval.m, 2)

# calculate the number of physicians who asked for a test in the practice and the average per participating physician
nrow(distinct(bnp1, naam))
nrow(bnp1)/nrow(distinct(bnp1, naam))
# calculate participation percentage
round(nrow(distinct(bnp1, naam))/nrow(distinct(filter(df.gp, praktijkcode ==1))), 2)

# comparison with global participation percentage


# Step 1. create the interval period indicating the interval between dmp start and current date
startdate <- ymd("2020-01-01")
enddate <- ymd(Sys.Date())
timespan <- interval(start = startdate, end = Sys.Date())
interval.m <- time_length(timespan2, unit ="month")

# Step 2. create arguments defining the total number of gp's, median tests per gp and participation percentage
