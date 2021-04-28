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
interval.m <- time_length(timespan, unit ="month")

# Step 2. create arguments defining the total number of gp's in the program and for each practice
total.gp <- nrow(distinct(df.gp))

total.gp1 <- nrow(distinct(filter(df.gp, praktijkcode ==1)))
total.gp2 <- nrow(distinct(filter(df.gp, praktijkcode ==2)))
total.gp3 <- nrow(distinct(filter(df.gp, praktijkcode ==3)))
total.gp4 <- nrow(distinct(filter(df.gp, praktijkcode ==4)))
total.gp5 <- nrow(distinct(filter(df.gp, praktijkcode ==5)))
total.gp6 <- nrow(distinct(filter(df.gp, praktijkcode ==6)))
total.gp7 <- nrow(distinct(filter(df.gp, praktijkcode ==7)))
total.gp8 <- nrow(distinct(filter(df.gp, praktijkcode ==8)))
total.gp9 <- nrow(distinct(filter(df.gp, praktijkcode ==9)))
total.gp10 <- nrow(distinct(filter(df.gp, praktijkcode ==10)))
total.gp11 <- nrow(distinct(filter(df.gp, praktijkcode ==11)))
total.gp12 <- nrow(distinct(filter(df.gp, praktijkcode ==12)))
total.gp13 <- nrow(distinct(filter(df.gp, praktijkcode ==13)))
total.gp14 <- nrow(distinct(filter(df.gp, praktijkcode ==14)))
total.gp15 <- nrow(distinct(filter(df.gp, praktijkcode ==15)))
total.gp16 <- nrow(distinct(filter(df.gp, praktijkcode ==16)))
total.gp17 <- nrow(distinct(filter(df.gp, praktijkcode ==17)))
total.gp18 <- nrow(distinct(filter(df.gp, praktijkcode ==18)))

# Step 3. calculate participation degree for the entire gp population
unique.gp.bnp <- nrow(distinct(as.tibble(df.bnp$naam)))
participation.total.bnp <- round(unique.gp.bnp/total.gp, 3)*100

# Step 4. calculate median number of tests for the entire population
df.bnp4 <- df.bnp%>%
  group_by(naam)%>%
  summarize(aantal = n())

median.total <- median(df.bnp4$aantal)

# generate a plot highlighting the number of tests per practice
