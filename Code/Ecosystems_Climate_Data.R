#######################################################
########  Ecosystems Climate Data (Bioclimatics)  #####
#######################################################

# Adapted from Step9a_Ecosystems Climate Data - CMIP6Updated.R by Annie Kellner 6/13/23

# Objects from Step3.Rmd needed to run this script include 
  #AllDays (list), model, AFB_Name, and start_year and end_year for the historical and future time periods

conflicts_prefer(month::lubridate) # set conflict preferences

# --------------------  Begin  --------------------------------- #

# Create function for adding month (e.g., "Jan") to dataframe

add_month <- function(df){
  df = df %>%
    mutate(month = month(df$date, label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME")))
}

# loop to add month to dataframes

for(i in 1:length(AllDays)){
  AllDays[[i]] = add_month(AllDays[[i]])
  AllDays[[i]]$month = as.character(AllDays[[i]]$month)
}

# Divide list into separate dataframes

dat.hist <- AllDays[[1]]
dat.RCP4.5.2030 <- AllDays[[2]]
dat.RCP4.5.2050 <- AllDays[[3]]
dat.RCP8.5.2030 <- AllDays[[4]]
dat.RCP8.5.2050 <- AllDays[[5]]


###HISTORICAL###

#setting aside appropriate data

hist.temp.av <- dat.hist %>%
  select(TMeanF, month)

hist.temp.hi <- dat.hist %>%
  select(TMaxF, month)

hist.temp.lo <- dat.hist %>%
  select(TMinF, month)

hist.precip <-  dat.hist %>%
  select(PPT_in, month)

#subsetting monthly data

jan.dat <- subset(dat.hist, month=="Jan")
feb.dat <- subset(dat.hist, month=="Feb")
mar.dat <- subset(dat.hist, month=="Mar")
apr.dat <- subset(dat.hist, month=="Apr")
may.dat <- subset(dat.hist, month=="May")
jun.dat <- subset(dat.hist, month=="Jun")
jul.dat <- subset(dat.hist, month=="Jul")
aug.dat <- subset(dat.hist, month=="Aug")
sep.dat <- subset(dat.hist, month=="Sep")
oct.dat <- subset(dat.hist, month=="Oct")
nov.dat <- subset(dat.hist, month=="Nov") #, na.rm=TRUE #November is sometimes wonky
dec.dat <- subset(dat.hist, month=="Dec")

#subsetting monthly tav

jan.tav <- mean(jan.dat$TMeanF)
feb.tav <- mean(feb.dat$TMeanF)
mar.tav <- mean(mar.dat$TMeanF)
apr.tav <- mean(apr.dat$TMeanF)
may.tav <- mean(may.dat$TMeanF)
jun.tav <- mean(jun.dat$TMeanF)
jul.tav <- mean(jul.dat$TMeanF)
aug.tav <- mean(aug.dat$TMeanF)
sep.tav <- mean(sep.dat$TMeanF)
oct.tav <- mean(oct.dat$TMeanF)
nov.tav <- mean(nov.dat$TMeanF)
dec.tav <- mean(dec.dat$TMeanF)

#subsetting different quarters

q1.dat <- rbind(jan.dat, feb.dat, mar.dat)
q2.dat <- rbind(feb.dat, mar.dat, apr.dat)
q3.dat <- rbind(mar.dat, apr.dat, may.dat)
q4.dat <- rbind(apr.dat, may.dat, jun.dat)
q5.dat <- rbind(may.dat, jun.dat, jul.dat)
q6.dat <- rbind(jun.dat, jul.dat, aug.dat)
q7.dat <- rbind(jul.dat, aug.dat, sep.dat)
q8.dat <- rbind(aug.dat, sep.dat, oct.dat)
q9.dat <- rbind(sep.dat, oct.dat, nov.dat)
q10.dat <- rbind(oct.dat, nov.dat, dec.dat)
q11.dat <- rbind(nov.dat, dec.dat, jan.dat)
q12.dat <- rbind(dec.dat, jan.dat, feb.dat)

#subsetting precipitation data

jan.prav <- (sum(jan.dat$PPT_in))/(baseline_end_year - baseline_start_year)
feb.prav <- (sum(feb.dat$PPT_in))/(baseline_end_year - baseline_start_year)
mar.prav <- (sum(mar.dat$PPT_in))/(baseline_end_year - baseline_start_year)
apr.prav <- (sum(apr.dat$PPT_in))/(baseline_end_year - baseline_start_year)
may.prav <- (sum(may.dat$PPT_in))/(baseline_end_year - baseline_start_year)
jun.prav <- (sum(jun.dat$PPT_in))/(baseline_end_year - baseline_start_year)
jul.prav <- (sum(jul.dat$PPT_in))/(baseline_end_year - baseline_start_year)
aug.prav <- (sum(aug.dat$PPT_in))/(baseline_end_year - baseline_start_year)
sep.prav <- (sum(sep.dat$PPT_in))/(baseline_end_year - baseline_start_year)
oct.prav <- (sum(oct.dat$PPT_in))/(baseline_end_year - baseline_start_year)
nov.prav <- (sum(nov.dat$PPT_in))/(baseline_end_year - baseline_start_year)
dec.prav <- (sum(dec.dat$PPT_in))/(baseline_end_year - baseline_start_year)

## 1) Annual Mean Diurnal Range
#difference between month's max and min temp averaged over 12 months
#SUMof(Tmax-Tmin)/12

jan.max <- mean(jan.dat$TMaxF)
jan.min <- mean(jan.dat$TMinF)

feb.max <- mean(feb.dat$TMaxF)
feb.min <- mean(feb.dat$TMinF)

mar.max <- mean(mar.dat$TMaxF)
mar.min <- mean(mar.dat$TMinF)

apr.max <- mean(apr.dat$TMaxF)
apr.min <- mean(apr.dat$TMinF)

may.max <- mean(may.dat$TMaxF)
may.min <- mean(may.dat$TMinF)

jun.max <- mean(jun.dat$TMaxF)
jun.min <- mean(jun.dat$TMinF)

jul.max <- mean(jul.dat$TMaxF)
jul.min <- mean(jul.dat$TMinF)

aug.max <- mean(aug.dat$TMaxF)
aug.min <- mean(aug.dat$TMinF)

sep.max <- mean(sep.dat$TMaxF)
sep.min <- mean(sep.dat$TMinF)

oct.max <- mean(oct.dat$TMaxF)
oct.min <- mean(oct.dat$TMinF)

nov.max <- mean(nov.dat$TMaxF)
nov.min <- mean(nov.dat$TMinF)

dec.max <- mean(dec.dat$TMaxF)
dec.min <- mean(dec.dat$TMinF)

#differences
jan.dif <- jan.max-jan.min
feb.dif <- feb.max-feb.min
mar.dif <- mar.max-mar.min
apr.dif <- apr.max-apr.min
may.dif <- may.max-may.min
jun.dif <- jun.max-jun.min
jul.dif <- jul.max-jul.min
aug.dif <- aug.max-aug.min
sep.dif <- sep.max-sep.min
oct.dif <- oct.max-oct.min
nov.dif <- nov.max-nov.min
dec.dif <- dec.max-dec.min

#sum of all months annual mean diurnal range
amdr.sum <- sum(jan.dif, feb.dif, mar.dif, apr.dif, may.dif, jun.dif, 
                jul.dif, aug.dif, sep.dif, oct.dif, nov.dif, dec.dif)

#divide by 12 to get one months value
annual.mean.diurnal.range <- (amdr.sum/12)

#value

annual.mean.diurnal.range

##Isothermality
#units in percent
#how large day to night temperatures oscilate relative to the summer to winter oscillations
#ratio of mean diurnal range to the annual temperature range
#(annual.mean.diurnal.range/annual.temp.range)*100

#conversion formula f to c
f.to.c <- function(temp) {
  celsius <- ((temp -32) *(5/9))
  return(celsius)
}

#absolute max and min and converting to celsius
abs.tmax.f <- max(dat.hist$TMaxF)
abs.max.c <- f.to.c(abs.tmax.f)
abs.tmin.f <- min(dat.hist$TMinF)
abs.tmin.c <- f.to.c(abs.tmin.f)

#annual temp range
annual.temp.range.c <- abs.max.c - abs.tmin.c

isothermality <- (annual.mean.diurnal.range/annual.temp.range.c)*100

#value
isothermality

##Temperature Seasonality SD
#amount of temperature variation over a given year (or averaged years) based on SD of monthly
#temp averages
#SD(Tav1,2,3...)
monthly.means <- data.frame(c(jan.tav, feb.tav, mar.tav, apr.tav, may.tav, jun.tav,
                              jul.tav, aug.tav, sep.tav, oct.tav, nov.tav, dec.tav))
#assign header to column
names(monthly.means)[1]<-paste("faverage")

#convert it to celsius
monthly.means.c <- f.to.c(monthly.means)

#assign header to column
names(monthly.means.c)[1]<-paste("caverage")

#now get the standard deviation of all twelve months
sd.temperature.seasonality <- sd(monthly.means.c$caverage)

#value
sd.temperature.seasonality

##Temperature Seasonality (CV)
#SD(TKav1-12)/annual mean temp in K
#need to use Kelvin for this one so lets create a function

f.to.k <- function(temp) {
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}

#function created, lets get Kelvin values for each month
jan.k <- f.to.k(jan.tav)
feb.k <- f.to.k(feb.tav)
mar.k <- f.to.k(mar.tav)
apr.k <- f.to.k(apr.tav)
may.k <- f.to.k(may.tav)
jun.k <- f.to.k(jun.tav)
jul.k <- f.to.k(jul.tav)
aug.k <- f.to.k(aug.tav)
sep.k <- f.to.k(sep.tav)
oct.k <- f.to.k(oct.tav)
nov.k <- f.to.k(nov.tav)
dec.k <- f.to.k(dec.tav)

#create a data frame

monthly.means.k <- data.frame(c(jan.k, feb.k, mar.k, apr.k, may.k, jun.k,
                                jul.k, aug.k, sep.k, oct.k, nov.k, dec.k))

#assign header to column
names(monthly.means.k)[1]<-paste("kaverage")

sd.mmk <- sd(monthly.means.k$kaverage)

#determine annual mean temp
annual.av.f <- mean(dat.hist$TMeanF)

#convert it to Kelvin
annual.av.k. <- f.to.k(annual.av.f)

##Temperature Seasonality CV
cv.temperature.seasonality <- ((sd.mmk)/annual.av.k.)*100

#value
cv.temperature.seasonality

##Max Temperature of Warmest Month
#lets first figure out the warmest month
#saving it in case we need it
warmest.day.warmest.month <- c(max(jan.dat$TMaxF), max(feb.dat$TMaxF), max(mar.dat$TMaxF), max(apr.dat$TMaxF),
                               max(may.dat$TMaxF), max(jun.dat$TMaxF), max(jul.dat$TMaxF), max(aug.dat$TMaxF),
                               max(sep.dat$TMaxF), max(oct.dat$TMaxF), max(nov.dat$TMaxF), max(dec.dat$TMaxF)
)[which.max(c(mean(jan.dat$TMaxF), mean(feb.dat$TMaxF), mean(mar.dat$TMaxF), mean(apr.dat$TMaxF), 
              mean(may.dat$TMaxF), mean(jun.dat$TMaxF), mean(jul.dat$TMaxF), mean(aug.dat$TMaxF), 
              mean(sep.dat$TMaxF), mean(oct.dat$TMaxF), mean(nov.dat$TMaxF), mean(dec.dat$TMaxF)))]

#value
warmest.day.warmest.month

##Min Temperature of Coldest Month
#saving it in case they want it
coldest.day.coldest.month <- c(min(jan.dat$TMinF), min(feb.dat$TMinF), min(mar.dat$TMinF), min(apr.dat$TMinF),
                               min(may.dat$TMinF), min(jun.dat$TMinF), min(jul.dat$TMinF), min(aug.dat$TMinF),
                               min(sep.dat$TMinF), min(oct.dat$TMinF), min(nov.dat$TMinF), min(dec.dat$TMinF)
)[which.min(c(mean(jan.dat$TMinF), mean(feb.dat$TMinF), mean(mar.dat$TMinF), mean(apr.dat$TMinF), 
              mean(may.dat$TMinF), mean(jun.dat$TMinF), mean(jul.dat$TMinF), mean(aug.dat$TMinF), 
              mean(sep.dat$TMinF), mean(oct.dat$TMinF), mean(nov.dat$TMinF), mean(dec.dat$TMinF)))]

#value
coldest.day.coldest.month

##Annual Temperature Range
#hottest day of hottest month minus coldest day of coldest month

annual.temperature.range <- warmest.day.warmest.month - coldest.day.coldest.month

#value
annual.temperature.range

##Mean Temperature of Wettest Quarter

mean.temp.wettest.quarter <- c(mean(q1.dat$TMeanF), mean(q2.dat$TMeanF), mean(q3.dat$TMeanF), mean(q4.dat$TMeanF),
                               mean(q5.dat$TMeanF), mean(q6.dat$TMeanF), mean(q7.dat$TMeanF), mean(q8.dat$TMeanF),
                               mean(q9.dat$TMeanF), mean(q10.dat$TMeanF), mean(q11.dat$TMeanF), mean(q12.dat$TMeanF)
)[which.max(c(mean(q1.dat$PPT_in)*90.25, mean(q2.dat$PPT_in)*89.25, mean(q3.dat$PPT_in)*92, mean(q4.dat$PPT_in)*91, 
              mean(q5.dat$PPT_in)*92, mean(q6.dat$PPT_in)*92, mean(q7.dat$PPT_in)*92, mean(q8.dat$PPT_in)*92, 
              mean(q9.dat$PPT_in)*91, mean(q10.dat$PPT_in)*92, mean(q11.dat$PPT_in)*92, mean(q12.dat$PPT_in)*90.25))]

#value
mean.temp.wettest.quarter

##Mean Temperature of Driest Quarter

mean.temp.driest.quarter <- c(mean(q1.dat$TMeanF), mean(q2.dat$TMeanF), mean(q3.dat$TMeanF), mean(q4.dat$TMeanF),
                              mean(q5.dat$TMeanF), mean(q6.dat$TMeanF), mean(q7.dat$TMeanF), mean(q8.dat$TMeanF),
                              mean(q9.dat$TMeanF), mean(q10.dat$TMeanF), mean(q11.dat$TMeanF), mean(q12.dat$TMeanF)
)[which.min(c(mean(q1.dat$PPT_in)*90.25, mean(q2.dat$PPT_in)*89.25, mean(q3.dat$PPT_in)*92, mean(q4.dat$PPT_in)*91, 
              mean(q5.dat$PPT_in)*92, mean(q6.dat$PPT_in)*92, mean(q7.dat$PPT_in)*92, mean(q8.dat$PPT_in)*92, 
              mean(q9.dat$PPT_in)*91, mean(q10.dat$PPT_in)*92, mean(q11.dat$PPT_in)*92, mean(q12.dat$PPT_in)*90.25))]

#value
mean.temp.driest.quarter

##Mean Temperature of Warmest Quarter

mean.temp.warmest.quarter <- c(mean(q1.dat$TMeanF), mean(q2.dat$TMeanF), mean(q3.dat$TMeanF), mean(q4.dat$TMeanF),
                               mean(q5.dat$TMeanF), mean(q6.dat$TMeanF), mean(q7.dat$TMeanF), mean(q8.dat$TMeanF),
                               mean(q9.dat$TMeanF), mean(q10.dat$TMeanF), mean(q11.dat$TMeanF), mean(q12.dat$TMeanF)
)[which.max(c(mean(q1.dat$TMeanF), mean(q2.dat$TMeanF), mean(q3.dat$TMeanF), mean(q4.dat$TMeanF), 
              mean(q5.dat$TMeanF), mean(q6.dat$TMeanF), mean(q7.dat$TMeanF), mean(q8.dat$TMeanF), 
              mean(q9.dat$TMeanF), mean(q10.dat$TMeanF), mean(q11.dat$TMeanF), mean(q12.dat$TMeanF)))]

#value
mean.temp.warmest.quarter

##Mean Temperature of Coldest Quarter

mean.temp.coldest.quarter <- c(mean(q1.dat$TMeanF), mean(q2.dat$TMeanF), mean(q3.dat$TMeanF), mean(q4.dat$TMeanF),
                               mean(q5.dat$TMeanF), mean(q6.dat$TMeanF), mean(q7.dat$TMeanF), mean(q8.dat$TMeanF),
                               mean(q9.dat$TMeanF), mean(q10.dat$TMeanF), mean(q11.dat$TMeanF), mean(q12.dat$TMeanF)
)[which.min(c(mean(q1.dat$TMeanF), mean(q2.dat$TMeanF), mean(q3.dat$TMeanF), mean(q4.dat$TMeanF), 
              mean(q5.dat$TMeanF), mean(q6.dat$TMeanF), mean(q7.dat$TMeanF), mean(q8.dat$TMeanF), 
              mean(q9.dat$TMeanF), mean(q10.dat$TMeanF), mean(q11.dat$TMeanF), mean(q12.dat$TMeanF)))]

#value
mean.temp.coldest.quarter

##Precipitation of Wettest Month

precip.wettest.month.pre <- c(jan.prav, feb.prav, mar.prav, apr.prav, may.prav, jun.prav,
                              jul.prav, aug.prav, sep.prav, oct.prav, nov.prav, dec.prav
)[which.max(c(jan.prav, feb.prav, mar.prav, apr.prav, may.prav, jun.prav,
              jul.prav, aug.prav, sep.prav, oct.prav, nov.prav, dec.prav))]



precip.wettest.month <- precip.wettest.month.pre

#value
precip.wettest.month

##Precipitation of Driest Month

precip.driest.month.pre <- c(jan.prav, feb.prav, mar.prav, apr.prav, may.prav, jun.prav,
                             jul.prav, aug.prav, sep.prav, oct.prav, nov.prav, dec.prav
)[which.min(c(jan.prav, feb.prav, mar.prav, apr.prav, may.prav, jun.prav,
              jul.prav, aug.prav, sep.prav, oct.prav, nov.prav, dec.prav))]

#since we are using sums of precip for a given month over a historical period we need to divide
#by the number of years in the period to get the sum of one month's precip

precip.driest.month <- precip.driest.month.pre

#value
precip.driest.month

##Precipitation Seasonality (CV)
#SD(precip1-12)/1+average monthly precip   X 100

#SD(Tav1,2,3...)
monthly.means.precip <- data.frame(c(jan.prav, feb.prav, mar.prav, apr.prav, may.prav, jun.prav,
                                     jul.prav, aug.prav, sep.prav, oct.prav, nov.prav, dec.prav))
#assign header to column
names(monthly.means.precip)[1]<-paste("praverage")

#now get the standard deviation of all twelve months
sd.precip <- sd(monthly.means.precip$praverage)

#annual precipitation / getting the sum of over historical period then dividing by the number of years to get annual average
an.precip <- (sum(dat.hist$PPT_mm)/(baseline_end_year - baseline_start_year))
mo.precip <- an.precip/12

#final calculation
precip.seasonality.cv <- (sd.precip/(1+mo.precip))*100

#value
precip.seasonality.cv

##Precipitation of Wettest Quarter

precip.wettest.quarter.pre <- c(sum(q1.dat$PPT_in), sum(q2.dat$PPT_in), sum(q3.dat$PPT_in), sum(q4.dat$PPT_in),
                                sum(q5.dat$PPT_in), sum(q6.dat$PPT_in), sum(q7.dat$PPT_in), sum(q8.dat$PPT_in),
                                sum(q9.dat$PPT_in), sum(q10.dat$PPT_in), sum(q11.dat$PPT_in), sum(q12.dat$PPT_in)
)[which.max(c(mean(q1.dat$PPT_in)*90.25, mean(q2.dat$PPT_in)*89.25, mean(q3.dat$PPT_in)*92, mean(q4.dat$PPT_in)*91, 
              mean(q5.dat$PPT_in)*92, mean(q6.dat$PPT_in)*92, mean(q7.dat$PPT_in)*92, mean(q8.dat$PPT_in)*92, 
              mean(q9.dat$PPT_in)*91, mean(q10.dat$PPT_in)*92, mean(q11.dat$PPT_in)*92, mean(q12.dat$PPT_in)*90.25))]

#previous calculation is the sum over historical period of said quarter
#divide to get the annual quarter calculation 
precip.wettest.quarter <- precip.wettest.quarter.pre/(baseline_end_year - baseline_start_year)
                                                      

#value
precip.wettest.quarter

##Precipitation of Driest Quarter

precip.driest.quarter.pre <- c(sum(q1.dat$PPT_in), sum(q2.dat$PPT_in), sum(q3.dat$PPT_in), sum(q4.dat$PPT_in),
                               sum(q5.dat$PPT_in), sum(q6.dat$PPT_in), sum(q7.dat$PPT_in), sum(q8.dat$PPT_in),
                               sum(q9.dat$PPT_in), sum(q10.dat$PPT_in), sum(q11.dat$PPT_in), sum(q12.dat$PPT_in)
)[which.min(c(mean(q1.dat$PPT_in)*90.25, mean(q2.dat$PPT_in)*89.25, mean(q3.dat$PPT_in)*92, mean(q4.dat$PPT_in)*91, 
              mean(q5.dat$PPT_in)*92, mean(q6.dat$PPT_in)*92, mean(q7.dat$PPT_in)*92, mean(q8.dat$PPT_in)*92, 
              mean(q9.dat$PPT_in)*91, mean(q10.dat$PPT_in)*92, mean(q11.dat$PPT_in)*92, mean(q12.dat$PPT_in)*90.25))]

#previous calculation is the sum over historical period of said quarter
#divide by number of years to get the annual quarter calculation 
precip.driest.quarter <- precip.driest.quarter.pre/(baseline_end_year - baseline_start_year)

#value
precip.driest.quarter

##Precipitation of Coldest Quarter

precip.coldest.quarter.pre <- c(sum(q1.dat$PPT_in), sum(q2.dat$PPT_in), sum(q3.dat$PPT_in), sum(q4.dat$PPT_in),
                                sum(q5.dat$PPT_in), sum(q6.dat$PPT_in), sum(q7.dat$PPT_in), sum(q8.dat$PPT_in),
                                sum(q9.dat$PPT_in), sum(q10.dat$PPT_in), sum(q11.dat$PPT_in), sum(q12.dat$PPT_in)
)[which.min(c(mean(q1.dat$TMeanF), mean(q2.dat$TMeanF), mean(q3.dat$TMeanF), mean(q4.dat$TMeanF), 
              mean(q5.dat$TMeanF), mean(q6.dat$TMeanF), mean(q7.dat$TMeanF), mean(q8.dat$TMeanF), 
              mean(q9.dat$TMeanF), mean(q10.dat$TMeanF), mean(q11.dat$TMeanF), mean(q12.dat$TMeanF)))]

#previous calculation is the sum over the historical period of said quarter
#divide by number of years to get the annual quarter calculation 
precip.coldest.quarter <- precip.coldest.quarter.pre/(baseline_end_year - baseline_start_year)

#value
precip.coldest.quarter

##Precipitation of the Warmest Quarter

precip.warmest.quarter.pre <- c(sum(q1.dat$PPT_in), sum(q2.dat$PPT_in), sum(q3.dat$PPT_in), sum(q4.dat$PPT_in),
                                sum(q5.dat$PPT_in), sum(q6.dat$PPT_in), sum(q7.dat$PPT_in), sum(q8.dat$PPT_in),
                                sum(q9.dat$PPT_in), sum(q10.dat$PPT_in), sum(q11.dat$PPT_in), sum(q12.dat$PPT_in)
)[which.max(c(mean(q1.dat$TMeanF), mean(q2.dat$TMeanF), mean(q3.dat$TMeanF), mean(q4.dat$TMeanF), 
              mean(q5.dat$TMeanF), mean(q6.dat$TMeanF), mean(q7.dat$TMeanF), mean(q8.dat$TMeanF), 
              mean(q9.dat$TMeanF), mean(q10.dat$TMeanF), mean(q11.dat$TMeanF), mean(q12.dat$TMeanF)))]

#previous calculation is the sum over historical period of said quarter
#divide to get the annual quarter calculation 
precip.warmest.quarter <- precip.warmest.quarter.pre/(baseline_end_year - baseline_start_year)


#value
precip.warmest.quarter

#####RCP 4.5 2030

#setting aside appropriate data

sc1.temp.av <- c(dat.RCP4.5.2030$TMeanF)
sc1.temp.hi <- c(dat.RCP4.5.2030$TMaxF)
sc1.temp.lo <- c(dat.RCP4.5.2030$TMinF)
sc1.precip <-  c(dat.RCP4.5.2030$PPT_in)

#subsetting monthly data

jan.dat.sc1 <- subset(dat.RCP4.5.2030, month=="Jan")
feb.dat.sc1 <- subset(dat.RCP4.5.2030, month=="Feb")
mar.dat.sc1 <- subset(dat.RCP4.5.2030, month=="Mar")
apr.dat.sc1 <- subset(dat.RCP4.5.2030, month=="Apr")
may.dat.sc1 <- subset(dat.RCP4.5.2030, month=="May")
jun.dat.sc1 <- subset(dat.RCP4.5.2030, month=="Jun")
jul.dat.sc1 <- subset(dat.RCP4.5.2030, month=="Jul")
aug.dat.sc1 <- subset(dat.RCP4.5.2030, month=="Aug")
sep.dat.sc1 <- subset(dat.RCP4.5.2030, month=="Sep")
oct.dat.sc1 <- subset(dat.RCP4.5.2030, month=="Oct")
nov.dat.sc1 <- subset(dat.RCP4.5.2030, month=="Nov") #, na.rm=TRUE #November is sometimes wonky
dec.dat.sc1 <- subset(dat.RCP4.5.2030, month=="Dec")

#subsetting monthly tav

jan.tav.sc1 <- mean(jan.dat.sc1$TMeanF)
feb.tav.sc1 <- mean(feb.dat.sc1$TMeanF)
mar.tav.sc1 <- mean(mar.dat.sc1$TMeanF)
apr.tav.sc1 <- mean(apr.dat.sc1$TMeanF)
may.tav.sc1 <- mean(may.dat.sc1$TMeanF)
jun.tav.sc1 <- mean(jun.dat.sc1$TMeanF)
jul.tav.sc1 <- mean(jul.dat.sc1$TMeanF)
aug.tav.sc1 <- mean(aug.dat.sc1$TMeanF)
sep.tav.sc1 <- mean(sep.dat.sc1$TMeanF)
oct.tav.sc1 <- mean(oct.dat.sc1$TMeanF)
nov.tav.sc1 <- mean(nov.dat.sc1$TMeanF)
dec.tav.sc1 <- mean(dec.dat.sc1$TMeanF)

#subsetting different quarters

q1.dat.sc1 <- rbind(jan.dat.sc1, feb.dat.sc1, mar.dat.sc1)
q2.dat.sc1 <- rbind(feb.dat.sc1, mar.dat.sc1, apr.dat.sc1)
q3.dat.sc1 <- rbind(mar.dat.sc1, apr.dat.sc1, may.dat.sc1)
q4.dat.sc1 <- rbind(apr.dat.sc1, may.dat.sc1, jun.dat.sc1)
q5.dat.sc1 <- rbind(may.dat.sc1, jun.dat.sc1, jul.dat.sc1)
q6.dat.sc1 <- rbind(jun.dat.sc1, jul.dat.sc1, aug.dat.sc1)
q7.dat.sc1 <- rbind(jul.dat.sc1, aug.dat.sc1, sep.dat.sc1)
q8.dat.sc1 <- rbind(aug.dat.sc1, sep.dat.sc1, oct.dat.sc1)
q9.dat.sc1 <- rbind(sep.dat.sc1, oct.dat.sc1, nov.dat.sc1)
q10.dat.sc1 <- rbind(oct.dat.sc1, nov.dat.sc1, dec.dat.sc1)
q11.dat.sc1 <- rbind(nov.dat.sc1, dec.dat.sc1, jan.dat.sc1)
q12.dat.sc1 <- rbind(dec.dat.sc1, jan.dat.sc1, feb.dat.sc1)

#subsetting precipitation data

jan.prav.dat.sc1 <- (sum(jan.dat.sc1$PPT_in))/(future1_end_year - future1_start_year)
feb.prav.dat.sc1 <- (sum(feb.dat.sc1$PPT_in))/(future1_end_year - future1_start_year)
mar.prav.dat.sc1 <- (sum(mar.dat.sc1$PPT_in))/(future1_end_year - future1_start_year)
apr.prav.dat.sc1 <- (sum(apr.dat.sc1$PPT_in))/(future1_end_year - future1_start_year)
may.prav.dat.sc1 <- (sum(may.dat.sc1$PPT_in))/(future1_end_year - future1_start_year)
jun.prav.dat.sc1 <- (sum(jun.dat.sc1$PPT_in))/(future1_end_year - future1_start_year)
jul.prav.dat.sc1 <- (sum(jul.dat.sc1$PPT_in))/(future1_end_year - future1_start_year)
aug.prav.dat.sc1 <- (sum(aug.dat.sc1$PPT_in))/(future1_end_year - future1_start_year)
sep.prav.dat.sc1 <- (sum(sep.dat.sc1$PPT_in))/(future1_end_year - future1_start_year)
oct.prav.dat.sc1 <- (sum(oct.dat.sc1$PPT_in))/(future1_end_year - future1_start_year)
nov.prav.dat.sc1 <- (sum(nov.dat.sc1$PPT_in))/(future1_end_year - future1_start_year)
dec.prav.dat.sc1 <- (sum(dec.dat.sc1$PPT_in))/(future1_end_year - future1_start_year)

## 1) Annual Mean Diurnal Range
#difference between month's max and min temp averaged over 12 months
#SUMof(Tmax-Tmin)/12

jan.max.sc1 <- mean(jan.dat.sc1$TMaxF)
jan.min.sc1 <- mean(jan.dat.sc1$TMinF)

feb.max.sc1 <- mean(feb.dat.sc1$TMaxF)
feb.min.sc1 <- mean(feb.dat.sc1$TMinF)

mar.max.sc1 <- mean(mar.dat.sc1$TMaxF)
mar.min.sc1 <- mean(mar.dat.sc1$TMinF)

apr.max.sc1 <- mean(apr.dat.sc1$TMaxF)
apr.min.sc1 <- mean(apr.dat.sc1$TMinF)

may.max.sc1 <- mean(may.dat.sc1$TMaxF)
may.min.sc1 <- mean(may.dat.sc1$TMinF)

jun.max.sc1 <- mean(jun.dat.sc1$TMaxF)
jun.min.sc1 <- mean(jun.dat.sc1$TMinF)

jul.max.sc1 <- mean(jul.dat.sc1$TMaxF)
jul.min.sc1 <- mean(jul.dat.sc1$TMinF)

aug.max.sc1 <- mean(aug.dat.sc1$TMaxF)
aug.min.sc1 <- mean(aug.dat.sc1$TMinF)

sep.max.sc1 <- mean(sep.dat.sc1$TMaxF)
sep.min.sc1 <- mean(sep.dat.sc1$TMinF)

oct.max.sc1 <- mean(oct.dat.sc1$TMaxF)
oct.min.sc1 <- mean(oct.dat.sc1$TMinF)

nov.max.sc1 <- mean(nov.dat.sc1$TMaxF)
nov.min.sc1 <- mean(nov.dat.sc1$TMinF)

dec.max.sc1 <- mean(dec.dat.sc1$TMaxF)
dec.min.sc1 <- mean(dec.dat.sc1$TMinF)

#differences
jan.dif.sc1 <- jan.max.sc1-jan.min.sc1
feb.dif.sc1 <- feb.max.sc1-feb.min.sc1
mar.dif.sc1 <- mar.max.sc1-mar.min.sc1
apr.dif.sc1 <- apr.max.sc1-apr.min.sc1
may.dif.sc1 <- may.max.sc1-may.min.sc1
jun.dif.sc1 <- jun.max.sc1-jun.min.sc1
jul.dif.sc1 <- jul.max.sc1-jul.min.sc1
aug.dif.sc1 <- aug.max.sc1-aug.min.sc1
sep.dif.sc1 <- sep.max.sc1-sep.min.sc1
oct.dif.sc1 <- oct.max.sc1-oct.min.sc1
nov.dif.sc1 <- nov.max.sc1-nov.min.sc1
dec.dif.sc1 <- dec.max.sc1-dec.min.sc1

#sum of all months annual mean diurnal range
amdr.sum.sc1 <- sum(jan.dif.sc1, feb.dif.sc1, mar.dif.sc1, apr.dif.sc1, may.dif.sc1, jun.dif.sc1, 
                    jul.dif.sc1, aug.dif.sc1, sep.dif.sc1, oct.dif.sc1, nov.dif.sc1, dec.dif.sc1)

#divide by 12 to get one months value
annual.mean.diurnal.range.sc1 <- (amdr.sum.sc1/12)

#value

annual.mean.diurnal.range.sc1

##Isothermality
#units in percent
#how large day to night temperatures oscilate relative to the summer to winter oscillations
#ratio of mean diurnal range to the annual temperature range
#(annual.mean.diurnal.range/annual.temp.range)*100

#conversion formula f to c
f.to.c <- function(temp) {
  celsius <- ((temp -32) *(5/9))
  return(celsius)
}

#absolute max and min
abs.tmax.pre.sc1 <- max(dat.RCP4.5.2030$TMaxF)
abs.tmax.c.sc1 <- f.to.c(abs.tmax.pre.sc1)
abs.tmin.pre.sc1 <- min(dat.RCP4.5.2030$TMinF)
abs.tmin.c.sc1 <- f.to.c(abs.tmin.pre.sc1)


#annual temp range
annual.temp.range.sc1 <- abs.tmax.c.sc1 - abs.tmin.c.sc1

isothermality.sc1 <- (annual.mean.diurnal.range.sc1/annual.temp.range.sc1)*100

#value
isothermality.sc1

##Temperature Seasonality SD
#amount of temperature variation over a given year (or averaged years) based on SD of monthly
#temp averages
#SD(Tav1,2,3...)
monthly.means.sc1 <- data.frame(c(jan.tav.sc1, feb.tav.sc1, mar.tav.sc1, apr.tav.sc1, may.tav.sc1, jun.tav.sc1,
                                  jul.tav.sc1, aug.tav.sc1, sep.tav.sc1, oct.tav.sc1, nov.tav.sc1, dec.tav.sc1))
#assign header to column
names(monthly.means.sc1)[1]<-paste("faverage")

#convert it to celsius
monthly.means.c.sc1 <- f.to.c(monthly.means.sc1)

#assign header to column
names(monthly.means.c.sc1)[1]<-paste("caverage")

#now get the standard deviation of all twelve months
sd.temperature.seasonality.sc1 <- sd(monthly.means.c.sc1$caverage)

#value
sd.temperature.seasonality.sc1

##Temperature Seasonality (CV)
#SD(TKav1-12)/annual mean temp in K
#need to use Kelvin for this one so lets create a function

f.to.k <- function(temp) {
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}

#function created, lets get Kelvin values for each month
jan.k.sc1 <- f.to.k(jan.tav.sc1)
feb.k.sc1 <- f.to.k(feb.tav.sc1)
mar.k.sc1 <- f.to.k(mar.tav.sc1)
apr.k.sc1 <- f.to.k(apr.tav.sc1)
may.k.sc1 <- f.to.k(may.tav.sc1)
jun.k.sc1 <- f.to.k(jun.tav.sc1)
jul.k.sc1 <- f.to.k(jul.tav.sc1)
aug.k.sc1 <- f.to.k(aug.tav.sc1)
sep.k.sc1 <- f.to.k(sep.tav.sc1)
oct.k.sc1 <- f.to.k(oct.tav.sc1)
nov.k.sc1 <- f.to.k(nov.tav.sc1)
dec.k.sc1 <- f.to.k(dec.tav.sc1)

#create a data frame

monthly.means.k.sc1 <- data.frame(c(jan.k.sc1, feb.k.sc1, mar.k.sc1, apr.k.sc1, may.k.sc1, jun.k.sc1,
                                    jul.k.sc1, aug.k.sc1, sep.k.sc1, oct.k.sc1, nov.k.sc1, dec.k.sc1))

#assign header to column
names(monthly.means.k.sc1)[1]<-paste("kaverage")

sd.mmk.sc1 <- sd(monthly.means.k.sc1$kaverage)

#determine annual mean temp
annual.av.f.sc1 <- mean(dat.RCP4.5.2030$TMeanF)

#convert it to Kelvin
annual.av.k.sc1. <- f.to.k(annual.av.f.sc1)

##Temperature Seasonality CV
cv.temperature.seasonality.sc1 <- ((sd.mmk.sc1)/annual.av.k.sc1.)*100

#value
cv.temperature.seasonality.sc1

##Max Temperature of Warmest Month
#lets first figure out the warmest month
#saving it in case we need it
warmest.day.warmest.month.sc1 <- c(max(jan.dat.sc1$TMaxF), max(feb.dat.sc1$TMaxF), max(mar.dat.sc1$TMaxF), max(apr.dat.sc1$TMaxF),
                                   max(may.dat.sc1$TMaxF), max(jun.dat.sc1$TMaxF), max(jul.dat.sc1$TMaxF), max(aug.dat.sc1$TMaxF),
                                   max(sep.dat.sc1$TMaxF), max(oct.dat.sc1$TMaxF), max(nov.dat.sc1$TMaxF), max(dec.dat.sc1$TMaxF)
)[which.max(c(mean(jan.dat.sc1$TMaxF), mean(feb.dat.sc1$TMaxF), mean(mar.dat.sc1$TMaxF), mean(apr.dat.sc1$TMaxF), 
              mean(may.dat.sc1$TMaxF), mean(jun.dat.sc1$TMaxF), mean(jul.dat.sc1$TMaxF), mean(aug.dat.sc1$TMaxF), 
              mean(sep.dat.sc1$TMaxF), mean(oct.dat.sc1$TMaxF), mean(nov.dat.sc1$TMaxF), mean(dec.dat.sc1$TMaxF)))]

#value
warmest.day.warmest.month.sc1

##Min Temperature of Coldest Month
#saving it in case they want it
coldest.day.coldest.month.sc1 <- c(min(jan.dat.sc1$TMinF), min(feb.dat.sc1$TMinF), min(mar.dat.sc1$TMinF), min(apr.dat.sc1$TMinF),
                                   min(may.dat.sc1$TMinF), min(jun.dat.sc1$TMinF), min(jul.dat.sc1$TMinF), min(aug.dat.sc1$TMinF),
                                   min(sep.dat.sc1$TMinF), min(oct.dat.sc1$TMinF), min(nov.dat.sc1$TMinF), min(dec.dat.sc1$TMinF)
)[which.min(c(mean(jan.dat.sc1$TMinF), mean(feb.dat.sc1$TMinF), mean(mar.dat.sc1$TMinF), mean(apr.dat.sc1$TMinF), 
              mean(may.dat.sc1$TMinF), mean(jun.dat.sc1$TMinF), mean(jul.dat.sc1$TMinF), mean(aug.dat.sc1$TMinF), 
              mean(sep.dat.sc1$TMinF), mean(oct.dat.sc1$TMinF), mean(nov.dat.sc1$TMinF), mean(dec.dat.sc1$TMinF)))]

#value
coldest.day.coldest.month.sc1

##Annual Temperature Range
#hottest day of hottest month minus coldest day of coldest month

annual.temperature.range.sc1 <- warmest.day.warmest.month.sc1 - coldest.day.coldest.month.sc1

#value
annual.temperature.range.sc1

##Mean Temperature of Wettest Quarter

mean.temp.wettest.quarter.sc1 <- c(mean(q1.dat.sc1$TMeanF), mean(q2.dat.sc1$TMeanF), mean(q3.dat.sc1$TMeanF), mean(q4.dat.sc1$TMeanF),
                                   mean(q5.dat.sc1$TMeanF), mean(q6.dat.sc1$TMeanF), mean(q7.dat.sc1$TMeanF), mean(q8.dat.sc1$TMeanF),
                                   mean(q9.dat.sc1$TMeanF), mean(q10.dat.sc1$TMeanF), mean(q11.dat.sc1$TMeanF), mean(q12.dat.sc1$TMeanF)
)[which.max(c(mean(q1.dat.sc1$PPT_in)*90.25, mean(q2.dat.sc1$PPT_in)*89.25, mean(q3.dat.sc1$PPT_in)*92, mean(q4.dat.sc1$PPT_in)*91, 
              mean(q5.dat.sc1$PPT_in)*92, mean(q6.dat.sc1$PPT_in)*92, mean(q7.dat.sc1$PPT_in)*92, mean(q8.dat.sc1$PPT_in)*92, 
              mean(q9.dat.sc1$PPT_in)*91, mean(q10.dat.sc1$PPT_in)*92, mean(q11.dat.sc1$PPT_in)*92, mean(q12.dat.sc1$PPT_in)*90.25))]

#value
mean.temp.wettest.quarter.sc1

##Mean Temperature of Driest Quarter

mean.temp.driest.quarter.sc1 <- c(mean(q1.dat.sc1$TMeanF), mean(q2.dat.sc1$TMeanF), mean(q3.dat.sc1$TMeanF), mean(q4.dat.sc1$TMeanF),
                                  mean(q5.dat.sc1$TMeanF), mean(q6.dat.sc1$TMeanF), mean(q7.dat.sc1$TMeanF), mean(q8.dat.sc1$TMeanF),
                                  mean(q9.dat.sc1$TMeanF), mean(q10.dat.sc1$TMeanF), mean(q11.dat.sc1$TMeanF), mean(q12.dat.sc1$TMeanF)
)[which.min(c(mean(q1.dat.sc1$PPT_in)*90.25, mean(q2.dat.sc1$PPT_in)*89.25, mean(q3.dat.sc1$PPT_in)*92, mean(q4.dat.sc1$PPT_in)*91, 
              mean(q5.dat.sc1$PPT_in)*92, mean(q6.dat.sc1$PPT_in)*92, mean(q7.dat.sc1$PPT_in)*92, mean(q8.dat.sc1$PPT_in)*92, 
              mean(q9.dat.sc1$PPT_in)*91, mean(q10.dat.sc1$PPT_in)*92, mean(q11.dat.sc1$PPT_in)*92, mean(q12.dat.sc1$PPT_in)*90.25))]

#value
mean.temp.driest.quarter.sc1

##Mean Temperature of Warmest Quarter

mean.temp.warmest.quarter.sc1 <- c(mean(q1.dat.sc1$TMeanF), mean(q2.dat.sc1$TMeanF), mean(q3.dat.sc1$TMeanF), mean(q4.dat.sc1$TMeanF),
                                   mean(q5.dat.sc1$TMeanF), mean(q6.dat.sc1$TMeanF), mean(q7.dat.sc1$TMeanF), mean(q8.dat.sc1$TMeanF),
                                   mean(q9.dat.sc1$TMeanF), mean(q10.dat.sc1$TMeanF), mean(q11.dat.sc1$TMeanF), mean(q12.dat.sc1$TMeanF)
)[which.max(c(mean(q1.dat.sc1$TMeanF), mean(q2.dat.sc1$TMeanF), mean(q3.dat.sc1$TMeanF), mean(q4.dat.sc1$TMeanF), 
              mean(q5.dat.sc1$TMeanF), mean(q6.dat.sc1$TMeanF), mean(q7.dat.sc1$TMeanF), mean(q8.dat.sc1$TMeanF), 
              mean(q9.dat.sc1$TMeanF), mean(q10.dat.sc1$TMeanF), mean(q11.dat.sc1$TMeanF), mean(q12.dat.sc1$TMeanF)))]

#value
mean.temp.warmest.quarter.sc1

##Mean Temperature of Coldest Quarter

mean.temp.coldest.quarter.sc1 <- c(mean(q1.dat.sc1$TMeanF), mean(q2.dat.sc1$TMeanF), mean(q3.dat.sc1$TMeanF), mean(q4.dat.sc1$TMeanF),
                                   mean(q5.dat.sc1$TMeanF), mean(q6.dat.sc1$TMeanF), mean(q7.dat.sc1$TMeanF), mean(q8.dat.sc1$TMeanF),
                                   mean(q9.dat.sc1$TMeanF), mean(q10.dat.sc1$TMeanF), mean(q11.dat.sc1$TMeanF), mean(q12.dat.sc1$TMeanF)
)[which.min(c(mean(q1.dat.sc1$TMeanF), mean(q2.dat.sc1$TMeanF), mean(q3.dat.sc1$TMeanF), mean(q4.dat.sc1$TMeanF), 
              mean(q5.dat.sc1$TMeanF), mean(q6.dat.sc1$TMeanF), mean(q7.dat.sc1$TMeanF), mean(q8.dat.sc1$TMeanF), 
              mean(q9.dat.sc1$TMeanF), mean(q10.dat.sc1$TMeanF), mean(q11.dat.sc1$TMeanF), mean(q12.dat.sc1$TMeanF)))]

#value
mean.temp.coldest.quarter.sc1

##Precipitation of Wettest Month

precip.wettest.month.pre.sc1 <- c(jan.prav.dat.sc1, feb.prav.dat.sc1, mar.prav.dat.sc1, apr.prav.dat.sc1, may.prav.dat.sc1, jun.prav.dat.sc1,
                                  jul.prav.dat.sc1, aug.prav.dat.sc1, sep.prav.dat.sc1, oct.prav.dat.sc1, nov.prav.dat.sc1, dec.prav.dat.sc1
)[which.max(c(jan.prav.dat.sc1, feb.prav.dat.sc1, mar.prav.dat.sc1, apr.prav.dat.sc1, may.prav.dat.sc1, jun.prav.dat.sc1,
              jul.prav.dat.sc1, aug.prav.dat.sc1, sep.prav.dat.sc1, oct.prav.dat.sc1, nov.prav.dat.sc1, dec.prav.dat.sc1))]



precip.wettest.month.sc1 <- precip.wettest.month.pre.sc1

#value
precip.wettest.month.sc1

##Precipitation of Driest Month

precip.driest.month.pre.sc1 <- c(jan.prav.dat.sc1, feb.prav.dat.sc1, mar.prav.dat.sc1, apr.prav.dat.sc1, may.prav.dat.sc1, jun.prav.dat.sc1,
                                 jul.prav.dat.sc1, aug.prav.dat.sc1, sep.prav.dat.sc1, oct.prav.dat.sc1, nov.prav.dat.sc1, dec.prav.dat.sc1
)[which.min(c(jan.prav.dat.sc1, feb.prav.dat.sc1, mar.prav.dat.sc1, apr.prav.dat.sc1, may.prav.dat.sc1, jun.prav.dat.sc1,
              jul.prav.dat.sc1, aug.prav.dat.sc1, sep.prav.dat.sc1, oct.prav.dat.sc1, nov.prav.dat.sc1, dec.prav.dat.sc1))]


precip.driest.month.sc1 <- precip.driest.month.pre.sc1

#value
precip.driest.month.sc1

##Precipitation Seasonality (CV)
#SD(precip1-12)/1+average monthly precip   X 100

#SD(Tav1,2,3...)
monthly.means.precip.sc1 <- data.frame(c(jan.prav.dat.sc1, feb.prav.dat.sc1, mar.prav.dat.sc1, apr.prav.dat.sc1, may.prav.dat.sc1, jun.prav.dat.sc1,
                                         jul.prav.dat.sc1, aug.prav.dat.sc1, sep.prav.dat.sc1, oct.prav.dat.sc1, nov.prav.dat.sc1, dec.prav.dat.sc1))
#assign header to column
names(monthly.means.precip.sc1)[1]<-paste("praverage")

#now get the standard deviation of all twelve months
sd.precip.sc1 <- sd(monthly.means.precip.sc1$praverage)

#annual precipitation / getting the sum of over x years then dividing by x to get annual average
an.precip.sc1 <- (sum(dat.RCP4.5.2030$PPT_mm)/(future1_end_year - future1_start_year))
mo.precip.sc1 <- an.precip.sc1/12

#final calculation
precip.seasonality.cv.sc1 <- (sd.precip.sc1/(1+mo.precip.sc1))*100

#value
precip.seasonality.cv.sc1

##Precipitation of Wettest Quarter

precip.wettest.quarter.pre.sc1 <- c(sum(q1.dat.sc1$PPT_in), sum(q2.dat.sc1$PPT_in), sum(q3.dat.sc1$PPT_in), sum(q4.dat.sc1$PPT_in),
                                    sum(q5.dat.sc1$PPT_in), sum(q6.dat.sc1$PPT_in), sum(q7.dat.sc1$PPT_in), sum(q8.dat.sc1$PPT_in),
                                    sum(q9.dat.sc1$PPT_in), sum(q10.dat.sc1$PPT_in), sum(q11.dat.sc1$PPT_in), sum(q12.dat.sc1$PPT_in)
)[which.max(c(mean(q1.dat.sc1$PPT_in)*90.25, mean(q2.dat.sc1$PPT_in)*89.25, mean(q3.dat.sc1$PPT_in)*92, mean(q4.dat.sc1$PPT_in)*91, 
              mean(q5.dat.sc1$PPT_in)*92, mean(q6.dat.sc1$PPT_in)*92, mean(q7.dat.sc1$PPT_in)*92, mean(q8.dat.sc1$PPT_in)*92, 
              mean(q9.dat.sc1$PPT_in)*91, mean(q10.dat.sc1$PPT_in)*92, mean(q11.dat.sc1$PPT_in)*92, mean(q12.dat.sc1$PPT_in)*90.25))]

#previous calculation is the sum over the historical period of said quarter
#divide by 10 to get the annual quarter calculation
precip.wettest.quarter.sc1 <- precip.wettest.quarter.pre.sc1/(future1_end_year - future1_start_year)

#value
precip.wettest.quarter.sc1

##Precipitation of Driest Quarter

precip.driest.quarter.pre.sc1 <- c(sum(q1.dat.sc1$PPT_in), sum(q2.dat.sc1$PPT_in), sum(q3.dat.sc1$PPT_in), sum(q4.dat.sc1$PPT_in),
                                   sum(q5.dat.sc1$PPT_in), sum(q6.dat.sc1$PPT_in), sum(q7.dat.sc1$PPT_in), sum(q8.dat.sc1$PPT_in),
                                   sum(q9.dat.sc1$PPT_in), sum(q10.dat.sc1$PPT_in), sum(q11.dat.sc1$PPT_in), sum(q12.dat.sc1$PPT_in)
)[which.min(c(mean(q1.dat.sc1$PPT_in)*90.25, mean(q2.dat.sc1$PPT_in)*89.25, mean(q3.dat.sc1$PPT_in)*92, mean(q4.dat.sc1$PPT_in)*91, 
              mean(q5.dat.sc1$PPT_in)*92, mean(q6.dat.sc1$PPT_in)*92, mean(q7.dat.sc1$PPT_in)*92, mean(q8.dat.sc1$PPT_in)*92, 
              mean(q9.dat.sc1$PPT_in)*91, mean(q10.dat.sc1$PPT_in)*92, mean(q11.dat.sc1$PPT_in)*92, mean(q12.dat.sc1$PPT_in)*90.25))]

#previous calculation is the sum over historical period of said quarter
#divide by 10 to get the annual quarter calculation 
precip.driest.quarter.sc1 <- precip.driest.quarter.pre.sc1/(baseline_end_year - baseline_start_year)

#value
precip.driest.quarter.sc1

##Precipitation of Coldest Quarter

precip.coldest.quarter.pre.sc1 <- c(sum(q1.dat.sc1$PPT_in), sum(q2.dat.sc1$PPT_in), sum(q3.dat.sc1$PPT_in), sum(q4.dat.sc1$PPT_in),
                                    sum(q5.dat.sc1$PPT_in), sum(q6.dat.sc1$PPT_in), sum(q7.dat.sc1$PPT_in), sum(q8.dat.sc1$PPT_in),
                                    sum(q9.dat.sc1$PPT_in), sum(q10.dat.sc1$PPT_in), sum(q11.dat.sc1$PPT_in), sum(q12.dat.sc1$PPT_in)
)[which.min(c(mean(q1.dat.sc1$TMeanF), mean(q2.dat.sc1$TMeanF), mean(q3.dat.sc1$TMeanF), mean(q4.dat.sc1$TMeanF), 
              mean(q5.dat.sc1$TMeanF), mean(q6.dat.sc1$TMeanF), mean(q7.dat.sc1$TMeanF), mean(q8.dat.sc1$TMeanF), 
              mean(q9.dat.sc1$TMeanF), mean(q10.dat.sc1$TMeanF), mean(q11.dat.sc1$TMeanF), mean(q12.dat.sc1$TMeanF)))]

#previous calculation is the sum over historical period of said quarter
#divide by 10 to get the annual quarter calculation 
precip.coldest.quarter.sc1 <- precip.coldest.quarter.pre.sc1/(future1_end_year - future1_start_year)

#value
precip.coldest.quarter.sc1

##Precipitation of the Warmest Quarter

precip.warmest.quarter.pre.sc1 <- c(sum(q1.dat.sc1$PPT_in), sum(q2.dat.sc1$PPT_in), sum(q3.dat.sc1$PPT_in), sum(q4.dat.sc1$PPT_in),
                                    sum(q5.dat.sc1$PPT_in), sum(q6.dat.sc1$PPT_in), sum(q7.dat.sc1$PPT_in), sum(q8.dat.sc1$PPT_in),
                                    sum(q9.dat.sc1$PPT_in), sum(q10.dat.sc1$PPT_in), sum(q11.dat.sc1$PPT_in), sum(q12.dat.sc1$PPT_in)
)[which.max(c(mean(q1.dat.sc1$TMeanF), mean(q2.dat.sc1$TMeanF), mean(q3.dat.sc1$TMeanF), mean(q4.dat.sc1$TMeanF), 
              mean(q5.dat.sc1$TMeanF), mean(q6.dat.sc1$TMeanF), mean(q7.dat.sc1$TMeanF), mean(q8.dat.sc1$TMeanF), 
              mean(q9.dat.sc1$TMeanF), mean(q10.dat.sc1$TMeanF), mean(q11.dat.sc1$TMeanF), mean(q12.dat.sc1$TMeanF)))]

#previous calculation is the sum over projected period of said quarter
#divide to get the annual quarter calculation 
precip.warmest.quarter.sc1 <- precip.warmest.quarter.pre.sc1/(future1_end_year - future1_start_year)

#value
precip.warmest.quarter.sc1

#####RCP 4.5 2050

#setting aside appropriate data

sc2.temp.av <- c(dat.RCP4.5.2050$TMeanF)
sc2.temp.hi <- c(dat.RCP4.5.2050$TMaxF)
sc2.temp.lo <- c(dat.RCP4.5.2050$TMinF)
sc2.precip <-  c(dat.RCP4.5.2050$PPT_in)

#subsetting monthly data

jan.dat.sc2 <- subset(dat.RCP4.5.2050, month=="Jan")
feb.dat.sc2 <- subset(dat.RCP4.5.2050, month=="Feb")
mar.dat.sc2 <- subset(dat.RCP4.5.2050, month=="Mar")
apr.dat.sc2 <- subset(dat.RCP4.5.2050, month=="Apr")
may.dat.sc2 <- subset(dat.RCP4.5.2050, month=="May")
jun.dat.sc2 <- subset(dat.RCP4.5.2050, month=="Jun")
jul.dat.sc2 <- subset(dat.RCP4.5.2050, month=="Jul")
aug.dat.sc2 <- subset(dat.RCP4.5.2050, month=="Aug")
sep.dat.sc2 <- subset(dat.RCP4.5.2050, month=="Sep")
oct.dat.sc2 <- subset(dat.RCP4.5.2050, month=="Oct")
nov.dat.sc2 <- subset(dat.RCP4.5.2050, month=="Nov") #, na.rm=TRUE #November is sometimes wonky
dec.dat.sc2 <- subset(dat.RCP4.5.2050, month=="Dec")

#subsetting monthly tav

jan.tav.sc2 <- mean(jan.dat.sc2$TMeanF)
feb.tav.sc2 <- mean(feb.dat.sc2$TMeanF)
mar.tav.sc2 <- mean(mar.dat.sc2$TMeanF)
apr.tav.sc2 <- mean(apr.dat.sc2$TMeanF)
may.tav.sc2 <- mean(may.dat.sc2$TMeanF)
jun.tav.sc2 <- mean(jun.dat.sc2$TMeanF)
jul.tav.sc2 <- mean(jul.dat.sc2$TMeanF)
aug.tav.sc2 <- mean(aug.dat.sc2$TMeanF)
sep.tav.sc2 <- mean(sep.dat.sc2$TMeanF)
oct.tav.sc2 <- mean(oct.dat.sc2$TMeanF)
nov.tav.sc2 <- mean(nov.dat.sc2$TMeanF)
dec.tav.sc2 <- mean(dec.dat.sc2$TMeanF)

#subsetting different quarters

q1.dat.sc2 <- rbind(jan.dat.sc2, feb.dat.sc2, mar.dat.sc2)
q2.dat.sc2 <- rbind(feb.dat.sc2, mar.dat.sc2, apr.dat.sc2)
q3.dat.sc2 <- rbind(mar.dat.sc2, apr.dat.sc2, may.dat.sc2)
q4.dat.sc2 <- rbind(apr.dat.sc2, may.dat.sc2, jun.dat.sc2)
q5.dat.sc2 <- rbind(may.dat.sc2, jun.dat.sc2, jul.dat.sc2)
q6.dat.sc2 <- rbind(jun.dat.sc2, jul.dat.sc2, aug.dat.sc2)
q7.dat.sc2 <- rbind(jul.dat.sc2, aug.dat.sc2, sep.dat.sc2)
q8.dat.sc2 <- rbind(aug.dat.sc2, sep.dat.sc2, oct.dat.sc2)
q9.dat.sc2 <- rbind(sep.dat.sc2, oct.dat.sc2, nov.dat.sc2)
q10.dat.sc2 <- rbind(oct.dat.sc2, nov.dat.sc2, dec.dat.sc2)
q11.dat.sc2 <- rbind(nov.dat.sc2, dec.dat.sc2, jan.dat.sc2)
q12.dat.sc2 <- rbind(dec.dat.sc2, jan.dat.sc2, feb.dat.sc2)

#subsetting precipitation data

jan.prav.dat.sc2 <- (sum(jan.dat.sc2$PPT_in))/(future2_end_year - future2_start_year)
feb.prav.dat.sc2 <- (sum(feb.dat.sc2$PPT_in))/(future2_end_year - future2_start_year)
mar.prav.dat.sc2 <- (sum(mar.dat.sc2$PPT_in))/(future2_end_year - future2_start_year)
apr.prav.dat.sc2 <- (sum(apr.dat.sc2$PPT_in))/(future2_end_year - future2_start_year)
may.prav.dat.sc2 <- (sum(may.dat.sc2$PPT_in))/(future2_end_year - future2_start_year)
jun.prav.dat.sc2 <- (sum(jun.dat.sc2$PPT_in))/(future2_end_year - future2_start_year)
jul.prav.dat.sc2 <- (sum(jul.dat.sc2$PPT_in))/(future2_end_year - future2_start_year)
aug.prav.dat.sc2 <- (sum(aug.dat.sc2$PPT_in))/(future2_end_year - future2_start_year)
sep.prav.dat.sc2 <- (sum(sep.dat.sc2$PPT_in))/(future2_end_year - future2_start_year)
oct.prav.dat.sc2 <- (sum(oct.dat.sc2$PPT_in))/(future2_end_year - future2_start_year)
nov.prav.dat.sc2 <- (sum(nov.dat.sc2$PPT_in))/(future2_end_year - future2_start_year)
dec.prav.dat.sc2 <- (sum(dec.dat.sc2$PPT_in))/(future2_end_year - future2_start_year)

## 1) Annual Mean Diurnal Range
#difference between month's max and min temp averaged over 12 months
#SUMof(Tmax-Tmin)/12

jan.max.sc2 <- mean(jan.dat.sc2$TMaxF)
jan.min.sc2 <- mean(jan.dat.sc2$TMinF)

feb.max.sc2 <- mean(feb.dat.sc2$TMaxF)
feb.min.sc2 <- mean(feb.dat.sc2$TMinF)

mar.max.sc2 <- mean(mar.dat.sc2$TMaxF)
mar.min.sc2 <- mean(mar.dat.sc2$TMinF)

apr.max.sc2 <- mean(apr.dat.sc2$TMaxF)
apr.min.sc2 <- mean(apr.dat.sc2$TMinF)

may.max.sc2 <- mean(may.dat.sc2$TMaxF)
may.min.sc2 <- mean(may.dat.sc2$TMinF)

jun.max.sc2 <- mean(jun.dat.sc2$TMaxF)
jun.min.sc2 <- mean(jun.dat.sc2$TMinF)

jul.max.sc2 <- mean(jul.dat.sc2$TMaxF)
jul.min.sc2 <- mean(jul.dat.sc2$TMinF)

aug.max.sc2 <- mean(aug.dat.sc2$TMaxF)
aug.min.sc2 <- mean(aug.dat.sc2$TMinF)

sep.max.sc2 <- mean(sep.dat.sc2$TMaxF)
sep.min.sc2 <- mean(sep.dat.sc2$TMinF)

oct.max.sc2 <- mean(oct.dat.sc2$TMaxF)
oct.min.sc2 <- mean(oct.dat.sc2$TMinF)

nov.max.sc2 <- mean(nov.dat.sc2$TMaxF)
nov.min.sc2 <- mean(nov.dat.sc2$TMinF)

dec.max.sc2 <- mean(dec.dat.sc2$TMaxF)
dec.min.sc2 <- mean(dec.dat.sc2$TMinF)

#differences
jan.dif.sc2 <- jan.max.sc2-jan.min.sc2
feb.dif.sc2 <- feb.max.sc2-feb.min.sc2
mar.dif.sc2 <- mar.max.sc2-mar.min.sc2
apr.dif.sc2 <- apr.max.sc2-apr.min.sc2
may.dif.sc2 <- may.max.sc2-may.min.sc2
jun.dif.sc2 <- jun.max.sc2-jun.min.sc2
jul.dif.sc2 <- jul.max.sc2-jul.min.sc2
aug.dif.sc2 <- aug.max.sc2-aug.min.sc2
sep.dif.sc2 <- sep.max.sc2-sep.min.sc2
oct.dif.sc2 <- oct.max.sc2-oct.min.sc2
nov.dif.sc2 <- nov.max.sc2-nov.min.sc2
dec.dif.sc2 <- dec.max.sc2-dec.min.sc2

#sum of all months annual mean diurnal range
amdr.sum.sc2 <- sum(jan.dif.sc2, feb.dif.sc2, mar.dif.sc2, apr.dif.sc2, may.dif.sc2, jun.dif.sc2, 
                    jul.dif.sc2, aug.dif.sc2, sep.dif.sc2, oct.dif.sc2, nov.dif.sc2, dec.dif.sc2)

#divide by 12 to get one months value
annual.mean.diurnal.range.sc2 <- (amdr.sum.sc2/12)

#value

annual.mean.diurnal.range.sc2

##Isothermality
#units in percent
#how large day to night temperatures oscilate relative to the summer to winter oscillations
#ratio of mean diurnal range to the annual temperature range
#(annual.mean.diurnal.range/annual.temp.range)*100

#conversion formula f to c
f.to.c <- function(temp) {
  celsius <- ((temp -32) *(5/9))
  return(celsius)
}

#absolute max and min
abs.tmax.pre.sc2 <- max(dat.RCP4.5.2050$TMaxF)
abs.tmax.c.sc2 <- f.to.c(abs.tmax.pre.sc2)
abs.tmin.pre.sc2 <- min(dat.RCP4.5.2050$TMinF)
abs.tmin.c.sc2 <- f.to.c(abs.tmin.pre.sc2)


#annual temp range
annual.temp.range.sc2 <- abs.tmax.c.sc2 - abs.tmin.c.sc2

isothermality.sc2 <- (annual.mean.diurnal.range.sc2/annual.temp.range.sc2)*100

#value
isothermality.sc2

##Temperature Seasonality SD
#amount of temperature variation over a given year (or averaged years) based on SD of monthly
#temp averages
#SD(Tav1,2,3...)
monthly.means.sc2 <- data.frame(c(jan.tav.sc2, feb.tav.sc2, mar.tav.sc2, apr.tav.sc2, may.tav.sc2, jun.tav.sc2,
                                  jul.tav.sc2, aug.tav.sc2, sep.tav.sc2, oct.tav.sc2, nov.tav.sc2, dec.tav.sc2))
#assign header to column
names(monthly.means.sc2)[1]<-paste("faverage")

#convert it to celsius
monthly.means.c.sc2 <- f.to.c(monthly.means.sc2)

#assign header to column
names(monthly.means.c.sc2)[1]<-paste("caverage")

#now get the standard deviation of all twelve months
sd.temperature.seasonality.sc2 <- sd(monthly.means.c.sc2$caverage)

#value
sd.temperature.seasonality.sc2

##Temperature Seasonality (CV)
#SD(TKav1-12)/annual mean temp in K
#need to use Kelvin for this one so lets create a function

f.to.k <- function(temp) {
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}

#function created, lets get Kelvin values for each month
jan.k.sc2 <- f.to.k(jan.tav.sc2)
feb.k.sc2 <- f.to.k(feb.tav.sc2)
mar.k.sc2 <- f.to.k(mar.tav.sc2)
apr.k.sc2 <- f.to.k(apr.tav.sc2)
may.k.sc2 <- f.to.k(may.tav.sc2)
jun.k.sc2 <- f.to.k(jun.tav.sc2)
jul.k.sc2 <- f.to.k(jul.tav.sc2)
aug.k.sc2 <- f.to.k(aug.tav.sc2)
sep.k.sc2 <- f.to.k(sep.tav.sc2)
oct.k.sc2 <- f.to.k(oct.tav.sc2)
nov.k.sc2 <- f.to.k(nov.tav.sc2)
dec.k.sc2 <- f.to.k(dec.tav.sc2)

#create a data frame

monthly.means.k.sc2 <- data.frame(c(jan.k.sc2, feb.k.sc2, mar.k.sc2, apr.k.sc2, may.k.sc2, jun.k.sc2,
                                    jul.k.sc2, aug.k.sc2, sep.k.sc2, oct.k.sc2, nov.k.sc2, dec.k.sc2))

#assign header to column
names(monthly.means.k.sc2)[1]<-paste("kaverage")

sd.mmk.sc2 <- sd(monthly.means.k.sc2$kaverage)

#determine annual mean temp
annual.av.f.sc2 <- mean(dat.RCP4.5.2050$TMeanF)

#convert it to Kelvin
annual.av.k.sc2. <- f.to.k(annual.av.f.sc2)

##Temperature Seasonality CV
cv.temperature.seasonality.sc2 <- ((sd.mmk.sc2)/annual.av.k.sc2.)*100

#value
cv.temperature.seasonality.sc2

##Max Temperature of Warmest Month
#lets first figure out the warmest month
#saving it in case we need it
warmest.day.warmest.month.sc2 <- c(max(jan.dat.sc2$TMaxF), max(feb.dat.sc2$TMaxF), max(mar.dat.sc2$TMaxF), max(apr.dat.sc2$TMaxF),
                                   max(may.dat.sc2$TMaxF), max(jun.dat.sc2$TMaxF), max(jul.dat.sc2$TMaxF), max(aug.dat.sc2$TMaxF),
                                   max(sep.dat.sc2$TMaxF), max(oct.dat.sc2$TMaxF), max(nov.dat.sc2$TMaxF), max(dec.dat.sc2$TMaxF)
)[which.max(c(mean(jan.dat.sc2$TMaxF), mean(feb.dat.sc2$TMaxF), mean(mar.dat.sc2$TMaxF), mean(apr.dat.sc2$TMaxF), 
              mean(may.dat.sc2$TMaxF), mean(jun.dat.sc2$TMaxF), mean(jul.dat.sc2$TMaxF), mean(aug.dat.sc2$TMaxF), 
              mean(sep.dat.sc2$TMaxF), mean(oct.dat.sc2$TMaxF), mean(nov.dat.sc2$TMaxF), mean(dec.dat.sc2$TMaxF)))]

#value
warmest.day.warmest.month.sc2

##Min Temperature of Coldest Month
#saving it in case they want it
coldest.day.coldest.month.sc2 <- c(min(jan.dat.sc2$TMinF), min(feb.dat.sc2$TMinF), min(mar.dat.sc2$TMinF), min(apr.dat.sc2$TMinF),
                                   min(may.dat.sc2$TMinF), min(jun.dat.sc2$TMinF), min(jul.dat.sc2$TMinF), min(aug.dat.sc2$TMinF),
                                   min(sep.dat.sc2$TMinF), min(oct.dat.sc2$TMinF), min(nov.dat.sc2$TMinF), min(dec.dat.sc2$TMinF)
)[which.min(c(mean(jan.dat.sc2$TMinF), mean(feb.dat.sc2$TMinF), mean(mar.dat.sc2$TMinF), mean(apr.dat.sc2$TMinF), 
              mean(may.dat.sc2$TMinF), mean(jun.dat.sc2$TMinF), mean(jul.dat.sc2$TMinF), mean(aug.dat.sc2$TMinF), 
              mean(sep.dat.sc2$TMinF), mean(oct.dat.sc2$TMinF), mean(nov.dat.sc2$TMinF), mean(dec.dat.sc2$TMinF)))]

#value
coldest.day.coldest.month.sc2

##Annual Temperature Range
#hottest day of hottest month minus coldest day of coldest month

annual.temperature.range.sc2 <- warmest.day.warmest.month.sc2 - coldest.day.coldest.month.sc2

#value
annual.temperature.range.sc2

##Mean Temperature of Wettest Quarter

mean.temp.wettest.quarter.sc2 <- c(mean(q1.dat.sc2$TMeanF), mean(q2.dat.sc2$TMeanF), mean(q3.dat.sc2$TMeanF), mean(q4.dat.sc2$TMeanF),
                                   mean(q5.dat.sc2$TMeanF), mean(q6.dat.sc2$TMeanF), mean(q7.dat.sc2$TMeanF), mean(q8.dat.sc2$TMeanF),
                                   mean(q9.dat.sc2$TMeanF), mean(q10.dat.sc2$TMeanF), mean(q11.dat.sc2$TMeanF), mean(q12.dat.sc2$TMeanF)
)[which.max(c(mean(q1.dat.sc2$PPT_in)*90.25, mean(q2.dat.sc2$PPT_in)*89.25, mean(q3.dat.sc2$PPT_in)*92, mean(q4.dat.sc2$PPT_in)*91, 
              mean(q5.dat.sc2$PPT_in)*92, mean(q6.dat.sc2$PPT_in)*92, mean(q7.dat.sc2$PPT_in)*92, mean(q8.dat.sc2$PPT_in)*92, 
              mean(q9.dat.sc2$PPT_in)*91, mean(q10.dat.sc2$PPT_in)*92, mean(q11.dat.sc2$PPT_in)*92, mean(q12.dat.sc2$PPT_in)*90.25))]

#value
mean.temp.wettest.quarter.sc2

##Mean Temperature of Driest Quarter

mean.temp.driest.quarter.sc2 <- c(mean(q1.dat.sc2$TMeanF), mean(q2.dat.sc2$TMeanF), mean(q3.dat.sc2$TMeanF), mean(q4.dat.sc2$TMeanF),
                                  mean(q5.dat.sc2$TMeanF), mean(q6.dat.sc2$TMeanF), mean(q7.dat.sc2$TMeanF), mean(q8.dat.sc2$TMeanF),
                                  mean(q9.dat.sc2$TMeanF), mean(q10.dat.sc2$TMeanF), mean(q11.dat.sc2$TMeanF), mean(q12.dat.sc2$TMeanF)
)[which.min(c(mean(q1.dat.sc2$PPT_in)*90.25, mean(q2.dat.sc2$PPT_in)*89.25, mean(q3.dat.sc2$PPT_in)*92, mean(q4.dat.sc2$PPT_in)*91, 
              mean(q5.dat.sc2$PPT_in)*92, mean(q6.dat.sc2$PPT_in)*92, mean(q7.dat.sc2$PPT_in)*92, mean(q8.dat.sc2$PPT_in)*92, 
              mean(q9.dat.sc2$PPT_in)*91, mean(q10.dat.sc2$PPT_in)*92, mean(q11.dat.sc2$PPT_in)*92, mean(q12.dat.sc2$PPT_in)*90.25))]

#value
mean.temp.driest.quarter.sc2

##Mean Temperature of Warmest Quarter

mean.temp.warmest.quarter.sc2 <- c(mean(q1.dat.sc2$TMeanF), mean(q2.dat.sc2$TMeanF), mean(q3.dat.sc2$TMeanF), mean(q4.dat.sc2$TMeanF),
                                   mean(q5.dat.sc2$TMeanF), mean(q6.dat.sc2$TMeanF), mean(q7.dat.sc2$TMeanF), mean(q8.dat.sc2$TMeanF),
                                   mean(q9.dat.sc2$TMeanF), mean(q10.dat.sc2$TMeanF), mean(q11.dat.sc2$TMeanF), mean(q12.dat.sc2$TMeanF)
)[which.max(c(mean(q1.dat.sc2$TMeanF), mean(q2.dat.sc2$TMeanF), mean(q3.dat.sc2$TMeanF), mean(q4.dat.sc2$TMeanF), 
              mean(q5.dat.sc2$TMeanF), mean(q6.dat.sc2$TMeanF), mean(q7.dat.sc2$TMeanF), mean(q8.dat.sc2$TMeanF), 
              mean(q9.dat.sc2$TMeanF), mean(q10.dat.sc2$TMeanF), mean(q11.dat.sc2$TMeanF), mean(q12.dat.sc2$TMeanF)))]

#value
mean.temp.warmest.quarter.sc2

##Mean Temperature of Coldest Quarter

mean.temp.coldest.quarter.sc2 <- c(mean(q1.dat.sc2$TMeanF), mean(q2.dat.sc2$TMeanF), mean(q3.dat.sc2$TMeanF), mean(q4.dat.sc2$TMeanF),
                                   mean(q5.dat.sc2$TMeanF), mean(q6.dat.sc2$TMeanF), mean(q7.dat.sc2$TMeanF), mean(q8.dat.sc2$TMeanF),
                                   mean(q9.dat.sc2$TMeanF), mean(q10.dat.sc2$TMeanF), mean(q11.dat.sc2$TMeanF), mean(q12.dat.sc2$TMeanF)
)[which.min(c(mean(q1.dat.sc2$TMeanF), mean(q2.dat.sc2$TMeanF), mean(q3.dat.sc2$TMeanF), mean(q4.dat.sc2$TMeanF), 
              mean(q5.dat.sc2$TMeanF), mean(q6.dat.sc2$TMeanF), mean(q7.dat.sc2$TMeanF), mean(q8.dat.sc2$TMeanF), 
              mean(q9.dat.sc2$TMeanF), mean(q10.dat.sc2$TMeanF), mean(q11.dat.sc2$TMeanF), mean(q12.dat.sc2$TMeanF)))]

#value
mean.temp.coldest.quarter.sc2

##Precipitation of Wettest Month

precip.wettest.month.pre.sc2 <- c(jan.prav.dat.sc2, feb.prav.dat.sc2, mar.prav.dat.sc2, apr.prav.dat.sc2, may.prav.dat.sc2, jun.prav.dat.sc2,
                                  jul.prav.dat.sc2, aug.prav.dat.sc2, sep.prav.dat.sc2, oct.prav.dat.sc2, nov.prav.dat.sc2, dec.prav.dat.sc2
)[which.max(c(jan.prav.dat.sc2, feb.prav.dat.sc2, mar.prav.dat.sc2, apr.prav.dat.sc2, may.prav.dat.sc2, jun.prav.dat.sc2,
              jul.prav.dat.sc2, aug.prav.dat.sc2, sep.prav.dat.sc2, oct.prav.dat.sc2, nov.prav.dat.sc2, dec.prav.dat.sc2))]

precip.wettest.month.sc2 <- precip.wettest.month.pre.sc2

#value
precip.wettest.month.sc2

##Precipitation of Driest Month

precip.driest.month.pre.sc2 <- c(jan.prav.dat.sc2, feb.prav.dat.sc2, mar.prav.dat.sc2, apr.prav.dat.sc2, may.prav.dat.sc2, jun.prav.dat.sc2,
                                 jul.prav.dat.sc2, aug.prav.dat.sc2, sep.prav.dat.sc2, oct.prav.dat.sc2, nov.prav.dat.sc2, dec.prav.dat.sc2
)[which.min(c(jan.prav.dat.sc2, feb.prav.dat.sc2, mar.prav.dat.sc2, apr.prav.dat.sc2, may.prav.dat.sc2, jun.prav.dat.sc2,
              jul.prav.dat.sc2, aug.prav.dat.sc2, sep.prav.dat.sc2, oct.prav.dat.sc2, nov.prav.dat.sc2, dec.prav.dat.sc2))]


precip.driest.month.sc2 <- precip.driest.month.pre.sc2

#value
precip.driest.month.sc2

##Precipitation Seasonality (CV)
#SD(precip1-12)/1+average monthly precip   X 100

#SD(Tav1,2,3...)
monthly.means.precip.sc2 <- data.frame(c(jan.prav.dat.sc2, feb.prav.dat.sc2, mar.prav.dat.sc2, apr.prav.dat.sc2, may.prav.dat.sc2, jun.prav.dat.sc2,
                                         jul.prav.dat.sc2, aug.prav.dat.sc2, sep.prav.dat.sc2, oct.prav.dat.sc2, nov.prav.dat.sc2, dec.prav.dat.sc2))
#assign header to column
names(monthly.means.precip.sc2)[1]<-paste("praverage")

#now get the standard deviation of all twelve months
sd.precip.sc2 <- sd(monthly.means.precip.sc2$praverage)

#annual precipitation / getting the sum of years then dividing by 12 to get annual average
an.precip.sc2 <- (sum(dat.RCP4.5.2050$PPT_mm)/(future2_end_year - future2_start_year))
mo.precip.sc2 <- an.precip.sc2/12

#final calculation
precip.seasonality.cv.sc2 <- (sd.precip.sc2/(1+mo.precip.sc2))*100

#value
precip.seasonality.cv.sc2

##Precipitation of Wettest Quarter

precip.wettest.quarter.pre.sc2 <- c(sum(q1.dat.sc2$PPT_in), sum(q2.dat.sc2$PPT_in), sum(q3.dat.sc2$PPT_in), sum(q4.dat.sc2$PPT_in),
                                    sum(q5.dat.sc2$PPT_in), sum(q6.dat.sc2$PPT_in), sum(q7.dat.sc2$PPT_in), sum(q8.dat.sc2$PPT_in),
                                    sum(q9.dat.sc2$PPT_in), sum(q10.dat.sc2$PPT_in), sum(q11.dat.sc2$PPT_in), sum(q12.dat.sc2$PPT_in)
)[which.max(c(mean(q1.dat.sc2$PPT_in)*90.25, mean(q2.dat.sc2$PPT_in)*89.25, mean(q3.dat.sc2$PPT_in)*92, mean(q4.dat.sc2$PPT_in)*91, 
              mean(q5.dat.sc2$PPT_in)*92, mean(q6.dat.sc2$PPT_in)*92, mean(q7.dat.sc2$PPT_in)*92, mean(q8.dat.sc2$PPT_in)*92, 
              mean(q9.dat.sc2$PPT_in)*91, mean(q10.dat.sc2$PPT_in)*92, mean(q11.dat.sc2$PPT_in)*92, mean(q12.dat.sc2$PPT_in)*90.25))]

#previous calculation is the sum over projected period of said quarter
#divide by number of years to get the annual quarter calculation 
precip.wettest.quarter.sc2 <- precip.wettest.quarter.pre.sc2/(future2_end_year - future2_start_year)

#value
precip.wettest.quarter.sc2

##Precipitation of Driest Quarter

precip.driest.quarter.pre.sc2 <- c(sum(q1.dat.sc2$PPT_in), sum(q2.dat.sc2$PPT_in), sum(q3.dat.sc2$PPT_in), sum(q4.dat.sc2$PPT_in),
                                   sum(q5.dat.sc2$PPT_in), sum(q6.dat.sc2$PPT_in), sum(q7.dat.sc2$PPT_in), sum(q8.dat.sc2$PPT_in),
                                   sum(q9.dat.sc2$PPT_in), sum(q10.dat.sc2$PPT_in), sum(q11.dat.sc2$PPT_in), sum(q12.dat.sc2$PPT_in)
)[which.min(c(mean(q1.dat.sc2$PPT_in)*90.25, mean(q2.dat.sc2$PPT_in)*89.25, mean(q3.dat.sc2$PPT_in)*92, mean(q4.dat.sc2$PPT_in)*91, 
              mean(q5.dat.sc2$PPT_in)*92, mean(q6.dat.sc2$PPT_in)*92, mean(q7.dat.sc2$PPT_in)*92, mean(q8.dat.sc2$PPT_in)*92, 
              mean(q9.dat.sc2$PPT_in)*91, mean(q10.dat.sc2$PPT_in)*92, mean(q11.dat.sc2$PPT_in)*92, mean(q12.dat.sc2$PPT_in)*90.25))]


precip.driest.quarter.sc2 <- precip.driest.quarter.pre.sc2/(future2_end_year - future2_start_year)

#value
precip.driest.quarter.sc2

##Precipitation of Coldest Quarter

precip.coldest.quarter.pre.sc2 <- c(sum(q1.dat.sc2$PPT_in), sum(q2.dat.sc2$PPT_in), sum(q3.dat.sc2$PPT_in), sum(q4.dat.sc2$PPT_in),
                                    sum(q5.dat.sc2$PPT_in), sum(q6.dat.sc2$PPT_in), sum(q7.dat.sc2$PPT_in), sum(q8.dat.sc2$PPT_in),
                                    sum(q9.dat.sc2$PPT_in), sum(q10.dat.sc2$PPT_in), sum(q11.dat.sc2$PPT_in), sum(q12.dat.sc2$PPT_in)
)[which.min(c(mean(q1.dat.sc2$TMeanF), mean(q2.dat.sc2$TMeanF), mean(q3.dat.sc2$TMeanF), mean(q4.dat.sc2$TMeanF), 
              mean(q5.dat.sc2$TMeanF), mean(q6.dat.sc2$TMeanF), mean(q7.dat.sc2$TMeanF), mean(q8.dat.sc2$TMeanF), 
              mean(q9.dat.sc2$TMeanF), mean(q10.dat.sc2$TMeanF), mean(q11.dat.sc2$TMeanF), mean(q12.dat.sc2$TMeanF)))]

#previous calculation is the sum over historical period of said quarter
#divide to get the annual quarter calculation 
precip.coldest.quarter.sc2 <- precip.coldest.quarter.pre.sc2/(future2_end_year - future2_start_year)

#value
precip.coldest.quarter.sc2

##Precipitation of the Warmest Quarter

precip.warmest.quarter.pre.sc2 <- c(sum(q1.dat.sc2$PPT_in), sum(q2.dat.sc2$PPT_in), sum(q3.dat.sc2$PPT_in), sum(q4.dat.sc2$PPT_in),
                                    sum(q5.dat.sc2$PPT_in), sum(q6.dat.sc2$PPT_in), sum(q7.dat.sc2$PPT_in), sum(q8.dat.sc2$PPT_in),
                                    sum(q9.dat.sc2$PPT_in), sum(q10.dat.sc2$PPT_in), sum(q11.dat.sc2$PPT_in), sum(q12.dat.sc2$PPT_in)
)[which.max(c(mean(q1.dat.sc2$TMeanF), mean(q2.dat.sc2$TMeanF), mean(q3.dat.sc2$TMeanF), mean(q4.dat.sc2$TMeanF), 
              mean(q5.dat.sc2$TMeanF), mean(q6.dat.sc2$TMeanF), mean(q7.dat.sc2$TMeanF), mean(q8.dat.sc2$TMeanF), 
              mean(q9.dat.sc2$TMeanF), mean(q10.dat.sc2$TMeanF), mean(q11.dat.sc2$TMeanF), mean(q12.dat.sc2$TMeanF)))]

#previous calculation is the sum over projected period of said quarter
#divide to get the annual quarter calculation 
precip.warmest.quarter.sc2 <- precip.warmest.quarter.pre.sc2/(future2_end_year - future2_start_year)

#value
precip.warmest.quarter.sc2

#####RCP 8.5 2030

#setting aside appropriate data

sc3.temp.av <- c(dat.RCP8.5.2030$TMeanF)
sc3.temp.hi <- c(dat.RCP8.5.2030$TMaxF)
sc3.temp.lo <- c(dat.RCP8.5.2030$TMinF)
sc3.precip <-  c(dat.RCP8.5.2030$PPT_in)

#subsetting monthly data

jan.dat.sc3 <- subset(dat.RCP8.5.2030, month=="Jan")
feb.dat.sc3 <- subset(dat.RCP8.5.2030, month=="Feb")
mar.dat.sc3 <- subset(dat.RCP8.5.2030, month=="Mar")
apr.dat.sc3 <- subset(dat.RCP8.5.2030, month=="Apr")
may.dat.sc3 <- subset(dat.RCP8.5.2030, month=="May")
jun.dat.sc3 <- subset(dat.RCP8.5.2030, month=="Jun")
jul.dat.sc3 <- subset(dat.RCP8.5.2030, month=="Jul")
aug.dat.sc3 <- subset(dat.RCP8.5.2030, month=="Aug")
sep.dat.sc3 <- subset(dat.RCP8.5.2030, month=="Sep")
oct.dat.sc3 <- subset(dat.RCP8.5.2030, month=="Oct")
nov.dat.sc3 <- subset(dat.RCP8.5.2030, month=="Nov") #, na.rm=TRUE #November is sometimes wonky
dec.dat.sc3 <- subset(dat.RCP8.5.2030, month=="Dec")

#subsetting monthly tav

jan.tav.sc3 <- mean(jan.dat.sc3$TMeanF)
feb.tav.sc3 <- mean(feb.dat.sc3$TMeanF)
mar.tav.sc3 <- mean(mar.dat.sc3$TMeanF)
apr.tav.sc3 <- mean(apr.dat.sc3$TMeanF)
may.tav.sc3 <- mean(may.dat.sc3$TMeanF)
jun.tav.sc3 <- mean(jun.dat.sc3$TMeanF)
jul.tav.sc3 <- mean(jul.dat.sc3$TMeanF)
aug.tav.sc3 <- mean(aug.dat.sc3$TMeanF)
sep.tav.sc3 <- mean(sep.dat.sc3$TMeanF)
oct.tav.sc3 <- mean(oct.dat.sc3$TMeanF)
nov.tav.sc3 <- mean(nov.dat.sc3$TMeanF)
dec.tav.sc3 <- mean(dec.dat.sc3$TMeanF)

#subsetting different quarters

q1.dat.sc3 <- rbind(jan.dat.sc3, feb.dat.sc3, mar.dat.sc3)
q2.dat.sc3 <- rbind(feb.dat.sc3, mar.dat.sc3, apr.dat.sc3)
q3.dat.sc3 <- rbind(mar.dat.sc3, apr.dat.sc3, may.dat.sc3)
q4.dat.sc3 <- rbind(apr.dat.sc3, may.dat.sc3, jun.dat.sc3)
q5.dat.sc3 <- rbind(may.dat.sc3, jun.dat.sc3, jul.dat.sc3)
q6.dat.sc3 <- rbind(jun.dat.sc3, jul.dat.sc3, aug.dat.sc3)
q7.dat.sc3 <- rbind(jul.dat.sc3, aug.dat.sc3, sep.dat.sc3)
q8.dat.sc3 <- rbind(aug.dat.sc3, sep.dat.sc3, oct.dat.sc3)
q9.dat.sc3 <- rbind(sep.dat.sc3, oct.dat.sc3, nov.dat.sc3)
q10.dat.sc3 <- rbind(oct.dat.sc3, nov.dat.sc3, dec.dat.sc3)
q11.dat.sc3 <- rbind(nov.dat.sc3, dec.dat.sc3, jan.dat.sc3)
q12.dat.sc3 <- rbind(dec.dat.sc3, jan.dat.sc3, feb.dat.sc3)

#subsetting precipitation data

jan.prav.dat.sc3 <- (sum(jan.dat.sc3$PPT_in))/(future1_end_year - future1_start_year)
feb.prav.dat.sc3 <- (sum(feb.dat.sc3$PPT_in))/(future1_end_year - future1_start_year)
mar.prav.dat.sc3 <- (sum(mar.dat.sc3$PPT_in))/(future1_end_year - future1_start_year)
apr.prav.dat.sc3 <- (sum(apr.dat.sc3$PPT_in))/(future1_end_year - future1_start_year)
may.prav.dat.sc3 <- (sum(may.dat.sc3$PPT_in))/(future1_end_year - future1_start_year)
jun.prav.dat.sc3 <- (sum(jun.dat.sc3$PPT_in))/(future1_end_year - future1_start_year)
jul.prav.dat.sc3 <- (sum(jul.dat.sc3$PPT_in))/(future1_end_year - future1_start_year)
aug.prav.dat.sc3 <- (sum(aug.dat.sc3$PPT_in))/(future1_end_year - future1_start_year)
sep.prav.dat.sc3 <- (sum(sep.dat.sc3$PPT_in))/(future1_end_year - future1_start_year)
oct.prav.dat.sc3 <- (sum(oct.dat.sc3$PPT_in))/(future1_end_year - future1_start_year)
nov.prav.dat.sc3 <- (sum(nov.dat.sc3$PPT_in))/(future1_end_year - future1_start_year)
dec.prav.dat.sc3 <- (sum(dec.dat.sc3$PPT_in))/(future1_end_year - future1_start_year)

## 1) Annual Mean Diurnal Range
#difference between month's max and min temp averaged over 12 months
#SUMof(Tmax-Tmin)/12

jan.max.sc3 <- mean(jan.dat.sc3$TMaxF)
jan.min.sc3 <- mean(jan.dat.sc3$TMinF)

feb.max.sc3 <- mean(feb.dat.sc3$TMaxF)
feb.min.sc3 <- mean(feb.dat.sc3$TMinF)

mar.max.sc3 <- mean(mar.dat.sc3$TMaxF)
mar.min.sc3 <- mean(mar.dat.sc3$TMinF)

apr.max.sc3 <- mean(apr.dat.sc3$TMaxF)
apr.min.sc3 <- mean(apr.dat.sc3$TMinF)

may.max.sc3 <- mean(may.dat.sc3$TMaxF)
may.min.sc3 <- mean(may.dat.sc3$TMinF)

jun.max.sc3 <- mean(jun.dat.sc3$TMaxF)
jun.min.sc3 <- mean(jun.dat.sc3$TMinF)

jul.max.sc3 <- mean(jul.dat.sc3$TMaxF)
jul.min.sc3 <- mean(jul.dat.sc3$TMinF)

aug.max.sc3 <- mean(aug.dat.sc3$TMaxF)
aug.min.sc3 <- mean(aug.dat.sc3$TMinF)

sep.max.sc3 <- mean(sep.dat.sc3$TMaxF)
sep.min.sc3 <- mean(sep.dat.sc3$TMinF)

oct.max.sc3 <- mean(oct.dat.sc3$TMaxF)
oct.min.sc3 <- mean(oct.dat.sc3$TMinF)

nov.max.sc3 <- mean(nov.dat.sc3$TMaxF)
nov.min.sc3 <- mean(nov.dat.sc3$TMinF)

dec.max.sc3 <- mean(dec.dat.sc3$TMaxF)
dec.min.sc3 <- mean(dec.dat.sc3$TMinF)

#differences
jan.dif.sc3 <- jan.max.sc3-jan.min.sc3
feb.dif.sc3 <- feb.max.sc3-feb.min.sc3
mar.dif.sc3 <- mar.max.sc3-mar.min.sc3
apr.dif.sc3 <- apr.max.sc3-apr.min.sc3
may.dif.sc3 <- may.max.sc3-may.min.sc3
jun.dif.sc3 <- jun.max.sc3-jun.min.sc3
jul.dif.sc3 <- jul.max.sc3-jul.min.sc3
aug.dif.sc3 <- aug.max.sc3-aug.min.sc3
sep.dif.sc3 <- sep.max.sc3-sep.min.sc3
oct.dif.sc3 <- oct.max.sc3-oct.min.sc3
nov.dif.sc3 <- nov.max.sc3-nov.min.sc3
dec.dif.sc3 <- dec.max.sc3-dec.min.sc3

#sum of all months annual mean diurnal range
amdr.sum.sc3 <- sum(jan.dif.sc3, feb.dif.sc3, mar.dif.sc3, apr.dif.sc3, may.dif.sc3, jun.dif.sc3, 
                    jul.dif.sc3, aug.dif.sc3, sep.dif.sc3, oct.dif.sc3, nov.dif.sc3, dec.dif.sc3)

#divide by 12 to get one months value
annual.mean.diurnal.range.sc3 <- (amdr.sum.sc3/12)

#value

annual.mean.diurnal.range.sc3

##Isothermality
#units in percent
#how large day to night temperatures oscilate relative to the summer to winter oscillations
#ratio of mean diurnal range to the annual temperature range
#(annual.mean.diurnal.range/annual.temp.range)*100

#conversion formula f to c
f.to.c <- function(temp) {
  celsius <- ((temp -32) *(5/9))
  return(celsius)
}

#absolute max and min
abs.tmax.pre.sc3 <- max(dat.RCP8.5.2030$TMaxF)
abs.tmax.c.sc3 <- f.to.c(abs.tmax.pre.sc3)
abs.tmin.pre.sc3 <- min(dat.RCP8.5.2030$TMinF)
abs.tmin.c.sc3 <- f.to.c(abs.tmin.pre.sc3)


#annual temp range
annual.temp.range.sc3 <- abs.tmax.c.sc3 - abs.tmin.c.sc3

isothermality.sc3 <- (annual.mean.diurnal.range.sc3/annual.temp.range.sc3)*100

#value
isothermality.sc3

##Temperature Seasonality SD
#amount of temperature variation over a given year (or averaged years) based on SD of monthly
#temp averages
#SD(Tav1,2,3...)
monthly.means.sc3 <- data.frame(c(jan.tav.sc3, feb.tav.sc3, mar.tav.sc3, apr.tav.sc3, may.tav.sc3, jun.tav.sc3,
                                  jul.tav.sc3, aug.tav.sc3, sep.tav.sc3, oct.tav.sc3, nov.tav.sc3, dec.tav.sc3))
#assign header to column
names(monthly.means.sc3)[1]<-paste("faverage")

#convert it to celsius
monthly.means.c.sc3 <- f.to.c(monthly.means.sc3)

#assign header to column
names(monthly.means.c.sc3)[1]<-paste("caverage")

#now get the standard deviation of all twelve months
sd.temperature.seasonality.sc3 <- sd(monthly.means.c.sc3$caverage)

#value
sd.temperature.seasonality.sc3

##Temperature Seasonality (CV)
#SD(TKav1-12)/annual mean temp in K
#need to use Kelvin for this one so lets create a function

f.to.k <- function(temp) {
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}

#function created, lets get Kelvin values for each month
jan.k.sc3 <- f.to.k(jan.tav.sc3)
feb.k.sc3 <- f.to.k(feb.tav.sc3)
mar.k.sc3 <- f.to.k(mar.tav.sc3)
apr.k.sc3 <- f.to.k(apr.tav.sc3)
may.k.sc3 <- f.to.k(may.tav.sc3)
jun.k.sc3 <- f.to.k(jun.tav.sc3)
jul.k.sc3 <- f.to.k(jul.tav.sc3)
aug.k.sc3 <- f.to.k(aug.tav.sc3)
sep.k.sc3 <- f.to.k(sep.tav.sc3)
oct.k.sc3 <- f.to.k(oct.tav.sc3)
nov.k.sc3 <- f.to.k(nov.tav.sc3)
dec.k.sc3 <- f.to.k(dec.tav.sc3)

#create a data frame

monthly.means.k.sc3 <- data.frame(c(jan.k.sc3, feb.k.sc3, mar.k.sc3, apr.k.sc3, may.k.sc3, jun.k.sc3,
                                    jul.k.sc3, aug.k.sc3, sep.k.sc3, oct.k.sc3, nov.k.sc3, dec.k.sc3))

#assign header to column
names(monthly.means.k.sc3)[1]<-paste("kaverage")

sd.mmk.sc3 <- sd(monthly.means.k.sc3$kaverage)

#determine annual mean temp
annual.av.f.sc3 <- mean(dat.RCP8.5.2030$TMeanF)

#convert it to Kelvin
annual.av.k.sc3. <- f.to.k(annual.av.f.sc3)

##Temperature Seasonality CV
cv.temperature.seasonality.sc3 <- ((sd.mmk.sc3)/annual.av.k.sc3.)*100

#value
cv.temperature.seasonality.sc3

##Max Temperature of Warmest Month
#lets first figure out the warmest month
#saving it in case we need it
warmest.day.warmest.month.sc3 <- c(max(jan.dat.sc3$TMaxF), max(feb.dat.sc3$TMaxF), max(mar.dat.sc3$TMaxF), max(apr.dat.sc3$TMaxF),
                                   max(may.dat.sc3$TMaxF), max(jun.dat.sc3$TMaxF), max(jul.dat.sc3$TMaxF), max(aug.dat.sc3$TMaxF),
                                   max(sep.dat.sc3$TMaxF), max(oct.dat.sc3$TMaxF), max(nov.dat.sc3$TMaxF), max(dec.dat.sc3$TMaxF)
)[which.max(c(mean(jan.dat.sc3$TMaxF), mean(feb.dat.sc3$TMaxF), mean(mar.dat.sc3$TMaxF), mean(apr.dat.sc3$TMaxF), 
              mean(may.dat.sc3$TMaxF), mean(jun.dat.sc3$TMaxF), mean(jul.dat.sc3$TMaxF), mean(aug.dat.sc3$TMaxF), 
              mean(sep.dat.sc3$TMaxF), mean(oct.dat.sc3$TMaxF), mean(nov.dat.sc3$TMaxF), mean(dec.dat.sc3$TMaxF)))]

#value
warmest.day.warmest.month.sc3

##Min Temperature of Coldest Month
#saving it in case they want it
coldest.day.coldest.month.sc3 <- c(min(jan.dat.sc3$TMinF), min(feb.dat.sc3$TMinF), min(mar.dat.sc3$TMinF), min(apr.dat.sc3$TMinF),
                                   min(may.dat.sc3$TMinF), min(jun.dat.sc3$TMinF), min(jul.dat.sc3$TMinF), min(aug.dat.sc3$TMinF),
                                   min(sep.dat.sc3$TMinF), min(oct.dat.sc3$TMinF), min(nov.dat.sc3$TMinF), min(dec.dat.sc3$TMinF)
)[which.min(c(mean(jan.dat.sc3$TMinF), mean(feb.dat.sc3$TMinF), mean(mar.dat.sc3$TMinF), mean(apr.dat.sc3$TMinF), 
              mean(may.dat.sc3$TMinF), mean(jun.dat.sc3$TMinF), mean(jul.dat.sc3$TMinF), mean(aug.dat.sc3$TMinF), 
              mean(sep.dat.sc3$TMinF), mean(oct.dat.sc3$TMinF), mean(nov.dat.sc3$TMinF), mean(dec.dat.sc3$TMinF)))]

#value
coldest.day.coldest.month.sc3

##Annual Temperature Range
#hottest day of hottest month minus coldest day of coldest month

annual.temperature.range.sc3 <- warmest.day.warmest.month.sc3 - coldest.day.coldest.month.sc3

#value
annual.temperature.range.sc3

##Mean Temperature of Wettest Quarter

mean.temp.wettest.quarter.sc3 <- c(mean(q1.dat.sc3$TMeanF), mean(q2.dat.sc3$TMeanF), mean(q3.dat.sc3$TMeanF), mean(q4.dat.sc3$TMeanF),
                                   mean(q5.dat.sc3$TMeanF), mean(q6.dat.sc3$TMeanF), mean(q7.dat.sc3$TMeanF), mean(q8.dat.sc3$TMeanF),
                                   mean(q9.dat.sc3$TMeanF), mean(q10.dat.sc3$TMeanF), mean(q11.dat.sc3$TMeanF), mean(q12.dat.sc3$TMeanF)
)[which.max(c(mean(q1.dat.sc3$PPT_in)*90.25, mean(q2.dat.sc3$PPT_in)*89.25, mean(q3.dat.sc3$PPT_in)*92, mean(q4.dat.sc3$PPT_in)*91, 
              mean(q5.dat.sc3$PPT_in)*92, mean(q6.dat.sc3$PPT_in)*92, mean(q7.dat.sc3$PPT_in)*92, mean(q8.dat.sc3$PPT_in)*92, 
              mean(q9.dat.sc3$PPT_in)*91, mean(q10.dat.sc3$PPT_in)*92, mean(q11.dat.sc3$PPT_in)*92, mean(q12.dat.sc3$PPT_in)*90.25))]


#value
mean.temp.wettest.quarter.sc3

##Mean Temperature of Driest Quarter

mean.temp.driest.quarter.sc3 <- c(mean(q1.dat.sc3$TMeanF), mean(q2.dat.sc3$TMeanF), mean(q3.dat.sc3$TMeanF), mean(q4.dat.sc3$TMeanF),
                                  mean(q5.dat.sc3$TMeanF), mean(q6.dat.sc3$TMeanF), mean(q7.dat.sc3$TMeanF), mean(q8.dat.sc3$TMeanF),
                                  mean(q9.dat.sc3$TMeanF), mean(q10.dat.sc3$TMeanF), mean(q11.dat.sc3$TMeanF), mean(q12.dat.sc3$TMeanF)
)[which.min(c(mean(q1.dat.sc3$PPT_in)*90.25, mean(q2.dat.sc3$PPT_in)*89.25, mean(q3.dat.sc3$PPT_in)*92, mean(q4.dat.sc3$PPT_in)*91, 
              mean(q5.dat.sc3$PPT_in)*92, mean(q6.dat.sc3$PPT_in)*92, mean(q7.dat.sc3$PPT_in)*92, mean(q8.dat.sc3$PPT_in)*92, 
              mean(q9.dat.sc3$PPT_in)*91, mean(q10.dat.sc3$PPT_in)*92, mean(q11.dat.sc3$PPT_in)*92, mean(q12.dat.sc3$PPT_in)*90.25))]

#value
mean.temp.driest.quarter.sc3

##Mean Temperature of Warmest Quarter

mean.temp.warmest.quarter.sc3 <- c(mean(q1.dat.sc3$TMeanF), mean(q2.dat.sc3$TMeanF), mean(q3.dat.sc3$TMeanF), mean(q4.dat.sc3$TMeanF),
                                   mean(q5.dat.sc3$TMeanF), mean(q6.dat.sc3$TMeanF), mean(q7.dat.sc3$TMeanF), mean(q8.dat.sc3$TMeanF),
                                   mean(q9.dat.sc3$TMeanF), mean(q10.dat.sc3$TMeanF), mean(q11.dat.sc3$TMeanF), mean(q12.dat.sc3$TMeanF)
)[which.max(c(mean(q1.dat.sc3$TMeanF), mean(q2.dat.sc3$TMeanF), mean(q3.dat.sc3$TMeanF), mean(q4.dat.sc3$TMeanF), 
              mean(q5.dat.sc3$TMeanF), mean(q6.dat.sc3$TMeanF), mean(q7.dat.sc3$TMeanF), mean(q8.dat.sc3$TMeanF), 
              mean(q9.dat.sc3$TMeanF), mean(q10.dat.sc3$TMeanF), mean(q11.dat.sc3$TMeanF), mean(q12.dat.sc3$TMeanF)))]

#value
mean.temp.warmest.quarter.sc3

##Mean Temperature of Coldest Quarter

mean.temp.coldest.quarter.sc3 <- c(mean(q1.dat.sc3$TMeanF), mean(q2.dat.sc3$TMeanF), mean(q3.dat.sc3$TMeanF), mean(q4.dat.sc3$TMeanF),
                                   mean(q5.dat.sc3$TMeanF), mean(q6.dat.sc3$TMeanF), mean(q7.dat.sc3$TMeanF), mean(q8.dat.sc3$TMeanF),
                                   mean(q9.dat.sc3$TMeanF), mean(q10.dat.sc3$TMeanF), mean(q11.dat.sc3$TMeanF), mean(q12.dat.sc3$TMeanF)
)[which.min(c(mean(q1.dat.sc3$TMeanF), mean(q2.dat.sc3$TMeanF), mean(q3.dat.sc3$TMeanF), mean(q4.dat.sc3$TMeanF), 
              mean(q5.dat.sc3$TMeanF), mean(q6.dat.sc3$TMeanF), mean(q7.dat.sc3$TMeanF), mean(q8.dat.sc3$TMeanF), 
              mean(q9.dat.sc3$TMeanF), mean(q10.dat.sc3$TMeanF), mean(q11.dat.sc3$TMeanF), mean(q12.dat.sc3$TMeanF)))]

#value
mean.temp.coldest.quarter.sc3

##Precipitation of Wettest Month

precip.wettest.month.pre.sc3 <- c(jan.prav.dat.sc3, feb.prav.dat.sc3, mar.prav.dat.sc3, apr.prav.dat.sc3, may.prav.dat.sc3, jun.prav.dat.sc3,
                                  jul.prav.dat.sc3, aug.prav.dat.sc3, sep.prav.dat.sc3, oct.prav.dat.sc3, nov.prav.dat.sc3, dec.prav.dat.sc3
)[which.max(c(jan.prav.dat.sc3, feb.prav.dat.sc3, mar.prav.dat.sc3, apr.prav.dat.sc3, may.prav.dat.sc3, jun.prav.dat.sc3,
              jul.prav.dat.sc3, aug.prav.dat.sc3, sep.prav.dat.sc3, oct.prav.dat.sc3, nov.prav.dat.sc3, dec.prav.dat.sc3))]

#since we are using sums of precip for a given month over a projected period we need to divide
#to get the sum of one month's precip

precip.wettest.month.sc3 <- precip.wettest.month.pre.sc3

#value
precip.wettest.month.sc3

##Precipitation of Driest Month

precip.driest.month.pre.sc3 <- c(jan.prav.dat.sc3, feb.prav.dat.sc3, mar.prav.dat.sc3, apr.prav.dat.sc3, may.prav.dat.sc3, jun.prav.dat.sc3,
                                 jul.prav.dat.sc3, aug.prav.dat.sc3, sep.prav.dat.sc3, oct.prav.dat.sc3, nov.prav.dat.sc3, dec.prav.dat.sc3
)[which.min(c(jan.prav.dat.sc3, feb.prav.dat.sc3, mar.prav.dat.sc3, apr.prav.dat.sc3, may.prav.dat.sc3, jun.prav.dat.sc3,
              jul.prav.dat.sc3, aug.prav.dat.sc3, sep.prav.dat.sc3, oct.prav.dat.sc3, nov.prav.dat.sc3, dec.prav.dat.sc3))]

precip.driest.month.sc3 <- precip.driest.month.pre.sc3

#value
precip.driest.month.sc3

##Precipitation Seasonality (CV)
#SD(precip1-12)/1+average monthly precip   X 100

#SD(Tav1,2,3...)
monthly.means.precip.sc3 <- data.frame(c(jan.prav.dat.sc3, feb.prav.dat.sc3, mar.prav.dat.sc3, apr.prav.dat.sc3, may.prav.dat.sc3, jun.prav.dat.sc3,
                                         jul.prav.dat.sc3, aug.prav.dat.sc3, sep.prav.dat.sc3, oct.prav.dat.sc3, nov.prav.dat.sc3, dec.prav.dat.sc3))
#assign header to column
names(monthly.means.precip.sc3)[1]<-paste("praverage")

#now get the standard deviation of all twelve months
sd.precip.sc3 <- sd(monthly.means.precip.sc3$praverage)

#annual precipitation / getting the sum of period then dividing by 12 to get annual average
an.precip.sc3 <- (sum(dat.RCP8.5.2030$PPT_mm)/(future1_end_year - future1_start_year))
mo.precip.sc3 <- an.precip.sc3/12

#final calculation
precip.seasonality.cv.sc3 <- (sd.precip.sc3/(1+mo.precip.sc3))*100

#value
precip.seasonality.cv.sc3

##Precipitation of Wettest Quarter

precip.wettest.quarter.pre.sc3 <- c(sum(q1.dat.sc3$PPT_in), sum(q2.dat.sc3$PPT_in), sum(q3.dat.sc3$PPT_in), sum(q4.dat.sc3$PPT_in),
                                    sum(q5.dat.sc3$PPT_in), sum(q6.dat.sc3$PPT_in), sum(q7.dat.sc3$PPT_in), sum(q8.dat.sc3$PPT_in),
                                    sum(q9.dat.sc3$PPT_in), sum(q10.dat.sc3$PPT_in), sum(q11.dat.sc3$PPT_in), sum(q12.dat.sc3$PPT_in)
)[which.max(c(mean(q1.dat.sc3$PPT_in)*90.25, mean(q2.dat.sc3$PPT_in)*89.25, mean(q3.dat.sc3$PPT_in)*92, mean(q4.dat.sc3$PPT_in)*91, 
              mean(q5.dat.sc3$PPT_in)*92, mean(q6.dat.sc3$PPT_in)*92, mean(q7.dat.sc3$PPT_in)*92, mean(q8.dat.sc3$PPT_in)*92, 
              mean(q9.dat.sc3$PPT_in)*91, mean(q10.dat.sc3$PPT_in)*92, mean(q11.dat.sc3$PPT_in)*92, mean(q12.dat.sc3$PPT_in)*90.25))]

precip.wettest.quarter.sc3 <- precip.wettest.quarter.pre.sc3/(future1_end_year - future1_start_year)

#value
precip.wettest.quarter.sc3

##Precipitation of Driest Quarter

precip.driest.quarter.pre.sc3 <- c(sum(q1.dat.sc3$PPT_in), sum(q2.dat.sc3$PPT_in), sum(q3.dat.sc3$PPT_in), sum(q4.dat.sc3$PPT_in),
                                   sum(q5.dat.sc3$PPT_in), sum(q6.dat.sc3$PPT_in), sum(q7.dat.sc3$PPT_in), sum(q8.dat.sc3$PPT_in),
                                   sum(q9.dat.sc3$PPT_in), sum(q10.dat.sc3$PPT_in), sum(q11.dat.sc3$PPT_in), sum(q12.dat.sc3$PPT_in)
)[which.min(c(mean(q1.dat.sc3$PPT_in)*90.25, mean(q2.dat.sc3$PPT_in)*89.25, mean(q3.dat.sc3$PPT_in)*92, mean(q4.dat.sc3$PPT_in)*91, 
              mean(q5.dat.sc3$PPT_in)*92, mean(q6.dat.sc3$PPT_in)*92, mean(q7.dat.sc3$PPT_in)*92, mean(q8.dat.sc3$PPT_in)*92, 
              mean(q9.dat.sc3$PPT_in)*91, mean(q10.dat.sc3$PPT_in)*92, mean(q11.dat.sc3$PPT_in)*92, mean(q12.dat.sc3$PPT_in)*90.25))]


precip.driest.quarter.sc3 <- precip.driest.quarter.pre.sc3/(future1_end_year - future1_start_year)

#value
precip.driest.quarter.sc3

##Precipitation of Coldest Quarter

precip.coldest.quarter.pre.sc3 <- c(sum(q1.dat.sc3$PPT_in), sum(q2.dat.sc3$PPT_in), sum(q3.dat.sc3$PPT_in), sum(q4.dat.sc3$PPT_in),
                                    sum(q5.dat.sc3$PPT_in), sum(q6.dat.sc3$PPT_in), sum(q7.dat.sc3$PPT_in), sum(q8.dat.sc3$PPT_in),
                                    sum(q9.dat.sc3$PPT_in), sum(q10.dat.sc3$PPT_in), sum(q11.dat.sc3$PPT_in), sum(q12.dat.sc3$PPT_in)
)[which.min(c(mean(q1.dat.sc3$TMeanF), mean(q2.dat.sc3$TMeanF), mean(q3.dat.sc3$TMeanF), mean(q4.dat.sc3$TMeanF), 
              mean(q5.dat.sc3$TMeanF), mean(q6.dat.sc3$TMeanF), mean(q7.dat.sc3$TMeanF), mean(q8.dat.sc3$TMeanF), 
              mean(q9.dat.sc3$TMeanF), mean(q10.dat.sc3$TMeanF), mean(q11.dat.sc3$TMeanF), mean(q12.dat.sc3$TMeanF)))]

#previous calculation is the sum over historical period of said quarter
#divide to get the annual quarter calculation 
precip.coldest.quarter.sc3 <- precip.coldest.quarter.pre.sc3/(future1_end_year - future1_start_year)

#value
precip.coldest.quarter.sc3

##Precipitation of the Warmest Quarter

precip.warmest.quarter.pre.sc3 <- c(sum(q1.dat.sc3$PPT_in), sum(q2.dat.sc3$PPT_in), sum(q3.dat.sc3$PPT_in), sum(q4.dat.sc3$PPT_in),
                                    sum(q5.dat.sc3$PPT_in), sum(q6.dat.sc3$PPT_in), sum(q7.dat.sc3$PPT_in), sum(q8.dat.sc3$PPT_in),
                                    sum(q9.dat.sc3$PPT_in), sum(q10.dat.sc3$PPT_in), sum(q11.dat.sc3$PPT_in), sum(q12.dat.sc3$PPT_in)
)[which.max(c(mean(q1.dat.sc3$TMeanF), mean(q2.dat.sc3$TMeanF), mean(q3.dat.sc3$TMeanF), mean(q4.dat.sc3$TMeanF), 
              mean(q5.dat.sc3$TMeanF), mean(q6.dat.sc3$TMeanF), mean(q7.dat.sc3$TMeanF), mean(q8.dat.sc3$TMeanF), 
              mean(q9.dat.sc3$TMeanF), mean(q10.dat.sc3$TMeanF), mean(q11.dat.sc3$TMeanF), mean(q12.dat.sc3$TMeanF)))]

#previous calculation is the sum over projected period of said quarter
#divide to get the annual quarter calculation 
precip.warmest.quarter.sc3 <- precip.warmest.quarter.pre.sc3/(future1_end_year - future1_start_year)

#value
precip.warmest.quarter.sc3


#####RCP 8.5 2050

#setting aside appropriate data

sc4.temp.av <- c(dat.RCP8.5.2050$TMeanF)
sc4.temp.hi <- c(dat.RCP8.5.2050$TMaxF)
sc4.temp.lo <- c(dat.RCP8.5.2050$TMinF)
sc4.precip <-  c(dat.RCP8.5.2050$PPT_in)

#subsetting monthly data

jan.dat.sc4 <- subset(dat.RCP8.5.2050, month=="Jan")
feb.dat.sc4 <- subset(dat.RCP8.5.2050, month=="Feb")
mar.dat.sc4 <- subset(dat.RCP8.5.2050, month=="Mar")
apr.dat.sc4 <- subset(dat.RCP8.5.2050, month=="Apr")
may.dat.sc4 <- subset(dat.RCP8.5.2050, month=="May")
jun.dat.sc4 <- subset(dat.RCP8.5.2050, month=="Jun")
jul.dat.sc4 <- subset(dat.RCP8.5.2050, month=="Jul")
aug.dat.sc4 <- subset(dat.RCP8.5.2050, month=="Aug")
sep.dat.sc4 <- subset(dat.RCP8.5.2050, month=="Sep")
oct.dat.sc4 <- subset(dat.RCP8.5.2050, month=="Oct")
nov.dat.sc4 <- subset(dat.RCP8.5.2050, month=="Nov") #, na.rm=TRUE #November is sometimes wonky
dec.dat.sc4 <- subset(dat.RCP8.5.2050, month=="Dec")

#subsetting monthly tav

jan.tav.sc4 <- mean(jan.dat.sc4$TMeanF)
feb.tav.sc4 <- mean(feb.dat.sc4$TMeanF)
mar.tav.sc4 <- mean(mar.dat.sc4$TMeanF)
apr.tav.sc4 <- mean(apr.dat.sc4$TMeanF)
may.tav.sc4 <- mean(may.dat.sc4$TMeanF)
jun.tav.sc4 <- mean(jun.dat.sc4$TMeanF)
jul.tav.sc4 <- mean(jul.dat.sc4$TMeanF)
aug.tav.sc4 <- mean(aug.dat.sc4$TMeanF)
sep.tav.sc4 <- mean(sep.dat.sc4$TMeanF)
oct.tav.sc4 <- mean(oct.dat.sc4$TMeanF)
nov.tav.sc4 <- mean(nov.dat.sc4$TMeanF)
dec.tav.sc4 <- mean(dec.dat.sc4$TMeanF)

#subsetting different quarters

q1.dat.sc4 <- rbind(jan.dat.sc4, feb.dat.sc4, mar.dat.sc4)
q2.dat.sc4 <- rbind(feb.dat.sc4, mar.dat.sc4, apr.dat.sc4)
q3.dat.sc4 <- rbind(mar.dat.sc4, apr.dat.sc4, may.dat.sc4)
q4.dat.sc4 <- rbind(apr.dat.sc4, may.dat.sc4, jun.dat.sc4)
q5.dat.sc4 <- rbind(may.dat.sc4, jun.dat.sc4, jul.dat.sc4)
q6.dat.sc4 <- rbind(jun.dat.sc4, jul.dat.sc4, aug.dat.sc4)
q7.dat.sc4 <- rbind(jul.dat.sc4, aug.dat.sc4, sep.dat.sc4)
q8.dat.sc4 <- rbind(aug.dat.sc4, sep.dat.sc4, oct.dat.sc4)
q9.dat.sc4 <- rbind(sep.dat.sc4, oct.dat.sc4, nov.dat.sc4)
q10.dat.sc4 <- rbind(oct.dat.sc4, nov.dat.sc4, dec.dat.sc4)
q11.dat.sc4 <- rbind(nov.dat.sc4, dec.dat.sc4, jan.dat.sc4)
q12.dat.sc4 <- rbind(dec.dat.sc4, jan.dat.sc4, feb.dat.sc4)

#subsetting precipitation data

jan.prav.dat.sc4 <- (sum(jan.dat.sc4$PPT_in))/(future2_end_year - future2_start_year)
feb.prav.dat.sc4 <- (sum(feb.dat.sc4$PPT_in))/(future2_end_year - future2_start_year)
mar.prav.dat.sc4 <- (sum(mar.dat.sc4$PPT_in))/(future2_end_year - future2_start_year)
apr.prav.dat.sc4 <- (sum(apr.dat.sc4$PPT_in))/(future2_end_year - future2_start_year)
may.prav.dat.sc4 <- (sum(may.dat.sc4$PPT_in))/(future2_end_year - future2_start_year)
jun.prav.dat.sc4 <- (sum(jun.dat.sc4$PPT_in))/(future2_end_year - future2_start_year)
jul.prav.dat.sc4 <- (sum(jul.dat.sc4$PPT_in))/(future2_end_year - future2_start_year)
aug.prav.dat.sc4 <- (sum(aug.dat.sc4$PPT_in))/(future2_end_year - future2_start_year)
sep.prav.dat.sc4 <- (sum(sep.dat.sc4$PPT_in))/(future2_end_year - future2_start_year)
oct.prav.dat.sc4 <- (sum(oct.dat.sc4$PPT_in))/(future2_end_year - future2_start_year)
nov.prav.dat.sc4 <- (sum(nov.dat.sc4$PPT_in))/(future2_end_year - future2_start_year)
dec.prav.dat.sc4 <- (sum(dec.dat.sc4$PPT_in))/(future2_end_year - future2_start_year)

## 1) Annual Mean Diurnal Range
#difference between month's max and min temp averaged over 12 months
#SUMof(Tmax-Tmin)/12

jan.max.sc4 <- mean(jan.dat.sc4$TMaxF)
jan.min.sc4 <- mean(jan.dat.sc4$TMinF)

feb.max.sc4 <- mean(feb.dat.sc4$TMaxF)
feb.min.sc4 <- mean(feb.dat.sc4$TMinF)

mar.max.sc4 <- mean(mar.dat.sc4$TMaxF)
mar.min.sc4 <- mean(mar.dat.sc4$TMinF)

apr.max.sc4 <- mean(apr.dat.sc4$TMaxF)
apr.min.sc4 <- mean(apr.dat.sc4$TMinF)

may.max.sc4 <- mean(may.dat.sc4$TMaxF)
may.min.sc4 <- mean(may.dat.sc4$TMinF)

jun.max.sc4 <- mean(jun.dat.sc4$TMaxF)
jun.min.sc4 <- mean(jun.dat.sc4$TMinF)

jul.max.sc4 <- mean(jul.dat.sc4$TMaxF)
jul.min.sc4 <- mean(jul.dat.sc4$TMinF)

aug.max.sc4 <- mean(aug.dat.sc4$TMaxF)
aug.min.sc4 <- mean(aug.dat.sc4$TMinF)

sep.max.sc4 <- mean(sep.dat.sc4$TMaxF)
sep.min.sc4 <- mean(sep.dat.sc4$TMinF)

oct.max.sc4 <- mean(oct.dat.sc4$TMaxF)
oct.min.sc4 <- mean(oct.dat.sc4$TMinF)

nov.max.sc4 <- mean(nov.dat.sc4$TMaxF)
nov.min.sc4 <- mean(nov.dat.sc4$TMinF)

dec.max.sc4 <- mean(dec.dat.sc4$TMaxF)
dec.min.sc4 <- mean(dec.dat.sc4$TMinF)

#differences
jan.dif.sc4 <- jan.max.sc4-jan.min.sc4
feb.dif.sc4 <- feb.max.sc4-feb.min.sc4
mar.dif.sc4 <- mar.max.sc4-mar.min.sc4
apr.dif.sc4 <- apr.max.sc4-apr.min.sc4
may.dif.sc4 <- may.max.sc4-may.min.sc4
jun.dif.sc4 <- jun.max.sc4-jun.min.sc4
jul.dif.sc4 <- jul.max.sc4-jul.min.sc4
aug.dif.sc4 <- aug.max.sc4-aug.min.sc4
sep.dif.sc4 <- sep.max.sc4-sep.min.sc4
oct.dif.sc4 <- oct.max.sc4-oct.min.sc4
nov.dif.sc4 <- nov.max.sc4-nov.min.sc4
dec.dif.sc4 <- dec.max.sc4-dec.min.sc4

#sum of all months annual mean diurnal range
amdr.sum.sc4 <- sum(jan.dif.sc4, feb.dif.sc4, mar.dif.sc4, apr.dif.sc4, may.dif.sc4, jun.dif.sc4, 
                    jul.dif.sc4, aug.dif.sc4, sep.dif.sc4, oct.dif.sc4, nov.dif.sc4, dec.dif.sc4)

#divide by 12 to get one months value
annual.mean.diurnal.range.sc4 <- (amdr.sum.sc4/12)

#value

annual.mean.diurnal.range.sc4

##Isothermality
#units in percent
#how large day to night temperatures oscilate relative to the summer to winter oscillations
#ratio of mean diurnal range to the annual temperature range
#(annual.mean.diurnal.range/annual.temp.range)*100

#conversion formula f to c
f.to.c <- function(temp) {
  celsius <- ((temp -32) *(5/9))
  return(celsius)
}

#absolute max and min
abs.tmax.pre.sc4 <- max(dat.RCP8.5.2050$TMaxF)
abs.tmax.c.sc4 <- f.to.c(abs.tmax.pre.sc4)
abs.tmin.pre.sc4 <- min(dat.RCP8.5.2050$TMinF)
abs.tmin.c.sc4 <- f.to.c(abs.tmin.pre.sc4)


#annual temp range
annual.temp.range.sc4 <- abs.tmax.c.sc4 - abs.tmin.c.sc4

isothermality.sc4 <- (annual.mean.diurnal.range.sc4/annual.temp.range.sc4)*100

#value
isothermality.sc4

##Temperature Seasonality SD
#amount of temperature variation over a given year (or averaged years) based on SD of monthly
#temp averages
#SD(Tav1,2,3...)
monthly.means.sc4 <- data.frame(c(jan.tav.sc4, feb.tav.sc4, mar.tav.sc4, apr.tav.sc4, may.tav.sc4, jun.tav.sc4,
                                  jul.tav.sc4, aug.tav.sc4, sep.tav.sc4, oct.tav.sc4, nov.tav.sc4, dec.tav.sc4))
#assign header to column
names(monthly.means.sc4)[1]<-paste("faverage")

#convert it to celsius
monthly.means.c.sc4 <- f.to.c(monthly.means.sc4)

#assign header to column
names(monthly.means.c.sc4)[1]<-paste("caverage")

#now get the standard deviation of all twelve months
sd.temperature.seasonality.sc4 <- sd(monthly.means.c.sc4$caverage)

#value
sd.temperature.seasonality.sc4

##Temperature Seasonality (CV)
#SD(TKav1-12)/annual mean temp in K
#need to use Kelvin for this one so lets create a function

f.to.k <- function(temp) {
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}

#function created, lets get Kelvin values for each month
jan.k.sc4 <- f.to.k(jan.tav.sc4)
feb.k.sc4 <- f.to.k(feb.tav.sc4)
mar.k.sc4 <- f.to.k(mar.tav.sc4)
apr.k.sc4 <- f.to.k(apr.tav.sc4)
may.k.sc4 <- f.to.k(may.tav.sc4)
jun.k.sc4 <- f.to.k(jun.tav.sc4)
jul.k.sc4 <- f.to.k(jul.tav.sc4)
aug.k.sc4 <- f.to.k(aug.tav.sc4)
sep.k.sc4 <- f.to.k(sep.tav.sc4)
oct.k.sc4 <- f.to.k(oct.tav.sc4)
nov.k.sc4 <- f.to.k(nov.tav.sc4)
dec.k.sc4 <- f.to.k(dec.tav.sc4)

#create a data frame

monthly.means.k.sc4 <- data.frame(c(jan.k.sc4, feb.k.sc4, mar.k.sc4, apr.k.sc4, may.k.sc4, jun.k.sc4,
                                    jul.k.sc4, aug.k.sc4, sep.k.sc4, oct.k.sc4, nov.k.sc4, dec.k.sc4))

#assign header to column
names(monthly.means.k.sc4)[1]<-paste("kaverage")

sd.mmk.sc4 <- sd(monthly.means.k.sc4$kaverage)

#determine annual mean temp
annual.av.f.sc4 <- mean(dat.RCP8.5.2050$TMeanF)

#convert it to Kelvin
annual.av.k.sc4. <- f.to.k(annual.av.f.sc4)

##Temperature Seasonality CV
cv.temperature.seasonality.sc4 <- ((sd.mmk.sc4)/annual.av.k.sc4.)*100

#value
cv.temperature.seasonality.sc4

##Max Temperature of Warmest Month
#lets first figure out the warmest month
#saving it in case we need it
warmest.day.warmest.month.sc4 <- c(max(jan.dat.sc4$TMaxF), max(feb.dat.sc4$TMaxF), max(mar.dat.sc4$TMaxF), max(apr.dat.sc4$TMaxF),
                                   max(may.dat.sc4$TMaxF), max(jun.dat.sc4$TMaxF), max(jul.dat.sc4$TMaxF), max(aug.dat.sc4$TMaxF),
                                   max(sep.dat.sc4$TMaxF), max(oct.dat.sc4$TMaxF), max(nov.dat.sc4$TMaxF), max(dec.dat.sc4$TMaxF)
)[which.max(c(mean(jan.dat.sc4$TMaxF), mean(feb.dat.sc4$TMaxF), mean(mar.dat.sc4$TMaxF), mean(apr.dat.sc4$TMaxF), 
              mean(may.dat.sc4$TMaxF), mean(jun.dat.sc4$TMaxF), mean(jul.dat.sc4$TMaxF), mean(aug.dat.sc4$TMaxF), 
              mean(sep.dat.sc4$TMaxF), mean(oct.dat.sc4$TMaxF), mean(nov.dat.sc4$TMaxF), mean(dec.dat.sc4$TMaxF)))]

#value
warmest.day.warmest.month.sc4

##Min Temperature of Coldest Month
#saving it in case they want it
coldest.day.coldest.month.sc4 <- c(min(jan.dat.sc4$TMinF), min(feb.dat.sc4$TMinF), min(mar.dat.sc4$TMinF), min(apr.dat.sc4$TMinF),
                                   min(may.dat.sc4$TMinF), min(jun.dat.sc4$TMinF), min(jul.dat.sc4$TMinF), min(aug.dat.sc4$TMinF),
                                   min(sep.dat.sc4$TMinF), min(oct.dat.sc4$TMinF), min(nov.dat.sc4$TMinF), min(dec.dat.sc4$TMinF)
)[which.min(c(mean(jan.dat.sc4$TMinF), mean(feb.dat.sc4$TMinF), mean(mar.dat.sc4$TMinF), mean(apr.dat.sc4$TMinF), 
              mean(may.dat.sc4$TMinF), mean(jun.dat.sc4$TMinF), mean(jul.dat.sc4$TMinF), mean(aug.dat.sc4$TMinF), 
              mean(sep.dat.sc4$TMinF), mean(oct.dat.sc4$TMinF), mean(nov.dat.sc4$TMinF), mean(dec.dat.sc4$TMinF)))]

#value
coldest.day.coldest.month.sc4

##Annual Temperature Range
#hottest day of hottest month minus coldest day of coldest month

annual.temperature.range.sc4 <- warmest.day.warmest.month.sc4 - coldest.day.coldest.month.sc4

#value
annual.temperature.range.sc4

##Mean Temperature of Wettest Quarter

mean.temp.wettest.quarter.sc4 <- c(mean(q1.dat.sc4$TMeanF), mean(q2.dat.sc4$TMeanF), mean(q3.dat.sc4$TMeanF), mean(q4.dat.sc4$TMeanF),
                                   mean(q5.dat.sc4$TMeanF), mean(q6.dat.sc4$TMeanF), mean(q7.dat.sc4$TMeanF), mean(q8.dat.sc4$TMeanF),
                                   mean(q9.dat.sc4$TMeanF), mean(q10.dat.sc4$TMeanF), mean(q11.dat.sc4$TMeanF), mean(q12.dat.sc4$TMeanF)
)[which.max(c(mean(q1.dat.sc4$PPT_in)*90.25, mean(q2.dat.sc4$PPT_in)*89.25, mean(q3.dat.sc4$PPT_in)*92, mean(q4.dat.sc4$PPT_in)*91, 
              mean(q5.dat.sc4$PPT_in)*92, mean(q6.dat.sc4$PPT_in)*92, mean(q7.dat.sc4$PPT_in)*92, mean(q8.dat.sc4$PPT_in)*92, 
              mean(q9.dat.sc4$PPT_in)*91, mean(q10.dat.sc4$PPT_in)*92, mean(q11.dat.sc4$PPT_in)*92, mean(q12.dat.sc4$PPT_in)*90.25))]

#value
mean.temp.wettest.quarter.sc4

##Mean Temperature of Driest Quarter

mean.temp.driest.quarter.sc4 <- c(mean(q1.dat.sc4$TMeanF), mean(q2.dat.sc4$TMeanF), mean(q3.dat.sc4$TMeanF), mean(q4.dat.sc4$TMeanF),
                                  mean(q5.dat.sc4$TMeanF), mean(q6.dat.sc4$TMeanF), mean(q7.dat.sc4$TMeanF), mean(q8.dat.sc4$TMeanF),
                                  mean(q9.dat.sc4$TMeanF), mean(q10.dat.sc4$TMeanF), mean(q11.dat.sc4$TMeanF), mean(q12.dat.sc4$TMeanF)
)[which.min(c(mean(q1.dat.sc4$PPT_in)*90.25, mean(q2.dat.sc4$PPT_in)*89.25, mean(q3.dat.sc4$PPT_in)*92, mean(q4.dat.sc4$PPT_in)*91, 
              mean(q5.dat.sc4$PPT_in)*92, mean(q6.dat.sc4$PPT_in)*92, mean(q7.dat.sc4$PPT_in)*92, mean(q8.dat.sc4$PPT_in)*92, 
              mean(q9.dat.sc4$PPT_in)*91, mean(q10.dat.sc4$PPT_in)*92, mean(q11.dat.sc4$PPT_in)*92, mean(q12.dat.sc4$PPT_in)*90.25))]

#value
mean.temp.driest.quarter.sc4

##Mean Temperature of Warmest Quarter

mean.temp.warmest.quarter.sc4 <- c(mean(q1.dat.sc4$TMeanF), mean(q2.dat.sc4$TMeanF), mean(q3.dat.sc4$TMeanF), mean(q4.dat.sc4$TMeanF),
                                   mean(q5.dat.sc4$TMeanF), mean(q6.dat.sc4$TMeanF), mean(q7.dat.sc4$TMeanF), mean(q8.dat.sc4$TMeanF),
                                   mean(q9.dat.sc4$TMeanF), mean(q10.dat.sc4$TMeanF), mean(q11.dat.sc4$TMeanF), mean(q12.dat.sc4$TMeanF)
)[which.max(c(mean(q1.dat.sc4$TMeanF), mean(q2.dat.sc4$TMeanF), mean(q3.dat.sc4$TMeanF), mean(q4.dat.sc4$TMeanF), 
              mean(q5.dat.sc4$TMeanF), mean(q6.dat.sc4$TMeanF), mean(q7.dat.sc4$TMeanF), mean(q8.dat.sc4$TMeanF), 
              mean(q9.dat.sc4$TMeanF), mean(q10.dat.sc4$TMeanF), mean(q11.dat.sc4$TMeanF), mean(q12.dat.sc4$TMeanF)))]

#value
mean.temp.warmest.quarter.sc4

##Mean Temperature of Coldest Quarter

mean.temp.coldest.quarter.sc4 <- c(mean(q1.dat.sc4$TMeanF), mean(q2.dat.sc4$TMeanF), mean(q3.dat.sc4$TMeanF), mean(q4.dat.sc4$TMeanF),
                                   mean(q5.dat.sc4$TMeanF), mean(q6.dat.sc4$TMeanF), mean(q7.dat.sc4$TMeanF), mean(q8.dat.sc4$TMeanF),
                                   mean(q9.dat.sc4$TMeanF), mean(q10.dat.sc4$TMeanF), mean(q11.dat.sc4$TMeanF), mean(q12.dat.sc4$TMeanF)
)[which.min(c(mean(q1.dat.sc4$TMeanF), mean(q2.dat.sc4$TMeanF), mean(q3.dat.sc4$TMeanF), mean(q4.dat.sc4$TMeanF), 
              mean(q5.dat.sc4$TMeanF), mean(q6.dat.sc4$TMeanF), mean(q7.dat.sc4$TMeanF), mean(q8.dat.sc4$TMeanF), 
              mean(q9.dat.sc4$TMeanF), mean(q10.dat.sc4$TMeanF), mean(q11.dat.sc4$TMeanF), mean(q12.dat.sc4$TMeanF)))]

#value
mean.temp.coldest.quarter.sc4

##Precipitation of Wettest Month

precip.wettest.month.pre.sc4 <- c(jan.prav.dat.sc4, feb.prav.dat.sc4, mar.prav.dat.sc4, apr.prav.dat.sc4, may.prav.dat.sc4, jun.prav.dat.sc4,
                                  jul.prav.dat.sc4, aug.prav.dat.sc4, sep.prav.dat.sc4, oct.prav.dat.sc4, nov.prav.dat.sc4, dec.prav.dat.sc4
)[which.max(c(jan.prav.dat.sc4, feb.prav.dat.sc4, mar.prav.dat.sc4, apr.prav.dat.sc4, may.prav.dat.sc4, jun.prav.dat.sc4,
              jul.prav.dat.sc4, aug.prav.dat.sc4, sep.prav.dat.sc4, oct.prav.dat.sc4, nov.prav.dat.sc4, dec.prav.dat.sc4))]

precip.wettest.month.sc4 <- precip.wettest.month.pre.sc4

#value
precip.wettest.month.sc4

##Precipitation of Driest Month

precip.driest.month.pre.sc4 <- c(jan.prav.dat.sc4, feb.prav.dat.sc4, mar.prav.dat.sc4, apr.prav.dat.sc4, may.prav.dat.sc4, jun.prav.dat.sc4,
                                 jul.prav.dat.sc4, aug.prav.dat.sc4, sep.prav.dat.sc4, oct.prav.dat.sc4, nov.prav.dat.sc4, dec.prav.dat.sc4
)[which.min(c(jan.prav.dat.sc4, feb.prav.dat.sc4, mar.prav.dat.sc4, apr.prav.dat.sc4, may.prav.dat.sc4, jun.prav.dat.sc4,
              jul.prav.dat.sc4, aug.prav.dat.sc4, sep.prav.dat.sc4, oct.prav.dat.sc4, nov.prav.dat.sc4, dec.prav.dat.sc4))]

precip.driest.month.sc4 <- precip.driest.month.pre.sc4

#value
precip.driest.month.sc4

##Precipitation Seasonality (CV)
#SD(precip1-12)/1+average monthly precip   X 100

#SD(Tav1,2,3...)
monthly.means.precip.sc4 <- data.frame(c(jan.prav.dat.sc4, feb.prav.dat.sc4, mar.prav.dat.sc4, apr.prav.dat.sc4, may.prav.dat.sc4, jun.prav.dat.sc4,
                                         jul.prav.dat.sc4, aug.prav.dat.sc4, sep.prav.dat.sc4, oct.prav.dat.sc4, nov.prav.dat.sc4, dec.prav.dat.sc4))
#assign header to column
names(monthly.means.precip.sc4)[1]<-paste("praverage")

#now get the standard deviation of all twelve months
sd.precip.sc4 <- sd(monthly.means.precip.sc4$praverage)

#annual precipitation / getting the sum of years then dividing by 12 to get annual average
an.precip.sc4 <- (sum(dat.RCP8.5.2050$PPT_mm)/(future2_end_year - future2_start_year))
mo.precip.sc4 <- an.precip.sc4/12

#final calculation
precip.seasonality.cv.sc4 <- (sd.precip.sc4/(1+mo.precip.sc4))*100

#value
precip.seasonality.cv.sc4

##Precipitation of Wettest Quarter

precip.wettest.quarter.pre.sc4 <- c(sum(q1.dat.sc4$PPT_in), sum(q2.dat.sc4$PPT_in), sum(q3.dat.sc4$PPT_in), sum(q4.dat.sc4$PPT_in),
                                    sum(q5.dat.sc4$PPT_in), sum(q6.dat.sc4$PPT_in), sum(q7.dat.sc4$PPT_in), sum(q8.dat.sc4$PPT_in),
                                    sum(q9.dat.sc4$PPT_in), sum(q10.dat.sc4$PPT_in), sum(q11.dat.sc4$PPT_in), sum(q12.dat.sc4$PPT_in)
)[which.max(c(mean(q1.dat.sc4$PPT_in)*90.25, mean(q2.dat.sc4$PPT_in)*89.25, mean(q3.dat.sc4$PPT_in)*92, mean(q4.dat.sc4$PPT_in)*91, 
              mean(q5.dat.sc4$PPT_in)*92, mean(q6.dat.sc4$PPT_in)*92, mean(q7.dat.sc4$PPT_in)*92, mean(q8.dat.sc4$PPT_in)*92, 
              mean(q9.dat.sc4$PPT_in)*91, mean(q10.dat.sc4$PPT_in)*92, mean(q11.dat.sc4$PPT_in)*92, mean(q12.dat.sc4$PPT_in)*90.25))]

precip.wettest.quarter.sc4 <- precip.wettest.quarter.pre.sc4/(future2_end_year - future2_start_year)

#value
precip.wettest.quarter.sc4

##Precipitation of Driest Quarter

precip.driest.quarter.pre.sc4 <- c(sum(q1.dat.sc4$PPT_in), sum(q2.dat.sc4$PPT_in), sum(q3.dat.sc4$PPT_in), sum(q4.dat.sc4$PPT_in),
                                   sum(q5.dat.sc4$PPT_in), sum(q6.dat.sc4$PPT_in), sum(q7.dat.sc4$PPT_in), sum(q8.dat.sc4$PPT_in),
                                   sum(q9.dat.sc4$PPT_in), sum(q10.dat.sc4$PPT_in), sum(q11.dat.sc4$PPT_in), sum(q12.dat.sc4$PPT_in)
)[which.min(c(mean(q1.dat.sc4$PPT_in)*90.25, mean(q2.dat.sc4$PPT_in)*89.25, mean(q3.dat.sc4$PPT_in)*92, mean(q4.dat.sc4$PPT_in)*91, 
              mean(q5.dat.sc4$PPT_in)*92, mean(q6.dat.sc4$PPT_in)*92, mean(q7.dat.sc4$PPT_in)*92, mean(q8.dat.sc4$PPT_in)*92, 
              mean(q9.dat.sc4$PPT_in)*91, mean(q10.dat.sc4$PPT_in)*92, mean(q11.dat.sc4$PPT_in)*92, mean(q12.dat.sc4$PPT_in)*90.25))]

precip.driest.quarter.sc4 <- precip.driest.quarter.pre.sc4/(future2_end_year - future2_start_year)

#value
precip.driest.quarter.sc4

##Precipitation of Coldest Quarter

precip.coldest.quarter.pre.sc4 <- c(sum(q1.dat.sc4$PPT_in), sum(q2.dat.sc4$PPT_in), sum(q3.dat.sc4$PPT_in), sum(q4.dat.sc4$PPT_in),
                                    sum(q5.dat.sc4$PPT_in), sum(q6.dat.sc4$PPT_in), sum(q7.dat.sc4$PPT_in), sum(q8.dat.sc4$PPT_in),
                                    sum(q9.dat.sc4$PPT_in), sum(q10.dat.sc4$PPT_in), sum(q11.dat.sc4$PPT_in), sum(q12.dat.sc4$PPT_in)
)[which.min(c(mean(q1.dat.sc4$TMeanF), mean(q2.dat.sc4$TMeanF), mean(q3.dat.sc4$TMeanF), mean(q4.dat.sc4$TMeanF), 
              mean(q5.dat.sc4$TMeanF), mean(q6.dat.sc4$TMeanF), mean(q7.dat.sc4$TMeanF), mean(q8.dat.sc4$TMeanF), 
              mean(q9.dat.sc4$TMeanF), mean(q10.dat.sc4$TMeanF), mean(q11.dat.sc4$TMeanF), mean(q12.dat.sc4$TMeanF)))]

#previous calculation is the sum over historical period of said quarter
#divide to get the annual quarter calculation 
precip.coldest.quarter.sc4 <- precip.coldest.quarter.pre.sc4/(future2_end_year - future2_start_year)

#value
precip.coldest.quarter.sc4

##Precipitation of Warmest Quarter

precip.warmest.quarter.pre.sc4 <- c(sum(q1.dat.sc4$PPT_in), sum(q2.dat.sc4$PPT_in), sum(q3.dat.sc4$PPT_in), sum(q4.dat.sc4$PPT_in),
                                    sum(q5.dat.sc4$PPT_in), sum(q6.dat.sc4$PPT_in), sum(q7.dat.sc4$PPT_in), sum(q8.dat.sc4$PPT_in),
                                    sum(q9.dat.sc4$PPT_in), sum(q10.dat.sc4$PPT_in), sum(q11.dat.sc4$PPT_in), sum(q12.dat.sc4$PPT_in)
)[which.max(c(mean(q1.dat.sc4$TMeanF), mean(q2.dat.sc4$TMeanF), mean(q3.dat.sc4$TMeanF), mean(q4.dat.sc4$TMeanF), 
              mean(q5.dat.sc4$TMeanF), mean(q6.dat.sc4$TMeanF), mean(q7.dat.sc4$TMeanF), mean(q8.dat.sc4$TMeanF), 
              mean(q9.dat.sc4$TMeanF), mean(q10.dat.sc4$TMeanF), mean(q11.dat.sc4$TMeanF), mean(q12.dat.sc4$TMeanF)))]

#previous calculation is the sum over projected period of said quarter
#divide to get the annual quarter calculation 
precip.warmest.quarter.sc4 <- precip.warmest.quarter.pre.sc4/(future2_end_year - future2_start_year)

#value
precip.warmest.quarter.sc4


##Create a dataframe then export it!

env.var <- matrix(c(1:90), ncol=5, byrow=TRUE)

#test#
env.var <- matrix(c(annual.mean.diurnal.range,
                    annual.mean.diurnal.range.sc1,
                    annual.mean.diurnal.range.sc2,
                    annual.mean.diurnal.range.sc3,
                    annual.mean.diurnal.range.sc4,
                    isothermality,
                    isothermality.sc1,
                    isothermality.sc2,
                    isothermality.sc3,
                    isothermality.sc4,
                    sd.temperature.seasonality,
                    sd.temperature.seasonality.sc1,
                    sd.temperature.seasonality.sc2,
                    sd.temperature.seasonality.sc3,
                    sd.temperature.seasonality.sc4,
                    cv.temperature.seasonality,
                    cv.temperature.seasonality.sc1,
                    cv.temperature.seasonality.sc2,
                    cv.temperature.seasonality.sc3,
                    cv.temperature.seasonality.sc4,
                    warmest.day.warmest.month,
                    warmest.day.warmest.month.sc1,
                    warmest.day.warmest.month.sc2,
                    warmest.day.warmest.month.sc3,
                    warmest.day.warmest.month.sc4,
                    coldest.day.coldest.month,
                    coldest.day.coldest.month.sc1,
                    coldest.day.coldest.month.sc2,
                    coldest.day.coldest.month.sc3,
                    coldest.day.coldest.month.sc4,
                    annual.temperature.range,
                    annual.temperature.range.sc1,
                    annual.temperature.range.sc2,
                    annual.temperature.range.sc3,
                    annual.temperature.range.sc4,
                    mean.temp.wettest.quarter,
                    mean.temp.wettest.quarter.sc1,
                    mean.temp.wettest.quarter.sc2,
                    mean.temp.wettest.quarter.sc3,
                    mean.temp.wettest.quarter.sc4,
                    mean.temp.driest.quarter,
                    mean.temp.driest.quarter.sc1,
                    mean.temp.driest.quarter.sc2,
                    mean.temp.driest.quarter.sc3,
                    mean.temp.driest.quarter.sc4,
                    mean.temp.warmest.quarter,
                    mean.temp.warmest.quarter.sc1,
                    mean.temp.warmest.quarter.sc2,
                    mean.temp.warmest.quarter.sc3,
                    mean.temp.warmest.quarter.sc4,
                    mean.temp.coldest.quarter,
                    mean.temp.coldest.quarter.sc1,
                    mean.temp.coldest.quarter.sc2,
                    mean.temp.coldest.quarter.sc3,
                    mean.temp.coldest.quarter.sc4,
                    precip.wettest.month,
                    precip.wettest.month.sc1,
                    precip.wettest.month.sc2,
                    precip.wettest.month.sc3,
                    precip.wettest.month.sc4,
                    precip.driest.month,
                    precip.driest.month.sc1,
                    precip.driest.month.sc2,
                    precip.driest.month.sc3,
                    precip.driest.month.sc4,
                    precip.seasonality.cv,
                    precip.seasonality.cv.sc1,
                    precip.seasonality.cv.sc2,
                    precip.seasonality.cv.sc3,
                    precip.seasonality.cv.sc4,
                    precip.wettest.quarter,
                    precip.wettest.quarter.sc1,
                    precip.wettest.quarter.sc2,
                    precip.wettest.quarter.sc3,
                    precip.wettest.quarter.sc4,
                    precip.driest.quarter,
                    precip.driest.quarter.sc1,
                    precip.driest.quarter.sc2,
                    precip.driest.quarter.sc3,
                    precip.driest.quarter.sc4,
                    precip.coldest.quarter,
                    precip.coldest.quarter.sc1,
                    precip.coldest.quarter.sc2,
                    precip.coldest.quarter.sc3,
                    precip.coldest.quarter.sc4,
                    precip.warmest.quarter,
                    precip.warmest.quarter.sc1,
                    precip.warmest.quarter.sc2,
                    precip.warmest.quarter.sc3,
                    precip.warmest.quarter.sc4),
                  ncol=5, byrow=TRUE)

#adding column and row names

colnames(env.var) <- c("Historical", "SSP2-4.5 2035", "SSP2-4.5 2065", "SSP5-8.5 2035", "SSP5-8.5 2065")
rownames(env.var) <- c("Annual Mean Diurnal Range, ?F", 
                       "Isothermality, %", 
                       "Temperature Seasonality (Standard Deviation), °F",
                       "Temperature Seasonality (Coefficient of Variation), %", 
                       "Max Temperature of Warmest Month, °F", 
                       "Min Temperature of Coldest Month, °F",
                       "Annual Temperature Range, ?F", 
                       "Mean Temperature of Wettest Quarter, °F", 
                       "Mean Temperature of Driest Quarter, °F",
                       "Mean Temperature of Warmest Quarter, °F",
                       "Mean Temperature of Coldest Quarter, °F",
                       "Precipitation of Wettest Month, inches",
                       "Precipitation of Driest Month, inches",
                       "Precipitation Seasonality (Coefficient of Variation), %",
                       "Precipitation of Wettest Quarter, inches",
                       "Precipitation of Driest Quarter, inches",
                       "Precipitation of Coldest Quarter, inches",
                       "Precipitation of Warmest Quarter, inches")

# Name the file and save it

bioclim_results_folder <- paste0(results_folder,"Bioclimatics")
dir.create(path = bioclim_results_folder)

spreadsheetName <- paste(AFB_Name,"Bioclimatics.csv", sep = "_")

filePath <- paste(bioclim_results_folder,spreadsheetName, sep = "/")

write.csv(env.var, file = filePath) 

#the end