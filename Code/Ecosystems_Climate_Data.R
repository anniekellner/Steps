##THINGS TO CHECK FIRST##
##MAKE SURE ALL 5 DATA BASES ARE CORRECT
##MAKE SURE THE PATHWAY FOR OUTPUT IS CORRECT
##Trevor Even - 01252022 - changed precip for driest/wettest/coldest/hottest quarter value calculation to be consistent when there are overlaps on these categories
##Trevor Even - 03162023 - changed future scenario year divisors to reflect 30 year scenario window going forward


# load data
dat.hist <- AllDays[[1]]
dat.RCP4.5.2030 <- AllDays[[2]]
dat.RCP4.5.2050 <- AllDays[[3]]
dat.RCP8.5.2030 <- AllDays[[4]]
dat.RCP8.5.2050 <- AllDays[[5]]


###HISTORICAL###

# Create function for adding month (e.g., "Jan") to dataframe

add_month <- function(df){
  df = df %>%
    mutate(month = month(df$date, label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME")))
}

AllDays1 <- tibble(AllDays)

AllDays1 <- AllDays1 %>%
  mutate(month = map(AllDays, ~ mutate(., 
                                       month = month(date, label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME")))))

# loop to add to dataframes

for(i in 1:length(AllDays)){
  add_month(AllDays[[i]])
}

#setting aside appropriate data

month <-        c(month.abb)
month           #make sure the data lines up with table dude
hist.temp.av <- c(dat.hist$TMeanF)
hist.temp.av   #make sure the data lines up with table dude
hist.temp.hi <- c(dat.hist$TMaxF)
hist.temp.hi    #make sure the data lines up with table dude
hist.temp.lo <- c(dat.hist$TMinF)
hist.temp.lo    #make sure the data lines up with table dude
hist.precip <-  c(dat.hist$PPT_in)
hist.precip     #make sure the data lines up with table dude

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

jan.tav <- mean(jan.dat$taveF)
feb.tav <- mean(feb.dat$taveF)
mar.tav <- mean(mar.dat$taveF)
apr.tav <- mean(apr.dat$taveF)
may.tav <- mean(may.dat$taveF)
jun.tav <- mean(jun.dat$taveF)
jul.tav <- mean(jul.dat$taveF)
aug.tav <- mean(aug.dat$taveF)
sep.tav <- mean(sep.dat$taveF)
oct.tav <- mean(oct.dat$taveF)
nov.tav <- mean(nov.dat$taveF)
dec.tav <- mean(dec.dat$taveF)

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

jan.prav <- (sum(jan.dat$ppt_in))/30
feb.prav <- (sum(feb.dat$ppt_in))/30
mar.prav <- (sum(mar.dat$ppt_in))/30
apr.prav <- (sum(apr.dat$ppt_in))/30
may.prav <- (sum(may.dat$ppt_in))/30
jun.prav <- (sum(jun.dat$ppt_in))/30
jul.prav <- (sum(jul.dat$ppt_in))/30
aug.prav <- (sum(aug.dat$ppt_in))/30
sep.prav <- (sum(sep.dat$ppt_in))/30
oct.prav <- (sum(oct.dat$ppt_in))/30
nov.prav <- (sum(nov.dat$ppt_in))/30
dec.prav <- (sum(dec.dat$ppt_in))/30

## 1) Annual Mean Diurnal Range
#difference between month's max and min temp averaged over 12 months
#SUMof(Tmax-Tmin)/12

jan.max <- mean(jan.dat$tmaxF)
jan.min <- mean(jan.dat$tminF)

feb.max <- mean(feb.dat$tmaxF)
feb.min <- mean(feb.dat$tminF)

mar.max <- mean(mar.dat$tmaxF)
mar.min <- mean(mar.dat$tminF)

apr.max <- mean(apr.dat$tmaxF)
apr.min <- mean(apr.dat$tminF)

may.max <- mean(may.dat$tmaxF)
may.min <- mean(may.dat$tminF)

jun.max <- mean(jun.dat$tmaxF)
jun.min <- mean(jun.dat$tminF)

jul.max <- mean(jul.dat$tmaxF)
jul.min <- mean(jul.dat$tminF)

aug.max <- mean(aug.dat$tmaxF)
aug.min <- mean(aug.dat$tminF)

sep.max <- mean(sep.dat$tmaxF)
sep.min <- mean(sep.dat$tminF)

oct.max <- mean(oct.dat$tmaxF)
oct.min <- mean(oct.dat$tminF)

nov.max <- mean(nov.dat$tmaxF)
nov.min <- mean(nov.dat$tminF)

dec.max <- mean(dec.dat$tmaxF)
dec.min <- mean(dec.dat$tminF)

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
abs.tmax.f <- max(dat.hist$tmaxF)
abs.max.c <- f.to.c(abs.tmax.f)
abs.tmin.f <- min(dat.hist$tminF)
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
annual.av.f <- mean(dat.hist$taveF)

#convert it to Kelvin
annual.av.k. <- f.to.k(annual.av.f)

##Temperature Seasonality CV
cv.temperature.seasonality <- ((sd.mmk)/annual.av.k.)*100

#value
cv.temperature.seasonality

##Max Temperature of Warmest Month
#lets first figure out the warmest month
#saving it in case we need it
warmest.day.warmest.month <- c(max(jan.dat$tmaxF), max(feb.dat$tmaxF), max(mar.dat$tmaxF), max(apr.dat$tmaxF),
                               max(may.dat$tmaxF), max(jun.dat$tmaxF), max(jul.dat$tmaxF), max(aug.dat$tmaxF),
                               max(sep.dat$tmaxF), max(oct.dat$tmaxF), max(nov.dat$tmaxF), max(dec.dat$tmaxF)
)[which.max(c(mean(jan.dat$tmaxF), mean(feb.dat$tmaxF), mean(mar.dat$tmaxF), mean(apr.dat$tmaxF), 
              mean(may.dat$tmaxF), mean(jun.dat$tmaxF), mean(jul.dat$tmaxF), mean(aug.dat$tmaxF), 
              mean(sep.dat$tmaxF), mean(oct.dat$tmaxF), mean(nov.dat$tmaxF), mean(dec.dat$tmaxF)))]

#value
warmest.day.warmest.month

##Min Temperature of Coldest Month
#saving it in case they want it
coldest.day.coldest.month <- c(min(jan.dat$tminF), min(feb.dat$tminF), min(mar.dat$tminF), min(apr.dat$tminF),
                               min(may.dat$tminF), min(jun.dat$tminF), min(jul.dat$tminF), min(aug.dat$tminF),
                               min(sep.dat$tminF), min(oct.dat$tminF), min(nov.dat$tminF), min(dec.dat$tminF)
)[which.min(c(mean(jan.dat$tminF), mean(feb.dat$tminF), mean(mar.dat$tminF), mean(apr.dat$tminF), 
              mean(may.dat$tminF), mean(jun.dat$tminF), mean(jul.dat$tminF), mean(aug.dat$tminF), 
              mean(sep.dat$tminF), mean(oct.dat$tminF), mean(nov.dat$tminF), mean(dec.dat$tminF)))]

#value
coldest.day.coldest.month

##Annual Temperature Range
#hottest day of hottest month minus coldest day of coldest month

annual.temperature.range <- warmest.day.warmest.month - coldest.day.coldest.month

#value
annual.temperature.range

##Mean Temperature of Wettest Quarter

mean.temp.wettest.quarter <- c(mean(q1.dat$taveF), mean(q2.dat$taveF), mean(q3.dat$taveF), mean(q4.dat$taveF),
                               mean(q5.dat$taveF), mean(q6.dat$taveF), mean(q7.dat$taveF), mean(q8.dat$taveF),
                               mean(q9.dat$taveF), mean(q10.dat$taveF), mean(q11.dat$taveF), mean(q12.dat$taveF)
)[which.max(c(mean(q1.dat$ppt_in)*90.25, mean(q2.dat$ppt_in)*89.25, mean(q3.dat$ppt_in)*92, mean(q4.dat$ppt_in)*91, 
              mean(q5.dat$ppt_in)*92, mean(q6.dat$ppt_in)*92, mean(q7.dat$ppt_in)*92, mean(q8.dat$ppt_in)*92, 
              mean(q9.dat$ppt_in)*91, mean(q10.dat$ppt_in)*92, mean(q11.dat$ppt_in)*92, mean(q12.dat$ppt_in)*90.25))]

#value
mean.temp.wettest.quarter

##Mean Temperature of Driest Quarter

mean.temp.driest.quarter <- c(mean(q1.dat$taveF), mean(q2.dat$taveF), mean(q3.dat$taveF), mean(q4.dat$taveF),
                              mean(q5.dat$taveF), mean(q6.dat$taveF), mean(q7.dat$taveF), mean(q8.dat$taveF),
                              mean(q9.dat$taveF), mean(q10.dat$taveF), mean(q11.dat$taveF), mean(q12.dat$taveF)
)[which.min(c(mean(q1.dat$ppt_in)*90.25, mean(q2.dat$ppt_in)*89.25, mean(q3.dat$ppt_in)*92, mean(q4.dat$ppt_in)*91, 
              mean(q5.dat$ppt_in)*92, mean(q6.dat$ppt_in)*92, mean(q7.dat$ppt_in)*92, mean(q8.dat$ppt_in)*92, 
              mean(q9.dat$ppt_in)*91, mean(q10.dat$ppt_in)*92, mean(q11.dat$ppt_in)*92, mean(q12.dat$ppt_in)*90.25))]

#value
mean.temp.driest.quarter

##Mean Temperature of Warmest Quarter

mean.temp.warmest.quarter <- c(mean(q1.dat$taveF), mean(q2.dat$taveF), mean(q3.dat$taveF), mean(q4.dat$taveF),
                               mean(q5.dat$taveF), mean(q6.dat$taveF), mean(q7.dat$taveF), mean(q8.dat$taveF),
                               mean(q9.dat$taveF), mean(q10.dat$taveF), mean(q11.dat$taveF), mean(q12.dat$taveF)
)[which.max(c(mean(q1.dat$taveF), mean(q2.dat$taveF), mean(q3.dat$taveF), mean(q4.dat$taveF), 
              mean(q5.dat$taveF), mean(q6.dat$taveF), mean(q7.dat$taveF), mean(q8.dat$taveF), 
              mean(q9.dat$taveF), mean(q10.dat$taveF), mean(q11.dat$taveF), mean(q12.dat$taveF)))]

#value
mean.temp.warmest.quarter

##Mean Temperature of Coldest Quarter

mean.temp.coldest.quarter <- c(mean(q1.dat$taveF), mean(q2.dat$taveF), mean(q3.dat$taveF), mean(q4.dat$taveF),
                               mean(q5.dat$taveF), mean(q6.dat$taveF), mean(q7.dat$taveF), mean(q8.dat$taveF),
                               mean(q9.dat$taveF), mean(q10.dat$taveF), mean(q11.dat$taveF), mean(q12.dat$taveF)
)[which.min(c(mean(q1.dat$taveF), mean(q2.dat$taveF), mean(q3.dat$taveF), mean(q4.dat$taveF), 
              mean(q5.dat$taveF), mean(q6.dat$taveF), mean(q7.dat$taveF), mean(q8.dat$taveF), 
              mean(q9.dat$taveF), mean(q10.dat$taveF), mean(q11.dat$taveF), mean(q12.dat$taveF)))]

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

#since we are using sums of precip for a given month over a 30 year historical period we need to divide
#by 30 to get the sum of ones months precip

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

#annual precipitation / getting the sum of over 30 years then dividing by 30 to get annual average
an.precip <- (sum(dat.hist$ppt_mm)/30)
mo.precip <- an.precip/12

#final calculation
precip.seasonality.cv <- (sd.precip/(1+mo.precip))*100

#value
precip.seasonality.cv

##Precipitation of Wettest Quarter

precip.wettest.quarter.pre <- c(sum(q1.dat$ppt_in), sum(q2.dat$ppt_in), sum(q3.dat$ppt_in), sum(q4.dat$ppt_in),
                                sum(q5.dat$ppt_in), sum(q6.dat$ppt_in), sum(q7.dat$ppt_in), sum(q8.dat$ppt_in),
                                sum(q9.dat$ppt_in), sum(q10.dat$ppt_in), sum(q11.dat$ppt_in), sum(q12.dat$ppt_in)
)[which.max(c(mean(q1.dat$ppt_in)*90.25, mean(q2.dat$ppt_in)*89.25, mean(q3.dat$ppt_in)*92, mean(q4.dat$ppt_in)*91, 
              mean(q5.dat$ppt_in)*92, mean(q6.dat$ppt_in)*92, mean(q7.dat$ppt_in)*92, mean(q8.dat$ppt_in)*92, 
              mean(q9.dat$ppt_in)*91, mean(q10.dat$ppt_in)*92, mean(q11.dat$ppt_in)*92, mean(q12.dat$ppt_in)*90.25))]

#previous calculation is the sum over 30 year historical period of said quarter
#divide by 30 to get the annual quarter calculation 
precip.wettest.quarter <- precip.wettest.quarter.pre/30

#value
precip.wettest.quarter

##Precipitation of Driest Quarter

precip.driest.quarter.pre <- c(sum(q1.dat$ppt_in), sum(q2.dat$ppt_in), sum(q3.dat$ppt_in), sum(q4.dat$ppt_in),
                               sum(q5.dat$ppt_in), sum(q6.dat$ppt_in), sum(q7.dat$ppt_in), sum(q8.dat$ppt_in),
                               sum(q9.dat$ppt_in), sum(q10.dat$ppt_in), sum(q11.dat$ppt_in), sum(q12.dat$ppt_in)
)[which.min(c(mean(q1.dat$ppt_in)*90.25, mean(q2.dat$ppt_in)*89.25, mean(q3.dat$ppt_in)*92, mean(q4.dat$ppt_in)*91, 
              mean(q5.dat$ppt_in)*92, mean(q6.dat$ppt_in)*92, mean(q7.dat$ppt_in)*92, mean(q8.dat$ppt_in)*92, 
              mean(q9.dat$ppt_in)*91, mean(q10.dat$ppt_in)*92, mean(q11.dat$ppt_in)*92, mean(q12.dat$ppt_in)*90.25))]

#previous calculation is the sum over 30 year historical period of said quarter
#divide by 30 to get the annual quarter calculation 
precip.driest.quarter <- precip.driest.quarter.pre/30

#value
precip.driest.quarter

##Precipitation of Coldest Quarter

precip.coldest.quarter.pre <- c(sum(q1.dat$ppt_in), sum(q2.dat$ppt_in), sum(q3.dat$ppt_in), sum(q4.dat$ppt_in),
                                sum(q5.dat$ppt_in), sum(q6.dat$ppt_in), sum(q7.dat$ppt_in), sum(q8.dat$ppt_in),
                                sum(q9.dat$ppt_in), sum(q10.dat$ppt_in), sum(q11.dat$ppt_in), sum(q12.dat$ppt_in)
)[which.min(c(mean(q1.dat$taveF), mean(q2.dat$taveF), mean(q3.dat$taveF), mean(q4.dat$taveF), 
              mean(q5.dat$taveF), mean(q6.dat$taveF), mean(q7.dat$taveF), mean(q8.dat$taveF), 
              mean(q9.dat$taveF), mean(q10.dat$taveF), mean(q11.dat$taveF), mean(q12.dat$taveF)))]

#previous calculation is the sum over 30 year historical period of said quarter
#divide by 30 to get the annual quarter calculation 
precip.coldest.quarter <- precip.coldest.quarter.pre/30

#value
precip.coldest.quarter

##Precipitation of the Warmest Quarter

precip.warmest.quarter.pre <- c(sum(q1.dat$ppt_in), sum(q2.dat$ppt_in), sum(q3.dat$ppt_in), sum(q4.dat$ppt_in),
                                sum(q5.dat$ppt_in), sum(q6.dat$ppt_in), sum(q7.dat$ppt_in), sum(q8.dat$ppt_in),
                                sum(q9.dat$ppt_in), sum(q10.dat$ppt_in), sum(q11.dat$ppt_in), sum(q12.dat$ppt_in)
)[which.max(c(mean(q1.dat$taveF), mean(q2.dat$taveF), mean(q3.dat$taveF), mean(q4.dat$taveF), 
              mean(q5.dat$taveF), mean(q6.dat$taveF), mean(q7.dat$taveF), mean(q8.dat$taveF), 
              mean(q9.dat$taveF), mean(q10.dat$taveF), mean(q11.dat$taveF), mean(q12.dat$taveF)))]

#previous calculation is the sum over 30 year historical period of said quarter
#divide by 30 to get the annual quarter calculation 
precip.warmest.quarter <- precip.warmest.quarter.pre/30


#value
precip.warmest.quarter

#####RCP 4.5 2030

#setting aside appropriate data

month <-        c(month.abb)
month           #make sure the data lines up with table dude
sc1.temp.av <- c(dat.RCP4.5.2030$taveF)
sc1.temp.av   #make sure the data lines up with table dude
sc1.temp.hi <- c(dat.RCP4.5.2030$tmaxF)
sc1.temp.hi    #make sure the data lines up with table dude
sc1.temp.lo <- c(dat.RCP4.5.2030$tminF)
sc1.temp.lo    #make sure the data lines up with table dude
sc1.precip <-  c(dat.RCP4.5.2030$ppt_in)
sc1.precip     #make sure the data lines up with table dude

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

jan.tav.sc1 <- mean(jan.dat.sc1$taveF)
feb.tav.sc1 <- mean(feb.dat.sc1$taveF)
mar.tav.sc1 <- mean(mar.dat.sc1$taveF)
apr.tav.sc1 <- mean(apr.dat.sc1$taveF)
may.tav.sc1 <- mean(may.dat.sc1$taveF)
jun.tav.sc1 <- mean(jun.dat.sc1$taveF)
jul.tav.sc1 <- mean(jul.dat.sc1$taveF)
aug.tav.sc1 <- mean(aug.dat.sc1$taveF)
sep.tav.sc1 <- mean(sep.dat.sc1$taveF)
oct.tav.sc1 <- mean(oct.dat.sc1$taveF)
nov.tav.sc1 <- mean(nov.dat.sc1$taveF)
dec.tav.sc1 <- mean(dec.dat.sc1$taveF)

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

jan.prav.dat.sc1 <- (sum(jan.dat.sc1$ppt_in))/30
feb.prav.dat.sc1 <- (sum(feb.dat.sc1$ppt_in))/30
mar.prav.dat.sc1 <- (sum(mar.dat.sc1$ppt_in))/30
apr.prav.dat.sc1 <- (sum(apr.dat.sc1$ppt_in))/30
may.prav.dat.sc1 <- (sum(may.dat.sc1$ppt_in))/30
jun.prav.dat.sc1 <- (sum(jun.dat.sc1$ppt_in))/30
jul.prav.dat.sc1 <- (sum(jul.dat.sc1$ppt_in))/30
aug.prav.dat.sc1 <- (sum(aug.dat.sc1$ppt_in))/30
sep.prav.dat.sc1 <- (sum(sep.dat.sc1$ppt_in))/30
oct.prav.dat.sc1 <- (sum(oct.dat.sc1$ppt_in))/30
nov.prav.dat.sc1 <- (sum(nov.dat.sc1$ppt_in))/30
dec.prav.dat.sc1 <- (sum(dec.dat.sc1$ppt_in))/30

## 1) Annual Mean Diurnal Range
#difference between month's max and min temp averaged over 12 months
#SUMof(Tmax-Tmin)/12

jan.max.sc1 <- mean(jan.dat.sc1$tmaxF)
jan.min.sc1 <- mean(jan.dat.sc1$tminF)

feb.max.sc1 <- mean(feb.dat.sc1$tmaxF)
feb.min.sc1 <- mean(feb.dat.sc1$tminF)

mar.max.sc1 <- mean(mar.dat.sc1$tmaxF)
mar.min.sc1 <- mean(mar.dat.sc1$tminF)

apr.max.sc1 <- mean(apr.dat.sc1$tmaxF)
apr.min.sc1 <- mean(apr.dat.sc1$tminF)

may.max.sc1 <- mean(may.dat.sc1$tmaxF)
may.min.sc1 <- mean(may.dat.sc1$tminF)

jun.max.sc1 <- mean(jun.dat.sc1$tmaxF)
jun.min.sc1 <- mean(jun.dat.sc1$tminF)

jul.max.sc1 <- mean(jul.dat.sc1$tmaxF)
jul.min.sc1 <- mean(jul.dat.sc1$tminF)

aug.max.sc1 <- mean(aug.dat.sc1$tmaxF)
aug.min.sc1 <- mean(aug.dat.sc1$tminF)

sep.max.sc1 <- mean(sep.dat.sc1$tmaxF)
sep.min.sc1 <- mean(sep.dat.sc1$tminF)

oct.max.sc1 <- mean(oct.dat.sc1$tmaxF)
oct.min.sc1 <- mean(oct.dat.sc1$tminF)

nov.max.sc1 <- mean(nov.dat.sc1$tmaxF)
nov.min.sc1 <- mean(nov.dat.sc1$tminF)

dec.max.sc1 <- mean(dec.dat.sc1$tmaxF)
dec.min.sc1 <- mean(dec.dat.sc1$tminF)

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
abs.tmax.pre.sc1 <- max(dat.RCP4.5.2030$tmaxF)
abs.tmax.c.sc1 <- f.to.c(abs.tmax.pre.sc1)
abs.tmin.pre.sc1 <- min(dat.RCP4.5.2030$tminF)
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
annual.av.f.sc1 <- mean(dat.RCP4.5.2030$taveF)

#convert it to Kelvin
annual.av.k.sc1. <- f.to.k(annual.av.f.sc1)

##Temperature Seasonality CV
cv.temperature.seasonality.sc1 <- ((sd.mmk.sc1)/annual.av.k.sc1.)*100

#value
cv.temperature.seasonality.sc1

##Max Temperature of Warmest Month
#lets first figure out the warmest month
#saving it in case we need it
warmest.day.warmest.month.sc1 <- c(max(jan.dat.sc1$tmaxF), max(feb.dat.sc1$tmaxF), max(mar.dat.sc1$tmaxF), max(apr.dat.sc1$tmaxF),
                                   max(may.dat.sc1$tmaxF), max(jun.dat.sc1$tmaxF), max(jul.dat.sc1$tmaxF), max(aug.dat.sc1$tmaxF),
                                   max(sep.dat.sc1$tmaxF), max(oct.dat.sc1$tmaxF), max(nov.dat.sc1$tmaxF), max(dec.dat.sc1$tmaxF)
)[which.max(c(mean(jan.dat.sc1$tmaxF), mean(feb.dat.sc1$tmaxF), mean(mar.dat.sc1$tmaxF), mean(apr.dat.sc1$tmaxF), 
              mean(may.dat.sc1$tmaxF), mean(jun.dat.sc1$tmaxF), mean(jul.dat.sc1$tmaxF), mean(aug.dat.sc1$tmaxF), 
              mean(sep.dat.sc1$tmaxF), mean(oct.dat.sc1$tmaxF), mean(nov.dat.sc1$tmaxF), mean(dec.dat.sc1$tmaxF)))]

#value
warmest.day.warmest.month.sc1

##Min Temperature of Coldest Month
#saving it in case they want it
coldest.day.coldest.month.sc1 <- c(min(jan.dat.sc1$tminF), min(feb.dat.sc1$tminF), min(mar.dat.sc1$tminF), min(apr.dat.sc1$tminF),
                                   min(may.dat.sc1$tminF), min(jun.dat.sc1$tminF), min(jul.dat.sc1$tminF), min(aug.dat.sc1$tminF),
                                   min(sep.dat.sc1$tminF), min(oct.dat.sc1$tminF), min(nov.dat.sc1$tminF), min(dec.dat.sc1$tminF)
)[which.min(c(mean(jan.dat.sc1$tminF), mean(feb.dat.sc1$tminF), mean(mar.dat.sc1$tminF), mean(apr.dat.sc1$tminF), 
              mean(may.dat.sc1$tminF), mean(jun.dat.sc1$tminF), mean(jul.dat.sc1$tminF), mean(aug.dat.sc1$tminF), 
              mean(sep.dat.sc1$tminF), mean(oct.dat.sc1$tminF), mean(nov.dat.sc1$tminF), mean(dec.dat.sc1$tminF)))]

#value
coldest.day.coldest.month.sc1

##Annual Temperature Range
#hottest day of hottest month minus coldest day of coldest month

annual.temperature.range.sc1 <- warmest.day.warmest.month.sc1 - coldest.day.coldest.month.sc1

#value
annual.temperature.range.sc1

##Mean Temperature of Wettest Quarter

mean.temp.wettest.quarter.sc1 <- c(mean(q1.dat.sc1$taveF), mean(q2.dat.sc1$taveF), mean(q3.dat.sc1$taveF), mean(q4.dat.sc1$taveF),
                                   mean(q5.dat.sc1$taveF), mean(q6.dat.sc1$taveF), mean(q7.dat.sc1$taveF), mean(q8.dat.sc1$taveF),
                                   mean(q9.dat.sc1$taveF), mean(q10.dat.sc1$taveF), mean(q11.dat.sc1$taveF), mean(q12.dat.sc1$taveF)
)[which.max(c(mean(q1.dat.sc1$ppt_in)*90.25, mean(q2.dat.sc1$ppt_in)*89.25, mean(q3.dat.sc1$ppt_in)*92, mean(q4.dat.sc1$ppt_in)*91, 
              mean(q5.dat.sc1$ppt_in)*92, mean(q6.dat.sc1$ppt_in)*92, mean(q7.dat.sc1$ppt_in)*92, mean(q8.dat.sc1$ppt_in)*92, 
              mean(q9.dat.sc1$ppt_in)*91, mean(q10.dat.sc1$ppt_in)*92, mean(q11.dat.sc1$ppt_in)*92, mean(q12.dat.sc1$ppt_in)*90.25))]

#value
mean.temp.wettest.quarter.sc1

##Mean Temperature of Driest Quarter

mean.temp.driest.quarter.sc1 <- c(mean(q1.dat.sc1$taveF), mean(q2.dat.sc1$taveF), mean(q3.dat.sc1$taveF), mean(q4.dat.sc1$taveF),
                                  mean(q5.dat.sc1$taveF), mean(q6.dat.sc1$taveF), mean(q7.dat.sc1$taveF), mean(q8.dat.sc1$taveF),
                                  mean(q9.dat.sc1$taveF), mean(q10.dat.sc1$taveF), mean(q11.dat.sc1$taveF), mean(q12.dat.sc1$taveF)
)[which.min(c(mean(q1.dat.sc1$ppt_in)*90.25, mean(q2.dat.sc1$ppt_in)*89.25, mean(q3.dat.sc1$ppt_in)*92, mean(q4.dat.sc1$ppt_in)*91, 
              mean(q5.dat.sc1$ppt_in)*92, mean(q6.dat.sc1$ppt_in)*92, mean(q7.dat.sc1$ppt_in)*92, mean(q8.dat.sc1$ppt_in)*92, 
              mean(q9.dat.sc1$ppt_in)*91, mean(q10.dat.sc1$ppt_in)*92, mean(q11.dat.sc1$ppt_in)*92, mean(q12.dat.sc1$ppt_in)*90.25))]

#value
mean.temp.driest.quarter.sc1

##Mean Temperature of Warmest Quarter

mean.temp.warmest.quarter.sc1 <- c(mean(q1.dat.sc1$taveF), mean(q2.dat.sc1$taveF), mean(q3.dat.sc1$taveF), mean(q4.dat.sc1$taveF),
                                   mean(q5.dat.sc1$taveF), mean(q6.dat.sc1$taveF), mean(q7.dat.sc1$taveF), mean(q8.dat.sc1$taveF),
                                   mean(q9.dat.sc1$taveF), mean(q10.dat.sc1$taveF), mean(q11.dat.sc1$taveF), mean(q12.dat.sc1$taveF)
)[which.max(c(mean(q1.dat.sc1$taveF), mean(q2.dat.sc1$taveF), mean(q3.dat.sc1$taveF), mean(q4.dat.sc1$taveF), 
              mean(q5.dat.sc1$taveF), mean(q6.dat.sc1$taveF), mean(q7.dat.sc1$taveF), mean(q8.dat.sc1$taveF), 
              mean(q9.dat.sc1$taveF), mean(q10.dat.sc1$taveF), mean(q11.dat.sc1$taveF), mean(q12.dat.sc1$taveF)))]

#value
mean.temp.warmest.quarter.sc1

##Mean Temperature of Coldest Quarter

mean.temp.coldest.quarter.sc1 <- c(mean(q1.dat.sc1$taveF), mean(q2.dat.sc1$taveF), mean(q3.dat.sc1$taveF), mean(q4.dat.sc1$taveF),
                                   mean(q5.dat.sc1$taveF), mean(q6.dat.sc1$taveF), mean(q7.dat.sc1$taveF), mean(q8.dat.sc1$taveF),
                                   mean(q9.dat.sc1$taveF), mean(q10.dat.sc1$taveF), mean(q11.dat.sc1$taveF), mean(q12.dat.sc1$taveF)
)[which.min(c(mean(q1.dat.sc1$taveF), mean(q2.dat.sc1$taveF), mean(q3.dat.sc1$taveF), mean(q4.dat.sc1$taveF), 
              mean(q5.dat.sc1$taveF), mean(q6.dat.sc1$taveF), mean(q7.dat.sc1$taveF), mean(q8.dat.sc1$taveF), 
              mean(q9.dat.sc1$taveF), mean(q10.dat.sc1$taveF), mean(q11.dat.sc1$taveF), mean(q12.dat.sc1$taveF)))]

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

#annual precipitation / getting the sum of over 30 years then dividing by 30 to get annual average
an.precip.sc1 <- (sum(dat.RCP4.5.2030$ppt_mm)/30)
mo.precip.sc1 <- an.precip.sc1/12

#final calculation
precip.seasonality.cv.sc1 <- (sd.precip.sc1/(1+mo.precip.sc1))*100

#value
precip.seasonality.cv.sc1

##Precipitation of Wettest Quarter

precip.wettest.quarter.pre.sc1 <- c(sum(q1.dat.sc1$ppt_in), sum(q2.dat.sc1$ppt_in), sum(q3.dat.sc1$ppt_in), sum(q4.dat.sc1$ppt_in),
                                    sum(q5.dat.sc1$ppt_in), sum(q6.dat.sc1$ppt_in), sum(q7.dat.sc1$ppt_in), sum(q8.dat.sc1$ppt_in),
                                    sum(q9.dat.sc1$ppt_in), sum(q10.dat.sc1$ppt_in), sum(q11.dat.sc1$ppt_in), sum(q12.dat.sc1$ppt_in)
)[which.max(c(mean(q1.dat.sc1$ppt_in)*90.25, mean(q2.dat.sc1$ppt_in)*89.25, mean(q3.dat.sc1$ppt_in)*92, mean(q4.dat.sc1$ppt_in)*91, 
              mean(q5.dat.sc1$ppt_in)*92, mean(q6.dat.sc1$ppt_in)*92, mean(q7.dat.sc1$ppt_in)*92, mean(q8.dat.sc1$ppt_in)*92, 
              mean(q9.dat.sc1$ppt_in)*91, mean(q10.dat.sc1$ppt_in)*92, mean(q11.dat.sc1$ppt_in)*92, mean(q12.dat.sc1$ppt_in)*90.25))]

#previous calculation is the sum over   30 year historical period of said quarter
#divide by 10 to get the annual quarter calculation
precip.wettest.quarter.sc1 <- precip.wettest.quarter.pre.sc1/30

#value
precip.wettest.quarter.sc1

##Precipitation of Driest Quarter

precip.driest.quarter.pre.sc1 <- c(sum(q1.dat.sc1$ppt_in), sum(q2.dat.sc1$ppt_in), sum(q3.dat.sc1$ppt_in), sum(q4.dat.sc1$ppt_in),
                                   sum(q5.dat.sc1$ppt_in), sum(q6.dat.sc1$ppt_in), sum(q7.dat.sc1$ppt_in), sum(q8.dat.sc1$ppt_in),
                                   sum(q9.dat.sc1$ppt_in), sum(q10.dat.sc1$ppt_in), sum(q11.dat.sc1$ppt_in), sum(q12.dat.sc1$ppt_in)
)[which.min(c(mean(q1.dat.sc1$ppt_in)*90.25, mean(q2.dat.sc1$ppt_in)*89.25, mean(q3.dat.sc1$ppt_in)*92, mean(q4.dat.sc1$ppt_in)*91, 
              mean(q5.dat.sc1$ppt_in)*92, mean(q6.dat.sc1$ppt_in)*92, mean(q7.dat.sc1$ppt_in)*92, mean(q8.dat.sc1$ppt_in)*92, 
              mean(q9.dat.sc1$ppt_in)*91, mean(q10.dat.sc1$ppt_in)*92, mean(q11.dat.sc1$ppt_in)*92, mean(q12.dat.sc1$ppt_in)*90.25))]

#previous calculation is the sum over   30 year historical period of said quarter
#divide by 10 to get the annual quarter calculation 
precip.driest.quarter.sc1 <- precip.driest.quarter.pre.sc1/30

#value
precip.driest.quarter.sc1

##Precipitation of Coldest Quarter

precip.coldest.quarter.pre.sc1 <- c(sum(q1.dat.sc1$ppt_in), sum(q2.dat.sc1$ppt_in), sum(q3.dat.sc1$ppt_in), sum(q4.dat.sc1$ppt_in),
                                    sum(q5.dat.sc1$ppt_in), sum(q6.dat.sc1$ppt_in), sum(q7.dat.sc1$ppt_in), sum(q8.dat.sc1$ppt_in),
                                    sum(q9.dat.sc1$ppt_in), sum(q10.dat.sc1$ppt_in), sum(q11.dat.sc1$ppt_in), sum(q12.dat.sc1$ppt_in)
)[which.min(c(mean(q1.dat.sc1$taveF), mean(q2.dat.sc1$taveF), mean(q3.dat.sc1$taveF), mean(q4.dat.sc1$taveF), 
              mean(q5.dat.sc1$taveF), mean(q6.dat.sc1$taveF), mean(q7.dat.sc1$taveF), mean(q8.dat.sc1$taveF), 
              mean(q9.dat.sc1$taveF), mean(q10.dat.sc1$taveF), mean(q11.dat.sc1$taveF), mean(q12.dat.sc1$taveF)))]

#previous calculation is the sum over   30 year historical period of said quarter
#divide by 10 to get the annual quarter calculation 
precip.coldest.quarter.sc1 <- precip.coldest.quarter.pre.sc1/30

#value
precip.coldest.quarter.sc1

##Precipitation of the Warmest Quarter

precip.warmest.quarter.pre.sc1 <- c(sum(q1.dat.sc1$ppt_in), sum(q2.dat.sc1$ppt_in), sum(q3.dat.sc1$ppt_in), sum(q4.dat.sc1$ppt_in),
                                    sum(q5.dat.sc1$ppt_in), sum(q6.dat.sc1$ppt_in), sum(q7.dat.sc1$ppt_in), sum(q8.dat.sc1$ppt_in),
                                    sum(q9.dat.sc1$ppt_in), sum(q10.dat.sc1$ppt_in), sum(q11.dat.sc1$ppt_in), sum(q12.dat.sc1$ppt_in)
)[which.max(c(mean(q1.dat.sc1$taveF), mean(q2.dat.sc1$taveF), mean(q3.dat.sc1$taveF), mean(q4.dat.sc1$taveF), 
              mean(q5.dat.sc1$taveF), mean(q6.dat.sc1$taveF), mean(q7.dat.sc1$taveF), mean(q8.dat.sc1$taveF), 
              mean(q9.dat.sc1$taveF), mean(q10.dat.sc1$taveF), mean(q11.dat.sc1$taveF), mean(q12.dat.sc1$taveF)))]

#previous calculation is the sum over 30 year projected period of said quarter
#divide by 10 to get the annual quarter calculation 
precip.warmest.quarter.sc1 <- precip.warmest.quarter.pre.sc1/30

#value
precip.warmest.quarter.sc1

#####RCP 4.5 2050

#setting aside appropriate data

month <-        c(month.abb)
month           #make sure the data lines up with table dude
sc2.temp.av <- c(dat.RCP4.5.2050$taveF)
sc2.temp.av   #make sure the data lines up with table dude
sc2.temp.hi <- c(dat.RCP4.5.2050$tmaxF)
sc2.temp.hi    #make sure the data lines up with table dude
sc2.temp.lo <- c(dat.RCP4.5.2050$tminF)
sc2.temp.lo    #make sure the data lines up with table dude
sc2.precip <-  c(dat.RCP4.5.2050$ppt_in)
sc2.precip     #make sure the data lines up with table dude

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

jan.tav.sc2 <- mean(jan.dat.sc2$taveF)
feb.tav.sc2 <- mean(feb.dat.sc2$taveF)
mar.tav.sc2 <- mean(mar.dat.sc2$taveF)
apr.tav.sc2 <- mean(apr.dat.sc2$taveF)
may.tav.sc2 <- mean(may.dat.sc2$taveF)
jun.tav.sc2 <- mean(jun.dat.sc2$taveF)
jul.tav.sc2 <- mean(jul.dat.sc2$taveF)
aug.tav.sc2 <- mean(aug.dat.sc2$taveF)
sep.tav.sc2 <- mean(sep.dat.sc2$taveF)
oct.tav.sc2 <- mean(oct.dat.sc2$taveF)
nov.tav.sc2 <- mean(nov.dat.sc2$taveF)
dec.tav.sc2 <- mean(dec.dat.sc2$taveF)

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

jan.prav.dat.sc2 <- (sum(jan.dat.sc2$ppt_in))/30
feb.prav.dat.sc2 <- (sum(feb.dat.sc2$ppt_in))/30
mar.prav.dat.sc2 <- (sum(mar.dat.sc2$ppt_in))/30
apr.prav.dat.sc2 <- (sum(apr.dat.sc2$ppt_in))/30
may.prav.dat.sc2 <- (sum(may.dat.sc2$ppt_in))/30
jun.prav.dat.sc2 <- (sum(jun.dat.sc2$ppt_in))/30
jul.prav.dat.sc2 <- (sum(jul.dat.sc2$ppt_in))/30
aug.prav.dat.sc2 <- (sum(aug.dat.sc2$ppt_in))/30
sep.prav.dat.sc2 <- (sum(sep.dat.sc2$ppt_in))/30
oct.prav.dat.sc2 <- (sum(oct.dat.sc2$ppt_in))/30
nov.prav.dat.sc2 <- (sum(nov.dat.sc2$ppt_in))/30
dec.prav.dat.sc2 <- (sum(dec.dat.sc2$ppt_in))/30

## 1) Annual Mean Diurnal Range
#difference between month's max and min temp averaged over 12 months
#SUMof(Tmax-Tmin)/12

jan.max.sc2 <- mean(jan.dat.sc2$tmaxF)
jan.min.sc2 <- mean(jan.dat.sc2$tminF)

feb.max.sc2 <- mean(feb.dat.sc2$tmaxF)
feb.min.sc2 <- mean(feb.dat.sc2$tminF)

mar.max.sc2 <- mean(mar.dat.sc2$tmaxF)
mar.min.sc2 <- mean(mar.dat.sc2$tminF)

apr.max.sc2 <- mean(apr.dat.sc2$tmaxF)
apr.min.sc2 <- mean(apr.dat.sc2$tminF)

may.max.sc2 <- mean(may.dat.sc2$tmaxF)
may.min.sc2 <- mean(may.dat.sc2$tminF)

jun.max.sc2 <- mean(jun.dat.sc2$tmaxF)
jun.min.sc2 <- mean(jun.dat.sc2$tminF)

jul.max.sc2 <- mean(jul.dat.sc2$tmaxF)
jul.min.sc2 <- mean(jul.dat.sc2$tminF)

aug.max.sc2 <- mean(aug.dat.sc2$tmaxF)
aug.min.sc2 <- mean(aug.dat.sc2$tminF)

sep.max.sc2 <- mean(sep.dat.sc2$tmaxF)
sep.min.sc2 <- mean(sep.dat.sc2$tminF)

oct.max.sc2 <- mean(oct.dat.sc2$tmaxF)
oct.min.sc2 <- mean(oct.dat.sc2$tminF)

nov.max.sc2 <- mean(nov.dat.sc2$tmaxF)
nov.min.sc2 <- mean(nov.dat.sc2$tminF)

dec.max.sc2 <- mean(dec.dat.sc2$tmaxF)
dec.min.sc2 <- mean(dec.dat.sc2$tminF)

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
abs.tmax.pre.sc2 <- max(dat.RCP4.5.2050$tmaxF)
abs.tmax.c.sc2 <- f.to.c(abs.tmax.pre.sc2)
abs.tmin.pre.sc2 <- min(dat.RCP4.5.2050$tminF)
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
annual.av.f.sc2 <- mean(dat.RCP4.5.2050$taveF)

#convert it to Kelvin
annual.av.k.sc2. <- f.to.k(annual.av.f.sc2)

##Temperature Seasonality CV
cv.temperature.seasonality.sc2 <- ((sd.mmk.sc2)/annual.av.k.sc2.)*100

#value
cv.temperature.seasonality.sc2

##Max Temperature of Warmest Month
#lets first figure out the warmest month
#saving it in case we need it
warmest.day.warmest.month.sc2 <- c(max(jan.dat.sc2$tmaxF), max(feb.dat.sc2$tmaxF), max(mar.dat.sc2$tmaxF), max(apr.dat.sc2$tmaxF),
                                   max(may.dat.sc2$tmaxF), max(jun.dat.sc2$tmaxF), max(jul.dat.sc2$tmaxF), max(aug.dat.sc2$tmaxF),
                                   max(sep.dat.sc2$tmaxF), max(oct.dat.sc2$tmaxF), max(nov.dat.sc2$tmaxF), max(dec.dat.sc2$tmaxF)
)[which.max(c(mean(jan.dat.sc2$tmaxF), mean(feb.dat.sc2$tmaxF), mean(mar.dat.sc2$tmaxF), mean(apr.dat.sc2$tmaxF), 
              mean(may.dat.sc2$tmaxF), mean(jun.dat.sc2$tmaxF), mean(jul.dat.sc2$tmaxF), mean(aug.dat.sc2$tmaxF), 
              mean(sep.dat.sc2$tmaxF), mean(oct.dat.sc2$tmaxF), mean(nov.dat.sc2$tmaxF), mean(dec.dat.sc2$tmaxF)))]

#value
warmest.day.warmest.month.sc2

##Min Temperature of Coldest Month
#saving it in case they want it
coldest.day.coldest.month.sc2 <- c(min(jan.dat.sc2$tminF), min(feb.dat.sc2$tminF), min(mar.dat.sc2$tminF), min(apr.dat.sc2$tminF),
                                   min(may.dat.sc2$tminF), min(jun.dat.sc2$tminF), min(jul.dat.sc2$tminF), min(aug.dat.sc2$tminF),
                                   min(sep.dat.sc2$tminF), min(oct.dat.sc2$tminF), min(nov.dat.sc2$tminF), min(dec.dat.sc2$tminF)
)[which.min(c(mean(jan.dat.sc2$tminF), mean(feb.dat.sc2$tminF), mean(mar.dat.sc2$tminF), mean(apr.dat.sc2$tminF), 
              mean(may.dat.sc2$tminF), mean(jun.dat.sc2$tminF), mean(jul.dat.sc2$tminF), mean(aug.dat.sc2$tminF), 
              mean(sep.dat.sc2$tminF), mean(oct.dat.sc2$tminF), mean(nov.dat.sc2$tminF), mean(dec.dat.sc2$tminF)))]

#value
coldest.day.coldest.month.sc2

##Annual Temperature Range
#hottest day of hottest month minus coldest day of coldest month

annual.temperature.range.sc2 <- warmest.day.warmest.month.sc2 - coldest.day.coldest.month.sc2

#value
annual.temperature.range.sc2

##Mean Temperature of Wettest Quarter

mean.temp.wettest.quarter.sc2 <- c(mean(q1.dat.sc2$taveF), mean(q2.dat.sc2$taveF), mean(q3.dat.sc2$taveF), mean(q4.dat.sc2$taveF),
                                   mean(q5.dat.sc2$taveF), mean(q6.dat.sc2$taveF), mean(q7.dat.sc2$taveF), mean(q8.dat.sc2$taveF),
                                   mean(q9.dat.sc2$taveF), mean(q10.dat.sc2$taveF), mean(q11.dat.sc2$taveF), mean(q12.dat.sc2$taveF)
)[which.max(c(mean(q1.dat.sc2$ppt_in)*90.25, mean(q2.dat.sc2$ppt_in)*89.25, mean(q3.dat.sc2$ppt_in)*92, mean(q4.dat.sc2$ppt_in)*91, 
              mean(q5.dat.sc2$ppt_in)*92, mean(q6.dat.sc2$ppt_in)*92, mean(q7.dat.sc2$ppt_in)*92, mean(q8.dat.sc2$ppt_in)*92, 
              mean(q9.dat.sc2$ppt_in)*91, mean(q10.dat.sc2$ppt_in)*92, mean(q11.dat.sc2$ppt_in)*92, mean(q12.dat.sc2$ppt_in)*90.25))]

#value
mean.temp.wettest.quarter.sc2

##Mean Temperature of Driest Quarter

mean.temp.driest.quarter.sc2 <- c(mean(q1.dat.sc2$taveF), mean(q2.dat.sc2$taveF), mean(q3.dat.sc2$taveF), mean(q4.dat.sc2$taveF),
                                  mean(q5.dat.sc2$taveF), mean(q6.dat.sc2$taveF), mean(q7.dat.sc2$taveF), mean(q8.dat.sc2$taveF),
                                  mean(q9.dat.sc2$taveF), mean(q10.dat.sc2$taveF), mean(q11.dat.sc2$taveF), mean(q12.dat.sc2$taveF)
)[which.min(c(mean(q1.dat.sc2$ppt_in)*90.25, mean(q2.dat.sc2$ppt_in)*89.25, mean(q3.dat.sc2$ppt_in)*92, mean(q4.dat.sc2$ppt_in)*91, 
              mean(q5.dat.sc2$ppt_in)*92, mean(q6.dat.sc2$ppt_in)*92, mean(q7.dat.sc2$ppt_in)*92, mean(q8.dat.sc2$ppt_in)*92, 
              mean(q9.dat.sc2$ppt_in)*91, mean(q10.dat.sc2$ppt_in)*92, mean(q11.dat.sc2$ppt_in)*92, mean(q12.dat.sc2$ppt_in)*90.25))]

#value
mean.temp.driest.quarter.sc2

##Mean Temperature of Warmest Quarter

mean.temp.warmest.quarter.sc2 <- c(mean(q1.dat.sc2$taveF), mean(q2.dat.sc2$taveF), mean(q3.dat.sc2$taveF), mean(q4.dat.sc2$taveF),
                                   mean(q5.dat.sc2$taveF), mean(q6.dat.sc2$taveF), mean(q7.dat.sc2$taveF), mean(q8.dat.sc2$taveF),
                                   mean(q9.dat.sc2$taveF), mean(q10.dat.sc2$taveF), mean(q11.dat.sc2$taveF), mean(q12.dat.sc2$taveF)
)[which.max(c(mean(q1.dat.sc2$taveF), mean(q2.dat.sc2$taveF), mean(q3.dat.sc2$taveF), mean(q4.dat.sc2$taveF), 
              mean(q5.dat.sc2$taveF), mean(q6.dat.sc2$taveF), mean(q7.dat.sc2$taveF), mean(q8.dat.sc2$taveF), 
              mean(q9.dat.sc2$taveF), mean(q10.dat.sc2$taveF), mean(q11.dat.sc2$taveF), mean(q12.dat.sc2$taveF)))]

#value
mean.temp.warmest.quarter.sc2

##Mean Temperature of Coldest Quarter

mean.temp.coldest.quarter.sc2 <- c(mean(q1.dat.sc2$taveF), mean(q2.dat.sc2$taveF), mean(q3.dat.sc2$taveF), mean(q4.dat.sc2$taveF),
                                   mean(q5.dat.sc2$taveF), mean(q6.dat.sc2$taveF), mean(q7.dat.sc2$taveF), mean(q8.dat.sc2$taveF),
                                   mean(q9.dat.sc2$taveF), mean(q10.dat.sc2$taveF), mean(q11.dat.sc2$taveF), mean(q12.dat.sc2$taveF)
)[which.min(c(mean(q1.dat.sc2$taveF), mean(q2.dat.sc2$taveF), mean(q3.dat.sc2$taveF), mean(q4.dat.sc2$taveF), 
              mean(q5.dat.sc2$taveF), mean(q6.dat.sc2$taveF), mean(q7.dat.sc2$taveF), mean(q8.dat.sc2$taveF), 
              mean(q9.dat.sc2$taveF), mean(q10.dat.sc2$taveF), mean(q11.dat.sc2$taveF), mean(q12.dat.sc2$taveF)))]

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

#annual precipitation / getting the sum of over 30 years then dividing by 10 to get annual average
an.precip.sc2 <- (sum(dat.RCP4.5.2050$ppt_mm)/30)
mo.precip.sc2 <- an.precip.sc2/12

#final calculation
precip.seasonality.cv.sc2 <- (sd.precip.sc2/(1+mo.precip.sc2))*100

#value
precip.seasonality.cv.sc2

##Precipitation of Wettest Quarter

precip.wettest.quarter.pre.sc2 <- c(sum(q1.dat.sc2$ppt_in), sum(q2.dat.sc2$ppt_in), sum(q3.dat.sc2$ppt_in), sum(q4.dat.sc2$ppt_in),
                                    sum(q5.dat.sc2$ppt_in), sum(q6.dat.sc2$ppt_in), sum(q7.dat.sc2$ppt_in), sum(q8.dat.sc2$ppt_in),
                                    sum(q9.dat.sc2$ppt_in), sum(q10.dat.sc2$ppt_in), sum(q11.dat.sc2$ppt_in), sum(q12.dat.sc2$ppt_in)
)[which.max(c(mean(q1.dat.sc2$ppt_in)*90.25, mean(q2.dat.sc2$ppt_in)*89.25, mean(q3.dat.sc2$ppt_in)*92, mean(q4.dat.sc2$ppt_in)*91, 
              mean(q5.dat.sc2$ppt_in)*92, mean(q6.dat.sc2$ppt_in)*92, mean(q7.dat.sc2$ppt_in)*92, mean(q8.dat.sc2$ppt_in)*92, 
              mean(q9.dat.sc2$ppt_in)*91, mean(q10.dat.sc2$ppt_in)*92, mean(q11.dat.sc2$ppt_in)*92, mean(q12.dat.sc2$ppt_in)*90.25))]

#previous calculation is the sum over 30 year projected period of said quarter
#divide by 10 to get the annual quarter calculation 
precip.wettest.quarter.sc2 <- precip.wettest.quarter.pre.sc2/30

#value
precip.wettest.quarter.sc2

##Precipitation of Driest Quarter

precip.driest.quarter.pre.sc2 <- c(sum(q1.dat.sc2$ppt_in), sum(q2.dat.sc2$ppt_in), sum(q3.dat.sc2$ppt_in), sum(q4.dat.sc2$ppt_in),
                                   sum(q5.dat.sc2$ppt_in), sum(q6.dat.sc2$ppt_in), sum(q7.dat.sc2$ppt_in), sum(q8.dat.sc2$ppt_in),
                                   sum(q9.dat.sc2$ppt_in), sum(q10.dat.sc2$ppt_in), sum(q11.dat.sc2$ppt_in), sum(q12.dat.sc2$ppt_in)
)[which.min(c(mean(q1.dat.sc2$ppt_in)*90.25, mean(q2.dat.sc2$ppt_in)*89.25, mean(q3.dat.sc2$ppt_in)*92, mean(q4.dat.sc2$ppt_in)*91, 
              mean(q5.dat.sc2$ppt_in)*92, mean(q6.dat.sc2$ppt_in)*92, mean(q7.dat.sc2$ppt_in)*92, mean(q8.dat.sc2$ppt_in)*92, 
              mean(q9.dat.sc2$ppt_in)*91, mean(q10.dat.sc2$ppt_in)*92, mean(q11.dat.sc2$ppt_in)*92, mean(q12.dat.sc2$ppt_in)*90.25))]


precip.driest.quarter.sc2 <- precip.driest.quarter.pre.sc2/30

#value
precip.driest.quarter.sc2

##Precipitation of Coldest Quarter

precip.coldest.quarter.pre.sc2 <- c(sum(q1.dat.sc2$ppt_in), sum(q2.dat.sc2$ppt_in), sum(q3.dat.sc2$ppt_in), sum(q4.dat.sc2$ppt_in),
                                    sum(q5.dat.sc2$ppt_in), sum(q6.dat.sc2$ppt_in), sum(q7.dat.sc2$ppt_in), sum(q8.dat.sc2$ppt_in),
                                    sum(q9.dat.sc2$ppt_in), sum(q10.dat.sc2$ppt_in), sum(q11.dat.sc2$ppt_in), sum(q12.dat.sc2$ppt_in)
)[which.min(c(mean(q1.dat.sc2$taveF), mean(q2.dat.sc2$taveF), mean(q3.dat.sc2$taveF), mean(q4.dat.sc2$taveF), 
              mean(q5.dat.sc2$taveF), mean(q6.dat.sc2$taveF), mean(q7.dat.sc2$taveF), mean(q8.dat.sc2$taveF), 
              mean(q9.dat.sc2$taveF), mean(q10.dat.sc2$taveF), mean(q11.dat.sc2$taveF), mean(q12.dat.sc2$taveF)))]

#previous calculation is the sum over   30 year historical period of said quarter
#divide by 10 to get the annual quarter calculation 
precip.coldest.quarter.sc2 <- precip.coldest.quarter.pre.sc2/30

#value
precip.coldest.quarter.sc2

##Precipitation of the Warmest Quarter

precip.warmest.quarter.pre.sc2 <- c(sum(q1.dat.sc2$ppt_in), sum(q2.dat.sc2$ppt_in), sum(q3.dat.sc2$ppt_in), sum(q4.dat.sc2$ppt_in),
                                    sum(q5.dat.sc2$ppt_in), sum(q6.dat.sc2$ppt_in), sum(q7.dat.sc2$ppt_in), sum(q8.dat.sc2$ppt_in),
                                    sum(q9.dat.sc2$ppt_in), sum(q10.dat.sc2$ppt_in), sum(q11.dat.sc2$ppt_in), sum(q12.dat.sc2$ppt_in)
)[which.max(c(mean(q1.dat.sc2$taveF), mean(q2.dat.sc2$taveF), mean(q3.dat.sc2$taveF), mean(q4.dat.sc2$taveF), 
              mean(q5.dat.sc2$taveF), mean(q6.dat.sc2$taveF), mean(q7.dat.sc2$taveF), mean(q8.dat.sc2$taveF), 
              mean(q9.dat.sc2$taveF), mean(q10.dat.sc2$taveF), mean(q11.dat.sc2$taveF), mean(q12.dat.sc2$taveF)))]

#previous calculation is the sum over 30 year projected period of said quarter
#divide by 10 to get the annual quarter calculation 
precip.warmest.quarter.sc2 <- precip.warmest.quarter.pre.sc2/30

#value
precip.warmest.quarter.sc2

#####RCP 8.5 2030

#setting aside appropriate data

month <-        c(month.abb)
month           #make sure the data lines up with table dude
sc3.temp.av <- c(dat.RCP8.5.2030$taveF)
sc3.temp.av   #make sure the data lines up with table dude
sc3.temp.hi <- c(dat.RCP8.5.2030$tmaxF)
sc3.temp.hi    #make sure the data lines up with table dude
sc3.temp.lo <- c(dat.RCP8.5.2030$tminF)
sc3.temp.lo    #make sure the data lines up with table dude
sc3.precip <-  c(dat.RCP8.5.2030$ppt_in)
sc3.precip     #make sure the data lines up with table dude

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

jan.tav.sc3 <- mean(jan.dat.sc3$taveF)
feb.tav.sc3 <- mean(feb.dat.sc3$taveF)
mar.tav.sc3 <- mean(mar.dat.sc3$taveF)
apr.tav.sc3 <- mean(apr.dat.sc3$taveF)
may.tav.sc3 <- mean(may.dat.sc3$taveF)
jun.tav.sc3 <- mean(jun.dat.sc3$taveF)
jul.tav.sc3 <- mean(jul.dat.sc3$taveF)
aug.tav.sc3 <- mean(aug.dat.sc3$taveF)
sep.tav.sc3 <- mean(sep.dat.sc3$taveF)
oct.tav.sc3 <- mean(oct.dat.sc3$taveF)
nov.tav.sc3 <- mean(nov.dat.sc3$taveF)
dec.tav.sc3 <- mean(dec.dat.sc3$taveF)

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

jan.prav.dat.sc3 <- (sum(jan.dat.sc3$ppt_in))/30
feb.prav.dat.sc3 <- (sum(feb.dat.sc3$ppt_in))/30
mar.prav.dat.sc3 <- (sum(mar.dat.sc3$ppt_in))/30
apr.prav.dat.sc3 <- (sum(apr.dat.sc3$ppt_in))/30
may.prav.dat.sc3 <- (sum(may.dat.sc3$ppt_in))/30
jun.prav.dat.sc3 <- (sum(jun.dat.sc3$ppt_in))/30
jul.prav.dat.sc3 <- (sum(jul.dat.sc3$ppt_in))/30
aug.prav.dat.sc3 <- (sum(aug.dat.sc3$ppt_in))/30
sep.prav.dat.sc3 <- (sum(sep.dat.sc3$ppt_in))/30
oct.prav.dat.sc3 <- (sum(oct.dat.sc3$ppt_in))/30
nov.prav.dat.sc3 <- (sum(nov.dat.sc3$ppt_in))/30
dec.prav.dat.sc3 <- (sum(dec.dat.sc3$ppt_in))/30

## 1) Annual Mean Diurnal Range
#difference between month's max and min temp averaged over 12 months
#SUMof(Tmax-Tmin)/12

jan.max.sc3 <- mean(jan.dat.sc3$tmaxF)
jan.min.sc3 <- mean(jan.dat.sc3$tminF)

feb.max.sc3 <- mean(feb.dat.sc3$tmaxF)
feb.min.sc3 <- mean(feb.dat.sc3$tminF)

mar.max.sc3 <- mean(mar.dat.sc3$tmaxF)
mar.min.sc3 <- mean(mar.dat.sc3$tminF)

apr.max.sc3 <- mean(apr.dat.sc3$tmaxF)
apr.min.sc3 <- mean(apr.dat.sc3$tminF)

may.max.sc3 <- mean(may.dat.sc3$tmaxF)
may.min.sc3 <- mean(may.dat.sc3$tminF)

jun.max.sc3 <- mean(jun.dat.sc3$tmaxF)
jun.min.sc3 <- mean(jun.dat.sc3$tminF)

jul.max.sc3 <- mean(jul.dat.sc3$tmaxF)
jul.min.sc3 <- mean(jul.dat.sc3$tminF)

aug.max.sc3 <- mean(aug.dat.sc3$tmaxF)
aug.min.sc3 <- mean(aug.dat.sc3$tminF)

sep.max.sc3 <- mean(sep.dat.sc3$tmaxF)
sep.min.sc3 <- mean(sep.dat.sc3$tminF)

oct.max.sc3 <- mean(oct.dat.sc3$tmaxF)
oct.min.sc3 <- mean(oct.dat.sc3$tminF)

nov.max.sc3 <- mean(nov.dat.sc3$tmaxF)
nov.min.sc3 <- mean(nov.dat.sc3$tminF)

dec.max.sc3 <- mean(dec.dat.sc3$tmaxF)
dec.min.sc3 <- mean(dec.dat.sc3$tminF)

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
abs.tmax.pre.sc3 <- max(dat.RCP8.5.2030$tmaxF)
abs.tmax.c.sc3 <- f.to.c(abs.tmax.pre.sc3)
abs.tmin.pre.sc3 <- min(dat.RCP8.5.2030$tminF)
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
annual.av.f.sc3 <- mean(dat.RCP8.5.2030$taveF)

#convert it to Kelvin
annual.av.k.sc3. <- f.to.k(annual.av.f.sc3)

##Temperature Seasonality CV
cv.temperature.seasonality.sc3 <- ((sd.mmk.sc3)/annual.av.k.sc3.)*100

#value
cv.temperature.seasonality.sc3

##Max Temperature of Warmest Month
#lets first figure out the warmest month
#saving it in case we need it
warmest.day.warmest.month.sc3 <- c(max(jan.dat.sc3$tmaxF), max(feb.dat.sc3$tmaxF), max(mar.dat.sc3$tmaxF), max(apr.dat.sc3$tmaxF),
                                   max(may.dat.sc3$tmaxF), max(jun.dat.sc3$tmaxF), max(jul.dat.sc3$tmaxF), max(aug.dat.sc3$tmaxF),
                                   max(sep.dat.sc3$tmaxF), max(oct.dat.sc3$tmaxF), max(nov.dat.sc3$tmaxF), max(dec.dat.sc3$tmaxF)
)[which.max(c(mean(jan.dat.sc3$tmaxF), mean(feb.dat.sc3$tmaxF), mean(mar.dat.sc3$tmaxF), mean(apr.dat.sc3$tmaxF), 
              mean(may.dat.sc3$tmaxF), mean(jun.dat.sc3$tmaxF), mean(jul.dat.sc3$tmaxF), mean(aug.dat.sc3$tmaxF), 
              mean(sep.dat.sc3$tmaxF), mean(oct.dat.sc3$tmaxF), mean(nov.dat.sc3$tmaxF), mean(dec.dat.sc3$tmaxF)))]

#value
warmest.day.warmest.month.sc3

##Min Temperature of Coldest Month
#saving it in case they want it
coldest.day.coldest.month.sc3 <- c(min(jan.dat.sc3$tminF), min(feb.dat.sc3$tminF), min(mar.dat.sc3$tminF), min(apr.dat.sc3$tminF),
                                   min(may.dat.sc3$tminF), min(jun.dat.sc3$tminF), min(jul.dat.sc3$tminF), min(aug.dat.sc3$tminF),
                                   min(sep.dat.sc3$tminF), min(oct.dat.sc3$tminF), min(nov.dat.sc3$tminF), min(dec.dat.sc3$tminF)
)[which.min(c(mean(jan.dat.sc3$tminF), mean(feb.dat.sc3$tminF), mean(mar.dat.sc3$tminF), mean(apr.dat.sc3$tminF), 
              mean(may.dat.sc3$tminF), mean(jun.dat.sc3$tminF), mean(jul.dat.sc3$tminF), mean(aug.dat.sc3$tminF), 
              mean(sep.dat.sc3$tminF), mean(oct.dat.sc3$tminF), mean(nov.dat.sc3$tminF), mean(dec.dat.sc3$tminF)))]

#value
coldest.day.coldest.month.sc3

##Annual Temperature Range
#hottest day of hottest month minus coldest day of coldest month

annual.temperature.range.sc3 <- warmest.day.warmest.month.sc3 - coldest.day.coldest.month.sc3

#value
annual.temperature.range.sc3

##Mean Temperature of Wettest Quarter

mean.temp.wettest.quarter.sc3 <- c(mean(q1.dat.sc3$taveF), mean(q2.dat.sc3$taveF), mean(q3.dat.sc3$taveF), mean(q4.dat.sc3$taveF),
                                   mean(q5.dat.sc3$taveF), mean(q6.dat.sc3$taveF), mean(q7.dat.sc3$taveF), mean(q8.dat.sc3$taveF),
                                   mean(q9.dat.sc3$taveF), mean(q10.dat.sc3$taveF), mean(q11.dat.sc3$taveF), mean(q12.dat.sc3$taveF)
)[which.max(c(mean(q1.dat.sc3$ppt_in)*90.25, mean(q2.dat.sc3$ppt_in)*89.25, mean(q3.dat.sc3$ppt_in)*92, mean(q4.dat.sc3$ppt_in)*91, 
              mean(q5.dat.sc3$ppt_in)*92, mean(q6.dat.sc3$ppt_in)*92, mean(q7.dat.sc3$ppt_in)*92, mean(q8.dat.sc3$ppt_in)*92, 
              mean(q9.dat.sc3$ppt_in)*91, mean(q10.dat.sc3$ppt_in)*92, mean(q11.dat.sc3$ppt_in)*92, mean(q12.dat.sc3$ppt_in)*90.25))]


#value
mean.temp.wettest.quarter.sc3

##Mean Temperature of Driest Quarter

mean.temp.driest.quarter.sc3 <- c(mean(q1.dat.sc3$taveF), mean(q2.dat.sc3$taveF), mean(q3.dat.sc3$taveF), mean(q4.dat.sc3$taveF),
                                  mean(q5.dat.sc3$taveF), mean(q6.dat.sc3$taveF), mean(q7.dat.sc3$taveF), mean(q8.dat.sc3$taveF),
                                  mean(q9.dat.sc3$taveF), mean(q10.dat.sc3$taveF), mean(q11.dat.sc3$taveF), mean(q12.dat.sc3$taveF)
)[which.min(c(mean(q1.dat.sc3$ppt_in)*90.25, mean(q2.dat.sc3$ppt_in)*89.25, mean(q3.dat.sc3$ppt_in)*92, mean(q4.dat.sc3$ppt_in)*91, 
              mean(q5.dat.sc3$ppt_in)*92, mean(q6.dat.sc3$ppt_in)*92, mean(q7.dat.sc3$ppt_in)*92, mean(q8.dat.sc3$ppt_in)*92, 
              mean(q9.dat.sc3$ppt_in)*91, mean(q10.dat.sc3$ppt_in)*92, mean(q11.dat.sc3$ppt_in)*92, mean(q12.dat.sc3$ppt_in)*90.25))]

#value
mean.temp.driest.quarter.sc3

##Mean Temperature of Warmest Quarter

mean.temp.warmest.quarter.sc3 <- c(mean(q1.dat.sc3$taveF), mean(q2.dat.sc3$taveF), mean(q3.dat.sc3$taveF), mean(q4.dat.sc3$taveF),
                                   mean(q5.dat.sc3$taveF), mean(q6.dat.sc3$taveF), mean(q7.dat.sc3$taveF), mean(q8.dat.sc3$taveF),
                                   mean(q9.dat.sc3$taveF), mean(q10.dat.sc3$taveF), mean(q11.dat.sc3$taveF), mean(q12.dat.sc3$taveF)
)[which.max(c(mean(q1.dat.sc3$taveF), mean(q2.dat.sc3$taveF), mean(q3.dat.sc3$taveF), mean(q4.dat.sc3$taveF), 
              mean(q5.dat.sc3$taveF), mean(q6.dat.sc3$taveF), mean(q7.dat.sc3$taveF), mean(q8.dat.sc3$taveF), 
              mean(q9.dat.sc3$taveF), mean(q10.dat.sc3$taveF), mean(q11.dat.sc3$taveF), mean(q12.dat.sc3$taveF)))]

#value
mean.temp.warmest.quarter.sc3

##Mean Temperature of Coldest Quarter

mean.temp.coldest.quarter.sc3 <- c(mean(q1.dat.sc3$taveF), mean(q2.dat.sc3$taveF), mean(q3.dat.sc3$taveF), mean(q4.dat.sc3$taveF),
                                   mean(q5.dat.sc3$taveF), mean(q6.dat.sc3$taveF), mean(q7.dat.sc3$taveF), mean(q8.dat.sc3$taveF),
                                   mean(q9.dat.sc3$taveF), mean(q10.dat.sc3$taveF), mean(q11.dat.sc3$taveF), mean(q12.dat.sc3$taveF)
)[which.min(c(mean(q1.dat.sc3$taveF), mean(q2.dat.sc3$taveF), mean(q3.dat.sc3$taveF), mean(q4.dat.sc3$taveF), 
              mean(q5.dat.sc3$taveF), mean(q6.dat.sc3$taveF), mean(q7.dat.sc3$taveF), mean(q8.dat.sc3$taveF), 
              mean(q9.dat.sc3$taveF), mean(q10.dat.sc3$taveF), mean(q11.dat.sc3$taveF), mean(q12.dat.sc3$taveF)))]

#value
mean.temp.coldest.quarter.sc3

##Precipitation of Wettest Month

precip.wettest.month.pre.sc3 <- c(jan.prav.dat.sc3, feb.prav.dat.sc3, mar.prav.dat.sc3, apr.prav.dat.sc3, may.prav.dat.sc3, jun.prav.dat.sc3,
                                  jul.prav.dat.sc3, aug.prav.dat.sc3, sep.prav.dat.sc3, oct.prav.dat.sc3, nov.prav.dat.sc3, dec.prav.dat.sc3
)[which.max(c(jan.prav.dat.sc3, feb.prav.dat.sc3, mar.prav.dat.sc3, apr.prav.dat.sc3, may.prav.dat.sc3, jun.prav.dat.sc3,
              jul.prav.dat.sc3, aug.prav.dat.sc3, sep.prav.dat.sc3, oct.prav.dat.sc3, nov.prav.dat.sc3, dec.prav.dat.sc3))]

#since we are using sums of precip for a given month over a 30 year projected period we need to divide
#by 10 to get the sum of ones months precip

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

#annual precipitation / getting the sum of over 30 years then dividing by 10 to get annual average
an.precip.sc3 <- (sum(dat.RCP8.5.2030$ppt_mm)/30)
mo.precip.sc3 <- an.precip.sc3/12

#final calculation
precip.seasonality.cv.sc3 <- (sd.precip.sc3/(1+mo.precip.sc3))*100

#value
precip.seasonality.cv.sc3

##Precipitation of Wettest Quarter

precip.wettest.quarter.pre.sc3 <- c(sum(q1.dat.sc3$ppt_in), sum(q2.dat.sc3$ppt_in), sum(q3.dat.sc3$ppt_in), sum(q4.dat.sc3$ppt_in),
                                    sum(q5.dat.sc3$ppt_in), sum(q6.dat.sc3$ppt_in), sum(q7.dat.sc3$ppt_in), sum(q8.dat.sc3$ppt_in),
                                    sum(q9.dat.sc3$ppt_in), sum(q10.dat.sc3$ppt_in), sum(q11.dat.sc3$ppt_in), sum(q12.dat.sc3$ppt_in)
)[which.max(c(mean(q1.dat.sc3$ppt_in)*90.25, mean(q2.dat.sc3$ppt_in)*89.25, mean(q3.dat.sc3$ppt_in)*92, mean(q4.dat.sc3$ppt_in)*91, 
              mean(q5.dat.sc3$ppt_in)*92, mean(q6.dat.sc3$ppt_in)*92, mean(q7.dat.sc3$ppt_in)*92, mean(q8.dat.sc3$ppt_in)*92, 
              mean(q9.dat.sc3$ppt_in)*91, mean(q10.dat.sc3$ppt_in)*92, mean(q11.dat.sc3$ppt_in)*92, mean(q12.dat.sc3$ppt_in)*90.25))]

precip.wettest.quarter.sc3 <- precip.wettest.quarter.pre.sc3/30

#value
precip.wettest.quarter.sc3

##Precipitation of Driest Quarter

precip.driest.quarter.pre.sc3 <- c(sum(q1.dat.sc3$ppt_in), sum(q2.dat.sc3$ppt_in), sum(q3.dat.sc3$ppt_in), sum(q4.dat.sc3$ppt_in),
                                   sum(q5.dat.sc3$ppt_in), sum(q6.dat.sc3$ppt_in), sum(q7.dat.sc3$ppt_in), sum(q8.dat.sc3$ppt_in),
                                   sum(q9.dat.sc3$ppt_in), sum(q10.dat.sc3$ppt_in), sum(q11.dat.sc3$ppt_in), sum(q12.dat.sc3$ppt_in)
)[which.min(c(mean(q1.dat.sc3$ppt_in)*90.25, mean(q2.dat.sc3$ppt_in)*89.25, mean(q3.dat.sc3$ppt_in)*92, mean(q4.dat.sc3$ppt_in)*91, 
              mean(q5.dat.sc3$ppt_in)*92, mean(q6.dat.sc3$ppt_in)*92, mean(q7.dat.sc3$ppt_in)*92, mean(q8.dat.sc3$ppt_in)*92, 
              mean(q9.dat.sc3$ppt_in)*91, mean(q10.dat.sc3$ppt_in)*92, mean(q11.dat.sc3$ppt_in)*92, mean(q12.dat.sc3$ppt_in)*90.25))]


precip.driest.quarter.sc3 <- precip.driest.quarter.pre.sc3/30

#value
precip.driest.quarter.sc3

##Precipitation of Coldest Quarter

precip.coldest.quarter.pre.sc3 <- c(sum(q1.dat.sc3$ppt_in), sum(q2.dat.sc3$ppt_in), sum(q3.dat.sc3$ppt_in), sum(q4.dat.sc3$ppt_in),
                                    sum(q5.dat.sc3$ppt_in), sum(q6.dat.sc3$ppt_in), sum(q7.dat.sc3$ppt_in), sum(q8.dat.sc3$ppt_in),
                                    sum(q9.dat.sc3$ppt_in), sum(q10.dat.sc3$ppt_in), sum(q11.dat.sc3$ppt_in), sum(q12.dat.sc3$ppt_in)
)[which.min(c(mean(q1.dat.sc3$taveF), mean(q2.dat.sc3$taveF), mean(q3.dat.sc3$taveF), mean(q4.dat.sc3$taveF), 
              mean(q5.dat.sc3$taveF), mean(q6.dat.sc3$taveF), mean(q7.dat.sc3$taveF), mean(q8.dat.sc3$taveF), 
              mean(q9.dat.sc3$taveF), mean(q10.dat.sc3$taveF), mean(q11.dat.sc3$taveF), mean(q12.dat.sc3$taveF)))]

#previous calculation is the sum over   30 year historical period of said quarter
#divide by 10 to get the annual quarter calculation 
precip.coldest.quarter.sc3 <- precip.coldest.quarter.pre.sc3/30

#value
precip.coldest.quarter.sc3

##Precipitation of the Warmest Quarter

precip.warmest.quarter.pre.sc3 <- c(sum(q1.dat.sc3$ppt_in), sum(q2.dat.sc3$ppt_in), sum(q3.dat.sc3$ppt_in), sum(q4.dat.sc3$ppt_in),
                                    sum(q5.dat.sc3$ppt_in), sum(q6.dat.sc3$ppt_in), sum(q7.dat.sc3$ppt_in), sum(q8.dat.sc3$ppt_in),
                                    sum(q9.dat.sc3$ppt_in), sum(q10.dat.sc3$ppt_in), sum(q11.dat.sc3$ppt_in), sum(q12.dat.sc3$ppt_in)
)[which.max(c(mean(q1.dat.sc3$taveF), mean(q2.dat.sc3$taveF), mean(q3.dat.sc3$taveF), mean(q4.dat.sc3$taveF), 
              mean(q5.dat.sc3$taveF), mean(q6.dat.sc3$taveF), mean(q7.dat.sc3$taveF), mean(q8.dat.sc3$taveF), 
              mean(q9.dat.sc3$taveF), mean(q10.dat.sc3$taveF), mean(q11.dat.sc3$taveF), mean(q12.dat.sc3$taveF)))]

#previous calculation is the sum over 30 year projected period of said quarter
#divide by 10 to get the annual quarter calculation 
precip.warmest.quarter.sc3 <- precip.warmest.quarter.pre.sc3/30

#value
precip.warmest.quarter.sc3


#####RCP 8.5 2050

#setting aside appropriate data

month <-        c(month.abb)
month           #make sure the data lines up with table dude
sc4.temp.av <- c(dat.RCP8.5.2050$taveF)
sc4.temp.av   #make sure the data lines up with table dude
sc4.temp.hi <- c(dat.RCP8.5.2050$tmaxF)
sc4.temp.hi    #make sure the data lines up with table dude
sc4.temp.lo <- c(dat.RCP8.5.2050$tminF)
sc4.temp.lo    #make sure the data lines up with table dude
sc4.precip <-  c(dat.RCP8.5.2050$ppt_in)
sc4.precip     #make sure the data lines up with table dude

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

jan.tav.sc4 <- mean(jan.dat.sc4$taveF)
feb.tav.sc4 <- mean(feb.dat.sc4$taveF)
mar.tav.sc4 <- mean(mar.dat.sc4$taveF)
apr.tav.sc4 <- mean(apr.dat.sc4$taveF)
may.tav.sc4 <- mean(may.dat.sc4$taveF)
jun.tav.sc4 <- mean(jun.dat.sc4$taveF)
jul.tav.sc4 <- mean(jul.dat.sc4$taveF)
aug.tav.sc4 <- mean(aug.dat.sc4$taveF)
sep.tav.sc4 <- mean(sep.dat.sc4$taveF)
oct.tav.sc4 <- mean(oct.dat.sc4$taveF)
nov.tav.sc4 <- mean(nov.dat.sc4$taveF)
dec.tav.sc4 <- mean(dec.dat.sc4$taveF)

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

jan.prav.dat.sc4 <- (sum(jan.dat.sc4$ppt_in))/30
feb.prav.dat.sc4 <- (sum(feb.dat.sc4$ppt_in))/30
mar.prav.dat.sc4 <- (sum(mar.dat.sc4$ppt_in))/30
apr.prav.dat.sc4 <- (sum(apr.dat.sc4$ppt_in))/30
may.prav.dat.sc4 <- (sum(may.dat.sc4$ppt_in))/30
jun.prav.dat.sc4 <- (sum(jun.dat.sc4$ppt_in))/30
jul.prav.dat.sc4 <- (sum(jul.dat.sc4$ppt_in))/30
aug.prav.dat.sc4 <- (sum(aug.dat.sc4$ppt_in))/30
sep.prav.dat.sc4 <- (sum(sep.dat.sc4$ppt_in))/30
oct.prav.dat.sc4 <- (sum(oct.dat.sc4$ppt_in))/30
nov.prav.dat.sc4 <- (sum(nov.dat.sc4$ppt_in))/30
dec.prav.dat.sc4 <- (sum(dec.dat.sc4$ppt_in))/30

## 1) Annual Mean Diurnal Range
#difference between month's max and min temp averaged over 12 months
#SUMof(Tmax-Tmin)/12

jan.max.sc4 <- mean(jan.dat.sc4$tmaxF)
jan.min.sc4 <- mean(jan.dat.sc4$tminF)

feb.max.sc4 <- mean(feb.dat.sc4$tmaxF)
feb.min.sc4 <- mean(feb.dat.sc4$tminF)

mar.max.sc4 <- mean(mar.dat.sc4$tmaxF)
mar.min.sc4 <- mean(mar.dat.sc4$tminF)

apr.max.sc4 <- mean(apr.dat.sc4$tmaxF)
apr.min.sc4 <- mean(apr.dat.sc4$tminF)

may.max.sc4 <- mean(may.dat.sc4$tmaxF)
may.min.sc4 <- mean(may.dat.sc4$tminF)

jun.max.sc4 <- mean(jun.dat.sc4$tmaxF)
jun.min.sc4 <- mean(jun.dat.sc4$tminF)

jul.max.sc4 <- mean(jul.dat.sc4$tmaxF)
jul.min.sc4 <- mean(jul.dat.sc4$tminF)

aug.max.sc4 <- mean(aug.dat.sc4$tmaxF)
aug.min.sc4 <- mean(aug.dat.sc4$tminF)

sep.max.sc4 <- mean(sep.dat.sc4$tmaxF)
sep.min.sc4 <- mean(sep.dat.sc4$tminF)

oct.max.sc4 <- mean(oct.dat.sc4$tmaxF)
oct.min.sc4 <- mean(oct.dat.sc4$tminF)

nov.max.sc4 <- mean(nov.dat.sc4$tmaxF)
nov.min.sc4 <- mean(nov.dat.sc4$tminF)

dec.max.sc4 <- mean(dec.dat.sc4$tmaxF)
dec.min.sc4 <- mean(dec.dat.sc4$tminF)

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
abs.tmax.pre.sc4 <- max(dat.RCP8.5.2050$tmaxF)
abs.tmax.c.sc4 <- f.to.c(abs.tmax.pre.sc4)
abs.tmin.pre.sc4 <- min(dat.RCP8.5.2050$tminF)
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
annual.av.f.sc4 <- mean(dat.RCP8.5.2050$taveF)

#convert it to Kelvin
annual.av.k.sc4. <- f.to.k(annual.av.f.sc4)

##Temperature Seasonality CV
cv.temperature.seasonality.sc4 <- ((sd.mmk.sc4)/annual.av.k.sc4.)*100

#value
cv.temperature.seasonality.sc4

##Max Temperature of Warmest Month
#lets first figure out the warmest month
#saving it in case we need it
warmest.day.warmest.month.sc4 <- c(max(jan.dat.sc4$tmaxF), max(feb.dat.sc4$tmaxF), max(mar.dat.sc4$tmaxF), max(apr.dat.sc4$tmaxF),
                                   max(may.dat.sc4$tmaxF), max(jun.dat.sc4$tmaxF), max(jul.dat.sc4$tmaxF), max(aug.dat.sc4$tmaxF),
                                   max(sep.dat.sc4$tmaxF), max(oct.dat.sc4$tmaxF), max(nov.dat.sc4$tmaxF), max(dec.dat.sc4$tmaxF)
)[which.max(c(mean(jan.dat.sc4$tmaxF), mean(feb.dat.sc4$tmaxF), mean(mar.dat.sc4$tmaxF), mean(apr.dat.sc4$tmaxF), 
              mean(may.dat.sc4$tmaxF), mean(jun.dat.sc4$tmaxF), mean(jul.dat.sc4$tmaxF), mean(aug.dat.sc4$tmaxF), 
              mean(sep.dat.sc4$tmaxF), mean(oct.dat.sc4$tmaxF), mean(nov.dat.sc4$tmaxF), mean(dec.dat.sc4$tmaxF)))]

#value
warmest.day.warmest.month.sc4

##Min Temperature of Coldest Month
#saving it in case they want it
coldest.day.coldest.month.sc4 <- c(min(jan.dat.sc4$tminF), min(feb.dat.sc4$tminF), min(mar.dat.sc4$tminF), min(apr.dat.sc4$tminF),
                                   min(may.dat.sc4$tminF), min(jun.dat.sc4$tminF), min(jul.dat.sc4$tminF), min(aug.dat.sc4$tminF),
                                   min(sep.dat.sc4$tminF), min(oct.dat.sc4$tminF), min(nov.dat.sc4$tminF), min(dec.dat.sc4$tminF)
)[which.min(c(mean(jan.dat.sc4$tminF), mean(feb.dat.sc4$tminF), mean(mar.dat.sc4$tminF), mean(apr.dat.sc4$tminF), 
              mean(may.dat.sc4$tminF), mean(jun.dat.sc4$tminF), mean(jul.dat.sc4$tminF), mean(aug.dat.sc4$tminF), 
              mean(sep.dat.sc4$tminF), mean(oct.dat.sc4$tminF), mean(nov.dat.sc4$tminF), mean(dec.dat.sc4$tminF)))]

#value
coldest.day.coldest.month.sc4

##Annual Temperature Range
#hottest day of hottest month minus coldest day of coldest month

annual.temperature.range.sc4 <- warmest.day.warmest.month.sc4 - coldest.day.coldest.month.sc4

#value
annual.temperature.range.sc4

##Mean Temperature of Wettest Quarter

mean.temp.wettest.quarter.sc4 <- c(mean(q1.dat.sc4$taveF), mean(q2.dat.sc4$taveF), mean(q3.dat.sc4$taveF), mean(q4.dat.sc4$taveF),
                                   mean(q5.dat.sc4$taveF), mean(q6.dat.sc4$taveF), mean(q7.dat.sc4$taveF), mean(q8.dat.sc4$taveF),
                                   mean(q9.dat.sc4$taveF), mean(q10.dat.sc4$taveF), mean(q11.dat.sc4$taveF), mean(q12.dat.sc4$taveF)
)[which.max(c(mean(q1.dat.sc4$ppt_in)*90.25, mean(q2.dat.sc4$ppt_in)*89.25, mean(q3.dat.sc4$ppt_in)*92, mean(q4.dat.sc4$ppt_in)*91, 
              mean(q5.dat.sc4$ppt_in)*92, mean(q6.dat.sc4$ppt_in)*92, mean(q7.dat.sc4$ppt_in)*92, mean(q8.dat.sc4$ppt_in)*92, 
              mean(q9.dat.sc4$ppt_in)*91, mean(q10.dat.sc4$ppt_in)*92, mean(q11.dat.sc4$ppt_in)*92, mean(q12.dat.sc4$ppt_in)*90.25))]

#value
mean.temp.wettest.quarter.sc4

##Mean Temperature of Driest Quarter

mean.temp.driest.quarter.sc4 <- c(mean(q1.dat.sc4$taveF), mean(q2.dat.sc4$taveF), mean(q3.dat.sc4$taveF), mean(q4.dat.sc4$taveF),
                                  mean(q5.dat.sc4$taveF), mean(q6.dat.sc4$taveF), mean(q7.dat.sc4$taveF), mean(q8.dat.sc4$taveF),
                                  mean(q9.dat.sc4$taveF), mean(q10.dat.sc4$taveF), mean(q11.dat.sc4$taveF), mean(q12.dat.sc4$taveF)
)[which.min(c(mean(q1.dat.sc4$ppt_in)*90.25, mean(q2.dat.sc4$ppt_in)*89.25, mean(q3.dat.sc4$ppt_in)*92, mean(q4.dat.sc4$ppt_in)*91, 
              mean(q5.dat.sc4$ppt_in)*92, mean(q6.dat.sc4$ppt_in)*92, mean(q7.dat.sc4$ppt_in)*92, mean(q8.dat.sc4$ppt_in)*92, 
              mean(q9.dat.sc4$ppt_in)*91, mean(q10.dat.sc4$ppt_in)*92, mean(q11.dat.sc4$ppt_in)*92, mean(q12.dat.sc4$ppt_in)*90.25))]

#value
mean.temp.driest.quarter.sc4

##Mean Temperature of Warmest Quarter

mean.temp.warmest.quarter.sc4 <- c(mean(q1.dat.sc4$taveF), mean(q2.dat.sc4$taveF), mean(q3.dat.sc4$taveF), mean(q4.dat.sc4$taveF),
                                   mean(q5.dat.sc4$taveF), mean(q6.dat.sc4$taveF), mean(q7.dat.sc4$taveF), mean(q8.dat.sc4$taveF),
                                   mean(q9.dat.sc4$taveF), mean(q10.dat.sc4$taveF), mean(q11.dat.sc4$taveF), mean(q12.dat.sc4$taveF)
)[which.max(c(mean(q1.dat.sc4$taveF), mean(q2.dat.sc4$taveF), mean(q3.dat.sc4$taveF), mean(q4.dat.sc4$taveF), 
              mean(q5.dat.sc4$taveF), mean(q6.dat.sc4$taveF), mean(q7.dat.sc4$taveF), mean(q8.dat.sc4$taveF), 
              mean(q9.dat.sc4$taveF), mean(q10.dat.sc4$taveF), mean(q11.dat.sc4$taveF), mean(q12.dat.sc4$taveF)))]

#value
mean.temp.warmest.quarter.sc4

##Mean Temperature of Coldest Quarter

mean.temp.coldest.quarter.sc4 <- c(mean(q1.dat.sc4$taveF), mean(q2.dat.sc4$taveF), mean(q3.dat.sc4$taveF), mean(q4.dat.sc4$taveF),
                                   mean(q5.dat.sc4$taveF), mean(q6.dat.sc4$taveF), mean(q7.dat.sc4$taveF), mean(q8.dat.sc4$taveF),
                                   mean(q9.dat.sc4$taveF), mean(q10.dat.sc4$taveF), mean(q11.dat.sc4$taveF), mean(q12.dat.sc4$taveF)
)[which.min(c(mean(q1.dat.sc4$taveF), mean(q2.dat.sc4$taveF), mean(q3.dat.sc4$taveF), mean(q4.dat.sc4$taveF), 
              mean(q5.dat.sc4$taveF), mean(q6.dat.sc4$taveF), mean(q7.dat.sc4$taveF), mean(q8.dat.sc4$taveF), 
              mean(q9.dat.sc4$taveF), mean(q10.dat.sc4$taveF), mean(q11.dat.sc4$taveF), mean(q12.dat.sc4$taveF)))]

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

#annual precipitation / getting the sum of over 30 years then dividing by 10 to get annual average
an.precip.sc4 <- (sum(dat.RCP8.5.2050$ppt_mm)/30)
mo.precip.sc4 <- an.precip.sc4/12

#final calculation
precip.seasonality.cv.sc4 <- (sd.precip.sc4/(1+mo.precip.sc4))*100

#value
precip.seasonality.cv.sc4

##Precipitation of Wettest Quarter

precip.wettest.quarter.pre.sc4 <- c(sum(q1.dat.sc4$ppt_in), sum(q2.dat.sc4$ppt_in), sum(q3.dat.sc4$ppt_in), sum(q4.dat.sc4$ppt_in),
                                    sum(q5.dat.sc4$ppt_in), sum(q6.dat.sc4$ppt_in), sum(q7.dat.sc4$ppt_in), sum(q8.dat.sc4$ppt_in),
                                    sum(q9.dat.sc4$ppt_in), sum(q10.dat.sc4$ppt_in), sum(q11.dat.sc4$ppt_in), sum(q12.dat.sc4$ppt_in)
)[which.max(c(mean(q1.dat.sc4$ppt_in)*90.25, mean(q2.dat.sc4$ppt_in)*89.25, mean(q3.dat.sc4$ppt_in)*92, mean(q4.dat.sc4$ppt_in)*91, 
              mean(q5.dat.sc4$ppt_in)*92, mean(q6.dat.sc4$ppt_in)*92, mean(q7.dat.sc4$ppt_in)*92, mean(q8.dat.sc4$ppt_in)*92, 
              mean(q9.dat.sc4$ppt_in)*91, mean(q10.dat.sc4$ppt_in)*92, mean(q11.dat.sc4$ppt_in)*92, mean(q12.dat.sc4$ppt_in)*90.25))]

precip.wettest.quarter.sc4 <- precip.wettest.quarter.pre.sc4/30

#value
precip.wettest.quarter.sc4

##Precipitation of Driest Quarter

precip.driest.quarter.pre.sc4 <- c(sum(q1.dat.sc4$ppt_in), sum(q2.dat.sc4$ppt_in), sum(q3.dat.sc4$ppt_in), sum(q4.dat.sc4$ppt_in),
                                   sum(q5.dat.sc4$ppt_in), sum(q6.dat.sc4$ppt_in), sum(q7.dat.sc4$ppt_in), sum(q8.dat.sc4$ppt_in),
                                   sum(q9.dat.sc4$ppt_in), sum(q10.dat.sc4$ppt_in), sum(q11.dat.sc4$ppt_in), sum(q12.dat.sc4$ppt_in)
)[which.min(c(mean(q1.dat.sc4$ppt_in)*90.25, mean(q2.dat.sc4$ppt_in)*89.25, mean(q3.dat.sc4$ppt_in)*92, mean(q4.dat.sc4$ppt_in)*91, 
              mean(q5.dat.sc4$ppt_in)*92, mean(q6.dat.sc4$ppt_in)*92, mean(q7.dat.sc4$ppt_in)*92, mean(q8.dat.sc4$ppt_in)*92, 
              mean(q9.dat.sc4$ppt_in)*91, mean(q10.dat.sc4$ppt_in)*92, mean(q11.dat.sc4$ppt_in)*92, mean(q12.dat.sc4$ppt_in)*90.25))]

precip.driest.quarter.sc4 <- precip.driest.quarter.pre.sc4/30

#value
precip.driest.quarter.sc4

##Precipitation of Coldest Quarter

precip.coldest.quarter.pre.sc4 <- c(sum(q1.dat.sc4$ppt_in), sum(q2.dat.sc4$ppt_in), sum(q3.dat.sc4$ppt_in), sum(q4.dat.sc4$ppt_in),
                                    sum(q5.dat.sc4$ppt_in), sum(q6.dat.sc4$ppt_in), sum(q7.dat.sc4$ppt_in), sum(q8.dat.sc4$ppt_in),
                                    sum(q9.dat.sc4$ppt_in), sum(q10.dat.sc4$ppt_in), sum(q11.dat.sc4$ppt_in), sum(q12.dat.sc4$ppt_in)
)[which.min(c(mean(q1.dat.sc4$taveF), mean(q2.dat.sc4$taveF), mean(q3.dat.sc4$taveF), mean(q4.dat.sc4$taveF), 
              mean(q5.dat.sc4$taveF), mean(q6.dat.sc4$taveF), mean(q7.dat.sc4$taveF), mean(q8.dat.sc4$taveF), 
              mean(q9.dat.sc4$taveF), mean(q10.dat.sc4$taveF), mean(q11.dat.sc4$taveF), mean(q12.dat.sc4$taveF)))]

#previous calculation is the sum over   30 year historical period of said quarter
#divide by 10 to get the annual quarter calculation 
precip.coldest.quarter.sc4 <- precip.coldest.quarter.pre.sc4/30

#value
precip.coldest.quarter.sc4

##Precipitation of Warmest Quarter

precip.warmest.quarter.pre.sc4 <- c(sum(q1.dat.sc4$ppt_in), sum(q2.dat.sc4$ppt_in), sum(q3.dat.sc4$ppt_in), sum(q4.dat.sc4$ppt_in),
                                    sum(q5.dat.sc4$ppt_in), sum(q6.dat.sc4$ppt_in), sum(q7.dat.sc4$ppt_in), sum(q8.dat.sc4$ppt_in),
                                    sum(q9.dat.sc4$ppt_in), sum(q10.dat.sc4$ppt_in), sum(q11.dat.sc4$ppt_in), sum(q12.dat.sc4$ppt_in)
)[which.max(c(mean(q1.dat.sc4$taveF), mean(q2.dat.sc4$taveF), mean(q3.dat.sc4$taveF), mean(q4.dat.sc4$taveF), 
              mean(q5.dat.sc4$taveF), mean(q6.dat.sc4$taveF), mean(q7.dat.sc4$taveF), mean(q8.dat.sc4$taveF), 
              mean(q9.dat.sc4$taveF), mean(q10.dat.sc4$taveF), mean(q11.dat.sc4$taveF), mean(q12.dat.sc4$taveF)))]

#previous calculation is the sum over 30 year projected period of said quarter
#divide by 10 to get the annual quarter calculation 
precip.warmest.quarter.sc4 <- precip.warmest.quarter.pre.sc4/30

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
                       "Temperature Seasonality (Standard Deviation), ?F",
                       "Temperature Seasonality (Coefficient of Variation), %", 
                       "Max Temperature of Warmest Month, ?F", 
                       "Min Temperature of Coldest Month, ?F",
                       "Annual Temperature Range, ?F", 
                       "Mean Temperature of Wettest Quarter, ?F", 
                       "Mean Temperature of Driest Quarter, ?F",
                       "Mean Temperature of Warmest Quarter, ?F",
                       "Mean Temperature of Coldest Quarter, ?F",
                       "Precipitation of Wettest Month, inches",
                       "Precipitation of Driest Month, inches",
                       "Precipitation Seasonality (Coefficient of Variation), %",
                       "Precipitation of Wettest Quarter, inches",
                       "Precipitation of Driest Quarter, inches",
                       "Precipitation of Coldest Quarter, inches",
                       "Precipitation of Warmest Quarter, inches")

#check out that beautiful table                    
env.var

#change WD to the appropriate output
setwd("N://RStor//mindyc//afccm//Climate Modeling//Results_MPI-ESM1-2-HR//CNMI_Proxy_v1//Bioclimatics")

#export/save file, and give it a name
write.csv(env.var, "CNMI_Proxy_v1_Bioclimatic_Variables_fix.csv")

#the end
#AMATO
