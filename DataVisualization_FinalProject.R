if(interactive()){
  ## Remove all objects; perform garbage collection
  rm(list=ls())
  gc(reset=TRUE)
  ## Detach libraries that are not used
  geneorama::detach_nonstandard_packages()
}
## Load libraries that are used
geneorama::loadinstall_libraries(c("data.table", 'zipcode', 'noncensus', ‘lubridate’, ‘plyr’, ‘dplyr’, ))
#======================business license and population ==============
abl_2012 <- read.csv('2012_Business_License_Data__Updated_June_2013_.csv')
abl_2011 <- read.csv('2011_Active_Business_License_Data.csv')
abl_2013 <- read.csv('2013_Active_Business_License_Data.csv')
abl_2014 <- read.csv('2014_Active_Business_License_Data.csv')
abl_2015 <- read.csv('2015_Active_Business_License_Data.csv')
abl_2012 <- data.table(abl_2012)
abl_2012 <- abl_2012[, list(business_legal_name, Ownership, trade_name, naics_code, NACISDesc, lic_start_date, city_state_zip)]
year <- c(2012)
abl_2012 <- cbind(abl_2012, year)
year <- c(2011)
abl_2011 <- cbind(abl_2011, year)
year <- c(2013)
abl_2013 <- cbind(abl_2013, year)
year <- c(2014)
abl_2014 <- cbind(abl_2014, year)
year <- c(2015)
abl_2015 <- cbind(abl_2015, year)
colnames(abl_2011) <- c('business_legal_name', 'Ownership', 'trade_name', 'naics_code', 'NACISDesc', 'lic_start_date', 'city_state_zip', 'year')
colnames(abl_2013) <- c('business_legal_name', 'Ownership', 'trade_name', 'naics_code', 'NACISDesc', 'lic_start_date', 'city_state_zip', 'year')
colnames(abl_2014) <- c('business_legal_name', 'Ownership', 'trade_name', 'naics_code', 'NACISDesc', 'lic_start_date', 'city_state_zip', 'year')
colnames(abl_2015) <- c('business_legal_name', 'Ownership', 'trade_name', 'naics_code', 'NACISDesc', 'lic_start_date', 'city_state_zip', 'year')
total <- rbind(abl_2011, abl_2012, abl_2013, abl_2014, abl_2015, fill=TRUE)
data(zipcode)
total <- data.table(total)
total[, c("city", "zip"):=tstrsplit(city_state_zip, "WA ", fixed=TRUE)]
#some NA in zip code, other states, get rid of them
total <- na.omit(total)
#get rid of space in zip
total$zip <- gsub(" ", "", total$zip, fixed = T)
total$zip <-  substr(total$zip, 1, 5)
setkey(total, zip)
zipcode <- data.table(zipcode)
setkey(zipcode, zip)
total <- merge(x=total, 
               y=zipcode,
               by="zip",
               all.x = TRUE)
sum(is.na(total$latitude)) #184
sum(is.na(total$zip)) #0
total <- na.omit(total)
#some address are wrong, in other states
total <- subset(total, state == 'WA')
data(zip_codes)
data(counties)
state_fips  = as.numeric(as.character(counties$state_fips))
county_fips = as.numeric(as.character(counties$county_fips))    
counties$fips = state_fips*1000+county_fips    
zip_codes$fips =  as.numeric(as.character(zip_codes$fips))
zip_codes <- data.table(zip_codes)
counties <- data.table(counties)
zip_codes <- zip_codes[, list(zip, fips)]
counties <- counties[, list(county_name, fips, population)]
setkey(zip_codes, zip)
total <- merge(x=total, 
               y=zip_codes,
               by="zip",
               all.x = TRUE)
setkey(total, fips)
setkey(counties, fips)
total <- merge(x=total, 
               y=counties,
               by="fips",
               all.x = TRUE)
total_short <- total[, list(year, county_name, population, zip, city.y, state, latitude, longitude)]
total_short <- total_short[order(year)]
total_short <- data.table(total_short)
total_short$X <- NULL
total_short <- total_short[, list(year, county_name, population)]
setnames(total_short, 'county_name', 'county')
total_short <- total_short[ , count := .N, by = list(year, county)]
total_short <-  data.frame(total_short)
total_short$ID <- id(total_short[c("year", 'county')], drop = FALSE)
total_short <- data.table(total_short)
total_short <- total_short[order(ID)]
setkey(total_short, ID)
total_short <- unique(total_short)
total_short <- total_short[, list(year, count, county, population)]
total_order <- total_short[order(county)]
makeup <- matrix(c(2011, 0, 'Asotin County', 21623, 2012, 0, 'Asotin County', 21623, 2014, 1, 'Asotin County', 21623, 2011, 0, 'Pacific County', 20920, 2011, 0, 'Wahkiakum County', 3978, 2012, 0, 'Wahkiakum County', 3978, 2013, 1, 'Wahkiakum County', 3978)
                 , ncol = 4, byrow = T)
makeup <- data.frame(makeup)
makeup$X1 <- as.numeric(as.character(makeup$X1))
makeup$X2 <- as.numeric(as.character(makeup$X2))
makeup$X4 <- as.numeric(as.character(makeup$X4))
colnames(makeup) <- colnames(total_short)
total_short <- rbind(total_short, makeup)
total_short <- total_short[order(county, year)]
p2011 = subset(total_short, year==2011)
p2012 = subset(total_short, year==2012)
p2013 = subset(total_short, year==2013)
p2014 = subset(total_short, year==2014)
p2015 = subset(total_short, year==2015)
p2012$population <- round(p2011$population * 1.05)
p2013$population <- round(p2012$population * 1.05)
p2014$population <- round(p2013$population * 1.05)
p2015$population <- round(p2014$population * 1.05)
total_popu <- rbind(p2011, p2012, p2013, p2014, p2015)
total_popu <- total_popu[order(county, year)]
str(total_popu)
total_popu$county <- as.character(total_popu$county)
total_popu$county <- sapply(total_popu$county, tolower)
set.seed(9)
count <- sample(30000:83861,nrow(total_popu),replace=T)
total_popu$count <- NULL
total_popu <- cbind(total_popu, count)
str(total_popu)
total_popu <- data.table(total_popu)
total_popu <- total_popu[, list(year, count, county, population)]
write.csv(total_popu, 'active_business_2011_2015_population_generated.csv')
#======================crime data =================================
crime <- read.csv('Seattle_Police_Department_911_Incident_Response.csv')
crime <- setNames(crime, gsub("\\.","_",colnames(crime)))
crime$Date <- as.Date(crime$Event_Clearance_Date, "%m/%d/%y")
sum(is.na(crime$Date))
crime$Event_Clearance_Date <- NULL
crime <- subset(crime, !is.na(Date))
crime <- data.table(crime)
crime <- crime[Date >= as.IDate('2011-01-01') & Date < as.IDate('2016-01-01')]
crime$Event_Clearance_SubGroup <- gsub('-', '', crime$Event_Clearance_SubGroup)
crime$Event_Clearance_Description <- NULL
sum(is.na(crime$Event_Clearance_SubGroup))
crime$Event_Clearance_SubGroup <- NULL
set.seed(7)
ss <- sample(c('burglary', 'larceny', 'sexual assault', 'homicide'),size=nrow(crime),replace=TRUE,prob=c(0.4, 0.3, 0.2, 0.1))
crime$type <- ss
set.seed(8)
county <- sample(c("Asotin County", "Benton County", "Chelan County", "Clallam County", "Clark County", "Cowlitz County", "Douglas County", "Ferry County", "Franklin County", "Grant County", "Grays Harbor County", "Island County", "Jefferson County", "King County", "Kitsap County", "Kittitas County", "Klickitat County", "Lewis County", "Lincoln County", "Mason County", "Okanogan County", "Pacific County", "Pend Oreille County", "Pierce County", "San Juan County", "Skagit County", "Skamania County", "Snohomish County", "Spokane County", "Stevens County", "Thurston County", "Wahkiakum County", "Whatcom County", "Whitman County", "Yakima County"), size=nrow(crime), replace=TRUE, prob=c(0.02, 0.004, 0.1, 0.03, 0.05, 0.001, 0.009, 0.023, 0.0065, 0.014, 0.034, 0.095, 0.0136, 0.025, 0.0087, 0.015, 0.023, 0.0056, 0.0145, 0.0087, 0.0365, 0.078, 0.0054, 0.015, 0.0025, 0.008, 0.05, 0.032, 0.065, 0.003, 0.004, 0.002, 0.1, 0.09, 0.008))
crime$county <- county
crime$Longitude <- NULL
crime$Latitude <- NULL
crime$year <- year(crime$Date)
crime$month <- month(crime$Date)
crime <- crime[, list(year, month, type, county)]
crime <- crime[ , count := .N, by = list(year, month, type, county)]
crime <-  data.frame(crime)
crime$ID <- id(crime[c("year", "month", 'county', 'type')], drop = FALSE)
crime <- data.table(crime)
setkey(crime, ID)
crime <- unique(crime)
crime <- crime[, list(year, month, type, count, county)]
crime$year <- as.factor(crime$year)
crime_year <- ddply(crime, .(year, type, county), summarize, count_sum = sum(count))
crime <- data.table(crime)
crime <- crime[order(county, type, year)]
crime$type <- as.character(crime$type)
crime$county <- as.character(crime$county)
crime$county <- sapply(crime$county, tolower)
write.csv(crime, 'crime_monthly.csv')
str(crime_year)
crime_year$type <- as.character(crime_year$type)
crime_year$county <- as.character(crime_year$county)
crime_year$county <- sapply(crime_year$county, tolower)
crime_year <- data.table(crime_year)
crime_year <- crime_year[, list(year, type, count_sum, county)]
crime_year <- crime_year[order(year, county, type)]
setnames(crime_year, 'count_sum', 'count')
write.csv(crime_year, 'crime_yearly.csv')
crime_total <- ddply(crime_year, .(type, county), summarize, count_sume = sum(count_sum))
crime_total <- data.table(crime_total)
crime_total <- crime_total[, list(type, count_sume, county)]
crime_total <- crime_total[order(county, type)]
setnames(crime_total, 'count_sume', 'count')
str(crime_total)
write.csv(crime_total, 'crime_total.csv')
