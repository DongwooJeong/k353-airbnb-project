load(url('https://www.dropbox.com/s/1nft4aj479wsqq9/AirbnbChallenge2.rdata?dl=1'))

# Data Cleansing
## BookingQ1
listing_2016Q1_booking = listing_2016Q1[listing_2016Q1$Status == 'R', ]
agg_booking_Q1 = aggregate(Status ~ PropertyID, data = listing_2016Q1_booking, FUN = length)
colnames(agg_booking_Q1) = c('PropertyID', 'BookingQ1')
property_info = merge(x = property_info, y = agg_booking_Q1, by = 'PropertyID', all.x = TRUE)
property_info$BookingQ1[is.na(property_info$BookingQ1)] = 0

## BookingQ2
listing_2016Q2_booking = listing_2016Q2[listing_2016Q2$Status == 'R', ]
agg_booking_Q2 = aggregate(Status ~ PropertyID, data = listing_2016Q2_booking, FUN = length)
colnames(agg_booking_Q2) = c('PropertyID', 'BookingQ2')
property_info = merge(x = property_info, y = agg_booking_Q2, by = 'PropertyID', all.x = TRUE)
property_info$BookingQ2[is.na(property_info$BookingQ2)] = 0


## BlockedQ1
listing_2016Q1_block = listing_2016Q1[listing_2016Q1$Status == 'B', ]
agg_block_Q1 = aggregate(Status ~ PropertyID, data = listing_2016Q1_block, FUN = length)
colnames(agg_block_Q1) = c('PropertyID', 'BlockedQ1')
property_info = merge(x = property_info, y = agg_block_Q1, by = 'PropertyID', all.x = TRUE)
property_info$BlockedQ1[is.na(property_info$BlockedQ1)] = 0

## BlockedQ2
listing_2016Q2_block = listing_2016Q2[listing_2016Q2$Status == 'B', ]
agg_block_Q2 = aggregate(Status ~ PropertyID, data = listing_2016Q2_block, FUN = length)
colnames(agg_block_Q2) = c('PropertyID', 'BlockedQ2')
property_info = merge(x = property_info, y = agg_block_Q2, by = 'PropertyID', all.x = TRUE)
property_info$BlockedQ2[is.na(property_info$BlockedQ2)] = 0

## PriceQ1
agg_price_Q1 = aggregate(Price ~ PropertyID, data = listing_2016Q1, FUN = mean)
colnames(agg_price_Q1)[2] = 'ListPriceQ1'
property_info = merge(x = property_info, y = agg_price_Q1, by = 'PropertyID', all.x = TRUE)

## PriceQ2
agg_price_Q2 = aggregate(Price ~ PropertyID, data = listing_2016Q2, FUN = mean)
colnames(agg_price_Q2)[2] = 'ListPriceQ2'
property_info = merge(x = property_info, y = agg_price_Q2, by = 'PropertyID', all.x = TRUE)

################## data cleansing from p1
#### Month

listing_2016Q1$Month = format(as.Date(listing_2016Q1$Date), "%m")
listing_2016Q2$Month = format(as.Date(listing_2016Q2$Date), "%m")

#### BookingJan

listing_2016Jan_booking = listing_2016Q1[listing_2016Q1$Month=="01" & listing_2016Q1$Status == 'R', ]
agg_booking_Jan = aggregate(Status ~ PropertyID, data = listing_2016Jan_booking, FUN = length)
colnames(agg_booking_Jan) = c("PropertyID", "BookingJan")
property_info = merge(x = property_info, y=agg_booking_Jan, by='PropertyID', all.x=TRUE)
property_info$BookingJan[is.na(property_info$BookingJan)] = 0

#### BookingFeb

listing_2016Feb_booking = listing_2016Q1[listing_2016Q1$Month=="02" & listing_2016Q1$Status == 'R', ]
agg_booking_Feb = aggregate(Status ~ PropertyID, data = listing_2016Feb_booking, FUN = length)
colnames(agg_booking_Feb) = c("PropertyID", "BookingFeb")
property_info = merge(x = property_info, y=agg_booking_Feb, by='PropertyID', all.x=TRUE)
property_info$BookingFeb[is.na(property_info$BookingFeb)] = 0

#### BookingMar

listing_2016Mar_booking = listing_2016Q1[listing_2016Q1$Month=="03" & listing_2016Q1$Status == 'R', ]
agg_booking_Mar = aggregate(Status ~ PropertyID, data = listing_2016Mar_booking, FUN = length)
colnames(agg_booking_Mar) = c("PropertyID", "BookingMar")
property_info = merge(x = property_info, y=agg_booking_Mar, by='PropertyID', all.x=TRUE)
property_info$BookingMar[is.na(property_info$BookingMar)] = 0

#### BookingApr

listing_2016Apr_booking = listing_2016Q2[listing_2016Q2$Month=="04" & listing_2016Q2$Status == 'R', ]
agg_booking_Apr = aggregate(Status ~ PropertyID, data = listing_2016Apr_booking, FUN = length)
colnames(agg_booking_Apr) = c("PropertyID", "BookingApr")
property_info = merge(x = property_info, y=agg_booking_Apr, by='PropertyID', all.x=TRUE)
property_info$BookingApr[is.na(property_info$BookingApr)] = 0

#### BookingMay

listing_2016May_booking = listing_2016Q2[listing_2016Q2$Month=="05" & listing_2016Q2$Status == 'R', ]
agg_booking_May = aggregate(Status ~ PropertyID, data = listing_2016May_booking, FUN = length)
colnames(agg_booking_May) = c("PropertyID", "BookingMay")
property_info = merge(x = property_info, y=agg_booking_May, by='PropertyID', all.x=TRUE)
property_info$BookingMay[is.na(property_info$BookingMay)] = 0

#### BookingJun

listing_2016Jun_booking = listing_2016Q2[listing_2016Q2$Month=="06" & listing_2016Q2$Status == 'R', ]
agg_booking_Jun = aggregate(Status ~ PropertyID, data = listing_2016Jun_booking, FUN = length)
colnames(agg_booking_Jun) = c("PropertyID", "BookingJun")
property_info = merge(x = property_info, y=agg_booking_Jun, by='PropertyID', all.x=TRUE)
property_info$BookingJun[is.na(property_info$BookingJun)] = 0

#### BlockedJan

listing_2016Jan_block = listing_2016Q1[listing_2016Q1$Month=="01" & listing_2016Q1$Status == 'B',]
agg_block_Jan = aggregate(Status ~ PropertyID, data = listing_2016Jan_block, FUN = length)
colnames(agg_block_Jan) = c('PropertyID','BlockedJan')
property_info = merge(x = property_info, y = agg_block_Jan, by = 'PropertyID', all.x = TRUE)
property_info$BlockedJan[is.na(property_info$BlockedJan)] = 0

#### BlockedFeb

listing_2016Feb_block = listing_2016Q1[listing_2016Q1$Month=="02" & listing_2016Q1$Status == 'B',]
agg_block_Feb = aggregate(Status ~ PropertyID, data = listing_2016Feb_block, FUN = length)
colnames(agg_block_Feb) = c('PropertyID','BlockedFeb')
property_info = merge(x = property_info, y = agg_block_Feb, by = 'PropertyID', all.x = TRUE)
property_info$BlockedFeb[is.na(property_info$BlockedFeb)] = 0

#### BlockedMar

listing_2016Mar_block = listing_2016Q1[listing_2016Q1$Month=="03" & listing_2016Q1$Status == 'B',]
agg_block_Mar = aggregate(Status ~ PropertyID, data = listing_2016Mar_block, FUN = length)
colnames(agg_block_Mar) = c('PropertyID','BlockedMar')
property_info = merge(x = property_info, y = agg_block_Mar, by = 'PropertyID', all.x = TRUE)
property_info$BlockedMar[is.na(property_info$BlockedMar)] = 0

#### BlockedApr

listing_2016Apr_block = listing_2016Q2[listing_2016Q2$Month=="04" & listing_2016Q2$Status == 'B',]
agg_block_Apr = aggregate(Status ~ PropertyID, data = listing_2016Apr_block, FUN = length)
colnames(agg_block_Apr) = c('PropertyID','BlockedApr')
property_info = merge(x = property_info, y = agg_block_Apr, by = 'PropertyID', all.x = TRUE)
property_info$BlockedApr[is.na(property_info$BlockedApr)] = 0

#### BlockedMay

listing_2016May_block = listing_2016Q2[listing_2016Q2$Month=="05" & listing_2016Q2$Status == 'B',]
agg_block_May = aggregate(Status ~ PropertyID, data = listing_2016May_block, FUN = length)
colnames(agg_block_May) = c('PropertyID','BlockedMay')
property_info = merge(x = property_info, y = agg_block_May, by = 'PropertyID', all.x = TRUE)
property_info$BlockedMay[is.na(property_info$BlockedMay)] = 0

#### BlockedJun

listing_2016Jun_block = listing_2016Q2[listing_2016Q2$Month=="06" & listing_2016Q2$Status == 'B',]
agg_block_Jun = aggregate(Status ~ PropertyID, data = listing_2016Jun_block, FUN = length)
colnames(agg_block_Jun) = c('PropertyID','BlockedJun')
property_info = merge(x = property_info, y = agg_block_Jun, by = 'PropertyID', all.x = TRUE)
property_info$BlockedJun[is.na(property_info$BlockedJun)] = 0

#### newly generated monthly price
## PriceJan
listing_2016Jan_price = listing_2016Q1[listing_2016Q1$Month=="01",]
agg_price_jan = aggregate(Price ~ PropertyID, data = listing_2016Jan_price, FUN = mean)
colnames(agg_price_jan)[2] = 'ListPriceJan'
property_info = merge(x = property_info, y = agg_price_jan, by = 'PropertyID', all.x = TRUE)
property_info$ListPriceJan[is.na(property_info$ListPriceJan)] = 0

## PriceFeb
listing_2016Feb_price = listing_2016Q1[listing_2016Q1$Month=="02",]
agg_price_feb = aggregate(Price ~ PropertyID, data = listing_2016Feb_price, FUN = mean)
colnames(agg_price_feb)[2] = 'ListPriceFeb'
property_info = merge(x = property_info, y = agg_price_feb, by = 'PropertyID', all.x = TRUE)
property_info$ListPriceFeb[is.na(property_info$ListPriceFeb)] = 0

## PriceMar
listing_2016Mar_price = listing_2016Q1[listing_2016Q1$Month=="03",]
agg_price_mar = aggregate(Price ~ PropertyID, data = listing_2016Mar_price, FUN = mean)
colnames(agg_price_mar)[2] = 'ListPriceMar'
property_info = merge(x = property_info, y = agg_price_mar, by = 'PropertyID', all.x = TRUE)
property_info$ListPriceMar[is.na(property_info$ListPriceMar)] = 0

## PriceApr
listing_2016Apr_price = listing_2016Q2[listing_2016Q2$Month=="04",]
agg_price_apr = aggregate(Price ~ PropertyID, data = listing_2016Apr_price, FUN = mean)
colnames(agg_price_apr)[2] = 'ListPriceApr'
property_info = merge(x = property_info, y = agg_price_apr, by = 'PropertyID', all.x = TRUE)
property_info$ListPriceApr[is.na(property_info$ListPriceApr)] = 0

## PriceMay
listing_2016May_price = listing_2016Q2[listing_2016Q2$Month=="05",]
agg_price_may = aggregate(Price ~ PropertyID, data = listing_2016May_price, FUN = mean)
colnames(agg_price_may)[2] = 'ListPriceMay'
property_info = merge(x = property_info, y = agg_price_may, by = 'PropertyID', all.x = TRUE)
property_info$ListPriceMay[is.na(property_info$ListPriceMay)] = 0

## PriceJun
listing_2016Jun_price = listing_2016Q2[listing_2016Q2$Month=="06",]
agg_price_jun = aggregate(Price ~ PropertyID, data = listing_2016Jun_price, FUN = mean)
colnames(agg_price_jun)[2] = 'ListPriceJun'
property_info = merge(x = property_info, y = agg_price_jun, by = 'PropertyID', all.x = TRUE)
property_info$ListPriceJun[is.na(property_info$ListPriceJun)] = 0

#### Anomalies
tb_neighborhood = table(property_info$Neighborhood)
rare_neighborhood = names(tb_neighborhood)[tb_neighborhood<=20]
property_info$Neighborhood[property_info$Neighborhood %in% rare_neighborhood] = "rare neighborhood"

property_info$Neighborhood[is.na(property_info$Neighborhood)] = "unknown neighborhood"
property_info$Superhost[is.na(property_info$Superhost)] = "unknown host type"

# mean(is.na(property_info$ExtraPeopleFee))

property_info$NumberofReviews[is.na(property_info$NumberofReviews)] = mean(property_info$NumberofReviews, na.rm = TRUE)
property_info$PublishedMonthlyRate[is.na(property_info$PublishedMonthlyRate)] = mean(property_info$PublishedMonthlyRate, na.rm = TRUE)
property_info$PublishedWeeklyRate[is.na(property_info$PublishedWeeklyRate)] = mean(property_info$PublishedWeeklyRate, na.rm = TRUE)
property_info$ResponseRate[is.na(property_info$ResponseRate)] = 0
property_info$Bedrooms[is.na(property_info$Bedrooms)] = mean(property_info$Bedrooms, na.rm = TRUE)
property_info$Bathrooms[is.na(property_info$Bathrooms)] = mean(property_info$Bathrooms, na.rm = TRUE)
property_info$OverallRating[is.na(property_info$OverallRating)] = 0

property_info$NumberofPhotos[is.na(property_info$NumberofPhotos)] = mean(property_info$NumberofPhotos, na.rm = TRUE)

property_info$HostID[is.na(property_info$HostID)] = mean(property_info$HostID, na.rm = TRUE)

property_info$ResponseTimemin[is.na(property_info$ResponseTimemin)] = 0

property_info$CancellationPolicy[is.na(property_info$CancellationPolicy)] = "unknown cancellation policy"

property_info$CleaningFee[is.na(property_info$CleaningFee)] = 0

property_info$SecurityDeposit[is.na(property_info$SecurityDeposit)] = 0

property_info$ExtraPeopleFee[is.na(property_info$ExtraPeopleFee)] = 0

# new Data anomalies
## rare property type
tb_property_type = table(property_info$PropertyType)
rare_property_types = names(tb_property_type)[tb_property_type<=50]
property_info$PropertyType[property_info$PropertyType %in% rare_property_types] = "rare property type"

##################
# histogram
# hist(property_info$SecurityDeposit, xlim = c(0, 6000))

# Outliers
property_info$NumberofReviews[property_info$NumberofReviews>150] = 150
property_info$PublishedMonthlyRate[property_info$PublishedMonthlyRate>20000] = 20000
property_info$PublishedNightlyRate[property_info$PublishedNightlyRate>1000] = 1000
property_info$NumberofPhotos[property_info$NumberofPhotos>60] = 60
property_info$ExtraPeopleFee[property_info$ExtraPeopleFee>80] = 80
property_info$CleaningFee[property_info$CleaningFee>200] = 200
property_info$MinimumStay[property_info$MinimumStay>40] = 40
property_info$ResponseTimemin[property_info$ResponseTimemin>1400] = 1400
property_info$SecurityDeposit[property_info$SecurityDeposit>2000] = 2000

# log
property_info$LogBookingQ1 = log(property_info$BookingQ1)
property_info$LogBookingQ1[property_info$LogBookingQ1 =="-Inf"] = 0

property_info$LogBookingQ2 = log(property_info$BookingQ2)
property_info$LogBookingQ2[property_info$LogBookingQ2 =="-Inf"] = 0

property_info$LogBlockedQ1 = log(property_info$BlockedQ1)
property_info$LogBlockedQ1[property_info$LogBlockedQ1 =="-Inf"] = 0

property_info$LogBlockedQ2 = log(property_info$BlockedQ2)
property_info$LogBlockedQ2[property_info$LogBlockedQ2 =="-Inf"] = 0

property_info$LogBookingJan = log(property_info$BookingJan)
property_info$LogBookingJan[property_info$LogBookingJan =="-Inf"] = 0

property_info$LogBookingFeb = log(property_info$BookingFeb)
property_info$LogBookingFeb[property_info$LogBookingFeb =="-Inf"] = 0

property_info$LogBookingMar = log(property_info$BookingMar)
property_info$LogBookingMar[property_info$LogBookingMar =="-Inf"] = 0

property_info$LogBookingApr = log(property_info$BookingApr)
property_info$LogBookingApr[property_info$LogBookingApr =="-Inf"] = 0

property_info$LogBookingMay = log(property_info$BookingMay)
property_info$LogBookingMay[property_info$LogBookingMay =="-Inf"] = 0

property_info$LogBookingJun = log(property_info$BookingJun)
property_info$LogBookingJun[property_info$LogBookingJun =="-Inf"] = 0

property_info$LogBlockedJan = log(property_info$BlockedJan)
property_info$LogBlockedJan[property_info$LogBlockedJan =="-Inf"] = 0

property_info$LogBlockedFeb = log(property_info$BlockedFeb)
property_info$LogBlockedFeb[property_info$LogBlockedFeb =="-Inf"] = 0

property_info$LogBlockedMar = log(property_info$BlockedMar)
property_info$LogBlockedMar[property_info$LogBlockedMar =="-Inf"] = 0

property_info$LogBlockedApr = log(property_info$BlockedApr)
property_info$LogBlockedApr[property_info$LogBlockedApr =="-Inf"] = 0

property_info$LogBlockedMay = log(property_info$BlockedMay)
property_info$LogBlockedMay[property_info$LogBlockedMay =="-Inf"] = 0

property_info$LogBlockedJun = log(property_info$BlockedJun)
property_info$LogBlockedJun[property_info$LogBlockedJun =="-Inf"] = 0

property_info$LogPNR = log(property_info$PublishedNightlyRate)
property_info$LogPNR[property_info$LogPNR =="-Inf"] = 0

property_info$LogPMR = log(property_info$PublishedMonthlyRate)
property_info$LogPMR[property_info$LogPMR =="-Inf"] = 0

property_info$LogPWR = log(property_info$PublishedWeeklyRate)
property_info$LogPWR[property_info$LogPWR =="-Inf"] = 0

property_info$LogLPQ1 = log(property_info$ListPriceQ1)
property_info$LogLPQ1[property_info$LogLPQ1 =="-Inf"] = 0

property_info$LogLPQ2 = log(property_info$ListPriceQ2)
property_info$LogLPQ2[property_info$LogLPQ2 =="-Inf"] = 0

property_info$LogLPJan = log(property_info$ListPriceJan)
property_info$LogLPJan[property_info$LogLPJan =="-Inf"] = 0

property_info$LogLPFeb = log(property_info$ListPriceFeb)
property_info$LogLPFeb[property_info$LogLPFeb =="-Inf"] = 0

property_info$LogLPMar = log(property_info$ListPriceMar)
property_info$LogLPMar[property_info$LogLPMar =="-Inf"] = 0

property_info$LogLPApr = log(property_info$ListPriceApr)
property_info$LogLPApr[property_info$LogLPApr =="-Inf"] = 0

property_info$LogLPMay = log(property_info$ListPriceMay)
property_info$LogLPMay[property_info$LogLPMay =="-Inf"] = 0

property_info$LogLPJun = log(property_info$ListPriceJun)
property_info$LogLPJun[property_info$LogLPJun =="-Inf"] = 0

property_info$Logcf = log(property_info$CleaningFee)
property_info$Logcf[property_info$Logcf =="-Inf"] = 0

property_info$Logcf = log(property_info$CleaningFee)
property_info$Logcf[property_info$Logcf =="-Inf"] = 0

# Data set
property_info_test = property_info[property_info$PropertyID %in% PropertyID_test, ]
property_info_train = property_info[!property_info$PropertyID %in% PropertyID_test, ]
property_info_train = merge(x = property_info_train, y = reserve_2016Q3_train, by = 'PropertyID', all.x = TRUE)
property_info_train = merge(x = property_info_train, y = blocked_2016Q3_train, by = 'PropertyID', all.x = TRUE)
property_info_train = merge(x = property_info_train, y = price_2016Q3_train, by = 'PropertyID', all.x = TRUE)

# booking regression
reg_bookingQ3 = lm(formula = NumReserveDays2016Q3 ~ ResponseRate +  NumberofReviews + NumberofPhotos + CreatedDate + ExtraPeopleFee +  
                     PublishedMonthlyRate + PublishedNightlyRate + PropertyType + BusinessReady + Latitude + Longitude + CleaningFee +
                     ResponseTimemin + Superhost + LogPNR + LogPWR + 
                     BookingQ2 + BookingMar + BookingApr + BookingJun + BlockedQ1 + BlockedMay + BlockedQ2 +  
                     LogBookingQ2 + LogBookingApr + LogBookingMay + LogBookingJun + LogBlockedJan + LogBlockedMar +  
                     ListPriceJan + ListPriceFeb +  ListPriceMar + ListPriceApr + ListPriceJun + LogLPQ1 + LogLPQ2 + LogLPFeb + LogLPApr, 
                   data = property_info_train)

# blocked regression
reg_blockedQ3 = lm(formula = NumBlockedDays2016Q3 ~ Superhost + OverallRating + NumberofReviews + ListingType + PropertyID + SecurityDeposit +
                     ResponseRate + Bathrooms + PropertyType + NumberofPhotos + HostID + InstantbookEnabled + Longitude + CreatedDate +
                     ResponseTimemin + ExtraPeopleFee + CleaningFee + LogPWR +
                     BookingApr + BookingJun + LogBookingQ1 +  LogBookingQ2 + LogBookingApr + LogBookingMay +
                     BlockedJan + BlockedApr + BlockedMay + BlockedQ1 + BlockedQ2 + LogLPQ1 + LogLPQ2 + LogLPMar + LogLPApr + LogLPJun +
                     ListPriceQ2 + ListPriceApr + ListPriceMay + ListPriceJun + 
                     LogBlockedQ1 + LogBlockedQ2 + LogBlockedJan + LogBlockedFeb + LogBlockedMar + LogBlockedApr + LogBlockedMay, 
                   data = property_info_train)

# price regression
reg_priceQ3 = lm(formula = Price2016Q3 ~ ResponseRate + NumberofPhotos + PublishedNightlyRate + LogPNR + Logcf + InstantbookEnabled + Latitude + 
                   BlockedMar + ListPriceJun + LogBookingMar + LogLPMar, data = property_info_train)

# save
save(property_info_train, property_info_test, file = 'C:/Users/dongw/OneDrive - Indiana University/Desktop/K353-project2/k353-airbnb-project/data-cleansing.rdata')


