load('C:/Users/dongw/OneDrive - Indiana University/Desktop/K353-project2/k353-airbnb-project/data-cleansing.rdata')

# Price
n_round = 20
set.seed(0)
property_info_train$CVLabel = sample(x = 1:n_round, size = nrow(property_info_train), replace = TRUE)
cv_mse_vec_p = rep(NA, n_round)
for (idx in 1:n_round) {
  lr_model_p = lm(formula = Price2016Q3 ~ ResponseRate + NumberofPhotos + PublishedNightlyRate + LogPNR + Logcf + InstantbookEnabled + Latitude + 
                    BlockedMar + ListPriceJun + LogBookingMar + LogLPMar, 
                  data = property_info_train[property_info_train$CVLabel != idx, ])
  lr_pred_p = predict(object = lr_model_p, newdata = property_info_train[property_info_train$CVLabel==idx,])
  cv_mse_vec_p[idx] = mean(abs(lr_pred_p - property_info_train$Price2016Q3[property_info_train$CVLabel==idx]))
}
print(mean(cv_mse_vec_p))

# Booking
n_round = 20
set.seed(0)
property_info_train$CVLabel = sample(x = 1:n_round, size = nrow(property_info_train), replace = TRUE)
cv_mse_vec_r = rep(NA, n_round)
for (idx in 1:n_round) {
  lr_model_r = lm(formula = NumReserveDays2016Q3 ~ ResponseRate +  NumberofReviews + NumberofPhotos + CreatedDate + ExtraPeopleFee +  
                    PublishedMonthlyRate + PublishedNightlyRate + PropertyType + BusinessReady + Latitude + Longitude + CleaningFee +
                    ResponseTimemin + Superhost + LogPNR + LogPWR + 
                    BookingQ2 + BookingMar + BookingApr + BookingJun + BlockedQ1 + BlockedMay + BlockedQ2 +  
                    LogBookingQ2 + LogBookingApr + LogBookingMay + LogBookingJun + LogBlockedJan + LogBlockedMar +  
                    ListPriceJan + ListPriceFeb +  ListPriceMar + ListPriceApr + ListPriceJun + LogLPQ1 + LogLPQ2 + LogLPFeb + LogLPApr, 
                  data = property_info_train[property_info_train$CVLabel != idx, ])
  lr_pred_r = predict(object = lr_model_r, newdata = property_info_train[property_info_train$CVLabel==idx,])
  cv_mse_vec_r[idx] = mean(abs(lr_pred_r - property_info_train$NumReserveDays2016Q3[property_info_train$CVLabel==idx]))
}
print(mean(cv_mse_vec_r))

# Blocked
n_round = 20
set.seed(0)
property_info_train$CVLabel = sample(x = 1:n_round, size = nrow(property_info_train), replace = TRUE)
cv_mse_vec_b = rep(NA, n_round)
for (idx in 1:n_round) {
  lr_model_b = lm(formula = NumBlockedDays2016Q3 ~ Superhost + OverallRating + NumberofReviews + ListingType + PropertyID + SecurityDeposit +
                    ResponseRate + Bathrooms + PropertyType + NumberofPhotos + HostID + InstantbookEnabled + Longitude +
                    CreatedDate + ResponseTimemin + ExtraPeopleFee + CleaningFee + LogPWR +
                    BookingApr + BookingJun + LogBookingQ1 +  LogBookingQ2 + LogBookingApr + LogBookingMay +
                    BlockedJan + BlockedApr + BlockedMay + BlockedQ1 + BlockedQ2 + LogLPQ1 + LogLPQ2 + LogLPMar + LogLPApr + LogLPJun +
                    ListPriceQ2 + ListPriceApr + ListPriceMay + ListPriceJun + 
                    LogBlockedQ1 + LogBlockedQ2 + LogBlockedJan + LogBlockedFeb + LogBlockedMar + LogBlockedApr + LogBlockedMay, 
                data = property_info_train[property_info_train$CVLabel != idx, ])
  lr_pred_b = predict(object = lr_model_b, newdata = property_info_train[property_info_train$CVLabel==idx,])
  cv_mse_vec_b[idx] = mean(abs(lr_pred_b - property_info_train$NumBlockedDays2016Q3[property_info_train$CVLabel==idx]))
}
print(mean(cv_mse_vec_b))
