load('C:/Users/dongw/OneDrive - Indiana University/Desktop/K353-project2/k353-airbnb-project/data-cleansing.rdata')

# Price
n_round = 20
set.seed(0)
property_info_train$CVLabel = sample(x = 1:n_round, size = nrow(property_info_train), replace = TRUE)
cv_mse_vec_p = rep(NA, n_round)
for (idx in 1:n_round) {
  lr_model_p = lm(formula = Price2016Q3 ~ NumberofReviews + ResponseRate + Bathrooms + ListingType + PropertyType + NumberofPhotos + BusinessReady +
                    PublishedMonthlyRate + PublishedNightlyRate + PublishedWeeklyRate + LogPNR + LogPMR + MaxGuests + CleaningFee + MinimumStay +
                    BlockedQ1 + BlockedQ2 + BookingJan + BookingFeb + BlockedMar + BookingJun + ListPriceJan + ListPriceJun + SecurityDeposit + Latitude + Longitude +
                    LogBookingMar + LogBookingQ2 + LogLPMar + LogLPApr, 
                  data = property_info_train[property_info_train$CVLabel != idx, ])
  lr_pred_p = predict(object = lr_model_p, newdata = property_info_train[property_info_train$CVLabel==idx,])
  cv_mse_vec_p[idx] = mean((lr_pred_p - property_info_train$Price2016Q3[property_info_train$CVLabel==idx])^2)
}
print(sqrt(mean(cv_mse_vec_p)))

# Booking
n_round = 20
set.seed(0)
property_info_train$CVLabel = sample(x = 1:n_round, size = nrow(property_info_train), replace = TRUE)
cv_mse_vec_r = rep(NA, n_round)
for (idx in 1:n_round) {
  lr_model_r = lm(formula = NumReserveDays2016Q3 ~ ResponseRate + OverallRating + NumberofReviews + LogPNR + LogPMR + LogPWR + NumberofPhotos +
                    PublishedMonthlyRate + PublishedNightlyRate + PublishedWeeklyRate + ListingType + PropertyType + PropertyID + HostID + BusinessReady +
                    BookingQ2 + BlockedQ1 + BlockedQ2 + BookingJun + BlockedMar + BlockedMay + BlockedJun + CreatedDate +  MaxGuests + ExtraPeopleFee + 
                    BookingMar + BookingApr + BlockedJan + CleaningFee + InstantbookEnabled + Latitude + Longitude + 
                    LogBookingQ2 + LogBlockedQ2 + LogBookingApr + LogBookingJun + LogBlockedJan +
                    ListPriceJan + ListPriceMar + ListPriceJun + LogLPJan + LogLPFeb + LogLPMar + LogLPApr + LogLPMay + LogLPJun, 
                data = property_info_train[property_info_train$CVLabel != idx, ])
  lr_pred_r = predict(object = lr_model_r, newdata = property_info_train[property_info_train$CVLabel==idx,])
  cv_mse_vec_r[idx] = mean((lr_pred_r - property_info_train$NumReserveDays2016Q3[property_info_train$CVLabel==idx])^2)
}
print(sqrt(mean(cv_mse_vec_r)))

# Blocked
n_round = 20
set.seed(0)
property_info_train$CVLabel = sample(x = 1:n_round, size = nrow(property_info_train), replace = TRUE)
cv_mse_vec_b = rep(NA, n_round)
for (idx in 1:n_round) {
  lr_model_b = lm(formula = NumBlockedDays2016Q3 ~ Neighborhood + Superhost + OverallRating + NumberofReviews + ListingType + PropertyID +
                    ResponseRate + Bedrooms + Bathrooms + PropertyType + PublishedMonthlyRate + PublishedNightlyRate + NumberofPhotos + HostID +
                    BookingJan + BookingApr + BookingMay + BookingJun + BlockedJan + BlockedFeb + CreatedDate + ResponseTimemin + ExtraPeopleFee +
                    BlockedApr + BlockedMay + BlockedQ1 + BlockedQ2 + ListPriceQ2 + CleaningFee + MinimumStay + BusinessReady + 
                    ListPriceApr + ListPriceJun + LogBookingApr + LogBookingMay +
                    LogBookingQ2 + LogBlockedQ1 + LogBlockedQ2 + LogPNR + LogPMR + LogPWR + LogLPJan + LogLPJun, 
                data = property_info_train[property_info_train$CVLabel != idx, ])
  lr_pred_b = predict(object = lr_model_b, newdata = property_info_train[property_info_train$CVLabel==idx,])
  cv_mse_vec_b[idx] = mean((lr_pred_b - property_info_train$NumBlockedDays2016Q3[property_info_train$CVLabel==idx])^2)
}
print(sqrt(mean(cv_mse_vec_b)))
