load('C:/Users/dongw/OneDrive - Indiana University/Desktop/K353-project2/data-cleansing.rdata')

# property_info_train$PropertyType[is.na(property_info_train$PropertyType)] = 0
# Price
n_round = 20
set.seed(0)
property_info_train$CVLabel = sample(x = 1:n_round, size = nrow(property_info_train), replace = TRUE)
cv_mse_vec_p = rep(NA, n_round)
for (idx in 1:n_round) {
  lr_model_p = lm(formula = Price2016Q3 ~ NumberofReviews + ResponseRate + Bathrooms + PublishedMonthlyRate + PublishedNightlyRate + PublishedWeeklyRate +
                    BlockedQ1 + BlockedQ2 + BookingJan + BookingFeb + BlockedMar + BookingJun + ListingType + PropertyType +
                    ListPriceJan + ListPriceJun +
                    LogBookingMar + LogBookingQ2 + LogPNR + LogPMR, 
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
  lr_model_r = lm(formula = NumReserveDays2016Q3 ~ ResponseRate + OverallRating + NumberofReviews + LogPNR + LogPMR + LogPWR +
                  PublishedMonthlyRate + PublishedNightlyRate + PublishedWeeklyRate + ListingType + PropertyType +
                  BookingQ2 + BlockedQ1 + BlockedQ2 + BookingJun + BlockedMar + BlockedMay + BlockedJun + 
                  BookingMar + BookingApr + BlockedJan +
                  LogBookingQ2 + LogBlockedQ2 + LogBookingApr + LogBookingJun + LogBlockedJan +
                  ListPriceJan + ListPriceMar + ListPriceJun, 
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
  lr_model_b = lm(formula = NumBlockedDays2016Q3 ~ Neighborhood + Superhost + OverallRating + NumberofReviews + ListingType +
                    ResponseRate + Bedrooms + Bathrooms + PropertyType + PublishedMonthlyRate + PublishedNightlyRate + 
                    BookingJan + BookingApr + BookingMay + BookingJun + BlockedJan + BlockedFeb + 
                    BlockedApr + BlockedMay + BlockedQ1 + BlockedQ2 + ListPriceQ2 +
                    ListPriceApr + ListPriceJun + LogBookingApr + LogBookingMay +
                    LogBookingQ2 + LogBlockedQ1 + LogBlockedQ2 + LogPNR + LogPMR + LogPWR, 
                data = property_info_train[property_info_train$CVLabel != idx, ])
  lr_pred_b = predict(object = lr_model_b, newdata = property_info_train[property_info_train$CVLabel==idx,])
  cv_mse_vec_b[idx] = mean((lr_pred_b - property_info_train$NumBlockedDays2016Q3[property_info_train$CVLabel==idx])^2)
}
print(sqrt(mean(cv_mse_vec_b)))
