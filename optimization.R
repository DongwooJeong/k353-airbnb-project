load('C:/Users/dongw/OneDrive - Indiana University/Desktop/K353-project2/k353-airbnb-project/data-cleansing.rdata')

# prediction
pred_bookingQ3 = predict(object = reg_bookingQ3, newdata = property_info_test)
pred_blockedQ3 = predict(object = reg_blockedQ3, newdata = property_info_test)
pred_priceQ3 = predict(object = reg_priceQ3, newdata = property_info_test)

# pred anomalies
pred_bookingQ3[is.na(pred_bookingQ3)]=0
pred_blockedQ3[is.na(pred_blockedQ3)]=0
pred_priceQ3[is.na(pred_priceQ3)]=0

pred_bookingQ3[pred_bookingQ3<0] = 0
pred_blockedQ3[pred_blockedQ3<0] = 0
pred_priceQ3[pred_priceQ3<0] = 0

pred_bookingQ3[pred_bookingQ3>92] = 92
pred_blockedQ3[pred_blockedQ3>92] = 92

pred_bookingQ3[pred_bookingQ3>92-pred_blockedQ3] = (92 - pred_blockedQ3)[pred_bookingQ3>92-pred_blockedQ3]

checkmin = c(min(pred_bookingQ3),min(pred_blockedQ3),min(pred_priceQ3))
checkmax = c(max(pred_bookingQ3),max(pred_blockedQ3))
checksum = max(pred_bookingQ3+pred_blockedQ3)

# Incentive optimization
source("C:/Users/dongw/OneDrive - Indiana University/Desktop/K353-project2/k353-airbnb-project/profit function share.R")

incentive = rep(NA, nrow(property_info_test))

pb = txtProgressBar(style = 3)
for (i in 1:length(incentive)) {
  setTxtProgressBar(pb = pb, value = i / length(incentive))
  opt_obj_i = optim(par = 0, fn = profit_fun, lower = 0, upper = Inf, method = 'L-BFGS-B', control = list(fnscale = -1), blockedQ3 = pred_blockedQ3[i], bookedQ3 = pred_bookingQ3[i], priceQ3 = pred_priceQ3[i])
  incentive[i] = opt_obj_i$par
}
close(pb)



save(incentive, file = 'C:/Users/dongw/OneDrive - Indiana University/Desktop/K353-project2/k353-airbnb-project/Airbob.rdata')







