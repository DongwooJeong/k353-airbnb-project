
# Formula to compute profit for a given property 
# profit = 0.1 * blockedQ3 * (1 - exp(-0.12 * incentive)) * (bookedQ3 / (92 - blockedQ3)) * priceQ3 - incentive

# Define function profit_fun to compute property's Q3 profit
# Arguments: 
#   incentive: incentive amount offered to the host
#   blockedQ3: number of booked days in Q3 when incentive is 0
#   bookedQ3:  number of blocked days in Q3 when incentive is 0
#   priceQ3:   average listing price in Q3 when incentive is 0

profit_fun = function(incentive, blockedQ3, bookedQ3, priceQ3) {
  profit = 0.1 * blockedQ3 * (1 - exp(-0.12 * incentive)) * (bookedQ3 / (92.001 - blockedQ3)) * priceQ3 - incentive
  return(profit)
}

# Questions
#   Given a property whose blocked days, booked days, and average price in Q3 are 
#   59 days, 12 days, and 144 dollars. 
profit_fun(incentive = c(1,200,45), blockedQ3 = 59, bookedQ3 = 12, priceQ3 = 144)
# Q1: What is the profit improvement from the property if an incentive of 1 dollar is offered? 
33.93436

# Q2: What is the profit from the property if an incentive of 200 dollars is offered?
108.9361

# Q3: What is the profit from the property if an incentive of 45 dollars is offered?
262.5408

# What do you find? 



