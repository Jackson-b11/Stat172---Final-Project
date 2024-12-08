
#trying subset training data with only elderly observations who live alone
cps_data = cps_data[cps_data$elderly == cps_data$hhsize & cps_data$elderly ==1, ]

