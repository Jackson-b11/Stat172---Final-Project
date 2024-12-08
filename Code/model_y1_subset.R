rm(list=ls())

library(tidyverse)
library(pROC)
library(glmnet)
library(lubridate)
library(sf)
library(tmap)
library(tigris)

source("Code/clean_cps.R")
source("Code/clean_acs.R")

# "FSWROUTY" = Worried that food would run out before able to afford more during past yea

#trying subset training data with only elderly observations who live alone
cps_data = cps_data[cps_data$elderly == cps_data$hhsize & cps_data$elderly ==1, ]

f = cps_data %>% 
  select(-c(CPSID,COUNTY,FSTOTXPNC_perpers,FSSTATUS,FSSTATUSMD,FSFOODS,FSBAL,
            FSRAWSCRA,FSTOTXPNC  ))

#dropping FSWROUTY na's
df <- f[!is.na(f$FSWROUTY), ]

train.idx = sample(x=1:nrow(df), size=.7*nrow(df))
train.df = df[train.idx,]
test.df = df[-train.idx,]


x.train = model.matrix(FSWROUTY ~ hhsize + female + hispanic + black + kids +elderly + education + married + livalone   , data =train.df %>% select(-weight))[,-1]
x.test = model.matrix(FSWROUTY ~  hhsize + female + hispanic + black + kids +elderly + education + married + livalone  , data =test.df  %>% select(-weight))[,-1]


y.train = as.vector(train.df$FSWROUTY)
y.test = as.vector(test.df$FSWROUTY)


#creating cv to find best lambda
lr_lasso_cv = cv.glmnet(x.train,
                        y.train, 
                        family = binomial(link = "logit"), 
                        alpha = 1,
                        weights = as.integer(train.df$weight))

lr_ridge_cv = cv.glmnet(x.train, #this is x matrix
                        y.train, # y vector
                        family = binomial(link = "logit"),
                        alpha = 0,# 0 for ridge regression
                        weights = as.integer(train.df$weight)) 


#extracting best lambda from cv.
best_lasso_lambda = lr_lasso_cv$lambda.min
best_ridge_lambda = lr_ridge_cv$lambda.min

#fitting final models using best lambda
final_lasso = glmnet(x.train,
                     y.train,
                     family = binomial(link = "logit"),
                     alpha = 1, # 1 for lasso
                     weights = as.integer(train.df$weight),
                     lambda = best_lasso_lambda)

final_ridge = glmnet(x.train,
                     y.train,
                     family = binomial(link = "logit"),
                     alpha = 0, # 1 for ridge
                     weights = as.integer(train.df$weight),
                     lambda = best_ridge_lambda)


#creating df with models predictions
test.df.preds = test.df %>% 
  mutate(
    lasso_pred = predict(final_lasso, x.test, type = "response")[,1],
    ridge_pred = predict(final_ridge, x.test, type = "response")[,1]
  )


#creating rocCurve
lasso_rocCurve = roc(response = as.factor(test.df.preds$FSWROUTY),
                     predictor = test.df.preds$lasso_pred, #predicted probs
                     levels = c("0","1"))

ridge_rocCurve = roc(response = as.factor(test.df.preds$FSWROUTY),
                     predictor = test.df.preds$ridge_pred, #predicted probs
                     levels = c("0","1"))



#Plotting ROC Curve to find sens and spec for threshold BEN
lasso = plot(lasso_rocCurve,print.thres = T, print.auc = T)
ridge = plot(ridge_rocCurve,print.thres = T, print.auc = T)


####################Predicting off ACS data######################


x.test.acs = model.matrix(PUMA ~ ., data = acs_data  %>% select(-c(serialno,weight)))[,-1] 

acs.preds = acs_data %>% 
  mutate(
    lasso_acs = predict(final_lasso, x.test.acs, type = "response")[,1],
    ridge_acs = predict(final_ridge, x.test.acs, type = "response")[,1]
  )



iowa_pumas <- pumas(state = "Iowa", cb = TRUE,year = 2020)

# Calculate weighted mean grouped by PUMA
acs_preds_ag <- acs.preds %>%
  group_by(PUMA) %>%
  summarize(weighted_mean = weighted.mean(x = lasso_acs, w = weight, na.rm = TRUE))

iowa_map_data <- iowa_pumas %>%
  left_join(acs_preds_ag, by = c("GEOID20" = "PUMA"))

ggplot(data = iowa_map_data) +
  geom_sf(aes(fill = weighted_mean), color = "white", size = 0.2) +  # Plot with fill by `value`
  scale_fill_distiller(
    palette = "Blues",
    direction = 1, # Light to dark
    na.value = "grey", 
    name = "Food Insecure\n Seniors"
  ) +
  theme_minimal() +
  labs(
    title = "Choropleth Map of Iowa PUMAs",
    subtitle = "Visualization of Provided Data",
    caption = "Source: Provided PUMA Data"
  )


# only obs where it's elderly
acs_preds_ag <- acs.preds %>%
  filter(elderly >= 1) %>% 
  group_by(PUMA) %>%
  summarize(weighted_mean = weighted.mean(x = lasso_acs, w = weight, na.rm = TRUE))

iowa_map_data <- iowa_pumas %>%
  left_join(acs_preds_ag, by = c("GEOID20" = "PUMA"))

ggplot(data = iowa_map_data) +
  geom_sf(aes(fill = weighted_mean), color = "white", size = 0.2) +  # Plot with fill by `value`
  scale_fill_distiller(
    palette = "Blues",
    direction = 1, # Light to dark
    na.value = "grey", 
    name = "Aggregated\n Probability"
  ) +
  theme_minimal() +
  labs(
    title = "Choropleth Map of Iowa PUMAs",
   
  )



####graphing total number of seniors 
puma_eld = read.csv("Data/iowa_seniors_by_puma.csv")

acs_preds_ag$PUMA = as.integer(as.character(acs_preds_ag$PUMA))
puma_eld$GEOID = as.integer(puma_eld$GEOID)

iowa_map_data_elderly <- puma_eld %>%
  left_join(acs_preds_ag, by = c("GEOID" = "PUMA"))

iowa_map_data$GEOID20 = as.integer(iowa_map_data$GEOID20)

iowa_map_data_elderly <- puma_eld %>%
  left_join(iowa_map_data, by = c("GEOID" = "GEOID20"))




ggplot(data = iowa_map_data_elderly) +
  geom_sf(aes(geometry = geometry, fill = weighted_mean * senior_population), 
          color = "white", size = 0.2) + 
  scale_fill_distiller(
    palette = "Blues",
    direction = 1, # Light to dark
    na.value = "grey", 
    name = "Food Insecure\n     Seniors"
  ) +
  theme_minimal() +
  labs(
    title = "Choropleth Map of Iowa PUMAs",
  )



#### Notes from Follet ####
# can explain auc sens, spec, on how we chose our model talk abotu sens in language
# use variable importance plot to find find variable make plot between x and y when interpretting
# filled histogram or barchart
# talk about coefficients of pumas: This puma has less education etc.
# variable coefficient interpretations: coefficient interpretation with plot
# variable on X and filled bars 
########### End of Notes from Follet #####################################


# Find best model based off auc,sens,and spec. Interpret this in human language - Ben/Nathn

# Use variable importance plot to find most important variable. - Ben
# Create a filled histogram/barchart of x and y variables - Ben
# Create interpretations of most important variables - Ben
# coef(final_lasso)

# put code into PT and ask for extra process info to add to READ.ME - Trevor

# create graph of x variables by pumas - Nathan

# Create cleropleth of elder*prob --> this dataframe iowa_map_data_elderly -Jackson

