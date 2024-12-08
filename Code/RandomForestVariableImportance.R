rm(list=ls())
library(randomForest)
library(logistf)
library(glmnet)
library(knitr)
library(tidyverse)
library(RColorBrewer)
library(pROC)

cps = read.csv("Data/cps_00006.csv")

cps <- cps %>% 
  
  mutate(SEX = SEX - 1 , # Create dummy variables 
         
         CHILD = ifelse(AGE < 18, 1, 0), 
         
         ELDERLY = ifelse(AGE > 59, 1, 0), #NOTE DEFINITION QQ: check if 60? 
         
         BLACK = ifelse(RACE==200, 1, 0), 
         
         HISPANIC = ifelse(HISPAN>0, 1, 0), 
         
         EDUC = as.integer(EDUC %in% c(91,92,111,123,124,125)), 
         
         EMP = as.integer(EMPSTAT %in% c(1,10,12)), 
         
         MARRIED = as.integer(MARST %in% c(1,2)), 
         
         DIFF = ifelse(DIFFANY==2, 1, 0), 
         
         COUNTY = as.factor(COUNTY)) 




cps_data <- cps %>%
  group_by(CPSID = as.factor(CPSID)) %>% 
  summarise(COUNTY = first(COUNTY), 
            
            #family level weight 
            
            weight = first(HWTFINL), 
            
            #household size 
            
            hhsize = n(), 
            
            #Y variables - i.e., measures of hunger 
            
            #see CPS website for details 
            
            #FSSTATUS, etc. is the same for each member -just take first value for each family 
            
            FSTOTXPNC_perpers = FSTOTXPNC/hhsize, # In per person terms 
            
            FSSTATUS = first(FSSTATUS), 
            
            FSSTATUSMD = first(FSSTATUSMD), 
            
            FSFOODS = first(FSFOODS), 
            
            FSWROUTY = first(FSWROUTY), 
            
            FSBAL = first(FSBAL), 
            
            FSRAWSCRA = first(FSRAWSCRA), 
            
            FSTOTXPNC = first(FSTOTXPNC), 
            
            FSSTATUS = first(FSSTATUS), 
            
            #count of family members in various categories 
            Female = sum(SEX), 
            Hispanic = sum(HISPANIC), 
            Black= sum(BLACK), 
            Kids= sum(CHILD), 
            Elderly= sum(ELDERLY), 
            Education= sum(EDUC), 
            Married= sum(MARRIED),
            Live_Alone = ifelse(hhsize ==1,1,0)
  ) %>% ungroup() 







cps_data <- cps_data %>% 
  
  mutate(FSSTATUS = ifelse(FSSTATUS %in% c(98,99), NA, FSSTATUS), 
         
         FSSTATUSMD = ifelse(FSSTATUSMD %in% c(98,99), NA, FSSTATUSMD), 
         
         FSFOODS = ifelse(FSFOODS %in% c(98,99), NA, FSFOODS), 
         
         FSWROUTY = ifelse(FSWROUTY %in% c(96,97,98,99), NA, FSWROUTY), 
         
         FSBAL = ifelse(FSBAL %in% c(96,97,98,99), NA, FSBAL), 
         
         FSRAWSCRA = ifelse(FSRAWSCRA %in% c(98,99), NA, FSRAWSCRA),#raw score 
         
         FSTOTXPNC = ifelse(FSTOTXPNC %in% c(999), NA, FSTOTXPNC)) %>% 
  
  mutate(FSSTATUS = ifelse(FSSTATUS > 1, 1, 0), 
         
         FSSTATUSMD = ifelse(FSSTATUSMD > 1, 1, 0), 
         
         FSFOODS = ifelse(FSFOODS > 1, 1, 0), 
         
         FSWROUTY = ifelse(FSWROUTY > 1, 1, 0),#more missings 
         
         FSBAL = ifelse(FSBAL > 1, 1, 0), 
         
         FSRAWSCRA=ifelse(FSRAWSCRA > 1, 1, 0)) 





# "FSWROUTY" = Could not afford to eat balanced meals in the past year 
cps_data = cps_data[cps_data$Elderly == cps_data$hhsize & cps_data$Elderly ==1, ]



f = cps_data %>%  
  
  select(-c(CPSID,COUNTY,FSTOTXPNC_perpers,FSSTATUS,FSSTATUSMD,FSFOODS,FSBAL, 
            
            FSRAWSCRA,FSTOTXPNC,weight  )) 



#dropping FSWROUTY na's 

df <- f[!is.na(f$FSWROUTY), ] 

df$FSWROUTY = as.factor(df$FSWROUTY)

train.idx = sample(x=1:nrow(df), size=.7*nrow(df)) 

train.df = df[train.idx,] 

test.df = df[-train.idx,] 



#what mtry values to consider
mtry = c(1:10) #to slim down number of tuning options

keeps = data.frame(m=rep(NA, length(mtry)),
                   OOB_err_rate = rep(NA, length(mtry)))



for (idx in 1:length(mtry)){
  print(paste0("Trying m = ", mtry[idx]))
  
  tempforest = randomForest(FSWROUTY ~.,
                            data = train.df,
                            ntree=1000,
                            mtry=mtry[idx])
  print(mean(predict(tempforest)!=train.df$FSWROUTY))
  
  #record iteration's m value in idx'th row
  keeps[idx, "m"] = mtry[idx]
  #record oob error in idx'th row
  keeps[idx, "OOB_err_rate"] = mean(predict(tempforest)!=train.df$FSWROUTY)
}


ggplot(data = keeps)+
  geom_line(aes(x=m, y = OOB_err_rate))+
  theme_bw()+labs(x="m(mtry) value", y = "OOB Error rate (minimize)")+scale_x_continuous(breaks = c(1:26))


finalforest = randomForest(FSWROUTY ~.,
                           data=train.df,
                           ntree=1000,
                           mtry=4,
                           importance =TRUE)

#validate model as a predictive tool
pi_hat = predict(finalforest, test.df, type = "prob")[, 1] #Choose positive event colum


rocCurve = roc(response = test.df$FSWROUTY,
               predictor = pi_hat,
               levels = c("0", "1")) #make sure this lines up with your chosen positive event

plot(rocCurve, print.thres = TRUE, print.auc = TRUE)




#Creating a plot for variable importance plot
varImpPlot(finalforest, type = 1)

vi <- as.data.frame(varImpPlot(finalforest, type = 1))
vi$Variable <-rownames(vi)


ggplot(data = vi, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = brewer.pal(9, "Blues")[6], color = "black", width = 0.7) +
  coord_flip() +
  labs(
    x = "Variable Name",
    y = "Mean Decrease in Accuracy",
    title = "Feature Importance (Subset)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank()
  )

