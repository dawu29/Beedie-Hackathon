library("tidyverse")
library("car")
library("rpart")
library("rpart.plot")
library("nnet")
library("randomForest")
library("effects")
library("data.table")
library("corrplot") 

source('C:/Users/Dawu/Desktop/BADM Hackathon/BCA_functions_source_file.R')



#Data Wrangling
#Merge 3 data sets together
cust  <- read_csv("CUST_DATASET_ST.CSV")
mg <- read_csv("MG_DATASET_ST.CSV")
fsa    <- read_csv("FSA_DATASET_ST.CSV")
# Roll each customer into a mortgage level data set
# Cumulative unique per unique mg
# Customern and mg_acc is many to one relationship
cust <- cust %>% mutate(number = sapply(1:length(mg_acc),function(i)sum(mg_acc[i]==mg_acc[1:i])))

uniq_mort <- dcast(setDT(cust), mg_acc~number, value.var=c('cust_age', 'cust_gender','cust_income','cust_cr_score'))

# Create full accounts for each mortgage
mort_full <- mg %>% 
  merge(uniq_mort,by = "mg_acc") %>% 
  merge(fsa, by = "FSA") 

#Check for missing values
na <- variable.summary(mort_full)

#Transforming data types
mort_full$default.f<-as.factor(mort_full$default)
mort_full$Sample<-as.factor(mort_full$Sample)
mort_full$FSA<-as.factor(mort_full$FSA)
mort_full$cust_gender_1<-as.factor(mort_full$cust_gender_1)
mort_full$cust_gender_2<-as.factor(mort_full$cust_gender_2)
mort_full$cust_gender_3<-as.factor(mort_full$cust_gender_3)
mort_full$cust_gender_4<-as.factor(mort_full$cust_gender_4)
mort_full$amort_period<-as.factor(mort_full$amort_period)
mort_full$property_type<-as.factor(mort_full$property_type)

today <- as.Date('2021/10/10')

#Creating Variables
#Time Left
mort_full <- mort_full %>% mutate(amort_len_days = ifelse(amort_period == "20 years",20,ifelse(amort_period =="25 years",25,30))*365)

mort_full$amort_left_days <- as.numeric(mort_full$amort_len_days - (today - mort_full$origin_date))

#Max credit score between all owners
mort_full$max_cr_score <- pmax(mort_full$cust_cr_score_1,mort_full$cust_cr_score_2,mort_full$cust_cr_score_3,mort_full$cust_cr_score_4,na.rm = TRUE)

#Determine total household income
mort_full$hhold_income <- rowSums(mort_full[ , c(20,23)], na.rm=TRUE)

#Minimum age
mort_full$min_age <- pmin(mort_full$cust_age_1,mort_full$cust_age_2,mort_full$cust_age_3,mort_full$cust_age_4,na.rm = TRUE)
#Number of owners
num_owner <- cust %>% 
  group_by(mg_acc) %>% 
  summarise(num_owner = max(number))

mort_clean <- mort_full %>% 
  merge(num_owner, by ="mg_acc") 

#Interactions
mort_clean$incomexsize <- mort_clean$hhold_income*mort_clean$loan_size

mort_clean$sl_max_cr <- scale(mort_clean$max_cr_score)

mort_clean$incomexscore <- mort_clean$hhold_income*mort_clean$sl_max_cr
#Splitting Estimation,validation and holdout data sets
r.train   <- filter(mort_clean,Sample == "Estimation")
r.test    <- filter(mort_clean,Sample == "Validation")
r.holdout <- filter(mort_clean,Sample =="Holdout" )
#Modeling
#Running a Logistic Regression
model1.logreg <- glm(default.f~amort_left_days+property_type+purchase_price+hhold_income+num_owner+cust_age_1+cust_gender_1+cust_cr_score_1+sl_max_cr+loan_size+interest_rate+TDS+famincome2018median+hholdpop2018+median_age+immigrant2018pct+aboriginalmothertongue2018pct+universitydegree2018pct+belowhighschool2018pct+highschool2018pct+trade2018pct+college2018pct+universitynondegree2018pct+Median_price_detach+Median_price_semi+Median_price_apart+cosmopolitan_elite2018+urbane_villagers2018+arts_affluence2018+suburban_success2018+asian_sophisticates2018+kids_careers2018+boomerang_city2018+satellite_burbs2018+emptying_nests2018+urban_digerati2018+street_scenes2018+asian_avenues2018+diversity_heights2018+heritage_hubs2018+pets_pcs2018+exurban_wonderland2018+management_material2018+grey_pride2018+aging_in_suburbia2018+asian_new_wave2018+fresh_air_families2018+south_asian_society2018+second_city_retirees2018+metro_multiculturals2018+silver_linings2018+new_world_symphony2018+heartland_retirees2018+rooms_with_a_view2018+country_acres2018+exurban_homesteaders2018+trucks_trades2018+grads_pads2018+our_time2018+wide_open_spaces2018+home_sweet_rows2018+traditional_town_living2018+suburban_scramble2018+min_age,data = r.train, family = binomial(logit))

summary(model1.logreg)
#Interactions
model1.logreg.interact <- glm(default.f~amort_left_days+property_type+purchase_price+hhold_income+num_owner+cust_age_1+cust_gender_1+cust_cr_score_1+sl_max_cr+loan_size+interest_rate+TDS+famincome2018median+hholdpop2018+median_age+immigrant2018pct+aboriginalmothertongue2018pct+universitydegree2018pct+belowhighschool2018pct+highschool2018pct+trade2018pct+college2018pct+universitynondegree2018pct+Median_price_detach+Median_price_semi+Median_price_apart+cosmopolitan_elite2018+urbane_villagers2018+arts_affluence2018+suburban_success2018+asian_sophisticates2018+kids_careers2018+boomerang_city2018+satellite_burbs2018+emptying_nests2018+urban_digerati2018+street_scenes2018+asian_avenues2018+diversity_heights2018+heritage_hubs2018+pets_pcs2018+exurban_wonderland2018+management_material2018+grey_pride2018+aging_in_suburbia2018+asian_new_wave2018+fresh_air_families2018+south_asian_society2018+second_city_retirees2018+metro_multiculturals2018+silver_linings2018+new_world_symphony2018+heartland_retirees2018+rooms_with_a_view2018+country_acres2018+exurban_homesteaders2018+trucks_trades2018+grads_pads2018+our_time2018+wide_open_spaces2018+home_sweet_rows2018+traditional_town_living2018+suburban_scramble2018+incomexscore+incomexsize+min_age,data = r.train, family = binomial(logit))
summary(model1.logreg.interact)

#Running a Stepwise Regression
model2.step <- step(model1.logreg,direction="both")
summary(model2.step)

model2.step_int <- step(model1.logreg.interact,direction="both")
summary(model2.step_int)

#Running a Classification Tree
model3.rpart <- rpart(formula = default.f ~ amort_left_days + property_type + purchase_price + 
                        loan_size + interest_rate + TDS + immigrant2018pct + cosmopolitan_elite2018 + 
                        satellite_burbs2018 + grey_pride2018 + asian_new_wave2018 + 
                        south_asian_society2018 + new_world_symphony2018 + trucks_trades2018 + 
                        incomexsize + suburban_scramble2018,
                      data = r.train,
                      cp = 0.0001, #set to 0.0001 to check 
                      model = TRUE)

plotcp(model3.rpart)
printcp(model3.rpart)

rpart.plot(model3.rpart,type=1,extra=2,fallen.leaves = FALSE,uniform=TRUE, yes.text="true",no.text="false",cex=0.6,digits=2)

#Running a Random Forest
model4.RF<-randomForest(default.f ~ amort_left_days + property_type +purchase_price + loan_size + interest_rate + TDS + immigrant2018pct +cosmopolitan_elite2018 + satellite_burbs2018 + grey_pride2018 + asian_new_wave2018 +  south_asian_society2018 + new_world_symphony2018 + trucks_trades2018 + incomexsize + suburban_scramble2018,
                        data=r.train,
                        mtry=sqrt(16), ntree= 500,
                        importance = TRUE)
model4.RF

importance(model4.RF,type = 2)
varImpPlot(model4.RF,type = 2, main = "Importance Plot")

#Running a Neural Net
model5.Neunet <- Nnet(formula = default.f ~ amort_left_days + property_type + purchase_price + 
                        loan_size + interest_rate + TDS + immigrant2018pct + cosmopolitan_elite2018 + 
                        satellite_burbs2018 + grey_pride2018 + asian_new_wave2018 + 
                        south_asian_society2018 + new_world_symphony2018 + trucks_trades2018 + 
                        incomexsize + suburban_scramble2018,
                      data = r.train,
                      decay = 0.10, # decay parameter
                      size = 2)
model5.Neunet$value
summary(model5.Neunet)

#Model Assessment
summary(r.train)

#Estimation - Cumulative
lift.chart(modelList = c("model1.logreg.interact","model2.step_int", "model3.rpart", "model4.RF","model5.Neunet"),
           data = filter(mort_clean, Sample == "Estimation"),
           targLevel = "1", 
           trueResp = 0.021,
           type = "cumulative", sub = "Estimation")
#Validation - Cumulative
lift.chart(modelList = c("model1.logreg.interact","model2.step_int", "model3.rpart", "model4.RF","model5.Neunet"),
           data = r.test,
           targLevel = "1", 
           trueResp = 0.021,
           type = "cumulative", sub = "Validation")
#Incremental 
lift.chart(modelList = c("model1.logreg.interact"),
           data = r.test,
           targLevel = "1",
           trueResp = 0.021,
           type = "incremental", # "incremental" instead of "cumulative" 
           sub = "Validation")
#Effects Plot
plot(allEffects(model2.step_int),type = "response")
#Partial Dependency Plot for Random Forest

partialPlot(model4.RF,
            pred.data = r.test,
            x.var = TDS,
            sub = "Validation Set", 
            which.class = "1")
partialPlot(model4.RF,
            pred.data = r.test,
            x.var = amort_left_days,
            sub = "Validation Set", 
            which.class = "1")
partialPlot(model4.RF,
            pred.data = r.test,
            x.var = incomexsize,
            sub = "Validation Set", 
            which.class = "1")

#Scoring Prediction
mort_clean$default.model2.step <- rawProbScore(model = "model1.logreg.interact",
                                               data = mort_clean,
                                               targLevel = "1")

mort_clean$default.model2.step

Submission.model2.step <- mort_clean[mort_clean$Sample == "Validation",c("mg_acc","default.model2.step","default",
                                                                      "interest_rate","loan_size")]



names(Submission.model2.step) <- c("mg_acc", "score","default", "interest","loan")

hist(abs(Submission.model2.step$score-Submission.model2.step$default))
mean(abs(Submission.model2.step$score-Submission.model2.step$default))
write.csv(Submission.model2.step,"Submission.model2.step_default.csv")

