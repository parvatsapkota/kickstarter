library("stats")
library("dplyr")
library("lmtest")
library("mfx")

#memory.limit(size = 3000)

kickdata <- read.csv("C:\\Users\\psapkota\\Desktop\\Econ Capstone\\Project\\Data\\kickstarter_data_with_features_roughdraft.csv")

#state
kickdata$bin_state=0
kickdata$bin_state[kickdata$state == "successful"] = 1

#country
kickdata$country = as.factor(kickdata$country)
kickdata$country <- relevel(kickdata$country,ref = "US")

#category
kickdata$category[kickdata$category == ""] = "Others"
kickdata$category = as.factor(kickdata$category)
kickdata$categoy <- relevel(kickdata$country,ref = "Academic")
unique(kickdata$category)

#deadline_weekday
kickdata$deadline_weekday = as.factor(kickdata$deadline_weekday)

#created_at_weekday
kickdata$created_at_weekday = as.factor(kickdata$created_at_weekday)

#launched_at_weekday
kickdata$launched_at_weekday = as.factor(kickdata$launched_at_weekday)

#deadline_month
kickdata$deadline_month = as.factor(kickdata$deadline_month)

#deadline_yr
kickdata$deadline_yr = as.factor(kickdata$deadline_yr)

#state_changed_at_month
#kickdata$state_changed_at_month = as.factor(kickdata$state_changed_at_month)

#state_changed_at_yr
#kickdata$state_changed_at_yr = as.factor(kickdata$state_changed_at_yr)

#created_at_month
kickdata$created_at_month = as.factor(kickdata$created_at_month)

#created_at_yr
kickdata$created_at_yr = as.factor(kickdata$created_at_yr)

#launched_at_month
kickdata$launched_at_month = as.factor(kickdata$launched_at_month)

#launched_at_yr
kickdata$launched_at_yr = as.factor(kickdata$launched_at_yr)

#staff_pick
kickdata$staff_pick = as.factor(kickdata$staff_pick)
#kickdata$bin_staffpick=0
#kickdata$bin_staffpick[kickdata$staff_pick == "True"] = 1


View(kickdata)

summary(kickdata)
sd(kickdata$goal)
sd(kickdata$name_len_clean)
sd(kickdata$blurb_len_clean)
sd(kickdata$create_to_launch)
sd(kickdata$launch_to_deadline)
sd(kickdata$name_len_clean)
sd(kickdata$blurb_len_clean)


#reg1 = lm(bin_state~ goal + country + staff_pick + category +  name_len_clean + blurb_len_clean+ deadline_weekday+ created_at_weekday+ launched_at_weekday + deadline_month +
#            deadline_yr+ state_changed_at_month+ state_changed_at_yr+ created_at_month+ created_at_yr+launched_at_month+ launched_at_yr+ create_to_launch+ launch_to_deadline, data = kickdata)
#summary(reg1)
#relevel(x,ref = WHATYOUWANT)
#unique(kickdata$category)


#Model 1:Regression consisting of all variables
#Probit regression
reg1 = lm(bin_state~ goal + country + staff_pick + category +  name_len_clean + blurb_len_clean+ deadline_weekday+ created_at_weekday+ launched_at_weekday + deadline_month +
            deadline_yr + created_at_month+ created_at_yr+launched_at_month+ launched_at_yr+ create_to_launch+ launch_to_deadline, data = kickdata,
          family = binomial(link = "probit"))
summary(reg1)

#Calculating the marginal effects
probitmfx(reg1, data = kickdata, atmean = TRUE)


#Model 2: Regression consisting of only small terms
#Probit regression

#Calculating the marginal effects


unique(kickdata$state)
count(kickdata,state)
#library("stargazer")
#stargazer(kickdata)
unique(kickdata$country)
unique(kickdata$currency)
unique(kickdata$launched_at_yr)



