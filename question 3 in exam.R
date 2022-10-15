
attach(Household_Pulse_data)
summary(Household_Pulse_data)
Household_Pulse_data$worried_people <- Household_Pulse_data$WORRY == "nearly every day worry"
summary(Household_Pulse_data$worried_people)
dat_use <- subset(Household_Pulse_data,use_varb) 
summary(dat_use)
regression1 <- lm(worried_people ~ WRKLOSSRV, data = dat_use )
summary(regression)

# people who lost their jobs are more worried than the others whi didn't lose their jobs.
# as 5.729 t value for people who didn't lose their jobs is more than the 16.429 for people who recently lost their jobs.
# The results from causation of workloos and worry is statistically significant because of the p-values.

# We can use a subset of people who live in califronia  , and for the people who born in 1996 


restrict1 <- as.logical ((EST_ST == "California" )) | ((TBIRTH_YEAR == "1996"))
data_new <- subset(Household_Pulse_data,restrict1)
educ_indx <- factor((educ_nohs + 2*educ_hs + 3*educ_somecoll + 4*educ_college + 5*educ_advdeg), levels=c(1,2,3,4,5),labels = c("No HS","HS","SmColl","Bach","Adv"))

attach(data_new)

norm_varb <- function(X_in) { (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}

regression2 <- lm(worried_people ~ WRKLOSSRV, KINDWORK , TBIRTH_YEAR , data = dat_use )

summary(regression2)

norm_WRKLOSS <- norm_varb(WRKLOSSRV)
norm_KINDWORK <= norm_varb(KINDWORK)
norm_TBIRTHYEAR <- norm_varb(TBIRTH_YEAR)

data_use_prelim <- data.frame(norm_WRKLOSS , norm_TBIRTHYEAR , norm_KINDWORK)

good_obs_data_use <- complete.cases(data_use_prelim,educ_indx)

dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(educ_indx,good_obs_data_use)

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.9)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]

summary(cl_data)
prop.table(summary(cl_data))
summary(train_data)
require(class)



cl_data_n <- as.numeric(cl_data)
model_ols1 <- lm(cl_data_n ~ train_data$norm_WRKLOSS , train_data$norm_KINDWORK , train_data$norm_TBIRTHYEAR) 
y_hat <- fitted.values(model_ols1)
