##################### Create & Visualize Data ##################### 
set.seed(0)
conc1 = rnorm(100, mean = 5, sd= 1)
conc2 = rnorm(100, mean = 30, sd= 5)
conc3 = rnorm(100, mean = 60, sd= 10)
conc4 = rnorm(100, mean = 100, sd= 10)

set.seed(0)
efficacy1 = rnorm(100, mean = 0, sd= 0)
efficacy2 = rnorm(100, mean = 30, sd= 5)
efficacy3= rnorm(100, mean = 60, sd= 5)
efficacy4= rnorm(100, mean = 60, sd= 5)

conc = c(conc1,conc2,conc3, conc4)
efficacy = c(efficacy1,efficacy2, efficacy3, efficacy4)

set.seed(0)
random = rnorm(400, mean = 0,sd = 0)
df = data.frame(conc, efficacy, random)
View(df)

plot(df$conc,df$efficacy,
     col = "blue3",
     xlab = "Blood Concentration",
     ylab = "Efficacy")

cor(df$efficacy,df$conc)

# correlation = 0.87

##################### Split Data ##################### 

library(caTools)
set.seed(0)
split = sample.split(df,SplitRatio = 0.8)
train = subset(df,split == TRUE)
test = subset(df, split == FALSE)


##################### Linear Regression #######################

lm = lm(efficacy~ conc , data = train)
summary(lm)

y_pred_lm = predict(lm,test)

cor(y_pred_lm, test$efficacy)
MAE_lm = mean(abs(y_pred_lm - test$efficacy))
MAE_lm  

# correlation = 0.87
# MAE = 10.46

##################### Cubist Model Tree #######################
library(Cubist)
mtree = cubist(train[,-2],train$efficacy, control = cubistControl(rules = 3))
summary(mtree)

y_pred_mtree = predict(mtree, test)

cor(y_pred_mtree,test$efficacy)
MAE_mtree = mean(abs(y_pred_mtree - test$efficacy)) 
MAE_mtree


# correlation = 0.96
# MAE = 3.54


                           ################

# Clearly; the Cubist Model Tree is FAR SUPERIOR in this case :D
                       


 