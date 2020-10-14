---
title: "Item Response theory (ITR) In R  "
author: "Turki Aljrees"

---

##======================TASKS IRT==============================###

# 1) Define the Models 
      ## 1.1 ) Note that: (ltm) library required pre-install the folloing package MASS, msm, polycor)
                ## which is  the latent trait model under the Item Response Theory (IRT) approach.

install.packages("ltm")
install.packages("MASS")
install.packages("msm")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("polycor")
install.packages("mvtnorm")

# 2) Activate the Models

library(ltm)
library(MASS)
library(msm)
library(dplyr)
library(ggplot2)
library(polycor)
library(mvtnorm)


##=======DATA Insight ==============================###


#Load the data
## I suggest we should save our data in .txt  format.

attach(LSAT)

data(LSAT)

head(LSAT)
View(LSAT)
cor(LSAT)
names(LSAT)
dim(LSAT)
summary(LSAT)
str(LSAT)
glimpse(LSAT)


# 3) run the Models 

    # 3.1 ) IRT Models:


IRTmodel = ltm(LSAT ~ z1,IRT.param = TRUE)

summary(IRTmodel)
#IRTmodel[["coefficients"]]
coef(IRTmodel)

## ** in the result the difficulty is the same as b = ability measuring the best participant 
## ** the discrimination how well measure those participants at those points
##** 

    # 3.2 )  Rasch Models (optional ) : 

model <- rasch(LSAT, IRT.param = TRUE)
summary(model)
coef(model)
#model[["coefficients"]]

           # 3.2.1 ) Asses the fit of module using GoF (Goodness of Fit for Rasch Models)

GoF.rasch(model)


## Result to be compared with: if your p value of Chi-square is = ? > .05 then is (ok) (good fit model).

##=============================================================###

# 4) find the item difficulty level 
coef(model, prob = TRUE, order = TRUE)
coef(IRTmodel, prob = TRUE, order = TRUE)

## 4.1 Result to be compared with:
##    A ) if the difficulty level of an item = 0, THEN 
##         the item measures learners with an AVERAGE ability. 
##     B) If the difficulty level of an item > 0, THEN
##         the item measures learners with a  HIGHER  ability.

##==============================(ICC) Vs (IIC) ===============================###

# 5 ) Plot Item  Characteristic Curve (ICC) :
                                ##==ka·ruhk·tuh·ri·stuhk===##
      ## yo show the probability of the item being answer correly against the abilty score
plot(IRTmodel, type = "ICC")
plot(model, type = "ICC")


    ## 5.1 ) to show some items 
plot(model, type = "ICC", items =c(4,5))
plot(IRTmodel, type = "ICC", items =c(1,3))


# 6 ) Plot the Item Information Curve (IIC) :
plot(model, type = "IIC")
plot(IRTmodel, type = "IIC")

    ## 6.1 ) to show some items 
plot(model, type = "IIC", items =c(3,5))


# 7 ) Plot the Test Information Function (TIF) :
plot(model, type = "IIC", items = 0 )

# Test Information result summary 
information(model, c(-4, 4))
information(IRTmodel, c(-4, 4))

## Result can provide how many percent % that our test in regard to test-takers' latent ability 

#===========FACTORS SCORES (2PL  ) =====##


# 8 ) fatcore score for each response pattern (combination)
## the Z score is the ability estimate for each pattern = obs

  #A) Rasch model :

  # 8.1)  fatcore score is about the pattern of the data overall 
## also it will give you all sort of single combination for each response pattern
factor.scores(model)
factor.scores(IRTmodel)
## **reult will have z1 = expected ability score 

  # 8.2) person  score is about the person pattern of the data and thier corresponding ability estimate  
## this is the refrence of the item 
person.fit(model)

  # 8.3) item.fit Item-Fit Statistics and P-values called aslo Catch-22, 
##becuse with large sample your item are always significant
          ## =  and this just like chi-square item.fit(model)in any structural analysis 
     
item.fit(model)
item.fit(IRTmodel)

  #B) IRT model :
summary(IRTmodel)
factor.scores(IRTmodel)
person.fit(IRTmodel)
item.fit(IRTmodel)
summary(IRTmodel)

### ========FACTORS SCORES (3PL  ) ========================

    ## you need to modle on the same date called 3PL moduel (3 parameter module)
    ## we repeat 3) nut instade to itm() we will used the tpm() moduel and the type would be = "latent.trait"
    # this was 3) : IRTmodel = ltm(LSAT ~ z1, constr = cbind(1:length(LSAT), 2, 1))

IRTmodel2 = tpm(LSAT, type =  "latent.trait", IRT.param = TRUE )

summary(IRTmodel2)

coef(IRTmodel2)

plot(IRTmodel2, type = "ICC")

plot(IRTmodel2, type = "IIC", item = 0 )

factor.scores(IRTmodel2)

person.fit(IRTmodel2)

item.fit(IRTmodel2)

#===ANOVA IRT table (comparison between models) ====#===========

#anova(IRTmodel, IRTmodel2)
anova(model, IRTmodel2)
## NOTE : The comparison between two or more models will only be valid if they are fitted to the same dataset. 


write.table(LSAT, file = "Book.CSV", sep = ",")
