##################################################################
#### Code uses the merged airbnb_crime_month_block_group data ####
#### for visualizations, and to conduct t-tests & RD analysis ####
##################################################################

#### Author: SSP
#### Created: 11/11/2016
#### Last date modified: 11/27/2016 #Trying out some additional analyses

rm(list=ls())
options(stringsAsFactors = F)
gc()

library(lattice)
library(plyr)
library(reshape2)


##############################
### 1) SET UP
##############################

setwd("~/SODA502")

#### Bring-in the required data
df <- read.csv("./Data/Merged/01-airbnb_crime_month_block_group_clean.csv", header = T)

df$total <- as.numeric(df[,6]) + as.numeric(df[,7]) +as.numeric(df[,8]) + as.numeric(df[,9]) + as.numeric(df[,10]) + as.numeric(df[,11]) + as.numeric(df[,12]) + as.numeric(df[,13]) + as.numeric(df[,14]) + as.numeric(df[,15]) + as.numeric(df[,16]) + as.numeric(df[,17]) + as.numeric(df[,18]) + as.numeric(df[,19])

#### Create a treatment variable
df$treat <- 0
df$treat[df$t >= 0] <- 1

#### Fix a variable name
names(df)[grep(names(df), pattern = "Offend")] <- "Sex_Offend_Other"


##############################
### 2) VISUALIZATIONS
##############################

#### Plots
for(i in 6:19){
  pdf(paste0("./Reports/Airbnb_and_Crime/",tolower(names(df)[i]),"_by_time.pdf"))
  
  print(xyplot(df[,i] ~ t, df, xlab = "Time", ylab = "Frequency of crime", main = paste0(names(df)[i]), groups = treat, pch=c(19,17), col=c("steelblue","orange"), type=c("p","r"), lwd=2, lty=c(2,1)))
  
  dev.off()
}

pdf(paste0("./Reports/Airbnb_and_Crime/",names(df)[21],"_by_time.pdf"))
xyplot(df[,21] ~ t, df, xlab = "Time", ylab = "Frequency of crime", main = "Total crime", groups = treat, pch=c(19,17), col=c("steelblue","orange"), type=c("p","r"), lwd=2, lty=c(2,1))
dev.off()

# For presentation
for(i in 6:19){
  jpeg(paste0("./Presentation/",tolower(names(df)[i]),"_by_time.jpg"))
  
  print(xyplot(df[,i] ~ t, df, xlab = "Time", ylab = "Frequency of crime", main = paste0(names(df)[i]), groups = treat, pch=c(19,17), col=c("steelblue","orange"), type=c("p","r"), lwd=2, lty=c(2,1)))
  
  dev.off()
}

jpeg(paste0("./Presentation/",names(df)[21],"_by_time.jpg"))
xyplot(df[,21] ~ t, df, xlab = "Time", ylab = "Frequency of crime", main = "Total crime", groups = treat, pch=c(19,17), col=c("steelblue","orange"), type=c("p","r"), lwd=2, lty=c(2,1))
dev.off()


##############################
### 3) T-TESTS
##############################

t_tests_greater <- rep(NA, length(6:19))
t_tests_less <- rep(NA, length(6:19))

for(i in 6:19){
  test.df <- rbind(cbind("pre" = df[df$t==-3,c("GEOID",paste0(colnames(df)[i]))], "post" = df[df$t==3,c("GEOID",paste0(colnames(df)[i]))]),
                   cbind("pre" = df[df$t==-2,c("GEOID",paste0(colnames(df)[i]))], "post" = df[df$t==2,c("GEOID",paste0(colnames(df)[i]))]),
                   cbind("pre" = df[df$t==-1,c("GEOID",paste0(colnames(df)[i]))], "post" = df[df$t==1,c("GEOID",paste0(colnames(df)[i]))]))
  
  j <- i-5
  t_tests_greater[j] <- t.test(test.df[,paste0("pre.",colnames(df)[i])], test.df[,paste0("post.",colnames(df)[i])], paired = T, alternative = "greater")$p.value
  t_tests_less[j] <- t.test(test.df[,paste0("pre.",colnames(df)[i])], test.df[,paste0("post.",colnames(df)[i])], paired = T, alternative = "less")$p.value
}

cbind(t_tests_less, names(df)[6:19])
cbind(t_tests_greater, names(df)[6:19])

# Same for total crime
test.df <- rbind(cbind("pre" = df[df$t==-3,c("GEOID",paste0(colnames(df)[21]))], "post" = df[df$t==3,c("GEOID",paste0(colnames(df)[21]))]),
                 cbind("pre" = df[df$t==-2,c("GEOID",paste0(colnames(df)[21]))], "post" = df[df$t==2,c("GEOID",paste0(colnames(df)[21]))]),
                 cbind("pre" = df[df$t==-1,c("GEOID",paste0(colnames(df)[21]))], "post" = df[df$t==1,c("GEOID",paste0(colnames(df)[21]))]))

t_total_greater <- t.test(test.df[,paste0("pre.",colnames(df)[21])], test.df[,paste0("post.",colnames(df)[21])], paired = T, alternative = "greater")$p.value
t_total_less <- t.test(test.df[,paste0("pre.",colnames(df)[21])], test.df[,paste0("post.",colnames(df)[21])], paired = T, alternative = "less")$p.value

## Need to export these tables into direct latex tables


##############################
### 4) REGRESSIONS
##############################

for(i in 6:19){
  lm.model <- lm(df[,i] ~ as.numeric(t)*treat, df)
  print(paste0(names(df)[i]))
  print(summary(lm.model))
}

lm.total <- lm(df$total ~ as.numeric(t)*treat, df)
summary(lm.total)


##############################
### 5) Additional analyses
##############################

#### Linear regression with poisson distribution on the outcome variable
for(i in 6:19){
  glm.poisson <- glm(df[,i] ~ as.numeric(t)*treat, df, family = quasipoisson(link=log))
  print(paste0(names(df)[i]))
  print(summary(glm.poisson))
}

glm.poisson.total <- glm(df$total ~ as.numeric(t)*treat, data = df, family = poisson(link=log))
summary(glm.poisson.total)


#### Block fixed effects with Gaussian outcome
for(i in 6:19){
  fixed.model <- lm(df[,i] ~ as.numeric(t)*treat + as.factor(GEOID), df)
  print(paste0(names(df)[i]))
  print(c(summary(fixed.model)$r.squared, summary(fixed.model)$adj.r.squared))
  # print(summary(fixed.model))
  print(as.matrix(summary(fixed.model)$coefficients)[c(1:3,194),4])
}

fixed.total <- lm(df$total ~ as.numeric(t)*treat + as.factor(GEOID), data = df)
summary(fixed.total)
# as.matrix(summary(fixed.total)$coefficients)[c(1:3,194),4]


#### Block fixed effects with poisson outcome
for(i in 6:19){
  fixed.poisson.model <- glm(df[,i] ~ as.numeric(t)*treat + as.factor(GEOID), df, family = quasipoisson(link=log))
  print(paste0(names(df)[i]))
  print((summary(fixed.poisson.model)$coefficients)[c(1:3,194),])
  # print(as.matrix(summary(fixed.poisson.model)$coefficients)[c(1:3,194),4])
}

fixed.poisson.total <- glm(df$total ~ as.numeric(t)*treat + as.factor(GEOID), data = df, family = quasipoisson(link=log))
as.matrix(summary(fixed.poisson.total)$coefficients)[c(1:3,194),]





################################################################################
#### Code checks robustness of analysis in 01-reg-discontinuity-block-level ####
################################################################################

#### Author: SSP
#### Created: 11/29/2016
#### Last date modified: 11/29/2016

rm(list=ls())
options(stringsAsFactors = F)
gc()

library(lattice)
library(plyr)
library(reshape2)


#### 1 month prior and post analysis
##############################
### 1) SET UP
##############################

setwd("~/SODA502")

#### Bring-in the required data
df <- read.csv("./Data/Merged/02-airbnb_crime_month_block_group_clean_1mo.csv", header = T)

df$total <- as.numeric(df[,6]) + as.numeric(df[,7]) +as.numeric(df[,8]) + as.numeric(df[,9]) + as.numeric(df[,10]) + as.numeric(df[,11]) + as.numeric(df[,12]) + as.numeric(df[,13]) + as.numeric(df[,14]) + as.numeric(df[,15]) + as.numeric(df[,16]) + as.numeric(df[,17]) + as.numeric(df[,18]) + as.numeric(df[,19])

#### Create a treatment variable
df$treat <- 0
df$treat[df$t >= 0] <- 1

#### Fix a variable name
names(df)[grep(names(df), pattern = "Offend")] <- "Sex_Offend_Other"


##############################
### 3) T-TESTS
##############################

t_tests_greater <- rep(NA, length(6:19))
t_tests_less <- rep(NA, length(6:19))

for(i in 6:19){
  test.df <- rbind(cbind("pre" = df[df$t==-1,c("GEOID",paste0(colnames(df)[i]))], "post" = df[df$t==1,c("GEOID",paste0(colnames(df)[i]))]))
  
  j <- i-5
  t_tests_greater[j] <- t.test(test.df[,paste0("pre.",colnames(df)[i])], test.df[,paste0("post.",colnames(df)[i])], paired = T, alternative = "greater")$p.value
  t_tests_less[j] <- t.test(test.df[,paste0("pre.",colnames(df)[i])], test.df[,paste0("post.",colnames(df)[i])], paired = T, alternative = "less")$p.value
}

cbind(t_tests_less, names(df)[6:19])
cbind(t_tests_greater, names(df)[6:19])

# Same for total crime
test.df <- rbind(cbind("pre" = df[df$t==-1,c("GEOID",paste0(colnames(df)[21]))], "post" = df[df$t==1,c("GEOID",paste0(colnames(df)[21]))]))

t_total_greater <- t.test(test.df[,paste0("pre.",colnames(df)[21])], test.df[,paste0("post.",colnames(df)[21])], paired = T, alternative = "greater")$p.value
t_total_less <- t.test(test.df[,paste0("pre.",colnames(df)[21])], test.df[,paste0("post.",colnames(df)[21])], paired = T, alternative = "less")$p.value


##############################
### 4) REGRESSIONS
##############################

for(i in 6:19){
  lm.model <- lm(df[,i] ~ as.numeric(t)*treat, df)
  print(paste0(names(df)[i]))
  print(summary(lm.model))
}

lm.total <- lm(df$total ~ as.numeric(t)*treat, df)
summary(lm.total)


##############################
### 5) Additional analyses
##############################

#### Linear regression with poisson distribution on the outcome variable
for(i in 6:19){
  glm.poisson <- glm(df[,i] ~ as.numeric(t)*treat, df, family = quasipoisson(link=log))
  print(paste0(names(df)[i]))
  print(summary(glm.poisson))
}

glm.poisson.total <- glm(df$total ~ as.numeric(t)*treat, data = df, family = poisson(link=log))
summary(glm.poisson.total)


#### Block fixed effects with Gaussian outcome
for(i in 6:19){
  fixed.model <- lm(df[,i] ~ as.numeric(t)*treat + as.factor(GEOID), df)
  print(paste0(names(df)[i]))
  print(c(summary(fixed.model)$r.squared, summary(fixed.model)$adj.r.squared))
  # print(summary(fixed.model))
  print(as.matrix(summary(fixed.model)$coefficients)[1:3,4])
}

fixed.total <- lm(df$total ~ as.numeric(t)*treat + as.factor(GEOID), data = df)
summary(fixed.total)
# as.matrix(summary(fixed.total)$coefficients)[c(1:3,194),4]


#### Block fixed effects with poisson outcome
for(i in 6:19){
  fixed.poisson.model <- glm(df[,i] ~ as.numeric(t)*treat + as.factor(GEOID), df, family = quasipoisson(link=log))
  print(paste0(names(df)[i]))
  print((summary(fixed.poisson.model)$coefficients)[1:3,])
  # print(as.matrix(summary(fixed.poisson.model)$coefficients)[c(1:3,194),4])
}

fixed.poisson.total <- glm(df$total ~ as.numeric(t)*treat + as.factor(GEOID), data = df, family = quasipoisson(link=log))
as.matrix(summary(fixed.poisson.total)$coefficients)[1:3,4]




#### 6 months prior and post analysis
##############################
### 1) SET UP
##############################

rm(list=ls())
options(stringsAsFactors = F)
gc()

library(lattice)
library(plyr)
library(reshape2)

setwd("~/SODA502")

#### Bring-in the required data
df <- read.csv("./Data/Merged/03-airbnb_crime_month_block_group_clean_6mo.csv", header = T)

df$total <- as.numeric(df[,6]) + as.numeric(df[,7]) +as.numeric(df[,8]) + as.numeric(df[,9]) + as.numeric(df[,10]) + as.numeric(df[,11]) + as.numeric(df[,12]) + as.numeric(df[,13]) + as.numeric(df[,14]) + as.numeric(df[,15]) + as.numeric(df[,16]) + as.numeric(df[,17]) + as.numeric(df[,18]) + as.numeric(df[,19])

#### Create a treatment variable
df$treat <- 0
df$treat[df$t >= 0] <- 1

#### Fix a variable name
names(df)[grep(names(df), pattern = "Offend")] <- "Sex_Offend_Other"


##############################
### 3) T-TESTS
##############################

t_tests_greater <- rep(NA, length(6:19))
t_tests_less <- rep(NA, length(6:19))

for(i in 6:19){
  test.df <- rbind(cbind("pre" = df[df$t==-6,c("GEOID",paste0(colnames(df)[i]))], "post" = df[df$t==6,c("GEOID",paste0(colnames(df)[i]))]),
                   cbind("pre" = df[df$t==-5,c("GEOID",paste0(colnames(df)[i]))], "post" = df[df$t==5,c("GEOID",paste0(colnames(df)[i]))]),
                   cbind("pre" = df[df$t==-4,c("GEOID",paste0(colnames(df)[i]))], "post" = df[df$t==4,c("GEOID",paste0(colnames(df)[i]))]),
                   cbind("pre" = df[df$t==-3,c("GEOID",paste0(colnames(df)[i]))], "post" = df[df$t==3,c("GEOID",paste0(colnames(df)[i]))]),
                   cbind("pre" = df[df$t==-2,c("GEOID",paste0(colnames(df)[i]))], "post" = df[df$t==2,c("GEOID",paste0(colnames(df)[i]))]),
                   cbind("pre" = df[df$t==-1,c("GEOID",paste0(colnames(df)[i]))], "post" = df[df$t==1,c("GEOID",paste0(colnames(df)[i]))]))
  
  j <- i-5
  t_tests_greater[j] <- t.test(test.df[,paste0("pre.",colnames(df)[i])], test.df[,paste0("post.",colnames(df)[i])], paired = T, alternative = "greater")$p.value
  t_tests_less[j] <- t.test(test.df[,paste0("pre.",colnames(df)[i])], test.df[,paste0("post.",colnames(df)[i])], paired = T, alternative = "less")$p.value
}

cbind(t_tests_less, names(df)[6:19])
cbind(t_tests_greater, names(df)[6:19])

# Same for total crime
test.df <- rbind(cbind("pre" = df[df$t==-6,c("GEOID",paste0(colnames(df)[i]))], "post" = df[df$t==6,c("GEOID",paste0(colnames(df)[i]))]),
                 cbind("pre" = df[df$t==-5,c("GEOID",paste0(colnames(df)[i]))], "post" = df[df$t==5,c("GEOID",paste0(colnames(df)[i]))]),
                 cbind("pre" = df[df$t==-4,c("GEOID",paste0(colnames(df)[i]))], "post" = df[df$t==4,c("GEOID",paste0(colnames(df)[i]))]),
                 cbind("pre" = df[df$t==-3,c("GEOID",paste0(colnames(df)[21]))], "post" = df[df$t==3,c("GEOID",paste0(colnames(df)[21]))]),
                 cbind("pre" = df[df$t==-2,c("GEOID",paste0(colnames(df)[21]))], "post" = df[df$t==2,c("GEOID",paste0(colnames(df)[21]))]),
                 cbind("pre" = df[df$t==-1,c("GEOID",paste0(colnames(df)[21]))], "post" = df[df$t==1,c("GEOID",paste0(colnames(df)[21]))]))

t_total_greater <- t.test(test.df[,paste0("pre.",colnames(df)[21])], test.df[,paste0("post.",colnames(df)[21])], paired = T, alternative = "greater")$p.value
t_total_less <- t.test(test.df[,paste0("pre.",colnames(df)[21])], test.df[,paste0("post.",colnames(df)[21])], paired = T, alternative = "less")$p.value


##############################
### 4) REGRESSIONS
##############################

for(i in 6:19){
  lm.model <- lm(df[,i] ~ as.numeric(t)*treat, df)
  print(paste0(names(df)[i]))
  print(summary(lm.model))
}

lm.total <- lm(df$total ~ as.numeric(t)*treat, df)
summary(lm.total)


##############################
### 5) Additional analyses
##############################

#### Linear regression with poisson distribution on the outcome variable
for(i in 6:19){
  glm.poisson <- glm(df[,i] ~ as.numeric(t)*treat, df, family = quasipoisson(link=log))
  print(paste0(names(df)[i]))
  print(summary(glm.poisson))
}

glm.poisson.total <- glm(df$total ~ as.numeric(t)*treat, data = df, family = poisson(link=log))
summary(glm.poisson.total)


#### Block fixed effects with Gaussian outcome
for(i in 6:19){
  fixed.model <- lm(df[,i] ~ as.numeric(t)*treat + as.factor(GEOID), df)
  print(paste0(names(df)[i]))
  print(c(summary(fixed.model)$r.squared, summary(fixed.model)$adj.r.squared))
  # print(summary(fixed.model))
  print(as.matrix(summary(fixed.model)$coefficients)[c(1:3,160),4])
}

fixed.total <- lm(df$total ~ as.numeric(t)*treat + as.factor(GEOID), data = df)
summary(fixed.total)
# as.matrix(summary(fixed.total)$coefficients)[c(1:3,194),4]


#### Block fixed effects with poisson outcome
for(i in 6:19){
  fixed.poisson.model <- glm(df[,i] ~ as.numeric(t)*treat + as.factor(GEOID), df, family = quasipoisson(link=log))
  print(paste0(names(df)[i]))
  print((summary(fixed.poisson.model)$coefficients)[c(1:3,160),])
  # print(as.matrix(summary(fixed.poisson.model)$coefficients)[c(1:3,194),4])
}

fixed.poisson.total <- glm(df$total ~ as.numeric(t)*treat + as.factor(GEOID), data = df, family = quasipoisson(link=log))
as.matrix(summary(fixed.poisson.total)$coefficients)[c(1:3,160),4]







##############################################################
#### Code uses merged airbnb_crime_geoid_yrmon_panel data ####
########## for unbalanced panel data analysis ##########
##################################################################

#### Author: SSP
#### Created: 11/30/2016
#### Last date modified: 11/30/2016

rm(list=ls())
options(stringsAsFactors = F)
gc()

library(DataCombine)
library(plm)


##############################
### 1) SET UP
##############################

setwd("~/SODA502")

#### Bring-in the required data
df <- read.csv("./Data/Merged/04-airbnb_crime_geoid_yrmon_panel.csv", header = T)

df <- df[,c(1:2, 5:21)]

df$total <- as.numeric(df[,4]) + as.numeric(df[,5]) + as.numeric(df[,6]) + as.numeric(df[,7]) + as.numeric(df[,8]) + as.numeric(df[,9]) + as.numeric(df[,10]) + as.numeric(df[,11]) + as.numeric(df[,12]) + as.numeric(df[,13]) + as.numeric(df[,14]) + as.numeric(df[,15]) + as.numeric(df[,16]) + as.numeric(df[,17])

df <- df[order(df$GEOID, df$yearmon),]
df <- slide(df, Var = "total", GroupVar = "GEOID", slideBy = -1)

df <- df[order(df$GEOID, df$yearmon),]
df <- slide(df, Var = "Rape", GroupVar = "GEOID", slideBy = -1)

pdf <- plm.data(x = df, indexes = c("GEOID", "yearmon"))

##############################
### 2) ANALYSIS
##############################


model <- plm(formula = total ~ rank + total.1,
             data = pdf, na.action = na.omit, effect = "time", model = "within")
summary(model)


model <- plm(formula = Rape ~ rank + Rape.1,
             data = pdf, na.action = na.omit, effect = "individual", model = "within")
summary(model)
