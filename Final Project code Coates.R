#Final Project 
#Brandon Coates

#set working directory for Homework Files
setwd("C:/Users/bjc_2/Documents/MA4720/Final Project")

# load R packages that will be used
library(lsmeans) # for lsmeans function
library(gplots)
library(pwr)
library(car) # for Levene's test

#Import Data
vacc <- read.table(file = "vaccine.data.txt", header = TRUE, stringsAsFactors = FALSE)
vacc
#Create Column for treatment combination
vacc$trtmt <- factor(vacc$Region * 10 + vacc$Race)
vacc

#--------------------------------------------------------------
#--------------------------------------------------------------
# Exploratory Analysis
#--------------------------------------------------------------
#Create Interaction Plot for data
# interaction plot
par(mfrow = c(1, 1))
interaction.plot(x.factor = vacc$Race, trace.factor = vacc$Region, response = vacc$Vaccination, 
                 type = "b", xlab = "Race", pch = c(1, 2, 3, 4), lty = c(2, 3, 4, 5), col = c(2, 4, 6, 8), # yaxt = "n", 
                 legend = FALSE, ylab = "Mean of Response", main = "Interaction Plot: Vaccine Experiment")
legend(x = 1.5, y = 60, legend = paste(c("Midwest", "Northeast", "South", "West"),"Region "), pch = c(1, 2, 3, 4), lty = c(2, 3, 4, 5), col = c(2, 4, 6, 8))

#Create Boxplot for each treatment level
boxplot(vacc$Vaccination~vacc$trtmt, xlab = "Treatment Combination", ylab = "Average Vaccination Rate",
        main = "Average Vaccination Rate Per Treatment Combination of Region and Race")

#--------------------------------------------------------------
#--------------------------------------------------------------
# Statistical Analysis
#--------------------------------------------------------------
#Conduct two-way complete model ANOVA
vacc.lm <- lm(Vaccination ~ factor(Region) * factor(Race), data = vacc)
anova(vacc.lm)

#Check Assumptions
# obtain the residuals and predicted values
vacc.z <- residuals(vacc.lm) / sd(residuals(vacc.lm))
vacc.p <- fitted(vacc.lm)
par(mfrow = c(2, 2))
# check outliers and model fitting
plot(vacc.z ~ as.numeric(vacc$trtmt), xlab = "Treatment Levels", ylab = "Standardized Residuals", main = "a")
lines(x = c(-1, 1000), y = c(0, 0))
# check normality
qqnorm(y = vacc.z, xlab = "Theory Quantiles", ylab = "Residual Quantiles", main = "b")
qqline(y = vacc.z)
# checking equal variance assumption using plot
plot(vacc.z ~ vacc.p, xlab = "Predcited Values", ylab = "Standardized Residuals", main = "c") 
# Levene's test of Equal Variance Assumption
leveneTest(vacc$Vaccination ~ vacc$trtmt, data = towel, center = mean)
#Check independence assumption
plot(vacc.z ~ vacc$Order, xlab = "Order", ylab = "Standardized Residuals", main = "d") 

#Construct 95% confidence intervals for pairwise contrasts for Region and Race differences Using Tukey Method
Region.lsm <- lsmeans(vacc.lm, ~ Region)
summary(contrast(object = Region.lsm, method = "pairwise", adjust = "Tukey"), level = 0.975, side = "two-sided", infer = c(T, T))
Race.lsm <- lsmeans(vacc.lm, ~ Race)
summary(contrast(object = Race.lsm, method = "pairwise", adjust = "Tukey"), level = 0.975, side = "two-sided", infer = c(T, T))




