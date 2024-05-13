hw <- read.csv("C:/Users/sonja/Documents/Dokumente/Studium/Master/Spatio_temporal_modelling/weight-height.csv")
head(hw)
summary(hw)

install.packages("measurements")
library(measurements)
hw2 <- data.frame(Gender = hw$Gender,
                  Weight = conv_unit(hw$Weight,"lbs","kg"),
                  Height = conv_unit(hw$Height,"inch","cm"))
head(hw2)
summary(hw2)

plot(hw2$Height, hw2$Weight)

library(dplyr)

# provide 10 random samples
dplyr::sample_n(hw2, 10)

# how are the values for male and female?
summary(filter(hw2, Gender == "Female"))
summary(filter(hw2, Gender == "Male"))

# any obvious anomalies or indications of significant correlation between male and female values?
# see it with notches (triangles), when they overlap there is a significant correlation
boxplot(filter(hw2, Gender =="Female")$Weight, filter(hw2, Gender=="Male")$Weight, notch = T)
boxplot(filter(hw2, Gender =="Female")$Height, filter(hw2, Gender=="Male")$Height, notch = T)
# no correlation between the two datasets

# is the set normaly distributed?
shapiro.test(hw2$Weight)
# Error in shapiro.test(hw2$Weight) : Stichprobengröße muss zwischen 3 und 5000 liegen

# make sample set smaller
shapiro.test(dplyr::sample_n(hw2, 5000)$Weight) #W = 0.98727, p-value < 2.2e-16
shapiro.test(dplyr::sample_n(hw2, 5000)$Height) #W = 0.99592, p-value = 1.442e-10

# some more exploratory plots:
plot(density(hw2$Weight))
plot(density(hw2$Height))

# seperate man and woman
# plot density by gender
plot(density(filter(hw2, Gender == "Female")$Weight), col ="red")
plot(density(filter(hw2, Gender == "Male")$Weight), col ="blue")

# both in the same plot
plot(density(filter(hw2, Gender == "Female")$Weight), col ="red")
lines(density(filter(hw2, Gender == "Male")$Weight), col ="blue")

plot(density(filter(hw2, Gender == "Female")$Height), col ="red")
lines(density(filter(hw2, Gender == "Male")$Height), col ="blue")
# looks like two different density distributions

# test for normality again
shapiro.test(dplyr::sample_n(filter(hw2,Gender =="Female"), 5000)$Weight) #W = 0.99978, p-value = 0.9134
shapiro.test(dplyr::sample_n(filter(hw2,Gender =="Male"), 5000)$Weight) #W = 0.99959, p-value = 0.3828

shapiro.test(dplyr::sample_n(filter(hw2,Gender =="Female"), 5000)$Height) #W = 0.99977, p-value = 0.9022
shapiro.test(dplyr::sample_n(filter(hw2,Gender =="Male"), 5000)$Height) #W = 0.99944, p-value = 0.1402
# not significantly different from normal distribution
# work with splitted dataset from now on!

# select just values for females
hw2.female <- filter(hw2, Gender =="Female")
summary(hw2.female)

# now linear regression
hw.lm <- lm(formula = Weight ~ Height, data= hw2.female)
summary(hw.lm)
# height and weight are significantly correlated for measurement of females

# can we predict weight based on this model?
hw.new <- data.frame(name=c("Agnes","Ela","Clara","Sonja","Jado","Rezna","Madhura"),
                     Height =c(173, 165, 168, 168, 171, 153, 160))

hw.lm.p <- predict(object = hw.lm, newdata = hw.new)

pred.weight <- data.frame(hw.new$name,
                          weight.pred=hw.lm.p)
pred.weight
# data set is from US, therefore prediction is too high for Europeans
# choose the right training data set for your prediction!