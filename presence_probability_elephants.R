# Spatial prediction of presence probability
# where are forest elephants??
# some locations are known because of sightings
# hypothesis: they are located around rivers and other water bodies in GABUN

install.packages("rgdal", repos="http://R-Forge.R-project.org")
library(rgdal)
library(sp)
library(raster)
library(sf)
library(terra)

setwd("C:/Users/sonja/OneDrive/Dokumente/EAGLE_Msc/Semester2/RS_in_Biodiversity/Data")

riverData <- vect("RiversCountriesOfInterest/RiversOneCountryTesting.gpkg")
elephantData <-  vect("ElephantsInOneCountry/elephantsInOneCountry.gpkg")

# add a row in elephant data showing that this is where elephants are
elephantData$true <- 1

plot(riverData)

# make new raster out of the river gpkg
r <- rast(riverData, ncols = 500, nrows = 500) #10000 makes it VERY big and it takes pretty long
r_pred <- rasterize(riverData, r, progress = "text")
plot(r_pred)

## make first prediction based on all rivers
# pred.d <- terra::distance(r_pred)
# names(pred.d) <- "dist"
# plot(pred.d)
# 
# e <- terra::extract(pred.d, elephantData)
# 
# # combine with response (excluding the ID column)
# v <- data.frame(cbind(pa=elephantData$true, dist = e$dist))
# 
# # build a model, here with glm
# model <- glm(formula=pa~., data=v)
# model
# 
# # predict to a raster
# r1 <- terra::predict(pred.d, model)
# 
# plot(r1)


## does elephant occurance have something to do with river size? based on strahler number

#strahler number 1
pred_strahler1 <- subset(riverData, riverData$Strahler=="1")

r_pred_strahler1 <- rasterize(pred_strahler1, r)

pred_strahler1.d <- terra::distance(r_pred_strahler1)
names(pred_strahler1.d) <- "dist.strahler1"

#strahler number 2

pred_strahler2 <- subset(riverData, riverData$Strahler=="2")

r_pred_strahler2 <- rasterize(pred_strahler2, r)

pred_strahler2.d <- terra::distance(r_pred_strahler2)
names(pred_strahler2.d) <- "dist.strahler2"

#strahler number 3

pred_strahler3 <- subset(riverData, riverData$Strahler=="3")

r_pred_strahler3 <- rasterize(pred_strahler3, r)

pred_strahler3.d <- terra::distance(r_pred_strahler3)
names(pred_strahler3.d) <- "dist.strahler3"

#strahler number 4

pred_strahler4 <- subset(riverData, riverData$Strahler=="4")

r_pred_strahler4 <- rasterize(pred_strahler4, r)

pred_strahler4.d <- terra::distance(r_pred_strahler4)
names(pred_strahler4.d) <- "dist.strahler4"

#strahler number 5

pred_strahler5 <- subset(riverData, riverData$Strahler=="5")

r_pred_strahler5 <- rasterize(pred_strahler5, r)

pred_strahler5.d <- terra::distance(r_pred_strahler5)
names(pred_strahler5.d) <- "dist.strahler5"

#strahler number 6

pred_strahler6 <- subset(riverData, riverData$Strahler=="6")

r_pred_strahler6 <- rasterize(pred_strahler6, r)

pred_strahler6.d <- terra::distance(r_pred_strahler6)
names(pred_strahler6.d) <- "dist.strahler6"

####
pred <- c(pred_strahler1.d, 
          pred_strahler2.d,
          pred_strahler3.d, 
          pred_strahler4.d, 
          pred_strahler5.d, 
          pred_strahler6.d)
pred

e <- terra::extract(pred, elephantData)

# combine with response (excluding the ID column)
v <- data.frame(cbind(pa=elephantData$true, 
                      dist.strahler1=e$dist.strahler1, 
                      dist.strahler2=e$dist.strahler2, 
                      dist.strahler3=e$dist.strahler3, 
                      dist.strahler4=e$dist.strahler4, 
                      dist.strahler5=e$dist.strahler5, 
                      dist.strahler6=e$dist.strahler6))
head(v)
v1 <- na.omit(v)
names(pred) <- names(v)[-1]
# build a model, here with glm
model <- glm(formula=pa~., data = v1)
model

# predict to a raster
r1 <- terra::predict(pred, model)

#plot
plot(r1, breaks = 50, legend = FALSE)
plot(pred_strahler1, add =T, col= "white", lwd = 1)
plot(pred_strahler2, add =T, col= "#C6E2FF", lwd = 2)
plot(pred_strahler3, add =T, col= "#B9D3EE", lwd = 3)
plot(pred_strahler4, add =T, col= "#9FB6CD", lwd = 4)
plot(pred_strahler5, add =T, col= "slategrey", lwd = 5)
plot(pred_strahler6, add =T, col= "darkgrey", lwd = 6)
plot(elephantData, add = T)

## make diagram of mean distance to rivers of certain strahler number
library(tidyr)
summary(v)
v_long <- v %>% pivot_longer(!pa)
ggplot(data = v_long, aes(x = name, y = value))+
  geom_violin(draw_quantiles = TRUE)+
  geom_point()+
  xlab("Strhaler Number/Rank of Rivers/Streams")+
  ylab("Distance of Elephant Observation to River/Stream")+
  ggtitle("Importance of small Rivers for Forest Elephants")

