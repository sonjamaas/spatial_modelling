# Spatial prediction of presence probability
# where can i meet EAGLE students?
# some locations are known because of sightings
# hypothesis: they are located around specific buildings on campus

install.packages("rgdal", repos="http://R-Forge.R-project.org")
library(rgdal)
library(sp)
library(raster)
# for laptop
# occ <- readOGR("C:/Users/sonja/Documents/Dokumente/Studium/Master/Spatio_temporal_modelling/occurence.gpkg")

class(occ)
summary(occ)
plot(occ)
# for laptop
# bui <- readOGR("C:/Users/sonja/Documents/Dokumente/Studium/Master/Spatio_temporal_modelling/campus_buildings.gpkg")
plot(bui)

# plotting additional infos of the points
plot(bui)
plot(occ[occ$students == 1,],col = 'blue', pch = 16, add = T)
plot(occ[occ$students == 0,],col = 'red', pch = 16, add = T)
# red = there is no student here, blue = there is a student here

# what might drive the presence of students? -> mensa, library, classrooms

# make new raster out of the building gpkg
r <- raster(bui, ncols = 100, nrows = 100)
rr.0 <- rasterize(bui, r, progress = "text")
plot(rr.0)

# calculate distance to every building
rr.0.d <- distance(rr.0)

preds <- rr.0.d # just renaming
plot(rr.0.d)

install.packages("sdm")
library(sdm)
library(caret)

d <- sdmData(formula=students~layer, train = occ, predictors = preds)
d

m1 <- sdm(students~., data=d, methods = c('glm', 'gam'))

p1 <- predict(m1, newdata = preds, filename ='sdm_preds_1.grd', overwrite = T) # doesnt work anymore!
