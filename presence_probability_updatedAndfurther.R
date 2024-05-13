# presence probability and first spatial prediction with up to date packages
library(terra)

# for PC
setwd("C:/Users/sonja/OneDrive/Dokumente/EAGLE_Msc/Semester2/Spatial_modeling_&_prediction")

occ <- vect("data/occurence.gpkg")

class(occ)
summary(occ)
plot(occ)
bui <- vect("data/campus_buildings.gpkg")
plot(bui)

# plot additional infos of the points
plot(occ[occ$students==1,],col = 'blue', pch=16,add=T)
plot(occ[occ$students==0,],col = 'red', pch=16,add=T)

# distance to uni buildings might explain presence of students
r <- rast(occ, ncols=100, nrows = 100)
r_pred <- rasterize(bui, r)
plot(r_pred)

pred.d <- terra::distance(r_pred)
names(pred.d) <- "dist"
plot(pred.d)

e <- terra::extract(pred.d, occ)

# combine with response (excluding the ID column)
v <- data.frame(cbind(pa=occ$students, dist = e$dist))

# build a model, here with glm
model <- glm(formula=pa~., data=v)
model

# predict to a raster
r1 <- terra::predict(pred.d, model)

plot(r1)

# EAGLES are everywhere on campus - can we be more specific?
# creating more relevant spatial data

pred_mensa <- subset(bui, bui$type=="Mensa")

r_pred_Mensa <- rasterize(pred_mensa, r)

pred_mensa.d <- terra::distance(r_pred_Mensa)
names(pred_mensa.d) <- "dist.M"

# course
pred_course <- subset(bui, bui$type=="course")

r_pred_course <- rasterize(pred_course, r)

pred_course.d <- terra::distance(r_pred_course)
names(pred_course.d) <- "dist.C"

# dorm
pred_dorm <- subset(bui, bui$type=="dorm")

r_pred_dorm <- rasterize(pred_dorm, r)

pred_dorm.d <- terra::distance(r_pred_dorm)
names(pred_dorm.d) <- "dist.D"

# what have we done and why? can the code be simplified?
pred <- c(pred_course.d, pred_dorm.d,pred_mensa.d)
pred

e <- terra::extract(pred, occ)

# combine with response (excluding the ID column)
v <- data.frame(cbind(pa=occ$students, dist.C=e$dist.C, dist.D=e$dist.D, dist.M=e$dist.M))
head(v)

# build a model, here with glm
model <- glm(formula=pa~., data = v)
model

# predict to a raster
r1 <- terra::predict(pred, model)

plot(r1)
plot(bui, add =T)

# much better spatial prediction!
# subsetting our data for better space-time predictions
occ.10h <- occ[occ$time == 10,]
occ.13h <- occ[occ$time == 13,]
occ.22h <- occ[occ$time == 22,]
plot(occ.10h)
plot(occ.13h, col ='red', add =T)
plot(occ.22h, col = 'blue', add =T)
plot(bui, add=T)

# what does it show? EAGLES are at different buildings at different times

# redo process but split occurence by time of day and stack the final prediction of 10am, 1pm and 10pm
# 10am
e.10 <- terra::extract(pred, occ.10h)
v.10 <- data.frame(cbind(pa=occ.10h$students, dist.C=e.10$dist.C, dist.D=e.10$dist.D, dist.M=e.10$dist.M))
head(v.10)
model.10 <- glm(formula=pa~., data = v.10)
model.10
r10 <- terra::predict(pred, model.10)
plot(r10)
plot(bui, add =T)

# 1pm
e.13 <- terra::extract(pred, occ.13h)
v.13 <- data.frame(cbind(pa=occ.13h$students, dist.C=e.13$dist.C, dist.D=e.13$dist.D, dist.M=e.13$dist.M))
head(v.13)
model.13 <- glm(formula=pa~., data = v.13)
model.13
r13 <- terra::predict(pred, model.13)
plot(r13)
plot(bui, add =T)

# 10 pm
e.22 <- terra::extract(pred, occ.22h)
v.22 <- data.frame(cbind(pa=occ.22h$students, dist.C=e.22$dist.C, dist.D=e.22$dist.D, dist.M=e.22$dist.M))
head(v.22)
model.22 <- glm(formula=pa~., data = v.22)
model.22
r22 <- terra::predict(pred, model.22)
plot(r22)
plot(bui, add =T)

# stack the raster layers
# r10_raster <- rast(r10)
# r13_raster <- rast(r13)
# r22_raster <- rast(r22)
# p.time <- c(r10_raster,r13_raster,r22_raster)
p.time1 <- c(r10, r13, r22)
nlyr(p.time1)

plotRGB(p.time1,r=1,g=2,b=3, stretch="lin")
