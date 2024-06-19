devtools::install_github('sjevelazco/flexsdm')
library(flexsdm)
library(terra)
library(dplyr)
library(rgdal)

# set working directory
setwd("C:/Users/sonja/OneDrive/Dokumente/EAGLE_Msc/Semester2/Spatial_modeling_&_prediction/data")

# environmental data
#somevar <- system.file("external/somevar.tif", package = "flexsdm")
#somevar <- terra::rast(somevar) 
#names(somevar) <- c("aet", "cwd", "tmx", "tmn")

# load locust data
locust <- readOGR("locusts/locustsItaly2020to2021.shp")
plot(locust)

# environmental data (mean annual max temperature, mean annual min temperature, mean annual precipitation)
maxtemp <- terra::rast("locusts/meanMaxTemp2020to2021Italy.tif")
mintemp <- terra::rast("locusts/meantemp2020to2021Italy.tif")
precip <- terra::rast("locusts/meanPrec2020to2021Italy.tif")

# species occurence data (presence-only)
# data(hespero)
# hespero <- hespero %>% dplyr::select(-id)

# California ecoregions
#regions <- system.file("external/regions.tif", package = "flexsdm")
#regions <- terra::rast(regions)
#regions <- as.polygons(regions)
#sp_region <- terra::subset(regions, regions$category == "SCR") # ecoregion where *Hesperocyparis stephensonii* is found

# visualize the species occurrences
# plot(
#   sp_region,
#   col = "gray80",
#   legend = FALSE,
#   axes = FALSE,
#   main = "Hesperocyparis stephensonii occurrences"
# )
# points(hespero[, c("x", "y")], col = "black", pch = 16)
# cols <- rep("gray80", 8)
# cols[regions$category == "SCR"] <- "yellow"
# terra::inset(
#   regions,
#   loc = "bottomleft",
#   scale = .3,
#   col = cols
# )

### delimit calibration area

ca <- calib_area(
  data = locust,
  x = "coordinate",
  y = "decimalLon",
  method = c('buffer', width=25000),
  crs = crs(maxtemp)
)
plot(ca)
# visualize the species occurrences & calibration area
#plot(
#  sp_region,
#  col = "gray80",
#  legend = FALSE,
#  axes = FALSE,
#  main = "Calibration area and occurrences")
plot(ca, add=TRUE)
plot(locust, add = TRUE)

# make spatvector into data frame
locust_df <- as.data.frame(locust)

### create pseudo absence data

# Sample the same number of species presences
set.seed(10)
psa <- sample_pseudoabs(
  data = locust_df,
  x = "coordinate",
  y = "decimalLon",
  n = 55, # selecting number of pseudo-absence points that is equal to number of presences
  method = "random",
  rlayer = maxtemp,
  calibarea = ca
)

# look at presence and pseudo absence points
plot(maxtemp)
plot(ca, add = TRUE)
points(psa[,c("coordinate","decimalLon")], add = TRUE)
points(locust_df[,c("coordinate","decimalLon")], add = TRUE)


# Bind a presences and pseudo-absences
locust_pa <- bind_rows(locust_df, psa)
locust_pa # Presence-Pseudo-absence database

#pairing data for evaluating models
set.seed(10)

# Repeated K-fold method
locust_pa2 <- part_random(
  data = locust_pa,
  pr_ab = "pr_ab",
  method = c(method = "rep_kfold", folds = 5, replicates = 10)
)