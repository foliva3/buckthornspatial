install.packages("sf","raster","tmap")
library(sf)
library(raster)
library(tmap)
library(rgdal)

m0503bRGB <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/odm_orthophotoRGB.tif")
plotRGB(m0503bRGB, r = 3, g = 2, b = 1)

m0503b <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_blue.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_green.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_red.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_red edge.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_nir.tif")

plotRGB(m0503b, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")

ndvi0503b <- (m0503b[[5]] - m0503b[[3]] / m0503b[[5]] + m0503b[[3]])
plot(ndvi0503b)


m0519b <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_19_21_buckthorn/05_19_21_transparent_reflectance_blue.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_19_21_buckthorn/05_19_21_transparent_reflectance_green.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_19_21_buckthorn/05_19_21_transparent_reflectance_red.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_19_21_buckthorn/05_19_21_transparent_reflectance_red edge.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_19_21_buckthorn/05_19_21_transparent_reflectance_nir.tif")

plotRGB(m0519b, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")

ndvi0519b <- (m0519b[[5]] - m0519b[[3]] / m0519b[[5]] + m0519b[[3]])
plot(ndvi0519b)



m0607b <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_07_21_buckthorn/June_7_transparent_reflectance_blue.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_07_21_buckthorn/June_7_transparent_reflectance_green.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_07_21_buckthorn/June_7_transparent_reflectance_red.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_07_21_buckthorn/June_7_transparent_reflectance_red edge.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_07_21_buckthorn/June_7_transparent_reflectance_nir.tif")

plotRGB(m0607b, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0607b <- (m0607b[[5]] - m0607b[[3]]) / (m0607b[[5]] + m0607b[[3]])
plot(ndvi0607b)



m0610b <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_10_21_buckthorn/06_10_transparent_reflectance_blue.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_10_21_buckthorn/06_10_transparent_reflectance_green.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_10_21_buckthorn/06_10_transparent_reflectance_red.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_10_21_buckthorn/06_10_transparent_reflectance_red edge.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_10_21_buckthorn/06_10_transparent_reflectance_nir.tif")

plotRGB(m0610b, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0610b <- (m0610b[[5]] - m0610b[[3]]) / (m0610b[[5]] + m0610b[[3]])
plot(ndvi0610b)


#rerun 06_18 part 2 in pix4d
m0618b1 <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_blue.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_green.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_red.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_red edge.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_nir.tif")

plotRGB(m0618b1, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0618b1 <- (m0618b1[[5]] - m0618b1[[3]]) / (m0618b1[[5]] + m0618b1[[3]])
plot(ndvi0618b1)

m0618b2 <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p2/06_18_21_buckthorn_p2_rerun_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p2/06_18_21_buckthorn_p2_rerun_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p2/06_18_21_buckthorn_p2_rerun_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p2/06_18_21_buckthorn_p2_rerun_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p2/06_18_21_buckthorn_p2_rerun_transparent_reflectance_nir.tif")

plotRGB(m0618b2, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0618b2 <- (m0618b2[[5]] - m0618b2[[3]]) / (m0618b2[[5]] + m0618b2[[3]])
plot(ndvi0618b2)




m0625b1 <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_nir.tif")
plotRGB(m0625b1, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0625b1 <- (m0625b1[[5]] - m0625b1[[3]]) / (m0625b1[[5]] + m0625b1[[3]])
plot(ndvi0625b1)


m0625b2 <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p2/6_25_21_buckthorn_part2_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p2/6_25_21_buckthorn_part2_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p2/6_25_21_buckthorn_part2_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p2/6_25_21_buckthorn_part2_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p2/6_25_21_buckthorn_part2_transparent_reflectance_nir.tif")

plotRGB(m0625b2, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0625b2 <- (m0625b2[[5]] - m0625b2[[3]]) / (m0625b2[[5]] + m0625b2[[3]])
plot(ndvi0625b2)


m0625r <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_reforestation/6_25_21_reforestation_p2_transparent_reflectance_blue.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_reforestation/6_25_21_reforestation_p2_transparent_reflectance_green.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_reforestation/6_25_21_reforestation_p2_transparent_reflectance_red.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_reforestation/6_25_21_reforestation_p2_transparent_reflectance_red edge.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_reforestation/6_25_21_reforestation_p2_transparent_reflectance_nir.tif")
plotRGB(m0625r, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0625r <- (m0625r[[5]] - m0625r[[3]]) / (m0625r[[5]] + m0625r[[3]])
plot(ndvi0625r)


m0701b1 <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_blue.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_green.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_red.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_red edge.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_nir.tif")

plotRGB(m0701b1, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")


ndvi0701b1 <- (m0701b1[[5]] - m0701b1[[3]]) / (m0701b1[[5]] + m0701b1[[3]])
plot(ndvi0701b1)


m0701b2 <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p2/07_01_21_buckthorn_p2_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p2/07_01_21_buckthorn_p2_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p2/07_01_21_buckthorn_p2_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p2/07_01_21_buckthorn_p2_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p2/07_01_21_buckthorn_p2_transparent_reflectance_nir.tif")

plotRGB(m0701b2, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0701b2 <- (m0701b2[[5]] - m0701b2[[3]]) / (m0701b2[[5]] + m0701b2[[3]])
plot(ndvi0701b2)

m0707b <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_07_21_buckthorn/07_07_21_buckthorn_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_07_21_buckthorn/07_07_21_buckthorn_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_07_21_buckthorn/07_07_21_buckthorn_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_07_21_buckthorn/07_07_21_buckthorn_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_07_21_buckthorn/07_07_21_buckthorn_transparent_reflectance_nir.tif")

plotRGB(m0707b, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0707b <- (m0707b[[5]] - m0707b[[3]]) / (m0707b[[5]] + m0707b[[3]])
plot(ndvi0707b)

#adding gps coordinates to maps
#read data for longitude and latitude
sensort <- read.csv("K:/Environmental_Studies/hkropp/Data/campus/buckthorn/sapflux/sensors_meta.csv")
sensorc <- st_as_sf(sensort, coords = c("Longitude", "Latitude"), 
                    crs = 4326)
#plots points without map
plot(sensorc$geometry)
#transforms sensorc so that points are in utm zone 18N wgs 84
sensorInfo <- st_transform(sensorc, crs = 32618)
#add tree coordinates to ndvi map of 0707b
plot(ndvi0707b)
plot(sensorInfo$geometry, add = TRUE, pch = 19)
#zoomed out view of plot
extentB <- extent(466520, 466610, 4767390, 4767480)
#zoomed in view of plot
extentS <- extent(466535, 466600, 4767390, 4767430)
#extentB view of 0503RGB with tree coords
m0503RGBc <- crop(m0503bRGB, extentB)
plotRGB(m0503RGBc, r = 3, g = 2, b = 1)
plot(sensorInfo$geometry, add = TRUE, pch = 19)

#install.packages(c("mapview", "mapedit"))
library(mapview)
library(mapedit)
#changes view of map so that the max number of pixels is 5000000
viewRGB(m0503RGBc, r = 3, g = 2, b = 1, maxpixels = 5000000)+
  mapview(sensorInfo)
m0503RGBc@ncols*m0503RGBc@nrows
#made box for removal plot
removalBox <- st_polygon(list(rbind(c(-75.410795, 43.058728), 
                                    c(-75.410668, 43.058772),
                                    c(-75.410570, 43.058607),
                                    c(-75.410795, 43.058607),
                                    c(-75.410795, 43.058728))))
rmbox <- st_sfc(removalBox, crs = 4326)
rmboxs <- st_sf(data.frame(name = "removal"), geometry = rmbox)
#transformed rmbox so that the coordinates are in wgs 84 utm zone 18N
removalp <- st_transform(rmboxs, crs = 32618)
plot(rmbox)
#why do we plot rmbox instead of removalp?
viewRGB(m0503RGBc, r = 3, g = 2, b = 1, maxpixels = 5000000)+
  mapview(removalBox)
plotRGB(m0503RGBc, r = 3, g = 2, b = 1)
plot(removalp$geometry, add = TRUE)

#drew manually the bounds of removal plot because of inaccuracies for removal plot above
#removalPoly <- drawFeatures(
viewRGB(m0503RGBc, r = 3, g = 2, b = 1)+
  mapview(removalBox)+
  mapview(sensorInfo))

#st_write(removalPoly, "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/out/removal_bounds.shp")

#controlPoly <- drawFeatures(
  viewRGB(m0503RGBc, r = 3, g = 2, b = 1)+
    mapview(removalBox)+
    mapview(sensorInfo)+
    mapview(removalPoly))
#st_write(controlPoly, "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/out/control_bounds.shp")
#transformed removalPoly and controlPoly so that they are in wgs 84 utm zone 18N
removalPoly <- st_transform(st_read("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/out/removal_bounds.shp"), 32618)
controlPoly <- st_transform(st_read("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/out/control_bounds.shp"), 32618)



#histogram of ndvi value distribution for 0707b in removal
rm0707b <- extract(ndvi0707b, removalPoly)[[1]]
hist(rm0707b)
#histogram of ndvi value distribution for 0707b in control
ctr0707b <- extract(ndvi0707b, controlPoly)[[1]]
hist(ctr0707b)
#mean ndvi value for control and removal in 0707b
mean(rm0707b)
mean(ctr0707b)

#resampling where 0707b is the base map
ndvi0701b2R <- resample(ndvi0701b2, ndvi0707b)
plot(ndvi0701b2R - ndvi0707b)
#resampling where 0503b is the base map
ndvi0707bR <- resample(ndvi0707b, ndvi0503b)
plot(ndvi0707bR - ndvi0503b)

ndvi0618b1R <- resample(ndvi0618b1, ndvi0503b)
plot(ndvi0618b1R - ndvi0503b)
#histogram for ndvi value distribution from 0618b1 removal
rm0618b1 <- extract(ndvi0618b1, removalPoly)[[1]]
hist(rm0618b1)
mean(rm0618b1)
#histogram for ndvi value distribution from 0618b control
ctr0618b1 <- extract(ndvi0618b1, controlPoly)[[1]]
hist(ctr0618b1)
mean(ctr0618b1)
#histogram of ndvi values for 0625r
hist(ndvi0625r)
#drew rough plot of where trees were planted for reforestation
#reforestbox <- drawFeatures(
  viewRGB(m0625r, r = 3, g = 2, b = 1)
#st_write(reforestbox, "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/out/reforest_bounds.shp")
rf0625 <- extract(ndvi0625r, reforestbox)[[1]]
hist(rf0625)
mean(rf0625)
