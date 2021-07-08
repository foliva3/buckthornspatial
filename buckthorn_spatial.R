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
sensort <- read.csv("K:/Environmental_Studies/hkropp/Data/campus/buckthorn/sapflux/sensors_meta.csv")
sensorc <- st_as_sf(sensort, coords = c("Longitude", "Latitude"), 
                    crs = 4326)
plot(sensorc$geometry)
sensorInfo <- st_transform(sensorc, crs = 32618)
plot(ndvi0707b)
plot(sensorInfo$geometry, add = TRUE, pch = 19)
extentB <- extent(466520, 466610, 4767390, 4767480)
extentS <- extent(466535, 466600, 4767390, 4767430)
m0503RGBc <- crop(m0503bRGB, extentB)
plotRGB(m0503RGBc, r = 3, g = 2, b = 1)
plot(sensorInfo$geometry, add = TRUE, pch = 19)
install.packages(c("mapview", "mapedit"))
library(mapview)
library(mapedit)
viewRGB(m0503RGBc, r = 3, g = 2, b = 1, maxpixels = 5000000)+
  mapview(sensorInfo)
m0503RGBc@ncols*m0503RGBc@nrows
removalBox <- st_polygon(list(rbind(c(-75.410795, 43.058728), 
                                    c(-75.410668, 43.058772),
                                    c(-75.410570, 43.058607),
                                    c(-75.410795, 43.058607),
                                    c(-75.410795, 43.058728))))
rmbox <- st_sfc(removalBox, crs = 4326)
rmboxs <- st_sf(data.frame(name = "removal"), geometry = rmbox)
removalp <- st_transform(rmboxs, crs = 32618)
plot(rmbox)
viewRGB(m0503RGBc, r = 3, g = 2, b = 1, maxpixels = 5000000)+
  mapview(removalBox)
plotRGB(m0503RGBc, r = 3, g = 2, b = 1)
plot(removalp$geometry, add = TRUE)

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
removalPoly <- st_transform(st_read("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/out/removal_bounds.shp"), 32618)
controlPoly <- st_transform(st_read("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/out/control_bounds.shp"), 32618)




rm1 <- extract(ndvi0707b, removalPoly)[[1]]
hist(rm1)

ctr1 <- extract(ndvi0707b, controlPoly)[[1]]
hist(ctr1)
mean(rm1)
mean(ctr1)
plot(removalPoly$geometry)
plot(controlPoly$geometry)

ndvi0701b2R <- resample(ndvi0701b2, ndvi0707b)
plot(ndvi0701b2R - ndvi0707b)
