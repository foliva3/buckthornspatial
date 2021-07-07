install.packages("sf","raster","tmap")
library(sf)
library(raster)
library(tmap)
library(rgdal)

m0503b <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/05_03_21_transparent_reflectance_blue.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/05_03_21_transparent_reflectance_green.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/05_03_21_transparent_reflectance_red.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/05_03_21_transparent_reflectance_red edge.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/05_03_21_transparent_reflectance_nir.tif"

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
