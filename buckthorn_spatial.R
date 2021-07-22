install.packages("sf","raster","tmap")
install.packages("maptools")
library(sf)
library(raster)
library(tmap)
library(rgdal)
library(ggplot2)


#extent for tmaps
extentB <- extent(466520, 466610, 4767390, 4767480)

m0503bRGB <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/odm_orthophotoRGB.tif")
plotRGB(m0503bRGB, r = 3, g = 2, b = 1)

tm_shape(m0503bRGB, bbox = extentB, unit = "m")+
  tm_rgb(r = 3, g = 2, b = 1)+
  tm_layout(title = "05/03/21", legend.outside = TRUE)+
  tm_compass(type= "arrow", size= 4, position = c("left", "bottom"), text.color = "white")+
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, text.color = "white", 
               position = c("right", "bottom"))+
  tm_shape(removalPoly)+
  tm_fill(alpha = 0, id = "Removal")+
  tm_borders(col = "red",
             lwd = 2,
             lty = "solid",
             alpha = NA,
             zindex = NA,
             group = NA)+
  tm_shape(controlPoly)+
  tm_fill(alpha = 0, title = "Treatment", id = "Control")+
  tm_borders(col = "blue",
             lwd = 2,
             lty = "solid",
             alpha = NA,
             zindex = NA,
             group = NA)+
  tm_add_legend(type = "symbol",
    labels = c("Removal", "Control"),
    col = c("red", "blue"),
    shape = NULL,
    border.col = "black",
    border.lwd = 1,
    border.alpha = NA,
    title = "Treatment",
    is.portrait = TRUE)
  





m0503b <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_blue.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_green.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_red.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_red edge.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_nir.tif")

plot(m0503b)
plotRGB(m0503b, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")


ndvi0503b <- (m0503b[[5]] - m0503b[[3]]) / (m0503b[[5]] + m0503b[[3]])
plot(ndvi0503b)


#tmap for ndvi0503b
tm_shape(ndvi0503b, bbox = extentB, unit = "m")+
  tm_raster(palette= "BrBG", style = "fisher", n= 10, midpoint = NA, title = "NDVI")+
  tm_layout(title = "05/03/21", legend.outside = TRUE)+
  tm_compass(type= "arrow", size= 4, position = c("left", "bottom"))+
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, position = c("right", "bottom"))



m0519b <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_19_21_buckthorn/05_19_21_transparent_reflectance_blue.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_19_21_buckthorn/05_19_21_transparent_reflectance_green.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_19_21_buckthorn/05_19_21_transparent_reflectance_red.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_19_21_buckthorn/05_19_21_transparent_reflectance_red edge.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/05_19_21_buckthorn/05_19_21_transparent_reflectance_nir.tif")

plotRGB(m0519b, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")

ndvi0519b <- (m0519b[[5]] - m0519b[[3]]) / (m0519b[[5]] + m0519b[[3]])
plot(ndvi0519b)



m0607b <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_07_21_buckthorn/June_7_transparent_reflectance_blue.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_07_21_buckthorn/June_7_transparent_reflectance_green.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_07_21_buckthorn/June_7_transparent_reflectance_red.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_07_21_buckthorn/June_7_transparent_reflectance_red edge.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_07_21_buckthorn/June_7_transparent_reflectance_nir.tif")

#plotRGB(m0607b, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0607b <- (m0607b[[5]] - m0607b[[3]]) / (m0607b[[5]] + m0607b[[3]])
#plot(ndvi0607b)



m0610b <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_10_21_buckthorn/06_10_transparent_reflectance_blue.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_10_21_buckthorn/06_10_transparent_reflectance_green.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_10_21_buckthorn/06_10_transparent_reflectance_red.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_10_21_buckthorn/06_10_transparent_reflectance_red edge.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_10_21_buckthorn/06_10_transparent_reflectance_nir.tif")

#plotRGB(m0610b, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
m0610bRGB <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_10_21_buckthorn/rgb/06_10_21_buckthorn_rgb_transparent_reflectance_group1.tif")

#rgb tmap 0610b
tm_shape(m0610bRGB, bbox = extentB, unit = "m")+
  tm_rgb(r = 1, g = 2, b = 3)+
  tm_layout(title = "06/10/21", title.color = "black", legend.outside = TRUE, title.snap.to.legend = TRUE)+
  tm_compass(type= "arrow", size= 4, position = c("left", "bottom"), text.color = "white")+
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, text.color = "white", 
               position = c("right", "bottom"))+
  tm_shape(removalPoly)+
  tm_fill(alpha = 0, id = "Removal")+
  tm_borders(col = "red",
             lwd = 2,
             lty = "solid",
             alpha = NA,
             zindex = NA,
             group = NA)+
  tm_shape(controlPoly)+
  tm_fill(alpha = 0, title = "Treatment", id = "Control")+
  tm_borders(col = "blue",
             lwd = 2,
             lty = "solid",
             alpha = NA,
             zindex = NA,
             group = NA)+
  tm_add_legend(type = "symbol",
                labels = c("Removal", "Control"),
                col = c("red", "blue"),
                shape = NULL,
                border.col = "black",
                border.lwd = 1,
                border.alpha = NA,
                title = "Treatment",
                is.portrait = TRUE)


ndvi0610b <- (m0610b[[5]] - m0610b[[3]]) / (m0610b[[5]] + m0610b[[3]])
#plot(ndvi0610b)

tm_shape(ndvi0610b, bbox = extentB, unit = "m")+
  tm_raster(palette= "BrBG", style = "fisher", n= 10, midpoint = NA, title = "NDVI")+
  tm_layout(title = "06/10/21", legend.outside = TRUE)+
  tm_compass(type= "arrow", size= 4, position = c("left", "bottom"))+
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, position = c("right", "bottom"))



#rerun 06_18 part 2 in pix4d
m0618b1 <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_blue.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_green.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_red.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_red edge.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_nir.tif")
m0618b1RGB <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p1/rgb/06_18_21_buckthorn_p1_unmerged_rgb_transparent_reflectance_group1.tif")

tm_shape(m0618b1RGB, bbox = extentB, unit = "m")+
  tm_rgb(r = 1, g = 2, b = 3)+
  tm_layout(title = "06/18/21", title.color = "black", 
            legend.outside = TRUE)+
  tm_compass(type= "arrow", size= 4, position = c("left", "bottom"), 
             text.color = "white")+
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, text.color = "white", 
               position = c("right", "bottom"))+
  tm_shape(removalPoly)+
  tm_fill(alpha = 0, id = "Removal")+
  tm_borders(col = "red",
             lwd = 2,
             lty = "solid",
             alpha = NA,
             zindex = NA,
             group = NA)+
  tm_shape(controlPoly)+
  tm_fill(alpha = 0, title = "Treatment", id = "Control")+
  tm_borders(col = "blue",
             lwd = 2,
             lty = "solid",
             alpha = NA,
             zindex = NA,
             group = NA)+
  tm_add_legend(type = "symbol",
                labels = c("Removal", "Control"),
                col = c("red", "blue"),
                shape = NULL,
                border.col = "black",
                border.lwd = 1,
                border.alpha = NA,
                title = "Treatment",
                is.portrait = TRUE)



plotRGB(m0618b1RGB, r = 1, g = 2, b = 3, scale = 0.5, stretch = "lin")




ndvi0618b1 <- (m0618b1[[5]] - m0618b1[[3]]) / (m0618b1[[5]] + m0618b1[[3]])
#plot(ndvi0618b1)

tm_shape(ndvi0618b1, bbox = extentB, unit = "m")+
  tm_raster(palette= "BrBG", style = "fisher", n= 10, midpoint = NA, title = "NDVI")+
  tm_layout(title = "06/18/21", legend.outside = TRUE)+
  tm_compass(type= "arrow", size= 4, position = c("left", "bottom"))+
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, position = c("right", "bottom"))



m0618b2 <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p2/06_18_21_buckthorn_p2_rerun_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p2/06_18_21_buckthorn_p2_rerun_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p2/06_18_21_buckthorn_p2_rerun_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p2/06_18_21_buckthorn_p2_rerun_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p2/06_18_21_buckthorn_p2_rerun_transparent_reflectance_nir.tif")

plotRGB(m0618b2, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0618b2 <- (m0618b2[[5]] - m0618b2[[3]]) / (m0618b2[[5]] + m0618b2[[3]])
#plot(ndvi0618b2)

m0618b2RGB <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_18_21_buckthorn_p2/rgb/06_18_21_buckthorn_p2_rgb_unmerged_transparent_reflectance_group1.tif")
plotRGB(m0618b2RGB, r = 1, g = 2, b = 3, scale = 0.5, stretch = "lin")

m0618b2RGBc <- crop(m0618b2RGB, extentB)

plotRGB(m0618b2RGBc, r = 1, g = 2, b = 3, scale = 0.3, stretch = "lin")



tm_shape(m0618b2RGB, bbox = extentB, unit = "m")+
  tm_rgb(r = 1, g = 2, b = 3, max.value = 120000)+
  tm_layout(title = "06/18/21", title.color = "white")+
  tm_compass(type= "arrow", size= 4, position = c("left", "bottom"), text.color = "white")+
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, text.color = "white", position = c("right", "bottom"))


m0625b1 <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_nir.tif")
plotRGB(m0625b1, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0625b1 <- (m0625b1[[5]] - m0625b1[[3]]) / (m0625b1[[5]] + m0625b1[[3]])
#plot(ndvi0625b1)

m0625b1RGB <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/P4M/flight_6_25_21_buckthorn_p1/ortho/06_25_21_buckthorn_p1_rgb/4_index/reflectance/06_25_21_buckthorn_p1_rgb_transparent_reflectance_group1.tif")
plotRGB(m0625b1RGB, r= 1, g= 2, b= 3, scale =0.5, stretch = "lin")

#rgb tmap
tm_shape(m0625b1RGB, bbox = extentB, unit = "m")+
  tm_rgb(r = 1, g = 2, b = 3, max.value = 0.38)+
  tm_layout(title = "06/25/21", title.color = "white")+
  tm_compass(type= "arrow", size= 4, position = c("left", "bottom"), text.color = "white")+
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, text.color = "white", position = c("right", "bottom"))


#rgb multi tmap
tm_shape(m0625b1, bbox = extentB, unit = "m")+
  tm_rgb(r = 3, g = 2, b = 1, max.value = 0.514)+
  tm_layout(title = "06/25/21", title.color = "white")+
  tm_compass(type= "arrow", size= 4, position = c("left", "bottom"), text.color = "white")+
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, text.color = "white", position = c("right", "bottom"))

#ndvi tmap
tm_shape(ndvi0625b1, bbox = extentB, unit = "m")+
  tm_raster(palette= "BrBG", style = "fisher", n= 10, midpoint = NA, title = "NDVI")+
  tm_layout(title = "06/25/21", legend.outside = TRUE)+
  tm_compass(type= "arrow", size= 4, position = c("left", "bottom"))+
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, position = c("right", "bottom"))



m0625b2 <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p2/6_25_21_buckthorn_part2_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p2/6_25_21_buckthorn_part2_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p2/6_25_21_buckthorn_part2_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p2/6_25_21_buckthorn_part2_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_buckthorn_p2/6_25_21_buckthorn_part2_transparent_reflectance_nir.tif")

#plotRGB(m0625b2, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0625b2 <- (m0625b2[[5]] - m0625b2[[3]]) / (m0625b2[[5]] + m0625b2[[3]])
#plot(ndvi0625b2)


m0625r <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_reforestation/6_25_21_reforestation_p2_transparent_reflectance_blue.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_reforestation/6_25_21_reforestation_p2_transparent_reflectance_green.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_reforestation/6_25_21_reforestation_p2_transparent_reflectance_red.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_reforestation/6_25_21_reforestation_p2_transparent_reflectance_red edge.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/06_25_21_reforestation/6_25_21_reforestation_p2_transparent_reflectance_nir.tif")
#plotRGB(m0625r, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0625r <- (m0625r[[5]] - m0625r[[3]]) / (m0625r[[5]] + m0625r[[3]])
#plot(ndvi0625r)


m0701b1 <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_blue.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_green.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_red.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_red edge.tif",
            "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_nir.tif")

plotRGB(m0701b1, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")

m0701b1RGB <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p1/rgb/07_01_21_buckthorn_p1_rgb_transparent_reflectance_group1.tif")
plotRGB(m0701b1RGB, r = 1, g = 2, b = 3, stretch = "lin")

tm_shape(m0701b1RGB, bbox = extentB, unit = "m")+
  tm_rgb(r = 1, g = 2, b = 3)+
  tm_layout(title = "07/01/21", title.color = "black", legend.outside = TRUE)+
  tm_compass(type= "arrow", size= 4, position = c("left", "bottom"), text.color = "white")+
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, text.color = "white", 
               position = c("right", "bottom"))+
  tm_shape(removalPoly)+
  tm_fill(alpha = 0, id = "Removal")+
  tm_borders(col = "red",
             lwd = 2,
             lty = "solid",
             alpha = NA,
             zindex = NA,
             group = NA)+
  tm_shape(controlPoly)+
  tm_fill(alpha = 0, title = "Treatment", id = "Control")+
  tm_borders(col = "blue",
             lwd = 2,
             lty = "solid",
             alpha = NA,
             zindex = NA,
             group = NA)+
  tm_add_legend(type = "symbol",
                labels = c("Removal", "Control"),
                col = c("red", "blue"),
                shape = NULL,
                border.col = "black",
                border.lwd = 1,
                border.alpha = NA,
                title = "Treatment",
                is.portrait = TRUE)



ndvi0701b1 <- (m0701b1[[5]] - m0701b1[[3]]) / (m0701b1[[5]] + m0701b1[[3]])
#plot(ndvi0701b1)

tm_shape(ndvi0701b1, bbox = extentB, unit = "m")+
  tm_raster(palette= "BrBG", style = "fisher", n= 10, midpoint = NA, title = "NDVI")+
  tm_layout(title = "07/01/21", legend.outside = TRUE)+
  tm_compass(type= "arrow", size= 4, position = c("left", "bottom"))+
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, position = c("right", "bottom"))



m0701b2 <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p2/07_01_21_buckthorn_p2_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p2/07_01_21_buckthorn_p2_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p2/07_01_21_buckthorn_p2_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p2/07_01_21_buckthorn_p2_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_01_21_buckthorn_p2/07_01_21_buckthorn_p2_transparent_reflectance_nir.tif")

#plotRGB(m0701b2, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0701b2 <- (m0701b2[[5]] - m0701b2[[3]]) / (m0701b2[[5]] + m0701b2[[3]])
#plot(ndvi0701b2)

m0707b <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_07_21_buckthorn/07_07_21_buckthorn_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_07_21_buckthorn/07_07_21_buckthorn_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_07_21_buckthorn/07_07_21_buckthorn_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_07_21_buckthorn/07_07_21_buckthorn_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_07_21_buckthorn/07_07_21_buckthorn_transparent_reflectance_nir.tif")

#plotRGB(m0707b, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0707b <- (m0707b[[5]] - m0707b[[3]]) / (m0707b[[5]] + m0707b[[3]])
#plot(ndvi0707b)

m0712b1 <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_12_21_buckthorn_p1/07_12_21_buckthorn_p1_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_12_21_buckthorn_p1/07_12_21_buckthorn_p1_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_12_21_buckthorn_p1/07_12_21_buckthorn_p1_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_12_21_buckthorn_p1/07_12_21_buckthorn_p1_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_12_21_buckthorn_p1/07_12_21_buckthorn_p1_transparent_reflectance_nir.tif")

#plotRGB(m0712b1, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0712b1 <- (m0712b1[[5]] - m0712b1[[3]]) / (m0712b1[[5]] + m0712b1[[3]])
#plot(ndvi0712b1)


m0712b2 <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_12_21_buckthorn_p2/07_12_21_buckthorn_p2_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_12_21_buckthorn_p2/07_12_21_buckthorn_p2_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_12_21_buckthorn_p2/07_12_21_buckthorn_p2_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_12_21_buckthorn_p2/07_12_21_buckthorn_p2_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_12_21_buckthorn_p2/07_12_21_buckthorn_p2_transparent_reflectance_nir.tif")

#plotRGB(m0712b2, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0712b2 <- (m0712b2[[5]] - m0712b2[[3]]) / (m0712b2[[5]] + m0712b2[[3]])
#plot(ndvi0712b2)

tm_shape(ndvi0712b2, bbox = extentB)+
  tm_raster(palette= "BrBG", style = "fisher", n= 10)+
  tm_layout(legend.outside= TRUE)
  
  


m0715rb <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_15_21_rogers_buckthorn/07_15_21_rogers_buckthorn_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_15_21_rogers_buckthorn/07_15_21_rogers_buckthorn_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_15_21_rogers_buckthorn/07_15_21_rogers_buckthorn_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_15_21_rogers_buckthorn/07_15_21_rogers_buckthorn_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_15_21_rogers_buckthorn/07_15_21_rogers_buckthorn_transparent_reflectance_nir.tif")

#plotRGB(m0715rb, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0715rb <- (m0715rb[[5]] - m0715rb[[3]]) / (m0715rb[[5]] + m0715rb[[3]])
#plot(ndvi0715rb)


m0715rg <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_15_21_rogers_glen/07_15_21_rogers_glen_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_15_21_rogers_glen/07_15_21_rogers_glen_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_15_21_rogers_glen/07_15_21_rogers_glen_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_15_21_rogers_glen/07_15_21_rogers_glen_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_15_21_rogers_glen/07_15_21_rogers_glen_transparent_reflectance_nir.tif")

#plotRGB(m0715rg, r = 3, g = 2, b = 1, scale = 0.5, stretch = "lin")
ndvi0715rg <- (m0715rg[[5]] - m0715rg[[3]]) / (m0715rg[[5]] + m0715rg[[3]])
#plot(ndvi0715rg)

m0719b <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_19_21_buckthorn_p2/07_19_21_buckthorn_p2_transparent_reflectance_blue.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_19_21_buckthorn_p2/07_19_21_buckthorn_p2_transparent_reflectance_green.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_19_21_buckthorn_p2/07_19_21_buckthorn_p2_transparent_reflectance_red.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_19_21_buckthorn_p2/07_19_21_buckthorn_p2_transparent_reflectance_red edge.tif",
                "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_19_21_buckthorn_p2/07_19_21_buckthorn_p2_transparent_reflectance_nir.tif")

ndvi0719b <- (m0719b[[5]] - m0719b[[3]]) / (m0719b[[5]] + m0719b[[3]])

m0719bRGB <- stack("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/07_19_21_buckthorn_p2/rgb/07_19_21_buckthorn_p2_rgb_transparent_reflectance_group1.tif")

#rgb from multi images
tm_shape(m0719b, bbox = extentB, unit = "m")+
  tm_rgb(r = 3, g = 2, b = 1, max.value = 0.5)+
  tm_layout(title = "07/19/21", title.color = "black", legend.outside = TRUE)+
  tm_compass(type= "arrow", size= 4, position = c("left", "bottom"), 
             text.color = "white")+
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, text.color = "white", 
               position = c("right", "bottom"))+
  tm_shape(removalPoly)+
  tm_fill(alpha = 0, id = "Removal")+
  tm_borders(col = "red",
             lwd = 2,
             lty = "solid",
             alpha = NA,
             zindex = NA,
             group = NA)+
  tm_shape(controlPoly)+
  tm_fill(alpha = 0, title = "Treatment", id = "Control")+
  tm_borders(col = "blue",
             lwd = 2,
             lty = "solid",
             alpha = NA,
             zindex = NA,
             group = NA)+
  tm_add_legend(type = "symbol",
                labels = c("Removal", "Control"),
                col = c("red", "blue"),
                shape = NULL,
                border.col = "black",
                border.lwd = 1,
                border.alpha = NA,
                title = "Treatment",
                is.portrait = TRUE)

#ndvi tmap
tm_shape(ndvi0719b, bbox = extentB, unit = "m")+
  tm_raster(palette= "BrBG", style = "fisher", n= 10, midpoint = NA, title = "NDVI")+
  tm_layout(title = "07/19/21", legend.outside = TRUE)+
  tm_compass(type= "arrow", size= 4, position = c("left", "bottom"))+
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, position = c("right", "bottom"))

#rgb tmap
tm_shape(m0719bRGB, bbox = extentB, unit = "m")+
  tm_rgb(r = 1, g = 2, b = 3, max.value = 0.16)+
  tm_layout(title = "07/19/21", title.color = "black", legend.outside = TRUE)+
  tm_compass(type= "arrow", size= 4, position = c("left", "bottom"), text.color = "white")+
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, text.color = "white", 
               position = c("right", "bottom"))+
  tm_shape(removalPoly)+
  tm_fill(alpha = 0, id = "Removal")+
  tm_borders(col = "red",
             lwd = 2,
             lty = "solid",
             alpha = NA,
             zindex = NA,
             group = NA)+
  tm_shape(controlPoly)+
  tm_fill(alpha = 0, title = "Treatment", id = "Control")+
  tm_borders(col = "blue",
             lwd = 2,
             lty = "solid",
             alpha = NA,
             zindex = NA,
             group = NA)+
  tm_add_legend(type = "symbol",
                labels = c("Removal", "Control"),
                col = c("red", "blue"),
                shape = NULL,
                border.col = "black",
                border.lwd = 1,
                border.alpha = NA,
                title = "Treatment",
                is.portrait = TRUE)




#adding gps coordinates to maps
#read data for longitude and latitude
sensort <- read.csv("K:/Environmental_Studies/hkropp/Data/campus/buckthorn/sapflux/sensors_meta.csv")
sensorc <- st_as_sf(sensort, coords = c("Longitude", "Latitude"), 
                    crs = 4326)
#plots points without map
#plot(sensorc$geometry)
#transforms sensorc so that points are in utm zone 18N wgs 84
sensorInfo <- st_transform(sensorc, crs = 32618)
#add tree coordinates to ndvi map of 0707b
#plot(ndvi0707b)
#plot(sensorInfo$geometry, add = TRUE, pch = 19)
#zoomed out view of plot
extentB <- extent(466520, 466610, 4767390, 4767480)
#zoomed in view of plot
extentS <- extent(466535, 466600, 4767390, 4767430)
#extentB view of 0503RGB with tree coords
m0503RGBc <- crop(m0503bRGB, extentB)
plotRGB(m0503RGBc, r = 3, g = 2, b = 1)
#plot(sensorInfo$geometry, add = TRUE, pch = 19)

#comparison with old coords
sensormeta <- read.csv("K:/Environmental_Studies/hkropp/Data/campus/buckthorn/sapflux/sensors_meta.csv")
sensorc2 <- st_as_sf(sensormeta, coords = c("Longitude", "Latitude"), 
                     crs = 4326)
#plot(sensorc2$geometry)
sensorInfo2 <- st_transform(sensorc2, crs = 32618)
#plot(ndvi0707b)
#plot(sensorInfo2$geometry, add = TRUE, pch = 19)

#install.packages(c("mapview", "mapedit"))
library(mapview)
library(mapedit)
#changes view of map so that the max number of pixels is 5000000
#viewRGB(m0503RGBc, r = 3, g = 2, b = 1, maxpixels = 5000000)+
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
#plot(rmbox)
#why do we plot rmbox instead of removalp?
viewRGB(m0503RGBc, r = 3, g = 2, b = 1, maxpixels = 5000000)+
  mapview(removalBox)
#plotRGB(m0503RGBc, r = 3, g = 2, b = 1)
#plot(removalp$geometry, add = TRUE)

#drew manually the bounds of removal plot because of inaccuracies for removal plot above
#removalPoly <- drawFeatures(
#viewRGB(m0503RGBc, r = 3, g = 2, b = 1)+
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
ashPoly <- st_transform(st_read("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/out/ash_canopies.shp"), 32618)
maplePoly <- st_transform(st_read("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/out/ash_canopies.shp"), 32618)
buckPoly <- st_transform(st_read("K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/out/ash_canopies.shp"), 32618)



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

ndvi0701b1R <- resample(ndvi0701b1, ndvi0707b)
plot(ndvi0701b1R - ndvi0707b)
#resampling where 0503b is the base map
ndvi0707bR <- resample(ndvi0707b, ndvi0503b)
plot(ndvi0707bR - ndvi0503b)

ndvi0618b1R <- resample(ndvi0618b1, ndvi0503b)
plot(ndvi0618b1R - ndvi0503b)

#resampling where 0712 is the base map for 0707
ndvi0707bR2 <- resample(ndvi0707b, ndvi0712b1)
plot(ndvi0707bR2 - ndvi0712b1)


#resampling where 0712 is base map for 0701
ndvi0701b1R2 <- resample(ndvi0701b1, ndvi0712b1)
plot(ndvi0707bR2 - ndvi0712b1)


#resampling where 0712 is base map for 0625
ndvi0625bR2 <- resample(ndvi0625b1, ndvi0712b1)
plot(ndvi0625bR2 - ndvi0712b1)


#resampling where 0712 is base map for 0618
ndvi0618bR2 <- resample(ndvi0618b1, ndvi0712b1)
plot(ndvi0618bR2 - ndvi0712b1)


#resampling where 0712 is base map for 0610
ndvi0610bR2 <- resample(ndvi0610b, ndvi0712b1)
plot(ndvi0610bR2 - ndvi0712b1)


#resampling where 0712 is base map for 0607
ndvi0607bR2 <- resample(ndvi0607b, ndvi0712b1)
plot(ndvi0607bR2 - ndvi0712b1)


#resampling where 0712 is base map for 0519
ndvi0519bR2 <- resample(ndvi0519b, ndvi0712b1)
plot(ndvi0519bR2 - ndvi0712b1)


#resampling where 0712 is base map for 0503
ndvi0503bR2 <- resample(ndvi0503b, ndvi0712b1)
plot(ndvi0503bR2 - ndvi0712b1)




#ndvi value dist from 0503b
rm0503b <- extract(ndvi0503b, removalPoly)[[1]]
meanrm0503b <- mean(rm0503b)
#ndvi value dist from 0625b2
ctr0503b <- extract(ndvi0503b, controlPoly)[[1]]
meanctr0503b <- mean(ctr0503b)
hist(ctr0503b)

#ndvi value dist from 0519b
rm0519b <- extract(ndvi0519b, removalPoly)[[1]]
meanrm0519b <- mean(rm0519b, na.rm = TRUE)
#ndvi value dist from 0519b
ctr0519b <- extract(ndvi0519b, controlPoly)[[1]]
meanctr0519b <- mean(ctr0519b, na.rm = TRUE)


#ndvi value dist from 0607b
rm0607b <- extract(ndvi0607b, removalPoly)[[1]]
meanrm0607b <- mean(rm0607b)
#ndvi value dist from 0607b
ctr0607b <- extract(ndvi0607b, controlPoly)[[1]]
meanctr0607b <- mean(ctr0607b)


#ndvi value dist from 0610b
rm0610b <- extract(ndvi0610b, removalPoly)[[1]]
meanrm0610b <- mean(rm0610b)
#ndvi value dist from 0625b2
ctr0610b <- extract(ndvi0610b, controlPoly)[[1]]
meanctr0610b <- mean(ctr0610b)


#histogram for ndvi value distribution from 0618b1 removal
rm0618b1 <- extract(ndvi0618b1, removalPoly)[[1]]
hist(rm0618b1)
meanrm0618b1 <- mean(rm0618b1)
#histogram for ndvi value distribution from 0618b control
ctr0618b1 <- extract(ndvi0618b1, controlPoly)[[1]]
hist(ctr0618b1)
meanctr0618b1 <- mean(ctr0618b1)

#histogram for ndvi value distribution from 0618b2 removal
rm0618b2 <- extract(ndvi0618b2, removalPoly)[[1]]

meanrm0618b2 <- mean(rm0618b2)

#histogram for ndvi value distribution from 0618b2 control
ctr0618b2 <- extract(ndvi0618b2, controlPoly)[[1]]

meanctr0618b2 <- mean(ctr0618b2)

#histogram of ndvi values for 0625r
hist(ndvi0625r)

#ndvi value distribution from 0625b1
rm0625b1 <- extract(ndvi0625b1, removalPoly)[[1]]
meanrm0625b1 <- mean(rm0625b1)
#ndvi value dist from 0625b1
ctr0625b1 <- extract(ndvi0625b1, controlPoly)[[1]]
meanctr0625b1 <- mean(ctr0625b1)


#ndvi value dist from 0625b2
rm0625b2 <- extract(ndvi0625b2, removalPoly)[[1]]
meanrm0625b2 <- mean(rm0625b2)
#ndvi value dist from 0625b2
ctr0625b2 <- extract(ndvi0625b2, controlPoly)[[1]]
meanctr0625b2 <- mean(ctr0625b2)


#ndvi value dist from 0701b1
rm0701b1 <- extract(ndvi0701b1, removalPoly)[[1]]
meanrm0701b1 <- mean(rm0701b1)
#ndvi value dist from 0701b1
ctr0701b1 <- extract(ndvi0701b1, controlPoly)[[1]]
meanctr0701b1 <- mean(ctr0701b1)


#ndvi value dist from 0701b2
rm0701b2 <- extract(ndvi0701b2, removalPoly)[[1]]
meanrm0701b2 <- mean(rm0701b2)
#ndvi value dist from 0625b2
ctr0701b2 <- extract(ndvi0701b2, controlPoly)[[1]]
meanctr0701b2 <- mean(ctr0701b2)


#ndvi value dist from 0707b
rm0707b <- extract(ndvi0707b, removalPoly)[[1]]
meanrm0707b <- mean(rm0707b)
#ndvi value dist from 0707b
ctr0707b <- extract(ndvi0707b, controlPoly)[[1]]
meanctr0707b <- mean(ctr0707b)


#ndvi value dist from 0712b1
rm0712b1 <- extract(ndvi0712b1, removalPoly)[[1]]
meanrm0712b1 <- mean(rm0712b1)
#ndvi value dist from 0712b1
ctr0712b1 <- extract(ndvi0712b1, controlPoly)[[1]]
meanctr0712b1 <- mean(ctr0712b1)


#ndvi value dist from 0712b2
rm0712b2 <- extract(ndvi0712b2, removalPoly)[[1]]
meanrm0712b2 <- mean(rm0712b2, na.rm = TRUE)
#ndvi value dist from 0712b2
ctr0712b2 <- extract(ndvi0712b2, controlPoly)[[1]]
meanctr0712b2 <- mean(ctr0712b2)

#ndvi by treatment for 07-19-21 b2
rm0719b2 <- extract(ndvi0719b, removalPoly)[[1]]
meanrm0719b2 <- mean(rm0719b2, na.rm = TRUE)

ctr0719b2 <- extract(ndvi0719b, controlPoly)[[1]]
meanctr0712b2 <- mean(ctr0719b2)

#adds ndvi values from removal and control into a dataframe

dfcontrol0503b <- data.frame(ndvi = ctr0503b, 
                             trt = rep("ctr", length(ctr0503b)),
                             date = rep("05/03/21", length(ctr0503b)))

dfremoval0503b <- data.frame(ndvi = rm0503b, 
                             trt = rep("rm", length(rm0503b)),
                             date = rep("05/03/21", length(rm0503b)))

dfcontrol0519b <- data.frame(ndvi = ctr0519b, 
                             trt = rep("ctr", length(ctr0519b)),
                             date = rep("05/19/21", length(ctr0519b)))

dfremoval0519b <- data.frame(ndvi = rm0519b, 
                             trt = rep("rm", length(rm0519b)),
                             date = rep("05/19/21", length(rm0519b)))

dfcontrol0607b <- data.frame(ndvi = ctr0607b, 
                              trt = rep("ctr", length(ctr0607b)),
                              date = rep("06/07/21", length(ctr0607b)))

dfremoval0607b <- data.frame(ndvi = rm0607b, 
                              trt = rep("rm", length(rm0607b)),
                              date = rep("06/07/21", length(rm0607b)))

dfcontrol0610b <- data.frame(ndvi = ctr0610b, 
                              trt = rep("ctr", length(ctr0610b)),
                              date = rep("06/10/21", length(ctr0610b)))

dfremoval0610b <- data.frame(ndvi = rm0610b, 
                              trt = rep("rm", length(rm0610b)),
                              date = rep("06/10/21", length(rm0610b)))

dfcontrol0618b1 <- data.frame(ndvi = ctr0618b1, 
                              trt = rep("ctr", length(ctr0618b1)),
                              date = rep("06/18/21 p1", length(ctr0618b1)))

dfremoval0618b1 <- data.frame(ndvi = rm0618b1, 
                              trt = rep("rm", length(rm0618b1)),
                              date = rep("06/18/21 p1", length(rm0618b1)))

dfcontrol0618b2 <- data.frame(ndvi = ctr0618b2, 
                              trt = rep("ctr", length(ctr0618b2)),
                              date = rep("06/18/21 p2", length(ctr0618b2)))

dfremoval0618b2 <- data.frame(ndvi = rm0618b2, 
                              trt = rep("rm", length(rm0618b2)),
                              date = rep("06/18/21 p2", length(rm0618b2)))

dfcontrol0625b1 <- data.frame(ndvi = ctr0625b1, 
                              trt = rep("ctr", length(ctr0625b1)),
                              date = rep("06/25/21 p1", length(ctr0625b1)))

dfremoval0625b1 <- data.frame(ndvi = rm0625b1, 
                              trt = rep("rm", length(rm0625b1)),
                              date = rep("06/25/21 p1", length(rm0625b1)))

dfcontrol0625b2 <- data.frame(ndvi = ctr0625b2, 
                              trt = rep("ctr", length(ctr0625b2)),
                              date = rep("06/25/21 p2", length(ctr0625b2)))

dfremoval0625b2 <- data.frame(ndvi = rm0625b2, 
                              trt = rep("rm", length(rm0625b2)),
                              date = rep("06/25/21 p2", length(rm0625b2)))

dfcontrol0701b1 <- data.frame(ndvi = ctr0701b1, 
                              trt = rep("ctr", length(ctr0701b1)),
                              date = rep("07/01/21 p1", length(ctr0701b1)))

dfremoval0701b1 <- data.frame(ndvi = rm0701b1, 
                              trt = rep("rm", length(rm0701b1)),
                              date = rep("07/01/21 p1", length(rm0701b1)))
dfcontrol0701b2 <- data.frame(ndvi = ctr0701b2, 
                              trt = rep("ctr", length(ctr0701b2)),
                              date = rep("07/01/21 p2", length(ctr0701b2)))

dfremoval0701b2 <- data.frame(ndvi = rm0701b2, 
                              trt = rep("rm", length(rm0701b2)),
                              date = rep("07/01/21 p2", length(rm0701b2)))


dfcontrol0707b <- data.frame(ndvi = ctr0707b, 
                              trt = rep("ctr", length(ctr0707b)),
                              date = rep("07/07/21", length(ctr0707b)))

dfremoval0707b <- data.frame(ndvi = rm0707b, 
                              trt = rep("rm", length(rm0707b)),
                              date = rep("07/07/21", length(rm0707b)))

dfcontrol0712b1 <- data.frame(ndvi = ctr0712b1, 
                              trt = rep("ctr", length(ctr0712b1)),
                              date = rep("07/12/21 p1", length(ctr0712b1)))

dfremoval0712b1 <- data.frame(ndvi = rm0712b1, 
                              trt = rep("rm", length(rm0712b1)),
                              date = rep("07/12/21 p1", length(rm0712b1)))



dfcontrol0712b2 <- data.frame(ndvi = ctr0712b2, 
                              trt = rep("ctr", length(ctr0712b2)),
                              date = rep("07/12/21 p2", length(ctr0712b2)))

dfremoval0712b2 <- data.frame(ndvi = rm0712b2, 
                              trt = rep("rm", length(rm0712b2)),
                              date = rep("07/12/21 p2", length(rm0712b2)))

dfcontrol0719b <- data.frame(ndvi = ctr0719b2, 
                              trt = rep("ctr", length(ctr0719b2)),
                              date = rep("07/19/21", length(ctr0719b2)))

dfremoval0719b <- data.frame(ndvi = rm0719b2, 
                              trt = rep("rm", length(rm0719b2)),
                              date = rep("07/19/21", length(rm0719b2)))

ndvidfall <- rbind(dfremoval0503b, dfcontrol0503b, dfremoval0519b, 
                   dfcontrol0519b, dfremoval0607b, dfcontrol0607b, dfremoval0610b, 
                   dfcontrol0610b, dfremoval0618b1, dfcontrol0618b1, 
                   dfremoval0618b2, dfcontrol0618b2, dfremoval0625b1, dfcontrol0625b1, 
                   dfremoval0625b2, dfcontrol0625b2, dfremoval0701b1, 
                   dfcontrol0701b1, dfremoval0701b2, dfcontrol0701b2, 
                   dfcontrol0707b, dfremoval0707b, dfremoval0712b1,
                   dfcontrol0712b1, dfcontrol0712b2, 
                   dfremoval0712b2, dfcontrol0719b, dfremoval0719b)

library(ggplot2)
dodge <- position_dodge(width = 1)
ggplot(ndvidfall, aes(date, ndvi, fill = trt))+
  geom_violin(position = dodge)+
  geom_boxplot(width = 0.1, position = dodge)
  
ndvidfsome <- rbind(dfremoval0503b, dfcontrol0503b, dfremoval0610b, 
                   dfcontrol0610b, dfremoval0618b1, dfcontrol0618b1, 
                   dfremoval0618b2, dfcontrol0618b2, dfremoval0625b1, dfcontrol0625b1, 
                   dfremoval0625b2, dfcontrol0625b2, dfremoval0701b1, 
                   dfcontrol0701b1, dfremoval0701b2, dfcontrol0701b2, 
                   dfcontrol0719b, dfremoval0719b)

ggplot(ndvidfsome, aes(date, ndvi, fill = trt))+
  geom_violin(position = dodge)+
  geom_boxplot(width = 0.1, position = dodge)

#adds all mean values of removal and control into a table
mean.table <- data.frame(meanndvi = c(meanctr0503b, meanrm0503b, meanctr0519b, meanrm0519b,meanctr0607b, meanrm0607b,
                                       meanctr0610b, meanrm0610b, meanctr0618b1, meanrm0618b1, meanctr0618b2, meanrm0618b2,
                                       meanctr0625b1, meanrm0625b1, meanctr0625b2, meanrm0625b2, meanctr0701b1, meanrm0701b1,
                                       meanctr0707b, meanrm0707b, meanctr0712b2, meanrm0712b2, meanctr0712b1, meanrm0712b1), 
                         trt = rep(c("ctr", "rm"), times = 12), 
                         date = as.Date(rep(c("05/03/21", "05/19/21", "06/07/21", "06/10/21", 
                                      "06/18/21", "06/18/21", "06/25/21", "06/25/21", 
                                      "07/01/21", "07/07/21", "07/12/21", "07/12/21"), each = 2), "%m/%d/%y"))


ggplot(mean.table, aes(x = as.factor(date), meanndvi, color = trt))+
  geom_point()


ggplot(mean.table[5:24,], aes(x = as.factor(date), meanndvi, color = trt, size = 4))+
  geom_point()
#these last few lines of code aren't necessary

meanctr0503bdat <- data.frame(meanctr0503b)
meanrm0503bdat <- data.frame(meanrm0503b)

meanctr0519bdat <- data.frame(meanctr0519b)
meanrm0519bdat <- data.frame(meanrm0519b)

meanctr0607bdat <- data.frame(meanctr0607b)
meanrm0607bdat <- data.frame(meanrm0607b)

meanctr0610bdat <- data.frame(meanctr0610b)
meanrm0610bdat <- data.frame(meanrm0610b)

meanctr0618b1dat <- data.frame(meanctr0618b1)
meanrm0618b1dat <- data.frame(meanrm0618b1)

meanctr0618b2dat <- data.frame(meanctr0618b2)
meanrm0618b2dat <- data.frame(meanrm0618b2)

meanctr0625b1dat <- data.frame(meanctr0625b1)
meanrm0625b1dat <- data.frame(meanrm0625b1)

meanctr0625b2dat <- data.frame(meanctr0625b2)
meanrm0625b2dat <- data.frame(meanrm0625b2)

#mean0625rdat <- data.frame(mean0625r)

meanctr0701b1dat <- data.frame(meanctr0701b1)
meanrm0701b1dat <- data.frame(meanrm0701b1)

meanctr0701b2dat <- data.frame(meanctr0701b2)
meanrm0701b2dat <- data.frame(meanrm0701b2)

meanctr0707bdat <- data.frame(meanctr0707b)
meanrm0707bdat <- data.frame(meanrm0707b)

meanctr0712b1dat <- data.frame(meanctr0712b1)
meanrm0712b1dat <- data.frame(meanrm0712b1)

meanctr0712b2dat <- data.frame(meanctr0712b2)
meanrm0712b2dat <- data.frame(meanrm0712b2)

#combining all means to a single table
meanrmctrDat <- (rbind(meanctr0503b, meanrm0503b, meanctr0519b, meanrm0519b,meanctr0607b, meanrm0607b,
                       meanctr0610b, meanrm0610b, meanctr0618b1, meanrm0618b1, meanctr0618b2, meanrm0618b2,
                       meanctr0625b1, meanrm0625b1, meanctr0625b2, meanrm0625b2, meanctr0701b1, meanrm0701b1,
                       meanctr0707b, meanrm0707b, meanctr0712b2, meanrm0712b2, meanctr0712b1, meanrm0712b1))


#drew rough plot of where trees were planted for reforestation
#reforestbox <- drawFeatures(
  viewRGB(m0625r, r = 3, g = 2, b = 1)
#st_write(reforestbox, "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M/out/reforest_bounds.shp")
rf0625 <- extract(ndvi0625r, reforestbox)[[1]]
hist(rf0625)
mean(rf0625)

sensorcoor <- st_coordinates(sensorInfo[c(1,14), ])

sensorcoor[1,2] + (sin(71*(pi / 180)) * 11.7)

sensorcoor[1,1] - (cos(71*(pi / 180)) * 11.7)

sensor7.9 <- data.frame(sensorID=7, 
                        x = sensorcoor[1,1] - (cos(71*(pi / 180)) * 11.7),
                        y = sensorcoor[1,2] + (sin(71*(pi / 180)) * 11.7))

sensor5 <- data.frame(sensorID=5, 
                      x = sensorcoor[2,1] + (cos(8*(pi / 180)) * 13.8),
                      y = sensorcoor[2,2] + (sin(8*(pi / 180)) * 13.8))

sensor3.4 <- data.frame(sensorID=3, 
                        x = sensorcoor[1,1] - (cos(81*(pi / 180)) * 10.5),
                        y = sensorcoor[1,2] + (sin(81*(pi / 180)) * 10.5))

sensorcombined <- st_as_sf(rbind(sensor7.9, sensor5, sensor3.4), coords = c("x", "y"), crs = 32618)

plotRGB(m0503RGBc, r = 3, g = 2, b = 1)
plot(sensorInfo$geometry[c(1,14)], add = TRUE, pch = 19, col = "red")
plot(sensorcombined$geometry, add = TRUE, pch = 19)


