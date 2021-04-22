library(raster)
library(tidyverse)
library(mapview)
library(ncdf4)
library(sf)
library(spdep)
library(tmap)
library(spatstat)
library(ggmap)
library(mapproj)
library(rgeoda)
library(ape)
library(elsa)
library(geosphere)
library(parallelDist)
library(ozmaps)
library(leaflet)
library(ncdf4)

r <- brick("2007.daily_rain.nc")
r <- raster("2007.daily_rain.nc")
data <- read_csv("cleaned_data.csv")
r <- read_ncdf("2007.daily_rain.nc")

decade_spatial <- read_csv("Tot_rainfall_spatial.csv",col_types = cols(X1 = col_integer(),
                                                                       Longitude = col_double(),
                                                                    Latitude = col_double(),
                                                                       Rainfall = col_double()))
plot(decade_r_poly_df)  
decade_r <- rasterFromXYZ(decade_spatial[,2:4])
decade_r_poly_df <- rasterToPolygons(decade_r)
decade_sf <- st_as_sf(decade_r_poly_df)
decade_sf$Rainfall

plot(st_geometry(decade_sf))

decade_sf
ozmap_states
sf::plot_sf(ozmap_states)
# Map of Oz states
ggplot() +
  geom_sf(data=ozmap_states,aes(fill = NAME)) +

scale_fill_manual(values = viridis::cividis(9))




# Spatial Aggregation to form a choropleth map

# Setting the same coordinate system for both of sf objects
st_crs(ozmap_states) == st_crs(decade_sf)
st_crs(decade_sf)
st_crs(ozmap_states)
st_crs(decade_sf) <- 4283 

?st_crs()
ozmap_states %>% 
  mutate(state = st_area(geometry)) %>% 
  head()

j<- ozmap_states %>% 
  mutate(area = st_area(geometry)) %>% 
  st_join(decade_sf) 
  
ggplot() +
  geom_sf(data=j,aes(fill = NAME))

st_cast(decade_sf, to = "MULTIPOLYGON")
decade_sf$geometry[1] %in% ozmap_states$geometry[9]
no <- st_geometry(ozmap_states)
nd <- st_geometry(decade_sf)
nd[[1]] 




# Density of Spatial Raster Data
density(decade_r,main="Density of Spatial Raster Rainfall Data")
?density
# Histogram of Spatial Raster Data
hist(decade_r,main="Histogram of Spatial Raster Rainfall Data")

# Moran's I Coefficient
MI_decade_r <- Moran(decade_r)
# Geary's Coefficient
Geary_decade_r <- Geary(decade_r)
MI_decade_r
Geary_decade_r

paste("Moran's I Coefficient: ",MI_decade_r)
spat_corr <- data.frame(Morans_I =MI_decade_r, Geary_C = Geary_decade_r)
spat_corr
ggplot(spat_corr) + geom_histogram()

plot(r[,,,1])
filled.contour(r)

mapview(r[,,,1])

r[,,,1]
names(r)
r$daily_rain
r[[1]]


st_crs(r)
# Bounding boxes
st_bbox(r)
# Resoultion
st_dimensions(r)$lon$delta

r[[1]][,,365]
r[[1]][,,1:5]




decade_r_poly_df <- rasterToPolygons(decade_r)
decade_r_poly_df

tm_shape(decade_r_poly_df) + tm_polygons(style="quantile", col = "Rainfall") +
  tm_legend(outside = TRUE, text.size = .8)

nb <- poly2nb(decade_r_poly_df, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
print(lw, zero.policy=TRUE)
# Moran's I Hypothesis Test
plot(moran.test(decade_r_poly_df$Rainfall,lw,zero.policy = TRUE))
#Monte Carlo Simulation of Moran's I
mc <- moran.mc(decade_r_poly_df$Rainfall, lw, nsim=1000,zero.policy = TRUE)
mc
plot(mc, main="Monte Carlo Simulation of Moran's I", las=1)

# Moran's I value of 0.99 suggesting high clustering therefore high correlation between Rainfall and Location
plot(decade_r_poly_df$Rainfall)

# Clustering 
decade_spatial$Rainfall <- replace_na(decade_spatial$Rainfall,0)
d <- decade_spatial[! is.na(decade_spatial$Rainfall),]
d <- subset(d , select = -X1)
kmeans(d[,1:3],centers =7)$cluster
plot(d[,c("Longitude","Latitude")], col = kmeans(d,centers=6)$cluster)
ggplot(d,aes(x=Longitude,y=Latitude,color = kmeans(d,centers=6)$cluster)) + geom_point() +
  scale_color_distiller(type="seq",palette = "Blues") +
  theme(panel.background = element_blank(),panel.border = element_blank(),axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title="Spatial Clustering using KMeans on rainfall",color = "Cluster")
  
as.vector(brewer.pal(6,"Blues"))

# Data Availability of Spatial data
d <- decade_spatial[! is.na(decade_spatial$Rainfall),]
d <- subset(d , select = -X1)
d$Grid_Data_Availability <- is.na(d$Rainfall)
d$Grid_Data_Availability <- case_when(
  
  d$Grid_Data_Availability == TRUE ~ "Missing Spatial Data",
  d$Grid_Data_Availability == FALSE ~ "Available Spatial Data"
)
ggplot(d,aes(x=Longitude,y=Latitude,fill = Grid_Data_Availability)) + geom_point(color = "orange") + theme_bw() +
  labs(title="Scatter Plot of Available Spatial Data",subtitle="Notice how data is only available in locations inside the country")

