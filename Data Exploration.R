library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(leaflet)
library(scales)
library(gridExtra)
library(hrbrthemes)
library(GGally)
library(viridis)
library(sp)
library(fields)
library(skimr)
library(forcats)
library(ozmaps)

data <- read_csv("cleaned_data.csv")
decade_spatial <- read_csv("Tot_rainfall_spatial.csv",col_types = cols(X1 = col_integer(),
                                                                       Longitude = col_double(),
                                                                       Latitude = col_double(),
                                                                      Rainfall = col_double()))
jan_spatial <- read_csv("Overall_Jan_Rainfall.csv",col_types = cols(X1 = col_integer(),
                                                                    Longitude = col_double(),
                                                                    Latitude = col_double(),
                                                                    Rainfall = col_double()))
feb_spatial <- read_csv("Overall_Feb_Rainfall.csv",col_types = cols(X1 = col_integer(),
                                                                    Longitude = col_double(),
                                                                    Latitude = col_double(),
                                                                    Rainfall = col_double()))
mar_spatial <- read_csv("Overall_Mar_Rainfall.csv",col_types = cols(X1 = col_integer(),
                                                                    Longitude = col_double(),
                                                                    Latitude = col_double(),
                                                                    Rainfall = col_double()))
apr_spatial <- read_csv("Overall_Apr_Rainfall.csv",col_types = cols(X1 = col_integer(),
                                                                    Longitude = col_double(),
                                                                    Latitude = col_double(),
                                                                    Rainfall = col_double()))
may_spatial <- read_csv("Overall_May_Rainfall.csv",col_types = cols(X1 = col_integer(),
                                                                    Longitude = col_double(),
                                                                    Latitude = col_double(),
                                                                    Rainfall = col_double()))
jun_spatial <- read_csv("Overall_Jun_Rainfall.csv",col_types = cols(X1 = col_integer(),
                                                                    Longitude = col_double(),
                                                                    Latitude = col_double(),
                                                                    Rainfall = col_double()))
jul_spatial <- read_csv("Overall_Jul_Rainfall.csv",col_types = cols(X1 = col_integer(),
                                                                    Longitude = col_double(),
                                                                    Latitude = col_double(),
                                                                    Rainfall = col_double()))
aug_spatial <- read_csv("Overall_Aug_Rainfall.csv",col_types = cols(X1 = col_integer(),
                                                                    Longitude = col_double(),
                                                                    Latitude = col_double(),
                                                                    Rainfall = col_double()))
sep_spatial <- read_csv("Overall_Sep_Rainfall.csv",col_types = cols(X1 = col_integer(),
                                                                    Longitude = col_double(),
                                                                    Latitude = col_double(),
                                                                    Rainfall = col_double()))
oct_spatial <- read_csv("Overall_Oct_Rainfall.csv",col_types = cols(X1 = col_integer(),
                                                                    Longitude = col_double(),
                                                                    Latitude = col_double(),
                                                                    Rainfall = col_double()))
nov_spatial <- read_csv("Overall_Nov_Rainfall.csv",col_types = cols(X1 = col_integer(),
                                                                    Longitude = col_double(),
                                                                    Latitude = col_double(),
                                                                    Rainfall = col_double()))
dec_spatial <- read_csv("Overall_Dec_Rainfall.csv",col_types = cols(X1 = col_integer(),
                                                                    Longitude = col_double(),
                                                                    Latitude = col_double(),
                                                                    Rainfall = col_double()))


data$Season

location_wise_rainfall <- data %>% group_by(data$Location) %>% summarise(sum(Rainfall)) %>% dplyr::rename(Location = 'data$Location',Total_Rainfall = 'sum(Rainfall)')

#Location-wise rainfall barchart
location_wise_rainfall %>% mutate(Location = fct_reorder(Location,Total_Rainfall)) %>%
ggplot(aes(x=Location,y=Total_Rainfall)) + 
  geom_col(fill = "#92C5DE",color="black",width = 0.5) +
  labs(title="Location-wise total rainfall") + theme_bw() +
  theme(axis.text.x = element_text(angle = 45))
summary(decade_spatial)
# Contour map of overall rainfall in the decade
filled.contour(tbl_2017$dims$lon,tbl_2017$dims$lat,tot_rainfall,axes=FALSE,
               plot.title = title(main = "Isarithmic(Contour) plot of rainfall in Australia over 2007-2017"),
               color.palette = function(n) hcl.colors(n, "Blues 3", rev = TRUE),
                key.title = title(main="Rainfall\n(mm)"),
                key.axes = axis(4, seq(1240, 62991, by = 4750)))

shp <- raster::getData(country='AUS', level=1)

filled.contour(tbl_2017$dims$lon,tbl_2017$dims$lat,tot_rainfall,axes=FALSE,
               plot.title = title(main = "Isarithmic(Contour) plot of rainfall in Australia over 2007-2017"),
               color.palette = function(n) hcl.colors(n, "Blues 3", rev = TRUE),
               key.title = title(main="Rainfall\n(mm)"),
               key.axes = axis(4, seq(1240, 62991, by = 4750)))

image(tbl_2017$dims$lon,tbl_2017$dims$lat,tot_rainfall,axes=FALSE,
      col = hcl.colors(6, "Blues 3", rev = TRUE),
      plot.title = title(main = "Isarithmic(Contour) plot of rainfall in Australia over 2007-2017"),
    )
plot(shp,add=TRUE)


#Daily rainfall over decade
ggplot(data,aes(x=data$Date,y=data$Rainfall,color=data$Season)) +
  geom_point() +
  theme_bw()

#Seasonal monthly rainfall variable
monthly_rainfall <- data %>% group_by(format(Date,format = "%m")) %>% 
  summarise(sum(Rainfall)) %>%
  dplyr::rename(Total_Rainfall = 'sum(Rainfall)',Month = 'format(Date, format = "%m")')

#Gradual monthly rainfall variable
gradual_monthly_rainfall <- data %>% group_by(format(Date,format = "%m-%y")) %>% 
  summarise(sum(Rainfall)) %>%
  dplyr::rename(Total_Rainfall = 'sum(Rainfall)',Month = 'format(Date, format = "%m-%y")')

#Seasonal monthly rainfall
ggplot(monthly_rainfall,aes(x=as.numeric(Month),y=Total_Rainfall)) +
  geom_segment( aes(x=as.numeric(Month), xend=as.numeric(Month), y=0, yend=Total_Rainfall), color="grey") +
  geom_point( color="orange", size=4) +
  geom_smooth() + 
  geom_rug() +
  labs(title = "Seasonal monthly rainfall",
       subtitle = "2007 to 2017",
       x = "Month of Year",
       y = "Total Rainfall") +
  scale_x_continuous(breaks = seq(0, 12, 1)) +
  scale_y_continuous(limits = c(0,50000)) +
  theme(panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_continuous(breaks = 1:12,labels=as.vector(month.abb))


?scale_y_continuous
   
filled.contour(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_jan)
filled.contour(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_feb)
filled.contour(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_mar)
filled.contour(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_apr)
filled.contour(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_may)
filled.contour(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_jun)
filled.contour(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_jul)
filled.contour(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_aug)
filled.contour(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_sep)
filled.contour(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_oct)
filled.contour(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_nov)
filled.contour(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_dec)

par(mfrow = c(2, 3))
image.plot(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_jan,axes=FALSE,xlab="January",ylab = "",col = hcl.colors(7, "Blues 3", rev = TRUE),horizontal=TRUE,main=title("Hi"))
image.plot(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_feb,axes=FALSE,xlab="February",ylab = "",col = hcl.colors(7, "Blues 3", rev = TRUE),horizontal=TRUE)
image.plot(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_mar,axes=FALSE,xlab="March",ylab = "",col = hcl.colors(7, "Blues 3", rev = TRUE),horizontal=TRUE)
image.plot(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_apr,axes=FALSE,xlab="April",ylab = "",col = hcl.colors(7, "Blues 3", rev = TRUE),horizontal=TRUE)
image.plot(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_may,axes=FALSE,xlab="May",ylab = "",col = hcl.colors(7, "Blues 3", rev = TRUE),horizontal=TRUE)
image.plot(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_jun,axes=FALSE,xlab="June",ylab = "",col = hcl.colors(7, "Blues 3", rev = TRUE))
par(mfrow = c(2, 3))
image.plot(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_jul,axes=FALSE,xlab="July",ylab = "",col = hcl.colors(7, "Blues 3", rev = TRUE),horizontal=TRUE)
image.plot(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_aug,axes=FALSE,xlab="August",ylab = "",col = hcl.colors(7, "Blues 3", rev = TRUE),horizontal=TRUE)
image.plot(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_sep,axes=FALSE,xlab="September",ylab = "",col = hcl.colors(7, "Blues 3", rev = TRUE),horizontal=TRUE)
image.plot(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_oct,axes=FALSE,xlab="October",ylab = "",col = hcl.colors(7, "Blues 3", rev = TRUE),horizontal=TRUE)
image.plot(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_nov,axes=FALSE,xlab="Novembor",ylab = "",col = hcl.colors(7, "Blues 3", rev = TRUE),horizontal=TRUE)
image.plot(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_dec,axes=FALSE,xlab="December",ylab = "",col = hcl.colors(7, "Blues 3", rev = TRUE),horizontal=TRUE)

# Seasons effect on rainfall
seasons_tot_rainfall <- data %>% group_by(Season) %>% summarise(sum(Rainfall)) %>% dplyr::rename(Total_Rainfall = 'sum(Rainfall)')

seasons_tot_rainfall %>% mutate(Season = fct_relevel(Season,"summer","autumn","winter","spring")) %>%
ggplot(aes(x=Season,y=Total_Rainfall)) +
  geom_bar(stat = "identity",color = "black",fill=c("#92C5DE","#F4A582","#CA0020","#0571B0"),width = 0.5) +
  theme_bw() +
  labs(title="Variation of rainfall over seasons in a year over the decade")

seasons_tot_rainfall %>% mutate(Season = fct_reorder(Season,desc(Total_Rainfall)))
seasons_tot_rainfall %>% mutate(Season = fct_reorder(Season,desc(Total_Rainfall))) %>%
ggplot(aes(x=c(2,4,1,3),y=Total_Rainfall)) +
  geom_segment( aes(x=c(2,4,1,3), xend=c(2,4,1,3), y=0, yend=Total_Rainfall), color="grey") +
  geom_point( color="orange", size=4) +
  geom_rug() +
  geom_smooth() + 
  labs(title="Variation of rainfall over seasons in a year over the decade",x ="Season") +
  scale_x_continuous(breaks=1:4,labels = c("Summer","Autumn","Winter","Spring")) +
  theme(panel.background = element_blank(),
    plot.background = element_rect(size = 1),
    axis.text.x = element_text(),
    panel.border = element_rect(colour = "black", fill=NA, size=1))



#Gradual monthly rainfall
ggplot(gradual_monthly_rainfall,aes(x=Month,y=Total_Rainfall)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))

#Gradual monthly rainfall
ggplot(data,aes(x=Date,y=Rainfall)) +
  geom_point() +
  scale_x_date(date_breaks = '1 month', 
               labels = date_format("%b-%y")) +
  labs(title = "Seasonal monthly rainfall",
       subtitle = "2007 to 2017",
       x = "",
       y = "Rainfall") +
  theme(axis.text.x = element_text(angle = 90))

#Gradual monthly rainfall
ggplot(data,aes(x=Date,y=Rainfall)) +
  geom_line() +
  scale_x_date(date_breaks = '1 month', 
               labels = date_format("%b-%y")) +
  labs(title = "Seasonal monthly rainfall",
       subtitle = "2007 to 2017",
       x = "",
       y = "Rainfall") +
  theme(axis.text.x = element_text(angle = 90))
# Gradual Rainfall
ggplot(data,aes(x=Date,y=Rainfall)) +
  geom_point() +
  scale_x_date(date_breaks = '1 month', 
               labels = date_format("%b-%y")) +
  labs(title = "Seasonal monthly rainfall",
       subtitle = "2007 to 2017",
       x = "",
       y = "Rainfall") +
  theme(axis.text.x = element_text(angle = 90))

# Gradual rainfall over the decade wrt Seasons
ggplot(data,aes(x=Date,y=Rainfall,color=Season)) +
  geom_point() +
  scale_x_date(date_breaks = '6 months', 
               labels = date_format("%b-%y")) +
  labs(title = "Seasonal monthly rainfall",
       subtitle = "2007 to 2017",
       x = "",
       y = "Rainfall") + theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("#92C5DE","#F4A582","#CA0020","#0571B0"))

#  Effect of Sunshine on rainfall
ggplot(data,aes(x=data$Sunshine,y=data$Rainfall)) +
  geom_jitter(colour = "orange") + 
  geom_smooth(method = "gam",formula = y~x,colour= "black") +
  geom_label(data = data.frame(x=10,y=350,label=paste("Pearson's Correlation: ",cor(data$Sunshine,data$Rainfall))),aes(x=x,y=y,label=label),color = "black",size=5) +
  labs(x = "Sunshine(in Watts/metre squared)",y="Rainfall(in mm)") +
  theme_bw()

cor(data$Sunshine,data$Rainfall)

#Effect of Average Temperature on Rainfall
ggplot(data,aes(x=data$AvgTemp,y=data$Rainfall)) + 
  geom_jitter() + 
  geom_smooth(method = "gam",formula = y~x) +
  geom_label(data = data.frame(x=12.5,y=300,label=cor(data$AvgTemp,data$Rainfall)),aes(x=x,y=y,label=label),color = "Orange",size=5) +
  labs(x = "Average Temperature",y="Rainfall(in mm)") +
  theme_bw()

cluster <- kmeans(data[,"Rainfall"],centers = 2)$cluster
ggplot(data,aes(x=data$Date,y=data$Rainfall,colour = cluster)) + geom_point()

# Effect of Cloud Cover on Rainfall
grid.arrange(ggplot(data) +
  geom_jitter(aes(x=data$Cloud9am,y=data$Rainfall),colour="orange") + 
  geom_smooth(method = "gam",formula = y~x,aes(x=data$Cloud9am,y=data$Rainfall),colour="black") +
  geom_label(data = data.frame(x=4,y=350,label=paste("Pearson's Correlation: ",cor(data$Cloud9am,data$Rainfall))),aes(x=x,y=y,label=label),color = "black",size=4) +
  labs(x = "Cloud9AM(in oktas)",y="Rainfall(in mm)") +
  theme_bw() ,
  ggplot(data) +
    geom_jitter(aes(x=data$Cloud3pm,y=data$Rainfall),colour="cornflowerblue") + 
    geom_smooth(method = "gam",formula = y~x,aes(x=data$Cloud3pm,y=data$Rainfall),colour="black") +
    geom_label(data = data.frame(x=4,y=350,label=paste("Pearson's Correlation: ",cor(data$Cloud3pm,data$Rainfall))),aes(x=x,y=y,label=label),color = "black",size=4) +
    labs(x = "Cloud3PM(in oktas)",y="Rainfall(in mm)") +
    theme_bw(),
  ncol=2)
  
#Effect of Min Temperature on Rainfall
grid.arrange(
ggplot(data,aes(x=data$MinTemp,y=data$Rainfall)) + 
  geom_jitter(colour="orange") + 
  geom_smooth(method = "gam",formula = y~x,colour = "black") +
  geom_label(data = data.frame(x=12,y=325,label=paste("Pearson's Correlation: ",cor(data$MinTemp,data$Rainfall))),aes(x=x,y=y,label=label),color = "black",size=4) +
  labs(x = "Minimum Temperature",y="Rainfall(in mm)") +
  theme_bw(),

ggplot(data,aes(x=data$MaxTemp,y=data$Rainfall)) + 
  geom_jitter(colour="cornflowerblue") + 
  geom_smooth(method = "gam",formula = y~x,colour = "black") +
  geom_label(data = data.frame(x=22,y=325,label=paste("Pearson's Correlation: ",cor(data$MaxTemp,data$Rainfall))),aes(x=x,y=y,label=label),color = "black",size=4) +
  labs(x = "Maximum Temperature",y="Rainfall(in mm)") +
  theme_bw(),
ncol =2)

# Effect of Pressure on Rainfall
grid.arrange(ggplot(data) +
               geom_jitter(aes(x=data$Pressure9am,y=data$Rainfall),colour="orange") + 
               geom_smooth(method = "gam",formula = y~x,aes(x=data$Pressure9am,y=data$Rainfall),colour ="black") +
               geom_label(data = data.frame(x=1010,y=300,label=paste("Pearson's Correlation: ",cor(data$Pressure9am,data$Rainfall))),aes(x=x,y=y,label=label),color = "black",size=4) +
               labs(x = "Pressure9AM(in Pascal)",y="Rainfall(in mm)") +
               theme_bw() ,
             ggplot(data) +
               geom_jitter(aes(x=data$Pressure3pm,y=data$Rainfall),colour = "cornflowerblue") + 
               geom_smooth(method = "gam",formula = y~x,aes(x=data$Pressure3pm,y=data$Rainfall),colour = "black") +
               geom_label(data = data.frame(x=1010,y=300,label=paste("Pearson's Correlation: ",cor(data$Pressure3pm,data$Rainfall))),aes(x=x,y=y,label=label),color = "black",size=4) +
               labs(x = "Pressure3PM(in Pascal)",y="Rainfall(in mm)") +
               theme_bw(),
             ncol=2)

# Effect of Humidity on Rainfall
grid.arrange(ggplot(data) +
               geom_jitter(aes(x=data$Humidity9am,y=data$Rainfall),colour="orange") + 
               geom_smooth(method = "gam",formula = y~x,aes(x=data$Humidity9am,y=data$Rainfall),colour="black") +
               geom_label(data = data.frame(x=50,y=300,label=paste("Pearson's Correlation: ",cor(data$Humidity9am,data$Rainfall))),aes(x=x,y=y,label=label),color = "black",size=4) +
               labs(x = "Humidity",y="Rainfall(in mm)") +
               theme_bw() ,
             ggplot(data) +
               geom_jitter(aes(x=data$Humidity3pm,y=data$Rainfall),colour = "cornflowerblue") + 
               geom_smooth(method = "gam",formula = y~x,aes(x=data$Humidity3pm,y=data$Rainfall),colour="black") +
               geom_label(data = data.frame(x=50,y=300,label=paste("Pearson's Correlation: ",cor(data$Humidity3pm,data$Rainfall))),aes(x=x,y=y,label=label),color = "black",size=4) +
               labs(x = "Humidity",y="Rainfall(in mm)") +
               theme_bw(),
             ncol=2)


# Heatmap of all quantitative variables
data_copy <- data
data_copy$RainToday <- case_when(
  
  data_copy$RainToday == "Yes" ~ TRUE,
  data_copy$RainToday == "No" ~ FALSE
  
  
)

data_copy$RainTomorrow <- case_when(
  
  data_copy$RainTomorrow == "Yes" ~ TRUE,
  data_copy$RainTomorrow == "No" ~ FALSE
  
  
)

quant_data <- sapply(data_copy[sapply(data_copy,function(x){is.double(x) | is.logical(x)})],function(x){as.numeric(x)})

quant_data <- subset(quant_data,select=-Date)
quant_data <- subset(quant_data,select=-Year)

r <- cor(quant_data,use="complete.obs")
round(r,2)
ggcorrplot(r, type="full",
           hc.order = TRUE, 
           lab = TRUE)
?ggcorrplot
# Parallel Coordinates
ggparcoord(quant_data,
           columns = c(2,3,4,5,6,7),
           scale="robust",
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the Data",
           alphaLines = 0.3
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )

# SPLOM

pairs(data[,c("Pressure9am","Rainfall","Humidity9am")])
colnames(data)


leaflet(data) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addMarkers(~Longitude,~Latitude, popup = ~Rainfall, clusterOptions = markerClusterOptions()) 

location_wise_rainfall[["Longitude"]] <- data



d <-unique(data %>% group_by(Location) %>% summarise(Longitude,Latitude,sum(Rainfall)) %>% dplyr::rename(Total_Rainfall = 'sum(Rainfall)'))

# Symbol Proportional Map for Total Rainfall
ggplot(d) +theme_void() + theme_void() +
  geom_sf(data=ozmap_states,aes(fill = NAME))+ geom_point(aes(x=Longitude,y=Latitude,size = Total_Rainfall),alpha = 1/2) +
    scale_fill_brewer(palette = "Pastel1") +
  labs(title="Proportional Symbol Map of Total Rainfall in major locations",fill="State/UT")



# Symbol Proportional Map for Total Rainfall
leaflet(d) %>%  
  addTiles() %>%
  addCircles(~Longitude,~Latitude,radius = ~Total_Rainfall * 5,label = ~as.character(Location))
decade_spatial <- subset(decade_spatial,select = -X1)
leaflet(decade_spatial[1:1000,]) %>%  
  addProviderTiles("CartoDB.Positron") %>% 
  addCircles(~Longitude,~Latitude,weight = 1)

# Dot Density Map for Total Rainfall
decade_spatial <- read_csv("Tot_rainfall_spatial.csv",col_types = cols(X1 = col_integer(),
                                                                       Longitude = col_double(),
                                                                       Latitude = col_double(),
                                                             Rainfall = col_double()))
decade_r
decade_r <- rasterFromXYZ(decade_spatial[,2:4])
pal <- colorNumeric(c("#C6DBEF","#4292C6","#084594"), values(decade_r),
                    na.color = "transparent")
crs(decade_r) <-  "+init=EPSG:4283"
leaflet() %>%   addTiles() %>%
  addRasterImage(decade_r, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(decade_r),
            title = "Rainfall")

as.vector(brewer.pal(7,"Blues"))
c("#C6DBEF","#4292C6","#084594")