library(tidyverse)
library(ggplot2)
library(raster) # For creating raster file formats for easy plotting
library(rgdal) # For geospatial analysis
library(ncdf4) # For dealing with netCDF grid data
library(tidync)
library(tmaptools) # To use geocode_OSM for lattitude and longitude
library(reshape)
library(RColorBrewer)
library(skimr)
library(arsenal)

g <- raster("2021030320210303.grid")
g.spdf <- as(g,"SpatialPixelsDataFrame")
g.df <- as.data.frame(g.spdf)
spplot(g)

# Either read the nc file as raster object OR directly use tidync package
tmp_raster <- brick("2007.daily_rain.nc")
tmp_raster
spplot(tmp_raster)

# Use tidync package to clean your netCDF files

nc_2007 <- tidync("2007.daily_rain.nc")
str(nc_2007)

# Prints the variables in the file
nc_2007$variable$name

# Prints the dimensions in the file. LAT LON and TIME in this case
nc_2007$dimension$name


# filtering based on longitude,lattitude or time(available variables)
nc_2007 %>% hyper_filter(lon = lon < 120)
nc_2007 %>% hyper_filter(time = time <= 5 & time >= 2) # 2nd Jan to 5th Jan 2007 data
nc_2007 %>% hyper_filter(lat = lat <120,time = between(time,1,10))

# active is similar to select of dplyr

lat_2007 <- nc_2007 %>% activate("D0")
lon_2007 <- nc_2007 %>% activate("D1")
time_2007 <- nc_2007 %>% activate("D2")
main_grid_2007 <- nc_2007 %>% activate("daily_rain")



lat_2007
# Prints the variables of active grid
hyper_vars(nc_2007)

# Prints the dimensions of active grid
hyper_dims(nc_2007)

# Prints the name of the current active grid
active(nc_2007)

# transform tables: Describes the relationship between dimensions and coordinates
trans_2007 <- hyper_transforms(nc_2007)

# PRINTS the latitude values 
hyper_transforms(nc_2007)$lat$lat

# PRINTS the longitude values 
hyper_transforms(nc_2007)$lon$lon

# PRINTS the time values 
hyper_transforms(nc_2007)$time$time


# Converting to an multidimensional array
array_2007 <- nc_2007 %>% hyper_array()

# Rain values in 3 dimensions
array_2007[[1]]

length(array_2007)

lapply(array_2007,dim)

trans_2007 <- attr(array_2007,"transforms")

dim(array_2007[[1]])
summary(array_2007[[1]])

# Percentage of missing values
class(array_2007)
sum(is.na(array_2007[[1]]))/length(array_2007[[1]]) * 100

rain_2007 <- array_2007[[1]]

# % of missing values each day of the year 2007
i <- 0
for (i in seq(1,365)){
  print(sum(is.na(rain_2007[,,i]))/length(rain_2007[,,i]) * 100)
}
missing_spat_perc <- sum(is.na(rain_2007[,,i]))/length(rain_2007[,,i]) * 100
x <- c(100 - missing_spat_perc,missing_spat_perc)
labels <- c("Available Data", "Missing Data")

pie(x,labels=round(x,2),main="Proportion of missing values in NetCDF spatial-temporal grid",col = brewer.pal(2, "Paired"))
legend("topright", c("Available Data","Missing Data"), cex = 0.8,
       fill = brewer.pal(2, "Paired"))
# Creating a pie chart of percentage of available spatial data
pie_df <- data.frame(x = c(100 - missing_spat_perc,missing_spat_perc), labels=c("Available Data", "Missing Data"))
ggplot(pie_df , aes(x="", y=x, fill=labels)) + geom_bar(stat="identity", width=1,color = "black") +
coord_polar("y", start=0) + geom_text(aes(label = paste0(round(x,2), "%")), position = position_stack(vjust = 0.5)) +
scale_fill_manual(values=c("#A6CEE3", "#1F78B4")) +
labs(x = NULL, y = NULL, fill = NULL, title = "Proportion of missing values in NetCDF spatial-temporal grid") +
theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))

# Reshaping the vector of 0s and 1s based on missing values into a matrix for plotting
missing_day1_2007 <- matrix(as.integer(is.na(rain_2007[,,1])),nrow=dim(rain_2007)[1],byrow = TRUE)
matrix(as.integer(!is.na(rain_2007[,,1])),nrow=dim(rain_2007)[1],byrow = TRUE)
# dot density plot of rainfall on day 1, 2007
image(trans_2007$lon$lon, trans_2007$lat$lat, rain_2007[is.na(rain_2007)][,,1])
# Contour plot of rainfaill on day 1 , 2007
filled.contour(trans_2007$lon$lon, trans_2007$lat$lat, rain_2007[,,200])


# Plotting the missing values of rain on locations of Australia on day 1 
image(trans_2007$lon$lon, trans_2007$lat$lat, missing_day1_2007)
maps::map("world2",add=TRUE)

?maps::map
help(package='maps')
ncmeta::nc_grids("2007.daily_rain.nc")
ncmeta::nc_vars("2007.daily_rain.nc")
ncmeta::nc_dims("2007.daily_rain.nc")

ncmeta::nc_var("2007.daily_rain.nc","lat")
ncmeta::nc_var("2007.daily_rain.nc",0)

ncmeta::nc_atts("2007.daily_rain.nc")
ncmeta::nc_atts("2007.daily_rain.nc","time")

# Activating a dimension/variable of the grid for querying and exploration
time_2007 <- nc_2007 %>% activate("D2")  %>% hyper_array()
time_2007
rain_2007 <- nc_2007 %>% activate("daily_rain") %>% hyper_array()
rain_2007

length(array_2007)
# Prints the variables part of the grid
names(array_2007)
# Prints the dimensions associated with the variable
dim(array_2007[[1]])
array_2007


# image(): plot equivalent of Raster library
image(array_2007[[1]])

gc() # Garbage collection


# Loading and wrangling remaining years NetCDF files
nc_2008 <- tidync("2008.daily_rain.nc")

trans_2008 <- nc_2008 %>% hyper_transforms()

array_2008 <- nc_2008 %>% hyper_array()

rain_2008 <- array_2008[[1]]

image(trans_2008$lon$lon, trans_2008$lat$lat, rain_2008[,,1])

tidync("2007.daily_rain.nc")
# Loading and wrangling all years NETCDF data

tbl_2007 <- tidync("2007.daily_rain.nc") %>% hyper_tbl_cube()

tbl_2007 <- tidync("2007.monthly_rain.nc") %>% hyper_tbl_cube()
tbl_2008 <- tidync("2008.monthly_rain.nc") %>% hyper_tbl_cube()
tbl_2009 <- tidync("2009.monthly_rain.nc") %>% hyper_tbl_cube()
tbl_2010 <- tidync("2010.monthly_rain.nc") %>% hyper_tbl_cube()
tbl_2011 <- tidync("2011.monthly_rain.nc") %>% hyper_tbl_cube()
tbl_2012 <- tidync("2012.monthly_rain.nc") %>% hyper_tbl_cube()
tbl_2013 <- tidync("2013.monthly_rain.nc") %>% hyper_tbl_cube()
tbl_2014 <- tidync("2014.monthly_rain.nc") %>% hyper_tbl_cube()
tbl_2015 <- tidync("2015.monthly_rain.nc") %>% hyper_tbl_cube()
tbl_2016 <- tidync("2016.monthly_rain.nc") %>% hyper_tbl_cube()
tbl_2017 <- tidync("2017.monthly_rain.nc") %>% hyper_tbl_cube()

sum(is.na(tbl_2007$mets[[1]])) / length(tbl_2007$mets[[1]]) * 100
filled.contour(tbl_2007$dims$lon,tbl_2007$dims$lat,tbl_2007$mets[[1]][,,1])
filled.contour(tbl_2008$dims$lon,tbl_2008$dims$lat,tbl_2008$mets[[1]][,,1])
filled.contour(tbl_2009$dims$lon,tbl_2009$dims$lat,tbl_2009$mets[[1]][,,1])


impute_zeros <- function(tbl){
  for(i in seq(1,length(tbl$dims$lon))){
    for(j in seq(1,length(tbl$dims$lat))){
      for(k in seq(1,length(tbl$dims$time))){
        if(is.na(tbl$mets[[1]][i,j,k])){
          tbl$mets[[1]][i,j,k] <- 0L
        }
      }
    }
  }
  
}

impute_zeros(tbl_2007)
impute_zeros(tbl_2008)
impute_zeros(tbl_2009)
impute_zeros(tbl_2010)
impute_zeros(tbl_2011)
impute_zeros(tbl_2012)
impute_zeros(tbl_2013)
impute_zeros(tbl_2014)
impute_zeros(tbl_2015)
impute_zeros(tbl_2016)
impute_zeros(tbl_2017)

sum_of_months <- function(i){
  sum_matrix <- tbl_2007$mets[[1]][,,i] + 
    tbl_2008$mets[[1]][,,i] + 
    tbl_2009$mets[[1]][,,i] + 
    tbl_2010$mets[[1]][,,i] + 
    tbl_2011$mets[[1]][,,i] + 
    tbl_2012$mets[[1]][,,i] + 
    tbl_2013$mets[[1]][,,i] + 
    tbl_2014$mets[[1]][,,i] + 
    tbl_2015$mets[[1]][,,i] + 
    tbl_2016$mets[[1]][,,i] + 
    tbl_2017$mets[[1]][,,i] 
  return(sum_matrix)
}
tot_rainfall_jan <- sum_of_months(1)
tot_rainfall_feb <- sum_of_months(2)
tot_rainfall_mar <- sum_of_months(3)
tot_rainfall_apr <- sum_of_months(4)
tot_rainfall_may <- sum_of_months(5)
tot_rainfall_jun <- sum_of_months(6)
tot_rainfall_jul <- sum_of_months(7)
tot_rainfall_aug <- sum_of_months(8)
tot_rainfall_sep <- sum_of_months(9)
tot_rainfall_oct <- sum_of_months(10)
tot_rainfall_nov <- sum_of_months(11)
tot_rainfall_dec <- sum_of_months(12)

#Adding back the NA values for total_rainfall
tot_rainfall_jan[tot_rainfall_jan == 0L] <- NA
tot_rainfall_feb[tot_rainfall_feb == 0L] <- NA
tot_rainfall_mar[tot_rainfall_mar == 0L] <- NA
tot_rainfall_apr[tot_rainfall_apr == 0L] <- NA
tot_rainfall_may[tot_rainfall_may == 0L] <- NA
tot_rainfall_jun[tot_rainfall_jun == 0L] <- NA
tot_rainfall_jul[tot_rainfall_jul == 0L] <- NA
tot_rainfall_aug[tot_rainfall_aug == 0L] <- NA
tot_rainfall_sep[tot_rainfall_sep == 0L] <- NA
tot_rainfall_oct[tot_rainfall_oct == 0L] <- NA
tot_rainfall_nov[tot_rainfall_nov == 0L] <- NA
tot_rainfall_dec[tot_rainfall_dec == 0L] <- NA

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

# Convert Large Matrix into Data Frame
large_matrix_to_df <- function(tot_rainfall){
  tot_rainfall_matrix <- matrix(nrow = length(tbl_2017$dims$lon) *length(tbl_2017$dims$lat),ncol = 3)
  index <- 1
  for(i in seq(1,length(tbl_2017$dims$lon))){
    for(j in seq(1,length(tbl_2017$dims$lat))){
      tot_rainfall_matrix[index,1] <- tbl_2017$dims$lon[i]
      tot_rainfall_matrix[index,2] <- tbl_2017$dims$lat[j]
      tot_rainfall_matrix[index,3] <- tot_rainfall[i,j]
      index <- index + 1
    }
  }
  return(data.frame(Longitude = tot_rainfall_matrix[,1],Latitude = tot_rainfall_matrix[,2],Rainfall = tot_rainfall_matrix[,3]))
  
}

tot_rainfall_jan_df <- large_matrix_to_df(tot_rainfall_jan)
tot_rainfall_feb_df <- large_matrix_to_df(tot_rainfall_feb)
tot_rainfall_mar_df <- large_matrix_to_df(tot_rainfall_mar)
tot_rainfall_apr_df <- large_matrix_to_df(tot_rainfall_apr)
tot_rainfall_may_df <- large_matrix_to_df(tot_rainfall_may)
tot_rainfall_jun_df <- large_matrix_to_df(tot_rainfall_jun)
tot_rainfall_jul_df <- large_matrix_to_df(tot_rainfall_jul)
tot_rainfall_aug_df <- large_matrix_to_df(tot_rainfall_aug)
tot_rainfall_sep_df <- large_matrix_to_df(tot_rainfall_sep)
tot_rainfall_oct_df <- large_matrix_to_df(tot_rainfall_oct)
tot_rainfall_nov_df <- large_matrix_to_df(tot_rainfall_nov)
tot_rainfall_dec_df <- large_matrix_to_df(tot_rainfall_dec)


write.csv(tot_rainfall_jan_df,"Overall_Jan_Rainfall.csv")
write.csv(tot_rainfall_feb_df,"Overall_Feb_Rainfall.csv")
write.csv(tot_rainfall_mar_df,"Overall_Mar_Rainfall.csv")
write.csv(tot_rainfall_apr_df,"Overall_Apr_Rainfall.csv")
write.csv(tot_rainfall_may_df,"Overall_May_Rainfall.csv")
write.csv(tot_rainfall_jun_df,"Overall_Jun_Rainfall.csv")
write.csv(tot_rainfall_jul_df,"Overall_Jul_Rainfall.csv")
write.csv(tot_rainfall_aug_df,"Overall_Aug_Rainfall.csv")
write.csv(tot_rainfall_sep_df,"Overall_Sep_Rainfall.csv")
write.csv(tot_rainfall_oct_df,"Overall_Oct_Rainfall.csv")
write.csv(tot_rainfall_nov_df,"Overall_Nov_Rainfall.csv")
write.csv(tot_rainfall_dec_df,"Overall_Dec_Rainfall.csv")

# Replacing NA with 0 and adding all months rainfall 2007

# replace_na(tbl_2007$mets[[1]],0)

for(i in seq(1,length(tbl_2007$dims$lon))){
  for(j in seq(1,length(tbl_2007$dims$lat))){
    for(k in seq(1,length(tbl_2007$dims$time))){
      if(is.na(tbl_2007$mets[[1]][i,j,k])){
        tbl_2007$mets[[1]][i,j,k] <- 0L
      }
    }
  }
}

sum(is.na(tbl_2007$mets[[1]]))      

tot_rainfall_2007 <- tbl_2007$mets[[1]][,,1]
for(i in seq(2,length(tbl_2007$dims$time))){
  tot_rainfall_2007 <- tot_rainfall_2007 + tbl_2007$mets[[1]][,,i]
}

#Adding back the NA values for total_rainfall
tot_rainfall_2007[tot_rainfall_2007 == 0L] <- NA

dim(tot_rainfall_2007)
# filled.contour(tbl_2007$dims$lon,tbl_2007$dims$lat,tot_rainfall_2007)

# Replacing NA with 0 and adding all months rainfall 2008

# replace_na(tbl_2008$mets[[1]],0)

for(i in seq(1,length(tbl_2008$dims$lon))){
  for(j in seq(1,length(tbl_2008$dims$lat))){
    for(k in seq(1,length(tbl_2008$dims$time))){
      if(is.na(tbl_2008$mets[[1]][i,j,k])){
        tbl_2008$mets[[1]][i,j,k] <- 0
      }
    }
  }
}

sum(is.na(tbl_2008$mets[[1]]))      

tot_rainfall_2008 <- tbl_2008$mets[[1]][,,1]
for(i in seq(2,length(tbl_2008$dims$time))){
  tot_rainfall_2008 <- tot_rainfall_2008 + tbl_2008$mets[[1]][,,i]
}

#Replacing the NA values for total_rainfall
tot_rainfall_2008[tot_rainfall_2008 == 0L] <- NA

dim(tot_rainfall_2008)
# filled.contour(tbl_2008$dims$lon,tbl_2008$dims$lat,tot_rainfall_2008)


# Replacing NA with 0 and adding all months rainfall 2009

# replace_na(tbl_2009$mets[[1]],0)
na_2009 <- as.integer(!is.na(tbl_2009$mets[[1]]))

for(i in seq(1,length(tbl_2009$dims$lon))){
  for(j in seq(1,length(tbl_2009$dims$lat))){
    for(k in seq(1,length(tbl_2009$dims$time))){
      if(is.na(tbl_2009$mets[[1]][i,j,k])){
        tbl_2009$mets[[1]][i,j,k] <- 0
      }
    }
  }
}

sum(is.na(tbl_2009$mets[[1]]))      

tot_rainfall_2009 <- tbl_2009$mets[[1]][,,1]
for(i in seq(2,length(tbl_2009$dims$time))){
  tot_rainfall_2009 <- tot_rainfall_2009 + tbl_2009$mets[[1]][,,i]
}

#Replacing the NA values for total_rainfall
tot_rainfall_2009[tot_rainfall_2009 == 0L] <- NA

dim(tot_rainfall_2009)
# filled.contour(tbl_2009$dims$lon,tbl_2009$dims$lat,tot_rainfall_2009)

# Replacing NA with 0 and adding all months rainfall 2010

# replace_na(tbl_2010$mets[[1]],0)
na_2010 <- as.integer(!is.na(tbl_2010$mets[[1]]))

for(i in seq(1,length(tbl_2010$dims$lon))){
  for(j in seq(1,length(tbl_2010$dims$lat))){
    for(k in seq(1,length(tbl_2010$dims$time))){
      if(is.na(tbl_2010$mets[[1]][i,j,k])){
        tbl_2010$mets[[1]][i,j,k] <- 0
      }
    }
  }
}

sum(is.na(tbl_2010$mets[[1]]))      

tot_rainfall_2010 <- tbl_2010$mets[[1]][,,1]
for(i in seq(2,length(tbl_2010$dims$time))){
  tot_rainfall_2010 <- tot_rainfall_2010 + tbl_2010$mets[[1]][,,i]
}

#Replacing the NA values for total_rainfall
tot_rainfall_2010[tot_rainfall_2010 == 0L] <- NA

dim(tot_rainfall_2010)
# filled.contour(tbl_2010$dims$lon,tbl_2010$dims$lat,tot_rainfall_2010)

# Replacing NA with 0 and adding all months rainfall 2011

# replace_na(tbl_2011$mets[[1]],0)
na_2011 <- as.integer(!is.na(tbl_2011$mets[[1]]))

for(i in seq(1,length(tbl_2011$dims$lon))){
  for(j in seq(1,length(tbl_2011$dims$lat))){
    for(k in seq(1,length(tbl_2011$dims$time))){
      if(is.na(tbl_2011$mets[[1]][i,j,k])){
        tbl_2011$mets[[1]][i,j,k] <- 0
      }
    }
  }
}

sum(is.na(tbl_2011$mets[[1]]))      

tot_rainfall_2011 <- tbl_2011$mets[[1]][,,1]
for(i in seq(2,length(tbl_2011$dims$time))){
  tot_rainfall_2011 <- tot_rainfall_2011 + tbl_2011$mets[[1]][,,i]
}

#Replacing the NA values for total_rainfall
tot_rainfall_2011[tot_rainfall_2011 == 0L] <- NA

dim(tot_rainfall_2011)
# filled.contour(tbl_2011$dims$lon,tbl_2011$dims$lat,tot_rainfall_2011)

# Replacing NA with 0 and adding all months rainfall 2012

# replace_na(tbl_2012$mets[[1]],0)
na_2012 <- as.integer(!is.na(tbl_2012$mets[[1]]))

for(i in seq(1,length(tbl_2012$dims$lon))){
  for(j in seq(1,length(tbl_2012$dims$lat))){
    for(k in seq(1,length(tbl_2012$dims$time))){
      if(is.na(tbl_2012$mets[[1]][i,j,k])){
        tbl_2012$mets[[1]][i,j,k] <- 0
      }
    }
  }
}

sum(is.na(tbl_2012$mets[[1]]))      

tot_rainfall_2012 <- tbl_2012$mets[[1]][,,1]
for(i in seq(2,length(tbl_2012$dims$time))){
  tot_rainfall_2012 <- tot_rainfall_2012 + tbl_2012$mets[[1]][,,i]
}

#Replacing the NA values for total_rainfall
tot_rainfall_2012[tot_rainfall_2012 == 0L] <- NA

dim(tot_rainfall_2012)
# filled.contour(tbl_2012$dims$lon,tbl_2012$dims$lat,tot_rainfall_2012)

# Replacing NA with 0 and adding all months rainfall 2013

# replace_na(tbl_2013$mets[[1]],0)


for(i in seq(1,length(tbl_2013$dims$lon))){
  for(j in seq(1,length(tbl_2013$dims$lat))){
    for(k in seq(1,length(tbl_2013$dims$time))){
      if(is.na(tbl_2013$mets[[1]][i,j,k])){
        tbl_2013$mets[[1]][i,j,k] <- 0
      }
    }
  }
}

sum(is.na(tbl_2013$mets[[1]]))      

tot_rainfall_2013 <- tbl_2013$mets[[1]][,,1]
for(i in seq(2,length(tbl_2013$dims$time))){
  tot_rainfall_2013 <- tot_rainfall_2013 + tbl_2013$mets[[1]][,,i]
}

#Replacing the NA values for total_rainfall
tot_rainfall_2013[tot_rainfall_2013 == 0L] <- NA

dim(tot_rainfall_2013)
# filled.contour(tbl_2013$dims$lon,tbl_2013$dims$lat,tot_rainfall_2013)


# Replacing NA with 0 and adding all months rainfall 2014

# replace_na(tbl_2014$mets[[1]],0)


for(i in seq(1,length(tbl_2014$dims$lon))){
  for(j in seq(1,length(tbl_2014$dims$lat))){
    for(k in seq(1,length(tbl_2014$dims$time))){
      if(is.na(tbl_2014$mets[[1]][i,j,k])){
        tbl_2014$mets[[1]][i,j,k] <- 0
      }
    }
  }
}

sum(is.na(tbl_2014$mets[[1]]))      

tot_rainfall_2014 <- tbl_2014$mets[[1]][,,1]
for(i in seq(2,length(tbl_2014$dims$time))){
  tot_rainfall_2014 <- tot_rainfall_2014 + tbl_2014$mets[[1]][,,i]
}

#Replacing the NA values for total_rainfall
tot_rainfall_2014[tot_rainfall_2014 == 0L] <- NA

dim(tot_rainfall_2014)
# filled.contour(tbl_2014$dims$lon,tbl_2014$dims$lat,tot_rainfall_2014)


# Replacing NA with 0 and adding all months rainfall 2015

# replace_na(tbl_2015$mets[[1]],0)

for(i in seq(1,length(tbl_2015$dims$lon))){
  for(j in seq(1,length(tbl_2015$dims$lat))){
    for(k in seq(1,length(tbl_2015$dims$time))){
      if(is.na(tbl_2015$mets[[1]][i,j,k])){
        tbl_2015$mets[[1]][i,j,k] <- 0
      }
    }
  }
}

sum(is.na(tbl_2015$mets[[1]]))      

tot_rainfall_2015 <- tbl_2015$mets[[1]][,,1]
for(i in seq(2,length(tbl_2015$dims$time))){
  tot_rainfall_2015 <- tot_rainfall_2015 + tbl_2015$mets[[1]][,,i]
}

#Replacing the NA values for total_rainfall
tot_rainfall_2015[tot_rainfall_2015 == 0L] <- NA

dim(tot_rainfall_2015)
# filled.contour(tbl_2015$dims$lon,tbl_2015$dims$lat,tot_rainfall_2015)

# Replacing NA with 0 and adding all months rainfall 2016

# replace_na(tbl_2016$mets[[1]],0)

for(i in seq(1,length(tbl_2016$dims$lon))){
  for(j in seq(1,length(tbl_2016$dims$lat))){
    for(k in seq(1,length(tbl_2016$dims$time))){
      if(is.na(tbl_2016$mets[[1]][i,j,k])){
        tbl_2016$mets[[1]][i,j,k] <- 0
      }
    }
  }
}

sum(is.na(tbl_2016$mets[[1]]))      

tot_rainfall_2016 <- tbl_2016$mets[[1]][,,1]
for(i in seq(2,length(tbl_2016$dims$time))){
  tot_rainfall_2016 <- tot_rainfall_2016 + tbl_2016$mets[[1]][,,i]
}

#Replacing the NA values for total_rainfall
tot_rainfall_2016[tot_rainfall_2016 == 0L] <- NA

dim(tot_rainfall_2016)
# filled.contour(tbl_2016$dims$lon,tbl_2016$dims$lat,tot_rainfall_2016)


# Replacing NA with 0 and adding all months rainfall 2017

# replace_na(tbl_2017$mets[[1]],0)

for(i in seq(1,length(tbl_2017$dims$lon))){
  for(j in seq(1,length(tbl_2017$dims$lat))){
    for(k in seq(1,length(tbl_2017$dims$time))){
      if(is.na(tbl_2017$mets[[1]][i,j,k])){
        tbl_2017$mets[[1]][i,j,k] <- 0
      }
    }
  }
}

sum(is.na(tbl_2017$mets[[1]]))      

tot_rainfall_2017 <- tbl_2017$mets[[1]][,,1]
for(i in seq(2,length(tbl_2017$dims$time))){
  tot_rainfall_2017 <- tot_rainfall_2017 + tbl_2017$mets[[1]][,,i]
}

#Replacing the NA values for total_rainfall
tot_rainfall_2017[tot_rainfall_2017 == 0L] <- NA

dim(tot_rainfall_2017)
# filled.contour(tbl_2017$dims$lon,tbl_2017$dims$lat,tot_rainfall_2017)


##### Summing all 10 years rainfall

tot_rainfall <- tot_rainfall_2007 + tot_rainfall_2008 +
  tot_rainfall_2009 + tot_rainfall_2010 + tot_rainfall_2011 +
  tot_rainfall_2012 + tot_rainfall_2013 + tot_rainfall_2014 +
  tot_rainfall_2015 + tot_rainfall_2016 + tot_rainfall_2017

table(tot_rainfall)
# Displays the contour plot of total rainfall over the decade
filled.contour(tbl_2017$dims$lon,tbl_2017$dims$lat,tot_rainfall)

# Convert Large Matrix into Data Frame
tot_rainfall_matrix <- matrix(nrow = length(tbl_2017$dims$lon) *length(tbl_2017$dims$lat),ncol = 3)
index <- 1
for(i in seq(1,length(tbl_2017$dims$lon))){
  for(j in seq(1,length(tbl_2017$dims$lat))){
    tot_rainfall_matrix[index,1] <- tbl_2017$dims$lon[i]
    tot_rainfall_matrix[index,2] <- tbl_2017$dims$lat[j]
    tot_rainfall_matrix[index,3] <- tot_rainfall[i,j]
    index <- index + 1
  }
}
tot_rainfall_df <- data.frame(Longitude = tot_rainfall_matrix[,1],Latitude = tot_rainfall_matrix[,2],Rainfall = tot_rainfall_matrix[,3])
tot_rainfall_df    
?write.csv
write.csv(tot_rainfall_df,"Tot_rainfall_spatial.csv")
# tbl_2007 <- nc_2007 %>% hyper_tbl_cube()
# 
# tbl_2007$mets[[1]]
# tbl_2007$dims$lon
# tbl_2007$dims$lat
# tbl_2007$dims$time


lon_2007 <- trans_2007$lon$lon
lat_2007 <- trans_2007$lat$lat
time_2007 <- trans_2007$time$time

length(lat_2007)

lon_2007[2]
lat_2007[2]
time_2007[2]

attr_values <-  cbind(c(lon_2007,lat_2007,time_2007))

attr_values
data <- read_csv("weatherAUS.csv",col_types = cols(
  Date = col_date(format = ""),
  Location = col_character(),
  MinTemp = col_double(),
  MaxTemp = col_double(),
  Rainfall = col_double(),
  Evaporation = col_double(),
  Sunshine = col_double(),
  WindGustDir = col_character(),
  WindGustSpeed = col_double(),
  WindDir9am = col_character(),
  WindDir3pm = col_character(),
  WindSpeed9am = col_double(),
  WindSpeed3pm = col_double(),
  Humidity9am = col_double(),
  Humidity3pm = col_double(),
  Pressure9am = col_double(),
  Pressure3pm = col_double(),
  Cloud9am = col_double(),
  Cloud3pm = col_double(),
  Temp9am = col_double(),
  Temp3pm = col_double(),
  RainToday = col_character(),
  RainTomorrow = col_character()
))

head(data)
# Data Summaries for whole dataset
skim(data)

summary(data)

table_one <- tableby(continent ~ ., data = data) 
summary(table_one, title = "Gapminder Data")


# Missing Values
col <- ''
for (col in colnames(data)){
  print(paste("Missing Values for column: ",col))
  print(sum(is.na(data[col])))
}

# Missing Values percentage
missing_perc <- c()

col <- ''
for (col in colnames(data)){
  print(paste("Missing Values % for column: ",col))
  print((sum(is.na(data[col]))/dim(data[col])[1]) * 100)
  missing_perc <- c(missing_perc,(sum(is.na(data[col]))/dim(data[col])[1]) * 100)
}

missing_df <- data.frame(
  column = as.vector(colnames(data)),
  missing = missing_perc
)
missing_df

ggplot(missing_df,aes(x=column,y=missing)) +
  geom_bar(stat='identity',fill = "slateblue",colour = "black") +
  labs(title = "Missing Values %",
       x = "",
       y = "Missing Values %") +
  theme(axis.text.x = element_text(angle = 90)) 



# Imputing missing values for quantitative variables with mean
data[["RainTomorrow"]][is.na(data$RainTomorrow)]
data[["RainTomorrow"]][is.na(data[["RainTomorrow"]])]
data$RainTomorrow[is.na(data$RainTomorrow)]

col <- ''
for (col in colnames(data)){
  if(is.numeric(data[[col]]) | is.double(data[[col]])){
    data[[col]][is.na(data[[col]])] <- mean(data[[col]],na.rm = TRUE)
  }
  else if(is.character(data[[col]])){
    data[[col]][is.na(data[[col]])] <- as.character(as.data.frame(table(data[[col]]))[1,1])
  }
}
table(data$RainToday)
as.character(as.data.frame(table(data$RainToday))[1,1])



# Distinct values per column
col <- ''
for (col in colnames(data)){
  print(paste("Distinct values for column: ",col))
  print(unique(data[col]))
}

col <- ''
for (col in colnames(data)){
  print(paste("Count of distinct values for column: ",col))
  print(n_distinct(data[col]))
}

col <- ''
for (col in colnames(data)){
  print(data %>% group_by(data[col]) %>% count() %>% arrange('n'))
}

# Finding outliers of all the attributes
col <- ''
for (col in colnames(data)){
  if(is.integer(data[[col]]) | is.numeric(data[[col]])){
    print(paste(col,boxplot.stats(data[[col]])$out))
  }
  
}
print(boxplot.stats(data$MinTemp)$out)

data[ ! data$Sunshine  %in% boxplot.stats(data$Sunshine)$out,]

# Removing outliers from each column of dataset
col <- ''
for (col in colnames(data)){
  if(is.integer(data[[col]]) | is.numeric(data[[col]])){
    data <- data[ ! data[[col]]  %in% boxplot.stats(data[[col]])$out,]
  }
  
}

dim(data)

length(boxplot.stats(data$Evaporation)$out)
dim(data)
# Evaporation Outlier
max(boxplot.stats(data$Evaporation)$out)


table(data$Evaporation != max(boxplot.stats(data$Evaporation)$out))

Evaporation_outlier <- data$Evaporation != max(boxplot.stats(data$Evaporation)$out)

dim(data)
dim(data[Evaporation_outlier,])

data <- data[Evaporation_outlier,]

# WindSpeed9AM outlier

summary(boxplot.stats(data$WindSpeed9am)$out)

ws9_outlier <- data$WindSpeed9am != max(boxplot.stats(data$WindSpeed9am)$out)

dim(data[ws9_outlier,])

data <- data[ws9_outlier,]

# Cloud9AM Outlier - Cloud value is measured in oktas ranging from 0 to 8
max(data$Cloud9am)
length(data$Cloud9am[data$Cloud9am == max(data$Cloud9am)])

min(data$Cloud9am)

data <- data[data$Cloud9am != max(data$Cloud9am),]


# Cloud9AM Outlier - Cloud value is measure in oktas ranging from 0 to 8

max(data$Cloud3pm)
length(data$Cloud3pm[data$Cloud3pm == max(data$Cloud3pm)])
min(data$Cloud3pm)

data <- data[data$Cloud3pm != max(data$Cloud3pm),]

rain_bool <- function(row){
  if (is.na(row)){
    return(NA)
  }
  else if (row == "Yes"){
    row <- TRUE
  }
  else{
    row <- FALSE
  }
  return(row)
}



# Convert RainToday and RainTomorrow to Boolean Type based on "Yes", "No"
data$RainToday <- lapply(data$RainToday,rain_bool)
data$RainTomorrow <- lapply(data$RainTomorrow,rain_bool)
str(data)

# Converting using str_replace
data$RainToday <- str_replace(data$RainToday,"Yes","TRUE")
data$RainToday <- str_replace(data$RainToday,"No","FALSE")
data$RainToday <- lapply(data$RainToday,as.logical)

data$RainTomorrow <- str_replace(data$RainTomorrow,"Yes","TRUE")
data$RainTomorrow <- str_replace(data$RainTomorrow,"No","FALSE")
data$RainTomorrow <- lapply(data$RainTomorrow,as.logical)

# Converting using case_when()
data$RainToday <- case_when(
  
  data$RainToday == "Yes" ~ TRUE,
  data$RainToday == "No" ~ FALSE
  
  
)

data$RainTomorrow <- case_when(
  
  data$RainTomorrow == "Yes" ~ TRUE,
  data$RainTomorrow == "No" ~ FALSE
  
  
)
data$RainTomorrow
# Standardization or Z Score transformation
plot(scale(data$Rainfall,center = TRUE,scale = TRUE))
r <- scale(data$Rainfall,center = TRUE,scale = TRUE)

col <- ""
for (col in seq(1,length(colnames(data)))){
    print(class(data[col]))
  data["Date"]
}
colnames(data)

# Create a new column Year corresponding to date

data[["Year"]] <- format(data$Date, format = "%Y")
data$Year

# Create a new column Month corresponding to date
data[["Month"]] <- as.integer(format(data$Date, format = "%m"))
data$Month

# Create seasons based on Australia
data[["Season"]] <- NA
data$Season[data$Month %in% c(12, 1:2)] = "summer"
data$Season[data$Month %in% 3:5] = "autumn"
data$Season[data$Month %in% 6:8] = "winter"
data$Season[data$Month %in% 9:11] = "spring"

data$Season

# Create Daily Average Temperature column
data[["AvgTemp"]] <- rowMeans(data[c("MinTemp","MaxTemp")])

# Create longitude and latitude for location

geocode_OSM("Sydney")$coords[1]
geocode_OSM(trimws(gsub('([[:upper:]])', ' \\1', col)))$coords[1]
trimws(gsub('([[:upper:]])', ' \\1', "SydneyAirport"))
sub('([[:upper:]])', ' \\1', "ydneyAirport")

location_df <- data.frame(location = unique(data$Location),long = rep(1,times=n_distinct(data$Location)),lat =rep(1,times=n_distinct(data$Location)))

location_df[location_df$location == "Perth","long"]
col <- ''
for(col in unique(data$Location)){
  if (col == "PearceRAAF"){
    print(paste(col,geocode_OSM("Pearce RAAF")$coords))
  }
  else{
  print(paste(col,geocode_OSM(trimws(gsub('([[:upper:]])', ' \\1', paste(col,"Australia"))))$coords))
  }
}

col <- ''
for(col in unique(data$Location)){
  if (col == "PearceRAAF"){
    location_df[location_df$location == col,"long"] <- geocode_OSM("Pearce RAAF")$coords[1]
    location_df[location_df$location == col,"lat"] <- geocode_OSM("Pearce RAAF")$coords[2]
  }
  else{
    location_df[location_df$location == col,"long"] <- geocode_OSM(trimws(gsub('([[:upper:]])', ' \\1', paste(col,"Australia"))))$coords[1]
    location_df[location_df$location == col,"lat"] <- geocode_OSM(trimws(gsub('([[:upper:]])', ' \\1', paste(col,"Australia"))))$coords[2]
  }
}

for(col in location_df$location){
  
  data[["Longitude"]][data$Location == col] <- location_df[location_df$location == col,"long"]
  data[["Latitude"]][data$Location == col] <- location_df[location_df$location == col,"lat"]
}
data

# Plotting the distribution of each attribute
ggplot(data,aes(x=Date)) + geom_bar()
ggplot(data,aes(x=Location)) + geom_bar()
ggplot(data,aes(x=MinTemp)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()
ggplot(data,aes(x=MinTemp)) + geom_boxplot()
ggplot(data,aes(x=MaxTemp)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()
ggplot(data,aes(x=MaxTemp)) + geom_boxplot()
ggplot(data,aes(x=Rainfall)) + geom_histogram(fill="cornflowerblue",color="black") + theme_bw() + labs(title="Right skewed Rainfall Distribution(in mm)")
ggplot(data,aes(x=Rainfall)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue",color="black") + geom_density() + scale_x_log10() + theme_bw() + labs(title="Log transformation done on Rainfall attribute")
ggplot(data,aes(x=Rainfall)) + geom_boxplot()
# Notice the outlier with Evaporation = 150
ggplot(data,aes(x=Evaporation)) + geom_dotplot(binwidth = 1)
ggplot(data,aes(x=Evaporation)) + geom_boxplot(fill="slateblue", alpha=0.2) + theme_bw() + labs(title = "Evaporation Outliers")
ggplot(data,aes(x=Sunshine)) + geom_bar()
ggplot(data,aes(x=WindGustDir)) + geom_bar()
ggplot(data,aes(x=WindGustSpeed)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()
ggplot(data,aes(x=WindGustSpeed)) + geom_boxplot()
ggplot(data,aes(x=WindDir9am)) + geom_bar()
ggplot(data,aes(x=WindDir3pm)) + geom_bar()
# You can see the right tail for the density plot of WindSpeed9AM so probably a outlier
ggplot(data,aes(x=WindSpeed9am)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()
# You can notice the extreme outlier for WindSpeed9AM in boxplot
ggplot(data,aes(x=WindSpeed9am)) + geom_boxplot()
ggplot(data,aes(x=WindSpeed3pm)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()
ggplot(data,aes(x=Humidity9am)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()
ggplot(data,aes(x=Humidity3pm)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()
ggplot(data,aes(x=Pressure9am)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()
ggplot(data,aes(x=Pressure3pm)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()
ggplot(data,aes(x=Cloud9am)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()
ggplot(data,aes(x=Cloud3pm)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()
ggplot(data,aes(x=Temp9am)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()
ggplot(data,aes(x=Temp3pm)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()
ggplot(data,aes(x=RainToday)) + geom_bar()
ggplot(data,aes(x=RainTomorrow)) + geom_bar()

write_csv(data,"cleaned_data.csv")


data_long <- melt(as.data.frame(data[,c("Evaporation","WindSpeed9am","WindGustSpeed","Rainfall")]))
data_long
  ggplot(data_long, aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Outlier Detection") +
  xlab("")

grid.arrange(
  ggplot(data, aes(x = Cloud9am)) +
    geom_dotplot(binwidth = 0.5, method = "histodot", fill = "#69b3a2",dotsize = 0.5) +
    scale_x_continuous(breaks = seq(0, 12, 1)) + labs(title = "Cloud9am Cover Outlier",subtitle="Cloud Cover's range must be between 0 to 8") +
    theme_bw(),
  ggplot(data, aes(x = Cloud3pm)) +
    geom_dotplot(binwidth = 0.5, method = "histodot", fill = "#404080",dotsize = 0.5) +
    scale_x_continuous(breaks = seq(0, 12, 1)) + labs(title = "Cloud3pm Cover Outlier",subtitle="Cloud Cover's range must be between 0 to 8") +
    theme_bw(),ncol =2)

grid.arrange(ggplot(data,aes(x=MinTemp)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()+theme_bw(),
             ggplot(data,aes(x=MaxTemp)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()+theme_bw(),
             ggplot(data,aes(x=Rainfall)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density() + scale_x_log10()+theme_bw(),
             ggplot(data,aes(x=Sunshine)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()+theme_bw(),
             ggplot(data,aes(x=WindGustSpeed)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()+theme_bw(),
             ggplot(data,aes(x=WindSpeed9am)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()+theme_bw(),
             ggplot(data,aes(x=WindSpeed3pm)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()+theme_bw(),
             ggplot(data,aes(x=Humidity9am)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()+theme_bw(),
             ggplot(data,aes(x=Humidity3pm)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()+theme_bw(),
             ggplot(data,aes(x=Pressure9am)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()+theme_bw(),
             ggplot(data,aes(x=Pressure3pm)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()+theme_bw(),
             ggplot(data,aes(x=Cloud9am)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()+theme_bw(),
             ggplot(data,aes(x=Cloud3pm)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()+theme_bw(),
             ggplot(data,aes(x=Temp9am)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()+theme_bw(),
             ggplot(data,aes(x=Temp3pm)) + geom_histogram(aes(y= ..density..),fill="cornflowerblue") + geom_density()+theme_bw(),
             ggplot(data,aes(x=RainToday)) + geom_bar(fill ="cornflowerblue",colour= "black") +theme_bw(),
             nrow = 4, ncol =4)
