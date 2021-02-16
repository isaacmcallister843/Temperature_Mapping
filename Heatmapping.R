# -----------------------  Libraries
library("tidyverse")
library("ggplot2")
library('gridExtra')
library("dplyr")
library("reshape")
library("class")
library("raster")

# ----------------------- Global Var
generate_frame = FALSE
set.seed(123)
file.names <- dir(path_data, pattern ="csv")
path_data = "C:/Users/dmcallister/Desktop/Thermal_map_camera/Temp_data"
path_out = "C:/Users/dmcallister/Desktop/Thermal_map_camera/Temp_processed"
raster_path = "C:/Users/dmcallister/Desktop/Thermal_map_camera/Rasters"
heat_path = "C:/Users/dmcallister/Desktop/Thermal_map_camera/Heatmaps_long"

# ----------------------- Data Importing and Processing 
if (generate_frame){
  setwd(path_data)
  proccess <- function(test_mod){
    date1 = test_mod[,1][1]
    tav = 0 
    count = 0 
    test_final <- c()
    for (i in 1:nrow(test_mod)){
      date = test_mod[,1][i]
      if(date == date1){
        tav = tav + as.numeric(test_mod[,2][i])
        count = count + 1
      }
      else{
        tav = tav / count
        test_final = rbind(test_final, c(as.character(date), tav))
        tav = 0 
        count = 0
        date1 = date
      }
      
    }
    test_final <- as.data.frame(test_final)
    rownames(test_final) <- 1:nrow(test_final)
    return(test_final) 
  }
  
  
  for(i in 1:length(file.names)){
    setwd(path_data)
    file <- read.csv(file.names[i])
    out_file <- cbind(file$Date , file$Temperature)
    out_file <- as.data.frame(out_file)
    
    for (item in 1:(nrow(out_file))){
      out_file[item,2] <- as.numeric(str_split(out_file[item,2],"Â")[[1]][1])
    }
    
    name = str_split(file.names[i], pattern = ".csv")[[1]][1]
    out_file <- proccess(out_file)
    
    # Check date format 
    if (str_split(out_file[1,1], pattern = "-")[[1]][3] == "2019"){
      out_file$V1 <- as.Date(out_file$V1, format = "%d-%b-%Y",origin="1970-01-01")
    }else{
      out_file$V1 <- as.Date(out_file$V1, format = "%d-%b-%y",origin="1970-01-01")
    }
    out_file$V2 <- as.numeric(out_file$V2)
    
    colnames(out_file) <- c("V1", paste(name, "_Temp", sep=""))
    print(i)
    
    if (i == 1){
      merged_frame = out_file
    } else {
      merged_frame = merge(merged_frame, out_file ,by = "V1", all=T)
      
    }
    
    setwd(path_out)
    write.csv(out_file,name)
    assign(name, out_file)
    
  }
  write.csv(merged_frame, "merged_frame.csv")
  
}else{
  setwd(path_out)
  merged_frame <- read.csv("merged_frame.csv")
  merged_frame <- merged_frame[, c(2:ncol(merged_frame))]
}

# -------------------- Heat Map plotting
setwd("C:/Users/dmcallister/Desktop/Thermal_map_camera/")

# --------- Create standarized coordinate system 
points <- read.csv("Wildlife_Cam_Points.csv")

# ---------------------- Heat map function 

generate_heat_map <- function(merged_frame, points, date, low_temp=-2, high_temp=12, 
                              raster=TRUE, inter = FALSE, save = FALSE, by_var = 2, single=FALSE){
  # -------- Coordinate Systems
  xy_coords <- cbind(points$Easting, points$Northing)
  #xy_coords <- xy_coords[-c(4, 30, 34),] # Removing for camera data we don't have
  
  # For scaling
  adjust = (max(xy_coords[,1]) - min(xy_coords[,1])) / (max(xy_coords[,2]) - min(xy_coords[,2])) 
  
  # --------  Generate list of over points 
  grid = c()
  
  for (i in 1:nrow(xy_coords)){
    for (j in 1:nrow(xy_coords)){
      grid = rbind(grid, c(xy_coords[i,1], xy_coords[j,2]))
    }
  }
  
  # -------- Get data temperatures merge with standard coordinate system
  if(single){
    date1 = merged_frame
    date1 = date1[,2:ncol(date1)]
    rownames(date1)=NULL
    date1 = as.numeric(date1)
    date1 = t(date1)
  }else{
    date1 <- merged_frame[merged_frame[,1] == date, c(2:ncol(merged_frame))]
  }
  
  heat_frame <- as.data.frame(cbind(xy_coords[,1],xy_coords[,2],t(date1) ) )
  rownames(heat_frame) <- NULL
  colnames(heat_frame) <- c("X", "Y", "Data")
  
  heat_frame <- heat_frame[complete.cases(heat_frame),]
  heat_frame[,3]=as.numeric(heat_frame[,3])
  # ------- KNN 
  training_set = as.matrix(cbind(scale(heat_frame[-3]), heat_frame[3]))
  test_set = scale(grid)
  
  y_pred = knn(train = training_set[, -3],
               test = test_set,
               cl = training_set[, 3],
               k = 3,
               prob = TRUE)
  
  y_pred_standarized = as.numeric(as.character(y_pred))
  
  output_frame = as.data.frame(cbind(grid, y_pred_standarized))
  
  # ------- Heat Map Generation
  title = paste(paste(date, "Cameras Reporting:", sep=" "), nrow(heat_frame))
  
  heat_map <- ggplot(output_frame, aes(x=as.factor(V1), y=as.factor(V2))) +
    geom_tile(aes(fill = y_pred_standarized)) +
    scale_fill_gradient(low ="#001c80", high = "#ff0000", limits=c(low_temp, high_temp), 
                        breaks=seq(low_temp,high_temp,by=by_var),name = "Temperature (C)" )+ 
    theme(axis.text.x = element_text(angle = 90))+
    xlab("Easting Value")+
    ylab("Northing Value")+ 
    ggtitle(title) + 
    guides(fill=guide_legend(reverse=TRUE))
   
  heat_map 

  if (save){
    ggsave(plot=heat_map, paste(date, "-heat_map.JPG",sep=""), device = "jpg")
    
  }
  # ------- Raster Creation 
  
  if (raster){
    raster_img <- ggplot(output_frame, aes(x=as.factor(V1), y=as.factor(V2))) +
                  geom_raster(aes(fill = y_pred_standarized), interpolate = inter) +
                  scale_fill_gradient(low = "blue", high = "red", limits=c(low_temp, high_temp), breaks=seq(low_temp,high_temp,by=2))+ 
                  theme(    
                      axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(),
                      axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      plot.margin=grid::unit(c(-2,-2,-2,-2), "mm"),
                      legend.position = 'none',
                      plot.background = element_rect(fill=NA, colour=NA)
                    ) + labs(x=NULL, y=NULL)
    
    ggsave(plot=raster_img, paste(date, "-sample.JPG",sep=""), device = "tiff",width = 7, height = 7 *adjust)
    raster = raster(paste(date, "-sample.JPG",sep=""))
    extent(raster) = c(c(min(points$Easting), max(points$Easting)),c(min(points$Northing), max(points$Northing)))
    projection(raster) <- CRS("+init=epsg:32619")
    
    slope = (max(output_frame$y_pred_standarized) - min(output_frame$y_pred_standarized)) / (max(values(raster))-min(values(raster)))
    values_new = values(raster) * slope + (max(output_frame$y_pred_standarized) - slope*max(values(raster)))
    values(raster) = values_new
    writeRaster(raster, filename=paste(date, "-raster.TIFF",sep=""), format="GTiff", overwrite=TRUE)
    return(list(heat_map = heat_map, raster_img = raster_img, raster =raster))
  
  }
  
  # Function Output
  return(list(heat_map = heat_map, raster =raster))
  
}

# ------------------- Heat Map Generation

generate_heat_map(merged_frame, points,"2019-09-07")
generate_heat_map(merged_frame, points,"2019-09-08")
generate_heat_map(merged_frame, points,"2019-09-09")
generate_heat_map(merged_frame, points,"2019-09-10")
generate_heat_map(merged_frame, points,"2019-09-11")
generate_heat_map(merged_frame, points,"2019-09-12")
generate_heat_map(merged_frame, points,"2019-09-13")
generate_heat_map(merged_frame, points,"2019-09-14")

# ----------------- Heat Mapping for time
date_get="2019-08-10"
setwd("C:/Users/dmcallister/Desktop/Thermal_map_camera/Camera Data")
file.names <- dir(pattern ="csv")

date_frame_final <- c()
for (i in 1:length(file.names)){
  date_frame <- read.csv(file.names[i])
  
  dates <- date_frame$CreateDate
  temp  <- date_frame$AmbientTemperature
  
  day = c()
  hour = c()
  
  for (j in 1:length(dates)){
    val = str_split(dates[j], pattern=" ")
    if(str_split(val[[1]][2], patter =":")[[1]][3] == "00"){
      day = rbind(day, val[[1]][1])
      hour = rbind(hour, val[[1]][2])  
    }
  }
  
  out_frame <- as.data.frame(cbind(day,hour,temp))
  out_frame$V1 <- as.Date(out_frame$V1,  format = "%Y:%m:%d")
  
  combine = out_frame[out_frame$V1 == date_get & 
              (out_frame$V2 == "15:00:00" |  out_frame$V2 == "16:00:00"), ]
  
  if(i==1){
    date_frame_final = combine
  }else{
    date_frame_final = cbind(date_frame_final,combine[3])
  }
}


frame_1 <- date_frame_final[1,-2]
frame_2 <- date_frame_final[2,-2]
setwd("C:/Users/dmcallister/Desktop/Thermal_map_camera/hourlymaps")

test <- generate_heat_map(frame_1, points,date_get,low_temp = 0, high_temp = 12, 
                  raster=TRUE, single=TRUE, save=TRUE, inter = TRUE)
test$raster_img
test2 <- generate_heat_map(frame_2, points,date_get,low_temp = 0, high_temp = 12, 
                          raster=TRUE, single=TRUE, save=TRUE)

# ------------------- K means Clustering 
library(cluster)
training_set = heat_frame 

training_set = scale(training_set)
test_set = scale(test_set)

set.seed(29)
kmeans = kmeans(x = training_set, centers = 8)
y_kmeans = kmeans$cluster

clusplot(training_set,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Temperature Clustering'),
         xlab = 'Normalized Easting Value',
         ylab = 'Normalized Northing Value')


# ----------------------- Plotting Facet Grid 

df <- melt(merged_frame ,  id.vars = 'V1', variable.name = 'series')

ggplot(df, aes(V1,value)) + 
  geom_point(size=.8, color="#42a4f5") +
  coord_cartesian(ylim = c(-50, 15))+ 
  ylab("Temperature (C)")+
  xlab("Time") +
  facet_wrap(variable ~ .)

# -------------------- SWI value 
SWI_cal <- function(frame, date1, date2){

  list <- unlist(frame[date1 < frame$V1 & frame$V1 < date2, c(2:ncol(frame))])
  list <- list[!is.na(list)]
  count = 0
  sum = 0 
  
  for(j in 1:length(list)){
    if(list[j] > 0 ){
      sum = sum + list[j]
      count = count + 1
    }
  }
  
  SWI = sum/count
  return(SWI)
}

sep_2019 <- SWI_cal(merged_frame, "2019-09-01", "2020-10-01")

# ------------------- Mean annual temp 
setwd("C:/Users/dmcallister/Desktop/Thermal_map_camera/")

ClimNA <- list.files(getwd(), "*\\.asc", recursive=FALSE, full.names=TRUE,  ignore.case = TRUE) 
r1 <- raster(ClimNA, package="raster", native=TRUE)
writeRaster(r1,"test" ,format="GTiff")
raster = raster("test.tif")

plot(raster, main = "MAT Temp (°C)")
values(raster)






