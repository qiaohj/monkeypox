library(data.table)
library(ggplot2)
library(sf)
sf_use_s2(FALSE)
setwd("/media/huijieqiao/SSD_Fast/monkeypox/monkeypox")
regions_shp<-read_sf("../Shape/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")

files<-list.files("../Data/opensky_202206_csv", pattern="\\.csv")
files<-gsub("\\.csv", "", files)
filename<-files[5]
for (filename in files){
  target<-sprintf("../Tables/Flight_Data/%s.rda", filename)
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  flight_data<-fread(sprintf("../Data/opensky_202206_csv/%s.csv", filename))
  flight_data$origin_country<-""
  flight_data$origin_country_iso3<-""
  flight_data$destination_country<-""
  flight_data$destination_country_iso3<-""
  flight_data<-flight_data[(!is.na(flight_data$longitude_1))&
                             (!is.na(flight_data$longitude_2))&
                             (!is.na(flight_data$latitude_1))&
                             (!is.na(flight_data$latitude_2))]
  flight_data$ID<-c(1:nrow(flight_data))
  projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  cols<-c("ID", "longitude_1", "latitude_1", "origin")
  flight_data_origin<-flight_data[, ..cols]
  flight_data_origin$lon<-flight_data_origin$longitude_1
  flight_data_origin$lat<-flight_data_origin$latitude_1
  
  flight_data_origin <- st_as_sf(x = flight_data_origin, 
                                 coords = c("longitude_1", "latitude_1"),
                                 crs = projcrs)
  
  cols<-c("ID", "longitude_2", "latitude_2", "destination")
  flight_data_destination<-flight_data[, ..cols]
  flight_data_destination$lon<-flight_data_destination$longitude_2
  flight_data_destination$lat<-flight_data_destination$latitude_2
  
  flight_data_destination <- st_as_sf(x = flight_data_destination, 
                                      coords = c("longitude_2", "latitude_2"),
                                      crs = projcrs)
  
  iso3<-"NGA"
  if (F){
    regions_shp[which(regions_shp$ISO3==iso3),]
    flight_data_origin[which(flight_data_origin$origin=="MMIA"),]
    point<-flight_data_origin[which(flight_data_origin$origin!=""),]
    point<-point[sample(nrow(point), 100000),]
    plot(point$geometry, pch=".")
  }
  for (i in c(1:length(unique(regions_shp$ISO3)))){
    iso3<-unique(regions_shp$ISO3)[i]
    print(paste(filename, i, length(unique(regions_shp$ISO3)), iso3))
    country<-regions_shp[which(regions_shp$ISO3==iso3),]
    box<-st_bbox(country)
    sub_points<-flight_data_origin[which(between(flight_data_origin$lon, box[1], box[3])&
                                           between(flight_data_origin$lat, box[2], box[4])),]
    flight_data_origin[which(between(flight_data_origin$lat, box[2], box[4])),]
    
    if (nrow(sub_points)==0){
      next()
    }
    in_points<-st_contains(country, sub_points)
    in_points<-unlist(in_points)
    if (length(in_points)==0){
      next()
    }
    sub_points<-sub_points[in_points, ]
    if (F){
      plot(country$geometry)
      plot(sub_points$geometry, add=T, pch=".")
      
      
      plot(sub_points$geometry)
      plot(country$geometry, add=T)
    }
    flight_data[which(ID %in% sub_points$ID)]$origin_country<-country$NAME
    flight_data[which(ID %in% sub_points$ID)]$origin_country_iso3<-iso3
    
    sub_points<-flight_data_destination[which(between(flight_data_destination$lon, box[1], box[3])&
                                                between(flight_data_destination$lat, box[2], box[4])),]
    if (nrow(sub_points)==0){
      next()
    }
    in_points<-st_contains(country, sub_points)
    in_points<-unlist(in_points)
    if (length(in_points)==0){
      next()
    }
    sub_points<-sub_points[in_points, ]
    if (F){
      plot(country$geometry)
      plot(sub_points$geometry, add=T, pch=".")
    }
    flight_data[which(ID %in% sub_points$ID)]$destination_country<-country$NAME
    flight_data[which(ID %in% sub_points$ID)]$destination_country_iso3<-iso3
    
  }
  saveRDS(flight_data, target)
}
