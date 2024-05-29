library(ggmap)
library(gridExtra)
library(dplyr)
library(raster)
setwd("/media/huijieqiao/WD10T_12/monkeypox/monkeypox")

map<-readRDS("../Data/background.rda")
if (F){
  library(ggmap)
  library(gridExtra)
  library(dplyr)
  library(raster)
  map<-readRDS("../Data/background.rda")
  ggmap_bbox <- function(map) {
    if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
    # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
    # and set the names to what sf::st_bbox expects:
    map_bbox <- setNames(unlist(attr(map, "bb")), 
                         c("ymin", "xmin", "ymax", "xmax"))
    
    # Coonvert the bbox to an sf polygon, transform it to 3857, 
    # and convert back to a bbox (convoluted, but it works)
    bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
    
    # Overwrite the bbox of the ggmap object with the transformed coordinates 
    attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
    attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
    attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
    attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
    map
  }
  map<-ggmap_bbox(map)
  world <- read_sf("../Shape/World/country_fixed.shp")
  world<-st_transform(world, st_crs(3857))
  #world[which(world$NAME=="Russia"),]
  write_sf(world, "../Shape/World/country_fixed_3857.shp")
  world_fixed<-read_sf("../Shape/World/country_fixed_3857.shp")
  p<-ggmap(map, darken = c(0.5, "white")) + 
    coord_sf(crs = st_crs(3857))+
    geom_sf(data=world_fixed,  aes(fill=NAME),
             fill=NA, color="grey", size=0.1, inherit.aes = FALSE) +
  
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(), 
      panel.background = element_blank(), 
      legend.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.border = element_blank(),
      legend.position="none",
      #legend.key.width=unit(0.8,"in"),
      #strip.background.x = element_blank(),
      #strip.text.x = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
  p
  ggsave(p, filename="../Figures/bg_map.pdf", width=5, height=3)
}
points<-readRDS("../Tables/flight_density.rda")
hist(points$N)
world <- map_data('world')
world

#points$N_RAW<-points$N
#points[N>10000]$N<-10000
ggplot()+geom_tile(data=points[!is.na(N)], aes(x=lon, y=lat, fill=N))
p<-ggmap(map, darken = c(0.5, "white")) + 
  geom_tile(data=points[!is.na(N)], aes(x=x, y=y, fill=N))+
  geom_map(data=world, map=world, aes(long, lat, map_id=region), 
           fill=NA, color="grey", size=0.1) +
  scale_fill_gradientn(colours=rev(magma(6)),
                       name="",
                       na.value = "grey100", 
                       trans = "log", 
                       breaks=c(1, max(points$N, na.rm=T)),
                       labels=c("Lowest", "Highest"))+
  labs(fill = "Number of arrival")+
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    legend.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.border = element_blank(),
    legend.position="none",
    #legend.key.width=unit(0.8,"in"),
    #strip.background.x = element_blank(),
    #strip.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
p
ggsave(p, filename="../Figures/Flights/Flights_Density.png", width=5, height=3)
ggsave(p, filename="../Figures/Flights/Flights_Density.pdf", width=5, height=3)

