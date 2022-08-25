library(ggmap)
library(gridExtra)
library(dplyr)
library(raster)
setwd("/media/huijieqiao/SSD_Fast/monkeypox/monkeypox")

map<-readRDS("../Data/background.rda")
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

