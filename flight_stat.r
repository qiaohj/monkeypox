library(data.table)
library(ggplot2)
library(sf)
library(raster)
library(scales)
library(viridis)
library(RColorBrewer)
sf_use_s2(FALSE)
setwd("/media/huijieqiao/SSD_Fast/monkeypox/monkeypox")
dates<-expand.grid(y=c(2019:2022), m=c(5:6))
i=1
all_data<-list()
for (i in c(1:nrow(dates))){
  print(paste(i, nrow(dates)))
  date<-dates[i,]
  filename<-sprintf("../Tables/Flight_Data/flightlist_%d0%d01_%d0%d30.rda", date$y, date$m, date$y, date$m)
  if (!file.exists(filename)){
    filename<-sprintf("../Tables/Flight_Data/flightlist_%d0%d01_%d0%d31.rda", date$y, date$m, date$y, date$m)
  }
  flight_data<-readRDS(filename)
  flight_data$month<-date$m
  flight_data$year<-date$y
  all_data[[i]]<-flight_data
}
all_data_bind<-rbindlist(all_data)
all_data_bind<-all_data_bind[origin_country_iso3!=""&
                               destination_country_iso3!=""&
                               origin_country_iso3!=destination_country_iso3]

if (F){
  mask_1<-mask_fine
  values(mask_1)[!is.na(values(mask_1))]<-c(1:length(values(mask_1)[!is.na(values(mask_1))]))
  plot(mask_1)
  writeRaster(mask_1, "../Raster/mask_0.1_degree.tif")
  
  mask<-raster("../Raster/alt_zones_v31.tif")
  mask_mo<-projectRaster(mask, res=c(50000, 50000), crs=crs("+proj=merc"))
  plot(mask_mo)
  values(mask_mo)[!is.na(values(mask_mo))]<-c(1:length(values(mask_mo)[!is.na(values(mask_mo))]))
  writeRaster(mask_mo, "../Raster/mask_mercator_50km.tif")
  
  mask<-raster("../Raster/alt_zones_v31.tif")
  mask_0.5<-projectRaster(mask, res=c(0.5, 0.5), crs=crs(mask))
  plot(mask_0.5)
  values(mask_0.5)[!is.na(values(mask_0.5))]<-c(1:length(values(mask_0.5)[!is.na(values(mask_0.5))]))
  writeRaster(mask_0.5, "../Raster/mask_0.5_degree.tif")
  
  mask<-raster("../Raster/alt_zones_v31.tif")
  mask_0.2<-projectRaster(mask, res=c(0.2, 0.2), crs=crs(mask))
  plot(mask_0.2)
  values(mask_0.2)[!is.na(values(mask_0.2))]<-c(1:length(values(mask_0.2)[!is.na(values(mask_0.2))]))
  writeRaster(mask_0.2, "../Raster/mask_0.2_degree.tif")
}
mask<-raster("../Raster/mask_0.2_degree.tif")
#mask_lonlat<-raster("../Raster/mask_0.1_degree.tif")
#mask_fine<-projectRaster(mask, res=c(0.1, 0.1), crs=crs(mask))
#plot(mask_fine)
#values(mask_fine)[!is.na()]
cols<-c("longitude_2", "latitude_2")
#all_data_bind_points<-st_as_sf(all_data_bind, coords=cols, crs=st_crs(mask_lonlat))
#all_data_bind_points_mer<-st_transform(all_data_bind_points, crs=st_crs(mask))
#all_data_bind$x<-st_coordinates(all_data_bind_points_mer)[,1]
#all_data_bind$y<-st_coordinates(all_data_bind_points_mer)[,2]
#cols<-c("x", "y")
all_data_bind$index<-extract(mask, all_data_bind[, ..cols])
saveRDS(all_data_bind, "../Tables/all_international_flights.rda")
all_data_bind<-all_data_bind[!is.na(index)]
all_data_bind_se<-all_data_bind[, .(N=.N), by="index"]
points<-data.table(rasterToPoints(mask))
points<-merge(points, all_data_bind_se, by.x="mask_0.2_degree", by.y="index", all=T)
#points_xx<-merge(points, all_data_bind_se, by.x="mask_mercator_50km", by.y="index", all=F)
#plot(points_xx[lat>-70]$x, points_xx[lat>-70]$y, pch=".")
#plot(points$x, points$y)
values(mask)[!is.na(values(mask))]<-points$N
plot(mask)
hist(points[!is.na(N)&N<100]$N)
#points_p<-st_as_sf(points, coords=cols, crs=st_crs(mask))
#points_p<-st_transform(points_p, crs=st_crs(mask_lonlat))
#points$lon<-st_coordinates(points_p)[,1]
#points$lat<-st_coordinates(points_p)[,2]

saveRDS(points, "../Tables/flight_density.rda")

world <- map_data('world')
p<-ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill="white", color="black", size=0.1) +
  geom_tile(data=points[!is.na(N)], aes(x=x, y=y, fill=N))+
  
  coord_equal()+
  geom_map(map=world, aes(map_id=region), fill=NA, color="black", size=0.1) +
  
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
    plot.background = element_rect(fill = "blue", color = NA), 
    panel.background = element_blank(), 
    legend.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.border = element_blank(),
    legend.position="bottom",
    legend.key.width=unit(0.8,"in"),
    #strip.background.x = element_blank(),
    #strip.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
p
ggsave(p, filename="../Figures/Flights/Flights_Density.png", width=9, height=6)
ggsave(p, filename="../Figures/Flights/Flights_Density.pdf", width=9, height=6)

#Top 10 busiest countries
flight_data_se<-all_data_bind[, .(N=.N), 
                              by=list(destination_country, destination_country_iso3, year)]

flight_data_se_all<-all_data_bind[, .(N=.N), 
                              by=list(destination_country, destination_country_iso3)]
saveRDS(list(flight_data_se=flight_data_se, flight_data_se_all=flight_data_se_all),
        "../Tables/flight_data_se.rda")
setorderv(flight_data_se_all, "N", c(-1))

p<-ggplot(flight_data_se[destination_country_iso3 %in% flight_data_se_all[1:10]$destination_country_iso3])+
  geom_bar(aes(x=destination_country, y=N, fill=factor(year)), stat="identity", position = "dodge2")+
  scale_fill_viridis_d()+
  theme_bw()+
  labs(fill="", x="Destination", "Flights")
ggsave(p, filename="../Figures/Flights/overview.pdf", width=10, height=5)

p<-ggplot(flight_data_se[destination_country_iso3 %in% flight_data_se_all[1:10]$destination_country_iso3])+
  geom_line(aes(x=year, y=N, color=destination_country_iso3))+
  geom_text(data=flight_data_se[year==2022 & 
                                  destination_country_iso3 %in% 
                                  flight_data_se_all[1:10]$destination_country_iso3],
            aes(x=2022.1, y=N, label=destination_country_iso3))+
  scale_color_viridis_d()+
  theme_bw()+
  labs(color="", x="Year", y="Flights")+
  theme(legend.position = "none")
p
ggsave(p, filename="../Figures/Flights/overview_line.pdf", width=10, height=5)

write.csv(flight_data_se, "../Tables/flight_data_se.csv")
quantile(flight_data_se$N)

target_countries<-flight_data_se
target_countries<-target_countries[, .(N=.N), by=list(destination_country)]
target_countries<-target_countries[N==4]
target_countries<-flight_data_se[destination_country %in% target_countries$destination_country]

flight_data_2019<-flight_data_se[year==2019]
flight_data_2022<-flight_data_se[year==2022]
colnames(flight_data_2019)[c(3,4)]<-c("y_2019", "N_2019")
colnames(flight_data_2022)[c(3,4)]<-c("y_2022", "N_2022")
flight_data_change<-merge(flight_data_2019, flight_data_2022, 
                          by=c("destination_country", "destination_country_iso3"))
flight_data_change$change<-flight_data_change$N_2022/flight_data_change$N_2019
flight_data_change$recovered<-"No"
flight_data_change[change>=0.9]$recovered<-"Yes"
target_countries<-merge(target_countries, flight_data_change, 
                        by=c("destination_country", "destination_country_iso3"))
local_cases_se<-readRDS("../Tables/local_cases_se.rda")
colnames(local_cases_se)[5]<-"N_cases"
target_countries_with_cases<-merge(target_countries, local_cases_se, 
                                   by.x=c("destination_country", "destination_country_iso3"),
                                   by.y=c("NAME", "Country_ISO3"))
target_countries_with_cases$monkeypox_level<-"Light"
target_countries_with_cases[N_cases>=100]$monkeypox_level<-"Mid"
target_countries_with_cases[N_cases>=1000]$monkeypox_level<-"Caution"

flight_data_se[destination_country=="Nigeria"]

target_countries_with_cases[is.na(N)]
ggplot(target_countries_with_cases)+
  geom_line(aes(x=year, y=N, group=destination_country, linetype=recovered, color=monkeypox_level))+
  scale_y_log10()



target_countries_with_cases$recovered_int<-9
target_countries_with_cases[recovered=="No"]$recovered_int<-4

eq <- function(x,y) {
  m <- lm(y ~ x)

    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3))
    )
  )
}
p<-ggplot(target_countries_with_cases[year==2022])+
  geom_point(aes(x=N, y=N_cases, color=recovered, size=N_cases * 2), alpha=0.5)+
  #geom_smooth(aes(x=N, y=N_cases), method="glm")+
  #geom_text(x = 1000, y = 100, label = eq(log10(target_countries_with_cases[year==2022]$N),
  #                                         log10(target_countries_with_cases[year==2022]$N_cases)), 
  #          parse = TRUE)+
  
  #scale_shape_manual(values=c(4, 9))+
  geom_text(aes(x=N, y=N_cases, label=destination_country_iso3), size=1)+
  scale_color_manual(breaks=c("Yes", "No"), values=c("#FF3333", "#0072B2"))+
  scale_y_log10(breaks=c(1, 10, 1e2, 1e3, 1e4),
                labels=c("1", "10", "100", "1K", "10K"))+
  scale_x_log10(
    breaks=c(10, 1e2, 1e3, 1e4, 1e5),
    labels=c("10", "100", "1K", "10K", "100K"))+
  
  #labs(x="Number of flights", y="Number of cases", shape="Is recovered", color="MP Level",
  #     title=eq((target_countries_with_cases[N>=1000&year==2022]$N),
  #                         log10(target_countries_with_cases[N>=1000&year==2022]$N_cases)))+
  labs(x="Number of flights", y="Number of cases", color="Is recovered")+
  theme_bw()
p

saveRDS(target_countries_with_cases, "../Tables/target_countries_with_cases.rda")
target_countries_with_cases[monkeypox_level=="Caution"&N<1e4&year==2022]
ggsave(p, filename="../Figures/Flight_Cases/Flight_Cases_with_labels.pdf", width=5, height=4)  
  
p<-ggplot(target_countries_with_cases[year==2022])+
  geom_point(aes(x=N, y=N_cases, color=recovered, size=N_cases * 2), alpha=0.5)+
  #geom_smooth(aes(x=N, y=N_cases), method="glm")+
  #geom_text(x = 1000, y = 100, label = eq(log10(target_countries_with_cases[year==2022]$N),
  #                                         log10(target_countries_with_cases[year==2022]$N_cases)), 
  #          parse = TRUE)+
  
  #scale_shape_manual(values=c(4, 9))+
  #geom_text(aes(x=N, y=N_cases, label=destination_country_iso3))+
  scale_color_manual(breaks=c("Yes", "No"), values=c("#FF3333", "#0072B2"))+
  scale_y_log10(breaks=c(1, 10, 1e2, 1e3, 1e4),
                labels=c("1", "10", "100", "1K", "10K"))+
  scale_x_log10(
    breaks=c(10, 1e2, 1e3, 1e4, 1e5),
    labels=c("10", "100", "1K", "10K", "100K"))+
  
  #labs(x="Number of flights", y="Number of cases", shape="Is recovered", color="MP Level",
  #     title=eq((target_countries_with_cases[N>=1000&year==2022]$N),
  #                         log10(target_countries_with_cases[N>=1000&year==2022]$N_cases)))+
  labs(x="Number of flights", y="Number of cases", color="Is recovered")+
  theme_bw()+
  theme(legend.position = "none")
p


ggsave(p, filename="../Figures/Flight_Cases/Flight_Cases_without_labels.pdf", width=5, height=4)  

