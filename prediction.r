library(data.table)
library(sf)
library(ggplot2)
setwd("/media/huijieqiao/SSD_Fast/monkeypox/monkeypox")
flight_data_se<-readRDS("../Tables/flight_data_se.rda")
flight_data_se<-flight_data_se$flight_data_se
target_countries_with_cases<-readRDS("../Tables/target_countries_with_cases.rda")
target_countries_with_cases_2022<-target_countries_with_cases[year==2022]


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
  geom_point(aes(x=N, y=sqrt(N_cases), color=recovered, size=N_cases), alpha=0.5)+
  geom_smooth(aes(x=N, y=sqrt(N_cases)), method="lm")+
  #geom_text(x = 1000, y = 100, label = eq(log10(target_countries_with_cases[year==2022]$N),
  #                                         log10(target_countries_with_cases[year==2022]$N_cases)), 
  #          parse = TRUE)+
  
  #scale_shape_manual(values=c(4, 9))+
  #geom_text(aes(x=N, y=N_cases, label=destination_country_iso3))+
  scale_color_manual(breaks=c("Yes", "No"), values=c("#FF3333", "#0072B2"))+
  #scale_y_log10(breaks=c(1, 10, 1e2, 1e3, 1e4),
  #              labels=c("1", "10", "100", "1K", "10K"))+
  #scale_x_log10(
  #  breaks=c(10, 1e2, 1e3, 1e4, 1e5),
  #  labels=c("10", "100", "1K", "10K", "100K"))+
  
  labs(x="Number of flights", y="Number of cases (sqrt-transformed)", 
       shape="Is recovered", color="MP Level",
       title=eq((target_countries_with_cases[year==2022]$N),
                           sqrt(target_countries_with_cases[year==2022]$N_cases)))+
  theme_bw()+
  theme(legend.position = "none")
p
ggsave(p, filename="../Figures/Prediction/lm.pdf", width=5, height=4)

lm<-lm(data=target_countries_with_cases_2022,
       sqrt(N_cases)~N_2022)
summary(lm)$r.squared

data_2022<-flight_data_se[year==2022]
colnames(data_2022)[4]<-"N_2022"


data_2022$predicted_cases_2022<-predict(lm, data_2022)
data_2019<-flight_data_se[year==2019]
colnames(data_2019)[4]<-"N_2022"
data_2019$predicted_cases_2019<-predict(lm, data_2019)
data_predicted<-merge(data_2019, data_2022, by=c("destination_country", "destination_country_iso3"), all=T)
data_predicted[is.na(predicted_cases_2019)]$predicted_cases_2019<-0
data_predicted[is.na(predicted_cases_2022)]$predicted_cases_2022<-0
data_predicted[predicted_cases_2019<predicted_cases_2022]$predicted_cases_2019<-
  data_predicted[predicted_cases_2019<predicted_cases_2022]$predicted_cases_2022  

countries<-read_sf("../Shape/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
countries_merc <- st_transform(countries, st_crs("+proj=merc"))
countries_merc<-countries_merc[which(countries_merc$ISO3!="ATA"),]
countries_predicted<-merge(countries_merc, data_predicted, by.x="ISO3", by.y="destination_country_iso3",
                      all=T)

countries_predicted$predicted_cases_2019<-countries_predicted$predicted_cases_2019^2
countries_predicted$predicted_cases_2022<-countries_predicted$predicted_cases_2022^2

p_2022<-ggplot() + 
  geom_sf(data=countries_predicted, aes(fill=predicted_cases_2022), inherit.aes = FALSE)+
  coord_sf()+
  scale_fill_gradient(high="#FF3333", low="#ffe0e0")+
  labs(fill="N cases")+
  theme_bw()
p_2022
ggsave(p_2022, filename="../Figures/Prediction/2022_flight_based_prediction.pdf", width=6, height=4)
p_2019<-ggplot() + 
  geom_sf(data=countries_predicted, aes(fill=predicted_cases_2019), inherit.aes = FALSE)+
  coord_sf()+
  scale_fill_gradient(high="#FF3333", low="#ffe0e0")+
  labs(fill="N cases")+
  theme_bw()

ggsave(p_2019, filename="../Figures/Prediction/2019_flight_based_prediction.pdf", width=6, height=4)

countries_predicted$differ<-countries_predicted$predicted_cases_2019-countries_predicted$predicted_cases_2022
countries_predicted[which(countries_predicted$differ<=0), "differ"]<-0
p_diff<-ggplot() + 
  geom_sf(data=countries_predicted, 
          aes(fill=differ), inherit.aes = FALSE)+
  coord_sf()+
  scale_fill_gradient2(high="#FF3333", mid="white", low="#0072B2", midpoint=0)+
  labs(fill="2019-2022")+
  theme_bw()
p_diff
ggsave(p_diff, filename="../Figures/Prediction/prediction_differ.pdf", width=6, height=4)


