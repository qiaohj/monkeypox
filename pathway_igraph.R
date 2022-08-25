library(igraph)
library(ggraph)
library(graphlayouts)
library(ggpubr)
setwd("/media/huijieqiao/SSD_Fast/monkeypox/monkeypox")
imported_cases_se<-readRDS("../Tables/imported_cases_se.rda")

actors <- data.frame(name=unique(c(imported_cases_se$Travel_ISO3, imported_cases_se$Country_ISO3)),
                     color="Normal")
actors[which(actors$name %in% c("ESP", "DEU", "PRT", "USA")),]$color<-"Key"
#actors<-merge(actors, imported_cases_se, by.x="name", by.y="Travel_ISO3", all.x=T)
relations <- data.frame(from=imported_cases_se$Travel_ISO3,
                        to=imported_cases_se$Country_ISO3,
                        N=imported_cases_se$N)

g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)
plot(g)
l <- layout_with_fr(g)
plot(g, vertex.color="white", layout=l, edge.curved=.3)

ggraph(g, layout = "graphopt")+
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name)), 
                 arrow = arrow(length = unit(4, 'mm')))+
  geom_node_text(aes(label = name))+
  theme_graph()+
  coord_fixed()

from <- match(relations$from, actors$name)
to <- match(relations$to, actors$name)

V(g)
V(g)$degree <- degree(g, mode = "out")
p<-ggraph(g, layout="graphopt")+
  
  geom_edge_link(
    aes(end_cap = circle(node2.degree + 13, "pt")),
    edge_colour = "grey25",
    arrow = arrow(
      angle = 10,
      length = unit(0.15, "inches"),
      ends = "last",
      type = "closed"
    )
  ) +
  geom_node_point(aes(fill=color), shape = 21,
                  size = graph.strength(g) + 10,)+
  geom_node_text(aes(label = name), repel = F)+
  theme_graph(background="white")+
  scale_fill_manual(values=c("#FF3333", "#0072B2"))+
  theme(legend.position = "none")
p
ggsave(p, filename=sprintf("../Figures/Imported_Cases_Graph/Imported_Cases_Graph_%s.png", "World"),
       width=12, height=12)
#Spain
plist<-list()
for (key in c("ESP", "DEU", "PRT", "USA")){
  imported_cases_se_spain<-imported_cases_se[Country_ISO3==key|Travel_ISO3==key]
  actors <- data.frame(name=unique(c(imported_cases_se_spain$Travel_ISO3, imported_cases_se_spain$Country_ISO3)),
                       color="Normal")
  actors[which(actors$name %in% c(key)),]$color<-"Key"
  
  relations <- data.frame(from=imported_cases_se_spain$Travel_ISO3,
                          to=imported_cases_se_spain$Country_ISO3,
                          N=imported_cases_se_spain$N,
                          in_out="in")
  relations[which(relations$from==key), "in_out"]<-"out"
  g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
  V(g)$degree<-degree(g)
  ppp<-ggraph(g,layout = "focus", focus = 1)+
    #draw_circle(col = "#00BFFF", use = "focus",max.circle = 2)+
    geom_edge_link(
      aes(end_cap = circle(node2.degree + 18, "pt"), label=N, 
          edge_color=in_out),
      arrow = arrow(
        angle = 20,
        length = unit(0.2, "inches"),
        ends = "last",
        type = "closed"
      )
    ) +
    geom_node_point(aes(fill=color), shape = 21,
                    size = graph.strength(g) + 15, fill="white")+
    geom_node_text(aes(label = name), repel = F)+
    theme_graph(background="white")+
    scale_edge_color_manual(values=c("in"="#0072B2", out="#FF3333"))+
    theme(legend.position = "none")
  ggsave(ppp, filename=sprintf("../Figures/Imported_Cases_Graph/Imported_Cases_Graph_%s.png", key),
         width=6.5, height=6.5)
  plist[[length(plist)+1]]<-ppp
  plist[[length(plist)+1]]<-ggplot()
}
p1<-ggarrange(plotlist=plist, nrow=1, ncol=8, widths=rep(c(1, 0), 4))
p1
ggarrange(plotlist=list(p, p1), nrow=2, ncol=1)
