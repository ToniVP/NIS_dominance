###############################!
#1: Descriptive figures #######!
###############################!
remove(list=ls())
#Load libraries and functions
source("./Code/Libraries_Functions.R")

# Fig. 2: Description maps ----
# NOTE: The stations map was saved in vector format and then the names of the regions were added manually in Adobe Illustrator.

# Fig. 2A: Dominance map for Marenzelleria ----
test_Mar = read.csv("./Data/Marenzelleria_data_metrics.txt",header=T,dec=".",sep="\t", check.names = FALSE)

#Dominance of Marenzelleria per region
dom_ar = test_Mar %>% mutate(area = as.factor(case_when(lat > 62.14347 ~ "Northern Baltic Sea",
                                                        lat < 57.53198 ~ "Central Baltic Sea",
                                                        lat < 62.14347 | lat > 57.53198 ~ "Southern Baltic Sea"))) %>% 
  group_by(station,area) %>% 
  dplyr::summarize(lon = mean(lon), lat = mean(lat), mean_dom=mean(NIS_relab), .groups = "drop_last") %>% 
  mutate(dom_NAT = 100-mean_dom)

pie_ar = test_Mar %>% mutate(area = as.factor(case_when(lat > 62.14347 ~ "Northern Baltic Sea",
                                                        lat < 57.53198 ~ "Southern Baltic Sea",
                                                        lat < 62.14347 | lat > 57.53198 ~ "Central Baltic Sea"))) %>% 
  group_by(area) %>% 
  dplyr::summarize(mean_dom=mean(NIS_relab), .groups = "drop_last") %>% 
  mutate(dom_NAT = 100-mean_dom, 
         lon = case_when(area %in% "Northern Baltic Sea" ~ 20.5,
                         area %in% "Central Baltic Sea" ~ 18.7,
                         area %in% "Southern Baltic Sea" ~ 20.3),
         lat = case_when(area %in% "Northern Baltic Sea" ~ 68.7,
                         area %in% "Central Baltic Sea" ~ 62,
                         area %in% "Southern Baltic Sea" ~ 57))


#Put the stations in the map plus the dominance of NIS vs natives

p <- ggplot(dom_ar, aes(x = lon, y = lat)) + 
  #scale_color_viridis(option = "A", discrete = T)+
  geom_hline(mapping = aes(yintercept = 62.14347), size = 2, linetype = 1, colour = "black")+
  geom_hline(mapping = aes(yintercept = 57.53198), size = 2, linetype = 1, colour = "black")+
  geom_point(dom_ar, mapping = aes(x = lon, y = lat, color = mean_dom), size = 2) +
  scale_color_gradient(low = "peachpuff", high = "orange2") + 
  borders(fill="grey55", colour = "grey5") + labs(color = "Mean relative\nbiomass") +
  theme(panel.background = element_rect(fill='aliceblue', colour = 'black')) +
  coord_quickmap(xlim=c(min(test_Mar$lon), max(test_Mar$lon)),ylim= c(min(test_Mar$lat),max(test_Mar$lat)))+
  labs(x = "Lon",y="Lat")+  
  annotate(geom = "label", x = c(15.5, 20.9, 20.9), y = c(65.5, 61.1, 55.2), 
           label = c("Northern Baltic Sea","Central Baltic Sea", "Southern Baltic Sea"), color = "black", size = 5) +
  theme(legend.position = "right", legend.title = element_text())+
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10))+
  guides(shape = guide_legend(override.aes = list(size = 1))); p


pie_plot <- ggplot() + theme_void() + 
  ylim(55, 70) + xlim(15, 26) + scale_fill_manual(
    values = c("orange2", "gray90")) + 
  geom_scatterpie(data = pie_ar, mapping = aes(x=lon, y=lat, r = 100*0.008),
                  cols = c("mean_dom", "dom_NAT"), linetype = 0) + theme(legend.position = "none") + 
  geom_text_repel(data = pie_ar, mapping = aes(x=lon, y=lat, label = round(mean_dom,1)), size = 6, colour = "orange2", fontface = 2) + 
  geom_text_repel(data = pie_ar, mapping = aes(x=lon, y=lat, label = round(dom_NAT,1)), size = 6, colour = "gray75", fontface = 2) +
  #theme(panel.background = element_rect(fill  = "transparent")) + 
  coord_fixed(); pie_plot

pie_map_Mar = ggdraw() + draw_plot(p, x = 0, y = 0) + draw_plot(pie_plot, x = 0, y = 0); pie_map_Mar

#ggsave(file="./Plots/Dominance_map_Mar.svg", plot=pie_map_Mar, width=5.5, height=7.5)


# Fig. 2B: Dominance map for round goby ----
test_RG = read.csv("./Data/round_goby_data_metrics.txt",header=T,dec=".",sep="\t", check.names = FALSE)
stat = unique(test_RG[,7:8]); mean(stat$lat)

dom_ar_RG = test_RG %>% mutate(area = as.factor(case_when(lat < mean(stat$lat) ~ "Southern Baltic Sea",
                                                          lat > mean(stat$lat) ~ "Central Baltic Sea"))) %>% 
  group_by(Sample,area) %>% dplyr::summarize(mean_dom=mean(NIS_relab), mean_lon = mean(lon), mean_lat = mean(lat), .groups = "drop_last") %>% 
  mutate(dom_nat = 100 - mean_dom)

pie_ar_RG = test_RG %>% mutate(area = as.factor(case_when(lat < mean(stat$lat) ~ "Southern Baltic Sea",
                                                          lat > mean(stat$lat) ~ "Central Baltic Sea"))) %>% 
  group_by(area) %>% dplyr::summarize(mean_dom=mean(NIS_relab), mean_lon = mean(lon), mean_lat = mean(lat), .groups = "drop_last") %>% 
  mutate(dom_nat = 100 - mean_dom,
         lon = case_when(area %in% "Central Baltic Sea" ~ 16.8,
                         area %in% "Southern Baltic Sea" ~ 15.4),
         lat = case_when(area %in% "Central Baltic Sea" ~ 59.2,
                         area %in% "Southern Baltic Sea" ~ 57.1))

#Put the stations in the map plus the dominance of NIS vs natives
library(scatterpie)
p <- ggplot(test_RG, aes(x = lon,y = lat)) + 
  #scale_color_viridis(option = "A", discrete = T)+
  geom_hline(mapping = aes(yintercept = mean(stat$lat)), size = 2, linetype = 1, colour = "black")+
  geom_point(dom_ar_RG, mapping = aes(x = mean_lon, y = mean_lat, color = mean_dom), size = 5) +
  scale_color_gradient(low = "slategray1", high = "steelblue4") + 
  borders(fill="grey55", colour = "grey5") +
  theme(panel.background = element_rect(fill='aliceblue', colour = 'black')) +
  coord_quickmap(xlim=c(min(test_RG$lon), max(test_RG$lon)),ylim= c(min(test_RG$lat),max(test_RG$lat)))+
  labs(x = "Lon",y="Lat", color = "Mean relative\nbiomass")+
  annotate(geom = "label", x = c(16, 17.5), y = c(61.3, 55.5), label = c("Central Baltic Sea", "Southern Baltic Sea"), color = "black", size = 5) +
  theme(legend.position = "right")+
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10))+
  guides(shape = guide_legend(override.aes = list(size = 1))); p

pie_plot <- ggplot() + theme_void() + 
  ylim(56, 60) + 
  xlim(15, 18) + 
  scale_fill_manual(values = c("steelblue4", "gray90")) + 
  geom_scatterpie(data = pie_ar_RG, mapping = aes(x=lon, y=lat, r = 100*0.003), cols = c("mean_dom", "dom_nat"), linetype = 0)+
  #geom_label_repel(data = dom_ar_RG, mapping = aes(x=lon, y=lat, label = area), color = "red4", point.padding = 0.05) +
  geom_text_repel(data = pie_ar_RG, mapping = aes(x=lon, y=lat, label = round(mean_dom,1)), size = 6, colour = "steelblue4", fontface = 2) + 
  geom_text_repel(data = pie_ar_RG, mapping = aes(x=lon, y=lat, label = round(dom_nat,1)), size = 6, colour = "gray75", fontface = 2) +
  theme(legend.position = "none") + 
  # theme(axis.ticks = element_line(),
  #       axis.text = element_text()) + 
  coord_fixed(); pie_plot

pie_map_RG = ggdraw() + draw_plot(p, x = 0, y = 0) + draw_plot(pie_plot, x = 0, y = 0); pie_map_RG

#ggsave(file="./Plots/Dominance_map_RG.svg", plot=pie_map_RG, width=5.5, height=7.5)

#Merge all plots together
dom_distr = egg::ggarrange(pie_map_Mar, pie_map_RG, ncol = 2, nrow = 1, labels = c("A", "B")); dom_distr
#ggsave(file="./Plots/Dominance_map_both.svg", plot=dom_distr, width=5.5, height=7.5)