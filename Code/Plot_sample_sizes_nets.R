library(tidyverse)
library(data.table)
library(ggpubr)

W18 <- fread("./Data/Larvae_ID_sheets/P_vetulus larvae inventory - W18- DAY.csv", header=TRUE)

W18_MOC1 <- W18 %>%
  filter(Moc == 1)
W18_MOC4 <- W18 %>%
  filter(Moc == 4)



g1 <-ggplot(data = W18_MOC1,aes(x=Net.no, y = no.individuals.raw)) + 
  geom_bar(stat = "identity")+
  facet_grid(Transect~Station) +
  ggtitle("Winter 2018 Day, MOC1 ")+
  geom_text(aes(label = no.individuals.raw), vjust = -0.3)
g2 <-ggplot(data = W18_MOC4,aes(x=Net.no, y = no.individuals.raw)) + 
  geom_bar(stat = "identity")+
  facet_grid(Transect~Station) +
  ggtitle("Winter 2018 Day, MOC4 ")+
  geom_text(aes(label = no.individuals.raw), vjust = -0.3)

g3 <- ggarrange(g1, g2)
ggsave(g3, file = "./Figures/Winter_2018_samples.jpg", height = 8, width = 10, units = "in")

W19_day <- fread("./Data/Larvae_ID_sheets/P_vetulus larvae inventory - W19-DAY.csv", header = TRUE)
W19_day_MOC1 <- W19_day %>%
  filter(Moc == 1)
W19_day_MOC4 <- W19_day %>%
  filter(Moc == 4)
g4 <- ggplot(data = W19_day_MOC1, aes(x=Net.no, y = no.individuals.raw)) + 
  geom_bar(stat = "identity")+
  facet_grid(Transect~Station) +
  ggtitle("Winter 2019 Day, MOC1")+
  theme_bw()+
  geom_text(aes(label = no.individuals.raw), vjust = -0.3)

g5 <- ggplot(data = W19_day_MOC4, aes(x=Net.no, y = no.individuals.raw)) + 
  geom_bar(stat = "identity")+
  facet_grid(Transect~Station) +
  ggtitle("Winter 2019 Day, MOC4")+
  theme_bw()+
  geom_text(aes(label = no.individuals.raw), vjust = -0.3)

g6 <- ggarrange(g4,g5)
ggsave(g6, file = "./Figures/Winter_2019_day_samples.jpg", height = 8, width = 12, units = "in")

W19_night <- fread("./Data/Larvae_ID_sheets/P_vetulus larvae inventory - W19-NIGHT.csv", header = TRUE)


W19_night_MOC1 <- W19_night %>%
  filter(Moc == 1)
W19_night_MOC4 <- W19_night %>%
  filter(Moc == 4)
g7 <-ggplot(data = W19_night_MOC1,aes(x=Net.no, y = no.individuals.raw)) + 
  geom_bar(stat = "identity")+
  facet_grid(Transect~Station) +
  ggtitle("Winter 2019 Night MOC1")+
  geom_text(aes(label = no.individuals.raw), vjust = -0.3)+
  theme_bw()

g8 <-ggplot(data = W19_night_MOC4,aes(x=Net.no, y = no.individuals.raw)) + 
  geom_bar(stat = "identity")+
  facet_grid(Transect~Station) +
  ggtitle("Winter 2019 Night MOC4")+
  geom_text(aes(label = no.individuals.raw), vjust = -0.3)+
  theme_bw()
g9 <- ggarrange(g7, g8)

ggsave(g9, file = "./Figures/Winter_2019_night_samples.jpg", height = 8, width = 12, units = "in")

