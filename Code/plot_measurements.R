#January 10, 2025
#plotting the measured fish larvae from MEZCAL cruises winter 2018 and 2019
#NOT ALL FISH ARE MEASURED- there are probably 500 left- this is a first go at it 

library(tidyverse)
library(data.table)

#load the two data tables
W18_larval_measurements <- fread("./Data/measurement_data/pvet_measurements_plotting_W18_01102025.csv")
W19_larval_measurements <- fread("./Data/measurement_data/pvet_measurements_plotting_W19_01102025.csv")

#see if the column names are the same
identical(colnames(W18_larval_measurements), colnames(W19_larval_measurements))

#rename non-identical columns
W18_larval_measurements <- W18_larval_measurements %>%
  rename(Cruise = cruise,
         Date = date)
W19_larval_measurements <- W19_larval_measurements %>%
  rename(MOC = Moc4)
#remove any NAs
W19_larval_measurements <- na.omit(W19_larval_measurements)

#make sure both datasets have the same column attributes, haul was something weird in W19
W19_larval_measurements$Haul <- as.character(W19_larval_measurements$Haul)

#combine the datasets into 1 
both_years <- rbind(W18_larval_measurements, 
                    W19_larval_measurements)

#make sure length is a numeric 
both_years$Length_mm <- as.numeric(both_years$Length_mm)

#make sure the MOC designations are correct, and replace those that say "MOC4" with "Moc4" for consistency
both_years$MOC[both_years$MOC == "MOC4"] <- "Moc4"
#check that it worked
unique(both_years$MOC)

mean_length_W18_MOC1 <- mean(W18_larval_measurements$Length_mm[W18_larval_measurements$MOC == "Moc1"])
mean_length_W18_MOC4 <- mean(W18_larval_measurements$Length_mm[W18_larval_measurements$MOC == "Moc4"])

mean_length_W19_MOC1 <- mean(W19_larval_measurements$Length_mm[W18_larval_measurements$MOC == "Moc1"])
mean_length_W19_MOC4 <- mean(W19_larval_measurements$Length_mm[W18_larval_measurements$MOC == "Moc4"])

#plot the length data by MOC1, MOC4, and year 
plot_bothyears <- ggplot(data=both_years, aes(x=Length_mm)) +
  geom_histogram(aes(color=MOC, fill = MOC), alpha= 0.5, bins = 50) +
  scale_fill_manual(values = c("darkolivegreen3", "tomato3")) +
  scale_color_manual(values = c("darkolivegreen3", "tomato3")) + 
  geom_vline(xintercept = mean_length_W18_MOC1, color = "cornflowerblue") + 
  geom_vline(xintercept = mean_length_W18_MOC4, color = "purple") + 
  geom_vline(xintercept = mean_length_W19_MOC1, color = "cornflowerblue", linetype = "dashed") + 
  geom_vline(xintercept = mean_length_W19_MOC4, color = "purple", linetype="dashed") +
  xlab("Total Length (mm)") + 
  labs(caption= "Blue = MOC1, Purple = MOC4, Solid = W18, Dashed = W19") + 
  facet_grid(.~Cruise) + theme_few() +
  theme(legend.title = element_blank())
  
ggsave(plot = plot_bothyears, "./Figures/measurements/Pvet_measured_MEZCAL.png")

#looking at per station diffs
plot_bothyears_station <- ggplot(data=both_years, aes(x=Length_mm)) +
  geom_histogram(aes(color=MOC, fill = MOC), alpha= 0.5) +
  scale_fill_manual(values = c("darkolivegreen3", "tomato3")) +
  scale_color_manual(values = c("darkolivegreen3", "tomato3")) + 
  geom_vline(xintercept = mean_length_W18_MOC1, color = "cornflowerblue") + 
  geom_vline(xintercept = mean_length_W18_MOC4, color = "purple") + 
  geom_vline(xintercept = mean_length_W19_MOC1, color = "cornflowerblue", linetype = "dashed") + 
  geom_vline(xintercept = mean_length_W19_MOC4, color = "purple", linetype="dashed") +
  xlab("Total Length (mm)") + 
  labs(caption= "Blue = MOC1, Purple = MOC4, Solid = W18, Dashed = W19") + 
  facet_grid(Station~Cruise) + theme_few() +
  theme(legend.title = element_blank())

ggsave(plot = plot_bothyears_station, "./Figures/measurements/Pvet_measured_MEZCAL_bystation.png")

#looking at by net- most fish are within the same nets, but different 
ggplot(data=both_years, aes(x=Length_mm)) +
  geom_histogram(aes(color=MOC, fill = MOC), alpha= 0.5) +
  scale_fill_manual(values = c("darkolivegreen3", "tomato3")) +
  scale_color_manual(values = c("darkolivegreen3", "tomato3")) + 
  geom_vline(xintercept = mean_length_W18_MOC1, color = "cornflowerblue") + 
  geom_vline(xintercept = mean_length_W18_MOC4, color = "purple") + 
  geom_vline(xintercept = mean_length_W19_MOC1, color = "cornflowerblue", linetype = "dashed") + 
  geom_vline(xintercept = mean_length_W19_MOC4, color = "purple", linetype="dashed") +
  xlab("Total Length (mm)") + 
  labs(caption= "Blue = MOC1, Purple = MOC4, Solid = W18, Dashed = W19") + 
  facet_grid(Net~Cruise) + theme_few() +
  theme(legend.title = element_blank())



#plotting by net 
moc1.moc4.yearcounts <- table(both_years$Cruise, both_years$MOC)


#plotting the general distribution for Marco 
hist_all <- ggplot(data = both_years, aes(x = Length_mm)) +
  geom_density(color = "darkolivegreen3", fill = "darkolivegreen3", alpha = 0.5) +
  theme_bw() +
  xlab("Total Length (mm)")+
  ggtitle("English sole measurements 2018-2019")

ggsave(plot = hist_all,
       "./Figures/measurements/pvet_measured_combined.png")

density_all <- ggplot(data=both_years, aes(x=Length_mm)) +
  geom_histogram(fill = "darkolivegreen3", binwidth = 1) +
  theme_bw()+
  xlab("Total Length (mm)")+
  ggtitle("English sole measurements 2018-2019")

ggsave(plot = density_all,
       "./Figures/measurements/pvet_measured_combined_density.png")
