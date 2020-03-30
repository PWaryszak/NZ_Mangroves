#Compute CAR (Carbon Acrretion Rate) i NZ =====
library(tidyverse)
library(gridExtra)

#Load Carbon Data:
cn_nz <- read.csv("cn_nz.csv")#LOAD CN DATA

#Create MAR data.frame off Pere's spreadsheet dataset (MAR's units = g/cm2/y)
MAR <- data.frame( site = c("Chevalier","Mangere"),
                   MAR = c(0.087, 0.46),#Mass Accretion Rate
                   SAR = c(1.61,15))   #Sediment Accretion Rate

#Compute CAR (Carbon Accretion Rate, Mg/ha/year)======
cars_nz <-  left_join(cn_nz, MAR, by = "site") %>%
  group_by(site) %>%
  mutate(C_WeigthedMean =  weighted.mean(C_percent)/100,
         N_WeigthedMean =  weighted.mean(N_percent)/100, #Nitrogen Accretion Rate
         CAR = C_WeigthedMean * MAR,
         NAR= N_WeigthedMean * MAR,
         Stock = "Belowground")

#Plot CAR vs SAR:
ggplot(cars_nz, aes(SAR, CAR))  +
  geom_point(aes(color = site), size = 7) +
  labs(x= bquote('SAR ' (mm* ~y^-1)), y = bquote('CAR  ' (Mg*~ha^-1 ~y^-1)))+
  scale_y_continuous(limits = c(0, 0.0125))+
  scale_x_continuous(limits = c(0, 16))+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 17),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))

#Save the plot in 600 DPI:
ggsave(filename = "NZ_CAR_SAR_600DPI.png", width = 8, height = 5, dpi = 600)
