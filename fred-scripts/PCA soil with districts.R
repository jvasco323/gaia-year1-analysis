
# LOADING REQUIRED PACKAGES ----------------------------------------------------

library(janitor)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggdist)
library(ggthemes)
library(egg)
library(cowplot)
library(lsmeans)
library(ggeffects)
library(lme4)
library(lmerTest)
library(patchwork)
library(ade4)
library(factoextra)

# SETTING THE DIRECTORY (WHERE INPUT FILES ARE FOUND AND WHERE OUTPUTS WILL BE -
# SAVED)

setwd('D:\\Mes Donnees\\2. CIMMYT\\1. GAIA\\WP3\\With soil analysis\\Final\\')

# PREPARING THE DATA------------------------------------------------------------

# PREPARING THE DATA------------------------------------------------------------

agro_data = read.csv("gaia_trials.csv")

eth_soil = read.csv('Prediction_J669_GAIA_Ethiopia_Updated_5April23.csv')
tan_soil = read.csv('Predicted_GAIA-Tanzania_Updated_05April23.csv')
rwa_soil = read.csv('Predicted_GAIA-Rwanda_Updated_05April23.csv')


eth_soil = eth_soil[, c(8, 1, 10:28)]
tan_soil = tan_soil[, c(5, 1, 14:32)]
rwa_soil = rwa_soil[, c(6, 1, 11:29)]

eth_corr = read.csv('ETH correspondance.csv')
tan_corr = read.csv('TAN correspondance.csv')
rwa_corr = read.csv('RWA correspondance.csv')

soil = rbind(eth_soil, tan_soil, rwa_soil)

corr = rbind(eth_corr, tan_corr, rwa_corr)
corr = unique(corr)

soil$Country = ifelse(soil$Country == "Tanzania, United Republic of", "Tanzania", soil$Country)
soil = merge(soil, corr, by = "SSN")
soil = soil[ which(soil$depth == "0-20"), ]
names(soil)[2] = "country"
soil$country = ifelse(soil$country == "Ethiopia ", "Ethiopia", soil$country)

soil$m3.Ca = ifelse(soil$m3.Ca == "<0.1", 0.1, soil$m3.Ca)
soil$m3.Fe = ifelse(soil$m3.Fe == "<30", 30, soil$m3.Fe)
soil$m3.K = ifelse(soil$m3.K == "<2", 2, soil$m3.K)
soil$m3.Mg = ifelse(soil$m3.Mg == "<3", 3, soil$m3.Mg)
soil$m3.Mn = ifelse(soil$m3.Mn == "<0.15", 0.15, soil$m3.Mn)
soil$CEC = ifelse(soil$CEC == "<0.6", 0.6, soil$CEC)

soil$m3.Ca = as.numeric(soil$m3.Ca)
soil$m3.Fe = as.numeric(soil$m3.Fe)
soil$m3.K = as.numeric(soil$m3.K)
soil$m3.Mg = as.numeric(soil$m3.Mg)
soil$m3.Mn = as.numeric(soil$m3.Mn)
soil$CEC = as.numeric(soil$CEC)


soil = merge(soil, agro_data, by = c("country", "FID"))

soil$count_dist = paste(soil$country, "", soil$district_gadm)

soil$count_dist = ifelse(soil$count_dist == "Ethiopia  NA", "Ethiopia  Jimma", soil$count_dist)
soil$count_dist = ifelse(soil$count_dist == "Rwanda  NA", "Rwanda  Nyaruguru", soil$count_dist)


soil$ph_class = factor(soil$ph_class, level = c("below_5.5", "above_5.5"))





names(soil)

soil_pca = scale(soil[, c(5, 7:8, 10:12, 14:15, 17:20)])

# delta.pca <- dudi.pca(soil_pca)

delta.pca <- dudi.pca(df = soil_pca, scannf = FALSE, nf = 3)

delta.pca$eig

#cumulated percentage of variability explained by the PC
cumsum(delta.pca$eig) / sum(delta.pca$eig)

summary(delta.pca)

#correlation coefficients between the PCs and the variables
delta.pca$co

fviz_pca_var(delta.pca, col.var = "contrib") + 
  scale_color_gradient2(low = "blue", mid = "purple", high = "red", midpoint = 7.5) +
  theme_minimal()



fviz_pca_ind(delta.pca, label = "none", habillage = soil$count_dist,
             addEllipses = TRUE, ellipse.level = 0.95) +
  theme_minimal()



 fviz_pca_biplot(delta.pca, label = "var", habillage = soil$count_dist,
                 addEllipses = TRUE, ellipse.level = 0.95,
                 col.var = "black", pointsize = 3, pointshape = 16) +
   theme_minimal()
 
# ggsave("PCA soil analysis - districts.jpeg", units="cm", width = 18, height = 15, dpi = 320)
 
names(soil)
 
ggplot(soil, aes(x = count_dist, y = ExAc)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black", fill = "grey") + 
  geom_point(shape = 21, size = 1.5, alpha = 0.4, position = position_jitter(seed = 1, width = .1), aes(fill = country)) + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.4, fill = "grey") +
  ylab("pH - lab prediction") + xlab("pH - SoilGrids prediction") +
  ggtitle("") +
  theme_bw() +
  guides(fill = guide_legend(nrow = 2,byrow = TRUE)) +
  theme(plot.title = element_blank(),
        legend.position = "right",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, face = "bold"))


ggplot(soil, aes(x = ExAc, y = m3.Ca, fill = count_dist)) + 
  geom_point(shape = 21, size = 4, alpha = 0.5) + 
  xlab("Exchangeable acidity") + ylab("Calcium (Mehlich)") +
  ggtitle("") +
  theme_bw() +
  theme(plot.title = element_blank(),
        legend.position = "right",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, face = "bold"))



































