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
library(grid)


# SETTING THE DIRECTORY (WHERE INPUT FILES ARE FOUND AND WHERE OUTPUTS WILL BE -
# SAVED)

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


data = merge(agro_data, soil, by = c("country", "FID"))
write.csv(data, 'gaia-trials-year1.csv')

data = merge(agro_data, soil, by = c("country", "FID"), all.x=T)
debug <- subset(data, is.na(data$pH))
df <- unique(debug[c(1,2)])

# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------

data$ph_class = factor(data$ph_class, level = c("below_5.5", "above_5.5"))


ggplot(data[!(is.na(data$ph_class)), ], aes(x = ph_class, y = pH)) + 
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


# ggsave("pH SoilGrids vs lab raincloud.jpeg", units="cm", width = 25, height = 15, dpi = 320)


names(data)

ggplot(data, aes(x = sgrids_ph, y = pH, fill = country)) + 
  geom_point(shape = 21, size = 4, alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1) +
  ylab("pH - lab prediction") + xlab("pH - SoilGrids prediction") +
  ggtitle("") +
  theme_bw() +
  theme(plot.title = element_blank(),
        legend.position = "right",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, face = "bold"))

# ggsave("pH SoilGrids vs lab raincloud - scatter plot.jpeg", units="cm", width = 25, height = 15, dpi = 320)


data$count_dist = paste(data$country, "", data$district_gadm)

data$count_dist = ifelse(data$count_dist == "Ethiopia  NA", "Ethiopia  Jimma", data$count_dist)
data$count_dist = ifelse(data$count_dist == "Rwanda  NA", "Rwanda  Nyaruguru", data$count_dist)


data$ph_class_lab = ifelse(data$pH < 5.5, "< 5.5", "> 5.5")


summary(data$ExAc)

data$ex_ac_class = ifelse(data$ExAc < 1, "< 1", ">= 1")


summary(data$PSI)

data$psi_class = ifelse(data$PSI < 150, "< 150", ">= 150")


summary(data$m3.Ca)

data$ca_class = ifelse(data$m3.Ca < 1000, "< 1000", ">= 1000")



rcbd = data[ which(data$trial_type == "RCBD"), ]
sgtr = data[ which(data$trial_type == "Shotgun"), ]

sgtr_mz = sgtr[ which(sgtr$crop == "Maize"), ]
sgtr_bn = sgtr[ which(sgtr$crop == "Beans"), ]

sgtr_ot <- subset(sgtr, subset = crop == "Fababean" | crop ==  "Soybean" | crop ==  "Wheat")

sgtr_sb <- subset(sgtr_ot, subset = crop ==  "Soybean")
sgtr_fb <- subset(sgtr_ot, subset = crop ==  "Fababean")
sgtr_wt <- subset(sgtr_ot, subset = crop ==  "Wheat")


boxplot(sgtr_mz$yield_tha)

prc1 =  ggplot(sgtr_mz, aes(x = treatment, y = yield_tha)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black", fill = "grey") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8, fill = "grey") +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitter(seed = 1, width = .1), aes(fill = count_dist)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("A - Maize") +
  theme_bw() +
  guides(fill = guide_legend(nrow = 2,byrow = TRUE)) +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "bottom",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        #        legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))

prc1

# ggsave("Maize raincloud.jpeg", units="cm", width = 25, height = 15, dpi = 320)



boxplot(sgtr_bn$yield_tha)

prc2 =  ggplot(sgtr_bn, aes(x = treatment, y = yield_tha)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black", fill = "grey") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8, fill = "grey") +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitter(seed = 1, width = .1), aes(fill = count_dist)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("B - Bean") +
  theme_bw() +
  guides(fill = guide_legend(nrow = 2,byrow = TRUE)) +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "bottom",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        #       legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))

prc2

# ggsave("Bean raincloud.jpeg", units="cm", width = 25, height = 15, dpi = 320)


boxplot(sgtr_sb$yield_tha)

prc3 =  ggplot(sgtr_sb, aes(x = treatment, y = yield_tha, fill = treatment)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitter(seed = 1, width = .1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("C - Soybean") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)))

prc3


boxplot(sgtr_fb$yield)

prc4 =  ggplot(sgtr_fb, aes(x = treatment, y = yield_tha, fill = treatment)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitter(seed = 1, width = .1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("D - Fababean") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)))

prc4

boxplot(sgtr_wt$yield)
sgtr_wt <- sgtr_wt[sgtr_wt$yield > 1,]
boxplot(sgtr_wt$yield)

prc5 =  ggplot(sgtr_wt, aes(x = treatment, y = yield_tha, fill = treatment)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitter(seed = 1, width = .1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("E - Wheat") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)))

prc5

FIGURE1 <- (prc1 + prc2) / (prc3 + prc4 + prc5) 

ggdraw(FIGURE1) 

# ggsave("Rainclouds.jpeg", units="cm", width = 40, height = 25, dpi = 320)



prcph1 =  ggplot(sgtr_mz, aes(x = treatment, y = yield_tha, fill = ph_class_lab)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.25, jitter.width = 0.1, jitter.height = 0.1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("A - Maize") +
  theme_bw() +
  guides(fill = guide_legend(title = "pH", nrow = 2, byrow = TRUE)) +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "right",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        # legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

prcph1

# ggsave("Maize raincloud with pH classes.jpeg", units="cm", width = 25, height = 15, dpi = 320)



boxplot(sgtr_bn$yield_tha)

prcph2 =  ggplot(sgtr_bn, aes(x = treatment, y = yield_tha, fill = ph_class_lab)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.25, jitter.width = 0.1, jitter.height = 0.1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("B - Bean") +
  theme_bw() +
  guides(fill = guide_legend(title = "pH", nrow = 2, byrow = TRUE)) +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "right",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        # legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

prcph2

# ggsave("Bean raincloud with pH classes.jpeg", units="cm", width = 25, height = 15, dpi = 320)


boxplot(sgtr_sb$yield_tha)

prcph3 =  ggplot(sgtr_sb, aes(x = treatment, y = yield_tha, fill = ph_class_lab)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.25, jitter.width = 0.1, jitter.height = 0.1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("C - Soybean") +
  theme_bw() +
  guides(fill = guide_legend(title = "pH", nrow = 2, byrow = TRUE)) +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "right",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        # legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

prcph3


boxplot(sgtr_fb$yield)

prcph4 =  ggplot(sgtr_fb, aes(x = treatment, y = yield_tha, fill = ph_class_lab)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.25, jitter.width = 0.1, jitter.height = 0.1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("D - Fababean") +
  theme_bw() +
  guides(fill = guide_legend(title = "pH", nrow = 2, byrow = TRUE)) +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "right",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        # legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

prcph4


prcph5 =  ggplot(sgtr_wt, aes(x = treatment, y = yield_tha, fill = ph_class_lab)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.25, jitter.width = 0.1, jitter.height = 0.1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("E - Wheat") +
  theme_bw() +
  guides(fill = guide_legend(title = "pH", nrow = 2, byrow = TRUE)) +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "right",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        # legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

prcph5

FIGURE2 <- (prcph1 + theme(legend.position = c(0.05, 0.95), legend.justification = c(0.1, 0.9),
                           legend.background = element_rect(color = "black", fill = "white", linetype = "solid")) + 
              prcph2 + theme(legend.position = "none")) /
  (prcph3 + theme(legend.position = "none") + prcph4 + theme(legend.position = "none") +
     prcph5 + theme(legend.position = "none")) 

ggdraw(FIGURE2) 

# ggsave("Rainclouds with pH classes.jpeg", units="cm", width = 40, height = 25, dpi = 320)


prcex1 =  ggplot(sgtr_mz, aes(x = treatment, y = yield_tha, fill = ex_ac_class)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.25, jitter.width = 0.1, jitter.height = 0.1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("A - Maize") +
  theme_bw() +
  guides(fill = guide_legend(title = "Ex. Ac.", nrow = 2, byrow = TRUE)) +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "right",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        # legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

prcex1


prcex2 =  ggplot(sgtr_bn, aes(x = treatment, y = yield_tha, fill = ex_ac_class)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.25, jitter.width = 0.1, jitter.height = 0.1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("B - Bean") +
  theme_bw() +
  guides(fill = guide_legend(title = "Ex. Ac.", nrow = 2, byrow = TRUE)) +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "right",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        # legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

prcex2


prcp1 =  ggplot(sgtr_mz, aes(x = treatment, y = yield_tha, fill = psi_class)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.25, jitter.width = 0.1, jitter.height = 0.1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("A - Maize") +
  theme_bw() +
  guides(fill = guide_legend(title = "PSI", nrow = 2, byrow = TRUE)) +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "right",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        # legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

prcp1


prcp2 =  ggplot(sgtr_bn, aes(x = treatment, y = yield_tha, fill = psi_class)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.25, jitter.width = 0.1, jitter.height = 0.1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("B - Bean") +
  theme_bw() +
  guides(fill = guide_legend(title = "PSI", nrow = 2, byrow = TRUE)) +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "right",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        # legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

prcp2


prcca1 =  ggplot(sgtr_mz, aes(x = treatment, y = yield_tha, fill = ca_class)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.25, jitter.width = 0.1, jitter.height = 0.1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("A - Maize") +
  theme_bw() +
  guides(fill = guide_legend(title = "Ca", nrow = 2, byrow = TRUE)) +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "right",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        # legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

prcca1


prcca2 =  ggplot(sgtr_bn, aes(x = treatment, y = yield_tha, fill = ca_class)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.25, jitter.width = 0.1, jitter.height = 0.1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("B - Bean") +
  theme_bw() +
  guides(fill = guide_legend(title = "Ca", nrow = 2, byrow = TRUE)) +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "right",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        # legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

prcca2




MMZ1 <- glm(yield_tha ~ count_dist + pH * treatment, data = sgtr_mz, family = gaussian())
summary(MMZ1)

pred_treat_mz1 <- ggpredict(MMZ1, term = "treatment")
plot(pred_treat_mz1)

MMZ2 <- glm(yield_tha ~ count_dist + pH + SOC + Clay + TN + m3.Al + m3.B + 
              m3.Ca + m3.Fe + m3.K + m3.S + m3.Mg + m3.Mn + m3.Na + ExAc + 
              PSI + Sand + CEC + Estimated_Silt + treatment,
            data = sgtr_mz, family = gaussian())
summary(MMZ2)

MMZ2_a <- step(MMZ2)
summary(MMZ2_a)

pred_treat_mz2 <- ggpredict(MMZ2_a, term = "treatment")
plot(pred_treat_mz2)


MBN1 <- glm(yield_tha ~ count_dist + pH * treatment, data = sgtr_bn, family = gaussian())
summary(MBN1)

pred_treat_bn1 <- ggpredict(MBN1, term = "treatment")
plot(pred_treat_bn1)

MBN2 <- glm(yield_tha ~ count_dist + pH + SOC + Clay + TN + m3.Al + m3.B +
              m3.Ca + m3.Fe + m3.K + m3.S + m3.Mg + m3.Mn + m3.Na + ExAc +
              PSI + Sand + CEC + Estimated_Silt + treatment,
            data = sgtr_bn, family = gaussian())
summary(MBN2)

MBN2_a <- step(MBN2)
summary(MBN2_a)

pred_treat_bn2 <- ggpredict(MBN2_a, term = "treatment")
plot(pred_treat_bn2)


MSB1 <- glm(yield_tha ~ pH * treatment, data = sgtr_sb, family = gaussian())
summary(MSB1)

pred_treat_sb1 <- ggpredict(MSB1, term = "treatment")
plot(pred_treat_sb1)

MSB2 <- glm(yield_tha ~ pH + SOC + Clay + TN + m3.Al + m3.B +
              m3.Ca + m3.Fe + m3.K + m3.S + m3.Mg + m3.Mn + m3.Na + ExAc +
              PSI + Sand + CEC + Estimated_Silt + treatment,
            data = sgtr_sb, family = gaussian())
summary(MSB2)

MSB2_a <- step(MSB2)
summary(MSB2_a)

pred_treat_sb2 <- ggpredict(MSB2_a, term = "treatment")
plot(pred_treat_sb2)


MFB1 <- glm(yield_tha ~ pH * treatment, data = sgtr_fb, family = gaussian())
summary(MFB1)

pred_treat_fb1 <- ggpredict(MFB1, term = "treatment")
plot(pred_treat_fb1)

MFB2 <- glm(yield_tha ~ pH + SOC + Clay + TN + m3.Al + m3.B +
              m3.Ca + m3.Fe + m3.K + m3.S + m3.Mg + m3.Mn + m3.Na + ExAc +
              PSI + Sand + CEC + Estimated_Silt + treatment,
            data = sgtr_fb, family = gaussian())
summary(MFB2)

MFB2_a <- step(MFB2)
summary(MFB2_a)

pred_treat_fb2 <- ggpredict(MFB2_a, term = "treatment")
plot(pred_treat_fb2)


MWT1 <- glm(yield_tha ~ pH * treatment, data = sgtr_wt, family = gaussian())
summary(MWT1)

pred_treat_wt1 <- ggpredict(MWT1, term = "treatment")
plot(pred_treat_wt1)

MWT2 <- glm(yield_tha ~ pH + SOC + Clay + TN + m3.Al + m3.B +
              m3.Ca + m3.Fe + m3.K + m3.S + m3.Mg + m3.Mn + m3.Na + ExAc +
              PSI + Sand + CEC + Estimated_Silt + treatment,
            data = sgtr_wt, family = gaussian())
summary(MWT2)

MWT2_a <- step(MWT2)
summary(MWT2_a)

pred_treat_wt2 <- ggpredict(MWT2_a, term = "treatment")
plot(pred_treat_wt2)



library(scales)
show_col(hue_pal()(5))

pred_treat_mz2$x = c(7, 2.5, 1, 0)
pred_treat_bn2$x = c(7, 0, 2.5, 1)
pred_treat_sb2$x = c(7, 1, 0, 2.5)
pred_treat_fb2$x = c(7, 1, 0, 2.5)
pred_treat_wt2$x = c(2.5, 7, 1, 0)


pm1 = ggplot(pred_treat_mz2, aes(x = x, y = predicted)) + 
  geom_point(size  = 5, color = "#F8766D") + ggtitle("A - Maize") +
  geom_errorbar(aes(ymin = predicted - std.error, ymax = predicted + std.error),
                width = 0.1, size = 1, color = "#F8766D") + 
  geom_line(color = "#F8766D", size = 1, linetype = 2)+
  theme_bw() + 
  ylab("") + xlab("") +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_blank()) +
  ylim(0, max(pred_treat_mz2$predicted) + max(pred_treat_mz2$std.error)*1.1)

pm1


pm2 = ggplot(pred_treat_bn2, aes(x = x, y = predicted)) + 
  geom_point(size  = 5, color = "#A3A500") + ggtitle("B - Bean") +
  geom_errorbar(aes(ymin = predicted - std.error, ymax = predicted + std.error),
                width = 0.1, size = 1, color = "#A3A500") + 
  geom_line(color = "#A3A500", size = 1, linetype = 2)+
  theme_bw() + 
  ylab("") + xlab("") +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_blank()) +
  ylim(0, max(pred_treat_bn2$predicted) + max(pred_treat_bn2$std.error)*1.1)

pm2


pm3 = ggplot(pred_treat_sb2, aes(x = x, y = predicted)) + 
  geom_point(size  = 5, color = "#00BF7D") + ggtitle("C - Soybean") +
  geom_errorbar(aes(ymin = predicted - std.error, ymax = predicted + std.error),
                width = 0.1, size  = 1, color = "#00BF7D") + 
  geom_line(color = "#00BF7D", size = 1, linetype = 2)+
  theme_bw() + 
  ylab("") + xlab("") +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_blank()) +
  ylim(0, max(pred_treat_sb2$predicted) + max(pred_treat_sb2$std.error)*1.1)

pm3


pm4 = ggplot(pred_treat_fb2, aes(x = x, y = predicted)) + 
  geom_point(size  = 5, color = "#00B0F6") + ggtitle("D - Fababean") +
  geom_errorbar(aes(ymin = predicted - std.error, ymax = predicted + std.error),
                width = 0.1, size  = 1, color = "#00B0F6") + 
  geom_line(color = "#00B0F6", size = 1, linetype = 2)+
  theme_bw() + 
  ylab("") + xlab("") +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_blank()) +
  ylim(0, max(pred_treat_fb2$predicted) + max(pred_treat_fb2$std.error)*1.1)

pm4

pm5 = ggplot(pred_treat_wt2, aes(x = x, y = predicted)) + 
  geom_point(size  = 5, color = "#E76BF3") + ggtitle("E - Wheat") +
  geom_errorbar(aes(ymin = predicted - std.error, ymax = predicted + std.error),
                width = 0.1, size  = 1, color = "#E76BF3") + 
  geom_line(color = "#E76BF3", size = 1, linetype = 2)+
  theme_bw() + 
  ylab("") + xlab("") +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_blank()) +
  ylim(0, max(pred_treat_wt2$predicted) + max(pred_treat_wt2$std.error)*1.1)

pm5


PRED = ggarrange(pm1, pm2, pm3, pm4, pm5,  
                 ncol = 3, nrow = 2, widths = c(1, 1, 1), heights = c(1,1))

y.grob <- textGrob("Predicted values of grain yield (t/ha)", gp = gpar(fontface = "bold", fontsize = 16), rot = 90)
x.grob <- textGrob("Lime rate (t/ha)", gp = gpar(fontface = "bold", fontsize = 16))


ggdraw(arrangeGrob(PRED, left = y.grob, bottom = x.grob)) +  theme(plot.margin = margin(10, 10, 10, 10))

# ggsave("Predicted values.jpeg", units="cm", width = 40, height = 23, dpi = 320)


table(sgtr_mz$count_dist)

sgtr_mz_jimma = sgtr[ which(sgtr_mz$count_dist == "Ethiopia  Jimma"), ]
sgtr_mz_nyaruguru = sgtr[ which(sgtr_mz$count_dist == "Rwanda  Nyaruguru"), ]
sgtr_mz_geita = sgtr[ which(sgtr_mz$count_dist == "Tanzania  Geita"), ]


sgtr_bn_jimma = sgtr[ which(sgtr_bn$count_dist == "Ethiopia  Jimma"), ]
sgtr_bn_geita = sgtr[ which(sgtr_bn$count_dist == "Tanzania  Geita"), ]


sgtr_mz_rwanda = sgtr[ which(sgtr_mz$country == "Rwanda"), ]
sgtr_bn_rwanda = sgtr[ which(sgtr_bn$country == "Rwanda"), ]



ggplot(sgtr_bn_rwanda, aes(x = treatment, y = yield_tha, fill = treatment)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.25, jitter.width = 0.1, jitter.height = 0.1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("Bean yield as a function of lime rates in Rwanda") +
  theme_bw() +
  guides(fill = guide_legend(title = "Ca", nrow = 2, byrow = TRUE)) +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        # legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# ggsave("Bean yield Rwanda.jpeg", units="cm", width = 25, height = 15, dpi = 320)
