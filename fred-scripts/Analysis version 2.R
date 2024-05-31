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

setwd("C:\\Users\\FBaudron\\Documents\\CIMMYT\\1. GAIA\\WP3\\Final\\")


# LOADING & CLEANING THE DATA---------------------------------------------------

data = read.csv("Rwanda_Ethiopia_Tanzania_combined.csv")

data$Crop = ifelse(data$Crop == "Maize ", "Maize", data$Crop)

data = clean_names(data)


# ASSIGNING A UNIQUE ID PER TRIAL-----------------------------------------------

data = data %>% 
  mutate(id = group_indices(., country, trial_type, district, crop))

data$id2 = rep("0", nrow(data))

data$id2 = as.numeric(data$id2)

data = data %>%
  group_by(country, trial_type, district, crop, treatment) %>%
  mutate(id2 = id2 + row_number()-1)

data$id = as.factor(data$id)
data$id2 = as.factor(data$id2)


unique_id = data[,c(1,9)] %>% 
  mutate(id = group_indices(., id, id2))

data = cbind(unique_id[,c(1)], data[,c(2:8)])

# max(data$id)

# write.csv(data, "data_with_id.csv")

# table(data$district)

data$district = ifelse(data$district == "Dedo" | data$district == "Kersa" |
                         data$district == "Nadi Gibe" | data$district == "Nadigibe" |
                         data$district == "Omonada" | data$district == "Sokoru", 
                       "Jimma", data$district)

data$count_dist = paste(data$country, "", data$district)

# table(data$count_dist)

# data$count_dist = ifelse(data$country == "Rwanda", "Rwanda", data$count_dist)

rcbd = data[ which(data$trial_type == "RCBD"), ]
sgtr = data[ which(data$trial_type == "Shotgun"), ]

sgtr_wide <- spread(sgtr[, c(1, 9, 2, 4, 5, 6, 8)], treatment, yield_t_ha)

# write.csv(sgtr_wide, "shotgun_wide_format.csv")

# table(sgtr_wide$crop)

sgtr_mz = sgtr_wide[ which(sgtr_wide$crop == "Maize"), ]
sgtr_bn = sgtr_wide[ which(sgtr_wide$crop == "Beans"), ]

sgtr_ot <- subset(sgtr_wide, subset = crop == "Fababean" | crop ==  "Soybeans" | crop ==  "Wheat")

# table(sgtr_ot$crop)



# SCATTER PLOTS-----------------------------------------------------------------

plot(sgtr_mz$TS1, sgtr_mz$TS2)

plot(sgtr_mz$TS1, sgtr_mz$TS3)

plot(sgtr_mz$TS1, sgtr_mz$TS4)


plot(sgtr_bn$TS1, sgtr_bn$TS2)

plot(sgtr_bn$TS1, sgtr_bn$TS3)

plot(sgtr_bn$TS1, sgtr_bn$TS4)


pmz1 = ggplot(sgtr_mz, aes(x = TS1, y = TS2, colour = count_dist)) + 
  theme_bw() +
  ggtitle("A - Maize - Control vs. 1 t/ha lime") +
  xlab("Grain yield control (t/ha)") + ylab("Grain yield with lime (t/ha)") +
  geom_point(size = 5, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, size = 1) +
  theme(plot.title = element_text(size = 22, face="bold", margin = margin(0, 0, 10, 0)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 12,  face = "bold"),
        legend.position = c(0.01, 0.99), legend.justification = c(0.01, 0.99),
        legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16)) +
  ylim(0, 10)


pmz2 = ggplot(sgtr_mz, aes(x = TS1, y = TS3, colour = count_dist)) + 
  theme_bw() +
  ggtitle("B - Maize - Control vs. 2.5 t/ha lime") +
  xlab("Grain yield control (t/ha)") + ylab("Grain yield with lime (t/ha)") +
  geom_point(size = 5, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, size = 1) +
  theme(plot.title = element_text(size = 22, face="bold", margin = margin(0, 0, 10, 0)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 12,  face = "bold"),
        legend.position = "none") +
  ylim(0, 10)


pmz3 = ggplot(sgtr_mz, aes(x = TS1, y = TS4, colour = count_dist)) + 
  theme_bw() +
  ggtitle("C - Maize - Control vs. 7 t/ha lime") +
  xlab("Grain yield control (t/ha)") + ylab("Grain yield with lime (t/ha)") +
  geom_point(size = 5, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, size = 1) +
  theme(plot.title = element_text(size = 22, face="bold", margin = margin(0, 0, 10, 0)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 12,  face = "bold"),
        legend.position = "none") +
  ylim(0, 10)


figmz = ggarrange(pmz1, pmz2, pmz3, 
                     ncol = 3, nrow = 1, widths=c(1, 1, 1), heights=c(1)) 

ggdraw(figmz) +  theme(plot.margin = margin(10, 10, 10, 10))

# ggsave("Maize.jpeg", units="cm", width = 48, height = 18, dpi = 320)


sgtr_bn = sgtr_bn[sgtr_bn$TS1 < 4,]
sgtr_bn  =  sgtr_bn[sgtr_bn$TS2 < 5,]

sgtr_bn = na.omit(sgtr_bn)

pbn1 = ggplot(sgtr_bn, aes(x = TS1, y = TS2, colour = count_dist)) + 
  theme_bw() +
  ggtitle("D - Bean - Control vs. 1 t/ha lime") +
  xlab("Grain yield control (t/ha)") + ylab("Grain yield with lime (t/ha)") +
  geom_point(size = 5, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, size = 1) +
  theme(plot.title = element_text(size = 22, face="bold", margin = margin(10, 0, 10, 0)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 12,  face = "bold"),
        legend.position = c(0.01, 0.99), legend.justification = c(0.01, 0.99),
        legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16)) +
  ylim(0, 4.9)


pbn2 = ggplot(sgtr_bn, aes(x = TS1, y = TS3, colour = count_dist)) + 
  theme_bw() +
  ggtitle("E - Bean - Control vs. 2.5 t/ha lime") +
  xlab("Grain yield control (t/ha)") + ylab("Grain yield with lime (t/ha)") +
  geom_point(size = 5, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, size = 1) +
  theme(plot.title = element_text(size = 22, face="bold", margin = margin(10, 0, 10, 0)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 12,  face = "bold"),
        legend.position = "none") +
  ylim(0, 4.9)


pbn3 = ggplot(sgtr_bn, aes(x = TS1, y = TS4, colour = count_dist)) + 
  theme_bw() +
  ggtitle("F - Bean - Control vs. 7 t/ha lime") +
  xlab("Grain yield control (t/ha)") + ylab("Grain yield with lime (t/ha)") +
  geom_point(size = 5, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, size = 1) +
  theme(plot.title = element_text(size = 22, face="bold", margin = margin(10, 0, 10, 0)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 12,  face = "bold"),
        legend.position = "none") +
  ylim(0, 4.9)


figbn = ggarrange(pbn1, pbn2, pbn3, 
                  ncol = 3, nrow = 1, widths=c(1, 1, 1), heights=c(1)) 

ggdraw(figbn) +  theme(plot.margin = margin(10, 10, 10, 10))

# ggsave("Bean.jpeg", units="cm", width = 48, height = 18, dpi = 320)


sgtr_ot$dist_crop = paste(sgtr_ot$district, "", sgtr_ot$crop)

pot1 = ggplot(sgtr_ot, aes(x = TS1, y = TS2, colour = dist_crop)) + 
  theme_bw() +
  ggtitle("G - Other crops - Control vs. 1 t/ha lime") +
  xlab("Grain yield control (t/ha)") + ylab("Grain yield with lime (t/ha)") +
  geom_point(size = 5, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, size = 1) +
  theme(plot.title = element_text(size = 22, face="bold", margin = margin(10, 0, 10, 0)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 12,  face = "bold"),
        legend.position = c(0.01, 0.99), legend.justification = c(0.01, 0.99),
        legend.background = element_rect(color = "black", fill = "white", linetype = "solid"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16)) +
  ylim(0, 4)


pot2 = ggplot(sgtr_ot, aes(x = TS1, y = TS3, colour = dist_crop)) + 
  theme_bw() +
  ggtitle("H - Other crops - Control vs. 2.5 t/ha lime") +
  xlab("Grain yield control (t/ha)") + ylab("Grain yield with lime (t/ha)") +
  geom_point(size = 5, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, size = 1) +
  theme(plot.title = element_text(size = 22, face="bold", margin = margin(10, 0, 10, 0)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 12,  face = "bold"),
        legend.position = "none") +
  ylim(0, 4)


pot3 = ggplot(sgtr_ot, aes(x = TS1, y = TS4, colour = dist_crop)) + 
  theme_bw() +
  ggtitle("I - Other crops - Control vs. 7 t/ha lime") +
  xlab("Grain yield control (t/ha)") + ylab("Grain yield with lime (t/ha)") +
  geom_point(size = 5, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, size = 1) +
  theme(plot.title = element_text(size = 22, face="bold", margin = margin(10, 0, 10, 0)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 12,  face = "bold"),
        legend.position = "none") +
  ylim(0, 4)


figot = ggarrange(pot1, pot2, pot3, 
                  ncol = 3, nrow = 1, widths=c(1, 1, 1), heights=c(1)) 

ggdraw(figot) +  theme(plot.margin = margin(10, 10, 10, 10))

# ggsave("Other crops.jpeg", units="cm", width = 48, height = 18, dpi = 320)


FIG = ggarrange(pmz1, pmz2, pmz3, pbn1, pbn2, pbn3, pot1, pot2, pot3, 
                  ncol = 3, nrow = 3, widths = c(1, 1, 1), heights = c(1, 1, 1)) 

ggdraw(FIG) +  theme(plot.margin = margin(10, 10, 10, 10))

# ggsave("Scatter plots.jpeg", units="cm", width = 55, height = 50, dpi = 320)



# LINEAR MODELS-----------------------------------------------------------------


sgtr_mz_long <- gather(sgtr_mz, treatment, yield, "TS1":"TS4")
sgtr_bn_long <- gather(sgtr_bn, treatment, yield, "TS1":"TS4")
sgtr_ot_long <- gather(sgtr_ot, treatment, yield, "TS1":"TS4")

sgtr_sb_long = sgtr_ot_long[ which(sgtr_ot_long$crop == "Soybeans"), ]
sgtr_fb_long = sgtr_ot_long[ which(sgtr_ot_long$crop == "Fababean"), ]
sgtr_wt_long = sgtr_ot_long[ which(sgtr_ot_long$crop == "Wheat"), ]


MMZ <- glm(yield ~ count_dist + treatment, data = sgtr_mz_long, family = gaussian())
summary(MMZ)
# plot(MMZ)

pred_treat_mz <- ggpredict(MMZ, term = "treatment")
plot(pred_treat_mz)

MMZm <- lmer(yield ~ (1|id) + count_dist + treatment, data = sgtr_mz_long)
summary(MMZm)

pred_treat_mzm <- ggpredict(MMZm, term = "treatment")
plot(pred_treat_mzm)



MBN <- glm(yield ~ count_dist + treatment, data = sgtr_bn_long, family = gaussian())
summary(MBN)
# plot(MBN)

pred_treat_bn <- ggpredict(MBN, term = "treatment")
plot(pred_treat_bn)

MBNm <- lmer(yield ~ (1|id) + count_dist + treatment, data = sgtr_bn_long)
summary(MBNm)

pred_treat_bnm <- ggpredict(MBNm, term = "treatment")
plot(pred_treat_bnm)



MSB <- glm(yield ~ treatment, data = sgtr_sb_long, family = gaussian())
summary(MSB)


pred_treat_sb <- ggpredict(MSB, term = "treatment")
plot(pred_treat_sb)


MSBm <- lmer(yield ~ (1|id) + treatment, data = sgtr_sb_long)
summary(MSBm)

pred_treat_sbm <- ggpredict(MSBm, term = "treatment")
plot(pred_treat_sbm)



MSB <- glm(yield ~ treatment, data = sgtr_sb_long, family = gaussian())
summary(MSB)


pred_treat_sb <- ggpredict(MSB, term = "treatment")
plot(pred_treat_sb)


MSBm <- lmer(yield ~ (1|id) + treatment, data = sgtr_sb_long)
summary(MSBm)

pred_treat_sbm <- ggpredict(MSBm, term = "treatment")
plot(pred_treat_sbm)



MFB <- glm(yield ~ treatment, data = sgtr_fb_long, family = gaussian())
summary(MFB)


pred_treat_fb <- ggpredict(MFB, term = "treatment")
plot(pred_treat_fb)


MFBm <- lmer(yield ~ (1|id) + treatment, data = sgtr_fb_long)
summary(MFBm)

pred_treat_fbm <- ggpredict(MFBm, term = "treatment")
plot(pred_treat_fbm)



MWT <- glm(yield ~ treatment, data = sgtr_wt_long, family = gaussian())
summary(MWT)


pred_treat_wt <- ggpredict(MWT, term = "treatment")
plot(pred_treat_wt)


MWTm <- lmer(yield ~ (1|id) + treatment, data = sgtr_wt_long)
summary(MWTm)

pred_treat_wtm <- ggpredict(MWTm, term = "treatment")
plot(pred_treat_wtm)


library(scales)
show_col(hue_pal()(5))

pred_treat_mzm$x = c(0, 1, 2.5, 7)
pred_treat_bnm$x = c(0, 1, 2.5, 7)
pred_treat_sbm$x = c(0, 1, 2.5, 7)
pred_treat_fbm$x = c(0, 1, 2.5, 7)
pred_treat_wtm$x = c(0, 1, 2.5, 7)


pm1 = ggplot(pred_treat_mzm, aes(x = x, y = predicted)) + 
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
  ylim(0, max(pred_treat_mzm$predicted) + max(pred_treat_mz$std.error)*1.5)

pm1


pm2 = ggplot(pred_treat_bnm, aes(x = x, y = predicted)) + 
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
  ylim(0, max(pred_treat_bnm$predicted) + max(pred_treat_bnm$std.error)*1.5)

pm2

pm3 = ggplot(pred_treat_sbm, aes(x = x, y = predicted)) + 
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
  ylim(0, max(pred_treat_sbm$predicted) + max(pred_treat_sbm$std.error)*1.5)

pm3


pm4 = ggplot(pred_treat_fbm, aes(x = x, y = predicted)) + 
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
  ylim(0, max(pred_treat_fbm$predicted) + max(pred_treat_fbm$std.error)*1.5)

pm4

pm5 = ggplot(pred_treat_wtm, aes(x = x, y = predicted)) + 
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
  ylim(0, max(pred_treat_wtm$predicted) + max(pred_treat_wtm$std.error)*1.5)

pm5


y.grob <- textGrob("Predicted values of grain yield (t/ha)", gp = gpar(fontface = "bold", fontsize = 16), rot = 90)
x.grob <- textGrob("Lime rate (t/ha)", gp = gpar(fontface = "bold", fontsize = 16))


ggdraw(arrangeGrob(PRED, left = y.grob, bottom = x.grob)) +  theme(plot.margin = margin(10, 10, 10, 10))

# ggsave("Predicted values 2.jpeg", units="cm", width = 40, height = 23, dpi = 320)




M <- glm(yield_t_ha ~ count_dist + crop + treatment, data = sgtr, family = gaussian())
summary(M)
# plot(MMZ)

pred_treat_m <- ggpredict(M, term = "treatment")
plot(pred_treat_m)

Mm <- lmer(yield_t_ha ~ (1|id) + count_dist + crop + treatment, data = sgtr)
summary(Mm)

pred_treat_mm <- ggpredict(Mm, term = "treatment")
plot(pred_treat_mm)



# RAINCLOUD PLOTS---------------------------------------------------------------


boxplot(sgtr_bn_long$yield)
sgtr_bn_long <- sgtr_bn_long[sgtr_bn_long$yield < 4,]
boxplot(sgtr_bn_long$yield)


prc1 =  ggplot(sgtr_mz_long, aes(x = treatment, y = yield)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black", fill = "grey") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8, fill = "grey") +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitter(seed = 1, width = .1), aes(fill = count_dist)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("A - Maize") +
  theme_bw() +
  guides(fill = guide_legend(nrow = 2,byrow = TRUE)) +
  scale_x_discrete(labels=c("no lime", "1 t/ha lime", "2.5 t/ha lime", "7 t/ha lime")) +
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


prc2 =  ggplot(sgtr_bn_long, aes(x = treatment, y = yield)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black", fill = "grey") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8, fill = "grey") +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitter(seed = 1, width = .1), aes(fill = count_dist)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("B - Bean") +
  theme_bw() +
  guides(fill = guide_legend(nrow = 2,byrow = TRUE)) +
  scale_x_discrete(labels=c("no lime", "1 t/ha lime", "2.5 t/ha lime", "7 t/ha lime")) +
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


boxplot(sgtr_sb_long$yield)
sgtr_bn_long <- sgtr_bn_long[sgtr_bn_long$yield < 4,]
boxplot(sgtr_bn_long$yield)

prc3 =  ggplot(sgtr_sb_long, aes(x = treatment, y = yield, fill = treatment)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitter(seed = 1, width = .1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("C - Soybean") +
  scale_x_discrete(labels=c("no lime", "1 t/ha lime", "2.5 t/ha lime", "7 t/ha lime")) +
  theme_bw() +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)))


boxplot(sgtr_fb_long$yield)
sgtr_bn_long <- sgtr_bn_long[sgtr_bn_long$yield < 4,]
boxplot(sgtr_bn_long$yield)


prc4 =  ggplot(sgtr_fb_long, aes(x = treatment, y = yield, fill = treatment)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitter(seed = 1, width = .1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("D - Fababean") +
  scale_x_discrete(labels=c("no lime", "1 t/ha lime", "2.5 t/ha lime", "7 t/ha lime")) +
  theme_bw() +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)))


boxplot(sgtr_wt_long$yield)
sgtr_wt_long <- sgtr_bn_long[sgtr_wt_long$yield > 1,]
boxplot(sgtr_bn_long$yield)

prc5 =  ggplot(sgtr_wt_long, aes(x = treatment, y = yield, fill = treatment)) + 
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.8, color = "black") + 
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.8) +
  geom_point(shape = 21, size = 2, alpha = 0.6, position = position_jitter(seed = 1, width = .1)) + 
  ylab("Grain yield (t/ha)") + xlab("") +
  ggtitle("E - Wheat") +
  scale_x_discrete(labels=c("no lime", "1 t/ha lime", "2.5 t/ha lime", "7 t/ha lime")) +
  theme_bw() +
  theme(plot.title = element_text(size = 18, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)))



FIGURE <- (prc1 + prc2) / (prc3 + prc4 + prc5) 

ggdraw(FIGURE) 

# ggsave("Rainclouds.jpeg", units="cm", width = 40, height = 25, dpi = 320)



