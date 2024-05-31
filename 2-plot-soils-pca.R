
library(factoextra)
library(ggplot2)
library(ggdist)
library(ggthemes)
library(egg)
library(cowplot)
library(grid)

# load data
dir <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/2-yield-response/'
path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/2-yield-response/#-bisrat-clean/data/final_data/year_1/'
eth <- read.csv(paste0(path, 'gaia_trials_Ethiopia_season1_shotgun.csv'))
tza <- read.csv(paste0(path, 'gaia_trials_tanzania_season1_shotgun.csv'))
rwa <- read.csv(paste0(path, 'gaia_trials_Rwanda_season1_shotgun.csv'))
df <- rbind(eth, tza, rwa)
df$crop <- tolower(df$crop)
df <- na.omit(df) # to check how to fill some of the NA's
df$admin2_gadm <- ifelse(df$admin2_gadm == 'MisraqGojjam', 'East Gojjam', df$admin2_gadm)

# ------------------------------------------------------------------------------
# soils data
# ------------------------------------------------------------------------------

# add column
df$count_dist = paste(df$country, "", df$admin2_gadm)

# run pca ----------------------------------------------------------------------
soil_pca = scale(df[,c('ex_ac', 'psi', 'soc', 'tex', 'ecec', 'ca', 'mg', 'k')])
delta.pca = ade4::dudi.pca(df=soil_pca, scannf=F, nf=3)
delta.pca$eig
cumsum(delta.pca$eig) / sum(delta.pca$eig)
summary(delta.pca)
delta.pca$co

# run hc -----------------------------------------------------------------------
# get soil types in original dataframe

# plotting ---------------------------------------------------------------------

# prepare data
df$admin2_gadm <- with(df, reorder(admin2_gadm, ecec, median, na.rm=T))
df$admin2_gadm <- factor(df$admin2_gadm, levels=c('Jimma', 'East Gojjam', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru'))
my.col.var <- viridis::viridis(7)
sites <- c("JIM", "EGOJ", "GEI", "MBO", 'BUR', 'NGO', 'NYA')

# plot 1
p1 = fviz_pca_biplot(delta.pca, label="var", palette = my.col.var,
                     labelsize = 5,
                     habillage=df$admin2_gadm, addEllipses=T, 
                     ellipse.level = 0.75, col.var = "black", 
                     pointsize = 3, pointshape = 16) + 
  labs(title ="", x = "PC1 (49.0%)", y = "PC2 (28.4%)") + 
  xlim(-6, 7) + ylim (-6, 6) +
  annotate("text", x=-5.5, y=5.7, label= "A)", size=7) +
  theme(plot.margin = margin(0, 1, 0.5, 0.5, "cm"),
        axis.text=element_text(size=13),
        axis.title=element_text(size=15, face="bold"),
        legend.text = element_text(size=12),
        legend.position = c(0.65, 0.8), 
        legend.justification = "left",
        legend.direction = "vertical",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0))

# plot 2
p2 = ggplot(df, aes(x = admin2_gadm, y = ecec, fill = admin2_gadm)) + 
  geom_boxplot(width = .8, outlier.shape = 21, alpha = 0.8) +
  scale_fill_manual(values=my.col.var) +
  ylab("") + xlab("") +
  ggtitle("ECEC (cmol+/kg)") +
  ylim (0, 20) +
  scale_x_discrete(labels= sites) +
  annotate("text", x=1, y=19.5, label= "B)", size=5) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.text=element_text(size=12),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))

# plot 3
p3 = ggplot(df, aes(x = admin2_gadm, y = hp_sat, fill = admin2_gadm)) + 
  geom_boxplot(width = .8, outlier.shape = 21, alpha = 0.8) +
  scale_fill_manual(values=my.col.var) +
  ylab("") + xlab("") +
  ggtitle("Acidity saturation (% ECEC)") +
  ylim (0, 80) +
  scale_x_discrete(labels= sites) +
  annotate("text", x=1, y=78, label= "C)", size=5) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.text=element_text(size=12),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))

# plot 4
df$ca_sat <- 100 * df$ca / df$ecec
p4 = ggplot(df, aes(x = admin2_gadm, y = ca_sat, fill = admin2_gadm)) + 
  geom_boxplot(width = .8, outlier.shape = 21, alpha = 0.8) +
  scale_fill_manual(values=my.col.var) +
  ylab("") + xlab("") +
  ggtitle("Ca saturation (% ECEC)") +
  ylim (0, 80) +
  scale_x_discrete(labels= sites) +
  annotate("text", x=1, y=78, label= "D)", size=5) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.text=element_text(size=12),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))

# plot 5
p5 = ggplot(df, aes(x = admin2_gadm, y = psi, fill = admin2_gadm)) + 
  geom_boxplot(width = .8, outlier.shape = 21, alpha = 0.8) +
  scale_fill_manual(values=my.col.var) +
  ylab("") + xlab("") +
  ggtitle("P sorption index") +
  ylim (0, 300) +
  scale_x_discrete(labels= sites) +
  annotate("text", x=1, y=295, label= "E)", size=5) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face="bold", margin = margin(0, 0, 10, 0)),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.text=element_text(size=12),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))

# final figure
library(patchwork)
png(paste0(dir, "output-data/soil-acid-complex.png"), units="cm", width = 30, height = 19, res=100)
p1 + (p2 + p3 + p4 + p5) + plot_layout(ncol = 2, widths = c(2, 3))
dev.off()
