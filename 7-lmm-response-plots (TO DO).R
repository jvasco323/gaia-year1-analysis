
library(ggplot2)
dir <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/2-yield-response/'
path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/2-yield-response/#-bisrat-clean/data/final_data/year_1/'

# ------------------------------------------------------------------------------
# start here
# ------------------------------------------------------------------------------

# colors for plotting
site <- c('Jimma', 'MisraqGojjam', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')
abbr <- c('Jimma', 'East Gojjam', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')
col <- c(1,2,3,4,5,6,7)
df_col <- data.frame('site'=site, 'abbr'=abbr, 'col'=col)

# ------------------------------------------------------------------------------
# heatmap
# ------------------------------------------------------------------------------

# rf results
rf_results <- read.csv(paste0(dir, './output-data/randomforest-results.csv'))
rf_results <- merge(rf_results, df_col, by='site')
rf_results$crop <- ifelse(rf_results$crop == 'beans', 'bean', rf_results$crop)
rf_results$crop <- paste(toupper(substr(rf_results$crop, 1, 1)), substr(rf_results$crop, 2, nchar(rf_results$crop)), sep="")
rf_results$site_crop <- paste0(rf_results$abbr, '\n', rf_results$crop)
rf_results$IncNodePurity.1 <- round(rf_results$IncNodePurity.1, 2)
rf_results$var <- ifelse(rf_results$var == 'ecec', 'ECEC', rf_results$var)
rf_results$var <- ifelse(rf_results$var == 'psi', 'PSI', rf_results$var)
rf_results$var <- ifelse(rf_results$var == 'soc', 'SOC', rf_results$var)
rf_results$var <- ifelse(rf_results$var == 'ex_ac', 'Exch.\nacidity', rf_results$var)
rf_results$var <- ifelse(rf_results$var == 'variable', 'Treatment', rf_results$var)
rf_results <- rf_results[order(rf_results$crop, rf_results$col),]

# plotting
yield <- subset(rf_results, variable=='yieldT1')
yield$site_crop <- factor(yield$site_crop, rev(unique(rf_results$site_crop)))
ggplot(yield, aes(var, site_crop, fill= rank)) + 
  geom_tile(color='black', lwd=0.5, linetype=1) +
  geom_text(aes(label=IncNodePurity.1), color="white", size=4) +
  scale_fill_gradientn(colors=c(viridis::viridis(10, direction=1)[c(4,6,8)], 'gold1')) +
  coord_fixed(ratio=0.85) +
  xlab("") +
  ylab("") +
  theme(plot.margin = margin(0, 1, 0.5, 0.5, "cm"),
        axis.text=element_text(size=13, face="bold"),
        axis.title=element_text(size=15, face="bold"),
        legend.position='none',
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0))

resp <- subset(rf_results, variable!='yieldT1')
resp$site_crop <- factor(resp$site_crop, rev(unique(rf_results$site_crop)))
ggplot(resp, aes(var, site_crop, fill= rank)) + 
  geom_tile(color='black', lwd=0.5, linetype=1) +
  geom_text(aes(label=IncNodePurity.1), color="white", size=4) +
  scale_fill_gradientn(colors=c(viridis::viridis(10, direction=1)[c(3,5,6,8)], 'gold1')) +
  coord_fixed(ratio=0.85) +
  xlab("") +
  ylab("") +
  theme(plot.margin = margin(0, 1, 0.5, 0.5, "cm"),
        axis.text=element_text(size=13, face="bold"),
        axis.title=element_text(size=15, face="bold"),
        legend.position='none',
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0))



# ------------------------------------------------------------------------------
# barplot
# ------------------------------------------------------------------------------

# lmm results
df_maize <- read.csv(paste0(dir, 'output-data/models/district-predmeans-maize-response.csv')); df_maize$crop <- 'maize'
df_wheat <- read.csv(paste0(dir, 'output-data/models/district-predmeans-wheat-response.csv')); df_wheat$crop <- 'wheat'
df_bean <- read.csv(paste0(dir, 'output-data/models/district-predmeans-beans-response.csv')); df_bean$crop <- 'bean'
df_soybean <- read.csv(paste0(dir, 'output-data/models/district-predmeans-soybean-response.csv')); df_soybean$crop <- 'soybean'
df_fababean <- read.csv(paste0(dir, 'output-data/models/district-predmeans-fababean-response.csv')); df_fababean$crop <- 'fababean'
cereals <- rbind(df_maize, df_wheat) 
legumes <- rbind(df_bean, df_soybean, df_fababean) 
all <- rbind(cereals, legumes)
write.csv(all, paste0(dir, 'output-data/predicted-means-allcrops-response.csv'), row.names=F)
df_maize <- merge(df_maize, df_col, by='site')
df_wheat <- merge(df_wheat, df_col, by='site')
df_bean <- merge(df_bean, df_col, by='site')
df_soybean <- merge(df_soybean, df_col, by='site')
df_fababean <- merge(df_fababean, df_col, by='site')

# plotting




# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------
