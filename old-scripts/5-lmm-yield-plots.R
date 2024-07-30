
dir <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/2-yield-response/'

# ------------------------------------------------------------------------------
# start here
# ------------------------------------------------------------------------------

df_maize <- read.csv(paste0(dir, 'output-data/models/district-predmeans-maize.csv')); df_maize$crop <- 'maize'
df_wheat <- read.csv(paste0(dir, 'output-data/models/district-predmeans-wheat.csv')); df_wheat$crop <- 'wheat'
df_bean <- read.csv(paste0(dir, 'output-data/models/district-predmeans-beans.csv')); df_bean$crop <- 'bean'
df_soybean <- read.csv(paste0(dir, 'output-data/models/district-predmeans-soybean.csv')); df_soybean$crop <- 'soybean'
df_fababean <- read.csv(paste0(dir, 'output-data/models/district-predmeans-fababean.csv')); df_fababean$crop <- 'fababean'
cereals <- rbind(df_maize, df_wheat) 
legumes <- rbind(df_bean, df_soybean, df_fababean) 
all <- rbind(cereals, legumes)
write.csv(all, paste0(dir, 'output-data/predicted-means-allcrops.csv'), row.names=F)

# colors for plotting
site <- c('Jimma', 'MisraqGojjam', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')
col <- c(1,2,3,4,5,6,7)
df_col <- data.frame('site'=site, 'col'=col)
df_maize <- merge(df_maize, df_col, by='site')
df_wheat <- merge(df_wheat, df_col, by='site')
df_bean <- merge(df_bean, df_col, by='site')
df_soybean <- merge(df_soybean, df_col, by='site')
df_fababean <- merge(df_fababean, df_col, by='site')

# ------------------------------------------------------------------------------
# summary plots - cereals
# ------------------------------------------------------------------------------

png(paste0(dir, "output-data/summary-yield-response.png"), units="in", width=11.2, height=5.5, res=1000)
par(mfrow=c(1,2), mar=c(4,4,1,1), mgp=c(2.5,0.75,0), cex.axis=1.3, cex.lab=1.5, xaxs='i', yaxs='i', las=1)
# plot 1 -----------------------------------------------------------------------
plot(0, 0, col='white', ylim=c(0,8), xlim=c(-0.5, 7.5), xlab='Lime rate (t/ha)', ylab='Cereal yield (t/ha)')
legend("bottomright", bty='n', ncol=2, cex=1, legend=c('Maize, Jimma', 'Wheat, Gojjam', 'Maize, Geita', 'Maize, Mbozi', 'Maize, Burera', 'Maize, Ngororero', 'Maize, Nyaruguru', 'Maize, All sites'), pch=c(21, 22, rep(21, 6)), lty=c(1,1,1,1,1,1,1,2), col=c(viridis::viridis(7), 1), pt.bg=c(viridis::viridis(7), 1), lwd=2) # inset=c(-0.45,0),
par(xpd=F)
grid(nx=16, ny=8)
# maize
i <- 1
for(s in c('Jimma', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')){
  df_sub <- subset(df_maize, site == s)
  col <- unique(df_sub$col)
  nreg_qad <- lm(emmean ~ lime_tha + I(lime_tha**2), data=df_sub)
  nreg_exp <- lm(emmean ~ lime_tha + I(0.99**lime_tha), data=df_sub)
  print(AIC(nreg_qad, nreg_exp))
  curve(coef(nreg_exp)[1] + coef(nreg_exp)[2]*x + coef(nreg_exp)[3]*I(0.99**x), lty=1, lwd=2, col=viridis::viridis(7)[col], from=0, to=7, add=T)
  points(df_sub$lime_tha, df_sub$emmean, pch=21, cex=2, col=viridis::viridis(7)[col], bg=viridis::viridis(7)[col])
  i <- i + 1 }
predmeans_pool <- read.csv(paste0(dir, 'output-data/models/pooled-site-random-predmeans-maize.csv'))
nreg_qad <- lm(emmean ~ lime_tha + I(lime_tha**2), data=predmeans_pool)
nreg_exp <- lm(emmean ~ lime_tha + I(0.99**lime_tha), data=predmeans_pool)
AIC(nreg_qad, nreg_exp)
curve(coef(nreg_exp)[1] + coef(nreg_exp)[2]*x + coef(nreg_exp)[3]*I(0.99**x), from=0, to=7, lwd=2, lty=2, col='black', add=T)
points(predmeans_pool$lime_tha, predmeans_pool$emmean, pch=21, cex=2, col='black', bg='black')
# wheat
nreg_exp <- lm(emmean ~ lime_tha + I(0.99**lime_tha), data=df_wheat)
curve(coef(nreg_exp)[1] + coef(nreg_exp)[2]*x + coef(nreg_exp)[3]*I(0.99**x), lty=1, lwd=2, col=viridis::viridis(7)[unique(df_wheat$col)], from=0, to=7, add=T)
points(df_wheat$lime_tha, df_wheat$emmean, pch=22, cex=2, col=viridis::viridis(7)[unique(df_wheat$col)], bg=viridis::viridis(7)[unique(df_wheat$col)])
text(0, 7.5, 'A)', cex=1.5)
box()
# plot 2 -----------------------------------------------------------------------
plot(0, 0, col='white', ylim=c(0,2.6), xlim=c(-0.5, 7.5), xlab='Lime rate (t/ha)', ylab='Legume yield (t/ha)')
legend("bottomright", bty='n', ncol=2, cex=1, legend=c('Soy, Jimma', 'Faba, Gojjam', 'Bean, Geita', 'Bean, Mbozi', 'Bean, Burera', 'Bean, Ngororero', 'Bean, Nyaruguru', 'Bean, All sites'), pch=c(24, 25, rep(21, 6)), lty=c(1,1,1,1,1,1,1,2), col=c(viridis::viridis(7), 1), pt.bg=c(viridis::viridis(7), 1), lwd=2) # inset=c(-0.45,0),
par(xpd=F)
grid(nx=16, ny=NULL)
# bean
i <- 1
for(s in c('Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')){
  df_sub <- subset(df_bean, site == s)
  col <- unique(df_sub$col)
  nreg_qad <- lm(emmean ~ lime_tha + I(lime_tha**2), data=df_sub)
  # try plateau model
  nreg_exp <- lm(emmean ~ lime_tha + I(0.99**lime_tha), data=df_sub)
  print(AIC(nreg_qad, nreg_exp))
  curve(coef(nreg_exp)[1] + coef(nreg_exp)[2]*x + coef(nreg_exp)[3]*I(0.99**x), lty=1, lwd=2, col=viridis::viridis(7)[col], from=0, to=7, add=T)
  points(df_sub$lime_tha, df_sub$emmean, pch=21, cex=2, col=viridis::viridis(7)[col], bg=viridis::viridis(7)[col])
  i <- i + 1 }
predmeans_pool <- read.csv(paste0(dir, 'output-data/models/pooled-site-random-predmeans-beans.csv'))
nreg_qad <- lm(emmean ~ lime_tha + I(lime_tha**2), data=predmeans_pool)
nreg_exp <- lm(emmean ~ lime_tha + I(0.99**lime_tha), data=predmeans_pool)
AIC(nreg_qad, nreg_exp)
curve(coef(nreg_exp)[1] + coef(nreg_exp)[2]*x + coef(nreg_exp)[3]*I(0.99**x), from=0, to=7, lwd=2, lty=2, col='black', add=T)
points(predmeans_pool$lime_tha, predmeans_pool$emmean, pch=21, cex=2, col='black', bg='black')
# soybean
nreg_exp <- lm(emmean ~ lime_tha + I(0.99**lime_tha), data=df_soybean)
curve(coef(nreg_exp)[1] + coef(nreg_exp)[2]*x + coef(nreg_exp)[3]*I(0.99**x), lty=1, lwd=2, col=viridis::viridis(7)[unique(df_soybean$col)], from=0, to=7, add=T)
points(df_soybean$lime_tha, df_soybean$emmean, pch=24, cex=2, col=viridis::viridis(7)[unique(df_soybean$col)], bg=viridis::viridis(7)[unique(df_soybean$col)])
# fababean
nreg_exp <- lm(emmean ~ lime_tha + I(0.99**lime_tha), data=df_fababean)
curve(coef(nreg_exp)[1] + coef(nreg_exp)[2]*x + coef(nreg_exp)[3]*I(0.99**x), lty=1, lwd=2, col=viridis::viridis(7)[unique(df_fababean$col)], from=0, to=7, add=T)
points(df_fababean$lime_tha, df_fababean$emmean, pch=25, cex=2, col=viridis::viridis(7)[unique(df_fababean$col)], bg=viridis::viridis(7)[unique(df_fababean$col)])
text(0, 2.42, 'B)', cex=1.5)
box()
dev.off()

# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------
