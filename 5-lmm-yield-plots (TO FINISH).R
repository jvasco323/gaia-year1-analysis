
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
site <- c('Jimma', 'Misraq Gojjam', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')
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
# complete plots - cereals
# ------------------------------------------------------------------------------

png(paste0(dir, "output-data/cereals-complete-response.png"), units="in", width=8, height=7.8, res=1000)
par(mfrow=c(2,2), mar=c(4,4,1,1), mgp=c(2.5,0.75,0), cex.axis=1.3, cex.lab=1.4, xaxs='i', yaxs='i', las=1)
# plot 1
plot(0, 0, col='white', ylim=c(0,8), xlim=c(-0.5, 7.5), xlab='Lime rate (t/ha)', ylab='Cereal yield (t/ha)')
par(xpd=F)
grid(nx=16, ny=8)
# maize
i <- 1
for(s in c('Jimma', 'Mbozi', 'Geita', 'Nyaruguru', 'Ngororero', 'Burera')){
  df_sub <- subset(df_maize, site == s)
  col <- unique(df_sub$col)
  nreg_exp <- lm(emmean ~ lime_tha + I(0.99**lime_tha), data=df_sub)
  curve(coef(nreg_exp)[1] + coef(nreg_exp)[2]*x + coef(nreg_exp)[3]*I(0.99**x), lty=1, col=viridis::viridis(6)[col], from=0, to=7, add=T)
  points(df_sub$lime_tha, df_sub$emmean, pch=21, cex=2, col=viridis::viridis(6)[col], bg=viridis::viridis(6)[col])
  i <- i + 1 }
predmeans_pool <- read.csv(paste0(dir, 'output-data/models/pooled-predmeans-maize.csv'))
nreg_qad <- lm(emmean ~ lime_tha + I(lime_tha**2), data=predmeans_pool)
nreg_exp <- lm(emmean ~ lime_tha + I(0.99**lime_tha), data=predmeans_pool)
AIC(nreg_qad, nreg_exp)
curve(coef(nreg_exp)[1] + coef(nreg_exp)[2]*x + coef(nreg_exp)[3]*I(0.99**x), from=0, to=7, lwd=2, lty=2, col='black', add=T)
points(predmeans_pool$lime_tha, predmeans_pool$emmean, pch=21, cex=2, col='black', bg='black')
# wheat
nreg_exp <- lm(emmean ~ lime_tha + I(0.99**lime_tha), data=df_wheat)
curve(coef(nreg_exp)[1] + coef(nreg_exp)[2]*x + coef(nreg_exp)[3]*I(0.99**x), lty=1, col=viridis::viridis(unique(df_wheat$col)), from=0, to=7, add=T)
points(df_wheat$lime_tha, df_wheat$emmean, pch=22, cex=2, col=viridis::viridis(unique(df_wheat$col)), bg=viridis::viridis(unique(df_wheat$col)))
text(0.3, 7.25, 'A)', cex=1.4)
box()
# plot 2
df_maize$lue <- (df_maize$emmean - min(df_maize$emmean)) / df_maize$lime_tha
df_maize$lue <- ifelse(df_maize$lue == 0, Inf, df_maize$lue)
df_wheat$lue <- (df_wheat$emmean - min(df_wheat$emmean)) / df_wheat$lime_tha
df_wheat$lue <- ifelse(df_wheat$lue == 0, Inf, df_wheat$lue)
plot(df_maize$lime_tha, df_maize$lue, col='white', ylim=c(0,1.5), xlim=c(-0.5, 7.5), xlab='Lime rate (t/ha)', ylab='Lime use effiency (t/t)')
grid(nx=16, ny=15)
legend("topright", ncol=1, cex=1, legend=c('Wheat, Gojjam', 'Maize, Jimma', 'Maize, Mbozi', 'Maize, Geita', 'Maize, Nyaruguru', 'Maize, Ngororero', 'Maize, Burera', 'Maize, All sites'), pch=c(22, rep(21, 7)), lty=1, col=c(viridis::viridis(1), viridis::viridis(6), 1), pt.bg=c(viridis::viridis(1), viridis::viridis(6), 1), lwd=2)
# maize
for(s in c('Jimma', 'Mbozi', 'Geita', 'Nyaruguru', 'Ngororero', 'Burera')){
  df_sub <- subset(df_maize, site==s)
  df_sub <- df_sub[order(df_sub$lime_tha),]
  df_sub$lue <- (df_sub$emmean - min(df_sub$emmean)) / df_sub$lime_tha
  df_sub$lue <- ifelse(df_sub$lue == 0, Inf, df_sub$lue)
  col <- unique(df_sub$col)
  points(df_sub$lime_tha, df_sub$lue, pch=21, cex=2, col=viridis::viridis(6)[col], bg=viridis::viridis(6)[col])
  lines(df_sub$lime_tha, df_sub$lue, col=viridis::viridis(6)[col], lty=1)
  i <- i + 1 }
predmeans_pool <- read.csv(paste0(dir, 'output-data/models/pooled-predmeans-maize.csv'))
predmeans_pool$lue <- (predmeans_pool$emmean - min(predmeans_pool$emmean)) / predmeans_pool$lime_tha
predmeans_pool$lue <- ifelse(predmeans_pool$lue == 0, Inf, predmeans_pool$lue)
points(predmeans_pool$lime_tha, predmeans_pool$lue, pch=21, cex=2, col='black', bg='black')
lines(predmeans_pool$lime_tha, predmeans_pool$lue, lty=2, col=1)
# wheat
points(df_wheat$lime_tha, df_wheat$lue, pch=22, cex=2, col=viridis::viridis(unique(df_wheat$col)), bg=viridis::viridis(unique(df_wheat$col)))
lines(df_wheat$lime_tha, df_wheat$lue, lty=1)
text(0.3, 1.35, 'B)', cex=1.4)
box()
# plot 3
plot(df_maize$lime_tha, df_maize$emmean, col='white', ylim=c(0,150), xlim=c(-0.5, 7.5), xlab='Lime rate (t/ha)', ylab='Response over T1 (%)')
grid(nx=16, ny=15)
# maize
for(s in c('Jimma', 'Mbozi', 'Geita', 'Nyaruguru', 'Ngororero', 'Burera')){
  df_sub <- subset(df_maize, site==s)
  df_sub$respT1 <- (100*df_sub$emmean/min(df_sub$emmean))-100
  df_sub <- df_sub[order(df_sub$lime_tha),]
  col <- unique(df_sub$col)
  points(df_sub$lime_tha, df_sub$respT1, pch=21, cex=2, col=viridis::viridis(6)[col], bg=viridis::viridis(6)[col])
  lines(df_sub$lime_tha, df_sub$respT1, col=viridis::viridis(6)[col], lty=1)
  i <- i + 1 }
predmeans_pool <- read.csv(paste0(dir, 'output-data/models/pooled-predmeans-maize.csv'))
predmeans_pool$respT1 <- (100*predmeans_pool$emmean/min(predmeans_pool$emmean))-100
points(predmeans_pool$lime_tha, predmeans_pool$respT1, pch=21, cex=2, col='black', bg='black')
lines(predmeans_pool$lime_tha, predmeans_pool$respT1, lty=2, col=1)
# wheat
points(df_wheat$lime_tha, (100*df_wheat$emmean/min(df_wheat$emmean))-100, pch=22, cex=2, col=viridis::viridis(unique(df_wheat$col)), bg=viridis::viridis(unique(df_wheat$col)))
lines(df_wheat$lime_tha, (100*df_wheat$emmean/min(df_wheat$emmean))-100, lty=1)
text(0.3, 135, 'C)', cex=1.4)
box()
# plot 4
predmeans_pool$respT4 <- 100-(100*predmeans_pool$emmean/max(predmeans_pool$emmean))
plot(df_maize$lime_tha, df_maize$respT4, col='white', ylim=c(0,70), xlim=c(-0.5, 7.5), xlab='Lime rate (t/ha)', ylab='Response relative to T4 (%)')
grid(nx=16, ny=7)
# maize
for(s in c('Jimma', 'Mbozi', 'Geita', 'Nyaruguru', 'Ngororero', 'Burera')){
  df_sub <- subset(df_maize, site==s)
  df_sub$respT4 <- 100-(100*df_sub$emmean/max(df_sub$emmean))
  df_sub <- df_sub[order(df_sub$lime_tha),]
  col <- unique(df_sub$col)
  points(df_sub$lime_tha, df_sub$respT4, pch=21, cex=2, col=viridis::viridis(6)[col], bg=viridis::viridis(6)[col])
  lines(df_sub$lime_tha, df_sub$respT4, col=viridis::viridis(6)[col], lty=1)
  i <- i + 1 }
predmeans_pool <- read.csv(paste0(dir, 'output-data/models/pooled-predmeans-maize.csv'))
predmeans_pool$respT4 <- 100-(100*predmeans_pool$emmean/max(predmeans_pool$emmean))
points(predmeans_pool$lime_tha, predmeans_pool$respT4, pch=21, cex=2, col='black', bg='black')
lines(predmeans_pool$lime_tha, predmeans_pool$respT4, lty=2, col=1)
# wheat
points(df_wheat$lime_tha, 100-(100*df_wheat$emmean/max(df_wheat$emmean)), pch=22, cex=2, col=viridis::viridis(unique(df_wheat$col)), bg=viridis::viridis(unique(df_wheat$col)))
lines(df_wheat$lime_tha, 100-(100*df_wheat$emmean/max(df_wheat$emmean)), lty=1)
text(0.3, 64, 'D)', cex=1.4)
box()
# close
dev.off()

# ------------------------------------------------------------------------------
# complete plots - legumes
# ------------------------------------------------------------------------------

png(paste0(dir, "output-data/legumes-complete-response.png"), units="in", width=8, height=7.8, res=1000)
par(mfrow=c(2,2), mar=c(4,4,1,1), mgp=c(2.5,0.75,0), cex.axis=1.3, cex.lab=1.4, xaxs='i', yaxs='i', las=1)
# plot 1
plot(0, 0, col='white', ylim=c(0,3), xlim=c(-0.5, 7.5), xlab='Lime rate (t/ha)', ylab='Legume yield (t/ha)')
grid(nx=16, ny=6)
# bean
i <- 1
for(s in c('Mbozi', 'Geita', 'Nyaruguru', 'Ngororero', 'Burera')){
  df_sub <- subset(df_bean, site == s)
  col <- unique(df_sub$col)
  nreg_exp <- lm(emmean ~ lime_tha + I(0.99**lime_tha), data=df_sub)
  curve(coef(nreg_exp)[1] + coef(nreg_exp)[2]*x + coef(nreg_exp)[3]*I(0.99**x), lty=1, col=viridis::viridis(5)[col], from=0, to=7, add=T)
  points(df_sub$lime_tha, df_sub$emmean, pch=21, cex=2, col=viridis::viridis(5)[col], bg=viridis::viridis(5)[col])
  i <- i + 1 }
predmeans_pool <- read.csv(paste0(dir, 'output-data/models/pooled-predmeans-beans.csv'))
nreg_qad <- lm(emmean ~ lime_tha + I(lime_tha**2), data=predmeans_pool)
nreg_exp <- lm(emmean ~ lime_tha + I(0.99**lime_tha), data=predmeans_pool)
AIC(nreg_qad, nreg_exp)
curve(coef(nreg_exp)[1] + coef(nreg_exp)[2]*x + coef(nreg_exp)[3]*I(0.99**x), from=0, to=7, lwd=2, lty=2, col='black', add=T)
points(predmeans_pool$lime_tha, predmeans_pool$emmean, pch=21, cex=2, col='black', bg='black')
# fababean
nreg_exp <- lm(emmean ~ lime_tha + I(0.99**lime_tha), data=df_fababean)
curve(coef(nreg_exp)[1] + coef(nreg_exp)[2]*x + coef(nreg_exp)[3]*I(0.99**x), lty=1, col=viridis::viridis(unique(df_fababean$col)), from=0, to=7, add=T)
points(df_fababean$lime_tha, df_fababean$emmean, pch=22, cex=2, col=viridis::viridis(unique(df_fababean$col)), bg=viridis::viridis(unique(df_fababean$col)))
# soybean
nreg_exp <- lm(emmean ~ lime_tha + I(0.99**lime_tha), data=df_soybean)
curve(coef(nreg_exp)[1] + coef(nreg_exp)[2]*x + coef(nreg_exp)[3]*I(0.99**x), lty=1, col=viridis::viridis(unique(df_soybean$col)), from=0, to=7, add=T)
points(df_soybean$lime_tha, df_soybean$emmean, pch=23, cex=2, col=viridis::viridis(unique(df_soybean$col)), bg=viridis::viridis(unique(df_soybean$col)))
text(0.3, 2.7, 'A)', cex=1.4)
box()
# plot 2
df_bean$lue <- (df_bean$emmean - min(df_bean$emmean)) / df_bean$lime_tha
df_bean$lue <- ifelse(df_bean$lue == 0, Inf, df_bean$lue)
df_soybean$lue <- (df_soybean$emmean - min(df_soybean$emmean)) / df_soybean$lime_tha
df_soybean$lue <- ifelse(df_soybean$lue == 0, Inf, df_soybean$lue)
df_fababean$lue <- (df_fababean$emmean - min(df_fababean$emmean)) / df_fababean$lime_tha
df_fababean$lue <- ifelse(df_fababean$lue == 0, Inf, df_fababean$lue)
plot(df_bean$lime_tha, df_bean$lue, col='white', ylim=c(0,1.5), xlim=c(-0.5, 7.5), xlab='Lime rate (t/ha)', ylab='Lime use effiency (t/t)')
grid(nx=16, ny=15)
legend('topright', ncol=1, cex=1, legend=c('Fababean, Gojjam', 'Soybean, Jimma', 'Bean, Mbozi', 'Bean, Geita', 'Bean, Nyaruguru', 'Bean, Ngororero', 'Bean, Burera', 'Bean, All sites'), pch=c(22, 23, rep(21, 6)), lty=1, col=c(viridis::viridis(1), viridis::viridis(1), viridis::viridis(5), 1), pt.bg=c(viridis::viridis(1), viridis::viridis(1), viridis::viridis(5), 1), lwd=2)
# bean
for(s in c('Mbozi', 'Geita', 'Nyaruguru', 'Ngororero', 'Burera')){
  df_sub <- subset(df_bean, site==s)
  df_sub <- df_sub[order(df_sub$lime_tha),]
  df_sub$lue <- (df_sub$emmean - min(df_sub$emmean)) / df_sub$lime_tha
  df_sub$lue <- ifelse(df_sub$lue == 0, Inf, df_sub$lue)
  col <- unique(df_sub$col)
  points(df_sub$lime_tha, df_sub$lue, pch=21, cex=2, col=viridis::viridis(6)[col], bg=viridis::viridis(6)[col])
  lines(df_sub$lime_tha, df_sub$lue, col=viridis::viridis(6)[col], lty=1)
  i <- i + 1 }
predmeans_pool <- read.csv(paste0(dir, 'output-data/models/pooled-predmeans-beans.csv'))
predmeans_pool$lue <- (predmeans_pool$emmean - min(predmeans_pool$emmean)) / predmeans_pool$lime_tha
predmeans_pool$lue <- ifelse(predmeans_pool$lue == 0, Inf, predmeans_pool$lue)
points(predmeans_pool$lime_tha, predmeans_pool$lue, pch=21, cex=2, col='black', bg='black')
lines(predmeans_pool$lime_tha, predmeans_pool$lue, lty=2, col=1)
# fababean
points(df_fababean$lime_tha, df_fababean$lue, pch=22, cex=2, col=viridis::viridis(unique(df_fababean$col)), bg=viridis::viridis(unique(df_fababean$col)))
lines(df_fababean$lime_tha, df_fababean$lue, lty=1)
# soybean
points(df_soybean$lime_tha, df_soybean$lue, pch=23, cex=2, col=viridis::viridis(unique(df_soybean$col)), bg=viridis::viridis(unique(df_soybean$col)))
lines(df_soybean$lime_tha, df_soybean$lue, lty=1)
text(0.3, 1.35, 'B)', cex=1.4)
box()
# plot 3
plot(df_bean$lime_tha, df_bean$emmean, col='white', ylim=c(0,150), xlim=c(-0.5, 7.5), xlab='Lime rate (t/ha)', ylab='Response over T1 (%)')
grid(nx=16, ny=15)
# bean
for(s in c('Mbozi', 'Geita', 'Nyaruguru', 'Ngororero', 'Burera')){
  df_sub <- subset(df_bean, site==s)
  df_sub$respT1 <- (100*df_sub$emmean/min(df_sub$emmean))-100
  df_sub <- df_sub[order(df_sub$lime_tha),]
  col <- unique(df_sub$col)
  points(df_sub$lime_tha, df_sub$respT1, pch=21, cex=2, col=viridis::viridis(6)[col], bg=viridis::viridis(6)[col])
  lines(df_sub$lime_tha, df_sub$respT1, col=viridis::viridis(6)[col], lty=1)
  i <- i + 1 }
predmeans_pool <- read.csv(paste0(dir, 'output-data/models/pooled-predmeans-beans.csv'))
predmeans_pool$respT1 <- (100*predmeans_pool$emmean/min(predmeans_pool$emmean))-100
points(predmeans_pool$lime_tha, predmeans_pool$respT1, pch=21, cex=2, col='black', bg='black')
lines(predmeans_pool$lime_tha, predmeans_pool$respT1, lty=2, col=1)
# fababean
df_fababean$respT1 <- (100*df_fababean$emmean/min(df_fababean$emmean))-100
points(df_fababean$lime_tha, df_fababean$respT1, pch=22, cex=2, col=viridis::viridis(unique(df_fababean$col)), bg=viridis::viridis(unique(df_fababean$col)))
lines(df_fababean$lime_tha, df_fababean$respT1, lty=1)
# soybean
df_soybean$respT1 <- (100*df_soybean$emmean/min(df_soybean$emmean))-100
points(df_soybean$lime_tha, df_soybean$respT1, pch=23, cex=2, col=viridis::viridis(unique(df_soybean$col)), bg=viridis::viridis(unique(df_soybean$col)))
lines(df_soybean$lime_tha, df_soybean$respT1, lty=1)
text(0.3, 135, 'C)', cex=1.4)
box()
# plot 4
df_bean$respT4 <- 100-(100*df_bean$emmean/max(df_bean$emmean))
plot(df_bean$lime_tha, df_bean$respT4, col='white', ylim=c(0,70), xlim=c(-0.5, 7.5), xlab='Lime rate (t/ha)', ylab='Response relative to T4 (%)')
grid(nx=16, ny=7)
# bean
for(s in c('Mbozi', 'Geita', 'Nyaruguru', 'Ngororero', 'Burera')){
  df_sub <- subset(df_bean, site==s)
  df_sub$respT4 <- 100-(100*df_sub$emmean/max(df_sub$emmean))
  df_sub <- df_sub[order(df_sub$lime_tha),]
  col <- unique(df_sub$col)
  points(df_sub$lime_tha, df_sub$respT4, pch=21, cex=2, col=viridis::viridis(6)[col], bg=viridis::viridis(6)[col])
  lines(df_sub$lime_tha, df_sub$respT4, col=viridis::viridis(6)[col], lty=1)
  i <- i + 1 }
predmeans_pool <- read.csv(paste0(dir, 'output-data/models/pooled-predmeans-beans.csv'))
predmeans_pool$respT4 <- 100-(100*predmeans_pool$emmean/max(predmeans_pool$emmean))
points(predmeans_pool$lime_tha, predmeans_pool$respT4, pch=21, cex=2, col='black', bg='black')
lines(predmeans_pool$lime_tha, predmeans_pool$respT4, lty=2, col=1)
# fababean
df_fababean$respT4 <- 100-(100*df_fababean$emmean/max(df_fababean$emmean))
points(df_fababean$lime_tha, df_fababean$respT4, pch=22, cex=2, col=viridis::viridis(unique(df_fababean$col)), bg=viridis::viridis(unique(df_fababean$col)))
lines(df_fababean$lime_tha, df_fababean$respT4, lty=1)
# soybean
df_soybean$respT4 <- 100-(100*df_soybean$emmean/max(df_soybean$emmean))
points(df_soybean$lime_tha, df_soybean$respT4, pch=23, cex=2, col=viridis::viridis(unique(df_soybean$col)), bg=viridis::viridis(unique(df_soybean$col)))
lines(df_soybean$lime_tha, df_soybean$respT4, lty=1)
text(0.3, 64, 'D)', cex=1.4)
box()
# close
dev.off()

# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------
