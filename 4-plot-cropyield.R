
dir <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/2-yield-response/'

# ------------------------------------------------------------------------------
# start here
# ------------------------------------------------------------------------------

# load data
eth <- read.csv(paste0(path, 'gaia_trials_Ethiopia_season1_shotgun.csv'))
tza <- read.csv(paste0(path, 'gaia_trials_tanzania_season1_shotgun.csv'))
rwa <- read.csv(paste0(path, 'gaia_trials_Rwanda_season1_shotgun.csv'))
df <- rbind(eth, tza, rwa)
df$crop <- tolower(df$crop)
df <- na.omit(df) # to check how to fill some of the NA's

# not unique ID's...
df <- unique(df[c("fid", "crop", "treatment", "lime_tha", "yield_tha", "admin2_gadm")])
data_agg <- aggregate(df$yield_tha, by=list('fid'=df$fid, 'admin2_gadm'=df$admin2_gadm, 'crop'=df$crop, 'treatment'=df$treatment), FUN=mean, na.rm=T)
data_rsh <- reshape2::dcast(data_agg, fid + admin2_gadm + crop ~ treatment, value.var='x')
data_rsh <- subset(data_rsh, T1 > 0)
data_rsh <- na.omit(data_rsh)
data_rsh$respT2 <- data_rsh$T2 - data_rsh$T1
data_rsh$respT3 <- data_rsh$T3 - data_rsh$T1
data_rsh$respT4 <- data_rsh$T4 - data_rsh$T1

# ------------------------------------------------------------------------------
# response absolute: cereals
# ------------------------------------------------------------------------------

# scatter plot
maize <- subset(data_rsh, crop == 'maize')
wheat <- subset(data_rsh, crop == 'wheat')
labels <- data.frame(site=c('Nyaruguru', 'Ngororero', 'Burera', 'Jimma', 'Mbozi', 'Geita'),
                     country=c('Rwanda', 'Rwanda', 'Rwanda', 'Ethiopia', 'Tanzania', 'Tanzania'),
                     letter=c('F)', 'E)', 'D)', 'A)', 'C)', 'B)'))
png(paste0(dir, "output-data/crop-maize-yield-response-scatter.png"), units="in", width=11, height=7.7, res=1000)
par(mfrow=c(2,3), mar=c(5,4.5,2.5,1), xaxs='i', yaxs='i', las=1, cex.main=1.6, cex.lab=1.6, cex.axis=1.4, mgp=c(2.5,0.75,0))
for(s in c('Jimma', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')){
  df_sub <- subset(maize, admin2_gadm == s)
  labels_sub <- subset(labels, site == s)
  plot(df_sub$T1, df_sub$T2, 
       ylab='Treatment yield (t/ha)', xlab='Control yield (t/ha)', ylim=c(0, 10), xlim=c(0, 8))
  grid(nx=8, ny=10)
  abline(a=0, b=1, lwd=2)
  abline(a=0, b=2, lty=2, lwd=2)
    if(s == 'Jimma'){
    title(paste0(labels_sub$letter, ' ', 'Jimma & E. Gojjam, Ethiopia'))
    points(df_sub$T1, df_sub$T4, pch=21, cex=1.8, bg='royalblue')
    points(df_sub$T1, df_sub$T3, pch=21, cex=1.8, bg='orange')
    points(df_sub$T1, df_sub$T2, pch=21, cex=1.8, bg='gold') 
    points(wheat$T1, wheat$T4, pch=24, cex=1.8, bg='royalblue')
    points(wheat$T1, wheat$T3, pch=24, cex=1.8, bg='orange')
    points(wheat$T1, wheat$T2, pch=24, cex=1.8, bg='gold') 
    legend('bottomright', cex=1.2, legend=c('Maize, Jimma', 'Wheat, E. Gojjam', 'Lime rate = 1.0 t/ha', 'Lime rate = 2.5 t/ha', 'Lime rate = 7.0 t/ha'), pch=c(21,24,21,21,21), pt.bg=c(1,1, 'gold', 'orange', 'royalblue'))
  } else{
    points(df_sub$T1, df_sub$T4, pch=21, cex=1.8, bg='royalblue')
    points(df_sub$T1, df_sub$T3, pch=21, cex=1.8, bg='orange')
    points(df_sub$T1, df_sub$T2, pch=21, cex=1.8, bg='gold') 
    title(paste0(labels_sub$letter, ' ', s, ', ', labels_sub$country))
    legend('bottomright', cex=1.2, legend=c('Maize', 'Lime rate = 1.0 t/ha', 'Lime rate = 2.5 t/ha', 'Lime rate = 7.0 t/ha'), pch=21, pt.bg=c(1, 'gold', 'orange', 'royalblue'))
  }
  box() }
dev.off()

# boxplot
maize <- subset(df, crop == 'maize')
labels <- data.frame(site=c('Nyaruguru', 'Ngororero', 'Burera', 'Jimma', 'Mbozi', 'Geita'),
                     country=c('Rwanda', 'Rwanda', 'Rwanda', 'Ethiopia', 'Tanzania', 'Tanzania'),
                     letter=c('F)', 'E)', 'D)', 'A)', 'C)', 'B)'))
png(paste0(dir, "output-data/crop-maize-yield-response-boxplot.png"), units="in", width=9.5, height=7.3, res=1000)
par(mfrow=c(2,3), mar=c(5,4.5,2.5,1), xaxs='i', yaxs='i', las=1, cex.main=1.6, cex.lab=1.6, cex.axis=1.4, mgp=c(2.5,0.75,0))
for(s in c('Jimma', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')){
  df_sub <- subset(maize, admin2_gadm == s)
  labels_sub <- subset(labels, site == s)
  boxplot(df_sub$yield_tha ~ df_sub$lime_tha, 
          main=paste0(labels_sub$letter, ' ', s, ', ', labels_sub$country), 
          ylim=c(0,12), ylab='Maize yield (t/ha)', xlab='Lime rate (t/ha)')
  grid(ny=12, nx=4)
  boxplot(df_sub$yield_tha ~ df_sub$lime_tha, main=s, ylim=c(0,12), ylab='Maize yield (t/ha)', xlab='Lime rate (t/ha)', add=T) 
  box() }
dev.off()

# ------------------------------------------------------------------------------
# response absolute: legumes
# ------------------------------------------------------------------------------

# scatter plot
bean <- subset(data_rsh, crop == 'beans')
soy <- subset(data_rsh, crop == 'soybean')
faba <- subset(data_rsh, crop == 'fababean') #  data is messed up!
labels <- data.frame(site=c('Nyaruguru', 'Ngororero', 'Burera', 'Jimma', 'Mbozi', 'Geita'),
                     country=c('Rwanda', 'Rwanda', 'Rwanda', 'Ethiopia', 'Tanzania', 'Tanzania'),
                     letter=c('F)', 'E)', 'D)', 'A)', 'C)', 'B)'))
png(paste0(dir, "output-data/crop-bean-yield-response-scatter.png"), units="in", width=11, height=7.7, res=1000)
par(mfrow=c(2,3), mar=c(5,4.5,2.5,1), xaxs='i', yaxs='i', las=1, cex.main=1.6, cex.lab=1.6, cex.axis=1.4, mgp=c(2.5,0.75,0))
for(s in c('Jimma', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')){
  df_sub <- subset(bean, admin2_gadm == s)
  labels_sub <- subset(labels, site == s)
  plot(0, 0, col='white',
       ylab='Treatment yield (t/ha)', xlab='Control yield (t/ha)', ylim=c(0, 4), xlim=c(0, 3))
  grid(nx=6, ny=8)
  abline(a=0, b=1, lwd=2)
  abline(a=0, b=2, lty=2, lwd=2)
  if(s == 'Jimma'){
    points(soy$T1, soy$T4, pch=24, cex=1.8, bg='royalblue')
    points(soy$T1, soy$T3, pch=24, cex=1.8, bg='orange')
    points(soy$T1, soy$T2, pch=24, cex=1.8, bg='gold') 
    points(faba$T1, faba$T4, pch=25, cex=1.8, bg='royalblue')
    points(faba$T1, faba$T3, pch=25, cex=1.8, bg='orange')
    points(faba$T1, faba$T2, pch=25, cex=1.8, bg='gold') 
    title(paste0(labels_sub$letter, ' ', 'Jimma & E. Gojjam, Ethiopia'))
    legend('bottomright', cex=1.2, legend=c('Soybean, Jimma', 'Fababean, E. Gojjam', 'Lime rate = 1.0 t/ha', 'Lime rate = 2.5 t/ha', 'Lime rate = 7.0 t/ha'), pch=c(24,25,21,21,21), pt.bg=c(1,1, 'gold', 'orange', 'royalblue'))
  } else{
    points(df_sub$T1, df_sub$T4, pch=21, cex=1.8, bg='royalblue')
    points(df_sub$T1, df_sub$T3, pch=21, cex=1.8, bg='orange')
    points(df_sub$T1, df_sub$T2, pch=21, cex=1.8, bg='gold') 
    title(paste0(labels_sub$letter, ' ', s, ', ', labels_sub$country))
    legend('bottomright', cex=1.2, legend=c('Bean', 'Lime rate = 1.0 t/ha', 'Lime rate = 2.5 t/ha', 'Lime rate = 7.0 t/ha'), pch=21, pt.bg=c(1, 'gold', 'orange', 'royalblue'))
  }
  box() }
dev.off()

# boxplot
bean <- subset(df, crop == 'beans')
labels <- data.frame(site=c('Nyaruguru', 'Ngororero', 'Burera', 'Mbozi', 'Geita'),
                     country=c('Rwanda', 'Rwanda', 'Rwanda', 'Tanzania', 'Tanzania'),
                     letter=c('E)', 'D)', 'C)', 'B)', 'A)'))
png(paste0(dir, "output-data/crop-bean-yield-response-boxplot.png"), units="in", width=9.5, height=7.3, res=1000)
par(mfrow=c(2,3), mar=c(5,4.5,2.5,1), xaxs='i', yaxs='i', las=1, cex.main=1.6, cex.lab=1.6, cex.axis=1.4, mgp=c(2.5,0.75,0))
plot.new()
for(s in c('Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')){
  df_sub <- subset(bean, admin2_gadm == s)
  labels_sub <- subset(labels, site == s)
  boxplot(df_sub$yield_tha ~ df_sub$lime_tha, 
          main=paste0(labels_sub$letter, ' ', s, ', ', labels_sub$country), 
          ylim=c(0,4), ylab='Bean yield (t/ha)', xlab='Lime rate (t/ha)')
  grid(ny=8, nx=4)
  boxplot(df_sub$yield_tha ~ df_sub$lime_tha, main=s, ylim=c(0,4), ylab='Bean yield (t/ha)', xlab='Lime rate (t/ha)', add=T) 
  box() }
dev.off()

# ------------------------------------------------------------------------------ 
# the end
# ------------------------------------------------------------------------------ 
