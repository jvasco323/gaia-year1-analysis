
path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/2-yield-response/#-bisrat-clean/data/final_data/year_1/'
dir <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/2-yield-response/'

# ------------------------------------------------------------------------------
# load data

eth <- read.csv(paste0(path, 'gaia_trials_Ethiopia_season1_shotgun.csv'))
tza <- read.csv(paste0(path, 'gaia_trials_tanzania_season1_shotgun.csv'))
rwa <- read.csv(paste0(path, 'gaia_trials_Rwanda_season1_shotgun.csv'))
data <- rbind(eth, tza, rwa)

# ------------------------------------------------------------------------------
# bisrat, why is this?

data <- subset(data, !is.na(data$treatment))
data <- unique(data[c("fid", "crop", "treatment", "yield_tha", "admin2_gadm")])
data_agg <- aggregate(data$yield_tha, by=list('fid'=data$fid, 'admin2_gadm'=data$admin2_gadm, 'crop'=data$crop, 'treatment'=data$treatment), FUN=mean, na.rm=T)
data_rsh <- reshape2::dcast(data_agg, fid + admin2_gadm + crop ~ treatment, value.var='x')
data_rsh <- subset(data_rsh, T1 > 0)
data_rsh <- na.omit(data_rsh)
data_rsh$respT2 <- data_rsh$T2 - data_rsh$T1
data_rsh$respT3 <- data_rsh$T3 - data_rsh$T1
data_rsh$respT4 <- data_rsh$T4 - data_rsh$T1

# ------------------------------------------------------------------------------
# final data

site <- c('Jimma', 'MisraqGojjam', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')
col <- c(1,2,3,4,5,6,7)
df_col <- data.frame('site'=site, 'col'=col)
maize <- merge(subset(data_rsh, crop=='Maize'), df_col, by.x='admin2_gadm', by.y='site')
wheat <- merge(subset(data_rsh, crop=='Wheat'), df_col, by.x='admin2_gadm', by.y='site')
bean <- merge(subset(data_rsh, crop=='Beans'), df_col, by.x='admin2_gadm', by.y='site')
legumes <- merge(subset(data_rsh, crop=='Soybean' | crop=='Fababean'), df_col, by.x='admin2_gadm', by.y='site')

# ------------------------------------------------------------------------------
# cumulative frequency distribution

freq_cal <- function(var){
  x <- sort(unique(round(var, 2)))
  table_x <- table(x)
  cumsum_table_x <- cumsum(table_x) 
  data_freq <- data.frame(x = x, Freq = as.numeric(table_x), Prob = as.numeric(table_x / length(x)),
                          Freq_CS = as.numeric(cumsum_table_x), Prob_CS = as.numeric(cumsum_table_x / length(x))) 
  return(data_freq) }

# ------------------------------------------------------------------------------
# plotting

site_plot <- function(df_cereal, df_legume, site){
  for(t in c('T2', 'T3', 'T4')){
    site_cereal <- subset(df_cereal, admin2_gadm == site)
    site_legume <- subset(df_legume, admin2_gadm == site)
    if(t == 'T2'){
      x <- sort(unique(round(site_cereal$respT2, 2)))
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(site_cereal$respT2)
      lines(data_freq$x, data_freq$Prob_CS, lwd=1.5, col=viridis::viridis(7)[2])
      points(data_freq$x, data_freq$Prob_CS, cex=1.5, pch=21, bg=viridis::viridis(7)[2])
      x <- sort(unique(round(site_legume$respT2, 2)))
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(site_legume$respT2)
      lines(data_freq$x, data_freq$Prob_CS, lwd=1.5, col=viridis::viridis(7)[5])
      points(data_freq$x, data_freq$Prob_CS, cex=1.5, pch=21, bg=viridis::viridis(7)[5]) }
    if(t == 'T3'){
      x <- sort(unique(round(site_cereal$respT3, 2)))
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(site_cereal$respT3)
      lines(data_freq$x, data_freq$Prob_CS, lwd=1.5, col=viridis::viridis(7)[2])
      points(data_freq$x, data_freq$Prob_CS, cex=1.5, pch=22, bg=viridis::viridis(7)[2]) 
      x <- sort(unique(round(site_legume$respT3, 2)))
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(site_legume$respT3)
      lines(data_freq$x, data_freq$Prob_CS, lwd=1.5, col=viridis::viridis(7)[5])
      points(data_freq$x, data_freq$Prob_CS, cex=1.5, pch=22, bg=viridis::viridis(7)[5]) } 
    if(t == 'T4'){
      x <- sort(unique(round(site_cereal$respT4, 2))) 
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(site_cereal$respT4)
      lines(data_freq$x, data_freq$Prob_CS, lwd=1.5, col=viridis::viridis(7)[2])
      points(data_freq$x, data_freq$Prob_CS, cex=1.5, pch=24, bg=viridis::viridis(7)[2]) 
      x <- sort(unique(round(site_legume$respT4, 2)))
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(site_legume$respT4)
      lines(data_freq$x, data_freq$Prob_CS, lwd=1.5, col=viridis::viridis(7)[5])
      points(data_freq$x, data_freq$Prob_CS, cex=1.5, pch=24, bg=viridis::viridis(7)[5]) } }  
  }

png(paste0(dir, "output-data/cumulative-probability-site.png"), units="in", width=7, height=10, res=1000)
par(mfrow=c(3,2), mar=c(4,4,1,1), mgp=c(2.75,0.75,0), cex.axis=1.5, cex.lab=1.6, xaxs='i', yaxs='i', las=1)
# plot 1
plot(0,0, col='white', ylim=c(0,1), xlim=c(-2,5), 
     xlab='Yield response to lime (t/ha)', ylab='Cumulative probability (%)')
grid(nx=NULL, ny=NULL)
abline(v=0)
abline(h=0.5, lty=2, col=2)
site_plot(df_cereal=maize, df_legume=legumes, site='Jimma')
text(-1, 0.9, 'A) JIM', cex=1.55)
legend("bottomright", bty='n', ncol=1, cex=1.3, lwd=2, legend=c('Maize T2', 'Maize T3', 'Maize T4', 'Soybean T2', 'Soybean T3', 'Soybean T4'), pch=c(21,22,24), lty=1, 
       col=c(rep(viridis::viridis(7)[2], 3), rep(viridis::viridis(7)[5], 3)), 
       pt.bg=c(rep(viridis::viridis(7)[2], 3), rep(viridis::viridis(7)[5], 3))) 
box()
# plot 2
plot(0,0, col='white', ylim=c(0,1), xlim=c(-2,5), 
     xlab='Yield response to lime (t/ha)', ylab='Cumulative probability (%)')
grid(nx=NULL, ny=NULL)
abline(v=0)
abline(h=0.5, lty=2, col=2)
site_plot(df_cereal=maize, df_legume=bean, site='Geita')
text(-1, 0.9, 'B) GEI', cex=1.55)
legend("bottomright", bty='n', ncol=1, cex=1.3, lwd=2, legend=c('Maize T2', 'Maize T3', 'Maize T4', 'Bean T2', 'Bean T3', 'Bean T4'), pch=c(21,22,24), lty=1, 
       col=c(rep(viridis::viridis(7)[2], 3), rep(viridis::viridis(7)[5], 3)), 
       pt.bg=c(rep(viridis::viridis(7)[2], 3), rep(viridis::viridis(7)[5], 3))) 
box()
# plot 3
plot(0,0, col='white', ylim=c(0,1), xlim=c(-2,5), 
     xlab='Yield response to lime (t/ha)', ylab='Cumulative probability (%)')
grid(nx=NULL, ny=NULL)
abline(v=0)
abline(h=0.5, lty=2, col=2)
site_plot(df_cereal=maize, df_legume=bean, site='Mbozi')
text(-1, 0.9, 'C) MBO', cex=1.55)
legend("bottomright", bty='n', ncol=1, cex=1.3, lwd=2, legend=c('Maize T2', 'Maize T3', 'Maize T4', 'Bean T2', 'Bean T3', 'Bean T4'), pch=c(21,22,24), lty=1, 
       col=c(rep(viridis::viridis(7)[2], 3), rep(viridis::viridis(7)[5], 3)), 
       pt.bg=c(rep(viridis::viridis(7)[2], 3), rep(viridis::viridis(7)[5], 3))) 
box()
# plot 4
plot(0,0, col='white', ylim=c(0,1), xlim=c(-2,5), 
     xlab='Yield response to lime (t/ha)', ylab='Cumulative probability (%)')
grid(nx=NULL, ny=NULL)
abline(v=0)
abline(h=0.5, lty=2, col=2)
site_plot(df_cereal=maize, df_legume=bean, site='Burera')
text(-1, 0.9, 'D) BUR', cex=1.55)
legend("bottomright", bty='n', ncol=1, cex=1.3, lwd=2, legend=c('Maize T2', 'Maize T3', 'Maize T4', 'Bean T2', 'Bean T3', 'Bean T4'), pch=c(21,22,24), lty=1, 
       col=c(rep(viridis::viridis(7)[2], 3), rep(viridis::viridis(7)[5], 3)), 
       pt.bg=c(rep(viridis::viridis(7)[2], 3), rep(viridis::viridis(7)[5], 3))) 
box()
# plot 5
plot(0,0, col='white', ylim=c(0,1), xlim=c(-2,5), 
     xlab='Yield response to lime (t/ha)', ylab='Cumulative probability (%)')
grid(nx=NULL, ny=NULL)
abline(v=0)
abline(h=0.5, lty=2, col=2)
site_plot(df_cereal=maize, df_legume=bean, site='Ngororero')
text(-1, 0.9, 'E) NGO', cex=1.55)
legend("bottomright", bty='n', ncol=1, cex=1.3, lwd=2, legend=c('Maize T2', 'Maize T3', 'Maize T4', 'Bean T2', 'Bean T3', 'Bean T4'), pch=c(21,22,24), lty=1, 
       col=c(rep(viridis::viridis(7)[2], 3), rep(viridis::viridis(7)[5], 3)), 
       pt.bg=c(rep(viridis::viridis(7)[2], 3), rep(viridis::viridis(7)[5], 3))) 
box()
# plot 6
plot(0,0, col='white', ylim=c(0,1), xlim=c(-2,5), 
     xlab='Yield response to lime (t/ha)', ylab='Cumulative probability (%)')
grid(nx=NULL, ny=NULL)
abline(v=0)
abline(h=0.5, lty=2, col=2)
site_plot(df_cereal=maize, df_legume=bean, site='Nyaruguru')
text(-1, 0.9, 'F) NYA', cex=1.55)
legend("bottomright", bty='n', ncol=1, cex=1.3, lwd=2, legend=c('Maize T2', 'Maize T3', 'Maize T4', 'Bean T2', 'Bean T3', 'Bean T4'), pch=c(21,22,24), lty=1, 
       col=c(rep(viridis::viridis(7)[2], 3), rep(viridis::viridis(7)[5], 3)), 
       pt.bg=c(rep(viridis::viridis(7)[2], 3), rep(viridis::viridis(7)[5], 3))) 
box()
dev.off()

# ------------------------------------------------------------------------------
# plotting

for(t in c('T2', 'T3', 'T4')){
  png(paste0(dir, "output-data/cumulative-probability-", t, ".png"), units="in", width=9.5, height=9.5, res=1000)
  par(mfrow=c(2,2), mar=c(4,4,1,1), mgp=c(2.75,0.75,0), cex.axis=1.4, cex.lab=1.5, xaxs='i', yaxs='i', las=1)
  # plot maize ---
  plot(0,0, col='white', ylim=c(0,1), xlim=c(-2,4), 
       xlab=paste0('Yield response to ', t, ' (t/ha)'), ylab='Cumulative probability (%)')
  grid(nx=NULL, ny=NULL)
  abline(v=0)
  abline(h=0.5, lty=2, col=2)
  i <- 1
  for(s in c('Jimma', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')){
    subst <- subset(maize, admin2_gadm == s)
    if(t == 'T2'){
      x <- sort(unique(round(subst$respT2, 2)))
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(subst$respT2) }
    if(t == 'T3'){
      x <- sort(unique(round(subst$respT3, 2)))
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(subst$respT3) } 
    if(t == 'T4'){
      x <- sort(unique(round(subst$respT4, 2))) 
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(subst$respT4) }
    lines(data_freq$x, data_freq$Prob_CS, lwd=1.5, col=viridis::viridis(7)[subst$col])
    points(data_freq$x, data_freq$Prob_CS, cex=1.5, pch=21, bg=viridis::viridis(7)[subst$col])
    i <- i + 1 }
  text(-1, 0.9, 'A) Maize', cex=1.5)
  legend("bottomright", bty='n', ncol=1, cex=1.2, legend=c('Jimma', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru'), pch=21, lty=1, col=viridis::viridis(7)[-2], pt.bg=viridis::viridis(7)[-2], lwd=2) 
  box()
  # plot wheat ---
  plot(0,0, col='white', ylim=c(0,1), xlim=c(-1,1.5), 
       xlab=paste0('Yield response to ', t, ' (t/ha)'), ylab='Cumulative probability (%)')
  grid(nx=NULL, ny=NULL)
  abline(v=0)
  abline(h=0.5, lty=2, col=2)
  i <- 1
  for(s in c('MisraqGojjam')){
    subst <- subset(wheat, admin2_gadm == s)
    if(t == 'T2'){
      x <- sort(unique(round(subst$respT2, 2)))
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(subst$respT2) }
    if(t == 'T3'){
      x <- sort(unique(round(subst$respT3, 2)))
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(subst$respT3) } 
    if(t == 'T4'){
      x <- sort(unique(round(subst$respT4, 2))) 
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(subst$respT4) }
    lines(data_freq$x, data_freq$Prob_CS, lwd=1.5, col=viridis::viridis(7)[subst$col])
    points(data_freq$x, data_freq$Prob_CS, cex=1.5, pch=21, bg=viridis::viridis(7)[subst$col])
    i <- i + 1 }
  text(-0.55, 0.9, 'B) Wheat', cex=1.5)
  legend("bottomright", bty='n', ncol=1, cex=1.2, legend=c('Misraq Gojjam'), pch=21, lty=1, col=viridis::viridis(7)[2], pt.bg=viridis::viridis(7)[2], lwd=2) 
  box()
  # plot bean ---
  plot(0,0, col='white', ylim=c(0,1), xlim=c(-1,2), 
       xlab=paste0('Yield response to ', t, ' (t/ha)'), ylab='Cumulative probability (%)')
  grid(nx=NULL, ny=NULL)
  abline(v=0)
  abline(h=0.5, lty=2, col=2)
  i <- 1
  for(s in c('Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')){
    subst <- subset(bean, admin2_gadm == s)
    if(t == 'T2'){
      x <- sort(unique(round(subst$respT2, 2)))
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(subst$respT2) }
    if(t == 'T3'){
      x <- sort(unique(round(subst$respT3, 2)))
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(subst$respT3) } 
    if(t == 'T4'){
      x <- sort(unique(round(subst$respT4, 2))) 
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(subst$respT4) }
    lines(data_freq$x, data_freq$Prob_CS, lwd=1.5, col=viridis::viridis(7)[subst$col])
    points(data_freq$x, data_freq$Prob_CS, cex=1.5, pch=21, bg=viridis::viridis(7)[subst$col])
    i <- i + 1 }
  text(-0.5, 0.9, 'C) Bean', cex=1.5)
  legend("bottomright", bty='n', ncol=1, cex=1.2, legend=c('Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru'), pch=21, lty=1, col=viridis::viridis(7)[c(-1,-2)], pt.bg=viridis::viridis(7)[c(-1,-2)], lwd=2) 
  box()
  # plot legumes ---
  plot(0,0, col='white', ylim=c(0,1), xlim=c(-1,1.5), 
       xlab=paste0('Yield response to ', t, ' (t/ha)'), ylab='Cumulative probability (%)')
  grid(nx=NULL, ny=NULL)
  abline(v=0)
  abline(h=0.5, lty=2, col=2)
  i <- 1
  for(s in c('Jimma', 'MisraqGojjam')){
    subst <- subset(legumes, admin2_gadm == s)
    if(t == 'T2'){
      x <- sort(unique(round(subst$respT2, 2)))
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(subst$respT2) }
    if(t == 'T3'){
      x <- sort(unique(round(subst$respT3, 2)))
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(subst$respT3) } 
    if(t == 'T4'){
      x <- sort(unique(round(subst$respT4, 2))) 
      table_x <- table(x)
      cumsum_table_x <- cumsum(table_x) 
      data_freq <- freq_cal(subst$respT4) }
    lines(data_freq$x, data_freq$Prob_CS, lwd=1.5, col=viridis::viridis(7)[subst$col])
    points(data_freq$x, data_freq$Prob_CS, cex=1.5, pch=21, bg=viridis::viridis(7)[subst$col])
    i <- i + 1 }
  text(-0.55, 0.9, 'D) Other', cex=1.5)
  legend("bottomright", bty='n', ncol=1, cex=1.2, legend=c('Jimma', 'Misraq Gojjam'), pch=21, lty=1, col=viridis::viridis(7)[c(1,2)], pt.bg=viridis::viridis(7)[c(1,2)], lwd=2) 
  box()
  dev.off()
  }

# ------------------------------------------------------------------------------
# the end
