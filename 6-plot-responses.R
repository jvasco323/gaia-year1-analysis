
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
