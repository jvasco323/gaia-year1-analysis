
dir <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/2-yield-response/'

# ------------------------------------------------------------------------------
# start here
# ------------------------------------------------------------------------------

# load data
path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/2-yield-response/#-bisrat-clean/data/final_data/year_1/'
eth <- read.csv(paste0(path, 'gaia_trials_Ethiopia_season1_shotgun.csv'))
tza <- read.csv(paste0(path, 'gaia_trials_tanzania_season1_shotgun.csv'))
rwa <- read.csv(paste0(path, 'gaia_trials_Rwanda_season1_shotgun.csv'))
df <- rbind(eth, tza, rwa)
df$crop <- tolower(df$crop)
df <- unique(df[c("admin2_gadm", 'p_h', 'ecec', 'hp_sat', 'ca', 'mg', 'tex', 'soc', 'psi')])
df <- na.omit(df) # to check how to fill some of the NA's

# plot function
function_loop <- function(dataframe, var1, var2){
  i <- 1
  for(s in c('Jimma', 'MisraqGojjam', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')){
    df_sub <- subset(dataframe, admin2_gadm==s)
    x <- df_sub[,var1]
    y <- df_sub[,var2]
    points(x, y, pch=21, cex=1.6, col='black', bg=viridis::viridis(7)[i])
    i <- i + 1 }  
  }

# relationships
png(paste0(dir, "output-data/soil-properties-full.png"), units="in", width=6.7, height=9.5, res=1000)
par(mfrow=c(3,2), mar=c(4,4,1,1), xaxs='i', yaxs='i', cex.axis=1.5, cex.lab=1.6, mgp=c(2.5,1,0))
plot(df$p_h, df$ecec, xlab='pH in water', ylab='ECEC', xlim=c(4.5,7), ylim=c(0,25)); abline(h=mean(df$ecec), lty=2, col=2, lwd=2); abline(v=mean(df$p_h), lty=2, col=2, lwd=2)
function_loop(datafram=df, var1='p_h', var2='ecec')
plot(df$soc, df$ecec, xlab='soc', ylab='ECEC', xlim=c(0,3), ylim=c(0,25)); abline(h=mean(df$ecec), lty=2, col=2, lwd=2); abline(v=mean(df$soc, col=2, lwd=2), lty=2)
function_loop(datafram=df, var1='soc', var2='ecec')
# plot(df$p_h, df$hp_sat, xlab='p_h in water', ylab='Acidity saturation', xlim=c(4.5,7), ylim=c(0,80)); abline(h=mean(df$hp_sat), lty=2, col=2, lwd=2); abline(v=mean(df$p_h), lty=2, col=2, lwd=2)
# function_loop(datafram=df, var1='p_h', var2='hp_sat')
# plot(df$ecec, df$hp_sat, xlab='ECEC', ylab='Acidity saturation', xlim=c(0,25), ylim=c(0,80)); abline(h=mean(df$hp_sat), lty=2, col=2, lwd=2); abline(v=mean(df$ecec), lty=2, col=2, lwd=2)
# function_loop(datafram=df, var1='ecec', var2='hp_sat')
df$CaMg <- df$ca/df$mg
plot(df$p_h, df$CaMg, xlab='pH in water', ylab='Ca:Mg ratio', xlim=c(4.5,7), ylim=c(0,8)); abline(h=mean(df$CaMg), lty=2, col=2, lwd=2); abline(v=mean(df$p_h), lty=2, col=2, lwd=2)
function_loop(datafram=df, var1='p_h', var2='CaMg')
plot(df$ecec, df$CaMg, xlab='ECEC', ylab='Ca:Mg ratio', xlim=c(0,25), ylim=c(0,8)); abline(h=mean(df$CaMg), lty=2, col=2, lwd=2); abline(v=mean(df$ecec), lty=2, col=2, lwd=2)
function_loop(datafram=df, var1='ecec', var2='CaMg')
plot(df$p_h, df$psi, xlab='pH in water', ylab='PSI', xlim=c(4.5,7), ylim=c(0,350)); abline(h=mean(df$psi), lty=2, col=2, lwd=2); abline(v=mean(df$p_h), lty=2, col=2, lwd=2) # interesting one! 
function_loop(datafram=df, var1='p_h', var2='psi')
plot(df$ecec, df$psi, xlab='ECEC', ylab='PSI', xlim=c(0,25), ylim=c(0,350)); abline(h=mean(df$psi), lty=2, col=2, lwd=2); abline(v=mean(df$ecec), lty=2, col=2, lwd=2)
function_loop(datafram=df, var1='ecec', var2='psi')
legend('topright', ncol=1, cex=1.2, legend=c('Jimma', 'East Gojjam', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru'), pch=21, pt.bg=viridis::viridis(7), col=1)
dev.off()

# ------------------------------------------------------------------------------
# soils data
# ------------------------------------------------------------------------------

png(paste0(dir, "output-data/soil-properties.png"), units="in", width=9.6, height=4.8, res=1000)
par(mfrow=c(1,2), mar=c(4,4,1,1), mgp=c(2.5,0.75,0), cex.axis=1.2, cex.lab=1.4, xaxs='i', yaxs='i', las=1)
# plot 1
plot(df$p_h, df$hp_sat, ylim=c(0,90), xlim=c(4,7.5), xlab='pH in water', ylab='Acidity saturation (% ECEC)')
grid(nx=7, ny=9)
abline(h=mean(df$hp_sat), lwd=2, lty=1, col=2)
abline(v=mean(df$p_h), lwd=2, lty=1, col=2)
i <- 1
for(s in c('Jimma', 'MisraqGojjam', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')){
  df_sub <- subset(df, admin2_gadm==s)
  points(df_sub$p_h, df_sub$hp_sat, pch=21, cex=1.2, col='black', bg=viridis::viridis(7)[i])
  i <- i + 1 }
fit <- quantreg::nlrq(hp_sat ~ SSasymp(p_h, a, b, c), data = df, tau=0.8)
summary(fit)
coefs3 <- coef(fit)
xvals <- seq(4, 7.6, 0.1)
yvals <- coefs3[1] + (coefs3[2] - coefs3[1]) * exp(-exp(coefs3[3]) * xvals)
df2 <- data.frame(xvals, yvals)
# lines(df2$xvals, df2$yvals, col='black', lwd=3)
legend('topright', ncol=1, cex=1, legend=c('Jimma', 'East Gojjam', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru'), pch=21, pt.bg=viridis::viridis(7), col=1)
text(4.4, 81, 'A)', cex=1.6)
box()
# plot 2
plot(df$ecec, df$hp_sat, ylim=c(0,90), xlim=c(0,30), xlab='ECEC (cmol+/kg)', ylab='Acidity saturation (% ECEC)')
grid(nx=6, ny=9)
abline(h=mean(df$hp_sat), lwd=2, lty=1, col=2)
abline(v=mean(df$ecec), lwd=2, lty=1, col=2)
i <- 1
for(s in c('Jimma', 'MisraqGojjam', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru')){
  df_sub <- subset(df, admin2_gadm==s)
  points(df_sub$ecec, df_sub$hp_sat, pch=21, cex=1.2, col='black', bg=viridis::viridis(7)[i])
  i <- i + 1 }
fit <- quantreg::nlrq(hp_sat ~ SSasymp(ecec, a, b, c), data = df, tau=0.5)
summary(fit)
coefs3 <- coef(fit)
xvals <- seq(0, 31, 1)
yvals <- coefs3[1] + (coefs3[2] - coefs3[1]) * exp(-exp(coefs3[3]) * xvals)
df2 <- data.frame(xvals, yvals)
lines(df2$xvals, df2$yvals, col='black', lwd=3)
legend('topright', ncol=1, cex=1, legend=c('Jimma', 'East Gojjam', 'Geita', 'Mbozi', 'Burera', 'Ngororero', 'Nyaruguru'), pch=21, pt.bg=viridis::viridis(7), col=1)
text(3.7, 81, 'B)', cex=1.6)
box()
# close
dev.off()

# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------
