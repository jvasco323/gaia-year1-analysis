

dir <- 'C:/Users/JSILVA/OneDrive - CIMMYT/Desktop/GAIA Shotguns/'

# ------------------------------------------------------------------------------
# start here
# ------------------------------------------------------------------------------

# load data
df <- read.csv(paste0(dir, "input-data/first-year/gaia-trials-year1.csv"))
df <- subset(df, trial_type == 'Shotgun')

# other crops to do: soybean, bean, wheat
maize <- subset(df, crop == 'Maize')

# and for bean only!


# ------------------------------------------------------------------------------
# for relative yield analysis - option 1
# sub_treat <- subset(maize, treatment=='T1')
# mx <- quantile(sub_treat$yield_tha, 0.95, na.rm=T)
# sub_treat$rel_yield <- 100 * ((sub_treat$yield_tha) / mx)

# for relative yield analysis - option 2
sub_treat <- subset(maize, treatment=='T1')
sub_treat <- lapply(unique(sub_treat$district_gadm), function(s){
  ss <- subset(sub_treat, district_gadm==s)
  mx <- subset(ss, yield_tha == max(ss$yield_tha, na.rm=T))
  mx <- mean(mx$yield_tha, na.rm=T)
  ss$rel_yield <- 100 * ((ss$yield_tha) / mx)
  ss })
sub_treat <- do.call(rbind.data.frame, sub_treat)


sub_treat$ca_sat <- 100 * sub_treat$Ca/sub_treat$ecec
sub_treat$mg_sat <- 100 * sub_treat$Mg/sub_treat$ecec

rf <- randomForest::randomForest(rel_yield ~ country + tex + SOC + ecec + hp_sat + ca_sat + mg_sat + PSI, data=sub_treat)
randomForest::varImpPlot(rf)

boxplot(sub_treat$pH ~ sub_treat$district_gadm)


par(mfrow=c(1,2), mar=c(4,4,1,1))
sub_treat$district_gadm <- with(sub_treat, reorder(district_gadm, yield_tha, median, na.rm=T))
boxplot(sub_treat$yield_tha ~ sub_treat$district_gadm, ylim=c(0,8))
sub_treat$district_gadm <- with(sub_treat, reorder(district_gadm, rel_yield, median, na.rm=T))
boxplot(sub_treat$rel_yield ~ sub_treat$district_gadm, ylim=c(0,100))



# ------------------------------------------------------------------------------
# plotting
pl <- function(var, lab, xmin, xmax){
  x <- sub_treat[,c(var)]
  y <- sub_treat[,c('rel_yield')]
  ymax <- 100
  plot(x, y, ylim=c(0,ymax), xlim=c(xmin, xmax), ylab='Maize yield (t/ha)', xlab=lab)
  grid(nx=0, ny=ymax/10)
  points(x, y, pch=21, cex=1.5, col='grey', bg='grey')
  i <- 1
  for(s in c('Jimma', 'Mbozi', 'Geita', 'Nyaruguru', 'Ngororero', 'Burera')){
    df_sub <- subset(sub_treat, district_gadm==s)
    x <- df_sub[,c(var)]
    y <- df_sub[,c('rel_yield')]
    points(x, y, pch=21, cex=1.5, col='black', bg=viridis::viridis(6)[i])
    box()
    i <- i + 1 
    }
}

png("soil-vs-yield.png", units="in", width=9, height=6, res=1000)
par(mfrow=c(2,3), mar=c(4.5,4,1,1), xaxs='i', yaxs='i', cex.lab=1.4, cex.axis=1.3, mgp=c(2,0.5,0))
pl(var='pH', lab='Soil pH in water', xmin=4.5, xmax=7)
pl(var='ExAc', lab='Exchangeable acidity (cmol+/kg)', xmin=0, xmax=3)
pl(var='hp_sat', lab='Acidity saturation (% of ECEC)', xmin=0, xmax=100)
pl(var='tex', lab='Clay and silt content (%)', xmin=0, xmax=100)
pl(var='PSI', lab='Soil P fixation (PSI)', xmin=0, xmax=400)
pl(var='SOC', lab='Soil organic C (g/kg)', xmin=0, xmax=3)
dev.off()

# ------------------------------------------------------------------------------
# fit models
m <- lm(yield_tha ~ pH + hp_sat + ecec + tex + PSI + SOC, data=sub_treat)
summary(m)
m <- randomForest::randomForest(yield_tha ~ pH + hp_sat + ecec + tex + PSI + SOC, data=sub_treat)
randomForest::varImpPlot(m)
mean(m$rsq)




maize_rshp <- reshape2::dcast(maize, FID + district_gadm + hp_sat ~ treatment, value.var='yield_tha')
sites <- c('Jimma', 'Mbozi', 'Geita', 'Nyaruguru', 'Ngororero', 'Burera')
png("resp-relative.png", units="in", width=12, height=4.5, res=1000)
par(mfrow=c(1,3), mar=c(5,5,3,1), xaxs='i', yaxs='i', cex.main=1.6, cex.lab=1.6, cex.axis=1.5, las=1)
for(t in c('T2', 'T3', 'T4')){
  plot(maize_rshp$hp_sat, maize_rshp$respT4_abs, col='white', xlim=c(0,100), ylim=c(-10,200), main=paste0('Response to ', t),
       xlab='Acidity saturation (% ECEC)', ylab=paste0('Yield response to lime (% of ', t, ')'))
  grid(nx=10, ny=21)
  abline(h=0, col=2, lwd=2)
  i <- 1 
  for(s in sites){
    df_sub <- subset(maize_rshp, district_gadm==s)
    col <- viridis::viridis(6)[i]
    i <- i + 1
    df_sub['resp'] <- 100 * (df_sub[t] - df_sub['T1'])/df_sub['T1']
    points(df_sub$hp_sat, df_sub$resp, pch=21, bg=col, cex=1.8)
  }
  box()
  legend('topright', ncol=1, cex=1.5, legend=sites, pch=21, pt.bg=viridis::viridis(6))
}
dev.off()
  


maize_rshp <- reshape2::dcast(maize, FID + district_gadm + hp_sat ~ treatment, value.var='yield_tha')
sites <- c('Jimma', 'Mbozi', 'Geita', 'Nyaruguru', 'Ngororero', 'Burera')
png("resp-absolute.png", units="in", width=12, height=4.5, res=1000)
par(mfrow=c(1,3), mar=c(5,5,3,1), xaxs='i', yaxs='i', cex.main=1.6, cex.lab=1.6, cex.axis=1.5, las=1)
for(t in c('T2', 'T3', 'T4')){
  plot(maize_rshp$hp_sat, maize_rshp$respT4_abs, col='white', xlim=c(0,100), ylim=c(-1,6), main=paste0('Response to ', t),
       xlab='Acidity saturation (% ECEC)', ylab=paste0('Yield response to lime in ', t, ' (t/ha)'))
  grid(nx=10, ny=7)
  abline(h=0, col=2, lwd=2)
  i <- 1 
  for(s in sites){
    df_sub <- subset(maize_rshp, district_gadm==s)
    col <- viridis::viridis(6)[i]
    i <- i + 1
    df_sub['resp'] <- (df_sub[t] - df_sub['T1'])
    points(df_sub$hp_sat, df_sub$resp, pch=21, bg=col, cex=1.8)
  }
  box()
  legend('topright', ncol=1, cex=1.5, legend=sites, pch=21, pt.bg=viridis::viridis(6))
}
dev.off()





# ------------------------------------------------------------------------------
# plotting
# fit models

# do the above for yield response as well
# problem now with non-unique FID's
