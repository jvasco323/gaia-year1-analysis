
library(akima)
library(sjPlot)
library(ggplot2)
theme_set(theme_sjplot())

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
df <- unique(df[c("fid", "crop", "treatment", "lime_tha", "yield_tha", "country", "admin2_gadm", "tex", "soc", "ecec", "ex_ac", "ca", "mg", "k", "psi", "hp_sat")])
df <- na.omit(df) # to check how to fill some of the NA's

# maize dataset
maize <- subset(df, crop == 'maize')
str(maize)
maize$country <- as.factor(maize$country)
maize$admin2_gadm <- as.factor(maize$admin2_gadm)
maize$fid <- as.factor(maize$fid)
maize$treatment <- as.factor(maize$treatment)

# ------------------------------------------------------------------------------
# linear model (ols) --- lime * ex_ac only
# ------------------------------------------------------------------------------

resid.inspect <- function(mod, col="black"){
  resid <- resid(mod)
  fitd <- fitted(mod)
  par(mfrow=c(1, 3))
  hist(resid / sd(resid, na.rm=T), 30, main="")
  plot(fitd, resid / sd(resid, na.rm=T), col=col)
  qqnorm(resid / sd(resid, na.rm=T))
  abline(coef=c(0, 1)) }

plotting <- function(lowquant, highquant, lime){
  if(lime=='continuous'){
    par(mfrow=c(1,2))
    # plot 1
    plot(newdata$lime_tha, newdata$response)
    grid(nx=NULL, ny=NULL)
    lines(newdata$lime_tha[newdata$ex_ac==lowquant], newdata$response[newdata$ex_ac==lowquant], col=2)
    points(newdata$lime_tha[newdata$ex_ac==lowquant], newdata$response[newdata$ex_ac==lowquant], pch=21, bg=2)
    lines(newdata$lime_tha[newdata$ex_ac==highquant], newdata$response[newdata$ex_ac==highquant], col=3)
    points(newdata$lime_tha[newdata$ex_ac==highquant], newdata$response[newdata$ex_ac==highquant], pch=21, bg=3)
    # plot 2
    plot(aa$lime_tha, aa$lime_tha.trend)
    grid(nx=NULL, ny=NULL)
    lines(aa$lime_tha[aa$ex_ac==lowquant], aa$lime_tha.trend[aa$ex_ac==lowquant], col=2)
    points(aa$lime_tha[aa$ex_ac==lowquant], aa$lime_tha.trend[aa$ex_ac==lowquant], pch=21, bg=2)
    lines(aa$lime_tha[aa$ex_ac==highquant], aa$lime_tha.trend[aa$ex_ac==highquant], col=3)
    points(aa$lime_tha[aa$ex_ac==highquant], aa$lime_tha.trend[aa$ex_ac==highquant], pch=21, bg=3)
    abline(h=0)  
  } else{
    par(mfrow=c(1,1))
    plot(newdata$lime_tha, newdata$response)
    grid(nx=NULL, ny=NULL)
    lines(newdata$lime_tha[newdata$ex_ac==lowquant], newdata$response[newdata$ex_ac==lowquant], col=2)
    points(newdata$lime_tha[newdata$ex_ac==lowquant], newdata$response[newdata$ex_ac==lowquant], pch=21, bg=2)
    lines(newdata$lime_tha[newdata$ex_ac==highquant], newdata$response[newdata$ex_ac==highquant], col=3)
    points(newdata$lime_tha[newdata$ex_ac==highquant], newdata$response[newdata$ex_ac==highquant], pch=21, bg=3)
  } }

# ---------------------------
# lime as continuous variable
mod0 <- lm(yield_tha ~ lime_tha * ex_ac, data=maize)
mod0 <- lm(yield_tha ~ poly(lime_tha,2) * ex_ac, data=maize)
resid.inspect(mod0)
anova(mod0)
plot_model(mod0, type="pred", terms=c("lime_tha [all]", "ex_ac"))
# predict response (same as emmeans)
ex_ac <- c(0.2, 2.5)
lime_tha <- c(0,1,2,3,4,5,6,7)
newdata <- expand.grid('ex_ac'=ex_ac, 'lime_tha'=lime_tha)
newdata$response <- predict(mod0, newdata)
# mean analysis
aa <- emmeans::emmeans(mod0, ~lime_tha*ex_ac, at=list(lime_tha=c(0,1,2,3,4,5,6,7), ex_ac=c(0.2, 2.5)))
aa <- emmeans::emtrends(mod0, ~lime_tha*ex_ac, var='lime_tha', at=list(lime_tha=c(0,1,2,3,4,5,6,7), ex_ac=c(0.2, 2.5)))
aa <- as.data.frame(aa)
# plotting
plotting(0.2, 2.5, 'continuous')
# countour plot
interp_data <- with(newdata, interp(lime_tha, ex_ac, response))
filled.contour(interp_data, color.palette=terrain.colors)

# ----------------------------
# lime as categorical variable
mod0 <- lm(yield_tha ~ treatment * ex_ac, data=maize)
resid.inspect(mod0)
anova(mod0)
plot_model(mod0, type="pred", terms=c("treatment [all]", "ex_ac"))
# predict response (same as emmeans)
ex_ac <- c(0.2, 2.5)
treatment <- c('T1', 'T2', 'T3', 'T4')
newdata <- expand.grid('ex_ac'=ex_ac, 'treatment'=treatment)
newdata$response <- predict(mod0, newdata)
newdata$lime_tha <- ifelse(newdata$treatment=='T1', 0, NA)
newdata$lime_tha <- ifelse(newdata$treatment=='T2', 1, newdata$lime_tha)
newdata$lime_tha <- ifelse(newdata$treatment=='T3', 2.5, newdata$lime_tha)
newdata$lime_tha <- ifelse(newdata$treatment=='T4', 7, newdata$lime_tha)
plotting(0.2, 2.5, 'categorical')
# mean analysis
aa <- emmeans::emmeans(mod0, ~treatment*ex_ac, at=list(ex_ac=c(0.2, 2.5)))
plot(aa)

# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------
