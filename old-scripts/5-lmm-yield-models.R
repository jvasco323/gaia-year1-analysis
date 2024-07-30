
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


mod0 <- randomForest::randomForest(yield_tha ~ lime_tha + ex_ac + soc + ecec + psi, data=maize, importance=T)
mod0$importance
ex_ac <- c(0.2,0.6,1.5)
lime_tha <- c(0,1,2.5,7)
newdata <- expand.grid('ex_ac'=ex_ac, 'lime_tha'=lime_tha, 'soc'=mean(maize$soc), 'psi'=mean(maize$psi), 'ecec'=mean(maize$ecec))
newdata$response <- predict(mod0, newdata)
plot(newdata$lime_tha, newdata$response)
lines(newdata$lime_tha[newdata$ex_ac==0.2], newdata$response[newdata$ex_ac==0.2], col=2)
points(newdata$lime_tha[newdata$ex_ac==0.2], newdata$response[newdata$ex_ac==0.2], pch=21, bg=2)
lines(newdata$lime_tha[newdata$ex_ac==0.6], newdata$response[newdata$ex_ac==0.6], col=3)
points(newdata$lime_tha[newdata$ex_ac==0.6], newdata$response[newdata$ex_ac==0.6], pch=21, bg=3)
lines(newdata$lime_tha[newdata$ex_ac==1.5], newdata$response[newdata$ex_ac==1.5], col=5)
points(newdata$lime_tha[newdata$ex_ac==1.5], newdata$response[newdata$ex_ac==1.5], pch=21, bg=5)
library(ranger)
library(kernelshap)
library(shapviz)
set.seed(1)
final <- maize[c('yield_tha', 'admin2_gadm', 'hp_sat', 'lime_tha', 'ex_ac', 'soc', 'ecec', 'psi')]
fit <- ranger(yield_tha ~ lime_tha + ex_ac + soc + ecec + psi, data=final)
s <- kernelshap(fit, final[-1], bg_X=final)
sv <- shapviz(s)
sv_importance(sv, kind="bee")
sv_dependence(sv, v="ex_ac", color_var="auto")
sv_dependence(sv, v="hp_sat", color_var="auto")
sv_dependence(sv, v="lime_tha", color_var="auto")
sv_dependence(sv, v="admin2_gadm", color_var="auto")


# ---------------------
# ---------------------
# linear model approach
library(sjPlot)
library(ggplot2)
theme_set(theme_sjplot())

# lime as continuous and quadratic
mod0 <- lm(yield_tha ~ admin2_gadm + poly(lime_tha, 2) * (soc + ecec + ex_ac + psi), data=maize)
# plot(mod0)
summary(mod0)
anova(mod0)
plot_model(mod0, type = "pred", terms = c("lime_tha [all]", "ex_ac"))
plot_model(mod0, type = "pred", terms = c("lime_tha [all]", "ecec"))

# quadratic behaviour
mod0 <- lm(yield_tha ~ admin2_gadm + poly(lime_tha, 2) * (soc + ecec + ex_ac + psi), data=maize)
exac <- emmeans::emmeans(mod0, ~ lime_tha * ex_ac, at=list(lime_tha=c(0,1,2.5,7, 10), ex_ac=c(0.21,2.5)), params="deg")
plot(exac, horizontal=F)
mod0 <- lm(yield_tha ~ admin2_gadm + lime_tha * (soc + ecec + ex_ac + psi), data=maize)
exac <- emmeans::emmeans(mod0, ~ lime_tha * ex_ac, at=list(lime_tha=c(0,1,2.5,7, 10), ex_ac=c(0.21,2.5)), params="deg")
plot(exac, horizontal=F)


mod0 <- lm(yield_tha ~ treatment * ex_ac, data=maize)
aa <- emmeans::emtrends(mod0, ~treatment, var='ex_ac' )
print(aa)
emtrends(mod0, pairwise ~ treatment, var = "ex_ac")
emmeans::emmip(mod0, treatment ~ ex_ac, cov.reduce = range)

mod0 <- lm(yield_tha ~ lime_tha * ex_ac, data=maize)
aa <- emmeans::emtrends(mod0, var='lime_tha', at=list(ex_ac=c(0.2, 2.5)))
aa <- emmeans::emtrends(mod0, ~lime_tha*ex_ac, var='lime_tha', at=list(ex_ac=c(0.2, 2.5)))
print(aa)


mod0 <- lm(yield_tha ~ poly(lime_tha,2) * ex_ac, data=maize)
# mod0 <- lm(yield_tha ~ lime_tha * ex_ac, data=maize)
aa <- emmeans::emtrends(mod0, ~lime_tha*ex_ac, var='lime_tha', at=list(lime_tha=c(0,1,2.5,5,7), ex_ac=c(0.2, 2.5)))
print(aa)
aa <- as.data.frame(aa)
plot(aa$lime_tha, aa$lime_tha.trend)
points(aa$lime_tha[aa$ex_ac==0.2], aa$lime_tha.trend[aa$ex_ac==0.2], col=2)
lines(aa$lime_tha[aa$ex_ac==0.2], aa$lime_tha.trend[aa$ex_ac==0.2], col=2)
points(aa$lime_tha[aa$ex_ac==2.5], aa$lime_tha.trend[aa$ex_ac==2.5], col=3)
lines(aa$lime_tha[aa$ex_ac==2.5], aa$lime_tha.trend[aa$ex_ac==2.5], col=3)
abline(h=0)


mod0 <- lm(yield_tha ~ (lime_tha + I(lime_tha^2)) * ex_ac, data=maize)
summary(mod0)
aa <- emmeans::emtrends(mod0, var='lime_tha', at=list(ex_ac=c(0.2, 1, 1.5, 2.5)))
aa <- as.data.frame(aa)

mod0 <- lm(yield_tha ~ lime_tha * ex_ac, data=maize)
summary(mod0)
ggplot(maize, aes(x=lime_tha, y=yield_tha, color=ex_ac)) + geom_point()
ex_ac <- seq(0, 2.5, 0.5)
lime_tha <- c(0,1,2,3,4,5,6,7)
newdata <- expand.grid('ex_ac'=ex_ac, 'lime_tha'=lime_tha)
newdata$response <- predict(mod0, newdata)
plot(newdata$lime_tha, newdata$response)

mod0 <- lm(yield_tha ~ poly(lime_tha,2) * ex_ac, data=maize)
ex_ac <- c(0.2, 2.5)
lime_tha <- c(0,1,2,3,4,5,6,7)
newdata <- expand.grid('ex_ac'=ex_ac, 'lime_tha'=lime_tha)
newdata$response <- predict(mod0, newdata)
plot(newdata$lime_tha, newdata$response)
points(newdata$lime_tha[newdata$ex_ac==0.2], newdata$response[newdata$ex_ac==0.2], pch=21, bg=2)
points(newdata$lime_tha[newdata$ex_ac==2.5], newdata$response[newdata$ex_ac==2.5], pch=21, bg=3)
library('akima')
interp_data <- with(newdata, interp(lime_tha, ex_ac, response))
filled.contour(interp_data, color.palette=terrain.colors)

# prediction with RF


exac <- emmeans::emmeans(mod0, ~ lime_tha * (soc + ecec + ex_ac + psi), at=list(lime_tha=c(0,1,2.5,7), ex_ac=c(0.21,2.5)), params="deg")
exac <- data.frame(multcomp::cld(exac))
ecec <- emmeans::emmeans(mod0, ~ lime_tha * ecec, at=list(lime_tha=c(0,1,2.5,7), ecec=c(5,15)), params="deg")
ecec <- data.frame(multcomp::cld(ecec))
soc <- emmeans::emmeans(mod0, ~ lime_tha * soc, at=list(lime_tha=c(0,1,2.5,7), soc=c(0.5,2)), params="deg")
soc <- data.frame(multcomp::cld(soc))
psi <- emmeans::emmeans(mod0, ~ lime_tha * psi, at=list(lime_tha=c(0,1,2.5,7), psi=c(50,200)), params="deg")
psi <- data.frame(multcomp::cld(psi))

# lime as factor
mod0 <- lm(yield_tha ~ admin2_gadm + treatment * (soc + ecec + ex_ac + psi), data=maize)
plot(mod0)
summary(mod0)
anova(mod0)
plot_model(mod0, type = "pred", terms = c("treatment", "ex_ac"))
plot_model(mod0, type = "pred", terms = c("treatment", "ecec"))
exac <- emmeans::emmeans(mod0, ~ treatment * ex_ac, at=list(ex_ac=c(0.5,2.5)))
exac <- data.frame(multcomp::cld(exac))
ecec <- emmeans::emmeans(mod0, ~ treatment * ecec, at=list(ecec=c(5,15)))
ecec <- data.frame(multcomp::cld(ecec))
soc <- emmeans::emmeans(mod0, ~ treatment * soc, at=list(soc=c(0.5,2)))
soc <- data.frame(multcomp::cld(soc))
psi <- emmeans::emmeans(mod0, ~ treatment * psi, at=list(psi=c(50,200)))
psi <- data.frame(multcomp::cld(psi))

# ---------------------------
# ---------------------------
# linear mixed model approach
library(sjPlot)
library(ggplot2)
theme_set(theme_sjplot())

# lime as continuous and quadratic
mod1 <- lmerTest::lmer(yield_tha ~ admin2_gadm + poly(lime_tha, 2) * (soc + ecec + ex_ac + psi) + (1 | fid), data=maize, REML=T)
mod0 <- lmerTest::lmer(yield_tha ~ poly(lime_tha, 2) * (soc + ecec + ex_ac + psi) + (1 | fid), data=maize, REML=T)
anova(mod0, mod1)
plot(mod1)
summary(mod1)
anova(mod1)
plot_model(mod1, type = "pred", terms = c("lime_tha [all]", "ex_ac"))
plot_model(mod1, type = "pred", terms = c("lime_tha [all]", "ecec"))


exac <- emmeans::emmeans(mod1, ~ lime_tha * (soc + ecec + ex_ac + psi), at=list(lime_tha=c(0,1,2.5,7), ex_ac=c(0.21,1.13)), params="deg")

exac <- emmeans::emmeans(mod1, ~ poly(lime_tha, 2) * ex_ac, at=list(lime_tha=c(0,1,2.5,7), ex_ac=c(0.21,1.13)))

maize$pred <- predict(mod1, maize)
plot(maize$yield_tha, maize$pred)
abline(a=0, b=1, col=2)

exac <- data.frame(multcomp::cld(exac))

plot(exac$lime_tha, exac$emmean)

ecec <- emmeans::emmeans(mod1, ~ lime_tha * ecec, at=list(lime_tha=c(0,1,2.5,7), ecec=c(5,15)), params="deg")
ecec <- data.frame(multcomp::cld(ecec))
soc <- emmeans::emmeans(mod1, ~ lime_tha * soc, at=list(lime_tha=c(0,1,2.5,7), soc=c(0.5,2)), params="deg")
soc <- data.frame(multcomp::cld(soc))
psi <- emmeans::emmeans(mod1, ~ lime_tha * psi, at=list(lime_tha=c(0,1,2.5,7), psi=c(50,200)), params="deg")
psi <- data.frame(multcomp::cld(psi))

# lime as factor
mod1 <- lmerTest::lmer(yield_tha ~ admin2_gadm + treatment * (soc + ecec + ex_ac + psi) + (1 | fid), data=maize, REML=T)
mod0 <- lmerTest::lmer(yield_tha ~ treatment * (soc + ecec + ex_ac + psi) + (1 | fid), data=maize, REML=T)
anova(mod0, mod1)
plot(mod1)
summary(mod1)
anova(mod1)
plot_model(mod1, type = "pred", terms = c("treatment", "ex_ac"))
plot_model(mod1, type = "pred", terms = c("treatment", "ecec"))
exac <- emmeans::emmeans(mod1, ~ treatment * ex_ac, at=list(ex_ac=c(0.21,1.13)))
exac <- data.frame(multcomp::cld(exac))
ecec <- emmeans::emmeans(mod1, ~ treatment * ecec, at=list(ecec=c(5,15)))
ecec <- data.frame(multcomp::cld(ecec))
soc <- emmeans::emmeans(mod1, ~ treatment * soc, at=list(soc=c(0.5,2)))
soc <- data.frame(multcomp::cld(soc))
psi <- emmeans::emmeans(mod1, ~ treatment * psi, at=list(psi=c(50,200)))
psi <- data.frame(multcomp::cld(psi))

# ---------------
# ---------------
# agwise approach
mod0 <- lmerTest::lmer(yield_tha ~ treatment + country + admin2_gadm + (1 | fid), data=maize, REML=T)
summary(mod0)
mod1 <- lmerTest::lmer(yield_tha ~ treatment + country + (1 | fid), data=maize, REML=T)
summary(mod1)
mod2 <- lmerTest::lmer(yield_tha ~ treatment + admin2_gadm + (1 | fid), data=maize, REML=T)
summary(mod2)
mod3 <- lmerTest::lmer(yield_tha ~ treatment + (1 | fid), data=maize, REML=T)
summary(mod3)
anova(mod0, mod1, mod2, mod3) # model selection: keep mod0
maize$blup <- predict(mod0, maize)
# next steps
# ML model with soil properties
# blups ~ RF ( treatment + soil (+ rainfall_gs ...) )
# map <- predict (treatment = c(0, 7, 0.5), raster_stack)
# measured properties vs predicted properties
# omit soil variables one by one.. soil ommitted vs R2
# compare to lime requirement models









                       
# ------------------------------------------------------------------------------
# function to check models
# ------------------------------------------------------------------------------

resid.inspect <- function(resid, fitd, col="black"){
  par(mfrow=c(1, 3))
  hist(resid / sd(resid, na.rm=T), 30, main="")
  plot(fitd, resid / sd(resid, na.rm=T), col=col)
  qqnorm(resid / sd(resid, na.rm=T))
  abline(coef=c(0, 1)) }

# ------------------------------------------------------------------------------
# function to run models
# ------------------------------------------------------------------------------

run_lmm <- function(dataset, crop_name, sites){
  folder <- paste0(dir, 'output-data/models/')
  crop <- subset(df, crop == crop_name)
  # -----------------------------
  if(crop_name == 'maize' | crop_name == 'beans'){

    base model 'yield_tha ~ as.factor(lime_tha) + country + site + (1 | fid)'
    
    
    formula0 <- as.formula('yield_tha ~ as.factor(lime_tha) + (1 | admin2_gadm/fid)')
    formula1 <- as.formula('yield_tha ~ as.factor(lime_tha) * as.factor(admin2_gadm) + (1 | fid)')
    formula2 <- as.formula('yield_tha ~ as.factor(lime_tha) * (soc + ecec + ex_ac + psi) + (1 | admin2_gadm)')
    formulas <- c(formula1, formula2, formula0)
    label <- c('pooled-site-interaction', 'pooled-soil-interaction', 'pooled-site-random')
    i_len <- c(1,2,3)
  } else{
    formula1 <- as.formula('yield_tha ~ as.factor(lime_tha) + (1 | fid)')
    formula2 <- as.formula('yield_tha ~ as.factor(lime_tha) * (soc + ecec + ex_ac + psi)') 
    formulas <- c(formula1, formula2)
    label <- c('pooled-site-interaction', 'pooled-soil-interaction')
    i_len <- c(1,2)
  }
  formula3 <- as.formula('yield_tha ~ as.factor(lime_tha) + (1 | fid)')
  # -----------------------------
  # (1) pooled model 
  for(i in i_len){
    print(i)
    # fit model
    if(i == 2 & crop_name == 'wheat' | i == 2 & crop_name == 'soybean' | i == 2 & crop_name == 'fababean'){
      m_pool <- lm(formulas[[i]], data=crop)
    } else{
      m_pool <- lmerTest::lmer(formulas[[i]], data=crop, REML=T) }  
    crop$resid1 = resid(m_pool)
    crop$fitted1 = fitted(m_pool)
    # anova
    anova_pool_final <- as.data.frame(anova(m_pool)) 
    anova_pool_final$variable <- row.names(anova_pool_final)
    anova_pool_final$model <- paste0(crop_name, '-', label[i])
    row.names(anova_pool_final) <- NULL
    write.csv(anova_pool_final, paste0(folder, label[i], '-anova-', crop_name, '.csv'), row.names=F)
    # fixed effects
    fixed_pool <- as.data.frame(coef(summary(m_pool)))
    fixed_pool$treatment <- row.names(fixed_pool)
    fixed_pool$model <- paste0(crop_name, '-', label[i])
    row.names(fixed_pool) <- NULL
    write.csv(fixed_pool, paste0(folder, label[i], '-fe-', crop_name, '.csv'), row.names=F)
    # random effects
    if(i == 2 & crop_name == 'wheat' | i == 2 & crop_name == 'soybean' | i == 2 & crop_name == 'fababean'){
      random_pool <- NULL 
    } else{
      random_pool <- as.data.frame(lme4::VarCorr(m_pool))
      random_pool <- random_pool[c(1,4,5)]
      random_pool$model <- paste0(crop_name, '-', label[i])
      colnames(random_pool)[1] <- 'treatment'
      colnames(random_pool)[2] <- 'Estimate'
      colnames(random_pool)[3] <- 'Std. Error'
      write.csv(random_pool, paste0(folder, label[i], '-re-', crop_name, '.csv'), row.names=F) }
    # predict means
    if(i == 1 & crop_name == 'maize' | i == 1 & crop_name == 'beans'){
      levels_pool <- emmeans::emmeans(m_pool, ~ as.factor(lime_tha) * as.factor(admin2_gadm), data=crop, adjust="none")
    } else if(i == 1 & crop_name != 'maize' | i == 1 & crop_name != 'beans' | i == 3 & crop_name == 'maize' | i == 3 & crop_name == 'beans'){
      levels_pool <- emmeans::emmeans(m_pool, ~ as.factor(lime_tha), data=crop, adjust="none")
    } else if(i == 2){
      levels_pool <- emmeans::emmeans(m_pool, ~ as.factor(lime_tha) * (soc + ecec + ex_ac + psi), data=crop, adjust="none")
    }
    predmeans_pool <- data.frame(multcomp::cld(levels_pool))
    write.csv(predmeans_pool, paste0(folder, label[i], '-predmeans-', crop_name, '.csv'), row.names=F)
  }
  # -----------------------------
  # (2) district model
  predmeans_f <- c()
  for(s in sites){
    df_sub <- subset(crop, admin2_gadm == s)
    print(s)
    m_district <- lmerTest::lmer(formula3, data=df_sub, REML=T)
    levels <- emmeans::emmeans(m_district, pairwise ~ as.factor(lime_tha), data=df_sub, adjust="none")
    predmeans <- data.frame(multcomp::cld(levels$emmeans))
    predmeans$site <- s
    predmeans_f <- rbind(predmeans_f, predmeans)
  }
  write.csv(predmeans_f, paste0(folder, 'district-predmeans-', crop_name, '.csv'), row.names=F)
  return(crop)
}

# ------------------------------------------------------------------------------
# run models
# ------------------------------------------------------------------------------

maize <- run_lmm(dataset=df, crop_name='maize', sites=c('Jimma', 'Mbozi', 'Geita', 'Nyaruguru', 'Ngororero', 'Burera'))
wheat <- run_lmm(dataset=df, crop_name='wheat', sites=c('MisraqGojjam'))
bean <- run_lmm(dataset=df, crop_name='beans', sites=c('Mbozi', 'Geita', 'Nyaruguru', 'Ngororero', 'Burera'))
soy <- run_lmm(dataset=df, crop_name='soybean', sites=c('Jimma'))
faba <- run_lmm(dataset=df, crop_name='fababean', sites=c('MisraqGojjam'))

# ------------------------------------------------------------------------------
# check residuals
# ------------------------------------------------------------------------------

resid.inspect(maize$resid1, maize$fitted1)
resid.inspect(wheat$resid1, wheat$fitted1)
resid.inspect(bean$resid1, bean$fitted1)
resid.inspect(soy$resid1, soy$fitted1)
resid.inspect(faba$resid1, faba$fitted1)

# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------
