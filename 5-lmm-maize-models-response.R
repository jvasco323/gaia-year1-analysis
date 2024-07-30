
library(akima)
library(sjPlot)
library(ggplot2)
theme_set(theme_sjplot())

# ------------------------------------------------------------------------------
# start here
# ------------------------------------------------------------------------------

dir <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/2-yield-response/'

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

## for bisrat to correct --------------------
maize$yield_tha <- round(maize$yield_tha, 3)
maize$treatment <- ifelse(maize$yield_tha==2.052, 'T1', maize$treatment)
## ------------------------------------------

str(maize)
maize$country <- as.factor(maize$country)
maize$admin2_gadm <- as.factor(maize$admin2_gadm)
maize$fid <- as.factor(maize$fid)
maize$treatment <- as.factor(maize$treatment)

# reshape data
maize_rshp <- reshape2::dcast(maize, fid + crop + country + admin2_gadm + soc + ecec + psi + ex_ac + hp_sat ~ treatment, value.var='yield_tha')
maize_rshp$resp_T2 <- maize_rshp$T2 - maize_rshp$T1
maize_rshp$resp_T3 <- maize_rshp$T3 - maize_rshp$T1
maize_rshp$resp_T4 <- maize_rshp$T4 - maize_rshp$T1
maize <- reshape2::melt(maize_rshp[c(1:9,14:16)], id.vars=c("fid", "crop", "country", "admin2_gadm", "soc", "ecec", "psi", "ex_ac", "hp_sat"))

# grid to predict
ex_ac <- c(quantile(maize$ex_ac, c(0.2, 0.5, 0.8)))
ecec <- c(quantile(maize$ecec, c(0.2, 0.5, 0.8)))
soc <- c(quantile(maize$soc, c(0.2, 0.5, 0.8)))
psi <- c(quantile(maize$psi, c(0.2, 0.5, 0.8)))

# ------------------------------------------------------------------------------
# functions
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
    df1 <- subset(lime_tha_means, ex_ac!=quantile(lime_tha_means$ex_ac, 0.5) & ecec==quantile(lime_tha_means$ecec, 0.5) & soc==quantile(lime_tha_means$soc, 0.5) & psi==quantile(lime_tha_means$psi, 0.5))
    df2 <- subset(lime_tha_trends, ex_ac!=quantile(lime_tha_trends$ex_ac, 0.5) & ecec==quantile(lime_tha_trends$ecec, 0.5) & soc==quantile(lime_tha_trends$soc, 0.5) & psi==quantile(lime_tha_trends$psi, 0.5))
    par(mfrow=c(1,2))
    # plot 1
    plot(df1$lime_tha, df1$emmean)
    grid(nx=NULL, ny=NULL)
    lines(df1$lime_tha[df1$ex_ac==lowquant], df1$emmean[df1$ex_ac==lowquant], col=2)
    points(df1$lime_tha[df1$ex_ac==lowquant], df1$emmean[df1$ex_ac==lowquant], pch=21, bg=2)
    lines(df1$lime_tha[df1$ex_ac==highquant], df1$emmean[df1$ex_ac==highquant], col=3)
    points(df1$lime_tha[df1$ex_ac==highquant], df1$emmean[df1$ex_ac==highquant], pch=21, bg=3)
    # plot 2
    plot(df2$lime_tha, df2$lime_tha.trend)
    grid(nx=NULL, ny=NULL)
    lines(df2$lime_tha[df2$ex_ac==lowquant], df2$lime_tha.trend[df2$ex_ac==lowquant], col=2)
    points(df2$lime_tha[df2$ex_ac==lowquant], df2$lime_tha.trend[df2$ex_ac==lowquant], pch=21, bg=2)
    lines(df2$lime_tha[df2$ex_ac==highquant], df2$lime_tha.trend[df2$ex_ac==highquant], col=3)
    points(df2$lime_tha[df2$ex_ac==highquant], df2$lime_tha.trend[df2$ex_ac==highquant], pch=21, bg=3)
    abline(h=0)  
  } else{
    df1 <- subset(lime_t_means, ex_ac!=quantile(lime_t_means$ex_ac, 0.5) & ecec==quantile(lime_t_means$ecec, 0.5) & soc==quantile(lime_t_means$soc, 0.5) & psi==quantile(lime_t_means$psi, 0.5))
    par(mfrow=c(1,1))
    plot(df1$lime_tha, df1$emmean)
    grid(nx=NULL, ny=NULL)
    lines(df1$lime_tha[df1$ex_ac==lowquant], df1$emmean[df1$ex_ac==lowquant], col=2)
    points(df1$lime_tha[df1$ex_ac==lowquant], df1$emmean[df1$ex_ac==lowquant], pch=21, bg=2)
    lines(df1$lime_tha[df1$ex_ac==highquant], df1$emmean[df1$ex_ac==highquant], col=3)
    points(df1$lime_tha[df1$ex_ac==highquant], df1$emmean[df1$ex_ac==highquant], pch=21, bg=3)
  } }

# ------------------------------------------------------------------------------
# linear model (ols) --- soil model
# ------------------------------------------------------------------------------

# fit models
mod0 <- lm(value ~ variable * (ex_ac + soc + ecec + psi), data=maize)
resid.inspect(mod0)
anova(mod0)
plot_model(mod0, type="pred", terms=c("variable [all]", "ex_ac"))




# this is it!!
maize$site_pca <- ifelse(maize$admin2_gadm == 'Jimma', 'cluster1', NA)
maize$site_pca <- ifelse(maize$admin2_gadm == 'Burera' | maize$admin2_gadm == 'Mbozi', 'cluster2', maize$site_pca)
maize$site_pca <- ifelse(maize$admin2_gadm == 'Ngororero' | maize$admin2_gadm == 'Nyaruguru', 'cluster3', maize$site_pca)
maize$site_pca <- ifelse(maize$admin2_gadm == 'Geita', 'cluster4', maize$site_pca)

mod2 <- lmerTest::lmer(value ~ admin2_gadm + variable * (soc + ecec + ex_ac + psi) + (1 | fid), data=maize, REML=T)
mod1 <- lmerTest::lmer(value ~ variable * (soc + ecec + ex_ac + psi) + (1 | admin2_gadm/fid), data=maize, REML=T)
mod0 <- lmerTest::lmer(value ~ variable * (soc + ecec + ex_ac + psi) + (1 | fid), data=maize, REML=T)
anova(mod2, mod1, mod0)
summary(mod1)
anova(mod1)

expand.grid(ex_ac, ecec)

mod2 <- lmerTest::lmer(value ~ admin2_gadm * variable * (soc + ecec + ex_ac + psi) + (1 | fid), data=maize, REML=T)
anova(mod2)
av <- emmeans::emmeans(mod2, ~variable*soc | admin2_gadm, at=list('soc'=c(0.5, 1.4)))
av <- multcomp::cld(av)


# -------------------------------------------
# -------------------------------------------
# the below is the way to go! 

# share outline of paper -- joao
# code for soil type + finalize script for yield response -- joao
# share the codes with Fred -- joao
# figures -- joao/fred?
# text -- joao/fred?

# submission:
# - submit R codes
# - 'simple' outline paper


# different soil properties explain response to lime in different sites:
mod2 <- lmerTest::lmer(value ~ admin2_gadm * variable * (soc + ecec + ex_ac + psi) + (1 | fid), data=maize, REML=T)
# prediction for the average soil properties per site

# most important model!
# H0: different soil properties explain response to lime in different soil types:
# petal by site in PCA to have them by "soil type" -- keep this in main paper, and the one by site in SI
mod2 <- lmerTest::lmer(value ~ soil_type * variable * (soc + ecec + ex_ac + psi) + (1 | admin2_gadm/fid), data=maize, REML=T)
# prediction for the average soil properties per soil type
# the predictions will depend on stat significance in the anova
av <- emmeans::emmeans(mod2, ~variable | soil_type)
av_ecec <- emmeans::emmeans(mod2, ~variable*ecec | soil_type)
av_soc <- emmeans::emmeans(mod2, ~variable*ecec | soil_type)
...
# -------------------------------------------
# -------------------------------------------





# this is it!!
mod2 <- lmerTest::lmer(value ~ site_pca + variable * (soc + ecec + ex_ac + psi) + (1 | fid), data=maize, REML=T)
mod1 <- lmerTest::lmer(value ~ variable * (soc + ecec + ex_ac + psi) + (1 | site_pca/fid), data=maize, REML=T)
mod0 <- lmerTest::lmer(value ~ variable * (soc + ecec + ex_ac + psi) + (1 | fid), data=maize, REML=T)
anova(mod2, mod1, mod0)
summary(mod1)
anova(mod1)

# mean analysis
lime_t_means <- as.data.frame(emmeans::emmeans(mod0, ~variable*(ex_ac+soc+ecec+psi), at=list(ex_ac=ex_ac, ecec=ecec, soc=soc, psi=psi)))
lime_t_means$lime_tha <- ifelse(lime_t_means$treatment=='T1', 0, NA)
lime_t_means$lime_tha <- ifelse(lime_t_means$treatment=='T2', 1, lime_t_means$lime_tha)
lime_t_means$lime_tha <- ifelse(lime_t_means$treatment=='T3', 2.5, lime_t_means$lime_tha)
lime_t_means$lime_tha <- ifelse(lime_t_means$treatment=='T4', 7, lime_t_means$lime_tha)
plotting(quantile(maize$ex_ac, 0.2), quantile(maize$ex_ac, 0.8), 'categorical')

# ------------------------------------------------------------------------------
# linear mixed model (ml) --- soil model
# ------------------------------------------------------------------------------

maize$site_pca <- ifelse(maize$admin2_gadm == 'Jimma', 'cluster1', NA)
maize$site_pca <- ifelse(maize$admin2_gadm == 'Burera' | maize$admin2_gadm == 'Mbozi', 'cluster2', maize$site_pca)
maize$site_pca <- ifelse(maize$admin2_gadm == 'Ngororero' | maize$admin2_gadm == 'Nyaruguru', 'cluster3', maize$site_pca)
maize$site_pca <- ifelse(maize$admin2_gadm == 'Geita', 'cluster4', maize$site_pca)

# ---------------------------
# lime as continuous variable
# ---------------------------

# fit models

# try with response
# better categorical
mod2 <- lmerTest::lmer(yield_tha ~ admin2_gadm + poly(lime_tha, 2) * (soc + ecec + ex_ac + psi) + (1 | fid), data=maize, REML=T)
mod1 <- lmerTest::lmer(yield_tha ~ poly(lime_tha, 2) * (soc + ecec + ex_ac + psi) + (1 | country/admin2_gadm/fid), data=maize, REML=T)
mod0 <- lmerTest::lmer(yield_tha ~ poly(lime_tha, 2) * (soc + ecec + ex_ac + psi) + (1 | fid), data=maize, REML=T)



anova(mod2, mod1, mod0)  # use model 1
resid.inspect(mod1)
summary(mod1)
anova(mod1); write.csv(as.data.frame(anova(mod1)), paste0(dir, 'output-data/maize-anova-continuous.csv'))
plot_model(mod1, type="pred", pred.type='fe', terms=c("lime_tha [all]", "ex_ac"))

# mean analysis
lime_tha_means <- as.data.frame(emmeans::emmeans(mod1, ~lime_tha*(soc+ecec+ex_ac+psi), at=list(lime_tha=c(0,1,2.5,7), ex_ac=ex_ac, ecec=ecec, soc=soc, psi=psi)))
lime_tha_trends <- as.data.frame(emmeans::emtrends(mod1, ~lime_tha*(soc+ecec+ex_ac+psi), var='lime_tha', at=list(lime_tha=c(0,1,2.5,7), ex_ac=ex_ac, ecec=ecec, soc=soc, psi=psi)))
plotting(quantile(maize$ex_ac, 0.2), quantile(maize$ex_ac, 0.8), 'continuous')
write.csv(lime_tha_means, paste0(dir, 'output-data/maize-means-continuous.csv'), row.names=F)
write.csv(lime_tha_trends, paste0(dir, 'output-data/maize-trends-continuous.csv'), row.names=F)

# ----------------------------
# lime as categorical variable
# ----------------------------

# fit models
mod2 <- lmerTest::lmer(yield_tha ~ admin2_gadm + treatment * (soc + ecec + ex_ac + psi) + (1 | fid), data=maize, REML=T)
mod1 <- lmerTest::lmer(yield_tha ~ treatment * (soc + ecec + ex_ac + psi) + (1 | country/admin2_gadm/fid), data=maize, REML=T)
mod0 <- lmerTest::lmer(yield_tha ~ treatment * (soc + ecec + ex_ac + psi) + (1 | fid), data=maize, REML=T)
anova(mod2, mod1, mod0) # use model 1
resid.inspect(mod1)
summary(mod1)
anova(mod1); write.csv(as.data.frame(anova(mod1)), paste0(dir, 'output-data/maize-anova-categorical.csv'))
plot_model(mod1, type="pred", pred.type='fe', terms=c("treatment [all]", "ex_ac"))

# mean analysis
lime_t_means <- as.data.frame(emmeans::emmeans(mod0, ~treatment*(soc+ecec+ex_ac+psi), at=list(ex_ac=ex_ac, ecec=ecec, soc=soc, psi=psi)))
lime_t_means$lime_tha <- ifelse(lime_t_means$treatment=='T1', 0, NA)
lime_t_means$lime_tha <- ifelse(lime_t_means$treatment=='T2', 1, lime_t_means$lime_tha)
lime_t_means$lime_tha <- ifelse(lime_t_means$treatment=='T3', 2.5, lime_t_means$lime_tha)
lime_t_means$lime_tha <- ifelse(lime_t_means$treatment=='T4', 7, lime_t_means$lime_tha)
plotting(quantile(maize$ex_ac, 0.2), quantile(maize$ex_ac, 0.8), 'categorical')
write.csv(lime_t_means, paste0(dir, 'output-data/maize-means-categorical.csv'), row.names=F)

# ------------------------------------------------------------------------------
# linear mixed model (ml) --- site model
# ------------------------------------------------------------------------------

# formulas
formula0 <- as.formula('yield_tha ~ treatment + (1 | fid)')
formula1 <- as.formula('yield_tha ~ treatment * admin2_gadm + (1 | fid)') 
formula2 <- as.formula('yield_tha ~ treatment * admin2_gadm + country + (1 | fid)')  # rank-deficient/no convergence

# model 0
mod0 <- lmerTest::lmer(formula0, data=maize, REML=T)
resid.inspect(mod0)
summary(mod0)
anova(mod0); write.csv(as.data.frame(anova(mod0)), paste0(dir, 'output-data/maize-anova-pooled-1.csv'))
plot_model(mod0, type="pred", pred.type='fe', terms=c("treatment"))
means_mod0 <- emmeans::emmeans(mod0, ~treatment)
means_mod0 <- data.frame(multcomp::cld(means_mod0))
write.csv(means_mod0, paste0(dir, 'output-data/maize-means-pooled-1.csv'), row.names=F)

# model 1
mod1 <- lmerTest::lmer(formula1, data=maize, REML=T)
resid.inspect(mod1)
summary(mod1)
anova(mod1); write.csv(as.data.frame(anova(mod1)), paste0(dir, 'output-data/maize-anova-pooled-2.csv'))
plot_model(mod1, type="pred", pred.type='fe', terms=c("treatment", "admin2_gadm"))
means_mod1 <- emmeans::emmeans(mod1, ~treatment*admin2_gadm)
means_mod1 <- data.frame(multcomp::cld(means_mod1, by='admin2_gadm'))
write.csv(means_mod1, paste0(dir, 'output-data/maize-means-pooled-2.csv'), row.names=F)

# model 2
mod2 <- lmerTest::lmer(formula2, data=maize, REML=T)
# stop: rank-deficient or not converging

# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------
