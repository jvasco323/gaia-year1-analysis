
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
df <- unique(df[c("fid", "crop", "treatment", "lime_tha", "yield_tha", "admin2_gadm", "tex", "soc", "ecec", "ex_ac", "ca", "mg", "k", "psi")])
df <- na.omit(df) # to check how to fill some of the NA's


names(df)

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
    formula0 <- as.formula('yield_tha ~ as.factor(treatment) + (1 | admin2_gadm/fid)')
    formula1 <- as.formula('yield_tha ~ as.factor(treatment) * as.factor(admin2_gadm) + (1 | fid)')
    formula2 <- as.formula('yield_tha ~ as.factor(treatment) * (soc + ecec + ex_ac + psi) + (1 | admin2_gadm)')
    formulas <- c(formula1, formula2, formula0)
    label <- c('pooled-site-interaction', 'pooled-soil-interaction', 'pooled-site-random')
    i_len <- c(1,2,3)
  } else{
    formula1 <- as.formula('yield_tha ~ as.factor(treatment) + (1 | fid)')
    formula2 <- as.formula('yield_tha ~ as.factor(treatment) * (soc + ecec + ex_ac + psi)') 
    formulas <- c(formula1, formula2)
    label <- c('pooled-site-interaction', 'pooled-soil-interaction')
    i_len <- c(1,2)
  }
  formula3 <- as.formula('yield_tha ~ as.factor(treatment) + (1 | fid)')
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
    anova_pool_final$model <- paste0(crop_name, '_pooled')
    row.names(anova_pool_final) <- NULL
    write.csv(anova_pool_final, paste0(folder, label[i], '-anova-', crop_name, '.csv'), row.names=F)
    # fixed effects
    fixed_pool <- as.data.frame(coef(summary(m_pool)))
    fixed_pool$treatment <- row.names(fixed_pool)
    fixed_pool$model <- paste0(crop_name, '_pooled')
    row.names(fixed_pool) <- NULL
    write.csv(fixed_pool, paste0(folder, label[i], '-fe-', crop_name, '.csv'), row.names=F)
    # random effects
    if(i == 2 & crop_name == 'wheat' | i == 2 & crop_name == 'soybean' | i == 2 & crop_name == 'fababean'){
      random_pool <- NULL 
    } else{
      random_pool <- as.data.frame(lme4::VarCorr(m_pool))
      random_pool <- random_pool[c(1,4,5)]
      random_pool$model <- paste0(crop_name, '_pooled')
      colnames(random_pool)[1] <- 'treatment'
      colnames(random_pool)[2] <- 'Estimate'
      colnames(random_pool)[3] <- 'Std. Error'
      write.csv(random_pool, paste0(folder, label[i], '-re-', crop_name, '.csv'), row.names=F) }
    # predict means
    if(i == 1 & crop_name == 'maize' | i == 1 & crop_name == 'beans'){
      levels_pool <- emmeans::emmeans(m_pool, ~ as.factor(treatment) * as.factor(admin2_gadm), data=crop, adjust="none")
    } else if(i == 1 & crop_name != 'maize' | i == 1 & crop_name != 'beans' | i == 3 & crop_name == 'maize' | i == 3 & crop_name == 'beans'){
      levels_pool <- emmeans::emmeans(m_pool, ~ as.factor(treatment), data=crop, adjust="none")
    } else if(i == 2){
      levels_pool <- emmeans::emmeans(m_pool, ~ as.factor(treatment) * (soc + ecec + ex_ac + psi), data=crop, adjust="none")
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
    levels <- emmeans::emmeans(m_district, pairwise ~ as.factor(treatment), data=df_sub, adjust="none")
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
