
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

# ------------------------------------------------------------------------------
# bisrat, why is this?

df <- subset(df, !is.na(df$treatment))
data <- unique(df[c("fid", "crop", "treatment", "yield_tha", "admin2_gadm")])
data_agg <- aggregate(data$yield_tha, by=list('fid'=data$fid, 'admin2_gadm'=data$admin2_gadm, 'crop'=data$crop, 'treatment'=data$treatment), FUN=mean, na.rm=T)
data_rsh1 <- reshape2::dcast(data_agg, fid + admin2_gadm + crop ~ treatment, value.var='x')
data_rsh1 <- subset(data_rsh1, T1 > 0)
data_rsh1 <- na.omit(data_rsh1)
data_rsh1$respT2 <- data_rsh1$T2 - data_rsh1$T1
data_rsh1$respT3 <- data_rsh1$T3 - data_rsh1$T1
data_rsh1$respT4 <- data_rsh1$T4 - data_rsh1$T1
data_rsh <- reshape2::melt(data_rsh1[c("fid", "admin2_gadm", "crop", "respT2", "respT3", "respT4")], id.vars=c("fid", "admin2_gadm", "crop"))
soil <- unique(df[c("fid", "admin2_gadm", "crop", "p_h", "soc", "ex_ac", "psi", "ca", "mg", "k", "ecec", "hp_sat", "tex")])
data_rsh <- merge(data_rsh, soil, by=c("fid", "admin2_gadm", "crop"), all.x=T)
data_rsh <- na.omit(data_rsh)

# ------------------------------------------------------------------------------
# soil properties vs yield and response
# ------------------------------------------------------------------------------

data_rsh_rf <- reshape2::melt(data_rsh1[c("fid", "admin2_gadm", "crop", "T1", "respT2", "respT3", "respT4")], id.vars=c("fid", "admin2_gadm", "crop"))
data_rsh_rf <- merge(data_rsh_rf, soil, by=c("fid", "admin2_gadm", "crop"), all.x=T)
data_rsh_rf <- na.omit(data_rsh_rf)

# yield control vs soils
yield <- subset(data_rsh_rf, variable=='T1')
yield_imp <- c()
for(crp in unique(yield$crop)){
  yield_crop <- subset(yield, crop == crp)
  for(site in unique(yield_crop$admin2_gadm)){
    yield_site <- subset(yield_crop, admin2_gadm == site)
    rf <- randomForest::randomForest(value ~ soc + ex_ac + ecec + psi, data=yield_site)
    yield_site$pred_y <- predict(rf, yield_site[c('soc', 'ex_ac', 'ecec', 'psi')])
    imp <- data.frame('variable'='yieldT1', 'crop'=crp, 'site'=site, 'var'=row.names(rf$importance), rf$importance, rf$importance/mean(rf$importance), 
                      'r2'=round(1 - sum((yield_site$value-yield_site$pred_y)^2)/sum((yield_site$value-mean(yield_site$value))^2), 3))
    imp <- imp[order(imp$IncNodePurity, decreasing=T),]
    imp$rank <- seq(1,4,1)
    yield_imp <- rbind(yield_imp, imp)
    }
  }

# response vs soils
response <- subset(data_rsh_rf, variable!='T1')
response_imp <- c()
for(v in unique(response$variable)){
  response_var <- subset(response, variable == v)
  for(crp in unique(response_var$crop)){
    response_crop <- subset(response_var, crop == crp)
    for(site in unique(response_crop$admin2_gadm)){
      response_site <- subset(response_crop, admin2_gadm == site)
      rf <- randomForest::randomForest(value ~ soc + ex_ac + ecec + psi, data=response_site)
      response_site$pred_y <- predict(rf, response_site[c('soc', 'ex_ac', 'ecec', 'psi')])
      imp <- data.frame('variable'='response', 'crop'=crp, 'site'=site, 'treatment'=v, 'var'=row.names(rf$importance), rf$importance, rf$importance/mean(rf$importance), 
                        'r2'=round(1 - sum((response_site$value-response_site$pred_y)^2)/sum((response_site$value-mean(response_site$value))^2), 3))
      imp <- imp[order(imp$IncNodePurity, decreasing=T),]
      imp$rank <- seq(1,4,1)
      response_imp <- rbind(response_imp, imp)
      }
    }
  }

# both
both <- plyr::rbind.fill(yield_imp, response_imp)
write.csv(both, paste0(dir, './output-data/randomforest-results.csv'), row.names=F)

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
  crop <- subset(data_rsh, crop == crop_name)
  # -----------------------------
  if(crop_name == 'maize' | crop_name == 'beans'){
    formula0 <- as.formula('value ~ as.factor(variable) + (1 | admin2_gadm/fid)')
    formula1 <- as.formula('value ~ as.factor(variable) * as.factor(admin2_gadm) + (1 | fid)')
    formula2 <- as.formula('value ~ as.factor(variable) * (soc + ecec + ex_ac + psi) + (1 | admin2_gadm)')
    formulas <- c(formula1, formula2, formula0)
    label <- c('pooled-site-interaction', 'pooled-soil-interaction', 'pooled-site-random')
    i_len <- c(1,2,3)
  } else{
    formula1 <- as.formula('value ~ as.factor(variable) + (1 | fid)')
    formula2 <- as.formula('value ~ as.factor(variable) * (soc + ecec + ex_ac + psi)') 
    formulas <- c(formula1, formula2)
    label <- c('pooled-site-interaction', 'pooled-soil-interaction')
    i_len <- c(1,2)
  }
  formula3 <- as.formula('value ~ as.factor(variable) + (1 | fid)')
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
    write.csv(anova_pool_final, paste0(folder, label[i], '-anova-', crop_name, '-response.csv'), row.names=F)
    # fixed effects
    fixed_pool <- as.data.frame(coef(summary(m_pool)))
    fixed_pool$treatment <- row.names(fixed_pool)
    fixed_pool$model <- paste0(crop_name, '_pooled')
    row.names(fixed_pool) <- NULL
    write.csv(fixed_pool, paste0(folder, label[i], '-fe-', crop_name, '-response.csv'), row.names=F)
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
      write.csv(random_pool, paste0(folder, label[i], '-re-', crop_name, '-response.csv'), row.names=F) }
    # predict means
    if(i == 1 & crop_name == 'maize' | i == 1 & crop_name == 'beans'){
      levels_pool <- emmeans::emmeans(m_pool, ~ as.factor(variable) * as.factor(admin2_gadm), data=crop, adjust="none")
    } else if(i == 1 & crop_name != 'maize' | i == 1 & crop_name != 'beans' | i == 3 & crop_name == 'maize' | i == 3 & crop_name == 'beans'){
      levels_pool <- emmeans::emmeans(m_pool, ~ as.factor(variable), data=crop, adjust="none")
    } else if(i == 2){
      levels_pool <- emmeans::emmeans(m_pool, ~ as.factor(variable) * (soc + ecec + ex_ac + psi), data=crop, adjust="none")
    }
    predmeans_pool <- data.frame(multcomp::cld(levels_pool))
    write.csv(predmeans_pool, paste0(folder, label[i], '-predmeans-', crop_name, '-response.csv'), row.names=F)
  }
  # -----------------------------
  # (2) district model
  predmeans_f <- c()
  for(s in sites){
    df_sub <- subset(crop, admin2_gadm == s)
    print(s)
    m_district <- lmerTest::lmer(formula3, data=df_sub, REML=T)
    levels <- emmeans::emmeans(m_district, pairwise ~ as.factor(variable), data=df_sub, adjust="none")
    predmeans <- data.frame(multcomp::cld(levels$emmeans))
    predmeans$site <- s
    predmeans_f <- rbind(predmeans_f, predmeans)
    }
  write.csv(predmeans_f, paste0(folder, 'district-predmeans-', crop_name, '-response.csv'), row.names=F)
  return(crop)
  }

# ------------------------------------------------------------------------------
# run models
# ------------------------------------------------------------------------------

maize <- run_lmm(dataset=df, crop_name='maize', sites=c('Jimma', 'Mbozi', 'Geita', 'Nyaruguru', 'Ngororero', 'Burera'))
bean <- run_lmm(dataset=df, crop_name='beans', sites=c('Mbozi', 'Geita', 'Nyaruguru', 'Ngororero', 'Burera'))
wheat <- run_lmm(dataset=df, crop_name='wheat', sites=c('MisraqGojjam'))
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
