
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
df <- unique(df[c("fid", "admin2_gadm", 'p_h', 'ecec', 'ex_ac', 'hp_sat', 'ca', 'mg', 'tex', 'soc', 'psi')])
df <- na.omit(df) # to check how to fill some of the NA's

# ------------------------------------------------------------------------------
# anova
# ------------------------------------------------------------------------------

predmeans_f <- c()
for(v in c('p_h', 'ecec', 'ca', 'mg', 'ex_ac', 'hp_sat', 'soc', 'psi', 'tex')){  # hp_sat --> predicted < 0 for jimma...
  print(v)
  m <- lmerTest::lmer(df[,c(v)] ~ admin2_gadm + (1 | fid), data=df)
  # get residuals
  df$resid = resid(m)
  hist(df$resid)
  # anova
  anova_final <- as.data.frame(anova(m)) 
  anova_final$variable <- row.names(anova_final)
  anova_final$m <- v
  row.names(anova_final) <- NULL
  # fixed effects
  fixed <- as.data.frame(coef(summary(m)))
  fixed$treatment <- row.names(fixed)
  fixed$m <- v
  row.names(fixed) <- NULL
  # random effects
  random <- as.data.frame(lme4::VarCorr(m))
  random <- random[c(1,4,5)]
  random$m <- v
  colnames(random)[1] <- 'treatment'
  colnames(random)[2] <- 'Estimate'
  colnames(random)[3] <- 'Std. Error'
  # predict means: year
  levels <- emmeans::emmeans(m, pairwise ~ admin2_gadm, data=df, adjust="none")
  predmeans <- data.frame(multcomp::cld(levels$emmeans))
  predmeans$m <- v
  predmeans_f <- rbind(predmeans_f, predmeans)
  }
write.csv(predmeans_f, paste0(dir, 'output-data/models/district-predmeans-soils.csv'), row.names=F)

# reshape data
aa <- predmeans_f
aa$emmean <- round(aa$emmean, 1)
aa$.group <- gsub(' ', '', aa$.group)
aa$.group <- gsub('1', 'a', aa$.group)
aa$.group <- gsub('2', 'b', aa$.group)
aa$.group <- gsub('3', 'c', aa$.group)
aa$.group <- gsub('4', 'd', aa$.group)
aa$.group <- gsub('5', 'e', aa$.group)
aa$.group <- gsub('6', 'f', aa$.group)
aa$emmean <- paste0(aa$emmean, ' ', aa$.group)
aa <- reshape2::dcast(aa, admin2_gadm ~ m, value.var='emmean')
aa <- aa[c('admin2_gadm', 'p_h', 'ecec', 'ca', 'mg', 'ex_ac', 'psi', 'soc', 'tex')]
write.csv(aa, paste0(dir, 'output-data/models/district-predmeans-soils-rshp.csv'), row.names=F)

# generate final tables
final <- predmeans_f[c("admin2_gadm", "emmean", "m")]
final <- reshape2::dcast(final, admin2_gadm ~ m, value.var='emmean')
final2 <- predmeans_f[c("admin2_gadm", ".group", "m")]
final2$.group <- gsub(' ', '', final2$.group)
final2 <- reshape2::dcast(final2, admin2_gadm ~ m, value.var='.group')

# merge with yield data
df_yield <- read.csv(paste0(dir, 'output-data/predicted-means-allcrops.csv'))
all <- merge(df_yield, final, by.x='site', by.y='admin2_gadm')
write.csv(all, paste0(dir, 'output-data/predicted_means_allcrops_soils.csv'), row.names=F)

# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------
