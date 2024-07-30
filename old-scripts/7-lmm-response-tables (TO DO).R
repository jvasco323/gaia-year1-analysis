
dir <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/2-yield-response/'

all_crops_aov <- c()
all_crops_fe <- c()
all_crops_re <- c()
for(crp in c('maize', 'beans', 'wheat', 'soybean', 'fababean')){
  
  # ----------------------------------------------------------------------------
  # anova
  # ----------------------------------------------------------------------------
  
  crp <- 'beans'
  # problem with model name in response models -- correct outputs of LMM!!
  
  fname <- paste0('pooled-site-random-anova-', crp, '-response.csv')
  if(fname %in% list.files(path=paste0(dir, 'output-data/models/'), pattern=".*csv$")){
    aov_random <- read.csv(paste0(dir, 'output-data/models/', fname)) 
  } else{ aov_random <- c() }
  fname <- paste0('pooled-site-interaction-anova-', crp, '-response.csv')
  if(fname %in% list.files(path=paste0(dir, 'output-data/models/'), pattern=".*csv$")){
    aov_interaction <- read.csv(paste0(dir, 'output-data/models/', fname)) 
  } else{ aov_interaction <- c() }  
  fname <- paste0('pooled-soil-interaction-anova-', crp, '-response.csv')
  if(fname %in% list.files(path=paste0(dir, 'output-data/models/'), pattern=".*csv$")){
    aov_soil <- read.csv(paste0(dir, 'output-data/models/', fname)) 
  } else{ aov_soil <- c() }  
  aov <- plyr::rbind.fill(aov_random, aov_interaction, aov_soil)[c(8,7,1:6)]
  aov$crop <- crp
  aov$model <- gsub(paste0(crp, '-pooled-'), '', aov$model, fixed=T)
  aov$variable <- gsub('as.factor', '', aov$variable)
  aov[c(3:8)] <- round(aov[c(3:8)], 2)
  aov <- aov[c(9,1:8)]
  all_crops_aov <- rbind(all_crops_aov, aov)
  
  # ----------------------------------------------------------------------------
  # fixed effects
  # ----------------------------------------------------------------------------
  
  fname <- paste0('pooled-site-random-fe-', crp, '-response.csv')
  if(fname %in% list.files(path=paste0(dir, 'output-data/models/'), pattern=".*csv$")){
    fe_random <- read.csv(paste0(dir, 'output-data/models/', fname)) 
  } else{ fe_random <- c() }
  fname <- paste0('pooled-site-interaction-fe-', crp, '-response.csv')
  if(fname %in% list.files(path=paste0(dir, 'output-data/models/'), pattern=".*csv$")){
    fe_interaction <- read.csv(paste0(dir, 'output-data/models/', fname)) 
  } else{ fe_interaction <- c() }  
  fname <- paste0('pooled-soil-interaction-fe-', crp, '-response.csv')
  if(fname %in% list.files(path=paste0(dir, 'output-data/models/'), pattern=".*csv$")){
    fe_soil <- read.csv(paste0(dir, 'output-data/models/', fname)) 
  } else{ fe_soil <- c() }  
  fe <- plyr::rbind.fill(fe_random, fe_interaction, fe_soil)[c(7,6,1:5)]
  fe$crop <- crp
  fe$model <- gsub(paste0(crp, '-pooled-'), '', fe$model, fixed=T)
  fe$treatment <- gsub('as.factor', '', fe$treatment)
  fe$treatment <- gsub('(admin2_gadm)', '', fe$treatment, fixed=T)
  fe$treatment <- gsub('(lime_tha)', '', fe$treatment, fixed=T)
  fe[c(3:7)] <- round(fe[c(3:7)], 2)
  fe <- fe[c(8,1:4,7)]
  all_crops_fe <- rbind(all_crops_fe, fe)
  
  # ----------------------------------------------------------------------------
  # random effects
  # ----------------------------------------------------------------------------
  
  fname <- paste0('pooled-site-random-re-', crp, '-response.csv')
  if(fname %in% list.files(path=paste0(dir, 'output-data/models/'), pattern=".*csv$")){
    re_random <- read.csv(paste0(dir, 'output-data/models/', fname)) 
  } else{ re_random <- c() }
  fname <- paste0('pooled-site-interaction-re-', crp, '-response.csv')
  if(fname %in% list.files(path=paste0(dir, 'output-data/models/'), pattern=".*csv$")){
    re_interaction <- read.csv(paste0(dir, 'output-data/models/', fname)) 
  } else{ re_interaction <- c() }  
  fname <- paste0('pooled-soil-interaction-re-', crp, '-response.csv')
  if(fname %in% list.files(path=paste0(dir, 'output-data/models/'), pattern=".*csv$")){
    re_soil <- read.csv(paste0(dir, 'output-data/models/', fname)) 
  } else{ re_soil <- c() }  
  re <- plyr::rbind.fill(re_random, re_interaction, re_soil)[c(4,1:3)]
  re$crop <- crp
  re$model <- gsub(paste0(crp, '-pooled-'), '', re$model, fixed=T)
  re[c(3:4)] <- round(re[c(3:4)], 2)
  re <- re[c(5,1:4)]
  all_crops_re <- rbind(all_crops_re, re)
  
  }

write.csv(all_crops_aov, paste0(dir, 'output-data/yresp-aov.csv'), row.names=F)
write.csv(all_crops_fe, paste0(dir, 'output-data/yresp-fe.csv'), row.names=F)
write.csv(all_crops_re, paste0(dir, 'output-data/yresp-re.csv'), row.names=F)

# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------
