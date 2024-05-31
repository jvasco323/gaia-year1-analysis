
dir <- 'C:/Users/JSILVA/OneDrive - CIMMYT/Desktop/GAIA Shotguns/'

# ------------------------------------------------------------------------------
# start here
# ------------------------------------------------------------------------------

# yield data
agro_data <- read.csv(paste0(dir, "input-data/first-year/gaia_trials.csv"))

# soil data
eth_soil <- read.csv(paste0(dir, "input-data/first-year/Prediction_J669_GAIA_Ethiopia_Updated_5April23.csv")); eth_soil <- eth_soil[, c(8, 1, 10:28)]
tan_soil <- read.csv(paste0(dir, "input-data/first-year/Predicted_GAIA-Tanzania_Updated_05April23.csv")); tan_soil <- tan_soil[, c(5, 1, 14:32)]
rwa_soil <- read.csv(paste0(dir, "input-data/first-year/Predicted_GAIA-Rwanda_Updated_05April23.csv")); rwa_soil <- rwa_soil[, c(6, 1, 11:29)]
soil <- rbind(eth_soil, tan_soil, rwa_soil)

# correspondence
eth_corr <- read.csv(paste0(dir, "input-data/first-year/ETH correspondance.csv"))
tan_corr <- read.csv(paste0(dir, "input-data/first-year/TAN correspondance.csv"))
rwa_corr <- read.csv(paste0(dir, "input-data/first-year/RWA correspondance.csv"))
corr <- rbind(eth_corr, tan_corr, rwa_corr)
corr <- unique(corr)

# ------------------------------------------------------------------------------
# clean soil data
# ------------------------------------------------------------------------------

soil$Country <- ifelse(soil$Country == "Tanzania, United Republic of", "Tanzania", soil$Country)
soil <- merge(soil, corr, by = "SSN")
soil <- soil[ which(soil$depth == "0-20"), ]
names(soil)[2] <- "country"
soil$country <- ifelse(soil$country == "Ethiopia ", "Ethiopia", soil$country)

soil$m3.Ca <- ifelse(soil$m3.Ca == "<0.1", 0.1, soil$m3.Ca)
soil$m3.Fe <- ifelse(soil$m3.Fe == "<30", 30, soil$m3.Fe)
soil$m3.K <- ifelse(soil$m3.K == "<2", 2, soil$m3.K)
soil$m3.Mg <- ifelse(soil$m3.Mg == "<3", 3, soil$m3.Mg)
soil$m3.Mn <- ifelse(soil$m3.Mn == "<0.15", 0.15, soil$m3.Mn)
soil$CEC <- ifelse(soil$CEC == "<0.6", 0.6, soil$CEC)
cols.num <- c("m3.Ca", "m3.Fe", "m3.K", "m3.Mg", "m3.Mn", "CEC")
soil[cols.num] <- sapply(soil[cols.num], as.numeric)

# ------------------------------------------------------------------------------
# merge yield and soil data
# ------------------------------------------------------------------------------

data <- merge(agro_data, soil, by = c("country", "FID"))
data$treatment <- gsub('R', '', data$treatment)
data$lime_tha <- ifelse(data$lime_tha==25, 2.5, data$lime_tha)
data$district_gadm <- ifelse(data$district=='Omonada', 'Jimma', data$district_gadm)
data$district_gadm <- ifelse(data$district=='Nyaruguru', 'Nyaruguru', data$district_gadm)
data$Ca <- data$m3.Ca / 200
data$Mg <- data$m3.Mg / 122
data$Na <- data$m3.Na / 230
data$K  <- data$m3.K  / 390
data$ecec <- data$ExAc + data$Ca + data$Mg + data$Na + data$K
data$hp_sat <- 100 * data$ExAc / data$ecec
data$tex <- 100 - data$Sand
write.csv(data, paste0(dir, "input-data/first-year/gaia-trials-year1.csv"))

### to check what the below does
data <- merge(agro_data, soil, by = c("country", "FID"), all.x=T)
debug <- subset(data, is.na(data$pH))
df <- unique(debug[c(1,2)])

# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------
