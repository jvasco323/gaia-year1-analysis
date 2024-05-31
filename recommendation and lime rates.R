
dir <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/2-yield-response/'

# ------------------------------------------------------------------------------
# start here
# ------------------------------------------------------------------------------

# load data
df <- read.csv(paste0(dir, "input-data/first-year/gaia-trials-year1.csv"))
df <- subset(df, trial_type == 'Shotgun')


df <- df[c('district_gadm', 'pH', 'ecec', 'hp_sat', 'Ca', 'Mg', 'tex', 'PSI')]
df <- unique(df)


plot(df$pH, df$ecec)
plot(df$pH, df$hp_sat); abline(h=10)
plot(df$pH, df$Ca/df$Mg, ylim=c(0,12)); abline(h=6)
plot(df$ecec, df$hp_sat); abline(h=10)
plot(df$ecec, df$Ca/df$Mg, ylim=c(0,12)); abline(h=6)
plot(df$hp_sat, df$Ca/df$Mg, ylim=c(0,12)); abline(h=6)
plot(df$pH, df$PSI)


# classification with pH
df$groups <- NA
df$groups <- ifelse(df$pH > 5.5, 'noprob_ph', df$groups)
df$groups <- ifelse(df$pH <= 5.5 & df$ecec > mean(df$ecec), 'noprob_ecec', df$groups)
df$groups <- ifelse(df$pH <= 5.5 & df$ecec <= mean(df$ecec) & df$hp_sat <= 10 & df$Ca/df$Mg > 6, 'lowrate_dolomite', df$groups)
df$groups <- ifelse(df$pH <= 5.5 & df$ecec <= mean(df$ecec) & df$hp_sat <= 10 & df$Ca/df$Mg <= 6, 'lowrate_calcite', df$groups)
df$groups <- ifelse(df$pH <= 5.5 & df$ecec <= mean(df$ecec) & df$hp_sat > 10 & df$Ca/df$Mg > 6, 'highrate_dolomite', df$groups)
df$groups <- ifelse(df$pH <= 5.5 & df$ecec <= mean(df$ecec) & df$hp_sat > 10 & df$Ca/df$Mg <= 6, 'highrate_calcite', df$groups)

# classification without pH
df$groups <- NA
df$groups <- ifelse(df$ecec > mean(df$ecec), 'noprob_ecec', df$groups)
df$groups <- ifelse(df$ecec <= mean(df$ecec) & df$hp_sat <= 10 & df$Ca/df$Mg > 6, 'lowrate_dolomite', df$groups)
df$groups <- ifelse(df$ecec <= mean(df$ecec) & df$hp_sat <= 10 & df$Ca/df$Mg <= 6, 'lowrate_calcite', df$groups)
df$groups <- ifelse(df$ecec <= mean(df$ecec) & df$hp_sat > 10 & df$Ca/df$Mg > 6, 'highrate_dolomite', df$groups)
df$groups <- ifelse(df$ecec <= mean(df$ecec) & df$hp_sat > 10 & df$Ca/df$Mg <= 6, 'highrate_calcite', df$groups)


boxplot(df$pH ~ df$groups)
boxplot(df$ecec ~ df$groups)
boxplot(df$hp_sat ~ df$groups)
boxplot(df$Ca/df$Mg ~ df$groups)
boxplot(df$tex ~ df$groups)
boxplot(df$PSI ~ df$groups)
table(df$groups, df$district_gadm)

# estimate lime rates for each sample with Merlos et al. to TAS=0
# make boxplot of lime rate vs. group


