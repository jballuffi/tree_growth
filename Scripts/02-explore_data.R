
# script to explore trends on tree growth data

# author: Juliana Balluffi-Fry
# date: May 2024

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in cleaned/prepped data that is in RDS format
dat <- readRDS("Output/Data/data_merged.rds")

#for now cut data to years 1986 onward. this is when cone counting started
dat <- dat[year > 1985]

str(dat)




ggplot(dat)+
  geom_point(aes(x = year, y = PAAI, color = mast))

ggplot(dat)+
  geom_point(aes(x = age, y = PAAI))

ggplot(dat)+
  geom_point(aes(x = cmi, y = PAAI))

ggplot(dat)+
  geom_point(aes(x = cones, y = PAAI))

ggplot(dat)+
  geom_boxplot(aes(x = mast, y = PAAI))

ggplot(dat)+
  geom_boxplot(aes(x = summer_mean_temp, y = PAAI))





# AIC model comparisons --------------------------------------------------------------------

#I am including age in all models because it has a significant effect on PAAI
#something we want to control for. question is do we use age as a random effect?
null <- lmer(PAAI ~ (1|age) + (1|tree), dat) 
mast <- lmer(PAAI ~ mast + (1|age) + (1|tree), dat)
cones <- lmer(PAAI ~ cones + (1|age) + (1|tree), dat)
cmi <- lmer(PAAI ~ cmi + (1|age) + (1|tree), dat)
cmimast <- lmer(PAAI ~ cmi + mast + (1|age) + (1|tree), dat)
cmicones <- lmer(PAAI ~ cmi + cones + (1|age) + (1|tree), dat)

#FROM OTHER SCRIPT NOT CHANGED YET
Mods <- list(null, mast, cones, cmi, cmimast, cmicones)
Names <- c('Null', 'Mast', 'Cones', 'CMI', 'CMI_Mast', 'CMI_cones')
AIC <- as.data.table(aictab(REML = F, cand.set = Mods, modnames = Names, sort = TRUE))
AIC[, ModelLik := NULL]
AIC[, Cum.Wt := NULL]
#round whole table to 3 dec places
AIC <- AIC %>% mutate_if(is.numeric, round, digits = 3)

#run collectR2 function and get R2s for all models
R2s <- lapply(Mods, collectR2)
R2s <- rbindlist(R2s, fill = TRUE)
R2s$Modnames <- Names

AIC <- merge(AIC, R2s, by = "Modnames")

