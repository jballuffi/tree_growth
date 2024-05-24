
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

