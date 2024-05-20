
# script that collects climate, CMI, cone count, and tree growth data
# here data is combined so one row of data is one 'tree-year'

# author: Juliana Balluffi-Fry
# date: May 2024


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
climate <- fread("Data/climate_data.csv")
cmi <- fread("Data/CMI_annual_sulphur_lake.csv")
cones <- fread("Data/cone_count.csv")
rg <- fread("Data/radial_growth.csv", header = TRUE)



# pivot rg data, make wide to long ----------------------------------------

str(rg)

#fix issue with character strings
rg[, `2022` := as.numeric(`2022`)][, `2021` := as.numeric(`2021`)]

#get the columns after tree. just years
yearcols <- colnames(rg)[2:104]

#pivot table by the year columns
rg2 <- as.data.table(tidyr::pivot_longer(data = rg, 
                          cols = yearcols, 
                          names_to = "year", 
                          values_to = "rg_t",
                          values_drop_na = TRUE))



# convert RG to BAI ----------------------------------------------------------

rg2[, year := as.integer(year)]

setorder(rg2, tree, year)

#get radius of previous year
#rg2[, rg_prev := shift(rg_t, n = 1, type = "lag"), by = tree]

#calculate total radius
#rg2[, total_radius := sum(rg_t), by = tree]


# test <- rg2[tree == "KL-15"]
# 
# years <- test$year
# 
# radius <- lapply(years, function(x){
#   test[year <= x, sum(rg_t)]
# })
# 
# radius <- unlist(radius)
# test$radius <- radius

trees <- rg2[, unique(tree)]

#x is a tree
getradius <- function(x){
  
  dt <- rg2[tree == x]
  
  #make list of years
  years <- dt$yearcol
  
  #sum all of the sumcol column up to and including each year
  r <- lapply(years, function(n){
    dt[year <= n, sum(rg_t)]
  })
  
  r <- unlist(r)
  
  dt[, radius := r]
  
  return(dt[, .(tree, year, radius)])
}

radiusout <- rbindlist(lapply(trees, getradius))


rg3 <- merge(rg2, radiusout, by = c("year", "tree"), all.x = TRUE)






#incorrect
#rg2[, BAI := (pi*(rg_t^2)) - (pi*(rg_prev^2)), by = tree]


# merge environmental variables with tree growth ---------------------------------------------

#first merge all environmental vars
vars <- merge(climate, cones, by = "Year", all = TRUE)
vars <- merge(vars, cmi, by = "Year", all = TRUE)

#change col names. dont want caps in final version
setnames(vars, c("Year", "Cone_count", "Mast", "CMI_annual_sulphur"),
         c("year", "cones", "mast", "cmi"))

#merge with tree growth
dat <- as.data.table(merge(rg2, vars, by = "year", all.x = TRUE))



# calculate tree age ------------------------------------------------------

#reorder by tree then year
setorder(dat, tree, year)

#set year to be a numeric
dat[, year := as.numeric(year)]

#calculate age of tree (approximate)
dat[, age := year - min(year), tree]



# save prepped data -------------------------------------------------------

saveRDS(dat, "Output/Data/data_merged.rds")

