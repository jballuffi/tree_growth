
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
yearcols <- colnames(rg)[2:104] #not reproducible

#pivot table by the year columns
rg2 <- as.data.table(tidyr::pivot_longer(data = rg, 
                          cols = yearcols, 
                          names_to = "year", 
                          values_to = "rg",
                          values_drop_na = TRUE))



# convert RG to PAAI ----------------------------------------------------------

#make year integer
rg2[, year := as.integer(year)]

#set order to be tree then year
setorder(rg2, tree, year)

#grab all tree IDs
trees <- rg2[, unique(tree)]

# function that rums the radius up to that year.
#l apply this function to the list of trees. therefore x = tree
getradius <- function(x){
  #first cut data in rg2 to that tree
  dt <- rg2[tree == x]
  #make list of years for that tree
  years <- dt$year
  #sum all of the rg column up to and including each year
  r <- lapply(years, function(n){
    dt[year <= n, sum(rg)]
  })
  #un list the output of that lapply
  r <- unlist(r)
  #create a column in data called radius
  dt[, radius := r]
  #return just tree, year, and radius
  return(dt[, .(tree, year, radius)])
}

#run function on list of trees using lapply
#rbind list to make one datasheet
radiusout <-rbindlist(lapply(trees, getradius))

#merge radiuses back into original rg2 by year and tree
rg3 <- merge(rg2, radiusout, by = c("year", "tree"), all.x = TRUE)

setorder(rg3, tree, year)

#create column for previous radius
rg3[, prev_radius := shift(radius, n = 1, type = "lag"), tree]

#calculate BAI (pi*R^2 - pi*prevR^2)
rg3[, BAI := (pi*(radius^2)) - (pi*(prev_radius^2))]

#calculate basal area increment total
rg3[, BAIT := sum(BAI, na.rm = TRUE), tree]

#calculate proportion of annual area increase
rg3[, PAAI := BAI/BAIT*100]

#calculate tree age
rg3[, age := year - min(year), tree]

#get previous PAAI
rg3[, prev_PAAI := shift(PAAI, n = 1, type = "lag"), tree]

#get difference in PAAI from year prior
rg3[, PAAI_diff := PAAI - prev_PAAI, tree]



# merge environmental variables with tree growth ---------------------------------------------

#first merge all environmental vars
vars <- merge(climate, cones, by = "Year", all = TRUE)
vars <- merge(vars, cmi, by = "Year", all = TRUE)

#change col names. dont want caps in final version
setnames(vars, c("Year", "Cone_count", "Mast", "CMI_annual_sulphur"),
         c("year", "cones", "mast", "cmi"))

#merge with tree growth
dat <- as.data.table(merge(rg3, vars, by = "year", all.x = TRUE))

#reorder by tree then year
setorder(dat, tree, year)



# save prepped data -------------------------------------------------------

saveRDS(dat, "Output/Data/data_merged.rds")

