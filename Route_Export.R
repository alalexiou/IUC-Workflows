###########################################################
# Export Routes between O-D to local file / with parallel::
# @ Origin: Greater Manchester
###########################################################

# Libraries
library(data.table)
library(ggplot2)

setwd("/home/alekos/Documents/BookOfMormon/")

# OA - WZ data for EW
od <- fread("data/origin_destination_2_bulk_all_tables/wf01aew_oa_v1.csv") 
od
colnames(od) <- c("Origin", "Destination", "Persons")

# Greater Manchester area codes
lookup <- fread("data/OA11_LSOA11_MSOA11_LAD11_EW_LUv2.csv")
lookup
manchester_lads <- c("Bolton", "Manchester", "Rochdale", "Stockport", "Trafford", "Tameside", "Wigan", "Bury", "Oldham", "Salford")

od <- merge(od, lookup[, c(1, 7)], by.x = "Origin", by.y = "OA11CD", all.x = T)
head(od)

od_sub <- od
od_sub <- od_sub[ LAD11NM %like% paste0(manchester_lads, collapse = "|") ]
# od_sub <- od_sub[ LAD11NM %like% "Manchester" ]

# attach lat lon
wz_centroids <- fread("data/Workplace_Zones_December_2011_Population_Weighted_Centroids.csv")
oa_centroids <- fread("data/Output_Areas_December_2011_Population_Weighted_Centroids.csv")
head(wz_centroids)
head(oa_centroids)
head(od_sub)

od_manch <- merge(od_sub, oa_centroids[, -3], by.x = "Origin", by.y = "oa11cd", all.x = T)
colnames(od_manch)[5:6] <- c("Or_OA_x", "Or_OA_y")
od_manch <- merge(od_manch, wz_centroids[, -3], by.x = "Destination", by.y = "wz11cd", all.x = T)
colnames(od_manch)[7:8] <- c("De_Wz_x", "De_Wz_y")
od_manch
od_manch <- od_manch[!is.na(od_manch$De_Wz_x), ]

# if ok then
remove(od)
remove(od_sub)

# Routino
setwd("/home/alekos/Documents/BookOfMormon/data/routino/")
fileloc <- "/home/alekos/Documents/BookOfMormon/data/routino/quickest.txt"  #This is the default working directory

library(doMC) # Linux and Mac OS X 
library(foreach)
registerDoMC(26) 

# nrow(od_manch)
a <- Sys.time()

foreach(i=1:nrow(od_manch)) %dopar% {
  tryCatch({
  router <- paste0("router --transport=motorcar --prefix=gb2 --quickest --oneway=0 --turns=0 --quiet --exact-nodes-only --lon1=", 
                   od_manch$Or_OA_x[i], " --lat1=", od_manch$Or_OA_y[i], " --lon2=", od_manch$De_Wz_x[i], " --lat2=", od_manch$De_Wz_y[i], 
                  " --output-text --dir=/home/alekos/Routino-3.2/routino-3.2",
                  " --translations=/home/alekos/Routino-3.2/routino-3.2/xml/routino-translations.xml --profiles=/home/alekos/Routino-3.2/routino-3.2/xml/routino-profiles.xml")
  
  system(router, wait = T)  # Send the routing command
  # Rename file
  file.rename(fileloc, paste0(od_manch$Origin[i], "-", od_manch$Destination[i], "-", formatC(od_manch$Persons[i], width=3, flag="0"), ".txt"))
  
  }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
  
}

# keep track of fails
route_fail <- .Last.value

b <- Sys.time()
b-a

# stopCluster(cl)
