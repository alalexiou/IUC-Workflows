#########################################################################
### READ AND AGGREGATE ROUTES: Point (Node) and Line (Street) Aggregation
#########################################################################

# Libraries
library(rgdal)
library(rgeos)
library(sp)

############################################

# Read list of routino txt files
route_files <- dir(path = "/routino", pattern = "txt", full.names = TRUE, ignore.case = TRUE)

# Test
length(route_files)
route_files[1]

# NOTE:
# Run in batches otherwise df becomes too large -> too slow


# Make DF that will store all route points
df <- data.table(Longitude = 0, Latitude = 0, Origin = 0, Destination = 0, Persons = 0)[-1, ]
  
for(i in 1:length(route_files)) {

  # Get lat long
  temp_file <- read.delim(route_files[i], header = F, sep = "\t", skip = 6, colClasses = "character")[, 1:2]
  temp_file <- as.data.table(sapply(temp_file, as.numeric))
  temp_file <- na.omit(temp_file)
  colnames(temp_file)[1:2] <- c("Longitude", "Latitude")
  
  df <- rbind(df, temp_file)
}

# Safekeeping
write.csv(df, "All_GManch_Points_RAW.csv")

####################################
# Fixing Raw Data
####################################

# Append LSOA codes to OAs
df <- merge(df, unique(nspl[, 3:4]), by.x = "Origin", by.y = "oa11", all.x = T)

# Append sales / penetration data
df <- merge(df, sales_iuc[, c(-2, -4)], by.x = "lsoa11", by.y = "LSOA11_CD", all.x = T)

# Test
table(is.na(df$LSOA11_NM))

# Aggregate segments
df_pen <- as.data.table(aggregate(Persons ~ Latitude + Longitude, data = df, FUN = "sum"))
df_pen

# Safekeeping
write.csv(df_pen, "Aggregated_Point_Flow_GManchester_Flow_Rate.csv")

# Remove original point data
remove(df)

########################################
# Append Data to Routes
########################################

library(dplyr)

### OUTPUT POINTS ###

points <- SpatialPointsDataFrame(coords = df_pen[, 1:2], data = df_pen, proj4string =  CRS("+init=epsg:4326"))
points@data$Latitude <- NULL
points@data$Longitude <- NULL

# Write output shp
writeOGR(points, "/outputs", "IUC_Flows_GManchester_Points", driver = "ESRI Shapefile", overwrite_layer = T)

### OUTPUT LINES ###

roads <- readOGR("outputs", "osm_roads_gmanch")
roads@data <- roads@data[, c(1,3,4)]
proj4string(roads) <- CRS("+init=epsg:4326")

# add IDs for matching
roads@data <- mutate(roads@data, id_roads = as.numeric(rownames(roads@data)))
# points@data <- mutate(points@data, id_points = as.numeric(rownames(points@data))) # not needed

# Use sp::over
# lineflow <- over(roads, points) # not work
# make lines into polys..
rbuffer <- gBuffer(roads, byid = T, capStyle="ROUND", width = 1/5555.55) # "SQUARE" for speed? "ROUND" is best technically | 111,111m per degree so 20m
proj4string(rbuffer) <- CRS("+init=epsg:4326")

# find the joins and sum
a <- Sys.time()
over_lines <- over(rbuffer, points, fn = "sum")
Sys.time() - a
over_lines <- mutate(over_lines, id_roads = as.numeric(rownames(over_lines)))
head(over_lines)

# Should be equal
nrow(over_lines) == nrow(roads@data)

# join to original line data 
roads@data <- merge(roads@data, over_lines, by = "id_roads", all.x = T)

# NAs to 0s
roads@data[is.na(roads@data$Flow_Rate), "Flow_Rate"] <- 0

head(roads@data)
hist(roads@data$Flow_Rate, breaks = 1000, xlim = c(0, 300), ylim = c(0,2000))
# plot(roads)
# plot(rbuffer, add = T)

# Write output shp
writeOGR(roads, "/outputs", "IUC_Flows_GManchester", driver = "ESRI Shapefile", overwrite_layer = T)
