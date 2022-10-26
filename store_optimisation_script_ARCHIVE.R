#install.packages("leaflet")
#install.packages("tidyverse")
#install.packages("leaflet")
#install.packages("httr")
#install.packages("writexl")
#install.packages("readxl")
#install.packages("leaflet")
#install.packages("geosphere")
#install.packages("sf")
#install.packages("cluster")
#install.packages("factoextra")
#install.packages("dendextend")
#install.packages("dbscan")
#install.packages("curl")

library(httr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(htmltools)
library(geosphere)
library(sf)
library(cluster)
library(factoextra)
library(dendextend)
library(dbscan)
library(curl)

##########
# FUNCTIONS

# add leaflet title
overlayTitle <- function(plot, text) {
  
  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: regular;
    font-size: 18px;
  }
  "))
  
  title <- tags$div(
    tag.map.title, HTML(text)
  )
  plot %>% addControl(title, position = "topleft", className = "map-title")
  
}


# compute distance from two coords
calcHaversine <- function(loc1, loc2) {
  
  df <- matrix(c(loc1[1], loc2[1], loc1[2], loc2[2]), nrow = 2)
  dist <- distHaversine(df)
  return(dist)
}


# compute the number of locations within 500m
calcCloseStore <- function(master_stores, target_stores) {
  
  # convert both tables to sf standard
  master_stores_sf <- st_as_sf(master_stores, coords=c('lng', 'lat'), crs="epsg:4326")
  target_stores_sf <- st_as_sf(target_stores, coords=c('lng', 'lat'), crs="epsg:4326")
  
  # compute distance matrix
  # each row represents a target location and columns their distance to a master store (in meters)
  dist = st_distance(target_stores_sf, master_stores_sf)
  
  # convert to dataframe and compute the row min (we only care if it is within 500m not how often)
  dist <- data.frame(dist)
  dist$min <- apply(dist[], MARGIN =  1, FUN = min)
  
  # compute number of stores within 500m of a master brand store
  dist_close <- dist %>% 
    filter(min <=500)
  
  close_stores <- nrow(dist_close)
  
  return(close_stores)
}

##########

all_stores <- read_xls('data/GEOLYTIX - UK RetailPoints/uk_glx_open_retail_points_v24_202206.xls')

# plot store count by retailer
retailer_count <- all_stores %>%
  group_by(retailer) %>%
  tally()

ggplot(retailer_count, aes(x = reorder(retailer, -n), y = n)) +
  geom_bar(stat = "identity") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))

# compute the percentage
######WORK OUT PERCENTAGE AND THEN INSERT INTO THE EXPLANATION NEXT

# plot location of major chains
majors <- c("Aldi", "Asda", "Lidl", "Marks and Spencer", "Morrisons", 
            "Sainsburys", "Tesco", "The Co-operative Group", "Waitrose")

major_retailers <- all_stores %>% filter(`retailer` %in% majors) %>%
  select(id, retailer, postcode, lng = long_wgs, lat = lat_wgs, size_band)

pal <- colorFactor(
  palette = c(
    "cyan", "green", "purple", "black", "yellow", "orange", "blue", "turquoise", "red" 
    ),
  domain = major_retailers$retailer)

p <- leaflet(major_retailers) %>% addTiles() %>%
  addCircles(~lng, ~lat,color = ~pal(retailer)) %>%
  overlayTitle("Major Retailer Store Locations") %>%
  addLegend(position = "bottomright", values = ~retailer, pal = pal); p


#------ PURCHASE COMPETITOR LOCATIONS

# assuming we are Tesco and are looking to acquire one of the other supermarket chains
# the condition under which we will acquire is based on which chain offers the best coverage
# but where there is minimum overlap with existing Tesco locations
# i.e. the competitor where least % of stores within a certain radius of Tesco stores

# we will set a cut-off of 500m each other - based on Haversine distance
major_retailers <- major_retailers %>% 
  rowwise %>%
  mutate(coordinates = list(c(lng, lat))) %>%
  ungroup

# isolate the tesco locations
tesco_loc <- major_retailers %>%
  filter(retailer == "Tesco")

# obtain list of all other target brands
targets <- majors[majors != 'Tesco']

# iterate and compute % of each brand within 500m
for (brand in targets){
  
  target_loc <- major_retailers %>%
    filter(retailer == brand)
  
  close_stores <- calcCloseStore(tesco_loc, target_loc)
  
  print(brand)
  print((close_stores/nrow(target_loc))*100)
}

# "Aldi"
# 26.34298
# "Asda"
# 14.28571
# "Lidl"
# 29.41788
# "Marks and Spencer"
# 33.98876
# "Morrisons"
# 12.31964
# "Sainsburys"
# 33.30966
# "The Co-operative Group"
# 15.45048
# "Waitrose"
# 35.18006

# from the above results we see that Morrisons or Asda have the lowest %

print(retailer_count %>% filter(retailer %in% c('Asda', 'Morrisons')))
# Asda        637
# Morrisons   901

# given Morrisons has the lowest % and more stores, we will elect for acquiring it

# create a combined dataset
combined_stores <- major_retailers %>%
  filter(retailer %in% c('Tesco', 'Morrisons'))

# now we have pretty good coverage across the UK
p <- leaflet(combined_stores) %>% addTiles() %>%
  addCircles(~lng, ~lat,color = 'blue', radius = 500, opacity = .5) %>%
  overlayTitle("Tesco Store Locations"); p


write_rds(combined_stores, "data/combined_store.rds")

#------ DECIDING WHICH STORES TO CLOSE

# now we have a group of store locations, next we can optimise the portfolio
# we will do this by ensuring that there are no stores within 5minutes drive
# and where there is we will prioritise stores based on size and demographic data

combined_stores <- read_rds("data/combined_store.rds")

# first find the row numbers for the morrisons stores 
tesco_stores <- combined_stores %>%
  filter(retailer == 'Tesco')

morrisons_stores <- combined_stores %>%
  filter(retailer == 'Morrisons')


# convert both tables to sf standard
tesco_stores_sf <- st_as_sf(tesco_stores, coords=c('lng', 'lat'), crs="epsg:4326")
morrisons_stores_sf <- st_as_sf(morrisons_stores, coords=c('lng', 'lat'), crs="epsg:4326")

# compute distance matrix
# each row represents a target location and columns their distance to a master store (in meters)
dist_sf = st_distance(tesco_stores_sf, morrisons_stores_sf)

M <- as.matrix(dist_sf)
M <- unclass(M)

#create binary matrix to show where Tesco store (row) within 500m of Morrisons (column) 
M[] <- ifelse(M<500,1,0)

# convert to dataframe
dist_sf <- data.frame(M)

dist_sf <- rownames_to_column(dist_sf) %>%
  rename(tesco_index = rowname)

close_store_df <- dist_sf %>% 
  gather(morrisons_index, flag, -c(tesco_index)) %>%
  filter(flag == 1)

# remove the "X" from the morrisons store index
close_store_df <- close_store_df %>%
  rowwise() %>%
  mutate(morrisons_index = str_remove(morrisons_index, "X")) %>%
  ungroup

# check if we have any duplicates (i.e. one store close to two others)
nrow(close_store_df)
# 121
n_distinct(close_store_df$tesco_index)
# 117
n_distinct(close_store_df$morrisons_index)
# 111

# join back to the master tesco to indicate morrisons index locations
tesco_stores_df <- rownames_to_column(tesco_stores) %>%
  rename(tesco_index = rowname) %>%
  inner_join(close_store_df, by='tesco_index')

# join on the relevant morrisons store data
morrisons_stores_df <- rownames_to_column(morrisons_stores) %>%
  rename(morrisons_index = rowname)
  
store_pairs <- tesco_stores_df %>%
  left_join(morrisons_stores_df, by='morrisons_index')


# map postcodes to output Layer Super Output Area (LSAO)
# https://geoportal.statistics.gov.uk/datasets/06938ffe68de49de98709b0c2ea7c21a/about
postcode_lsao_data <- read_csv("data/Postcode Data/PCD_OA_LSOA_MSOA_LAD_AUG19_UK_LU.csv")

# map income to LSAO
# https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/datasets/regionalgrossdisposablehouseholdincomegdhibylocalauthorityintheuk
income_data <- read_excel(
  "data/Postcode Data/vcregionalgdhibylareordered.xlsx", 
  sheet = "Gross disposable income", 
  skip = 2)




#------ DECIDING MOST APPROPRIATE WAREHOUSE LOCATIONS

# after optimising the locations, we will decide where best to set up outlet depots
# we will achieve this by determining the best geographic clusters - using KMeans
# the aim is to form clusters that capture the maximum number of stores, without overlap
# the distribution warehouses/depots would then be located at the centroids of the clusters


combined_stores <- read_rds("data/combined_store.rds")

locs_df <- combined_stores %>%
  select("id","lng", "lat") %>%
  column_to_rownames("id")

set.seed(123)


#-- KMEANS CLUSTERING

# Compute k-means with k = 20

km.res <- kmeans(locs_df, 20)

print(km.res)

locs_df_km<- locs_df %>% 
  mutate(clust = km.res$cluster)

# print
length(unique(locs_df_km$clust))
# 20

# plot outputs
pal <- colorFactor(
  palette = "RdYlBu",
  domain = locs_df_km$clust)

p <- leaflet(locs_df_km) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(~lng, ~lat,color = ~pal(clust)) %>%
  overlayTitle("Store Clusters") %>%
  addLegend(position = "bottomright", values = ~clust, pal = pal); p


# plot store count by cluster
cluster_count_km <- locs_df_km %>%
  group_by(clust) %>%
  tally()

ggplot(cluster_count_km, aes(x = reorder(clust, -n), y = n)) +
  geom_bar(stat = "identity") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.3))



#-- DBSCAN CLUSTERING


clusters <- dbscan(locs_df, eps = 0.25, minPts = 100)[['cluster']]

length(unique(clusters))
# 20

locs_df_db <- locs_df %>%
  mutate(clust = clusters)

# plot outputs
pal <- colorFactor(
  palette = "RdYlBu",
  domain = locs_df_db$clust)

p <- leaflet(locs_df_db) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(~lng, ~lat,color = ~pal(clust)) %>%
  overlayTitle("Store Clusters") %>%
  addLegend(position = "bottomright", values = ~clust, pal = pal); p



#-- HIERARCHICAL CLUSTERING

# compute distance matrix - between location pairs
locs_sf <- st_as_sf(locs_df, coords=c('lng', 'lat'), crs="epsg:4326")
dist_sf = st_distance(locs_sf, locs_sf)
mdist <- as.matrix(dist_sf)
mdist <- unclass(mdist)

# plot dendogram
hc <- hclust(as.dist(mdist), method="complete")
plot(hc, cex = 0.6, hang = -1)

# cluster based on defined distance separation
d <- 215000
locs_df_hc <- locs_df %>%
  mutate(clust = cutree(hc, h=d))

# print
length(unique(locs_df_hc$clust))
# 20

# plot outputs
pal <- colorFactor(
  palette = "RdYlBu",
  domain = locs_df_hc$clust)

p <- leaflet(locs_df_hc) %>% addProviderTiles(providers$CartoDB.Positron)  %>%
  addCircles(~lng, ~lat,color = ~pal(clust)) %>%
  overlayTitle("Store Clusters") %>%
  addLegend(position = "bottomright", values = ~clust, pal = pal); p


# plot store count by cluster
cluster_count_hc <- locs_df_hc %>%
  group_by(clust) %>%
  tally()

ggplot(cluster_count_hc, aes(x = reorder(clust, -n), y = n)) +
  geom_bar(stat = "identity") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.3))

# plot store count by cluster as %
cluster_count_hc <- cluster_count_hc %>%
  mutate(percentage = (n / nrow(locs_df_hc))*100)

ggplot(cluster_count_hc, aes(x = reorder(clust, -percentage), y = percentage)) +
  geom_bar(stat = "identity") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.3))


# we found that the clusters with few store locations tended to be on isoltated locations
# plot outputs - exploratory only
sample_cluster <- locs_df_hc %>%
  filter(clust == 8)

p <- leaflet(sample_cluster) %>% addProviderTiles(providers$CartoDB.Positron)  %>%
  addCircles(~lng, ~lat,color = ~pal(clust)) %>%
  overlayTitle("Store Clusters") %>%
  addLegend(position = "bottomright", values = ~clust, pal = pal); p



# e.g. Shetland Islands, Hebrides, Orkney Islands, or Guernsey & Jersey
# all of these have <1% of the total store locations

# we will remove such remote locations from our analysis and re-cluster
remote_clusters <- cluster_count_hc %>%
  filter(percentage < 1) %>%
  select(clust)

locs_df_hc_cut <- locs_df_hc %>%
  filter(!clust %in% remote_clusters$clust)

# compute distance matrix - between location pairs
locs_sf <- st_as_sf(locs_df_hc_cut %>% select(lng, lat), coords=c('lng', 'lat'), crs="epsg:4326")
dist_sf = st_distance(locs_sf, locs_sf)
mdist <- as.matrix(dist_sf)
mdist <- unclass(mdist)

# plot dendogram
hc <- hclust(as.dist(mdist), method="complete")
plot(hc, cex = 0.6, hang = -1)

# cluster based on defined distance separation
d <- 173000
locs_df_hc_cut <- locs_df_hc_cut %>%
  mutate(clust2 = cutree(hc, h=d))

# print
length(unique(locs_df_hc_cut$clust2))
# 19


# plot outputs
pal <- colorFactor(
  palette = "RdYlBu",
  domain = locs_df_hc_cut$clust2)

p <- leaflet(locs_df_hc_cut) %>% addProviderTiles(providers$CartoDB.Positron)  %>%
  addCircles(~lng, ~lat,color = ~pal(clust2)) %>%
  overlayTitle("Store Clusters") %>%
  addLegend(position = "bottomright", values = ~clust2, pal = pal); p



# compute the centre of each cluster and plot this
cluster_centroids <- locs_df_hc_cut %>%
  group_by(clust2) %>%
  summarize(mean_lng = mean(lng, na.rm=TRUE), mean_lat = mean(lat, na.rm=TRUE))


p <- leaflet(locs_df_hc_cut) %>% addProviderTiles(providers$CartoDB.Positron)  %>%
  addCircles(~lng, ~lat,color = ~pal(clust2)) %>%
  overlayTitle("Store Clusters") %>%
  addLegend(position = "bottomright", values = ~clust2, pal = pal) %>%
  addMarkers(data = cluster_centroids, ~mean_lng, ~mean_lat); p


# however, we will see that in some cases the hierarchical clustering does not result in sensible assignments
# for example, where bodies of water exist, that simply clustering by coordinates cannot take account of
cluster_number <- 8

sample_cluster <- locs_df_hc_cut %>%
  filter(clust2 == cluster_number)

sample_centroid <- cluster_centroids %>%
  filter(clust2 == cluster_number)

p <- leaflet(sample_cluster) %>% addProviderTiles(providers$CartoDB.Positron)  %>%
  addCircles(~lng, ~lat,color = ~pal(clust)) %>%
  overlayTitle("Store Clusters") %>%
  addLegend(position = "bottomright", values = ~clust2, pal = pal) %>%
  addMarkers(data = sample_centroid, ~mean_lng, ~mean_lat); p

# we can see that a location is on the other side of the estuary
# for this we will need to understand the drive time and reassign locations to their nearest cluster centre

write_rds(locs_df_hc_cut, "data/cut_store_locs.rds")
write_rds(cluster_centroids, "data/warehouse_locations.rds")


####################################################################################################################
####################################################################################################################
####################################################################################################################

# NOW WE NEED TO INSTALL DOCKER AND VROOM DOCKER IMAGES

# this can be a little painful, so i suggest doing the following steps in order:

# first to ensure compatibility, install Linux from the following link: https://learn.microsoft.com/en-gb/windows/wsl/install-manual#step-4---download-the-linux-kernel-update-package
# download the Linux update from step 4 and activate WSL 2 in step 5
# next download Ubuntu (22.04.1 LTS): https://apps.microsoft.com/store/detail/ubuntu-22041-lts/9PN20MSR04DW?hl=en-gb&gl=gb
# restart your PC
# next, install Docker from here: https://docs.docker.com/desktop/install/windows-install/
# upon opening, ensure your "Docker engine" starts correctly
# if so go to settings > resources > WSL Integration > select the Ubuntu distribution downloaded

# next download OSRM
# instructions taken from here: https://github.com/Project-OSRM/osrm-backend
# create a directory into which you'll install your repositories you wish to unpack for Docker
# in Powershell Prompt:
# $ mkdir Documents\Docker_Repos\OSRM
# $ cd  Documents\Docker_Repos\OSRM
# $ wget http://download.geofabrik.de/europe/britain-and-ireland-latest.osm.pbf
# or download from here and move to the folder (if you have memory issues like i do)
# http://download.geofabrik.de/europe.html

# check the instructions in "Using Docker" section of here: https://github.com/Project-OSRM/osrm-backend
# start the docker container and create directory "/data" inside the docker container 
# and makes the current working directory "%cd%" available there
# $ docker run -t -v "%cd%:/data" osrm/osrm-backend osrm-extract -p /opt/car.lua /data/britain-and-ireland-latest.osm.pbf
# ^ he above can take some time to run for the first time

# you will now see a running container on your Docker Desktop (with IMAGE showing as osrm-backend)
# next execute:
# $ docker run -t -v "%cd%:/data" osrm/osrm-backend osrm-partition /data/britain-and-ireland-latest.osrm
# lastly:
# $ docker run -t -v "%cd%:/data" osrm/osrm-backend osrm-customize /data/britain-and-ireland-latest.osrm

# we can now delete all running containers in Docker

# now before we run the docker container, we want to set up a private network in which we'll run it
# this avoids the risk of different dockers interacting with apps in an unintended manner

# $ docker network create OSRM_routing
# $ docker network ls

# now we can launch the container (ensuring you are in the correct directory)
# $ cd Documents/Docker_Repos/OSRM
# $ docker run --net OSRM_routing --name OSRM -t -i -p 5000:5000 -v "%cd%:/data" osrm/osrm-backend osrm-routed --algorithm mld /data/britain-and-ireland-latest.osrm 

# test it in another command prompt window using:
# $ curl "http://127.0.0.1:5000/route/v1/driving/-1.923127,52.517037;0.563423,52.496891?steps=true"

# API usage details: https://project-osrm.org/docs/v5.22.0/api/#route-service

# to get the nearest distance between location pairs, we can use the table method:
# $ curl "http://127.0.0.1:5000/table/v1/driving/-1.923127,52.517037;0.563423,52.496891;-1.520016,52.421238"
# in the command prompt we can see the responses returned from the app as a string
# the response returned is a 3x3 matrix showing the duration between each pair
# we are now ready to continue in R...


# execute the same command and take a response in R
response <- GET("http://127.0.0.1:5000/table/v1/driving/-1.923127,52.517037;0.563423,52.496891;-1.520016,52.421238")
result <- content(response, as='parsed')

duration_matrix <- as.matrix(do.call("cbind", result$durations))
print(duration_matrix)
#      [,1]   [,2]   [,3]  
# [1,] 0      9343.4 1976.6
# [2,] 9291.9 0      8172  
# [3,] 1963.2 8201.2 0  


####################################################################################################################
####################################################################################################################
####################################################################################################################

# to determine the distance from each location to the cluster centres
store_locations <- read_rds("data/cut_store_locs.rds")
warehouse_locations <- read_rds("data/warehouse_locations.rds") %>%
  rename(lat=mean_lat, lng=mean_lng, cluster = clust2)


# create the string of warehouse locations
coords <- c()
for (row in 1:nrow(warehouse_locations)){
  warehouse <- paste(warehouse_locations$lng[row], warehouse_locations$lat[row], sep=",")
  coords <- append(coords, warehouse)
  
}
warehouse_string <- paste(coords, collapse=";")


# obtain store coords
test_location <- store_locations %>%
  select(lng, lat) %>%
  slice(4) %>%
  paste(collapse=",")

# create full request
response <- GET(paste0("http://127.0.0.1:5000/table/v1/driving/", test_location, ";", warehouse_string, "?sources=0"))
result <- content(response, as='parsed')

duration_matrix <- result$durations[[1]][-1] 
# we drop the first element as this is duration from store to itself (i.e. 0)

# obtain nearest warehouse
nearest_warehouse <- warehouse_locations %>%
  mutate(store_travel_duration = duration_matrix) %>%
  slice(which.max(.$store_travel_duration)) %>%
  pull(cluster)

print(nearest_warehouse)


###### NOW TO ITERATE
cluster_vector <- c()
for (row in 1:nrow(store_locations)){
  location <- store_locations %>%
    select(lng, lat) %>%
    slice(row) %>%
    paste(collapse=",")
  
  response <- GET(paste0("http://127.0.0.1:5000/table/v1/driving/", location, ";", warehouse_string, "?sources=0"))
  result <- content(response, as='parsed')
  
  duration_matrix <- result$durations[[1]][-1] 
  
  nearest_warehouse <- warehouse_locations %>%
    mutate(store_travel_duration = as.numeric(duration_matrix)) %>%
    slice(which.min(.$store_travel_duration)) %>%
    pull(cluster)
  
  cluster_vector <- append(cluster_vector, nearest_warehouse)
  
  if (row%%10 == 0){
    print(paste0(row,"/",nrow(store_locations)))
  }
  
}

new_store_locations <- store_locations %>%
  mutate(updated_cluster = cluster_vector) %>%
  rename(original_cluster = clust2) %>%
  select(-clust) %>%
  mutate(match = case_when(
    original_cluster == updated_cluster ~ 1,
    original_cluster != updated_cluster ~ 0
    ))


# plot the new clusters
pal <- colorFactor(
  palette = "RdYlBu",
  domain = new_store_locations$updated_cluster)

p <- leaflet(new_store_locations) %>% addProviderTiles(providers$CartoDB.Positron)  %>%
  addCircles(~lng, ~lat,color = ~pal(updated_cluster)) %>%
  overlayTitle("Store Clusters") %>%
  addLegend(position = "bottomright", values = ~updated_cluster, pal = pal) %>%
  addMarkers(data = warehouse_locations, ~lng, ~lat, label = ~as.character(cluster)); p








# now we will decide the appropriate mix of main warehouses, and satellite depots

cluster_count <- new_store_locations %>%
  group_by(updated_cluster) %>%
  tally()

ggplot(cluster_count, aes(x = reorder(updated_cluster, -n), y = n)) +
  geom_bar(stat = "identity") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.3))

# we want the main warehouses to be fairly well spread, and 
top_7 <- c(12, 3, 11, 16, 1, 6, 5)

top_7_clusters <- new_store_locations %>%
  filter(updated_cluster %in% top_7)

pal <- colorFactor(
  palette = "RdYlBu",
  domain = top_7_clusters$updated_cluster)

p <- leaflet(top_7_clusters) %>% addProviderTiles(providers$CartoDB.Positron)  %>%
  addCircles(~lng, ~lat,color = ~pal(updated_cluster)) %>%
  overlayTitle("Store Clusters") %>%
  addLegend(position = "bottomright", values = ~updated_cluster, pal = pal) %>%
  addMarkers(data = warehouse_locations, ~lng, ~lat, label = ~as.character(cluster)); p

# unsurprisingly london with such a high population justifies two depots (3 & 12)
# Manchester in the north (cluster 11) is also reasonable
# For cluster 1 vs 16, a decision can be made to service Wales and the Midland from Birmingham
# ---- with the East of Engalnd being serviced out of Manchestest as appropriate

# it is also reasonable to assume a northern Depot in Scotland to service the country and North of England
# Glasgow (cluster 14) has the highest number of stores and is well located to service Northern Ireland also

# plot the final map
master_depots <- c(1, 3, 11, 12, 14)

warehouse_locations <- warehouse_locations %>%
  mutate(tier = ifelse(cluster %in% master_depots, 'master', 'sub')) %>%
  mutate(icon_colour = ifelse(tier == "master", "red", "blue"))

icons <- awesomeIcons(iconColor = "black",
                      library = "ion",
                      markerColor = warehouse_locations$icon_colour
                      )

pal <- colorFactor(
  palette = "RdYlBu",
  domain = new_store_locations$updated_cluster)

p <- leaflet(new_store_locations) %>% addProviderTiles(providers$CartoDB.Positron)  %>%
  addCircles(~lng, ~lat,color = ~pal(updated_cluster)) %>%
  overlayTitle("Store Clusters") %>%
  addLegend(position = "bottomright", values = ~updated_cluster, pal = pal) %>%
  addAwesomeMarkers(data = warehouse_locations, ~lng, ~lat, icon = icons, label = ~as.character(cluster)); p



# There we have it, our clustered store territories, with the proposed location of each warehouse
# and a proposed view of which should me major depots, and which should be sub-depots

# this could be viewed as a vehicle routing problem, i.e what is the most efficient method of one truck, 
# leaving each of the the main depots to service the sub-depots. For this we would use VROOM

# However, given the size of deliveries, it is realistic to assume a single van per sub-depot restock
# in which case it simply becomes a shortest duration problem - which OSRM docker can handle

























install.packages("rgdal")
library(rgdal)

x <- c(-1.482156, -1.482318, -1.482129, -1.482880, -1.485735, -1.485770, -1.485913, -1.484275, -1.485866)
y <- c(54.90083, 54.90078, 54.90077, 54.90011, 54.89936, 54.89935, 54.89935, 54.89879, 54.89902)

# convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(
  matrix(c(x,y), ncol=2), data.frame(ID=seq(1:length(x))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)


# convert both tables to sf standard
xy2 <- data.frame(x,y)
xy2_sf <- st_as_sf(xy2, coords=c('x', 'y'), crs="epsg:4326")

# compute distance matrix
# each row represents a target location and columns their distance to a master store (in meters)
dist_sf = st_distance(xy2_sf, xy2_sf)
mdist2 <- as.matrix(dist_sf)
mdist2 <- unclass(mdist2)

# cluster
hc <- hclust(as.dist(mdist2), method="complete")
plot(hc, cex = 0.6, hang = -1)
xy2$clust <- cutree(hc, h=200)
























########################################################################
########################################################################
########################################################################

# there are too many locations to join one-all for each store location, for example:
major_retailers %>%
  filter(retailer == "Tesco") %>%
  nrow()
# 2708

major_retailers %>%
  filter(retailer == "Sainsburys") %>%
  nrow()
# 1408

# number of rows for inner join = 2708*1408 = 3,812,864

# therefore, we will join on rounded lat, lng to capture nearby locations as starting point
# this will find nearby location-pairs, within c.2km, but avoids bloating table sizes too much

# NOTE: simply rounding to 2dp, will not capture all - we need to round up and down, and join
lng1 = 1.8249 #1.82
lat1 = 51.2499 #51.25
lng2 = 1.8251 #1.83
lat2 = 51.2501 #51.25
# would not join based on rounding to 2dp
calcHaversine(c(lng1, lat1), c(lng2, lat2))
# 26.27, so within distance, but not caught


#--- test 1
lng1 = 1.8249 
# 2dp = 1.82
lat1 = 51.2499 
# 2dp = 51.25

lng2 = 1.8251 
# 2dp = 1.83
lat2 = 51.2501 
# 2dp = 51.25


lng1_round = round(lng1, 3)
lng1_round
#1.825
round(lng1_round,2)
#1.82

lng2_round = round(lng2, 3)
lng2_round
#1.825
round(lng2_round, 2)
#1.82

lat1_round = round(lat1, 3)
lat1_round
#51.250
round(lat1_round, 2)
#51.25

lat2_round = round(lat2, 3)
lat2_round
#51.250
round(lat2_round, 2)
#51.25




# test 2 - comparison of rounded vs full join distanc

tesco_loc <- major_retailers %>%
  filter(retailer == "Tesco") %>%
  rowwise() %>%
  mutate(coordinates = list(c(lng, lat))) %>%
  mutate(lng_round = round(round(lng, 3), 2), lat_round = round(round(lat, 3), 2)) %>%
  ungroup


sain_loc <- major_retailers %>%
  filter(retailer == "Sainsburys") %>%
  rowwise %>%
  mutate(coordinates = list(c(lng, lat))) %>%
  mutate(lng_round = round(round(lng, 3), 2), lat_round = round(round(lat, 3), 2)) %>%
  ungroup


joined <- tesco_loc %>%
  inner_join(sain_loc, by =c("lng_round", "lat_round")) %>%
  rowwise() %>%
  mutate(haversine_dist = calcHaversine(coordinates.x, coordinates.y)) %>%
  ungroup

length(unique(joined$id.x)) # tesco
#451
length(unique(joined$id.y)) # sainsburys
#420

joined_cut <- joined %>%
  filter(haversine_dist <= 500)

length(unique(joined_cut$id.y)) # sainsburys locations within 500m
#326


# now we will test to see how many sainsburys locations are within 500m when we complete join
tesco_loc2 <- major_retailers %>%
  filter(retailer == "Tesco") %>%
  rowwise() %>%
  mutate(coordinates = list(c(lng, lat))) %>%
  mutate(join = 1) %>%
  ungroup

sain_loc2 <- major_retailers %>%
  filter(retailer == "Sainsburys") %>%
  rowwise %>%
  mutate(coordinates = list(c(lng, lat))) %>%
  mutate(join = 1) %>%
  ungroup

joined2 <- tesco_loc2 %>%
  inner_join(sain_loc2, by =c("join")) %>%
  rowwise() %>%
  mutate(haversine_dist = calcHaversine(coordinates.x, coordinates.y)) %>%
  ungroup

joined2_cut <- joined2 %>%
  filter(haversine_dist <= 500)

length(unique(joined2_cut$id.y)) # sainsburys locations within 500m
#469


diff <- joined2_cut %>%
  filter(!id.y %in% unique(joined_cut$id.y)) %>%
  rowwise() %>%
  mutate(lng_round.x = round(round(lng.x, 3), 2), lat_round.x = round(round(lat.x, 3), 2)) %>%
  mutate(lng_round.y = round(round(lng.x, 3), 2), lat_round.y = round(round(lat.y, 3), 2)) %>%
  ungroup


#--- we will attempt a different method using sf package for matrix computations


tesco_loc_sf <- major_retailers %>%
  filter(retailer == "Tesco")

tesco_loc_sf <- st_as_sf(tesco_loc_sf, coords=c('lng', 'lat'), crs="epsg:4326")

sain_loc_sf <- major_retailers %>%
  filter(retailer == "Sainsburys")

sain_loc_sf <- st_as_sf(sain_loc_sf, coords=c('lng', 'lat'), crs="epsg:4326")

dist = st_distance(sain_loc_sf, tesco_loc_sf)

# Now each row represents a sainsburys location 
# and each column represents their distance to a tesco box (in meters)

dist <- data.frame(dist)
dist <- dist %>%
  mutate(minimum)

dist$min <- apply(dist[], MARGIN =  1, FUN = min)

dist_cut <- dist %>% 
  filter(min <=500)

nrow(dist_cut)
# 469

# this equates to the same number as if we inner joined all, but is quicker computationally


########################################################################
########################################################################
########################################################################













lng1_round = round(lng1-0.005, 2) #1.82
lat1_round = round(lat1-0.005, 2) #51.24
lng2_round = round(lng2-0.005, 2) #1.82
lat2_round = round(lat2-0.005, 2) #51.25










#alternative logic
lng1 = 1.8249 
lng1_round2 = round(lng1, 2) #1.82
lng1_round1 = round(lng1, 1) #1.8

lat1 = 51.2501   
lat1_round2 = round(lat1, 2) #51.25
lat1_round1 = round(lat1, 1) #51.3

lng2 = 1.8251 
lng2_round2 = round(lng2, 2) #1.83
lng2_round1 = round(lng2, 1) #1.8

lat2 = 51.2499 
lat2_round2 = round(lat2, 2) #51.25
lat2_round1 = round(lat2, 1) #51.2

outputNearbyTable <- function(loc_df1, loc_df2) {
  
  loc_df1_upper <- loc_df1 %>% 
    rowwise %>%
    mutate(
      coordinates = list(c(lng, lat)),
      round_lng = round(lng, 2), # rounding to 2dp, loses 1100m precision,
      round_lat = round(lat, 2) #  joining ensures stores <2200m apart
    ) %>%
    ungroup
  
  
}

major_retailers <- major_retailers %>% 
  rowwise %>%
  mutate(
  coordinates = list(c(lng, lat)),
  round_lng = round(lng, 2), # rounding to 2dp, loses 1100m precision,
  round_lat = round(lat, 2) #  joining ensures stores <2200m apart
  ) %>%
  ungroup

tesco_loc <- major_retailers %>%
  filter(retailer == "Tesco")
  
other_retailers <- majors[! majors == "Tesco"]



sainsburys_loc <- major_retailers %>%
  filter(retailer == "Sainsburys")

rounded_merge <- tesco_loc %>%
  inner_join(sainsburys_loc, by =c("round_lng", "round_lat"))

rounded_merge <- rounded_merge %>%
  rowwise() %>%
  mutate(haversine_dist = calcHaversine(coordinates.x, coordinates.y))


joined_loc <- rbind(tesco_loc, sainsburys_loc)

p <- leaflet(joined_loc) %>% addTiles() %>%
  addCircles(~lng, ~lat,color = ~pal(retailer)) %>%
  overlayTitle("Major Retailer Store Locations") %>%
  addLegend(position = "bottomright", values = ~retailer, pal = pal); p





round(lat1, 2)

