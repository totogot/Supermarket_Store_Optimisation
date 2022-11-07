#################################################################################################
#################################################################################################
# PACKAGES

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



#################################################################################################
#################################################################################################
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



#################################################################################################
#################################################################################################
# MAIN RUN



############ Initial Exploration

# load starting data
all_stores <- read_xls('data/GEOLYTIX - UK RetailPoints/uk_glx_open_retail_points_v24_202206.xls')

# plot store count by retailer
retailer_count <- all_stores %>%
  group_by(retailer) %>%
  tally()

ggplot(retailer_count, aes(x = reorder(retailer, -n), y = n)) +
  geom_bar(stat = "identity") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))

# plot store count by cluster as %
retailer_count <- retailer_count %>%
  mutate(percentage = (n / sum(n))*100)

ggplot(retailer_count, aes(x = reorder(retailer, -percentage), y = percentage)) +
  geom_bar(stat = "identity") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))

# create list of largest stores
majors <- c("Aldi", "Asda", "Lidl", "Marks and Spencer", "Morrisons", 
            "Sainsburys", "Tesco", "The Co-operative Group", "Waitrose")

# compute % of stores captured by major chains
print(retailer_count %>% filter(retailer %in% majors) %>% pull(percentage) %>% sum)
# 66.975 %
# the major chains capture 2/3rds of the universe

# plot location of major chains
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



############ Purchase of Competitor

# assuming we are Tesco (as the largest supermarket) and are looking to acquire one of the other chains
# imagine that condition under which we will acquire is based on which chain offers the "best coverage"
# taht is to say, where there is minimum overlap with existing Tesco locations

# i.e. find the competitor where least % of stores within a certain radius of Tesco stores

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
# ------results
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

# from the above results we see that Morrisons and Asda have the lowest %
# it is also interesting to see the number of stores these brands offer

print(retailer_count %>% filter(retailer %in% c('Asda', 'Morrisons')))
# Asda        637
# Morrisons   901

# as well as the array of store sizes - and how this compares to Tesco
store_size_counts <- all_stores %>%
  filter(retailer %in% c('Tesco', 'Asda', 'Morrisons')) %>%
  group_by(retailer, size_band) %>%
  tally() %>%
  group_by(retailer) %>%
  mutate(percentage_stores = (n / sum(n))*100)


ggplot(store_size_counts, aes(x = size_band, y = percentage_stores, fill=retailer)) +
  geom_col(position = "dodge") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))

# interestingly we see that Asda has a distinctly different business model
# with far greater focus (>50%) of their stores being large superstores
# while Tesco and Morrisons are more similar (although Tesco has greater % small stores)

# given Morrisons has the lowest %, more stores and similar profile, we elect to "acquire" it

# create a combined dataset
combined_stores <- major_retailers %>%
  filter(retailer %in% c('Tesco', 'Morrisons'))

# now we have pretty good coverage across the UK
p <- leaflet(combined_stores) %>% addTiles() %>%
  addCircles(~lng, ~lat,color = 'blue', radius = 500, opacity = .5) %>%
  overlayTitle("Tesco Store Locations"); p

# save down combined dataset
write_rds(combined_stores, "data/combined_store.rds")




############ Identifying Which Stores to Close

# now we have a group of store locations, next we can optimise the portfolio
# we will do this by opting to close stores in the same location
# however, we will need some logic to prioritise which stores to keep stores

# load checkpoint dataset
combined_stores <- read_rds("data/combined_store.rds")

# split by brand
tesco_stores <- combined_stores %>%
  filter(retailer == 'Tesco')

morrisons_stores <- combined_stores %>%
  filter(retailer == 'Morrisons')


# convert both tables to sf standard
tesco_stores_sf <- st_as_sf(tesco_stores, coords=c('lng', 'lat'), crs="epsg:4326")
morrisons_stores_sf <- st_as_sf(morrisons_stores, coords=c('lng', 'lat'), crs="epsg:4326")

# compute distance matrix
dist_sf = st_distance(tesco_stores_sf, morrisons_stores_sf)

# note: each row represents a Tesco location and columns their distance to a Morrisons store (in meters)
M <- as.matrix(dist_sf)
M <- unclass(M)

#create binary matrix to show where Tesco store (row) within 500m of Morrisons (column) 
M[] <- ifelse(M<500,1,0)

# convert to dataframe and add column to indicate Tesco index number
dist_sf <- data.frame(M)

dist_sf <- rownames_to_column(dist_sf) %>%
  rename(tesco_index = rowname)

# convert from wide-form to long-form
close_store_df <- dist_sf %>% 
  gather(key = morrisons_index, value = flag, -c(tesco_index)) %>%
  filter(flag == 1)

# clean up the morrions index column - removing the "X" from the naming convention
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

# join back to the Tesco master to indicate Morrisons locations
tesco_stores_df <- rownames_to_column(tesco_stores) %>%
  rename(tesco_index = rowname) %>%
  inner_join(close_store_df, by='tesco_index')

# join on the relevant Morrisons store data
morrisons_stores_df <- rownames_to_column(morrisons_stores) %>%
  rename(morrisons_index = rowname)
  
store_pairs <- tesco_stores_df %>%
  left_join(morrisons_stores_df, by='morrisons_index')


#######
#######
#######
#######
#######
#######

# map postcodes to output Layer Super Output Area (LSAO)
# https://geoportal.statistics.gov.uk/datasets/06938ffe68de49de98709b0c2ea7c21a/about
postcode_lsao_data <- read_csv("data/Postcode Data/PCD_OA_LSOA_MSOA_LAD_AUG19_UK_LU.csv")

# map income to LSAO
# https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/datasets/regionalgrossdisposablehouseholdincomegdhibylocalauthorityintheuk
income_data <- read_excel(
  "data/Postcode Data/vcregionalgdhibylareordered.xlsx", 
  sheet = "Gross disposable income", 
  skip = 2)

#######
#######
#######
#######
#######




############ Deciding Most Appropriate Warehouse Locations

# after optimising the locations, we will decide where best to set up supply depots
# we will achieve this by determining the best geographic clusters
# the aim is to form clusters that capture the closest stores, without overlap
# the distribution warehouses/depots would then be located at the centroids of the clusters

# load combined store set
combined_stores <- read_rds("data/combined_store.rds")

# take just the lat and lng coordinates
locs_df <- combined_stores %>%
  select("id","lng", "lat") %>%
  column_to_rownames("id")

# set random seed to ensure repeatability of clustering
set.seed(123)


#-- KMEANS CLUSTERING

# ok results but bad practice to use K means when it comes to geospatial data

# Compute k-means with k = 20
km.res <- kmeans(locs_df, 20)

print(km.res)

# assign the cluster numbers
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

# trialed but lead to really poor results

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

# chosen method for initial clustering

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


# we found that the clusters with few store locations tended to be on isolated locations
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

















