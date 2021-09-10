#File for Spatial access models - Definitions
#Floating Catchment Areas
#This exercise has 3 components and it should be run in sequential order
# 1) Data Preparation: supply (primary care doctors) and demand (population in census tracts, etc.)
# 2) Run exercise in Python
# 3) Plot results from 2


#libraries
library(sf)
library(tidyverse)
library(tmap)
library(tidycensus)
library(tigris)
library(matrixStats)
library(SpatialAcc)
library(geojsonio)
library(rmapshaper)
library(sp)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(tibble)


#########################################################
# 1) Data Preparation: preparation of information census tract
#########################################################
#1.1 Population at census tract level for VA va_pop

#load in your Census API Key
census_api_key("eba406410c653b81d6a795ac4e989221f7bdf302")

# Bring in census tract data. 
va.tracts.ct <- get_acs(geography = "tract", 
                        year = 2019,
                        variables = c(tpop = "B01003_001E"
                                      , tpopr = "B03002_001E",
                                      nhwhite = "B03002_003E", nhblk = "B03002_004E",
                                      nhasn = "B03002_006E", hisp = "B03002_012E",
                                      medinc = "B19013_001E"
                        ),
                        state = "VA",
                        survey = "acs5",
                        output = "wide",
                        geometry = TRUE)

#select relevant variables
va.tracts.ct<-va.tracts.ct  %>%  select(GEOID, NAME, tpop, geometry )

#open name to separate county names
va.tracts.ct <- separate(va.tracts.ct, NAME, c( "ctract", "county", "state"), sep = ",")

# Make the data tidy, calculate percent race/ethnicity, and keep essential vars.
# va.tracts.ct <- va.tracts.ct %>% 
#   mutate(pnhwhite = nhwhite/tpopr, pnhasn = nhasn/tpopr, 
#          pnhblk = nhblk/tpopr, phisp = hisp/tpopr, county=county) %>%
#   #rename(tpop = tpop, medinc = medinc) %>%
#   dplyr::select(c(GEOID,tpop, pnhwhite, pnhasn, pnhblk, phisp, medinc, county))  


#1.2 Bring in city boundary data in VA
va.ct <- tracts(state = "VA", year = 2019, cb = TRUE)

#Clip tracts using VA boundary: ms_clip Removes portions of the target layer that fall outside the clipping layer or bounding box
va.ct.tracts <- ms_clip(target = va.tracts.ct, clip = va.ct, remove_slivers = TRUE)


#1.3 reproject to UTM 
va.ct.tracts.utm <-st_transform(va.ct.tracts, crs = 4326)  #4326 for whole US
#Identify invalid spherical geometry.
inv_geom <- which(st_is_valid(va.ct.tracts.utm)== "FALSE")
#1 block group has not adequate geometry.  then slice if needed
va.ct.tracts.utm <- va.ct.tracts.utm %>% slice(-c(inv_geom))

va_pop_ct <- va.ct.tracts.utm %>% select(GEOID, tpop, geometry)


#########################################################
#1.4 Primary Care Doctors at census tract level for VA va_doc
#Bring information from pgadmin: 
#a)log into datacommons
conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host =  "postgis1",
                               port = 5432,
                               user = "your_key",
                               password = "your_key")
#b)query
prim.care.va <- sf::st_read(
  conn, 
  query= "
    SELECT *
    FROM data_commons.virginia_primary_care_doctors_geolocated_blks"
)

#c) Disconnect
RPostgreSQL::dbDisconnect(conn)

#Make sure the tract and bank Coordinate Reference Systems are the same and in UTM.
st_crs(va.ct.tracts.utm)$proj4string
#st_crs(hosp_va)$proj4string
st_crs(prim.care.va)$proj4string


###############################################################################################
#1.5) Points in Polygon: identify and place the doctors in every census tract
###############################################################################################
#1 st_join
va.ct.tracts.prim.care <- prim.care.va  %>% 
  st_join(va.ct.tracts.utm)
#2 order
va.ct.tracts.prim.care <- va.ct.tracts.prim.care[order(va.ct.tracts.prim.care$GEOID),]
#3 drop geom
va.ct.tracts.prim.care <- st_drop_geometry(va.ct.tracts.prim.care)
#4 group and summarize
va.ct.tracts.prim.care <- va.ct.tracts.prim.care %>%
  group_by(GEOID) %>% 
  summarize(prim.care.n = n()) 
#rename for python file
va_doc_ct <- va.ct.tracts.prim.care

######################################################################################
#create geoid by ct
# va.bg.tracts.prim.care$geoidct <- substr( va.bg.tracts.prim.care$GEOID,1,11 )
# 
# #yes
# va.ct.tracts.prim.care <- as.data.frame(va.bg.tracts.prim.care)
# 
# 
# prim.care.va$GEOID <- substr( prim.care.va$geoid_blk,1,11 )
# #names(prim.care.va)
# 
# va.ct.tracts.prim.care <- prim.care.va  %>% 
#   st_join(va.ct.tracts.utm) 
# 
# va.ct.tracts.prim.care <- va.ct.tracts.prim.care %>%
#   group_by(GEOID.x) %>% 
#   dplyr:: summarize(prim.care.n = n()) 
# 
# va_doc_ct <- st_drop_geometry(va.ct.tracts.prim.care)

#prepare names for files in python
names(va_doc_ct) <- c("geoid","doc")
names(va_pop_ct) <- c("geoid","pop", "geometry")

#########################################################
#1.6 Create the centroid for every census tract

#CENTROIDS for census tract, use the sf function st_centroid()
tract.centroids_ct <- st_centroid(va.ct.tracts.utm) 

#1.7 matrix with distances between population (centroids of tracts) and doctors
prim.care.dist_ct <-st_distance(tract.centroids_ct, prim.care.va)

##work on matrix distances
#coordenates expand for every centroid with very doctor ()

# aaa <- st_drop_geometry(tract.centroids_ct[,1] )   
# bbb <- st_drop_geometry(prim.care.va[,6] )  

#create id for census tract for doctors
prim.care.va$GEOID <- substr( prim.care.va$geoid_blk,1,11 )
#vectors with geoid only
aa <- st_drop_geometry(tract.centroids_ct ) 
bb <- st_drop_geometry(prim.care.va ) 
# a <- aa$GEOID
# b <- bb$GEOID

#1.8 expand: each doctor with distance to centroids
coor_distances_ct <- expand.grid(aa$GEOID, bb$GEOID)
#change names
names(coor_distances_ct) <- c("dest","origin")

#rearrange columns
distance_line_ct <- c( prim.care.dist_ct)
#add distance
coor_distances_ct$distance <- distance_line_ct
#create matrix for py
va_times_ct <- coor_distances_ct
speed <- 40  #40 miles per hour
#1609 meters = mile
va_times_ct$cost <- va_times_ct$distance*60/(speed*1609)
names(va_times_ct)[3]<- 'euclidean'

######

#1.9 coordinates for centroids ct
centroid.coords_ct <- st_coordinates(tract.centroids_ct)
#drop geometry of pop
va_pop_ct <- st_drop_geometry(va_pop_ct)

###########################

#1.10 Save files for python
write.csv(va_pop_ct, "~/VDH/files_for_python/ct/va_pop_ct.csv")
write.csv(va_doc_ct, "~/VDH/files_for_python/ct/va_doc_ct.csv")
write.csv(va_times_ct, "~/VDH/files_for_python/ct/va_times_ct.csv")
write.csv(centroid.coords_ct, "~/VDH/files_for_python/ct/centroid.coords_ct.csv")


#########################################################
# 2) Python exercise
#########################################################
#Open the four files in python Jupyter and follow code

#bring file B.access_df.csv


#########################################################
# 3) Plot Catchment Areas
#########################################################
#Open the four files in python Jupyter and follow code: va_census_tract_access_vdh

B <- read_csv("~/myVDH/files_for_python/ct/B.access_df.csv")
names(B)[1] <- 'GEOID'

#select only geoid and geometry
va_geo_ct <- va.tracts.ct %>% select(GEOID)
#merge geometry
va_access_geo <- merge( B, va_geo_ct, by='GEOID'  )
#state format sf
va_access_geo_sf <- st_as_sf( va_access_geo)

#va_access_geo_sf <- read_rds("~/VDH/va_access_geo_sf")

#########PLOTS

#map view format
tmap_mode("view")

#1) FCA - Floating Catchment Area
fca<- tm_shape(va_access_geo_sf) +
  tm_polygons(col = "fca_doc", midpoint = 0)+
  tm_scale_bar(position = c("left", "bottom"))+
  tmap_options(check.and.fix = TRUE) 

fca 

#2) 2FCA - 2 Step Floating Catchment Area 
twofca <- tm_shape(va_access_geo_sf) +
  tm_polygons(col = "2sfca_doc", midpoint = 0)+
  tm_scale_bar(position = c("left", "bottom"))+
  tmap_options(check.and.fix = TRUE) 

twofca

#3) E2FCA - Enhanced 2 Step Floating Catchment Area
e2fca <- tm_shape(va_access_geo_sf) +
  tm_polygons(col = "2sfca30_doc", midpoint = 0)+
  tm_scale_bar(position = c("left", "bottom"))+
  tmap_options(check.and.fix = TRUE) 

e2fca

#3) 3FCA - 3 Step Floating Catchment Area
threefca <- tm_shape(va_access_geo_sf) +
  tm_polygons(col = "3sfca_doc", midpoint = 0)+
  tm_scale_bar(position = c("left", "bottom"))+
  tmap_options(check.and.fix = TRUE) 

threefca
