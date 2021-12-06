# This file is used to create the tileserver on Digital Ocean
# and create tiles from shape file.

library(sf)
library(analogsea)
Sys.setenv(DO_PAT="eae4166ed2fac0e3c41660fe26a009bb0176ab8bceeaf753faf5189f58a06520")
library(ccissdev)
source("./TS_Functions.R")

wna <- st_read("~/CommonTables/WNA_BGC_v12_6Sept2021.gpkg")

# ab <- st_read("~/CommonTables/ABOutline.gpkg")
# ab <- ab["PRENAME"]
# ab <- ab[ab$PRENAME == "Alberta",]
# ab <- st_transform(ab, 3005)
# colnames(ab)[1] <- "ID"
# us <- st_read("~/CommonTables/US_Outline.gpkg")
# us <- us["GEOID"]
# us <- st_transform(us,3005)
# colnames(us)[1] <- "ID"
# 
# us_ab <- rbind(ab,us)
# plot(us_ab)
# bc <- st_read("~/CommonTables/BC_Province_Outline_Clean.gpkg")
# us_ab <- st_buffer(us_ab,dist = 0)
# 
# wan <- st_buffer(wna,dist = 0)
# us_ab <- st_union(st_combine(us_ab))
# us_ab <- st_buffer(us_ab,dist = 500)
# bc_bgc <- st_difference(wan,us_ab)
# 
# bc_bbox <- st_read("~/CommonTables/BC_BBox.gpkg")
# st_crs(bc_bbox) <- 3005
# bc_bgc <- st_intersection(bc_bgc,bc_bbox)
# 
# st_write(bc_bgc,"~/CommonTables/BC_BGCv12_Fixed.gpkg", delete_dsn = T)
# bc_bgc <- st_intersection(wan,us_ab)
###########################

out_dir <- "./data-raw/shp"
#shp_name <- "BEC_MAP.shp"
layer <- "WNA_MAP"
system("rm -R ./data-raw")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
wna <- st_transform(wna,4326)
colnames(wna)
wna$ZONE <- NULL
wna$MAP_LABEL <- wna$BGC
st_write(wna,dsn = out_dir,layer = layer,driver = "ESRI Shapefile")

# dat <- st_read("../../OldGrowthApp/Seral34.gpkg")
# dat$PolyID <- seq_along(dat$Seral)
# # dat <- dat[,c("DISTRICT_N","ORG_UNIT","REGION_ORG","REGION_O_1")]
# # colnames(dat)[1:4] <- c("dist_name","dist_code","reg_code","reg_name")
# dat <- st_transform(dat,4326)
# st_write(dat,dsn = out_dir,layer = "Seral",driver = "ESRI Shapefile")
# # Digital Ocean provisioning - Setup your SSH keys in your accounts before running these.
# #tileserver <- setup_docklet(size = "s-2vcpu-4gb-intel")
# # Or Reuse an existing droplet


tileserver <- droplets()[["tileserver-wna"]]
# About 5-6h
#out_dir <- "./Data/Cutblocks"
remote_shp_tiles_kd(tileserver,
                 "-o /mapdata/wnabgc.mbtiles -Z5 -z17 --simplification=10 --force --coalesce-densest-as-needed --extend-zooms-if-still-dropping --detect-shared-borders",
                 source_dir = out_dir, skip_upload = F)

launch_tileserver_kd(tileserver,config = "./config/tileserver/config.json")


