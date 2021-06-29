# This file is used to create the tileserver on Digital Ocean
# and create tiles from shape file.

library(sf)
library(analogsea)
Sys.setenv(DO_PAT = "b02741f9a02e7386464b5dd7a2dbea6d890c3f8051fd32fd239e17590a0039bf")
library(bccciss)
source("./TS_Functions.R")

system("rm -R ./data-raw")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dat <- st_read("./OrigData/Forest_Region_District2.gpkg")
dat <- dat[,c("DISTRICT_N","ORG_UNIT","REGION_ORG","REGION_O_1")]
colnames(dat)[1:4] <- c("dist_name","dist_code","reg_code","reg_name")
dat <- st_transform(dat,4326)
st_write(dat,dsn = out_dir,layer = layer,driver = "ESRI Shapefile")
# Digital Ocean provisioning - Setup your SSH keys in your accounts before running these.
#tileserver <- setup_docklet(size = "s-2vcpu-4gb-intel")
# Or Reuse an existing droplet


tileserver <- droplets()[["TyrannicalAccessibility"]]
# About 5-6h
out_dir <- "./Data/Cutblocks"
remote_shp_tiles_kd(tileserver,
                 "-o /mapdata/cutblocks.mbtiles -z15 --simplification=10 --force --coalesce-densest-as-needed --extend-zooms-if-still-dropping --detect-shared-borders",
                 source_dir = out_dir, skip_upload = F)


launch_tileserver_kd(tileserver,config = "./config/tileserver/config.json")

