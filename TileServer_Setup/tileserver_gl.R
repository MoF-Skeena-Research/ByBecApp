# This file is used to create the tileserver on Digital Ocean
# and create tiles from shape file.

library(sf)
library(analogsea)
Sys.setenv(DO_PAT="eae4166ed2fac0e3c41660fe26a009bb0176ab8bceeaf753faf5189f58a06520")
library(bccciss)
source("./TS_Functions.R")

out_dir <- "./data-raw/Map"
#shp_name <- "WNA_MAP.shp"
layer <- "Districts"
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
tileserver <- droplets()[["tileserver-wna"]]
# About 5-6h
remote_shp_tiles_kd(tileserver,
                 "-z15 --simplification=10 --force --coalesce-densest-as-needed --extend-zooms-if-still-dropping --detect-shared-borders",
                 source_dir = out_dir, skip_upload = F)
launch_tileserver(tileserver,config = "./TileServer_Setup/config/tileserver/config.json")

