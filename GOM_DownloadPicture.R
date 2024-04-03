###Download all pictures and metadata from GOM trials database
##Kiri Daust, Feb 2024

library(RPostgres)
library(jsonlite)
library(data.table)

pgConn <- dbConnect(Postgres(), 
                    dbname = Sys.getenv("GOM_DB"),
                    host = Sys.getenv("GOM_HOST"),
                    port = Sys.getenv("GOM_PORT"), 
                    user = "postgres",
                    password = Sys.getenv("GOM_PWD"))

out_folder <- "gom_pics" ##specify folder for output
if(!dir.exists(out_folder)) dir.create(out_folder)

all_ids <- dbGetQuery(pgConn, "select picture_id from planting_picture")$picture_id

meta_ls <- list()
for(curr_id in all_ids){
  cat(".")
  metadat <- as.data.table(dbGetQuery(pgConn, paste0("select picture_id, trial_id, date_established, latitude, longitude 
                      from planting_info join planting_picture using (plantation_id) 
                      where planting_picture.picture_id = ",curr_id)))
  meta_ls[[curr_id]] <- metadat
  
  outfile_name <- paste0("gom_id_",curr_id,".png")
  thisQ = paste0("SELECT encode(picture, 'base64') AS image from planting_picture WHERE picture_id=",curr_id,";")
  resultSet = dbGetQuery(pgConn, thisQ)
  imageData <- resultSet$image
  
  # Decode from base64 back to binary
  imageDataDecoded <- jsonlite::base64_dec(imageData)
  write.filename = file(paste0(out_folder,"/", outfile_name), "wb")
  writeBin(imageDataDecoded, write.filename)
  close(write.filename)
}

meta_all <- rbindlist(meta_ls)
fwrite(meta_all, paste0(out_folder,"/meta_info.csv"))

#### add meta data to images
###https://www.thomasvanhoey.com/post/changing-photo-metadata-with-r/
# general packages
library(tidyverse) # our swiss army knife
library(fs) # reading in files in a tidyish way
library(lubridate) # working with dates 

# specialized packages
library(exifr) # for reading photo metadata

files <- dir_ls("./gom_pics",
recurse = TRUE,
regexp = ".png$")


# defining a function
# change_file_create_date <- function(afb){
#   # get correct exif date
#   read_exif(path = afb,
#             tags = c("DateTimeOriginal")) %>%
#     pull(DateTimeOriginal) %>%
#     parse_date_time("Ymd HMS", 
#                     tz = "Asia/Taipei" #timezone
#     ) -> correct_time
#   
#   # changing the creation date
#   Sys.setFileTime(afb, correct_time)
#   
# }
# 
# 
# # run the function over all the different files
# purrr::walk(files, change_file_create_date)