# Tiles hosting pipeline with Tileserver GL ----

setup_docklet_kd <- function(size = "s-2vcpu-4gb-intel",
                          region = "tor1",
                          image = "docker-20-04",
                          tags = c("bccciss", "tileserver"),
                          checks = FALSE,
                          ...) {

  
  # Check region is in Canada
  if (checks == TRUE && region != "tor1") {
    choices <- c("Yes", "No")
    title <- "Region selected is not `tor1`. You might be using a server located outside Canada. Are you sure?"
    if (utils::menu(choices = choices, title = title) == 2) {
      return(NULL)
    }
  }
  
  # Check image for Docker in region
  if (checks == TRUE && !image %in% docker_images(region)) {
    choices <- c("Absolutely", "What's a docker? (No)")
    title <- "Image selected is not an official Docker image in this region. Are you sure?"
    if (utils::menu(choices = choices, title = title) == 2) {
      return(NULL)
    }
  }
  
  # Create drocklet
  droplet <- analogsea::droplet_create(image = image, region = region,
                                       size = size, tags = tags, wait = TRUE, ...)
  
  # Wait for the network interface
  while (length(droplet$networks$v4) == 0) {
    droplet <- analogsea::droplet(id=droplet$id)
    Sys.sleep(10)
  }
  
  analogsea::debian_add_swap(droplet)
  setup_firewall(droplet)
  setup_updates(droplet)
  install_tippecanoe(droplet)
  install_nginx(droplet, system.file("tileserver", "nginx.conf", package="bccciss"))
  prepare_tileserver(droplet)
  return(invisible(droplet))
}

remote_shp_tiles_kd <- function(droplet, ..., source_dir, remote_dir = "/tmp/shp", skip_upload = FALSE) {
  
  layers <- list.files(source_dir, "\\.shp$", ignore.case = TRUE)
  if (length(layers) == 0) {
    message("Could not find any shape files to upload")
    return(NULL)
  }
  names(layers) <- tools::file_path_sans_ext(layers)
  
  if (!skip_upload == TRUE) {
    analogsea::droplet_ssh(droplet, paste("mkdir -p", remote_dir))
    analogsea::droplet_upload(droplet, local = list.files(source_dir, full.names = TRUE), remote = remote_dir)
    geojsons <- character()
    for (i in 1:length(layers)) {
      geojson <- shQuote(file.path(remote_dir, paste0(names(layers[i]), ".geojson")))
      shp <- shQuote(file.path(remote_dir, layers[i]))
      analogsea::droplet_ssh(droplet, paste("ogr2ogr -f GeoJSON", geojson, shp))
      geojsons <- c(geojsons, geojson)
    }
    analogsea::droplet_ssh(droplet, paste("ls -alh", remote_dir,"| grep geojson"))
  } else {
    geojsons <- shQuote(file.path(remote_dir, paste0(names(layers), ".geojson")))
  }
  
  base_cmd <- "tippecanoe"
  cmd <- paste(base_cmd, paste(..., collapse = " "), paste(geojsons, collapse = " "))
  analogsea::droplet_ssh(droplet, cmd)
  analogsea::droplet_ssh(droplet, "ls -alh /mapdata | grep mbtiles")
  
  return(invisible(droplet))

}



launch_tileserver_kd <- function(droplet, config, styles) {
  if (!missing(config)) {
    analogsea::droplet_upload(droplet, config, "/mapdata")
  }
  if (!missing(styles)) {
    analogsea::droplet_upload(droplet, styles, "/mapdata")
  }
  # Clear cache and reload nginx
  analogsea::droplet_ssh(droplet, "rm -rf /cache/nginx/*")
  analogsea::droplet_ssh(droplet, "systemctl reload nginx")
  # Stop all running containers
  analogsea::droplet_ssh(droplet, "docker ps -q -a | xargs -r docker stop")
  # Star tileserver as root
  analogsea::droplet_ssh(droplet, "docker run --rm -v /mapdata:/data -p 8080:8080 -d --user root maptiler/tileserver-gl -s")
  Sys.sleep(3)
  utils::browseURL(paste0("http://", analogsea:::droplet_ip_safe(droplet)))
  return(droplet)
}
