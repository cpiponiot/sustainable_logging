setwd("C:/Users/piponiot-laroche/projets-cirad/sustainable_logging/shapefiles")

zips = list.files(path = "originals/", pattern = "\\.zip", full.names = TRUE)
unzip(zips[1])
unzip(zips[2])
unzip(zips[3])

names_conc <- unique(unlist(lapply(strsplit(list.files(pattern = ".zip|.rar"), " "), first)))

# ## unzip -> extract to filename\
# sapply((list.files(pattern = "\\.zip")), function(x) unzip(x, exdir = "shapes"))
# ## remove zips
file.remove(list.files(pattern = "\\.zip|\\.rar"))

## open shapefiles

# library("rgdal")

foldernames <- list.files()[!grepl("originals|final", list.files())]
shape_list <- list()
concessions <- SpatialPolygons(list())
names_all <- c()
names_shapes <- c()

for (sh in foldernames){
  all_layers <- unique(sapply(strsplit(list.files(sh), "\\."), first))
  names_all <- c(names_shapes, all_layers)
  for (x in all_layers) {
    tryCatch({
      new <- rgdal::readOGR(dsn = sh, layer = x)
      new_proj <- sp::spTransform(new, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
      names_shapes <- c(names_shapes, x)
      shape_list <- c(shape_list, new_proj)
      names(shape_list) <- names_shapes
      concessions <- rgeos::gUnion(new_proj, concessions)
    }, error = function(e) {})
  }
}
missing <- names_all[!names_all %in% names(shape_list)]

concessions <- as(concessions, "SpatialPolygonsDataFrame" )
rgdal::writeOGR(concessions, dsn = "final/", layer = "BRconcessions", driver="ESRI Shapefile")

amazonia <- rgdal::readOGR(dsn="C:/Users/piponiot-laroche/projets-cirad/vrec-br-concessions/data/amazonia", layer = "amazlm_1608")
plot(amazonia)
plot(concessions, add = TRUE, col=2)
