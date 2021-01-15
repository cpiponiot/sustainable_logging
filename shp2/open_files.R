
setwd("C:/Users/piponiot-laroche/projets-cirad/sustainable_logging/shp2/")

files <- gsub("\\.shp", "", list.files(".", "shp$"))
all_flonas <- SpatialPolygons(list())

for (sh in files) {
  ufs <- readOGR(dsn = ".", layer = sh)
  if (any(grepl("FLORESTA NACIONAL", ufs$nome))) {
    flonas <- ufs[grep("FLORESTA NACIONAL", ufs$nome),]
    all_flonas <- rgeos::gUnion(flonas, all_flonas)
  }
}

amazonia <- rgdal::readOGR(dsn="C:/Users/piponiot-laroche/projets-cirad/vrec-br-concessions/data/amazonia", layer = "amazlm_1608")
plot(amazonia)
plot(all_flonas, add = TRUE, col=2)

all_flonas <- as(all_flonas, "SpatialPolygonsDataFrame" )
rgdal::writeOGR(all_flonas, dsn = "C:/Users/piponiot-laroche/projets-cirad/sustainable_logging/shapefiles/final", layer = "BRflonas", driver="ESRI Shapefile")
