library(sp)

#### PUBLIC FORESTS ####

## from http://www.florestal.gov.br/component/content/article/127-informacoes-florestais/cadastro-nacional-de-florestas-publicas-cnfp/1894-cadastro-nacional-de-florestas-publicas-atualizacao-2019?Itemid=
files <- gsub("\\.shp", "", list.files("data_sfb/public-forests/", "shp$"))
all_flor <- sp::SpatialPolygons(list())

for (sh in files) {
  ufs <- rgdal::readOGR(dsn = "data_sfb/public-forests", layer = sh)
  ufs <- spTransform(ufs, CRS("+proj=longlat +datum=WGS84"))
  if (any(ufs$protecao == "USO SUSTENTAVEL" & 
          grepl("Amaz", ufs$bioma) &
          ufs$comunitari == "NAO")) {
    flor <- ufs[ufs$protecao == "USO SUSTENTAVEL" & 
                  grepl("Amaz", ufs$bioma) &
                  ufs$comunitari == "NAO",]
    all_flor <- rgeos::gUnion(flor, all_flor)
  }
}


#### CURRENT CONCESSIONS ####
## from https://www.florestal.gov.br/documentos/concessoes-florestais/concessoes-florestais-florestas-sob-concessao
folders <- list.dirs("data_sfb/current-federal-concessions")
files_list <- sapply(folders, function(dir) gsub("\\.shp", "", list.files(dir, "shp$")))
files <- unlist(files_list)
names(files) <- rep(names(files_list), sapply(files_list, length))
files <- files[grepl("UMF", files)]

all_conc <- sp::SpatialPolygons(list())

for (i in seq_len(length(files))) {
  conc <- rgdal::readOGR(dsn = names(files)[i], layer = files[i])
  # change CRS
  conc <- spTransform(conc, CRS("+proj=longlat +datum=WGS84"))
  all_conc <- rgeos::gUnion(conc, all_conc)
}


## plot all 
amazonia <- rgdal::readOGR(dsn="C:/Users/piponiot-laroche/projets-cirad/vrec-br-concessions/data/amazonia", layer = "BassinAmazonien")
## change the projection system
amazonia <- spTransform(amazonia, CRS("+proj=longlat +datum=WGS84"))
plot(amazonia)
plot(all_flor, add = TRUE)
plot(all_conc, add = TRUE, col=2)

all_flor <- as(all_flor, "SpatialPolygonsDataFrame")
rgdal::writeOGR(all_flor, dsn = "data_sfb/final", layer = "BRflopub", driver="ESRI Shapefile")

all_conc <- as(all_conc, "SpatialPolygonsDataFrame")
rgdal::writeOGR(all_conc, dsn = "data_sfb/final", layer = "BRconc", driver="ESRI Shapefile")
