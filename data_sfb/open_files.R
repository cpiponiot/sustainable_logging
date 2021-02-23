library(sp)

#### PUBLIC FORESTS ####

## from http://www.florestal.gov.br/component/content/article/127-informacoes-florestais/cadastro-nacional-de-florestas-publicas-cnfp/1894-cadastro-nacional-de-florestas-publicas-atualizacao-2019?Itemid=
files <- gsub("\\.shp", "", list.files("data_sfb/public-forests/", "shp$"))
all_flor <- lapply(files, function(sh) rgdal::readOGR(dsn = "data_sfb/public-forests", layer = sh))
all_flor <- do.call(raster::bind, all_flor)
all_flor <- spTransform(all_flor, CRS("+proj=longlat +datum=WGS84"))

## subset to get only areas available for concessions
all_flor <- all_flor[all_flor$protecao == "USO SUSTENTAVEL" & 
                       grepl("Amaz", all_flor$bioma) & 
                       all_flor$comunitari == "NAO", ]
## clean area names
all_flor$name <- gsub("Ã\u0081|Ãƒ|Ã£", "A", all_flor$nome)
all_flor$name <- gsub("Ã‡|Ã§", "C", all_flor$name)
all_flor$name <- gsub("Ã“|Ã•|Ã”", "O", all_flor$name)
all_flor$name <- gsub("Ã‰", "E", all_flor$name)
all_flor$name <- gsub("Ãš", "U", all_flor$name)
all_flor$name <- gsub("Ã\u008d", "I", all_flor$name)
all_flor$name <- gsub(" | / | - ", "_", all_flor$name)
all_flor@data <- data.frame(ppf = "potential", name = all_flor$name)


#### CURRENT FEDERAL CONCESSIONS ####
## from https://www.florestal.gov.br/documentos/concessoes-florestais/concessoes-florestais-florestas-sob-concessao
folders <- list.dirs("data_sfb/current-federal-concessions")
files_list <- sapply(folders, function(dir) gsub("\\.shp", "", list.files(dir, "shp$")))
files <- unlist(files_list)
names(files) <- rep(names(files_list), sapply(files_list, length))
files <- files[grepl("UMF", files)]

all_fconc <- sp::SpatialPolygons(list())

for (i in seq_len(length(files))) {
  conc <- rgdal::readOGR(dsn = names(files)[i], layer = files[i])
  # change CRS
  conc <- spTransform(conc, CRS("+proj=longlat +datum=WGS84"))
  conc$flona <- gsub("data_sfb/current-federal-concessions/Shapes_", "", names(files)[i])
  all_fconc <- raster::bind(conc, all_fconc)
}
all_fconc$UMF[is.na(all_fconc$UMF)] <- all_fconc$NOME[is.na(all_fconc$UMF)]
all_fconc$UMF <- gsub("UNIDADE DE MANEJO FLORESTAL", "UMF", all_fconc$UMF)
all_fconc@data <- data.frame(ppf = "current - federal", 
                            name = gsub(" ", "", paste(all_fconc@data$flona, 
                                                       all_fconc@data$UMF, sep = "_")))

#### CURRENT STATE CONCESSIONS ####
# Para: https://ideflorbio.pa.gov.br/contratos-de-concessao-florestal/
files <- gsub("\\.shp", "", list.files("data_sfb/current-state-concessions/", "shp$"))
all_sconc <- lapply(files, function(sh) rgdal::readOGR(dsn = "data_sfb/current-state-concessions", layer = sh))
all_sconc <- do.call(raster::bind, all_sconc)
all_sconc@data <- data.frame(ppf = "current - state", name = c(files[1:3], rep("paru", nrow(all_sconc@data)-3)))
all_sconc <- spTransform(all_sconc, CRS("+proj=longlat +datum=WGS84"))

all_conc <- raster::bind(all_sconc, all_fconc)

## plot all 
amazonia <- rgdal::readOGR(dsn="C:/Users/piponiot-laroche/projets-cirad/vrec-br-concessions/data/amazonia", layer = "BassinAmazonien")
## change the projection system
amazonia <- spTransform(amazonia, CRS("+proj=longlat +datum=WGS84"))
plot(amazonia)
plot(all_flor, add = TRUE)
plot(all_fconc, add = TRUE, col=2)
plot(all_sconc, add = TRUE, col=3)

all_flor <- as(all_flor, "SpatialPolygonsDataFrame")
rgdal::writeOGR(all_flor, dsn = "data_sfb/final", layer = "BRflopub", 
                driver="ESRI Shapefile", overwrite_layer = TRUE)

all_conc <- as(all_conc, "SpatialPolygonsDataFrame")
rgdal::writeOGR(all_conc, dsn = "data_sfb/final", layer = "BRconc",
                driver="ESRI Shapefile", overwrite_layer = TRUE)
