######################################################
###########bajar lista de especies grandes https://data-blog.gbif.org/post/downloading-long-species-lists-on-gbif/
user <- "username" # your gbif.org username 
pwd <- "password" # your gbif.org password
email <- "email" # your email 

#No es necesario crear esos objetos si luego abajo, en occ_download, los pongo con comillas


# Cargamos las librerias

library(readxl)
library(dplyr)
library(purrr)
library(readr)  
library(magrittr) # for %T>% pipe
library(rgbif) # for occ_download
library(taxize) # for get_gbifid_
library(CoordinateCleaner)

########### DESCARGA DE DATOS ############

# cargamos el fichero donde estan los nombres que vamos a utilizar
base_datos_flora_PN <- read_csv("base_datos_flora_PN.csv")

#Creo una copia de la base de datos para mantenerla intacta
flora_rupicola <- base_datos_flora_PN


# match the names
gbif_taxon_keys <- 
  readr::read_csv("C:/Users/CARLOS/OneDrive/Escritorio/TFM/base_datos_flora_PN.csv") %>% 
  taxize::get_gbifid_(method="backbone") %>% # match names to the GBIF backbone to get taxon keys
    # Esto nos genera una estandarizacion de nombres, cada sp que le damos un nombre nos da tambien todos
    # los nombres potenciales, asi que si nuestra lista es de 1400 sp, la descarga es probable que sea de mas
  imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back into data.frame
  bind_rows() %T>% # combine all data.frames into one
  readr::write_tsv(path = "all_matches.tsv") %>% # save as side effect for you to inspect if you want
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted and matched names
  filter(kingdom == "Plantae") %>% # remove anything that might have matched to a non-plant
  pull(usagekey) # get the gbif taxonkeys

View(gbif_taxon_keys)

# Crear la descarga en gbif
occ_download(
  pred_in("taxonKey", gbif_taxon_keys),
  format = "SIMPLE_CSV",pred("country", "ES"),
  user="xxxx",pwd="xxxx",email="xxxx")


#The results should now be on your downloads user page https://www.gbif.org/user/download
#CITATION: GBIF.org (01 December 2023) GBIF Occurrence Download  https://doi.org/10.15468/dl.e9yssm

# Leemos csv de GBIF, importante comprobar que se incluyen todos los datos.

gbif <- read.delim("species_list/gbif_data_PN.csv", quote = '') #Funcion read.delim

# Creo un archivo igual al bajado de gbif para conservarlo, y hacer las pruebas y cambios de variables con el otro
gbif_safe       <- gbif 




####### DATOS DE LA FLORA RUPICOLA #########

# A?ado los datos de la flora de PN con la informaci?n de ?ndice de rup?colas, eliminando las columnas de todos los expertos
# OJO, el archivo base_datos_flora_PN ahora es este dataframe, no el .csv con tan solo los nombres de las especies para bajarlas de gbif
library(readxl)
base_datos_flora_PN <- read_excel("base_datos_flora_PN.xlsx", 
                                    +     sheet = "data_filter")
View(base_datos_flora_PN)

# ordeno mis dataframes por orden alfab?tico de la especie
base_datos_flora_PN <- base_datos_flora_PN[order(base_datos_flora_PN$Scientific_name), ]
base_datos_flora_PN[order(base_datos_flora_PN$Scientific_name_subsp),]
View(base_datos_flora_PN)

gbif <- gbif[order(gbif$species), ]
View(gbif)

#Uno el nombre cientifico con la subespecie en el archivo de gbif en un nuevo campo llamado species_sub


gbif$species_sub <- ifelse(is.na(gbif$infraspecificEpithet),
                           gbif$species,
                           paste(gbif$species, gbif$infraspecificEpithet))

#Reordeno las variables para tener el nuevo nombre al lado del Scientific_name
gbif        <- gbif[, c(1:11, 51, 12:50)] 


######## FILTROS DE DATOS por su posicion y calidad de georreferenciacion ######

# Coordenadas de la Pen?nsula Ibirica: -10 y 44, -10 y 36, 4 y 44, 4 y 36
library(dplyr)
gbif_geo <- filter(gbif, 
                         decimalLatitude < 44 & decimalLatitude > 36 & 
                         decimalLongitude < 4.6 & decimalLongitude > -10)
#Recortamos 165.040 observaciones

length(unique(gbif_geo$species_sub))

library(devtools)
library(usethis)
library(countrycode)
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(rgbif)
library(sf)


#Analizo datos problematicos

flags_gbif_geo <- clean_coordinates(x = gbif_geo)
summary(flags_gbif_geo)

#The flagged records
flags      <- gbif_geo[!flags_gbif_geo$.summary,]

write.table(flags, file="flags.txt", sep="\t", row.names=FALSE)

#Detectamos 67.726 observaciones problematicas

#Excluimos los datos analizados que son aparentemente problem?ticos

gbif_clean <- gbif_geo[flags_gbif_geo$.summary,]

sum(is.na(gbif_clean$`decimalLatitude`))
length(unique(gbif_clean$species_sub))

#Es un archivo con muchas columnas, mejor eliminar alguna para que sea menos pesado.
# Especifica las columnas que deseas eliminar
columnas_a_eliminar <- c(2:7, 13, 15, 16, 17, 20:22, 25, 26, 28:30, 31:34, 37:51)

# Elimina las columnas del gbif_clean

gbif_clean_2 <- gbif_clean[, -columnas_a_eliminar]
rm(gbif_clean_2)




######### HOMOGENIZACION DE NOMBRES ############

### Uno datos de gbif con los datos de la flora rupicola a partir de los campos de nombres

# 1. corrijo los espacios en blanco de gbif_clean_2
library(stringr)
gbif_clean_2$species_sub <- str_trim(gbif_clean_2$species_sub)
str(gbif_clean_2)

# 2. corrijo los espacios en blanco de base_datos_flora_PN (habia dos espacios entre especie y subespecie)
base_datos_flora_PN$Scientific_name_subsp <- gsub(" +", " ", base_datos_flora_PN$Scientific_name_subsp)
str(base_datos_flora_PN)

# 2.1. Corrijo un error en base_datos_flora_PN
base_datos_flora_PN$Scientific_name <- gsub("iberis spathulata", "Iberis spathulata", base_datos_flora_PN$Scientific_name)
base_datos_flora_PN$scientificName <- gsub("iberis spathulata", "Iberis spathulata", base_datos_flora_PN$scientificName)

# 3. Hago la fusion de los dos archivos.
#Aqui tengo que intentar que me coincida con species y con species_sub.
gbif_clean_2_rupicolas       <- merge(gbif_clean_2, base_datos_flora_PN, 
                                      by.x = "species_sub", by.y = "species_sub", 
                                      all.x = TRUE)
View(gbif_clean_2_rupicolas)
length(unique(gbif_clean_2_rupicolas$species_sub))

#Elimino algunas variables para que el archivo sea menos pesado
columnas_a_eliminar_rupicolas <- c(8, 9, 16, 17)
gbif_RUPICOLAS <- gbif_clean_2_rupicolas[, -columnas_a_eliminar_rupicolas]

# 4. Analizo los datos que no se han unido bien 

sum(is.na(gbif_RUPICOLAS$`mean aff. Index`))

# son datos de subespecies que no tenemos en nuestra base de datos en realidad

# Filtrar las filas con NA en "mean aff. Index" y mostrar las especies correspondientes
na_species <- gbif_RUPICOLAS$species_sub[is.na(gbif_RUPICOLAS$`mean aff. Index`)]
str(na_species)
# Imprimir resultados
cat("Número de NAs en 'mean aff. Index':", na_species, "\n") #observaciones NA
c(cat("Especies correspondientes:", unique(na_species), "\n")) #Especies NA
length(unique(na_species))


# Especies que quedan fuera del estudio y estan dentro del listado rupicolas
diff_elements <- setdiff(unique(base_datos_flora_PN$species_sub), common_elements)
diff_elements # Especies que no estan en nuestro archivo gbif_RUPICOLAS_TRUE
length(diff_elements)

diff_elements_df <- as.data.frame(diff_elements)
diff_elements_df <- merge(diff_elements_df, base_datos_flora_PN[, c("species_sub", "UICN_es", "mean aff. Index")], by.x = "diff_elements", by.y = "species_sub", all.x = TRUE)

sum(is.na(diff_elements_df$UICN_es))
sum(diff_elements_df$`mean aff. Index` >= 4, na.rm = TRUE)

diff_elements_CR_EN <- subset(diff_elements_df, UICN_es == 'CR' | UICN_es == 'EN')
view(diff_elements_CR_EN)

# CONCLUSION:
#los datos que no se han unido bien son de especies cuyo nombre no coincide exactamente
#con nuestra base de datos. Son o subespecies que no se han incluido en nuestra base de datos.
#Las especies que no estan en el gbif_RUPICOLAS_TRUE son por eso o porque sus datos los ha
#eliminado el paquete CoordinateCleaner (aprox 300 especies de la base_datos_flora)

# creo un subset de gbif_RUPICOLAS donde tan solo esten los datos con nombre correcto
gbif_RUPICOLAS_TRUE <- subset(gbif_RUPICOLAS, !is.na(ID))

# Lo analizo y veo si falta info del Index o si algun nombre no coincide con base-datos_flora_PN

sum(is.na(gbif_RUPICOLAS_TRUE$`mean aff. Index`))

length(unique(gbif_RUPICOLAS_TRUE$species_sub))
length(unique(base_datos_flora_PN$species_sub))

# 6.1 Analizo si los nombres todos de gbif_RUPICOLAS_TRUE coinciden con base_datos_flora_PN
# Identificar elementos comunes
common_elements <- intersect(unique(gbif_RUPICOLAS_TRUE$species_sub), unique(base_datos_flora_PN$species_sub))

# Contar elementos comunes
count_common <- length(common_elements)
print(count_common)

# Identificar elementos en gbif_RUPICOLAS_TRUE que no están en base_datos_flora_PN
diff_elements <- setdiff(unique(base_datos_flora_PN$species_sub), common_elements)
diff_elements # Especies que no estan en nuestro archivo gbif_RUPICOLAS_TRUE
length(diff_elements)

# Contar diferencias
count_diff <- length(diff_elements)
print(count_diff)

# 7. Repito paso 6 pero tan solo para gbif_RUPICOLAS, ahora deberia haber diferencias

# Hay 939 diferencias (especies), que corresponden a 112982 observaciones de especies donde el nombre no coincide.
# Siguientes pasos, trabajar con gbif_RUPICOLAS o gbif_RUPICOLAS_TRUE, segun lo que queramos hacer

write.table(gbif_RUPICOLAS_TRUE, file="gbif_RUPICOLAS_TRUE.txt", sep="\t", row.names=FALSE)

# Me quedo con las especies rupicolas y alta afinidad
gbif_RUPICOLAS_aff <- gbif_RUPICOLAS_TRUE[gbif_RUPICOLAS_TRUE$`mean aff. Index` >= 4, ]
write.table(gbif_RUPICOLAS_aff, file="gbif_RUPICOLAS_aff.txt", sep="\t", row.names=FALSE)





########### AJUSTE DE COORDENADAS ################

library(sf)
library(dplyr)

str(gbif_RUPICOLAS_TRUE)

# Crea un objeto sf a partir de tu dataframe
gbif_sf <- st_as_sf(gbif_RUPICOLAS_TRUE, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Transforma a UTM Zone 30N (EPSG:25830)
gbif_sf_utm <- st_transform(gbif_sf, crs = 25830)

# Verifica los resultados
head(gbif_sf_utm)

gbif_sf_RUPICOLAS_aff <- gbif_sf_utm[gbif_RUPICOLAS_TRUE$`mean aff. Index` >= 4, ]

# Guardar como Shapefile
st_write(gbif_sf_utm, "GBIF_UTM.shp")
st_write(gbif_sf_RUPICOLAS_aff, "GBIF_rupicolas_UTM.shp")

length(unique(gbif_sf_utm$species_sub))
length(unique(gbif_sf_RUPICOLAS_aff$species_sub))
# El resultado es 207 para rupicolas, divido la riqueza de especies entre eso en cada cuadricula

# El resultado es 1116 para todas las especies, divido la riqueza de especies entre eso en cada cuadricula
    # Ver por que se pierden algunas de las 1398 iniciales
    # Ver calculo de especies en Espana


### Especies fuera de ENPs
unique_species <- as.data.frame(unique(gbif_sf_utm$species_sub))
unique_species
str(unique_species)

species_in_ENPs <- read.csv("C:/Users/carlo/UNI_CARLOS_ECED/TFM/CARTOGRAFIA/GBIF_data/Outputs_FINAL/species_in_ENPs.csv")
str(species_in_ENPs)

# Calcula la diferencia para ver species fuera de ENPs

# Extraer los nombres de las especies de los dataframes, porque con data.frame no servia
unique_species <- unique_species$`unique(gbif_sf_utm$species_sub)`
species_in_ENPs <- species_in_ENPs$spcs_sb

str(unique_species)

# Calcular la diferencia
species_out_ENPs <- as.data.frame(setdiff(unique_species, species_in_ENPs))
species_out_ENPs

# Unir la información de la columna `UICN_es` al dataframe `species_difference_df`
species_out_ENPs_1 <- merge(species_out_ENPs, base_datos_flora_PN, by.x = "setdiff(unique_species, species_in_ENPs)", by.y = "species_sub", all.x = TRUE)


# Eliminar las columnas especificadas del dataframe species_out_ENPs_1
species_out_ENPs_1 <- subset(species_out_ENPs_1, select = -c(2:13, 15:21))

# Exporto un csv
write.csv(species_out_ENPs_1, file = "species_out_ENPs_1.csv")