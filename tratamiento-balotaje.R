library(tidyverse)

df <- read.csv("../../../Desktop/presentacionDeResultados (1).csv")

# Tratamiento de datos de elecciones 
votos <- df %>% 
  filter(cargo_nombre == "PRESIDENTE Y VICE") %>% 
  select(distrito_nombre, seccion_nombre, circuito_id, mesa_id, mesa_electores, agrupacion_nombre, votos_tipo, votos_cantidad)

# Recategorizar votos 
votos$votos_tipo <- case_when(
  votos$votos_tipo %in% c("IMPUGNADO", "RECURRIDO", "COMANDO") ~ "OTRO",
  votos$votos_tipo == "POSITIVO" ~ "POSITIVO",
  votos$votos_tipo == "EN BLANCO" ~ "EN BLANCO",
  votos$votos_tipo == "NULO" ~ "NULO" 
)

votos$agrupacion_nombre <- ifelse(votos$agrupacion_nombre == "undefined", votos$votos_tipo, votos$agrupacion_nombre)

# Antes de agrupar, agrego los votos de tolhuin y lezema. 
votos$seccion_nombre[votos$seccion_nombre == "Tolhuin"] <- "rio grande"
votos$seccion_nombre[votos$seccion_nombre == "Lezama"] <- "la capital"


# Minuscula y tildes
votos$distrito_nombre <- tolower(iconv(votos$distrito_nombre, to = "ASCII//TRANSLIT"))
votos$seccion_nombre <- tolower(iconv(votos$seccion_nombre, to = "ASCII//TRANSLIT"))

# Resumo y hago el wider
votos <- votos %>% 
  group_by(distrito_nombre, seccion_nombre, agrupacion_nombre) %>% 
  summarise(votos_totales = sum(votos_cantidad))

votos_wider <- votos %>% 
  pivot_wider(id_cols = c(distrito_nombre, seccion_nombre), names_from = agrupacion_nombre, values_from = votos_totales)

head(votos_wider)

votos_wider$total <- apply(votos_wider[,3:7], MARGIN = 1, FUN = sum)
votos_wider$total.valido <- apply(votos_wider[,c(4,7)], MARGIN = 1, FUN = sum)

# Renombro columnas
colnames(votos_wider)[1] <- "provincia"
colnames(votos_wider)[2] <- "departamen"

# Modificar comunas y provincias distintas
votos_wider$departamen <- gsub("comuna 0", "comuna ", votos_wider$departamen)
votos_wider$provincia <- ifelse(votos_wider$provincia == "tierra del fuego, antartida e islas del atlantico sur", "tierra del fuego", votos_wider$provincia)

### Tratamiento de datos del mapa ####
# Carga de mapa por provincia y departamento
shape <- rgdal::readOGR(dsn = "mapa/pxdptodatosok.shp", verbose = FALSE)
shapeData <- sp::spTransform(shape, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
shapeData <- sf::st_as_sf(shapeData, wkt = 'polygons', crs = st_crs(4326)) # make sf
shapeData <- rmapshaper::ms_simplify(shapeData, keep = 0.01, keep_shapes = TRUE)
shapeData <- shapeData %>% 
  select(provincia, departamen, geometry)

# Minuscula y sin tilde al mapa
shapeData$departamen <- tolower(iconv(shapeData$departamen, to = "ASCII//TRANSLIT"))
shapeData$provincia <- tolower(iconv(shapeData$provincia, to = "ASCII//TRANSLIT"))

### Estudio diferencias en los conjuntos ####
setdiff(votos_wider$departamen, shapeData$departamen)

# Soluciono las diferencias
votos_wider$departamen <- ifelse(votos_wider$departamen == "coronel de marina l. rosales", "coronel de marina leonardo rosales", votos_wider$departamen)
votos_wider$departamen <- ifelse(votos_wider$departamen == "general juan facundo quiroga", "general juan f. quiroga", votos_wider$departamen)
votos_wider$departamen <- ifelse(votos_wider$departamen == "general ortiz de ocampo", "general ocampo", votos_wider$departamen)
votos_wider$departamen <- ifelse(votos_wider$departamen == "angel vicente penaloza", "general angel v. penaloza", votos_wider$departamen)
votos_wider$departamen <- ifelse(votos_wider$departamen == "juan bautista alberdi", "juan b. alberdi", votos_wider$departamen)
votos_wider$departamen <- ifelse(votos_wider$departamen == "juan martin de pueyrredon", "la capital", votos_wider$departamen)
votos_wider$departamen <- ifelse(votos_wider$departamen == "antartida argentina", "islas del atlantico sur", votos_wider$departamen)

# Chascomus esta con lezema
# Juan martin de pueyrredon está como "la capital"
# Tolhuin está en "rio grande"

# Estudio nuevamente las diferencias en los conjuntos
setdiff(votos_wider$departamen, shapeData$departamen)
setdiff(shapeData$departamen, votos_wider$departamen)

# Antartida argentina estaba contenido en tierra del fuego

# Elimino islas del atlántico sur
shapeData <- shapeData %>% 
  filter(departamen != "antartida argentina")

### fusionar datasets ####

# Votos nominales
df <- left_join(votos_wider, shapeData, by = c("provincia", "departamen"))

colnames(df)
colnames(df) <- c("provincia", "departamen", 
                  "Blanco", "LLA",
                  "NULO", "OTRO", 
                  "UxP", "total",
                  "total.valido", "geometry")

# Votos como porcentaje del total departamental
prop.df <- df %>% 
  mutate(UxP = UxP/total.valido,
         LLA = LLA/total.valido,
         No.Positivo = sum(NULO, OTRO, Blanco) / total)

prop.df

# Proporciones generales (para chequear)
generales <- df %>% 
  mutate(no.validos = Blanco + NULO + OTRO) %>% 
  select(provincia, departamen, no.validos, UxP, LLA, total, total.valido)

generales <- data.frame(totales = colSums(generales[c(3:7)]))
generales$voto <- rownames(generales)
rownames(generales) <- NULL

for (i in 2:nrow(generales)-1) {
  generales$proporcion[i] = generales$totales[i] / 25992604
}

generales[1, 3] <- 870642 / 26863246
generales <- generales[-c(5,4),]

generales

df <- df[, 1:9]

write_delim(df, "balotaje-por-depto-total.txt", delim = "\t")

prop.df <- prop.df[,1:9]
write_delim(prop.df, "balotaje-por-depto-porcent.txt", delim = "\t")

generales
write_delim(generales, "balotaje-general-pais.txt", delim = "\t")

# Dataframe de proporciones

# Gráfico
prop.df %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = UxP)) +
  coord_sf(xlim = c(-74, -53.3), ylim = c(-54.03,-23.2)) +
  labs(title = "La Libertad Avanza") +
  scale_fill_continuous(low = "#161b33", high = "#C105F0") +
  labs(fill = "") +
  theme(legend.position = 'bottom') +
  guides(fill = guide_colorbar(title = "%",
                               label.position = "bottom",
                               title.position = "left", 
                               title.vjust = 0.75,
                               title.hjust = 0))
