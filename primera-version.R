library(tidyverse)
library(leaflet)
library(sp)
library(ggtext)
library(showtext)
showtext_auto()
font_add_google(name="Noto Sans", family = "noto")

# Formato de gráficos
theme_set(
  theme_light(
    base_family = "noto") +
    theme(
      # legend.title = element_text(face = "bold"),
      # axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "right", 
      text = element_text(size=10),
      axis.text.x = element_text(size = 10),
      strip.text = element_text(color = "black"),
      strip.background = element_blank()
    )
)

#### GENERALES #####

### Carga y procesamiento de datos ####
data <- read.csv("../../../Desktop/2023/patrones espaciales elecciones/2023_Generales/ResultadoElectorales_2023_Generales.csv")
  
# Tratamiento de datos de elecciones 
votos <- data %>% 
  filter(cargo_nombre == "PRESIDENTE Y VICE") %>% 
  select(distrito_nombre, seccion_nombre, circuito_id, mesa_id, mesa_electores, agrupacion_nombre, votos_tipo, votos_cantidad)

# Recategorizar votos 
votos$votos_tipo <- case_when(
  votos$votos_tipo %in% c("IMPUGNADO", "RECURRIDO", "COMANDO") ~ "OTRO",
  votos$votos_tipo == "POSITIVO" ~ "POSITIVO",
  votos$votos_tipo == "EN BLANCO" ~ "EN BLANCO",
  votos$votos_tipo == "NULO" ~ "NULO" 
)

votos$agrupacion_nombre <- ifelse(nchar(votos$agrupacion_nombre) == 0, votos$votos_tipo, votos$agrupacion_nombre)

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

votos_wider$total <- apply(votos_wider[,3:10], MARGIN = 1, FUN = sum)
  
# Renombro columnas
colnames(votos_wider)[1] <- "provincia"
colnames(votos_wider)[2] <- "departamen"

# Modificar comunas y provincias distintas
votos_wider$departamen <- gsub("comuna 0", "comuna ", votos_wider$departamen)
votos_wider$provincia <- ifelse(votos_wider$provincia == "tierra del fuego, antartida e islas del atlantico sur", "tierra del fuego", votos_wider$provincia)

### Tratamiento de datos del mapa ####
# Carga de mapa por provincia y departamento
shape <- rgdal::readOGR(dsn = "mapa/pxdptodatosok.shp", verbose = FALSE)
shapeData <- spTransform(shape, CRS("+proj=longlat +datum=WGS84 +no_defs"))
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

# Guardo votos wider
write_delim(votos_wider, "resultados-por-departamento.txt", delim = "\t")

# Elimino islas del atlántico sur
shapeData <- shapeData %>% 
  filter(departamen != "antartida argentina")

### fusionar datasets ####

# Votos nominales
df <- left_join(votos_wider, shapeData, by = c("provincia", "departamen"))

df <- df %>% 
  select(provincia, departamen, 
         `FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD`,
         `HACEMOS POR NUESTRO PAIS`, 
         `JUNTOS POR EL CAMBIO`,
         `LA LIBERTAD AVANZA`, 
         `NO POSITIVO`,
         `UNION POR LA PATRIA`, total, geometry)

colnames(df)
colnames(df) <- c("provincia", "departamen", 
                  "Fizq", "Hacemos",
                  "JxC", "LLA", 
                  "NOPositivo", "UxP",
                  "total", "geometry")

# Votos como porcentaje del total departamental
prop.df <- df %>%
  mutate(across(Fizq:UxP, ~ . / total, .names = "perc_{.col}")) %>% 
  select(provincia, departamen, perc_Fizq, perc_Hacemos, perc_JxC, perc_LLA, perc_NOPositivo, perc_UxP)

# Dataframe de proporciones

# Gráfico
votoslla <- df %>%
  mutate(prop = round(100 * LLA/total, 2)) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) +
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

votosuxp <- df %>%
  mutate(prop = round(100 * UxP/total, 2)) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) +
  coord_sf(xlim = c(-74, -53.3), ylim = c(-54.03,-23.2)) +
  labs(title = "Votos a Unión por la Patria") +
  scale_fill_continuous(low = "#161b33", high = "dodgerblue2") +
  labs(fill = "") +
  theme(legend.position = 'bottom') +
  guides(fill = guide_colorbar(title = "%",
                               label.position = "bottom",
                               title.position = "left", 
                               title.vjust = 0.75,
                               title.hjust = 0))

votosjxc <- df %>%
  mutate(prop = round(100 * JxC/total, 2)) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) +
  coord_sf(xlim = c(-74, -53.3), ylim = c(-54.03,-23.2)) +
  labs(title = "Juntos por el Cambio") +
  scale_fill_continuous(low = "#161b33", high = "orange") +
  labs(fill = "") +
  theme(legend.position = 'bottom') +
  guides(fill = guide_colorbar(title = "%",
                               label.position = "bottom",
                               title.position = "left", 
                               title.vjust = 0.75,
                               title.hjust = 0)) 

# Imprimo plots juntos
library(cowplot)
plot_grid(votosuxp, votoslla, votosjxc, nrow = 1, align = "v")

#### ¿Quién ganó en cada departamento? ####
ganadores <- cbind(provincia = votos_wider$provincia, departamen = votos_wider$departamen, votos_wider[, 3:8] / votos_wider$total)

ganadores <- ganadores %>% 
  pivot_longer(-c(provincia, departamen), names_to = "partido", values_to = "prop") %>% 
  group_by(provincia, departamen) %>%
  filter(prop == max(prop)) 

# añado al mapa
df.ganadores <- left_join(ganadores, shapeData, by = c("provincia", "departamen")) 

partidos_colores <- c(
  "JUNTOS POR EL CAMBIO" = "orange",
  "UNION POR LA PATRIA" = "dodgerblue",
  "LA LIBERTAD AVANZA" = "purple",
  "HACEMOS POR NUESTRO PAIS" = "darkgreen"
)

df.ganadores %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = partido)) +
  coord_sf(xlim = c(-74, -53.3), ylim = c(-54.03,-23.2)) +
  labs(title = "Partido ganador por departamento") +
  scale_fill_manual(values = partidos_colores) 


### Análisis de influencia vecina con mapa lisa ####
library(rgeoda)

# atach con proporciones
propmap <- left_join(shapeData, prop.df, by = c("provincia", "departamen"))

# Matriz de pesos espaciales
queen_w <- queen_weights(propmap)

# Indice local para U x P
lisa <- local_moran(queen_w, propmap["perc_Fizq"])

# Añado los clusters al dataframe original
propmap$cluster <- as.factor(lisa$GetClusterIndicators())
levels(propmap$cluster) <- lisa$GetLabels()
  
# Cambiar los nombres de los factores
propmap$cluster <- factor(propmap$cluster,
                               levels = c("Not significant", "Low-Low", "Low-High", "High-Low", "High-High", "Undefined"),
                               labels = c("Sin efecto", "Bajo-Bajo", "Bajo-Alto", "Alto-Bajo", "Alto Alto", "Indef."))

# Definir la nueva escala de colores
new_palette <- c("#fcffff", "dodgerblue", "lightblue", "lightpink", "firebrick1", "black")
  
propmap %>%
    ggplot() +
    geom_sf(aes(geometry = geometry, fill = cluster)) +
    coord_sf(xlim = c(-74, -53.3), ylim = c(-54.03,-23.2)) +
    scale_fill_manual(values = new_palette) +  # Aplicar la nueva paleta de colores
    labs(title = "Difusión espacial - Union por la patria") +
    labs(fill = "") +
    theme(legend.position = "right")

