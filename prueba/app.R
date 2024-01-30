

# App de prueba

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(bslib)
library(leaflet)
library(ggtext)
library(showtext)
library(rgeoda)

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

# Carga de datos
basedata <- read_delim("../resultados-por-departamento.txt", delim = "\t")
shapeData <- rgdal::readOGR(dsn = "../mapa/pxdptodatosok.shp", verbose = FALSE)
shapeData <- sp::spTransform(shapeData, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
shapeData <- sf::st_as_sf(shapeData, wkt = 'polygons', crs = st_crs(4326)) # make sf
shapeData <- rmapshaper::ms_simplify(shapeData, keep = 0.01, keep_shapes = TRUE)
shapeData <- shapeData %>% 
  select(provincia, departamen, geometry)

# Minuscula y sin tilde al mapa
shapeData$departamen <- tolower(iconv(shapeData$departamen, to = "ASCII//TRANSLIT"))
shapeData$provincia <- tolower(iconv(shapeData$provincia, to = "ASCII//TRANSLIT"))

# Elimino islas del atlántico sur
shapeData <- shapeData %>% 
  filter(departamen != "antartida argentina")

# Tablas para operar en la aplicación

# Tabla de nominales
df <- left_join(basedata, shapeData, by = c("provincia", "departamen"))

colnames(df) <- c("provincia", "departamen",
                  "Blanco",
                  "Fizq", "Hacemos",
                  "JxC", "LLA", 
                  "Nulo", "Otro", "UxP",
                  "total", "geometry")

df <- df %>% 
  mutate(no.validos = Blanco + Nulo + Otro) %>% 
  select(provincia, departamen, Fizq, Hacemos, JxC, LLA, UxP, no.validos, total, geometry)

df$total.validos <- apply(df[,3:7], MARGIN = 1, FUN = sum)

# Tabla de frecuencias
df.prop <- df %>%
  mutate(across(Fizq:UxP, ~ . / total.validos, .names = "prop_{.col}")) %>% 
  select(provincia, departamen, prop_Fizq, prop_Hacemos, prop_JxC, prop_LLA, prop_UxP)

df.prop$no.validos <- df$no.validos / df$total

df.prop <- left_join(df.prop, shapeData, by = c("provincia", "departamen"))

# Tabla de resultados generales (provisorios)
generales <- data.frame(totales = colSums(df[c(3:7,8)]))
generales$voto <- rownames(generales)
rownames(generales) <- NULL

totales.validos <- sum(generales$totales[generales$voto != "no.validos"])

for (i in 1:nrow(generales)-1) {
  generales$proporcion[i] = generales$totales[i] / totales.validos
}

generales <- generales %>% 
  mutate(porc = round(proporcion, 4)*100) %>% 
  select(voto, totales, porc)

generales[6, "porc"] <- generales$totales[generales$voto == "no.validos"] / sum(generales$totales)
generales$porc <- round(generales$porc, 2)

generales

# Tabla de ganadores
ganadores <- cbind(provincia = df$provincia, 
                   departamen = df$departamen, 
                   df[, 3:7] / df$total.validos)

ganadores <- ganadores %>% 
  pivot_longer(-c(provincia, departamen), names_to = "partido", values_to = "prop") %>% 
  group_by(provincia, departamen) %>%
  filter(prop == max(prop)) 

ganadores$partido <- case_when(
  ganadores$partido == "JxC" ~ "Juntos por el Cambio",
  ganadores$partido == "UxP" ~ "Union Por la Patria",
  ganadores$partido == "LLA" ~ "La Libertad Avanza",
  ganadores$partido == "Hacemos" ~ "Hacemos por Nuestro País"
)


# Define UI for application that draws a histogram
ui <- navbarPage("mapas de prueba",
                 theme = bs_theme(version = 5, bootswatch = "flatly"),
                 tabPanel("Indicadores",
                          sidebarLayout(
                            sidebarPanel(h4(strong("Opciones")),
                                         selectInput("Voto",
                                                     label = strong("Voto a "),
                                                     choices = list("Frente de Izquierda" = "prop_Fizq",
                                                                    "Hacemos" = "prop_Hacemos",
                                                                    "Juntos por el C." = "prop_JxC",
                                                                    "La Libertad A." = "prop_LLA",
                                                                    "Unión por la Pat." = "prop_UxP",
                                                                    "No validos" = "no.validos"),
                                                     selected = 4)),
                            mainPanel(
                              fluidRow(
                                column(6,
                                       plotOutput("mapa.calor",
                                                  height = "80vh",
                                                  width = "60vh")),
                                column(6,
                                       plotOutput("cluster.electoral",
                                                    height = "80vh",
                                                    width = "60vh"))
                              )
                            )
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  propmap <- df.prop[,1:8]
  propmap <- left_join(shapeData, propmap, by = c("provincia", "departamen"))
  queen_w <- queen_weights(propmap)
  new_palette <- c("#fcffff", "dodgerblue", "lightblue", "lightpink", "firebrick1", "black")
  
  
  # mapa de calor
  
  
  seleccion <- reactive({
    seleccion <- df.prop[, input$Voto]
    
  })
  
  output$mapa.calor <- renderPlot({
  
    plt <- df.prop %>% 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = seleccion()[[1]])) +
      coord_sf(xlim = c(-74, -53.3), ylim = c(-54.03,-23.2)) +
      labs(title = "") +
      scale_fill_continuous(low = "#161b33", high = "#C105F0") +
      labs(fill = "") +
      theme(legend.position = 'bottom') +
      guides(fill = guide_colorbar(title = "%",
                                   label.position = "bottom",
                                   title.position = "left", 
                                   title.vjust = 0.75,
                                   title.hjust = 0))
    plt
    
  })
  
  
  # Moran
  proporciones <- df.prop[, 1:8]
  propmap <- left_join(shapeData, proporciones, by = c("provincia", "departamen"))
  new_palette <- c("#fcffff", "dodgerblue", "lightblue", "lightpink", "firebrick1", "black")
  queen_w <- queen_weights(propmap)
  
  moran.reactivo <- reactive({
    
    moran <- propmap

    lisa <- local_moran(queen_w, propmap[, input$Voto])
    
    moran$cluster <- as.factor(lisa$GetClusterIndicators())
    levels(moran$cluster) <- lisa$GetLabels()
    
    moran
    
  })
  
  
  output$cluster.electoral <- renderPlot({
    
    plot <- moran.reactivo() %>% 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = cluster)) +
      coord_sf(xlim = c(-74, -53.3), ylim = c(-54.03,-23.2)) +
      scale_fill_manual(values = new_palette) +  # Aplicar la nueva paleta de colores
      labs(title = "Difusión espacial - Union por la patria") +
      labs(fill = "") +
      theme(legend.position = "right")
    
    plot
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
