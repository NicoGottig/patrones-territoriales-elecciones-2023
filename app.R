
# APP ELECCIONES PTE 2023

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
basedata <- read_delim("resultados-por-departamento.txt", delim = "\t")

shapeData <- rgdal::readOGR(dsn = "mapa/pxdptodatosok.shp", verbose = FALSE)
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

#### Interface ####

ui <- navbarPage("Patrones territoriales en las elecciones presidenciales 2023",
                
                theme = bs_theme(version = 5, bootswatch = "flatly"),
                
                tabPanel("Indicadores",
                  sidebarLayout(
                    sidebarPanel(h4(strong("Opciones")),
                                 
                                 # Opción de tipo de elección
                                 radioButtons("tipoeleccion",
                                              label = strong("Tipo de elección"),
                                              choices = list("Generales" = 1, 
                                                             "Segunda vuelta" = 2), 
                                              selected = 1),
                                 
                                 # Opción de tipo de elección
                                 radioButtons("Informacion",
                                              label = strong("Información provista: "),
                                              choices = list("Descripción general de votos" = 1,
                                                             "Clusters" = 2), 
                                              selected = 2),
                                 
                                 # Partido o tipo de voto
                                 conditionalPanel("input.tipoeleccion == 1",
                                 selectInput("Voto", 
                                             label = strong("Voto a "), 
                                             choices = list("Frente de Izquierda" = "prop_Fizq",
                                                            "Hacemos" = "prop_Hacemos", 
                                                            "Juntos por el C." = "prop_JxC",
                                                            "La Libertad A." = "prop_LLA",
                                                            "Unión por la Pat." = "prop_UxP",
                                                            "No validos" = "no.validos"), 
                                             selected = 4)
                                 ),
                                 
                                 conditionalPanel("input.tipoeleccion == 2",
                                 selectInput("Voto2",
                                             label = strong("Voto a"),
                                             choices = list("La Libertad A." = 1, 
                                                            "Union por la Pat." = 2,
                                                            "No validos" = 3)))

                                 ),
                    
                    mainPanel(
                    
                      conditionalPanel("input.Informacion == 1",
                                       
                      fluidRow(
                               column(6,
                                      plotlyOutput("barplot.generales", 
                                             height = "80vh",
                                             width = "60vh")),
                               column(6, 
                                      leafletOutput("mapa.ganadores", 
                                            height = "80vh",
                                            width = "60vh"))
                               )),
                    
                      conditionalPanel("input.Informacion == 2",
                                       
                      fluidRow(
                        column(6,
                               plotOutput("densidad.electoral",
                                          height = "80vh",
                                          width = "60vh")),
                        column(6,
                               plotOutput("cluster.electoral",
                                          height = "80vh",
                                          width = "60vh"))
                      )
                      )
                      )
                )),
                
                tabPanel("Acerca de",
                         sidebarLayout(
                           sidebarPanel("hola!"),
                           mainPanel("chau!")
                         ))

  
)

#### Servidor ####

server <- function(input, output) {

# Mapa de ganadores
  ganadores <- left_join(shapeData, ganadores, by = c("provincia", "departamen"))
  output$mapa.ganadores <- renderLeaflet({
    
    
    pal <- colorFactor(
      palette = c('#53684C', '#F1C93F', '#B87B89', '#60B9B5'),
      domain = ganadores$partido
    )
    
    textos <- paste0(str_to_title(ganadores$departamen), " - ", str_to_title(ganadores$provincia), 
                     "<br>", 
                     ganadores$partido, " (", 
                     round(ganadores$prop,2)*100, " %)")
    
    leaflet(ganadores) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(-66, -40.5, 4) %>%
      addPolygons(weight = 2,
                  color = "black",
                  dashArray = 1,
                  fillColor = ~pal(partido),
                  fillOpacity = .9,
                  highlight = highlightOptions(weight = 3,
                                               color = "black",
                                               fillOpacity = 0.9,
                                               dashArray = "",
                                               bringToFront = TRUE),
                  label = ~lapply(as.list(textos), htmltools::HTML),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"))
  })
  
# Barras de resultados generales
  output$barplot.generales <- renderPlotly({
    generales$voto <- case_when(
      generales$voto == "JxC" ~ "Juntos por el Cambio",
      generales$voto == "UxP" ~ "Union Por la Patria",
      generales$voto == "LLA" ~ "La Libertad Avanza",
      generales$voto == "Hacemos" ~ "Hacemos",
      generales$voto == "Fizq" ~ "Frente de Izq.",
      generales$voto == "no.validos" ~ "No val."
    )
    
    texto.generales <- paste0(generales$voto, " (", generales$porc, " %)")
    
    colores_partidos <- c(
      "firebrick",
      "#53684C",
      "#F1C93F",
      "#B87B89",
      "#60B9B5",
      "darkgray")
    
    plot_ly(data = generales, 
            y = ~reorder(voto, porc), 
            x = ~porc, 
            marker = list(color = colores_partidos),
            type = "bar",
            text = ~texto.generales,
            hoverinfo = "text",
            showlegend = FALSE) %>% 
      layout(yaxis = list(title = "", tickvals = list()),
             xaxis = list(title = "%"))
    
  })


# Distribución de votos por partido
  
  # Gráficos de densidad electoral
  
  seleccion <- reactive({
    seleccion <- df.prop[, input$Voto]
    
  })
  
  output$densidad.electoral <- renderPlot({
    
    brmax <- round(max(seleccion()[[1]])*100 - 0.5, 2)
    brmid <- round((max(seleccion()[[1]])*100)/2, 2)
    
    plt <- df.prop %>% 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = 100*seleccion()[[1]])) +
      coord_sf(xlim = c(-74, -53.3), ylim = c(-54.03,-23.2)) +
      labs(title = "VOTOS (EN % POR DEPARTAMENTO)") +
      scale_fill_continuous(low = "#161b33", high = "firebrick1") +
      labs(fill = "") +
      theme(legend.position = 'top',
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) +
      guides(fill = guide_colorbar(title = "%",
                                   label.position = "bottom",
                                   title.position = "left", 
                                   title.vjust = 0.75,
                                   title.hjust = 0))
    plt
    
    
  })
  

  # Gráficos de cluster
  
  proporciones <- df.prop[, 1:8]
  propmap <- left_join(shapeData, proporciones, by = c("provincia", "departamen"))
  new_palette <- c("#161b33", "dodgerblue", "lightblue", "lightpink", "firebrick1", "black")
  queen_w <- queen_weights(propmap)
  
  moran.reactivo <- reactive({
    
    moran <- propmap
    
    lisa <- local_moran(queen_w, propmap[, input$Voto])
    
    moran$cluster <- as.factor(lisa$GetClusterIndicators())
    levels(moran$cluster) <- lisa$GetLabels()
    
    # Cambiar los nombres de los factores
    moran$cluster <- factor(moran$cluster,
                              levels = c("Not significant", "Low-Low", "Low-High", "High-Low", "High-High", "Undefined"),
                              labels = c("Sin efecto", "Bajo-Bajo", "Bajo-Alto", "Alto-Bajo", "Alto Alto", "Indef."))
    
    moran
    
  })

  
  output$cluster.electoral <- renderPlot({
    
    plot <- moran.reactivo() %>% 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = cluster)) +
      coord_sf(xlim = c(-82, -48), ylim = c(-54.03,-23.2)) +
      scale_fill_manual(values = new_palette) +
      labs(fill = "",
           title = "CLUSTERS") +
      theme(legend.position = "top",
            axis.text.x = element_blank(),
            axis.text.y = element_blank())
    
    plot
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
