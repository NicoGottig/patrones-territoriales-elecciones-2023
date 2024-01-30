# pruebas

ganadores <- left_join(shapeData, ganadores, by = c("provincia", "departamen"))

pal <- colorFactor(
  palette = c('#53684C', '#F1C93F', '#B87B89', '#60B9B5'),
  domain = ganadores$partido
)

textos <- paste0(str_to_title(ganadores$departamen), " - ", str_to_title(ganadores$provincia), ")", 
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


### ganadores generales
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


# plots de densidad electoral
df.prop %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop_Fizq)) +
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

# plots de cluster

queen_w <- queen_weights(propmap)

### Matrices de indice local por partido
lisa.fiq <- local_moran(queen_w, propmap["prop_Fizq"])
lisa.hacemos <- local_moran(queen_w, propmap["prop_Hacemos"])
lisa.jxc <- local_moran(queen_w, propmap["prop_JxC"])
lisa.lla <- local_moran(queen_w, propmap["prop_LLA"])
lisa.uxp <- local_moran(queen_w, propmap["prop_UxP"])
lisa.nv <- local_moran(queen_w, propmap["no.validos"])



