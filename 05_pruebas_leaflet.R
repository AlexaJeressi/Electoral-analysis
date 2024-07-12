library(leaflet)
library(sf)
library(tidyverse)
library(cartography)
library(htmlwidgets)
library(htmltools)
library(readr)
library(scales)
library(highcharter)

### Datos espaciales
shp_ayuntamientos <- st_read(dsn = "~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/15-MUNICIPIOS", layer = "MUNICIPIO")%>%
  rename(id_municipio_R = MUNICIPIO)
shp_ayuntamientos <- st_transform(shp_ayuntamientos, "+init=epsg:4326") ## Para que corra el leaflet


shp_secciones <- st_read(dsn = "~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/15_secciones", layer = "secciones_nuevo2")
shp_secciones <- st_transform(shp_secciones, "+init=epsg:4326") ## Para que corra el leaflet
secciones_data <- as.data.frame(shp_secciones)%>%
  select(MUNICIPIO,SECCION,DISTRITO_L)


shp_local <- st_read(dsn= "~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/15-MEXICO", layer = "DISTRITO_LOCAL")%>%
  left_join(secciones_data, by = "DISTRITO_L")%>%
  rename(id_municipio_R = MUNICIPIO,
         id_seccion = SECCION,
         id_distrito_R = DISTRITO_L)%>%
  mutate(id_municipio_R = as.numeric(id_municipio_R),
         id_seccion = as.numeric(id_seccion),
         id_distrito_R = as.numeric(id_distrito_R))
shp_local<- st_transform(shp_local, "+init=epsg:4326") ## Para que corra el leaflet



### Bases de datos 
ayuntamientos <- read_csv("~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Bases/ayuntamientos_general.csv")
diputaciones <- read_csv("~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Bases/diputaciones_general.csv")
gobernador <- read_csv("~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Bases/gobernador_general.csv")


## Acomodar bases
ayuntamientos = ayuntamientos%>%
  gather("partido","votos",-c(id_seccion,no_registrados,nulos,votos_validos,
                              total,lista_nominal,ano,tipo,id_entidad_R,id_municipio_R,
                              id_distrito_R))%>%
  mutate(id_seccion = as.numeric(id_seccion),
         id_municipio_R = as.numeric(id_municipio_R),
         id_distrito_R = as.numeric(id_distrito_R))


diputaciones = diputaciones%>%
  gather("partido","votos",-c(id_seccion,no_registrados,nulos,
                              total,lista_nominal,ano,tipo,id_entidad_R,id_municipio_R,
                              id_distrito_R))%>%
  mutate(id_seccion = as.numeric(id_seccion),
         id_municipio_R = as.numeric(id_municipio_R),
         id_distrito_R = as.numeric(id_distrito_R))

gobernador = gobernador%>%
  gather("partido","votos",-c(id_seccion,nulos,
                              total,lista_nominal,ano,tipo,id_entidad_R,id_municipio_R,
                              id_distrito_R))%>%
  mutate(id_seccion = as.numeric(id_seccion),
         id_municipio_R = as.numeric(id_municipio_R),
         id_distrito_R = as.numeric(id_distrito_R))



# Paleta de colores por partido
#pri_colours <- c("#2B9348", "#449E58", "#5DAA68", "#75B578", "#8EC188", "#A7CC98", "#C0D7A8", "#D8E3B8", "#F1EEC8")
pri_colours <- c( "#8EC188","#75B578","#5DAA68","#449E58","#2B9348")

pan_colours <- c("#798C9A","#5A738E","#3C5A82","#1E4277","#00296B")

#prd_colours <- c("#FDC500", "#FCCA19", "#FACF32", "#F9D44B", "#F7DA64", "#F6DF7D", "#F4E496", "#F3E9AF", "#F1EEC8")
prd_colours <- c("#F7DA64","#F9D44B","#FACF32","#FCCA19", "#FDC500")

#pt_colours <- c("#C1121F", "#C72E34", "#CD4949", "#D3655E", "#D98074", "#DF9C89", "#E5B79E", "#EBD3B3", "#F1EEC8")
pt_colours <- c("#D98074","#D3655E","#CD4949","#C72E34","#C1121F")

#mc_colours <- c("#F77F00", "#F68D19", "#F69B32", "#F5A94B", "#F4B764", "#F3C47D", "#F3D296", "#F2E0AF", "#F1EEC8")
mc_colours <- c("#F4B764","#F5A94B", "#F69B32","#F68D19","#F77F00")

#morena_colours <- c("#81171B", "#8F3231", "#9D4D46", "#AB685C", "#B98372", "#C79D87", "#D5B89D", "#E3D3B2", "#F1EEC8")
morena_colours <- c("#B98372","#AB685C", "#9D4D46","#8F3231","#81171B")

#pvem_colours <- c("#80B918", "#8EC02E", "#9CC644", "#AACD5A", "#B9D470", "#C7DA86", "#D5E19C", "#E3E7B2", "#F1EEC8")
pvem_colours <- c( "#B9D470","#AACD5A","#9CC644","#8EC02E","#80B918")


## Crear paleta 
palpri <- colorNumeric(pri_colours,domain = prueba1$porcentaje)
palpan <- colorNumeric(pan_colours,domain = prueba1$porcentaje)
palprd <- colorNumeric(prd_colours,domain = prueba1$porcentaje)
palpt <- colorNumeric(pt_colours,domain = prueba1$porcentaje)
palmc <- colorNumeric(mc_colours,domain = prueba1$porcentaje)
palmorena <- colorNumeric(morena_colours,domain = prueba1$porcentaje)
palpvem <- colorNumeric(pvem_colours,domain = prueba1$porcentaje)



## Ejemplo shp ayuntamientos.
## 2021 --Filter
## PAN --Filter
## NIVEL municipio -- Es el group_by()


prueba1 <- ayuntamientos%>%
  filter(ano == 2021,
         partido == "pan")%>%
  group_by(id_municipio_R,)%>%
  summarise(votos = sum(votos),
            lista = sum(lista_nominal),
            porcentaje = round((votos/lista)*100),2)%>%
  left_join(shp_ayuntamientos, by = "id_municipio_R")%>%
  st_as_sf%>%
  na.omit()
  
  


leaflet() %>%
  addTiles() %>%
  addPolygons(data = na.omit(prueba1),
    color = "grey",weight = 0.6,
    smoothFactor = 0.1, fillOpacity = 1,
    fillColor = ~ binpal(prueba1$porcentaje),
    label = paste(
      "Municipio:",
      prueba1$NOMBRE, 
      "<br>",
      "Porcentaje:",
      paste0(prueba1$porcentaje,"%"),
      "<br>"
       )%>%
      lapply(htmltools::HTML),
    popup = paste(
      "Votos:",
      prueba1$votos,
      "<br>",
      "Distrito:",
      prueba1$id_distrito_R, 
      "<br>")
    %>%
    lapply(htmltools::HTML),
    )%>%
  addLegend(data = na.omit(prueba1), position =  "bottomleft", pal = binpal,
            values = ~porcentaje, title = "Votos", opacity = 1,
            group = "Leyenda")





###------------------------------- GRAFICA EN EL TIEMPO ------------------------

## Seleccionar lo 
prueba2 <- ayuntamientos%>%
  filter(partido == "pan")%>%
  group_by(id_municipio_R,ano)%>%
  summarise(
    votos = sum(votos),
            lista = sum(lista_nominal),
            porcentaje = round((votos/lista)*100),2)%>%
  left_join(shp_ayuntamientos, by = "id_municipio_R")%>%
  as.data.frame()


prueba2 %>%
  hchart("line", hcaes(x=ano,y=porcentaje,group=NOMBRE), 
         showInLegend =F, dataLabels=list(enabled=F),
         marker = list(enabled= FALSE)) %>% 
  hc_title(text = "<b>Porcentaje de votos</b>",
           margin = 20, align = "center",
           style = list(color = "gray80", useHTML = TRUE)) %>% 
  hc_yAxis(title = list(text = "")) %>% 
  hc_xAxis(title = list(text = "Fecha"))





### GRAFICA  --------------------------
library(highcharter)
library(tidyverse)
edomex <- read_csv("~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_dos/agrupados_15.csv")
edomex = edomex%>%
  rename(id_seccion = seccion)%>%
  mutate(sex_name = ifelse(sexo == 0,"Hombres","Mujeres"))
# 1. Grafica de pay 
# Para el agrupado de la las duras del PAN

try1 <- full_join(a_pan,edomex, by= "id_seccion")

try2 <- try1%>%
  group_by(sex_name)%>%
  summarise(total = sum(ln))%>%
  mutate(porcentaje = round((total/sum(total))*100))


try2%>%
  hchart("pie", 
         hcaes(x = sex_name, y = porcentaje),
  name = "Porcenataje"
)



# Grafica 2 PEA y POBTOT ----------------------------
barras = edomex%>%
  select(id_seccion,p_18ymas_f,p_18ymas_m,pea,pea_f,pea_m)%>%
  unique()%>%
  summarise(Hombres = (sum(pea_m)/sum(pea))*100,
            Mujeres = (sum(pea_f)/sum(pea))*100)%>%
  gather("nombre","valor")

barras %>% 
  hchart('column', hcaes(x = 'nombre', y = 'valor', group = 'nombre'),pointWidth = 90) %>%
  hc_colors(c("#FFFFCC","#B10026"))%>%
  hc_xAxis(title = list(text = ""))%>%
  hc_yAxis(title = list(text = "Porcentaje"))%>%
  hc_title(text= "Secciones por estrato") %>% 
  #hc_subtitle(text= "Top 10 countries by population") %>% 
  #hc_caption(text= "Based on year 2007 population")%>% 
  hc_add_theme(hc_theme_bloom())%>%
  hc_legend(enabled= FALSE)


# Grafica 3. Piramide de edad -----------------------------
#install.packages(c('XML', 'reshape2', 'devtools', 'plyr'))
library(devtools)

#install_github('ramnathv/rCharts@dev')

try3 = edomex%>%
  group_by(sex_name,nombre_rango)%>%
  summarise(total = sum(sv))%>%
  group_by(sex_name)%>%
  #mutate(porcentaje = round((total/sum(total))*100))%>%
  pivot_wider(names_from = nombre_rango,values_from = total)%>%
  mutate(sex_name = as.factor(sex_name))


highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Votantes por grupos de edad y sexo") %>% 
  hc_plotOptions(column = list(
    dataLabels = list(enabled = FALSE),
    stacking = "normal",
    enableMouseTracking = TRUE)
  ) %>% 
  hc_series(list(name="18 a 29 años",data=try3$`18 a 29 años`),
            list(name="30 a 39 años",data=try3$`30 a 39 años`),
            list(name="40 a 49 años",data=try3$`40 a 49 años`),
            list(name="50 a 59 años",data=try3$`50 a 59 años`),
            list(name="60 y mas años",data=try3$`60 y mas años`))%>%
  hc_xAxis(categories = c("Hombres","Mujeres"))%>%
  hc_yAxis(title = list(text = "Total"))


## Cuadros de informacion 
fluidRow(
  valueBox(
    inputId = "nivel_nse",
    "—", "NSE",
    color = "purple",
    #icon = icon("comment-dots"),
    width = dashboard_box_size),
  valueBox(
    inputId = "GPE",
    "—", "Escolaridad",
    color = "purple",
    #icon = icon("comment-dots"),
    width = dashboard_box_size),
  valueBox(
    inputId = "tot_ln",
    "—", "Total lista nominal",
    color = "purple",
    #icon = icon("comment-dots"),
    width = dashboard_box_size)
  
)


# NSE
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}




output$caja_escolaridad <- renderValueBox({
  base_agrupado <- base_agrupado()
  grape <-
    base_agrupado%>%
    select(id_seccion,graproes)%>%unique()%>% # Lo del censo necesita unique()
    summarise(round(sum(graproes)/n()))
  
  escolaridad <- valueBox(
    value = tags$p(comma(grape), style = "font-size: 70%;"),
           subtitle = tags$p("Escolatidad", style = "font-size: 100%;"),
           #icon= icon("procedures",class = "small_icon_test"),
           color = "red")

  return(escolaridad)
})




observe({
  grape <-
    edomex%>%
    select(id_seccion,graproes)%>%unique()%>% # Lo del censo necesita unique()
    summarise(round(sum(graproes)/n()))
  updateBoxValue(session, "GPE", grape)
})

observe({
  nivel_se <-
    edomex%>%
    select(id_seccion,nivel)%>%unique()%>% # Lo del censo necesita unique()
    summarise(Mode(nivel))
  updateBoxValue(session, "nivel_nse", nivel_se)
})


observe({
  tot_ln <-
    edomex%>%
    select(id_seccion,ln)%>%
    summarise(sum(ln))%>%
    prettyNum(.,big.mark = ",", scientific = FALSE)
   
  updateBoxValue(session, "tot_ln", tot_ln)
})




