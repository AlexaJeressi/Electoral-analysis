library(forecast)
library(tseries)
library(tidyverse)
library(gtools)

# Cargar los datos de elecciones 
dir <- "/Users/alexa/Library/CloudStorage/OneDrive-NextGateResearch/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Bases/"

ayuntamientos <- read_csv(paste0(dir,"ayuntamientos_general.csv"))
diputaciones <- read_csv(paste0(dir,"diputaciones_general.csv"))
gobernador <- read_csv(paste0(dir,"gobernador_general.csv"))
participacion <- read_csv(paste0(dir,"participacion_general.csv"))




# FUNCION PARA AYUNTAMIENTOS -------------------------------------

ids <- ayuntamientos$id_seccion%>% unique()
data_all <- data.frame(ids)%>%
  rename(j = ids)

data = c()

partidos <- c("pan","pri","prd","pt","pvem","mc","morena","nva","pes")
#partidos <- c("nva","pes")


for (partido in partidos){ 
  #for (i in 1:nrow(ayuntamientos)){
  for (j in ids){
    #j= 93
    #partido = "nva"
    
    prueba_data = ayuntamientos%>%
      filter(id_seccion == j)%>%
      select(ano,all_of(partido))%>%
      arrange(ano)
    
    Unidades <- pull(prueba_data,partido)
    
    
    tSUnidades <- ts(Unidades,start = 2015, end = 2021, frequency = 1/3)
    
    
    fit <- auto.arima(tSUnidades)
    
    preds <- cbind(j,as.data.frame(forecast(fit,1)))
    
    colnames(preds)[2] = partido
    
    data = rbind(data,preds)
    
  }
  
  data_all = data_all%>%
    left_join(data, by = "j")
  
  data = c()
}

# data_ayuntamientos <- data_all%>%
#   select(j,pan,pri,prd,pt,pvem,mc,nva,morena,pes)%>%
#   rename(id_seccion = j)%>%
#   pivot_longer(names_to = "partido", values_to = "votos", -id_seccion)%>%
#   mutate(tipo = "Ayuntamientos")%>%
#   mutate(partido = case_when(
#      partido == "pan" ~ "PAN",
#      partido == "pri" ~ "PRI",
#      partido == "prd" ~ "PRD",
#      partido == "pt" ~ "PT",
#      partido == "pvem" ~ "PVEM",
#      partido == "mc" ~ "MC",
#      partido == "morena" ~ "MORENA",
#     partido == "nva" ~ "PANAL",
#     partido == "pes" ~ "PES"),
#     
#   votos = round(votos)
#   )

# Solo para los dos partidos que se agregaron 
data_ayuntamientos_dos <- data_all%>%
  select(j,nva,pes)%>%
  rename(id_seccion = j)%>%
  pivot_longer(names_to = "partido", values_to = "votos", -id_seccion)%>%
  mutate(tipo = "Ayuntamientos")%>%
  mutate(partido = case_when(
    # partido == "pan" ~ "PAN",
    # partido == "pri" ~ "PRI",
    # partido == "prd" ~ "PRD",
    # partido == "pt" ~ "PT",
    # partido == "pvem" ~ "PVEM",
    # partido == "mc" ~ "MC",
    # partido == "morena" ~ "MORENA",
    partido == "nva" ~ "PANAL",
    partido == "pes" ~ "PES"),
    
    votos = round(votos)
  )

data_ayuntamientos_dos <- data_ayuntamientos_dos%>%
  mutate(votos = ifelse(votos < 0 ,0, votos))

write.csv(data_ayuntamientos_dos,"C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/ayuntamientos_2.csv", row.names = FALSE)



#ayuntamientos_participacion <- left_join(data_ayuntamientos,data_p, by = "id_seccion")

#write.csv(ayuntamientos_participacion,"C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/ayuntamientos_participacion.csv", row.names = FALSE)



# FUNCION PARA DIPUTACIONES  -------------------------------------
ids <- diputaciones$id_seccion%>% unique()
data_all <- data.frame(ids)%>%
  rename(j = ids)
data = c()
#partidos <- c("pan","pri","prd","pt","pvem","mc","morena","nva","pes")
partidos <- c("nva","pes")


for (partido in partidos){ 
  #for (i in 1:nrow(ayuntamientos)){
  for (j in ids){
    #j= 1
    #partido = "pri"
    
    prueba_data = diputaciones%>%
      filter(id_seccion == j)%>%
      select(ano,all_of(partido))%>%
      arrange(ano)
    
    Unidades <- pull(prueba_data,partido)
    
    
    tSUnidades <- ts(Unidades,start = 2015, end = 2021, frequency = 1/3)
    
    
    fit <- auto.arima(tSUnidades)
    
    preds <- cbind(j,as.data.frame(forecast(fit,1)))
    
    colnames(preds)[2] = partido
    
    data = rbind(data,preds)
    
  }
  
  data_all = data_all%>%
    left_join(data, by = "j")
  
  data = c()
}

# data_diputaciones <- data_all%>%
#   select(j,pan,pri,prd,pt,pvem,mc,morena)%>%
#   rename(id_seccion = j)%>%
#   pivot_longer(names_to = "partido", values_to = "votos", -id_seccion)%>%
#   mutate(tipo = "Diputados Locales")%>%
#   mutate(partido = case_when(
#     partido == "pan" ~ "PAN",
#     partido == "pri" ~ "PRI",
#     partido == "prd" ~ "PRD",
#     partido == "pt" ~ "PT",
#     partido == "pvem" ~ "PVEM",
#     partido == "mc" ~ "MC",
#     partido == "morena" ~ "MORENA",
#     partido == "nva" ~ "PANAL",
#     partido == "pes" ~ "PES"),
#     votos = round(votos))

data_diputaciones_dos <- data_all%>%
  select(j,nva,pes)%>%
  rename(id_seccion = j)%>%
  pivot_longer(names_to = "partido", values_to = "votos", -id_seccion)%>%
  mutate(tipo = "Diputados Locales")%>%
  mutate(partido = case_when(
    # partido == "pan" ~ "PAN",
    # partido == "pri" ~ "PRI",
    # partido == "prd" ~ "PRD",
    # partido == "pt" ~ "PT",
    # partido == "pvem" ~ "PVEM",
    # partido == "mc" ~ "MC",
    # partido == "morena" ~ "MORENA",
    partido == "nva" ~ "PANAL",
    partido == "pes" ~ "PES"),
    votos = round(votos))

data_diputaciones_dos <- data_diputaciones_dos%>%
  mutate(votos = ifelse(votos < 0, 0, votos))

#write.csv(data_diputaciones_dos,"C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/diputaciones_2.csv", row.names = FALSE)


#write.csv(data_diputaciones,"C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/diputaciones.csv", row.names = FALSE)


# FUNCION PARA GOBERNADOR  -------------------------------------

ids <- gobernador$id_seccion%>%unique()
data_all <- data.frame(ids)%>%
  rename(j = ids)
data = c()
#partidos <- c("pan","pri","prd","pt","pvem","morena","nva")
partidos <- c("nva")


for (partido in partidos){ 
  #for (i in 1:nrow(ayuntamientos)){
  for (j in ids){
    #j= 1
    #partido = "pri"
    
    prueba_data = gobernador%>%
      filter(id_seccion == j)%>%
      select(ano,all_of(partido))%>%
      arrange(ano)
    
    Unidades <- pull(prueba_data,partido)
    
    
    tSUnidades <- ts(Unidades,start = 2011, end = 2017, frequency = 1/6)
    
    
    fit <- auto.arima(tSUnidades)
    
    preds <- cbind(j,as.data.frame(forecast(fit,1)))
    
    colnames(preds)[2] = partido
    
    data = rbind(data,preds)
    
  }
  
  data_all = data_all%>%
    left_join(data, by = "j")
  
  data = c()
}

# data_gobernador <- data_all%>%
#   select(j,pan,pri,prd,pt,pvem,morena)%>%
#   rename(id_seccion = j)%>%
#   pivot_longer(names_to = "partido", values_to = "votos", -id_seccion)%>%
#   mutate(tipo = "Gobernador")%>%
#   mutate(partido = case_when(
#     partido == "pan" ~ "PAN",
#     partido == "pri" ~ "PRI",
#     partido == "prd" ~ "PRD",
#     partido == "pt" ~ "PT",
#     partido == "pvem" ~ "PVEM",
#     partido == "nva" ~ "PANAL",
#     partido == "morena" ~ "MORENA"),
#     votos = round(votos))

data_gobernador_dos <- data_all%>%
  select(j,nva)%>%
  rename(id_seccion = j)%>%
  pivot_longer(names_to = "partido", values_to = "votos", -id_seccion)%>%
  mutate(tipo = "Gobernador")%>%
  mutate(partido = case_when(
    # partido == "pan" ~ "PAN",
    # partido == "pri" ~ "PRI",
    # partido == "prd" ~ "PRD",
    # partido == "pt" ~ "PT",
    # partido == "pvem" ~ "PVEM",
    partido == "nva" ~ "PANAL"
    # partido == "morena" ~ "MORENA"
    ),
    votos = round(votos))

data_gobernador_dos <- data_gobernador_dos%>%
  mutate(votos = ifelse(votos < 0, 0, votos))

#write.csv(data_gobernador_dos,"C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/gobernador_2.csv", row.names = FALSE)


#write.csv(data_gobernador,"C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/gobernador.csv", row.names = FALSE)


# # Agregar lista a 2015 en ayuntamientos 
# 
# lista_2015 <- participacion%>%
#   filter(ano == 2015)%>%
#   select(id_seccion, lista_nominal)%>%
#   rename(`2015` = lista_nominal)
# 
# ano_lista = ayuntamientos%>%
#   select(id_seccion,ano, lista_nominal)%>%
#   pivot_wider(names_from = ano, values_from = lista_nominal)%>%
#   left_join(lista_2015, by = "id_seccion")%>%
#   select(-`2015.x`)%>%
#   rename(`2015` = `2015.y`)%>%
#   pivot_longer(names_to = "ano", values_to = "lista_nominal", -id_seccion)
# 
# ayuntamientos_try = ayuntamientos%>%
#   left_join(ano_lista, by = c("id_seccion"))



# FUNCION PARA PARTICIPACION ---------------------------------------------------
participacion = participacion%>%
  select(id_seccion,ano,si_voto)


ids <- participacion$id_seccion%>% unique()

data_all_participacion <- data.frame(ids)%>%
  rename(j = ids)

data_p = c()

for (j in ids){
  
  prueba_data = participacion%>%
    filter(id_seccion == j)%>%
    select(ano,si_voto)%>%
    arrange(ano)
  
  Unidades <- pull(prueba_data,si_voto)
  
  
  tSUnidades <- ts(Unidades,start = 2015, end = 2021, frequency = 1/3)
  
  
  fit <- auto.arima(tSUnidades)
  
  preds <- cbind(j,as.data.frame(forecast(fit,1)))
  
  
  data_p = rbind(data_p,preds)
  
  
  
  
}

data_p = data_p%>%
  select(j,"Point Forecast")%>%
  rename("id_seccion" = j,
         "participacion" = "Point Forecast")




# FUNCION PARA PARTICIPACION DE GOBERNADOR  ---------------------------------------------------
participacion = gobernador%>%
  select(id_seccion,ano,total)


ids <- participacion$id_seccion%>% unique()

data_all_participacion <- data.frame(ids)%>%
  rename(j = ids)

data_p = c()

for (j in ids){
  
  prueba_data = participacion%>%
    filter(id_seccion == j)%>%
    select(ano,total)%>%
    arrange(ano)
  
  Unidades <- pull(prueba_data,total)
  
  
  tSUnidades <- ts(Unidades,start = 2011, end = 2017, frequency = 1/6)
  
  
  fit <- auto.arima(tSUnidades)
  
  preds <- cbind(j,as.data.frame(forecast(fit,1)))
  
  
  data_p = rbind(data_p,preds)
  
  
  
  
}

data_p = data_p%>%
  select(j,"Point Forecast")%>%
  rename("id_seccion" = j,
         "participacion_gobernador" = "Point Forecast")


#write.csv(data_p,"C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/participacion_gobernador.csv", row.names = FALSE)



##-----------------------------------------------------------------------------
# FUNCION PARA LISTA NOMINAL AYUNTAMIENTOS Y DIPUTADOS LOCALES ----------------

# lista_a_d = ano_lista%>%
#   select(id_seccion,ano,lista_nominal)
# 
# 
# ids <- lista_a_d$id_seccion%>% unique()
# 
# data_all_lista_a_d <- data.frame(ids)%>%
#   rename(j = ids)
# 
# data_p = c()
# 
# for (j in ids){
# 
#   prueba_data = ano_lista%>%
#     filter(id_seccion == j)%>%
#     select(ano,lista_nominal)%>%
#     arrange(ano)
#   
#   Unidades <- pull(prueba_data,lista_nominal)
#   
#   
#   tSUnidades <- ts(Unidades,start = 2015, end = 2021, frequency = 1/3)
#   
#   
#   fit <- auto.arima(tSUnidades)
#   
#   
#   preds <- cbind(j,as.data.frame(forecast(fit,1)))
#   
#   
#   data_p = rbind(data_p,preds)
#   
#   
#   
#   
# }
# 
# data_lista_a_d = data_p%>%
#   select(j,"Point Forecast")%>%
#   rename("id_seccion" = j,
#          "lista_nominal" = "Point Forecast")

#data_lista_a_d = data_lista_a_d%>% na.omit()
#sum(data_lista_a_d$lista_nominal)

#write.csv(data_lista_a_d,"C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/prediccion_lista.csv", row.names = FALSE)




## Unir bases 

base1 = read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/ayuntamientos_participacion.csv")
base2 = read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/diputaciones.csv")
base3 = read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/gobernador.csv")
participacion = base1%>%
  select(id_seccion,participacion)%>%unique()

base2 = left_join(base2,participacion, by= "id_seccion")
base3 = left_join(base3,participacion, by = "id_seccion")
data_all = smartbind(as.data.frame(base1),as.data.frame(base2),as.data.frame(base3))

#write_csv(data_all,"C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/all_prediction.csv" )




## Agregar datos de prediccion a los nuevos partidos y agrupar en una sola base ----------------------------

# Filtar solo ayuntamientos y hacer smartbind con los nuevos partidos 
base1_1 = read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/ayuntamientos_participacion.csv")%>%
  select(-participacion)%>%
  as.data.frame()

base1_2 <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/ayuntamientos_2.csv")%>%
  as.data.frame()

base_1 <- smartbind(base1_1,base1_2)


base2_1 = read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/diputaciones.csv")%>%
  as.data.frame()

base2_2 = read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/diputaciones_2.csv")%>%
  as.data.frame()

base_2 <- smartbind(base2_1,base2_2)

base3_1 = read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/gobernador.csv")%>%
  as.data.frame()

base3_2 = read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/gobernador_2.csv")%>%
  as.data.frame()

base_3 <- smartbind(base3_1,base3_2)

base4 = read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/ayuntamientos_participacion.csv")%>%
  select(id_seccion,participacion)%>%
  unique()%>%
  as.data.frame()

base5 = read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/participacion_gobernador.csv")%>%
  as.data.frame()

base_participaciones <- left_join(base4,base5, by = "id_seccion")
base_participaciones[is.na(base_participaciones)] <- 0

# Unir las filas de los tipos de eleccion
data_all = smartbind(as.data.frame(base_1),as.data.frame(base_2),as.data.frame(base_3))

# Agregar las participaciones como columnas
data_all = data_all%>%
  left_join(base_participaciones, by = "id_seccion")

#write_csv(data_all,"C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/all_prediction_nuevo.csv" )

