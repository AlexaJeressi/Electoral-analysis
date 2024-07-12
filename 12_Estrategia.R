library(tidyverse)
library(gtools)
library(stringr)

dir <- 'C:/Users/Alexa/OneDrive - Next Gate Research/'

## Datos de clusters 
load(paste0(dir,"alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_tres/","data_clusters.RData"))

# Cargar datos completos para leer elecciones historicas
#load(paste0(dir,"alexa/2022/10_Plataforma/Scripts/elecciones/data/data_all.RData"))

#write.csv(data_all, "C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Bases/base_votos_elecciones.csv", row.names = FALSE)


# Cargar datos sobre las elecciones historicas 
dir <- "C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Bases/"

ayuntamientos <- read_csv(paste0(dir,"ayuntamientos_general.csv"))%>%
  pivot_longer(names_to = "partido", values_to = "votos",-c(id_seccion,ano,tipo,id_entidad_R,id_municipio_R,id_distrito_R,total))%>%
  select(id_seccion,ano,partido,votos,tipo)%>%
  as.data.frame()

diputaciones <- read_csv(paste0(dir,"diputaciones_general.csv"))%>%
  pivot_longer(names_to = "partido", values_to = "votos",-c(id_seccion,ano,tipo,id_entidad_R,id_municipio_R,id_distrito_R,total))%>%
  select(id_seccion,ano,partido,votos,tipo)%>%
  as.data.frame()

gobernador <- read_csv(paste0(dir,"gobernador_general.csv"))%>%
  pivot_longer(names_to = "partido", values_to = "votos",-c(id_seccion,ano,tipo,id_entidad_R,id_municipio_R,id_distrito_R,total,lista_nominal,nulos))%>%
  select(id_seccion,ano,partido,votos,tipo)%>%
  as.data.frame()

data_all <- smartbind(as.data.frame(ayuntamientos),as.data.frame(diputaciones),as.data.frame(gobernador))

data_all <- data_all%>%
  filter(ano %in% c(2021,2017))

data_all <- data_all%>%
  filter(!partido %in% c("otros","independientes"))%>%
  mutate(partido = case_when(
    partido == "pan" ~ "PAN",
    partido == "pri" ~ "PRI",
    partido == "prd" ~ "PRD",
    partido == "pt" ~ "PT",
    partido == "pvem" ~ "PVEM",
    partido == "pes" ~ "PES",
    partido == "mc" ~ "MC",
    partido == "morena" ~ "MORENA",
    partido == "nva" ~ "PANAL"
    
  ))


# Leer bases de competitividad por eleccion 
load("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Datos_indices/resultados_indices.RData")
ayuntamientos_index <- ayuntamientos_index%>%
  mutate(eleccion = "Ayuntamientos")

diputados_index <- diputados_index%>%
  mutate(eleccion = "Diputados Locales")

gobernador_index <- gobernador_index%>%
  mutate(eleccion = "Gobernador")

base_votos_elecciones = smartbind(ayuntamientos_index,diputados_index,gobernador_index)


#################### PRIMERA FUNCION PARA ALCANCE BASICO ########################################
#------------------- AYUNTAMIENTOS Y DIPUTADOS LOCALES   ---------------------------------------#

partido_1 <- c("PAN", "PRI","PRD","PT","PVEM","MC", "MORENA","PANAL","PES")
partido2 <- c("p_pan","p_pri","p_prd","p_pt","p_pvem","p_mc","p_morena","p_nva","p_pes")
eleccion_1 <- c("Ayuntamientos","Diputados Locales")
#bases <- c(ayuntamientos_competitividad, diputaciones_competitividad)


secciones_all <- data_all%>%select(id_seccion)%>%unique()
secciones_fin <- data_all%>%select(id_seccion)%>%unique()
i_n <- 0
k_n <- 0

for(i in eleccion_1){
  # Agregar contador de i 
  i_n <- i_n +1
  for (k in partido_1){
    k_n <- k_n + 1
     #i = "Ayuntamientos"
     #k = "PANAL"
     #j = "p_nva"
     #k_n = 8
    # Para obtener los votos por seccion para cada partido
    votos_partido = data_all%>%
      dplyr::filter(tipo == i)%>%
      dplyr::filter(partido == k)%>%
      #dplyr::filter(ano == 2021)%>%
      dplyr::select(id_seccion,votos,partido)
    
    
    # Para obtener la competitividad de las secciones para el partido 
    # Y obtener el cluster al que pertenece la seccion 
    t1 <- base_votos_elecciones %>%
      filter(partido == partido2[k_n],
             estratos == "Posibles",
             eleccion == i)%>%
      # Agregar a que cluster pertenece 
      left_join(clusters, by = "id_seccion")%>%
      # Agregar datos de la ultima votacion en ayuntamientos. Votos para el pan
      left_join(votos_partido, by= "id_seccion")
    
    # Obtener el cluster en el que vota más por el partido 
    cluster = t1%>%
      group_by(clus)%>%
      summarise(votos = sum(votos))%>%
      arrange(desc(votos))%>%
      slice(1)
    
    valor_cluster <- pull(cluster,clus)
    
    t1 = t1%>%
      filter(clus == valor_cluster)%>%
      select(id_seccion)%>%
      mutate(x = id_seccion)
    
    secciones_all = secciones_all%>%
      full_join(t1, by="id_seccion")
    
    colnames(secciones_all)[k_n+1] <- paste0(partido_1[k_n],"_",eleccion_1[i_n])
     
  }
  
  secciones_fin = secciones_fin%>%
    left_join(secciones_all , by = "id_seccion")
  
  secciones_all <- data_all%>%select(id_seccion)%>%unique()
  
  k_n <- 0 
  }

B_secciones_a_d <- secciones_fin

#------------------- GOBERNADOR   ---------------------------------------#  
partido_1 <- c("PAN", "PRI","PRD","PT","PVEM","MORENA","PANAL")
partido2 <- c("p_pan","p_pri","p_prd","p_pt","p_pvem","p_morena","p_nva")
eleccion_1 <- "Gobernador"
#bases <- c(gobernador_competitividad)

secciones_all <- data_all%>%select(id_seccion)%>%unique()
secciones_fin <- data_all%>%select(id_seccion)%>%unique()

i_n <- 0
k_n <- 0

for(i in eleccion_1){
  # Agregar contador de i 
  i_n <- i_n +1
  for (k in partido_1){
    k_n <- k_n + 1
    #i = "ayuntamientos"
    #k = "PAN"
    #j = "p_pan"
    # Para obtener los votos por seccion para cada partido
    votos_partido = data_all%>%
      dplyr::filter(tipo == i)%>%
      dplyr::filter(partido == k)%>%
      dplyr::filter(ano == 2017)%>%
      dplyr::select(id_seccion,votos,partido)
    
    
    # Para obtener la competitividad de las secciones para el partido 
    # Y obtener el cluster al que pertenece la seccion 
    t1 <- base_votos_elecciones %>%
      filter(partido == partido2[k_n],
             estratos == "Posibles",
             eleccion == i)%>%
      # Agregar a que cluster pertenece 
      left_join(clusters, by = "id_seccion")%>%
      # Agregar datos de la ultima votacion en ayuntamientos. Votos para el pan
      left_join(votos_partido, by= "id_seccion")
    
    # Obtener el cluster en el que vota más por el partido 
    cluster = t1%>%
      group_by(clus)%>%
      summarise(votos = sum(votos))%>%
      arrange(desc(votos))%>%
      slice(1)
    
    valor_cluster <- pull(cluster,clus)
    
    t1 = t1%>%
      filter(clus == valor_cluster)%>%
      select(id_seccion)%>%
      mutate(x = id_seccion)
    
    secciones_all = secciones_all%>%
      full_join(t1, by="id_seccion")
    
    colnames(secciones_all)[k_n+1] <- paste0(partido_1[k_n],"_",eleccion_1[i_n])
    
  }
  
  secciones_fin = secciones_fin%>%
    left_join(secciones_all , by = "id_seccion")
  
  #secciones_all <- data_all%>%select(id_seccion)%>%unique()
  
  #k_n <- 0 
}

B_secciones_g <- secciones_fin



#################### SEGUNDA FUNCION PARA ALCANCE INTERMEDIO ########################################
#------------------- AYUNTAMIENTOS Y DIPUTADOS LOCALES   ---------------------------------------#

partido_1 <- c("PAN", "PRI","PRD","PT","PVEM","MC", "MORENA","PANAL","PES")
partido2 <- c("p_pan","p_pri","p_prd","p_pt","p_pvem","p_mc","p_morena","p_nva","p_pes")
eleccion_1 <- c("Ayuntamientos","Diputados Locales")
#bases <- c(ayuntamientos_competitividad, diputaciones_competitividad)


secciones_all <- data_all%>%select(id_seccion)%>%unique()
secciones_fin <- data_all%>%select(id_seccion)%>%unique()

i_n <- 0
k_n <- 0

for(i in eleccion_1){
  # Agregar contador de i 
  i_n <- i_n +1
  for (k in partido_1){
    k_n <- k_n + 1
    #i = "ayuntamientos"
    #k = "PAN"
    #j = "p_pan"
    # Para obtener los votos por seccion para cada partido
    votos_partido = data_all%>%
      dplyr::filter(tipo == i)%>%
      dplyr::filter(partido == k)%>%
      dplyr::filter(ano == 2021)%>%
      dplyr::select(id_seccion,votos,partido)
    
    
    # Para obtener la competitividad de las secciones para el partido 
    # Y obtener el cluster al que pertenece la seccion 
    t1 <- base_votos_elecciones %>%
      filter(partido == partido2[k_n],
             estratos %in% c("Posibles", "Blandas"),
             eleccion == i)%>%
      # Agregar a que cluster pertenece 
      left_join(clusters, by = "id_seccion")%>%
      # Agregar datos de la ultima votacion en ayuntamientos. Votos para el pan
      left_join(votos_partido, by= "id_seccion")
    
    # Obtener el cluster en el que vota más por el partido 
    cluster = t1%>%
      group_by(clus)%>%
      summarise(votos = sum(votos))%>%
      arrange(desc(votos))%>%
      slice(1)
    
    valor_cluster <- pull(cluster,clus)
    
    t1 = t1%>%
      filter(clus == valor_cluster)%>%
      select(id_seccion)%>%
      mutate(x = id_seccion)
    
    secciones_all = secciones_all%>%
      full_join(t1, by="id_seccion")
    
    colnames(secciones_all)[k_n+1] <- paste0(partido_1[k_n],"_",eleccion_1[i_n])
    
  }
  
  secciones_fin = secciones_fin%>%
    left_join(secciones_all , by = "id_seccion")
  
  secciones_all <- data_all%>%select(id_seccion)%>%unique()
  
  k_n <- 0 
}

I_secciones_a_d <- secciones_fin
#------------------- GOBERNADOR   ---------------------------------------#  
partido_1 <- c("PAN", "PRI","PRD","PT","PVEM","MORENA","PANAL")
partido2 <- c("p_pan","p_pri","p_prd","p_pt","p_pvem","p_morena","p_nva")
eleccion_1 <- "Gobernador"
#bases <- c(gobernador_competitividad)

secciones_all <- data_all%>%select(id_seccion)%>%unique()
secciones_fin <- data_all%>%select(id_seccion)%>%unique()

i_n <- 0
k_n <- 0

for(i in eleccion_1){
  # Agregar contador de i 
  i_n <- i_n +1
  for (k in partido_1){
    k_n <- k_n + 1
    #i = "ayuntamientos"
    #k = "PAN"
    #j = "p_pan"
    # Para obtener los votos por seccion para cada partido
    votos_partido = data_all%>%
      dplyr::filter(tipo == i)%>%
      dplyr::filter(partido == k)%>%
      dplyr::filter(ano == 2017)%>%
      dplyr::select(id_seccion,votos,partido)
    
    
    # Para obtener la competitividad de las secciones para el partido 
    # Y obtener el cluster al que pertenece la seccion 
    t1 <- base_votos_elecciones %>%
      filter(partido == partido2[k_n],
             estratos %in% c("Posibles", "Blandas"),
             eleccion == i)%>%
      # Agregar a que cluster pertenece 
      left_join(clusters, by = "id_seccion")%>%
      # Agregar datos de la ultima votacion en ayuntamientos. Votos para el pan
      left_join(votos_partido, by= "id_seccion")
    
    # Obtener el cluster en el que vota más por el partido 
    cluster = t1%>%
      group_by(clus)%>%
      summarise(votos = sum(votos))%>%
      arrange(desc(votos))%>%
      slice(1)
    
    valor_cluster <- pull(cluster,clus)
    
    t1 = t1%>%
      filter(clus == valor_cluster)%>%
      select(id_seccion)%>%
      mutate(x = id_seccion)
    
    secciones_all = secciones_all%>%
      full_join(t1, by="id_seccion")
    
    colnames(secciones_all)[k_n+1] <- paste0(partido_1[k_n],"_",eleccion_1[i_n])
    
  }
  
  secciones_fin = secciones_fin%>%
    left_join(secciones_all , by = "id_seccion")
  
  #secciones_all <- data_all%>%select(id_seccion)%>%unique()
  
  #k_n <- 0 
}
I_secciones_g <- secciones_fin





#################### TERCERA FUNCION PARA ALCANCE COMPLETO ########################################
#------------------- AYUNTAMIENTOS Y DIPUTADOS LOCALES   ---------------------------------------#

partido_1 <- c("PAN", "PRI","PRD","PT","PVEM","MC", "MORENA","PANAL","PES")
partido2 <- c("p_pan","p_pri","p_prd","p_pt","p_pvem","p_mc","p_morena","p_nva","p_pes")
eleccion_1 <- c("Ayuntamientos","Diputados Locales")
#bases <- c(ayuntamientos_competitividad, diputaciones_competitividad)


secciones_all <- data_all%>%select(id_seccion)%>%unique()
secciones_fin <- data_all%>%select(id_seccion)%>%unique()

i_n <- 0
k_n <- 0

for(i in eleccion_1){
  # Agregar contador de i 
  i_n <- i_n +1
  for (k in partido_1){
    k_n <- k_n + 1
    #i = "ayuntamientos"
    #k = "PAN"
    #j = "p_pan"
    # Para obtener los votos por seccion para cada partido
    votos_partido = data_all%>%
      dplyr::filter(tipo == i)%>%
      dplyr::filter(partido == k)%>%
      dplyr::filter(ano == 2021)%>%
      dplyr::select(id_seccion,votos,partido)
    
    
    # Para obtener la competitividad de las secciones para el partido 
    # Y obtener el cluster al que pertenece la seccion 
   
    
    
    t1 <- base_votos_elecciones %>%
      filter(partido == partido2[k_n],
             estratos %in% c("Posibles", "Blandas"),
             eleccion == i)%>%
      select(id_seccion)%>%
      mutate(x = id_seccion)
    
    
    secciones_all = secciones_all%>%
      full_join(t1, by="id_seccion")
    
    colnames(secciones_all)[k_n+1] <- paste0(partido_1[k_n],"_",eleccion_1[i_n])
    
  }
  
  secciones_fin = secciones_fin%>%
    left_join(secciones_all , by = "id_seccion")
  
  secciones_all <- data_all%>%select(id_seccion)%>%unique()
  
  k_n <- 0 
}


C_secciones_a_d <- secciones_fin

#------------------- GOBERNADOR   ---------------------------------------#  
partido_1 <- c("PAN", "PRI","PRD","PT","PVEM","MORENA","PANAL")
partido2 <- c("p_pan","p_pri","p_prd","p_pt","p_pvem","p_morena","p_nva")
eleccion_1 <- "Gobernador"
#bases <- c(gobernador_competitividad)

secciones_all <- data_all%>%select(id_seccion)%>%unique()
secciones_fin <- data_all%>%select(id_seccion)%>%unique()

i_n <- 0
k_n <- 0

for(i in eleccion_1){
  # Agregar contador de i 
  i_n <- i_n +1
  for (k in partido_1){
    k_n <- k_n + 1
    #i = "ayuntamientos"
    #k = "PAN"
    #j = "p_pan"
    # Para obtener los votos por seccion para cada partido
    votos_partido = data_all%>%
      dplyr::filter(tipo == i)%>%
      dplyr::filter(partido == k)%>%
      dplyr::filter(ano == 2017)%>%
      dplyr::select(id_seccion,votos,partido)
    
    
    # Para obtener la competitividad de las secciones para el partido 
    # Y obtener el cluster al que pertenece la seccion 
    
    
    
    t1 <- base_votos_elecciones %>%
      filter(partido == partido2[k_n],
             estratos %in% c("Posibles", "Blandas"),
             eleccion == i)%>%
      select(id_seccion)%>%
      mutate(x = id_seccion)
    
    
    secciones_all = secciones_all%>%
      full_join(t1, by="id_seccion")
    
    colnames(secciones_all)[k_n+1] <- paste0(partido_1[k_n],"_",eleccion_1[i_n])
    
  }
  
  secciones_fin = secciones_fin%>%
    left_join(secciones_all , by = "id_seccion")
  
  #secciones_all <- data_all%>%select(id_seccion)%>%unique()
  
  #k_n <- 0 
}

C_secciones_g <- secciones_fin




# Agreagar columnas de indentificacion --------------------------------------------------------
B_secciones_a_d2 = B_secciones_a_d %>% 
  pivot_longer(names_to = "nombre", values_to = "valor",-id_seccion)%>%
  mutate(partido = str_extract(nombre,"[:alpha:]+(?=_)"),
         eleccion = str_extract(nombre,"(?<=_)[:alpha:]+"),
         alcalnce = "Basico")%>%
  select(-id_seccion,-nombre)%>%
  na.omit()%>%
  rename(id_seccion = valor)


B_secciones_g2 = B_secciones_g%>%
  pivot_longer(names_to = "nombre", values_to = "valor",-id_seccion)%>%
  mutate(partido = str_extract(nombre,"[:alpha:]+(?=_)"),
         eleccion = str_extract(nombre,"(?<=_)[:alpha:]+"),
         alcalnce = "Basico")%>%
  select(-id_seccion,-nombre)%>%
  na.omit()%>%
  rename(id_seccion = valor)

I_secciones_a_d2 = I_secciones_a_d %>%
  pivot_longer(names_to = "nombre", values_to = "valor",-id_seccion)%>%
  mutate(partido = str_extract(nombre,"[:alpha:]+(?=_)"),
         eleccion = str_extract(nombre,"(?<=_)[:alpha:]+"),
         alcalnce = "Intermedio")%>%
  select(-id_seccion,-nombre)%>%
  na.omit()%>%
  rename(id_seccion = valor)

I_secciones_g2 = I_secciones_g %>%
  pivot_longer(names_to = "nombre", values_to = "valor",-id_seccion)%>%
  mutate(partido = str_extract(nombre,"[:alpha:]+(?=_)"),
         eleccion = str_extract(nombre,"(?<=_)[:alpha:]+"),
         alcalnce = "Intermedio")%>%
  select(-id_seccion,-nombre)%>%
  na.omit()%>%
  rename(id_seccion = valor)

C_secciones_a_d2 = C_secciones_a_d %>%
  pivot_longer(names_to = "nombre", values_to = "valor",-id_seccion)%>%
  mutate(partido = str_extract(nombre,"[:alpha:]+(?=_)"),
         eleccion = str_extract(nombre,"(?<=_)[:alpha:]+"),
         alcalnce = "Completo")%>%
  select(-id_seccion,-nombre)%>%
  na.omit()%>%
  rename(id_seccion = valor)

C_secciones_g2 = C_secciones_g%>%
  pivot_longer(names_to = "nombre", values_to = "valor",-id_seccion)%>%
  mutate(partido = str_extract(nombre,"[:alpha:]+(?=_)"),
         eleccion = str_extract(nombre,"(?<=_)[:alpha:]+"),
         alcalnce = "Completo")%>%
  select(-id_seccion,-nombre)%>%
  na.omit()%>%
  rename(id_seccion = valor)


secciones_p_e_a <- smartbind(as.data.frame(B_secciones_a_d2),as.data.frame(B_secciones_g2),
                             as.data.frame(I_secciones_a_d2),as.data.frame(I_secciones_g2),
                             as.data.frame(C_secciones_a_d2),as.data.frame(C_secciones_g2))  


# Unir los sociodemograficos que nos van a ayudar a hacer las regresiones
# secciones_p_e_a = secciones_p_e_a%>%
#   left_join(demo, by="id_seccion")
# 
# 
# #Obtener el numero de "si voto" por eleccion de la ultima eleccion
# # Columnas: id_seccion,eleccion,lista_nominal,participacion
# 
# participacion <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/12_Edomex_elecciones/Datos/porcentaje_participacion.csv")
# 
# participacion_ag <- participacion%>%
#   filter(ano %in% c(2017,2021))%>%
#   mutate(eleccion = case_when(
#     ano == 2021 ~ "ayuntamientos",
#     ano == 2017 ~ "gobernador"
#   ))%>%
#   as.data.frame()
# 
# participacion_d <- participacion%>%
#   filter(ano == 2021)%>%
#   mutate(eleccion = "diputados")%>%
#   as.data.frame()
# 
# participacion_f <- smartbind(participacion_ag,participacion_d)
# 
# 
# # Agregar datos de participacion y lista nominal a la base completa
# secciones_p_e_a = secciones_p_e_a%>%
#   left_join(participacion_f, by = c("id_seccion","eleccion"))%>%
#   mutate(
#     nivel_numero = case_when(
#       nivel == "A/B" ~ 7,
#       nivel == "C+" ~ 6,
#       nivel == "C" ~ 5,
#       nivel == "C-" ~ 4,
#       nivel == "D+" ~ 3,
#       nivel == "D" ~ 2,
#       nivel == "E" ~ 1
# )
#   )
# 
# # Lista nominal 2022
# 
# lista <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/lista_nominal_2022/lista_csv_22.csv")%>%
#   janitor::clean_names()%>%
#   filter(nombre_entidad == "MEXICO")
# 
# lista_22 <- lista%>%
#   group_by(seccion)%>%
#   summarise(lista_nominal_22 = rowSums(across(lista_aguascalientes:lista_88)))%>%
#   rename(id_seccion = seccion)
# 
# 
# secciones_p_e_a <- secciones_p_e_a%>%
#   left_join(lista_22, by = "id_seccion")



#write.csv(secciones_p_e_a, "C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/alcance_predicciones.csv", row.names = FALSE)
# Aqui decidimos las secciones que le van a pertener a cada partido, en cada alcance y en cada eleccion
# Ahora toca acomodar a cada partido como primera fuerza para poder comparar. 


################################ Segunda parte ##########################################################
#---------- Agregar los datos de los votos a la tabla ya hecha -------------------------
# Datos por alcance, partido y eleccion 
datos <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/alcance_predicciones.csv")

# Numero de votos por partido por eleccion 
prediccion_votos <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/all_prediction_nuevo.csv")
# Estos datos nos van a servir porque con base en estos vamos a poder simular la nueva primera fuerza por partido 


### Vamos  hacer tres loop, por tipo de eleccion para reestructurar las primeras fuerzas

### -----------Ayuntamientos ---------------------------------------

#eleccion_t = "ayuntamientos"
eleccion_2 = "Ayuntamientos"
partidos = c("PANAL","PES","PAN","PRI","PRD","PT","PVEM","MC","MORENA")
alcances = c("Basico","Intermedio","Completo")


columnas_b3 = c("id_seccion","partido","votos","tipo",
                "participacion","partidos_nuevo","partido_base")
base3 = data.frame(matrix(nrow = 0, ncol = length(columnas_b3))) 
colnames(base3) <- columnas_b3

columnas_b6 = c("id_seccion","partido","votos","tipo",
                 "participacion","partidos_nuevo","tipo_alcance",
                "partido_base")
base6 = data.frame(matrix(nrow = 0, ncol = length(columnas_b6))) 
colnames(base6) <- columnas_b6
 



for (partidoi in partidos){
  
  columnas_b3 = c("id_seccion","partido","votos","tipo","participacion","partidos_nuevo","partido_base")
  base3 = data.frame(matrix(nrow = 0, ncol = length(columnas_b3))) 
  colnames(base3) <- columnas_b3

  for (alcance in alcances){
    #alcance = "Basico"
    #partidoi = "PANAL"
    #eleccion_2 = "Ayuntamientos"
    
    secciones = datos%>%
      filter(eleccion == eleccion_2,
             alcalnce == alcance,
             partido == partidoi)%>%
      pull(id_seccion)%>%
      unique()
    
    
    base1 = prediccion_votos%>%
      filter(tipo == eleccion_2,
             id_seccion %in% secciones)
    
    for(seccion in secciones){
      
      partidos_original = base1%>%
        filter(id_seccion == seccion)%>%
        arrange(desc(votos))%>%
        pull(partido)
      
      partidos_nuevo = append(partidos_original[!partidos_original == partidoi], partidoi, 0)
      
      # Itera las secciones  
      base2 = base1%>%
        filter(id_seccion == seccion)%>%
        arrange(desc(votos))%>%
        cbind(partidos_nuevo)%>%
        mutate(tipo_alcance = alcance,
               partido_base = partidoi)%>%
        as.data.frame()
    
      # Va almacenando las secciones con una nuestra estructura de partidos
      # En base 3 se van juntando las secciones 
      # Se termina base 3 cuando corrieron todas las secciones 
      base3 = smartbind(base3,base2)%>%
        as.data.frame()
      
      
      
    }
    # Ahora toca agregar  el partido 
    # Esta base almacena el alcance y el partido 
  
    #base5 = base3%>%
    #  mutate(partido_base = partidoi)%>%
    #  as.data.frame()
    # Esta base se va a ir llenando con diferentes alcances y repite el partido
    
    # Despues de pasar por los tres alcances en el mismo partido, 
    # Ahora va a cambiar de partido y hacer lo mismo para el siguiente 
    
  }
  
  # Aqui se van almacenando por partido 
  base6 = smartbind(base6,base3)
  
}


ayuntamientos = base6

#write.csv(ayuntamientos, "C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/ayuntamientos_pfs.csv",row.names = FALSE)
### ----------- Diputados Locales ---------------------------------------

#eleccion_t = "diputados"
eleccion_2 = "Diputados"
partidos = c("PAN","PRI","PRD","PT","PVEM","MC","MORENA","PANAL","PES")
alcances = c("Basico","Intermedio","Completo")


columnas_b3 = c("id_seccion","partido","votos","tipo",
                "participacion","partidos_nuevo","partido_base")
base3 = data.frame(matrix(nrow = 0, ncol = length(columnas_b3))) 
colnames(base3) <- columnas_b3

columnas_b6 = c("id_seccion","partido","votos","tipo",
                "participacion","partidos_nuevo","tipo_alcance",
                "partido_base")
base6 = data.frame(matrix(nrow = 0, ncol = length(columnas_b6))) 
colnames(base6) <- columnas_b6




for (partidoi in partidos){
  
  columnas_b3 = c("id_seccion","partido","votos","tipo","participacion","partidos_nuevo","partido_base")
  base3 = data.frame(matrix(nrow = 0, ncol = length(columnas_b3))) 
  colnames(base3) <- columnas_b3
  
  
  for (alcance in alcances){
    # alcance = "Basico"
    # partidoi = "PAN"
    # #eleccion_t = "ayuntamientos"
    # eleccion_2 = "Diputados"
    
    secciones = datos%>%
      filter(eleccion == eleccion_2,
             alcalnce == alcance,
             partido == partidoi)%>%
      pull(id_seccion)%>%
      unique()
    
    
    base1 = prediccion_votos%>%
      filter(tipo == "Diputados Locales",
             id_seccion %in% secciones)
    
    for(seccion in secciones){
      
      partidos_original = base1%>%
        filter(id_seccion == seccion)%>%
        arrange(desc(votos))%>%
        pull(partido)
      
      partidos_nuevo = append(partidos_original[!partidos_original == partidoi], partidoi, 0)
      
      # Itera las secciones  
      base2 = base1%>%
        filter(id_seccion == seccion)%>%
        arrange(desc(votos))%>%
        cbind(partidos_nuevo)%>%
        mutate(tipo_alcance = alcance,
               partido_base = partidoi)%>%
        as.data.frame()
      
      # Va almacenando las secciones con una nuestra estructura de partidos
      # En base 3 se van juntando las secciones 
      # Se termina base 3 cuando corrieron todas las secciones 
      base3 = smartbind(base3,base2)%>%
        as.data.frame()
      
      
      
    }
    # Ahora toca agregar el el partido 
    # Esta base almacena el alcance y el partido 
    
    # base5 = base3%>%
    #   mutate(partido_base = partidoi)%>%
    #   as.data.frame()
    # Esta base se va a ir llenando con diferentes alcances y repite el partido
    
    # Despues de pasar por los tres alcances en el mismo partido, 
    # Ahora va a cambiar de partido y hacer lo mismo para el siguiente 
    
  }
  
  # Aqui se van almacenando por partido 
  base6 = smartbind(base6,base3)
  
}


diputados = base6

#write.csv(diputados, "C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/diputados_pfs.csv",row.names = FALSE)

### ----------- Gobernador ---------------------------------------

#eleccion_t = "gobernador"
eleccion_2 = "Gobernador"
partidos = c("PAN","PRI","PRD","PT","PVEM","MORENA","PANAL")
alcances = c("Basico","Intermedio","Completo")


columnas_b3 = c("id_seccion","partido","votos","tipo","participacion","partidos_nuevo","partido_base")
base3 = data.frame(matrix(nrow = 0, ncol = length(columnas_b3))) 
colnames(base3) <- columnas_b3

columnas_b6 = c("id_seccion","partido","votos","tipo",
                "participacion","partidos_nuevo","tipo_alcance",
                "partido_base")
base6 = data.frame(matrix(nrow = 0, ncol = length(columnas_b6))) 
colnames(base6) <- columnas_b6




for (partidoi in partidos){
  columnas_b3 = c("id_seccion","partido","votos","tipo","participacion","partidos_nuevo","partido_base")
  base3 = data.frame(matrix(nrow = 0, ncol = length(columnas_b3))) 
  colnames(base3) <- columnas_b3
  
  for (alcance in alcances){
    #alcance = "Basico"
    #partidoi = "PAN"
    
    #eleccion_2 = "Ayuntamientos"
    
    secciones = datos%>%
      filter(eleccion == eleccion_2,
             alcalnce == alcance,
             partido == partidoi)%>%
      pull(id_seccion)%>%
      unique()
    
    
    base1 = prediccion_votos%>%
      filter(tipo == eleccion_2,
             id_seccion %in% secciones)
    
    for(seccion in secciones){
      
      partidos_original = base1%>%
        filter(id_seccion == seccion)%>%
        arrange(desc(votos))%>%
        pull(partido)
      
      partidos_nuevo = append(partidos_original[!partidos_original == partidoi], partidoi, 0)
      
      # Itera las secciones  
      base2 = base1%>%
        filter(id_seccion == seccion)%>%
        arrange(desc(votos))%>%
        cbind(partidos_nuevo)%>%
        mutate(tipo_alcance = alcance,
               partido_base = partidoi)%>%
        as.data.frame()
      
      # Va almacenando las secciones con una nuestra estructura de partidos
      # En base 3 se van juntando las secciones 
      # Se termina base 3 cuando corrieron todas las secciones 
      base3 = smartbind(base3,base2)%>%
        as.data.frame()
      
      
      
    }
    # Ahora toca agregar el el partido 
    # Esta base almacena el alcance y el partido 
    
    # base5 = base3%>%
    #   mutate(partido_base = partidoi)%>%
    #   as.data.frame()
    # Esta base se va a ir llenando con diferentes alcances y repite el partido
    
    # Despues de pasar por los tres alcances en el mismo partido, 
    # Ahora va a cambiar de partido y hacer lo mismo para el siguiente 
    
  }
  
  # Aqui se van almacenando por partido 
  base6 = smartbind(base6,base3)
  
}


gobernador = base6

#write.csv(gobernador, "C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/gobernador_pfs.csv",row.names = FALSE)





##################### Unir las bases en una sola ###########################
# ayuntamientos <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/ayuntamientos_pfs.csv")%>%
#   as.data.frame()
# diputados <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/diputados_pfs.csv")%>%
#   as.data.frame()
# goberandor <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/gobernador_pfs.csv")%>%
#   as.data.frame()
# 
# 
# general_primeras_fuerzas <- smartbind(ayuntamientos,diputados,goberandor)
# 
# #write.csv(general_primeras_fuerzas,"C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/general_pfs.csv", row.names = FALSE)
# 
# general_pf <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/general_pfs.csv")
# 
# 
# 
# ### Al general de las primeras fuerzas hay que agregarles los datos para hacer la regresion 
# all_prediction <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/all_prediction.csv")
# 
# # votos_prediccion <- all_prediction%>%
# #   filter(tipo == "Ayuntamientos")%>%
# #   pivot_wider(names_from = partido,values_from = votos)%>%
# #   rename(PAN_P = PAN,
# #          PRI_P = PRI,
# #          PRD_P = PRD,
# #          PT_P = PT,
# #          PVEM_P = PVEM,
# #          MC_P = MC,
# #          MORENA_P = MORENA
# #   )%>%
# #   select(-participacion,-tipo)
# 
# 
# dir <- 'C:/Users/Alexa/OneDrive - Next Gate Research/'
# agrupado <- read_csv(paste0(dir,"alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_dos/agrupados_15.csv"))
# 
# # Datos NSE
# nivel <- agrupado%>%
#   select(seccion,nivel)%>%
#   rename(id_seccion = seccion)%>%
#   unique()
# 
# # Grado escolaridad 
# grado <- agrupado%>%
#   select(seccion,graproes)%>%
#   rename(id_seccion = seccion)%>%
#   unique()%>%
#   mutate(graproes = round(graproes))
# 



