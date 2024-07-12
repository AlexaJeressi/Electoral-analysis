library(tidyverse)
library(readr)
library(sf)
library("readxl")
library(gtools)

dir <- "~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/participacion/"
secciones <- st_read(dsn = "~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/15-MEXICO", layer = "SECCION")

secciones_df = as.data.frame(secciones)%>%
  janitor::clean_names()%>%
  select(entidad, municipio,seccion,distrito_l)%>% # Agregar distrito federal 
  rename(id_entidad_R = entidad,
         id_municipio_R = municipio,
         id_seccion_R = seccion,
         id_distrito_R = distrito_l)




participacion_2021 <- read_excel(paste0(dir,"p_2021.xlsx"))%>%
   janitor::clean_names()%>%
   filter(!row_number() %in% c(1))%>%
   select(!c("nombre_etq","tit_crq","participacion","abstencion","no_esp_percent"))%>%
   mutate_at(vars(lista_nominal:no_esp),function(x)as.numeric(gsub(",", ".", gsub("\\.", "",x))))%>%
  mutate(geounidad = as.numeric(geounidad),
         seccion = as.numeric(str_extract(nombre,"[:digit:]+")), # Igual no la usas. Que este en numerica
         ano = "2021", # Identificador de año 
         tipo = "participacion" # Para después saber si estamos hablando de participacion o de votos
         
         )%>%
  rename(id_seccion = seccion)%>%
  left_join(secciones_df, by=c("id_seccion" = "id_seccion_R"))%>%
  as.data.frame()



summary(participacion_2021$si_voto)


participacion_2018 <- read_excel(paste0(dir,"p_2018.xlsx"))%>%
  janitor::clean_names()%>%
  filter(!row_number() %in% c(1))%>%
  select(!c("nombre_etq","tit_crq","participacion","abstencion","no_esp_percent"))%>%
  mutate_at(vars(lista_nominal:no_esp),function(x)as.numeric(gsub(",", ".", gsub("\\.", "",x))))%>%
  mutate(geounidad = as.numeric(geounidad),
         seccion = as.numeric(str_extract(nombre,"[:digit:]+")),
         ano = "2018",
         tipo = "participacion")%>%
  rename(id_seccion = seccion)%>%
  left_join(secciones_df, by=c("id_seccion" = "id_seccion_R"))%>%
  as.data.frame()
  
participacion_2015 <- read_excel(paste0(dir,"p_2015.xlsx"))%>%
  janitor::clean_names()%>%
  filter(!row_number() %in% c(1))%>%
  select(!c("nombre_etq","tit_crq","participacion","abstencion","no_esp_percent"))%>%
  mutate_at(vars(lista_nominal:no_esp),function(x)as.numeric(gsub(",", ".", gsub("\\.", "",x))))%>%
  mutate(geounidad = as.numeric(geounidad),
         seccion = as.numeric(str_extract(nombre,"[:digit:]+")),
         ano = "2015",
         tipo = "participacion")%>%
  rename(id_seccion = seccion)%>%
  left_join(secciones_df, by=c("id_seccion" = "id_seccion_R"))%>%
  as.data.frame()

participacion_2012 <- read_excel(paste0(dir,"p_2012.xlsx"))%>%
  janitor::clean_names()%>%
  filter(!row_number() %in% c(1))%>%
  select(!c("nombre_etq","tit_crq","participacion","abstencion","no_esp_percent"))%>%
  mutate_at(vars(lista_nominal:no_esp),function(x)as.numeric(gsub(",", ".", gsub("\\.", "",x))))%>%
  mutate(geounidad = as.numeric(geounidad),
         seccion = as.numeric(str_extract(nombre,"[:digit:]+")),
         ano = "2012",
         tipo = "participacion")%>%
  rename(id_seccion = seccion)%>%
  left_join(secciones_df, by=c("id_seccion" = "id_seccion_R"))%>%
  as.data.frame()



participacion_general = smartbind(participacion_2021,participacion_2018,
                                  participacion_2015,participacion_2012)



dir <- "C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Bases/"

write.csv(participacion_general,paste0(dir,"participacion_general.csv"), row.names = FALSE)

############################################################################################
# Usar los datos abiertos para obtener:
# 1. La participación por rangos de edad 
# 2. La participacion por sexo 
# 3. La participacion en general 

# Para el estado de Mexico 
edomex <- read_csv("/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Nacional/ConteosCensales2021/datosabiertos_deceyec_conteoscensales2021_mex.csv")%>%
  janitor::clean_names()

edomex = edomex%>%
  mutate(
  rango_edad = case_when(
    edad <=29 ~ "1",
    edad >=30 & edad <=39 ~ "2",
    edad >=40 & edad <=49 ~ "3",
    edad >=50 & edad <=59 ~ "4",
    edad >=60 ~ "5"),
    nombre_rango = case_when(
      rango_edad == 1 ~ "18 a 29 años",
      rango_edad == 2 ~ "30 a 39 años",
      rango_edad == 3 ~ "40 a 49 años",
      rango_edad == 4 ~ "50 a 59 años",
      rango_edad == 5 ~ "60 y mas años")
  )%>%
  select(seccion,sexo,edad,rango_edad,nombre_rango,ln,sv,nv,ns)


names(edomex)

# Participacion por demograficos última información 2021  --------------------------------------

write.csv(edomex,"~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_dos/participacion.csv", row.names = FALSE)
