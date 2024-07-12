library(tidyverse)
library(readr)
library(sf)
library(gtools)

dir <- "~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/votos/CSV/"

#secciones_1 <- st_read(dsn = "C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Distritacion_secciones/15_MEXICO", layer= "Seccion")
municipio <- st_read(dsn = "~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/15-MUNICIPIOS", layer = "MUNICIPIO")
secciones <- st_read(dsn = "~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/15-MEXICO", layer = "SECCION")

secciones_df = as.data.frame(secciones)%>%
  janitor::clean_names()%>%
  select(entidad, municipio,seccion,distrito_l)%>%
  rename(id_entidad_R = entidad,
         id_municipio_R = municipio,
         id_seccion_R = seccion,
         id_distrito_R = distrito_l)


gobernador_2017 <- read_csv(paste0(dir,"gobernador_2017.csv"))%>%
  janitor::clean_names()%>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  group_by(seccion)%>%
  summarise(pan = sum(pan) ,
            pri = sum(pri) + round(sum(pri_pvem_nva_alianza_es)/4) + round(sum(pri_pvem_nva_alianza)/3)+ round(sum(pri_pvem_es)/3)+
              round(sum(pri_nva_alianza_es)/3)+ round(sum(pri_pvem)/2) + round(sum(pri_nva_alianza)/2)+ round(sum(pri_es)/2),
            prd = sum(prd),
            pt = sum(pt),
            pvem = sum(pvem)+ round(sum(pri_pvem_nva_alianza_es)/4) + round(sum(pri_pvem_nva_alianza)/3)+ round(sum(pri_pvem_es)/3)+
              round(sum(pri_pvem)/2) + round(sum(pvem_nva_alianza_es)/3) + round(sum(pvem_nva_alianza)/2) + round(sum(pvem_es)/2),
            nva = sum(nva_alianza) + round(sum(pri_pvem_nva_alianza_es)/4),
            morena = sum(morena),
            otros = sum(es),
            independientes = sum(cand_ind1),
            #no_registrados = sum(no_registrados),
            #nulos = sum(num_votos_nulos),
            #votos_validos = sum(votos_validos),
            total = sum(total_votos),
            lista_nominal = sum(lista_nominal))%>%
  filter(!seccion == "VMRE")%>%
  mutate(ano = "2017",
         tipo = "Gobernador",
         seccion = as.numeric(seccion))%>%
  na.omit()%>%
  rename(id_seccion = seccion)%>%
  left_join(secciones_df, by=c("id_seccion" = "id_seccion_R"))%>%
  as.data.frame()%>%
  filter(!is.na(id_seccion))%>%
  mutate(id_seccion = as.numeric(id_seccion))

glimpse(gobernador_2017)

### ---------- 2011

## UPT son : pri, verde y nueva alianza 
## UPM son : PRD, PT y convergencia 
gobernador_2011 <- read_csv(paste0(dir,"gobernador_2011.csv"))%>%
  janitor::clean_names()%>%
  select(-c("x15","x16"))%>%
  mutate_if(is.numeric , replace_na, replace = 0)%>%
  group_by(seccion)%>%
  summarise(pan = sum(pan),
            pri = round(sum(upt)/3),
            pvem = round(sum(upt)/3),
            prd = round(sum(upm)/3),
            pt = round(sum(upm)/3),
            nva = round(sum(upt)/3),
            nulos = sum(nulos),
            total = sum(total),
            lista_nominal = sum(lista_nominal))%>%
  mutate(ano = "2011",
         tipo = "Gobernador",
         seccion = as.numeric(seccion))%>%
  rename(id_seccion = seccion)%>%
  filter(!id_seccion == 0)%>%
  left_join(secciones_df, by=c("id_seccion" = "id_seccion_R"))%>%
  na.omit()%>%
  as.data.frame()


glimpse(gobernador_2011)

gobernador_general = smartbind(gobernador_2017,gobernador_2011)%>%
   mutate_if(is.numeric , replace_na, replace = 0) # Hay partidos que no estaban en años anteriores. NA es cero 
   







### SON DISTRITOS NO SECCIONES ----------------------------------------------------------
##--------------- 2005 --------------------------------------------
gobernador_2005 <- read_csv(paste0(dir,"gobernador_2005.csv"))%>%
  janitor::clean_names()%>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  group_by(seccion)%>%
  summarise(pan = round(sum(pan_convergencia/2)),
            pri = round(sum(alianza_por_mexico)/2),
            pvem = round(sum(alianza_por_mexico)/2),
            prd = round(sum(unidos_para_ganar)/2),
            pt = round(sum(unidos_para_ganar)/2),
            nulos = sum(nulos),
            total = sum(total),
            lista_nominal = sum(lista_nominal))%>%
  mutate(ano = "2011",
         tipo = "gobernador",
         seccion = as.numeric(seccion))%>%
  rename(id_seccion = seccion)%>%
  left_join(secciones_df, by=c("id_seccion" = "id_seccion_R"))%>%
  as.data.frame()


glimpse(gobernador_2005)

