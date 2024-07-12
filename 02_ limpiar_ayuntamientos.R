library(tidyverse)
library(readr)
library(sf)
library(gtools)

dir <- "/Users/alexa/Library/CloudStorage/OneDrive-NextGateResearch/alexa/2022/10_Plataforma/Datos/Estado de Mexico/"

#secciones_1 <- st_read(dsn = "C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Distritacion_secciones/15_MEXICO", layer= "Seccion")
# municipio <- st_read(dsn = "/Users/alexa/Library/CloudStorage/OneDrive-NextGateResearch/alexa/2022/10_Plataforma/Datos/Estado de Mexico/15-MUNICIPIOS", layer = "MUNICIPIO")
# 
# municipio_df = municipio%>%
#   janitor::clean_names()%>%
#   rename(id_municipio_R = municipio)%>%
#   as.data.frame()%>%
#   select(-geometry,-entidad)


secciones <- st_read(dsn = "/Users/alexa/Library/CloudStorage/OneDrive-NextGateResearch/alexa/2022/10_Plataforma/Datos/Estado de Mexico/15-MEXICO", layer = "SECCION")

secciones_df = as.data.frame(secciones)%>%
  janitor::clean_names()%>%
  select(entidad, municipio,seccion,distrito_l)%>%
  rename(id_entidad_R = entidad,
         id_municipio_R = municipio,
         id_seccion_R = seccion,
         id_distrito_R = distrito_l)

## 2021 ------------------------------------------------------------------------------
ayuntamientos_2021 <- read_csv(paste0(dir,"votos/CSV/ayuntamientos_2021.csv"))%>% # Leer datos
  janitor::clean_names()%>% # Limpiar nombres de las columnas
  mutate_if(is.numeric , replace_na, replace = 0) %>% # Cambiar los NA a cero
  group_by(id_seccion)%>% # Vamos a agrupar por seccion para cada obtener los porcentajes de voto por partido para cada seccion. Esto aplica cuando lo datos estan a nivel casilla 
  # Numero de votos
  summarise(pan = sum(pan) + round(sum(pan_pri_prd)/3) + round(sum(pan_prd)/2),
            pri = sum(pri) + round(sum(pan_pri_prd)/3) + round(sum(pan_pri)/2) + round(sum(pri_prd)/2),
            prd = sum(prd) + round(sum(pan_pri_prd)/3) + round(sum(pan_prd)/2) + round(sum(pri_prd)/2),
            pt = sum(pt) + round(sum(pt_morena_naem)/3) + round(sum(pt_morena)/2) + round(sum(pt_naem)/2) + round(sum(pt_morena_naem_candidatura_comun)/4),
            pvem = sum(pvem),
            mc = sum(mc),
            nva = sum(naem) + round(sum(pt_morena_naem)/3) + round(sum(pt_naem)/2)  + round(sum(pt_morena_naem_candidatura_comun)/4),
            morena = sum(morena) + round(sum(pt_morena_naem)/3) + round(sum(pt_morena)/2)+ round(sum(morena_naem)/2) + round(sum(pt_morena_naem_candidatura_comun)/4),
            pes = sum(pes),
            #rsp = sum(rsp),
            #fxm = sum(fxm),
            otros = sum(rsp,fxm), # En otros ponemos los partidos que ya no existen. RSP, FXM, PES, etc.
            independientes = sum(across(("c_i_1":"c_i_15"))), # Suma los votos de los independientes. 
                                                              # En caso de que los independientes 
            #no_registrados = sum(no_registrados),
            #nulos = sum(nulos),
            #votos_validos = sum(votos_validos),
            total = sum(total),                     # Suma el total de los votos por seccion 
            lista_nominal = sum(lista_nominal)
            )%>%
  mutate(ano = "2021",
         tipo = "Ayuntamientos")%>%
  left_join(secciones_df, by=c("id_seccion" = "id_seccion_R"))%>%
  #na.omit()%>%
  as.data.frame()
  

  

glimpse(ayuntamientos_2021)

## 2018 -----------------------------------------------------------------------------
ayuntamientos_2018 <- read_csv(paste0(dir,"votos/CSV/ayuntamiento_2018.csv"))%>%
  janitor::clean_names()%>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  group_by(seccion)%>%
  summarise(pan = sum(pan) + round(sum(pan_prd_mc)/3) + round(sum(pan_prd)/2) + round(sum(pan_mc)/2),
            pri = sum(pri),
            prd = sum(prd)+ round(sum(pan_prd_mc)/3) + round(sum(pan_prd)/2) + round(sum(prd_mc)/2),
            pt = sum(pt) + round(sum(pt_morena_es)/3) + round(sum(pt_morena)/2)+ round(sum(pt_es)/2),
            pvem = sum(pvem),
            mc = sum(mc)+ round(sum(pan_prd_mc)/3)+ + round(sum(pan_mc)/2)+ round(sum(prd_mc)/2),
            morena = sum(morena)+ round(sum(pt_morena_es)/3)+ round(sum(pt_morena)/2)+ round(sum(morena_es)/2),
            nva = sum(na),
            #vr = sum(vr),
            #es = sum(vr),
            otros = sum(vr,es),
            independientes = sum(across(cand_ind2:cand_ind19)),
            #no_registrados = ,
            #nulos = sum(num_votos_nulos),
            #votos_validos = sum(num_votos_validos),
            total = sum(total_votos)
            #lista_nominal = sum(lista_nominal)
            )%>%
  mutate(ano = "2018",
         tipo = "Ayuntamientos")%>%
  rename(id_seccion = seccion)%>%
  left_join(secciones_df, by=c("id_seccion" = "id_seccion_R"))%>%
  na.omit()%>%
  as.data.frame()

glimpse(ayuntamientos_2018)

## 2015 ------------------------------------------------------------------------------
ayuntamientos_2015 <- read_csv(paste0(dir,"votos/CSV/ayuntamientos_2015.csv"))%>%
  janitor::clean_names()%>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  group_by(seccion)%>%
  summarise(pan = sum(pan) + round(sum(pan_pt)/2),
            pri = sum(pri) + round(sum(pri_pvem_na)/3) + round(sum(pri_pvem)/2) + round(sum(pri_na)/2),
            prd = sum(prd),
            pt = sum(pt) + round(sum(pan_pt)/2),
            pvem = sum(pvem)+ round(sum(pri_pvem_na)/3)+ round(sum(pri_pvem)/2) + sum(pvem_na)/2,
            mc = sum(mc),
            morena = sum(morena),
            nva = sum(na) + round(sum(pri_pvem_na)/3),
            pes = sum(pes),
            #ph = sum(ph),
            #pfd = sum(pfd),
            otros = sum(ph,pfd),
            independientes = sum(across(candidato_independiente_21:candidato_independiente_22)),
            #no_registrados = sum(no_registrados),
            #nulos = sum(nulos),
            #votos_validos = sum(num_votos_validos),
            total = sum(total)
            #lista_nominal = sum(lista_nominal
            )%>%
  mutate(ano = "2015",
         tipo = "Ayuntamientos")%>%
  rename(id_seccion = seccion)%>%
  left_join(secciones_df, by=c("id_seccion" = "id_seccion_R"))%>%
  as.data.frame()

glimpse(ayuntamientos_2015)




## 2012 -----------------------------------------------------------------------------
ayuntamientos_2012 <- read_csv(paste0(dir,"votos/CSV/ayuntamientos_2012.csv"))%>%
  janitor::clean_names()%>%
  mutate(ano = "2012")%>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  group_by(seccion)%>%
  summarise(pan = sum(pan) ,
            pri = round(sum(pri_pvem_na)/3) ,
            prd = sum(prd) + round(sum(prd_pt_mc)/3) + round(sum(prd_pt)/2) + round(sum(prd_mc)/2),
            pt = sum(pt) + round(sum(pt_mc)/2) + round(sum(prd_pt_mc)/3) + round(sum(prd_pt)/2),
            pvem = round(sum(pri_pvem_na)/3),
            mc = sum(m_c) + round(sum(prd_pt_mc)/3) + round(sum(prd_mc)/2) + round(sum(pt_mc)/2),
            nva = round(sum(pri_pvem_na)/3),
            #no_registrados = sum(no_reg),
            #nulos = sum(nulos),
            total = sum(total)
            
  )%>%
  mutate(ano = "2012",
         tipo = "Ayuntamientos")%>%
  rename(id_seccion = seccion)%>%
  left_join(secciones_df, by=c("id_seccion" = "id_seccion_R"))%>%
  as.data.frame()

# Para la plataforma solo usaremos tres años 
ayuntamientos_general = smartbind(ayuntamientos_2021,
                                  ayuntamientos_2018,
                                  ayuntamientos_2015)
# Los NA significan que ese partido no participo ese por lo que mejor se cambian a cero 
ayuntamientos_general[is.na(ayuntamientos_general)] = 0

write.csv(ayuntamientos_general, "/Users/alexa/Library/CloudStorage/OneDrive-NextGateResearch/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Bases/ayuntamientos_general.csv", row.names = FALSE)











# diputados_2012 <- read_csv(paste0(dir,"diputados_2012.csv"))%>%
#   janitor::clean_names()%>%
#   mutate(ano = "2012")

## 2009 -----------------------------------------------------------------------------
ayuntamientos_2009 <- read_csv(paste0(dir,"ayuntamiento_2009.csv"))%>%
  janitor::clean_names()%>%
  mutate(ano = "2009")

diputados_2009 <- read_csv(paste0(dir,"diputados_2009.csv"))%>%
  janitor::clean_names()%>%
  mutate(ano = "2009")

## 2006 -----------------------------------------------------------------------------
ayuntamientos_2006 <- read_csv(paste0(dir,"ayuntamiento_2006.csv"))%>%
  janitor::clean_names()%>%
  mutate(ano = "2006")

diputados_2006 <- read_csv(paste0(dir,"diputados_2006.csv"))%>%
  janitor::clean_names()%>%
  mutate(ano = "2006")

## 2003 -----------------------------------------------------------------------------
ayuntamientos_2003 <- read_csv(paste0(dir,"ayuntamiento_2003.csv"))%>%
  janitor::clean_names()%>%
  mutate(ano = "2003")

diputados_2003 <- read_csv(paste0(dir,"diputados_2003.csv"))%>%
  janitor::clean_names()%>%
  mutate(ano = "2003")

## 2000 ------------------------------------------------------------------------------
ayuntamientos_2000 <- read_csv(paste0(dir,"ayuntamiento_2000.csv"))%>%
  janitor::clean_names()%>%
  mutate(ano = "2000")

diputados_2000 <- read_csv(paste0(dir,"diputados_2000.csv"))%>%
  janitor::clean_names()%>%
  mutate(ano = "2000")


