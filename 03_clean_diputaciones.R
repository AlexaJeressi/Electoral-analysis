library(tidyverse)
library(readr)
library(sf)

dir <- "~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/votos/CSV/"

municipio <- st_read(dsn = "~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/15-MUNICIPIOS", layer = "MUNICIPIO")

secciones <- st_read(dsn = "~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/15-MEXICO", layer = "SECCION")

secciones_df = as.data.frame(secciones)%>%
  janitor::clean_names()%>%
  select(entidad, municipio,seccion,distrito_l)%>%
  rename(id_entidad_R = entidad,
         id_municipio_R = municipio,
         id_seccion_R = seccion,
         id_distrito_R = distrito_l)

## ----------- 2021
diputados_2021 <- read_csv(paste0(dir,"diputados_2021.csv"))%>%
  janitor::clean_names()%>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  group_by(id_seccion)%>%
  summarise(pan = sum(pan) + round(sum(pan_pri_prd)/3) + round(sum(pan_pri)/2) + round(sum(pan_prd)/2),
            pri = sum(pri)+ round(sum(pan_pri_prd)/3) + round(sum(pan_pri)/2) + round(sum(pri_prd)/2),
            prd = sum(prd)+ round(sum(pan_pri_prd)/3) + round(sum(pan_prd)/2)+ round(sum(pri_prd)/2),
            pt = sum(pt) + round(sum(pt_morena)/2) + round(sum(pt_naem)/2) + round(sum(pt_morena_naem_22)/3) + round(sum(pt_morena_naem_26)/3),
            pvem = sum(pvem),
            mc = sum(mc),
            morena = sum(morena)+ round(sum(pt_morena)/2)+ round(sum(morena_naem)/2) + round(sum(pt_morena_naem_22)/3) + round(sum(pt_morena_naem_26)/3),
            nva = sum(naem) + round(sum(pt_naem)/2) + round(sum(morena_naem)/2) + round(sum(pt_morena_naem_22)/3) + round(sum(pt_morena_naem_26)/3),
            pes = sum(pes),
            #rsp = sum(rsp),
            #fxm = sum(fxm),
            otros = sum(rsp,fxm),
            #no_registrados = sum(no_registrados),
            #nulos = sum(nulos),
            total = sum(total),
            #lista_nominal = sum(lista_nominal)
  )%>%
  mutate(ano = "2021",
         tipo = "Diputados Locales")%>%
  left_join(secciones_df, by=c("id_seccion" = "id_seccion_R"))%>%
  as.data.frame()


glimpse(diputados_2021)

## ---------  2018 
diputados_2018 <- read_csv(paste0(dir,"diputados_2018.csv"))%>%
  janitor::clean_names()%>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  group_by(seccion)%>%
  summarise(pan = sum(pan) + round(sum(pan_prd_mc)/3) + round(sum(pan_prd)/2) + round(sum(pan_mc)/2),
            pri = sum(pri),
            prd = sum(prd) + round(sum(pan_prd_mc)/3) + round(sum(pan_prd)/2) + round(sum(prd_mc)/2),
            pt = sum(pt) + round(sum(pt_morena_es)/3) + round(sum(pt_es)/2),
            pvem = sum(pvem),
            mc = sum(mc)+ round(sum(pan_prd_mc)/3) + round(sum(pan_mc)/2) + round(sum(prd_mc)/2),
            morena = sum(morena) + round(sum(pt_morena_es)/3) + round(sum(morena_es)/2),
            nva = sum(na),
            #es = sum(es),
            #vr = sum(vr),
            otros = sum(es,vr),
            independientes = sum(cand_ind1),
            #no_registrados = sum(no_registrados),
            #nulos = sum(num_votos_nulos),
            total = sum(total_votos),
            #lista_nominal = sum(lista_nominal)
  )%>%
  mutate(ano = "2018",
         tipo = "Diputados Locales")%>%
  rename(id_seccion = seccion)%>%
  left_join(secciones_df, by=c("id_seccion" = "id_seccion_R"))%>%
  as.data.frame()
  

glimpse(diputados_2018)

## ----------- 2015
diputados_2015 <- read_csv(paste0(dir,"diputados_2015.csv"))%>%
  janitor::clean_names()%>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  group_by(seccion)%>%
  summarise(pan = sum(pan) ,
            pri = sum(pri) + round(sum(pri_pvem)/2),
            prd = sum(prd) ,
            pt = sum(pt),
            pvem = sum(pvem) + round(sum(pri_pvem)/2),
            mc = sum(mc),
            morena = sum(morena),
            nva = sum(na),
            pes = sum(pes),
            #ph = sum(ph),
            #pfd = sum(pfd),
            otros = sum(ph,pfd),
            independientes = sum(candidato_independiente),
            #no_registrados = sum(no_registrados),
            #nulos = sum(nulos),
            total = sum(total)
            #lista_nominal = sum(lista_nominal)
  )%>%
  mutate(ano = "2015",
         tipo = "Diputados Locales")%>%
  rename(id_seccion = seccion)%>%
  left_join(secciones_df, by=c("id_seccion" = "id_seccion_R"))%>%
  as.data.frame()

glimpse(diputados_2015)


diputaciones_general = smartbind(diputados_2021,
                                 diputados_2018,
                                 diputados_2015)

# Los NA significan que ese partido no participo ese por lo que mejor se cambian a cero 
diputaciones_general[is.na(diputaciones_general)] = 0

#write.csv(diputaciones_general, "C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Bases/diputaciones_general.csv", row.names = FALSE)



