library(tidyverse)

dir <- '/Users/alexa/Library/CloudStorage/OneDrive-NextGateResearch/'

## Bases de datos votos 
ayuntamientos <- read_csv(paste0(dir,"alexa/2022/10_Plataforma/Datos/Estado de Mexico/Bases/ayuntamientos_general.csv"))
diputaciones <- read_csv(paste0(dir,"alexa/2022/10_Plataforma/Datos/Estado de Mexico/Bases/diputaciones_general.csv"))
gobernador <- read_csv(paste0(dir,"alexa/2022/10_Plataforma/Datos/Estado de Mexico/Bases/gobernador_general.csv"))


# Paso 1. Obtener los porcentajes de cada partido por seccion ------------------------------

# ayuntamientos_fake = ayuntamientos%>%
#   filter(ano == 2021)%>%
#   select(id_municipio_R,ano,pan,pri,prd,pt,pvem,mc,nva,morena,pes,total)%>%
#   group_by(id_municipio_R)%>%
#   summarise(across(everything(), list(sum)))%>%
#   mutate(p_pan = (pan_1/total_1)*100,
#          p_pri = (pri_1/total_1)*100,
#          p_prd = (prd_1/total_1)*100,
#          p_pt = (pt_1/total_1)*100,
#          p_pvem = (pvem_1/total_1)*100,
#          p_mc = (mc_1/total_1)*100,
#          p_nva = (nva_1/total_1)*100,
#          p_morena = (morena_1/total_1)*100,
#          p_pes = (pes_1/total_1)*100
#          #p_otros = otros/total,
#          #p_indepen = independientes/total
#   )







ayuntamientos = ayuntamientos%>%
  mutate(p_pan = pan/total,
         p_pri = pri/total,
         p_prd = prd/total,
         p_pt = pt/total,
         p_pvem = pvem/total,
         p_mc = mc/total,
         p_nva = nva/total,
         p_morena = morena/total,
         p_pes = pes/total
         #p_otros = otros/total,
         #p_indepen = independientes/total
         )

diputaciones = diputaciones%>%
  mutate(p_pan = pan/total,
         p_pri = pri/total,
         p_prd = prd/total,
         p_pt = pt/total,
         p_pvem = pvem/total,
         p_mc = mc/total,
         p_nva = nva/total,
         p_morena = morena/total,
         p_pes = pes/total
         #otros = otros/total
         )

gobernador = gobernador%>%
  mutate(p_pan = pan/total,
         p_pri = pri/total,
         p_prd = prd/total,
         p_pt = pt/total,
         p_pvem = pvem/total,
         p_nva = nva/total,
         #p_mc = mc/total,
         p_morena = morena/total
         )


# Paso 2. Crear los tres indicadores ------------------------------------------------
#------------------------------------ AYUNTAMIENTOS ---------------------------------
# ayuntamientos2_fake = ayuntamientos_fake%>%
#   select(id_municipio_R,p_pan,p_pri,p_prd,p_pt,p_pvem,p_mc,p_nva,p_morena,p_pes)%>%
#   gather("partido","porcentaje",-c(id_municipio_R))%>%
#   group_by(id_municipio_R)%>%
#   arrange(desc(porcentaje))%>%
#   # La posicion es el primer indicador 
#   mutate(posicion = row_number()
#          )%>%
#   na.omit()
#   
# ayuntamientos2_fake <-   ayuntamientos2_fake%>%  
#   group_by(id_municipio_R)%>%
#   arrange(desc(porcentaje))%>%
#   mutate(diferencia = porcentaje - lag(porcentaje) )%>%
#   left_join(shp_ayuntamientos, by = "id_municipio_R")%>%
#   as.data.frame()%>%
#   select(ID,posicion,porcentaje,diferencia,partido)%>%
#   arrange(desc(ID))
# 
# getwd()
# write.csv(ayuntamientos2_fake,"/Users/alexa/Library/CloudStorage/OneDrive-NextGateResearch/alexa/2022/10_Plataforma/posiciones.csv", row.names = FALSE
#             )
# 
# names(ayuntamientos2_fake)










ayuntamientos2 = ayuntamientos%>%
  select(id_seccion,ano,p_pan,p_pri,p_prd,p_pt,p_pvem,p_mc,p_nva,p_morena,p_pes)%>%
  gather("partido","porcentaje",-c(id_seccion,ano))%>%
  group_by(id_seccion,ano)%>%
  arrange(desc(porcentaje))%>%
  # La posicion es el primer indicador 
  mutate(posicion = row_number(),
         # La clasificacion del porcentaje es el segundo indicador 
         niv_porcentaje = case_when(porcentaje <= 0.10 ~ 1,
                                porcentaje > 0.10 & porcentaje <= 0.25 ~ 2,
                                porcentaje > 0.25 & porcentaje <= 0.50 ~ 3,
                                porcentaje > 0.50 & porcentaje <= 0.75 ~ 4,
                                porcentaje > 0.75 ~ 5
                                ))%>%
  na.omit()%>%
  ungroup()

ayuntamientos3 = ayuntamientos2%>%
  group_by(id_seccion,ano,partido)%>%
  mutate(historico_1 = ifelse(posicion == 1,2,
                              ifelse(posicion == 2,1,
                                     0)))%>%
  ungroup()%>%
  group_by(id_seccion,partido)%>%
  mutate(historico_fin1 = sum(historico_1)) %>%
  # La clasificacion del historico es el tercer indicador
  mutate(historico_fin = case_when(historico_fin1 == 0 ~ 1,
                                    historico_fin1 == 1 ~ 2,
                                    historico_fin1 == 2 ~ 3,
                                    historico_fin1 == 3 ~ 4,
                                    historico_fin1 == 4 ~ 5,
                                    historico_fin1 == 5 ~ 6,
                                    historico_fin1 == 6 ~ 7))%>%
  filter(ano == 2021)%>%
  select(id_seccion,partido,posicion,niv_porcentaje,historico_fin)
 


# Ayuntamientos pri 
a_pan = ayuntamientos3%>%
  filter(partido == "p_pan")
a_pri = ayuntamientos3%>%
  filter(partido == "p_pri")
a_prd = ayuntamientos3%>%
  filter(partido == "p_prd")
a_pt = ayuntamientos3%>%
  filter(partido == "p_pt")
a_pvem = ayuntamientos3%>%
  filter(partido == "p_pvem")
a_mc = ayuntamientos3%>%
  filter(partido == "p_mc")
a_panal = ayuntamientos3%>%
  filter(partido == "p_nva")
a_morena = ayuntamientos3%>%
  filter(partido == "p_morena")
a_pes = ayuntamientos3%>%
  filter(partido == "p_pes")

#------------------------------------ DIPUTACIONES ---------------------------------
diputaciones2 = diputaciones%>%
  select(id_seccion,ano,p_pan,p_pri,p_prd,p_pt,p_pvem,p_mc,p_nva,p_morena,p_pes)%>%
  gather("partido","porcentaje",-c(id_seccion,ano))%>%
  group_by(id_seccion,ano)%>%
  arrange(desc(porcentaje))%>%
  # La posicion es el primer indicador 
  mutate(posicion = row_number(),
         # La clasificacion del porcentaje es el segundo indicador 
         niv_porcentaje = case_when(porcentaje <= 0.10 ~ 1,
                                    porcentaje > 0.10 & porcentaje <= 0.25 ~ 2,
                                    porcentaje > 0.25 & porcentaje <= 0.50 ~ 3,
                                    porcentaje > 0.50 & porcentaje <= 0.75 ~ 4,
                                    porcentaje > 0.75 ~ 5
         ))%>%
  na.omit()%>%
  ungroup()

diputaciones3 = diputaciones2%>%
  group_by(id_seccion,ano,partido)%>%
  mutate(historico_1 = ifelse(posicion == 1,2,
                              ifelse(posicion == 2,1,
                                     0)))%>%
  ungroup()%>%
  group_by(id_seccion,partido)%>%
  mutate(historico_fin1 = sum(historico_1)) %>%
  # La clasificacion del historico es el tercer indicador
  mutate(historico_fin = case_when(historico_fin1 == 0 ~ 1,
                                   historico_fin1 == 1 ~ 2,
                                   historico_fin1 == 2 ~ 3,
                                   historico_fin1 == 3 ~ 4,
                                   historico_fin1 == 4 ~ 5,
                                   historico_fin1 == 5 ~ 6,
                                   historico_fin1 == 6 ~ 7))%>%
  filter(ano == 2021)%>%
  select(id_seccion,partido,posicion,niv_porcentaje,historico_fin)



# Ayuntamientos pri 
d_pan = diputaciones3%>%
  filter(partido == "p_pan")
d_pri = diputaciones3%>%
  filter(partido == "p_pri")
d_prd = diputaciones3%>%
  filter(partido == "p_prd")
d_pt = diputaciones3%>%
  filter(partido == "p_pt")
d_pvem = diputaciones3%>%
  filter(partido == "p_pvem")
d_mc = diputaciones3%>%
  filter(partido == "p_mc")
d_panal = diputaciones3%>%
  filter(partido == "p_nva")
d_morena = diputaciones3%>%
  filter(partido == "p_morena")
d_pes = diputaciones3%>%
  filter(partido == "p_pes")

#------------------------------------ GOBERNADOR ---------------------------------
gobernador2 = gobernador%>%
  select(id_seccion,ano,p_pan,p_pri,p_prd,p_pt,p_pvem,p_nva,p_morena)%>%
  gather("partido","porcentaje",-c(id_seccion,ano))%>%
  group_by(id_seccion,ano)%>%
  arrange(desc(porcentaje))%>%
  # La posicion es el primer indicador 
  mutate(posicion = row_number(),
         # La clasificacion del porcentaje es el segundo indicador 
         niv_porcentaje = case_when(porcentaje <= 0.10 ~ 1,
                                    porcentaje > 0.10 & porcentaje <= 0.25 ~ 2,
                                    porcentaje > 0.25 & porcentaje <= 0.50 ~ 3,
                                    porcentaje > 0.50 & porcentaje <= 0.75 ~ 4,
                                    porcentaje > 0.75 ~ 5
         ))%>%
  na.omit()%>%
  ungroup()

gobernador3 = gobernador2%>%
  group_by(id_seccion,ano,partido)%>%
  mutate(historico_1 = ifelse(posicion == 1,2,
                              ifelse(posicion == 2,1,
                                     0)))%>%
  ungroup()%>%
  group_by(id_seccion,partido)%>%
  mutate(historico_fin1 = sum(historico_1)) %>%
  # La clasificacion del historico es el tercer indicador
  mutate(historico_fin = case_when(historico_fin1 == 0 ~ 1,
                                   historico_fin1 == 1 ~ 2,
                                   historico_fin1 == 2 ~ 3,
                                   historico_fin1 == 3 ~ 4,
                                   historico_fin1 == 4 ~ 5,
                                   historico_fin1 == 5 ~ 6,
                                   historico_fin1 == 6 ~ 7))%>%
  filter(ano == 2017)%>%
  select(id_seccion,partido,posicion,niv_porcentaje,historico_fin)



# Ayuntamientos pri 
g_pan = gobernador3%>%
  filter(partido == "p_pan")
g_pri = gobernador3%>%
  filter(partido == "p_pri")
g_prd = gobernador3%>%
  filter(partido == "p_prd")
g_pt = gobernador3%>%
  filter(partido == "p_pt")
g_pvem = gobernador3%>%
  filter(partido == "p_pvem")
g_panal = gobernador3%>%
  filter(partido == "p_nva")
g_morena = gobernador3%>%
  filter(partido == "p_morena")


## Guardar las bases para los indices 
save(a_pan,
        a_pri,
        a_prd,
        a_pt,
        a_pvem,
        a_mc,
     a_panal,
        a_morena,
     a_pes,
        d_pan,
        d_pri,
        d_prd,
        d_pt,
        d_pvem,
        d_mc,
     d_panal,
        d_morena,
     d_pes,
        g_pan,
        g_pri,
        g_prd,
        g_pt,
        g_pvem,
     g_panal,
        g_morena,file = "C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Datos_indices/datos_indices.RData")

