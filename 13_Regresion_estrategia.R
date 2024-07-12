library(tidyverse)

# Le vamos a agregar a los datos de primeras fuerzas los demográficos para 
# que se pueda hacer la regresion en el shiny 

# Datos de las primeras fuerzas por alcance y por partido creados en el scrip 12
ayuntamientos <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/ayuntamientos_pfs.csv")%>%
  as.data.frame()
diputados <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/diputados_pfs.csv")%>%
  as.data.frame()
goberandor <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/gobernador_pfs.csv")%>%
  as.data.frame()


general_primeras_fuerzas <- smartbind(ayuntamientos,diputados,goberandor)


# Datos de participacion de la ultima eleccion igual que datos demográficos
dir <- 'C:/Users/Alexa/OneDrive - Next Gate Research/'
agrupado <- read_csv(paste0(dir,"alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_dos/agrupados_15.csv"))

# Ajustar los grupos de edad 
agrupado <- agrupado%>%
  mutate(grupo2 = case_when(
    edad < 30 ~ "Edad1", # Hasta 29 
    edad >= 30 & edad <= 55 ~ "Edad2", # De 30 a 56
    edad >=56 ~ "Edad3" # De 56 en adela andelante
  ))

#table(agrupado$grupo2)
# Datos NSE
nivel <- agrupado%>%
  select(seccion,nivel)%>%
  rename(id_seccion = seccion)%>%
  unique()

# Grado escolaridad 
grado <- agrupado%>%
  select(seccion,graproes)%>%
  rename(id_seccion = seccion)%>%
  unique()%>%
  mutate(graproes = round(graproes))

# Participacion edades
participacion_wider0 <- agrupado%>%
  select(seccion,grupo2,sexo,sv,ln)%>%
  group_by(seccion,sexo,grupo2)%>%
  summarise(votos = sum(sv))%>%
  mutate(sexo = case_when(
    sexo == 0 ~ "Hombre",
    sexo == 1 ~ "Mujer"))%>%
  pivot_wider(names_from = grupo2, values_from = votos)%>%
  ungroup()

participacion_wider1 <- participacion_wider0%>%
  mutate(votos = rowSums(across(Edad1:Edad3)))%>%
  select(-c(Edad1:Edad3))%>%
  pivot_wider(names_from = sexo, values_from = votos)%>%
  ungroup()%>%
  mutate(
    total =  Hombre+Mujer,
    porcentaje_h = Hombre/total,
    porcentaje_m = Mujer/total
  )

participacion_wider0 = participacion_wider0%>%
  select(-sexo)%>%
  group_by(seccion)%>%
  summarise(across(everything(), list(sum)))

participacion_wider <- left_join(participacion_wider0,participacion_wider1, by="seccion")%>%
  rename(id_seccion = seccion)




general_pf_completo <- general_primeras_fuerzas%>%
  left_join(grado, by="id_seccion")%>%
  left_join(nivel, by= "id_seccion")%>%
  mutate(nivel = as.factor(nivel))%>%
  left_join(participacion_wider, by = "id_seccion")



write.csv(general_pf_completo,"C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/generl_pf_regresion.csv",row.names = FALSE)































# Data all tiene los datos sobre los votos por partido. 
# Necesitamos solo el de 2021
# data_all <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Bases/base_votos_elecciones.csv")
# 
# # Estos son los resultados de las elecciones de 2021 y 2017
# 
# ayuntamientos_2021 <- data_all%>%
#   filter(ano == 2021,
#          tipo == "ayuntamientos")
# 
# diputados_2021 <- data_all%>%
#   filter(ano == 2021,
#          tipo == "diputados")
# 
# gobernador_2017 <- data_all%>%
#   filter(ano == 2017,
#          tipo == "gobernador")
# 

# Leer datos agrupado que ya tiene los datos demograficos del censo y 
# los datos de participacion por sexo y edad 
# ademas del nivel socioecnomico 
# Con estos datos no vamos a quedar solo con los demograficos de la participacion
dir <- 'C:/Users/Alexa/OneDrive - Next Gate Research/'
agrupado <- read_csv(paste0(dir,"alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_dos/agrupados_15.csv"))

# Ajustar los grupos de edad 
agrupado <- agrupado%>%
  mutate(grupo2 = case_when(
    edad <= 29 ~ "Edad1",
    edad >=30 & edad <= 55 ~ "Edad2",
    edad >=56 ~ "Edad3"
  ))

participacion_wider0 <- agrupado%>%
  select(seccion,grupo2,sexo,sv,ln)%>%
  group_by(seccion,sexo,grupo2)%>%
  summarise(votos = sum(sv))%>%
  mutate(sexo = case_when(
    sexo == 0 ~ "Hombre",
    sexo == 1 ~ "Mujer"))%>%
  pivot_wider(names_from = grupo2, values_from = votos)%>%
  ungroup()

participacion_wider1 <- participacion_wider0%>%
  mutate(votos = rowSums(across(Edad1:Edad3)))%>%
  select(-c(Edad1:Edad3))%>%
  pivot_wider(names_from = sexo, values_from = votos)%>%
  ungroup()%>%
  mutate(
    total =  Hombre+Mujer,
    porcentaje_h = Hombre/total,
    porcentaje_m = Mujer/total
  )

participacion_wider0 = participacion_wider0%>%
  select(-sexo)%>%
  group_by(seccion)%>%
  summarise(across(everything(), list(sum)))

participacion_wider <- left_join(participacion_wider0,participacion_wider1, by="seccion")%>%
  rename(id_seccion = seccion)


# Votos partido ---------------------------------------------------------------------
# ayuntamientos_2021 <- ayuntamientos_2021%>%
#   select(id_seccion,partido,votos)%>%
#   unique()%>%
#   na.omit()%>%
#   pivot_wider(names_from = partido, values_from = votos)
# 
# diputados_2021 <- diputados_2021%>%
#   select(id_seccion,partido,votos)%>%
#   unique()%>%
#   na.omit()%>%
#   pivot_wider(names_from = partido, values_from = votos)
# 
# 
# gobernador_2017 <- gobernador_2017%>%
#   select(id_seccion,partido,votos)%>%
#   unique()%>%
#   na.omit()%>%
#   pivot_wider(names_from = partido, values_from = votos)
# 
# 
# # Obtener porcentaje de participacion ----------------------------------------
# participacion_anos <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/12_Edomex_elecciones/Datos/porcentaje_participacion.csv")
# 
# participacion_21 <- participacion_anos%>%
#   filter(ano == 2021)%>%
#   select(id_seccion, si_voto,lista_nominal,participacion)
# 
# participacion_17 <- participacion_anos%>%
#   filter(ano == 2017)%>%
#   select(id_seccion, si_voto,lista_nominal,participacion)

# Datos NSE
nivel <- agrupado%>%
  select(seccion,nivel)%>%
  rename(id_seccion = seccion)%>%
  unique()

# Grado escolaridad 
grado <- agrupado%>%
  select(seccion,graproes)%>%
  rename(id_seccion = seccion)%>%
  unique()%>%
  mutate(graproes = round(graproes))

# Unir los datos de participacion y numero de votos para cada partido 
ayuntamientos_completo21 <- inner_join(participacion_wider,ayuntamientos_2021, by = "id_seccion")%>%
  inner_join(participacion_21, by = "id_seccion")



diputados_completo21 <- inner_join(participacion_wider,ayuntamientos_2021, by = "id_seccion")%>%
   inner_join(participacion_21, by = "id_seccion")
 
 
goberandor_completo17 <- inner_join(participacion_17,diputados_2021, by = "id_seccion")



# Vamos a usar los votos predichos en vez de los votos reales en 2021
# Con base en los demográficos de 2021, vamos a tratar de explicar el voto predicho 
# Leer los datos de prediccion
# all_prediction <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/all_prediction.csv")

# votos_prediccion <- all_prediction%>%
#   filter(tipo == "Ayuntamientos")%>%
#   pivot_wider(names_from = partido,values_from = votos)%>%
#   rename(PAN_P = PAN,
#          PRI_P = PRI,
#          PRD_P = PRD,
#          PT_P = PT,
#          PVEM_P = PVEM,
#          MC_P = MC,
#          MORENA_P = MORENA
#   )%>%
#   select(-participacion,-tipo)


################################# MODELOS AYUNTAMIENTOS #####################################


ayuntamientos_completo21 <- ayuntamientos_completo21%>%
  left_join(grado, by="id_seccion")%>%
  left_join(votos_prediccion, by = "id_seccion")%>%
  #mutate_at(vars(Edad1_1:MORENA_P), ~(scale(.) ))%>%
  left_join(nivel, by= "id_seccion")%>%
  mutate(nivel = as.factor(nivel))

# Vamos a escalar los datos y hacerlos numericos 
library(scales)

ayuntamientos_completo21_est <- ayuntamientos_completo21%>%
  mutate_at(vars(Edad1_1:MORENA_P), ~(scale(.) ))%>%
  mutate_at(vars(Edad1_1:MORENA_P), ~(as.numeric(.) ))
  
  

ayuntamientos_completo21_est <- as.data.frame(ayuntamientos_completo21_est)




###################### ACOMODAR LA BASE DE DATOS GENERAL################################
# Unir los datos de la regresion a general pf 

general_pf <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/general_pfs.csv")


# Necesito agregar
# grado
# nivel 
# Datos de grupos de edad 

# dir <- 'C:/Users/Alexa/OneDrive - Next Gate Research/'
# agrupado <- read_csv(paste0(dir,"alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_dos/agrupados_15.csv"))
# 
# # Ajustar los grupos de edad 
# agrupado <- agrupado%>%
#   mutate(grupo2 = case_when(
#     edad < 30 ~ "Edad1", # Hasta 29 
#     edad >= 30 & edad <= 55 ~ "Edad2", # De 30 a 56
#     edad >=56 ~ "Edad3" # De 56 en adela andelante
#   ))
# 
# #table(agrupado$grupo2)
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
# # Participacion edades
# participacion_wider0 <- agrupado%>%
#   select(seccion,grupo2,sexo,sv,ln)%>%
#   group_by(seccion,sexo,grupo2)%>%
#   summarise(votos = sum(sv))%>%
#   mutate(sexo = case_when(
#     sexo == 0 ~ "Hombre",
#     sexo == 1 ~ "Mujer"))%>%
#   pivot_wider(names_from = grupo2, values_from = votos)%>%
#   ungroup()
# 
# participacion_wider1 <- participacion_wider0%>%
#   mutate(votos = rowSums(across(Edad1:Edad3)))%>%
#   select(-c(Edad1:Edad3))%>%
#   pivot_wider(names_from = sexo, values_from = votos)%>%
#   ungroup()%>%
#   mutate(
#     total =  Hombre+Mujer,
#     porcentaje_h = Hombre/total,
#     porcentaje_m = Mujer/total
#   )
# 
# participacion_wider0 = participacion_wider0%>%
#   select(-sexo)%>%
#   group_by(seccion)%>%
#   summarise(across(everything(), list(sum)))
# 
# participacion_wider <- left_join(participacion_wider0,participacion_wider1, by="seccion")%>%
#   rename(id_seccion = seccion)
# 
# 
# 
# 
# general_pf_completo <- general_pf%>%
#   left_join(grado, by="id_seccion")%>%
#   left_join(nivel, by= "id_seccion")%>%
#   mutate(nivel = as.factor(nivel))%>%
#   left_join(participacion_wider, by = "id_seccion")
# 
# 
# 
# write.csv(general_pf_completo,"C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/generl_pf_regresion.csv",row.names = FALSE)













##### PAN -------------------------------

pan.model_a <- lm(PAN ~ Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes + nivel -1, 
                   data = ayuntamientos_completo21_est)
summary(pan.model_a)
vif(pan.model_a)


predicciones2 <- as.data.frame(predict(pan.model_a_P, newdata =ayuntamientos_completo21 ))


pan.model_a_P <- lm(PAN_P ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes + nivel -1, 
                    data = ayuntamientos_completo21_est)

summary(pan.model_a_P)



##### PRI ------------------------

pri.model_a <- lm(PRI ~  Edad1_1+ Edad2_1+ Edad3_1+porcentaje_m+ nivel + graproes, 
                  data = ayuntamientos_completo21_est)
summary(pri.model_a)

vif(pri.model_a)


pri.model_a_P <- lm(PRI_P ~  Edad1_1+ Edad2_1+ Edad3_1+porcentaje_m+ nivel + graproes, 
                    data = ayuntamientos_completo21_est)
summary(pri.model_a_P)

vif(pri.model_a)



#### PRD ----------------------------

prd.model_a <- lm(PRD ~ Edad1_1+ Edad2_1+ Edad3_1+porcentaje_m+ nivel + graproes, 
                  data = ayuntamientos_completo21_est)
summary(prd.model_a)
vif(prd.model_a)


prd.model_a_P <- lm(PRD_P ~ Edad1_1+ Edad2_1+ Edad3_1+porcentaje_m+ nivel + graproes, 
                    data = ayuntamientos_completo21_est)
summary(prd.model_a_P)
vif(prd.model_a_P)



#### PT ------------------------------
pt.model_a <- lm(PT ~ Edad1_1+ Edad2_1+ Edad3_1+porcentaje_m+ nivel + graproes, 
                 data = ayuntamientos_completo21_est)
summary(pt.model_a)
vif(pt.model_a)


pt.model_a_P <- lm(PT_P ~ Edad1_1+ Edad2_1+ Edad3_1+porcentaje_m+ nivel + graproes, 
                   data = ayuntamientos_completo21_est)
summary(pt.model_a)
vif(pt.model_a)


#### PVEM -----------------------------
pvem.model_a <- lm(PVEM ~ Edad1_1+ Edad2_1+ Edad3_1+porcentaje_m+ nivel + graproes, 
                   data = ayuntamientos_completo21_est)
summary(pvem.model_a)
vif(pvem.model_a)


pvem.model_a_P <- lm(PVEM_P ~ Edad1_1+ Edad2_1+ Edad3_1+porcentaje_m+ nivel + graproes, 
                     data = ayuntamientos_completo21_est)
summary(pvem.model_a_P)
vif(pvem.model_a_P)



#### MC -------------------------------
mc.model_a <- lm(MC ~ Edad1_1+ Edad2_1+ Edad3_1+porcentaje_m+ nivel + graproes, 
                 data = ayuntamientos_completo21_est)
summary(mc.model_a)
vif(mc.model_a)


mc.model_a_P <- lm(MC_P ~ Edad1_1+ Edad2_1+ Edad3_1+porcentaje_m+ nivel + graproes, 
                 data = ayuntamientos_completo21_est)
summary(mc.model_a_P)
vif(mc.model_a_P)


#### MORENA ---------------------------
morena.model_a <- lm(MORENA ~ Edad1_1+ Edad2_1+ Edad3_1+porcentaje_m+ nivel + graproes, 
                     data = ayuntamientos_completo21_est)
summary(morena.model_a)
vif(morena.model_a)



morena.model_a_P <- lm(MORENA_P ~ Edad1_1+ Edad2_1+ Edad3_1+porcentaje_m+ nivel + graproes, 
                       data = ayuntamientos_completo21_est)
summary(morena.model_a)
vif(morena.model_a)






#library(car)


# yx <- 1/3*(1-0.096830-0.315213)
# yy <- 0.096830 + 0.1959857
# yz <- 0.315213 + 0.1959857
# 
# yx + yy + yz
# 
# pan.model_a$coefficients
# 
# summary(pan.model_a)$coefficients[1:12]

