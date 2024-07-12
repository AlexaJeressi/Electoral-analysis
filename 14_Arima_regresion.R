
# Hacer prediccion por partido para todas las secciones con base en los datos predichos por el arima 

all_prediction <- read_csv("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cuatro/all_prediction_nuevo.csv" )

# Demograficos por grupos de edad
dir <- 'C:/Users/Alexa/OneDrive - Next Gate Research/'

agrupado <- read_csv(paste0(dir,"alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_dos/agrupados_15.csv"))
agrupado = agrupado%>%
  rename(id_seccion = seccion)%>%
  mutate(sex_name = ifelse(sexo == 0,"Hombres","Mujeres"))

agrupado <- agrupado%>%
  mutate(grupo2 = case_when(
    edad <= 29 ~ "Edad1",
    edad >=30 & edad <= 55 ~ "Edad2",
    edad >=56 ~ "Edad3"
  ))

participacion_wider0 <- agrupado%>%
  select(id_seccion,grupo2,sexo,sv,ln)%>%
  group_by(id_seccion,sexo,grupo2)%>%
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
  group_by(id_seccion)%>%
  summarise(across(everything(), list(sum)))

nivel <- agrupado%>%
  select(id_seccion,nivel)%>%
  unique()

# Grado escolaridad 
grado <- agrupado%>%
  select(id_seccion,graproes)%>%
  unique()%>%
  mutate(graproes = round(graproes))


participacion_wider <- left_join(participacion_wider0,participacion_wider1, by="id_seccion")%>%
  left_join(nivel, by = "id_seccion")%>%
  left_join(grado, by = "id_seccion")



 
#write.csv(participacion_wider,"C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/demograficos_regresion.csv", row.names = FALSE)


data_completo <- all_prediction%>%
  select(id_seccion,partido,votos,tipo)%>%
  pivot_wider(names_from = partido, values_from = votos)%>%
  left_join(participacion_wider, by = "id_seccion")



# Se deben dividir las bases por tipo de eleccion 
ayuntamientos <- data_completo%>%
  filter(tipo == "Ayuntamientos")%>%
  select(-tipo)


diputados <- data_completo%>%
  filter(tipo == "Diputados Locales")%>%
  select(-tipo)


goberanador <- data_completo%>%
  filter(tipo == "Gobernador")%>%
  select(-tipo)



# Estandarizar las bases 
ayuntamientos_est <- ayuntamientos%>%
  mutate_if(is.numeric, scale)%>%
  mutate_at(vars(!matches("nivel")), as.numeric)%>%
  as.data.frame()

diputados_est <- diputados%>%
  mutate_if(is.numeric, scale)%>%
  mutate_at(vars(!matches("nivel")), as.numeric)%>%
  as.data.frame()

gobernador_est <- goberanador%>%
  mutate_if(is.numeric, scale)%>%
  mutate_at(vars(!matches("nivel")), as.numeric)%>%
  as.data.frame()




# MODELOS AYUNTAMIENTOS --------------------------------------------------------
model_pan <- lm(PAN ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                + nivel -1, 
                data = ayuntamientos_est)
#summary(model_pan)
model_pri <- lm(PRI ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                + nivel -1, 
                data = ayuntamientos_est)
#summary(model_pri)
model_prd <- lm(PRD ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                + nivel -1, 
                data = ayuntamientos_est)
#summary(model_prd)
model_pt <- lm(PT ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
               + nivel -1, 
               data = ayuntamientos_est)
#summary(model_pt)
model_pvem <- lm(PVEM ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                 + nivel -1, 
                 data = ayuntamientos_est)
#summary(model_pvem)

model_mc <- lm(MC ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                 + nivel -1, 
                 data = ayuntamientos_est)
  #summary(model_mc)
model_pes <- lm(PES ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                  + nivel -1, 
                  data = ayuntamientos_est)
  

model_morena <- lm(MORENA ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                   + nivel -1, 
                   data = ayuntamientos_est)
#summary(model_morena)

model_panal <- lm(PANAL ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                  + nivel -1, 
                  data = ayuntamientos_est)

## Predicciones 
predict_pan <- as.data.frame(predict(model_pan, newdata = ayuntamientos))%>%
  rename("votos" = "predict(model_pan, newdata = ayuntamientos)")%>%
  mutate(partido = "PAN")%>%
  cbind(select(ayuntamientos,id_seccion))


predict_pri <- as.data.frame(predict(model_pri, newdata = ayuntamientos))%>%
  rename("votos" = "predict(model_pri, newdata = ayuntamientos)")%>%
  mutate(partido = "PRI")%>%
  cbind(select(ayuntamientos,id_seccion))


predict_prd <- as.data.frame(predict(model_prd, newdata = ayuntamientos))%>%
  rename("votos" = "predict(model_prd, newdata = ayuntamientos)")%>%
  mutate(partido = "PRD")%>%
  cbind(select(ayuntamientos,id_seccion))


predict_pt <- as.data.frame(predict(model_pt, newdata = ayuntamientos))%>%
  rename("votos" = "predict(model_pt, newdata = ayuntamientos)")%>%
  mutate(partido = "PT")%>%
  cbind(select(ayuntamientos,id_seccion))


predict_pvem <- as.data.frame(predict(model_pvem, newdata = ayuntamientos))%>%
  rename("votos" = "predict(model_pvem, newdata = ayuntamientos)")%>%
  mutate(partido = "PVEM")%>%
  cbind(select(ayuntamientos,id_seccion))

  predict_mc <- as.data.frame(predict(model_mc, newdata = ayuntamientos))%>%
    rename("votos" = "predict(model_mc, newdata = ayuntamientos)")%>%
    mutate(partido = "MC")%>%
    cbind(select(ayuntamientos,id_seccion))
  
  predict_pes <- as.data.frame(predict(model_pes, newdata = ayuntamientos))%>%
    rename("votos" = "predict(model_pes, newdata = ayuntamientos)")%>%
    mutate(partido = "PES")%>%
    cbind(select(ayuntamientos,id_seccion))

predict_morena <- as.data.frame(predict(model_morena, newdata = ayuntamientos))%>%
  rename("votos" = "predict(model_morena, newdata = ayuntamientos)")%>%
  mutate(partido = "MORENA")%>%
  cbind(select(ayuntamientos,id_seccion))

predict_panal <- as.data.frame(predict(model_panal, newdata = ayuntamientos))%>%
  rename("votos" = "predict(model_panal, newdata = ayuntamientos)")%>%
  mutate(partido = "PANAL")%>%
  cbind(select(ayuntamientos,id_seccion))


prediciones_all_ayuntamientos  <- smartbind(predict_pan,predict_pri,predict_prd,predict_pt,
                                  predict_pvem,predict_mc,predict_morena,predict_panal,
                                  predict_pes)%>%
  mutate(votos = round(votos),
         tipo = "Ayuntamientos")%>%
  as.data.frame()%>%
  mutate(votos = ifelse(votos < 0, 0, votos))






# DIPUTADOS LOCALES  --------------------------------------------------------
model_pan <- lm(PAN ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                + nivel -1, 
                data = diputados_est)
#summary(model_pan)
model_pri <- lm(PRI ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                + nivel -1, 
                data = diputados_est)
#summary(model_pri)
model_prd <- lm(PRD ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                + nivel -1, 
                data = diputados_est)
#summary(model_prd)
model_pt <- lm(PT ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
               + nivel -1, 
               data = diputados_est)
#summary(model_pt)
model_pvem <- lm(PVEM ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                 + nivel -1, 
                 data = diputados_est)
#summary(model_pvem)

model_mc <- lm(MC ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
               + nivel -1, 
               data = diputados_est)
#summary(model_mc)
model_pes <- lm(PES ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                + nivel -1, 
                data = diputados_est)


model_morena <- lm(MORENA ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                   + nivel -1, 
                   data = diputados_est)
#summary(model_morena)

model_panal <- lm(PANAL ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                  + nivel -1, 
                  data = diputados_est)

## Predicciones 
predict_pan <- as.data.frame(predict(model_pan, newdata = diputados))%>%
  rename("votos" = "predict(model_pan, newdata = diputados)")%>%
  mutate(partido = "PAN")%>%
  cbind(select(diputados,id_seccion))


predict_pri <- as.data.frame(predict(model_pri, newdata = diputados))%>%
  rename("votos" = "predict(model_pri, newdata = diputados)")%>%
  mutate(partido = "PRI")%>%
  cbind(select(diputados,id_seccion))


predict_prd <- as.data.frame(predict(model_prd, newdata = diputados))%>%
  rename("votos" = "predict(model_prd, newdata = diputados)")%>%
  mutate(partido = "PRD")%>%
  cbind(select(diputados,id_seccion))


predict_pt <- as.data.frame(predict(model_pt, newdata = diputados))%>%
  rename("votos" = "predict(model_pt, newdata = diputados)")%>%
  mutate(partido = "PT")%>%
  cbind(select(diputados,id_seccion))


predict_pvem <- as.data.frame(predict(model_pvem, newdata = diputados))%>%
  rename("votos" = "predict(model_pvem, newdata = diputados)")%>%
  mutate(partido = "PVEM")%>%
  cbind(select(diputados,id_seccion))

predict_mc <- as.data.frame(predict(model_mc, newdata = diputados))%>%
  rename("votos" = "predict(model_mc, newdata = diputados)")%>%
  mutate(partido = "MC")%>%
  cbind(select(diputados,id_seccion))

predict_pes <- as.data.frame(predict(model_pes, newdata = diputados))%>%
  rename("votos" = "predict(model_pes, newdata = diputados)")%>%
  mutate(partido = "PES")%>%
  cbind(select(diputados,id_seccion))

predict_morena <- as.data.frame(predict(model_morena, newdata = diputados))%>%
  rename("votos" = "predict(model_morena, newdata = diputados)")%>%
  mutate(partido = "MORENA")%>%
  cbind(select(diputados,id_seccion))

predict_panal <- as.data.frame(predict(model_panal, newdata = diputados))%>%
  rename("votos" = "predict(model_panal, newdata = diputados)")%>%
  mutate(partido = "PANAL")%>%
  cbind(select(diputados,id_seccion))


prediciones_all_diputados <- smartbind(predict_pan,predict_pri,predict_prd,predict_pt,
                                  predict_pvem,predict_mc,predict_morena,predict_panal,
                                  predict_pes)%>%
  mutate(votos = round(votos),
         tipo = "Diputados Locales")%>%
  as.data.frame()%>%
  mutate(votos = ifelse(votos < 0, 0, votos))


# GOBERNADOR  --------------------------------------------------------
model_pan <- lm(PAN ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                + nivel -1, 
                data = gobernador_est)
#summary(model_pan)
model_pri <- lm(PRI ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                + nivel -1, 
                data = gobernador_est)
#summary(model_pri)
model_prd <- lm(PRD ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                + nivel -1, 
                data = gobernador_est)
#summary(model_prd)
model_pt <- lm(PT ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
               + nivel -1, 
               data = gobernador_est)
#summary(model_pt)
model_pvem <- lm(PVEM ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                 + nivel -1, 
                 data = gobernador_est)
#summary(model_pvem)

# model_mc <- lm(MC ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
#                + nivel -1, 
#                data = gobernador_est)


model_morena <- lm(MORENA ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                   + nivel -1, 
                   data = gobernador_est)
#summary(model_morena)

model_panal <- lm(PANAL ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                  + nivel -1, 
                  data = gobernador_est)

## Predicciones 
predict_pan <- as.data.frame(predict(model_pan, newdata = goberanador))%>%
  rename("votos" = "predict(model_pan, newdata = goberanador)")%>%
  mutate(partido = "PAN")%>%
  cbind(select(goberanador,id_seccion))


predict_pri <- as.data.frame(predict(model_pri, newdata = goberanador))%>%
  rename("votos" = "predict(model_pri, newdata = goberanador)")%>%
  mutate(partido = "PRI")%>%
  cbind(select(goberanador,id_seccion))


predict_prd <- as.data.frame(predict(model_prd, newdata = goberanador))%>%
  rename("votos" = "predict(model_prd, newdata = goberanador)")%>%
  mutate(partido = "PRD")%>%
  cbind(select(goberanador,id_seccion))


predict_pt <- as.data.frame(predict(model_pt, newdata = goberanador))%>%
  rename("votos" = "predict(model_pt, newdata = goberanador)")%>%
  mutate(partido = "PT")%>%
  cbind(select(goberanador,id_seccion))


predict_pvem <- as.data.frame(predict(model_pvem, newdata = goberanador))%>%
  rename("votos" = "predict(model_pvem, newdata = goberanador)")%>%
  mutate(partido = "PVEM")%>%
  cbind(select(goberanador,id_seccion))


# predict_mc <- as.data.frame(predict(model_mc, newdata = goberanador))%>%
#   rename("votos" = "predict(model_mc, newdata = goberanador)")%>%
#   mutate(partido = "MC")%>%
#   cbind(select(goberanador,id_seccion))


predict_morena <- as.data.frame(predict(model_morena, newdata = goberanador))%>%
  rename("votos" = "predict(model_morena, newdata = goberanador)")%>%
  mutate(partido = "MORENA")%>%
  cbind(select(goberanador,id_seccion))

predict_panal <- as.data.frame(predict(model_panal, newdata = goberanador))%>%
  rename("votos" = "predict(model_panal, newdata = goberanador)")%>%
  mutate(partido = "PANAL")%>%
  cbind(select(goberanador,id_seccion))



prediciones_all_gobernador <- smartbind(predict_pan,predict_pri,predict_prd,predict_pt,
                                  predict_pvem,predict_morena,predict_panal)%>%
  mutate(votos = round(votos),
         tipo = "Gobernador")%>%
  as.data.frame()%>%
  mutate(votos = ifelse(votos < 0, 0, votos))




predicciones_alcance <- smartbind(as.data.frame(prediciones_all_ayuntamientos),
                                  as.data.frame(prediciones_all_diputados),
                                  as.data.frame(prediciones_all_gobernador))%>%
  mutate(grupo = "Alcance")

#write.csv(predicciones_alcance,"C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_cinco/predicciones_ols_alcance.csv", row.names = FALSE )


# Loop para obtener las bases separadas por partido 
#elecciones <- c("Ayuntamientos","Diputados Locales","Gobernador")
partidos_list <- c("MC","MORENA","PAN","PANAL","PES","PRD","PRI","PT","PVEM")


  for (df.name in partidos_list){
    assign(paste0(df.name,"_",election), general_pf%>%
             #select(id_seccion, votos, partidos_nuevo, partido_base,tipo)%>%
             filter(partido_base == df.name,
                    partidos_nuevo == df.name,
                    #tipo == election
                    )%>%
             unique()
    )
  }


# Mismos datos de arriba pero estandarizados 

for (df.name in partidos_list){
  assign(paste0(df.name,"_",election,"_est"), general_pf%>%
           #select(id_seccion, votos, partidos_nuevo, partido_base,tipo)%>%
           filter(partido_base == df.name,
                  partidos_nuevo == df.name,
                  #tipo == election
           )%>%
           unique()%>%
           mutate_if(is.numeric, scale)%>%
           mutate_at(vars(id_seccion,participacion,participacion_gobernador,
                          votos,graproes,Edad1_1:porcentaje_m), as.numeric)%>%
           as.data.frame()
  )
}


# Loop para hacer predicciones 







# Un modelo por partido porque las secciones se pueden repetir entre el basico, 
# intermedio y completo 
# Para un mismo partido 

# Aqui que deberia obtener, el mismo numero de votos para los partidos que comparten la
# seccion como parte de su alcance 

