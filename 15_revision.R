library(tidyverse)
dir <- 'C:/Users/Alexa/OneDrive - Next Gate Research/'
load(paste0(dir,'alexa/2022/10_Plataforma/Scripts/elecciones/data/objetos_data.Rda'))


base_agrupado1 <- base%>%
  filter(clus == 1)

base_agrupado2 <- base%>%
  filter(clus == 2)

base_agrupado3 <- base%>%
  filter(clus == 3)

base_agrupado4 <- base%>%
  filter(clus == 4)

base1 <- base_agrupado4%>%
  select(id_seccion,partido,votos)%>%
  unique()%>%
  pivot_wider(names_from = partido,values_from = votos)%>%
  mutate_if(is.numeric , replace_na, replace = 0)%>%
  #group_by(id_seccion)%>%
  summarise(total = sum(PAN) + sum(PRI) + sum(PRD) + sum(PT) + sum(PVEM) + sum(MC) + sum(MORENA) + sum(PANAL) + sum(PES),
            PAN = round((sum(PAN)/total)*100),
            PRI = round((sum(PRI)/total)*100),
            PRD = round((sum(PRD)/total)*100),
            PT = round((sum(PT)/total)*100),
            PVEM = round((sum(PVEM)/total)*100),
            MC = round((sum(MC)/total)*100),
            MORENA = round((sum(MORENA)/total)*100),
            PANAL = round((sum(PANAL)/total)*100),
            PES = round((sum(PES)/total)*100)
  )%>%
  select(-total)%>%
  gather("partido","Porcentaje")



### Variables sociodemograficas para cada segmento ----------------------------------
base_agrupado = base_agrupado%>%
  mutate(sex_name = ifelse(sexo == 0,"Hombres","Mujeres"))

# PARTICIPACION POR GRUPOS DE EDAD 
try3 = base_agrupado%>%
  select(id_seccion, sex_name,nombre_rango,sv)%>%
  unique()%>%
  group_by(sex_name,nombre_rango)%>%
  summarise(total = sum(sv))%>%
  group_by(sex_name)%>%
  #mutate(porcentaje = round((total/sum(total))*100))%>%
  pivot_wider(names_from = nombre_rango,values_from = total)%>%
  mutate(sex_name = as.factor(sex_name))


base_agrupado1 <- base_agrupado%>%
  filter(clus == 1)

base_agrupado2 <- base_agrupado%>%
  filter(clus == 2)

base_agrupado3 <- base_agrupado%>%
  filter(clus == 3)

base_agrupado4 <- base_agrupado%>%
  filter(clus == 4)



try3 = base_agrupado4%>%
  select(id_seccion, sex_name,nombre_rango,sv)%>%
  unique()%>%
  group_by(sex_name,nombre_rango)%>%
  summarise(total = sum(sv))%>%
  group_by(sex_name)%>%
  mutate(Porcentaje = round( (total/sum(total))*100,2 ))%>%
  select(-total)%>%
  #mutate(porcentaje = round((total/sum(total))*100))%>%
  pivot_wider(names_from = nombre_rango,values_from = Porcentaje)%>%
  mutate(sex_name = as.factor(sex_name))


# NIVEL SOCIOECONOMICO 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


nivel_se <- base_agrupado4%>%
  select(id_seccion,nivel)%>%
  unique()%>%
  summarise(Mode(nivel))

# GRADO DE ESCOLARIDAD 

grape <-
  base_agrupado4%>%
  select(id_seccion,GRAPROES)%>%
  unique()%>% # Lo del censo necesita unique()
  summarise(round(sum(GRAPROES)/n()))


######### Proceso para obtener el numero de secciones en grupo de competitividad por partido
pan <- base_all_competitividad%>%
  filter(partido == "PAN",
         tipo == "Ayuntamientos")

table(pan$estratos)


pri <- base_all_competitividad%>%
  filter(partido == "PRI",
         tipo == "Ayuntamientos")

table(pri$estratos)


prd <- base_all_competitividad%>%
  filter(partido == "PRD",
         tipo == "Ayuntamientos")

table(prd$estratos)

pt <- base_all_competitividad%>%
  filter(partido == "PT",
         tipo == "Ayuntamientos")

table(pt$estratos)

pvem <- base_all_competitividad%>%
  filter(partido == "PVEM",
         tipo == "Ayuntamientos")

table(pvem$estratos)

panal <- base_all_competitividad%>%
  filter(partido == "PANAL",
         tipo == "Ayuntamientos")

table(panal$estratos)

morena <- base_all_competitividad%>%
  filter(partido == "MORENA",
         tipo == "Ayuntamientos")

table(morena$estratos)

mc <- base_all_competitividad%>%
  filter(partido == "MC",
         tipo == "Ayuntamientos")

table(mc$estratos)



