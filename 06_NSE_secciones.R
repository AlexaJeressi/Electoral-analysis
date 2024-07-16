library(tidyverse)


censo <- read_csv("INE_SECCION_2020.csv")

censo = censo%>%
  filter(ENTIDAD == 15)

# AMAI POR SECCION ---------------------------------------------------------------------------------------

# Educacion 
data_educacion = censo%>%
  select(SECCION,POBTOT,P15YM_SE,P15PRI_CO,P15PRI_IN,
         P15SEC_CO,P15SEC_IN,P18YM_PB)%>%
  summarise(edu1 = P15YM_SE/POBTOT,
            edu2 = (P15PRI_IN/POBTOT)*6,
            edu3 = (P15PRI_CO/POBTOT)*11,
            edu4 = (P15SEC_IN/POBTOT)*12,
            edu5 = (P15SEC_CO/POBTOT)*18,
            edu6 = (P18YM_PB*0.7/POBTOT)*51,
            edu7 = (P18YM_PB*0.2/POBTOT)*73,
            edu8 = (P18YM_PB*0.1/POBTOT)*101)%>%
  mutate(valor_edu = rowSums(across(edu1:edu8)))%>%
  cbind(select(censo,c(SECCION)))%>%
  select(SECCION,valor_edu)


# Baño 
data_baño = censo%>%
  select(SECCION,VIVTOT,VPH_EXCSA)%>%
  mutate(baño1 = VPH_EXCSA/VIVTOT,
         valor_baño = ifelse(baño1 > 0.5 & baño1 <= 0.7 , 24,
                             ifelse(baño1 > 0.7,47,
                                    0)))%>%
  select(SECCION,valor_baño)

# Carro 
data_carro = censo%>%
  select(SECCION,VIVTOT,VPH_AUTOM)%>%
  mutate(valor_carro = ifelse(VPH_AUTOM/VIVTOT > 0.5 & VPH_AUTOM/VIVTOT <= 0.7, 22,
                              ifelse(VPH_AUTOM/VIVTOT > 0.7, 43,0)))%>%
  select(SECCION,valor_carro)

# internet 
data_internet = censo%>%
  select(SECCION,VIVTOT,VPH_INTER)%>%
  mutate(valor_internet = ifelse(VPH_INTER/VIVTOT > 0.7 , 32,0))%>%
  select(SECCION,valor_internet)
# Lo cambien al 70%

# Trabajo 
data_trabajo = censo%>%
  select(SECCION,VIVTOT,PEA,PROM_OCUP)%>%
  mutate(p_eav = PEA/PROM_OCUP,
         pea = VIVTOT/p_eav,
         valor_trabajo = ifelse(pea >= 1 & pea < 2, 15,
                                ifelse(pea >= 2 & pea < 3, 31,
                                       ifelse(pea >= 3 & pea < 4, 46,
                                              ifelse(pea >= 4, 61,
                                                     ifelse(pea < 1, 0, 0))))))%>%
  select(SECCION,valor_trabajo)


# Cuartos 
data_cuartos = censo%>%
  select(VIVTOT,VPH_1DOR,VPH_2YMASD)%>%
  summarise(vivienda1 = (VPH_1DOR/VIVTOT)*4,# 4 el promedio de 0 y 8
            vivienda2 = (VPH_2YMASD/VIVTOT)*24)%>%
  mutate(
    valor_cuartos = rowSums(across(vivienda1:vivienda2)),
    mayor_cuartos = names(.)[max.col(.)])%>%
  cbind(select(censo, c(SECCION)))%>%
  select(SECCION,valor_cuartos)


seccion_nse <- data_educacion%>%
  left_join(data_baño, by = "SECCION")%>%
  left_join(data_carro, by = "SECCION")%>%
  left_join(data_internet, by = "SECCION")%>%
  left_join(data_trabajo, by = "SECCION")%>%
  left_join(data_cuartos, by = "SECCION")

seccion_nse = seccion_nse%>%
  mutate(calificacion = round(rowSums(across(valor_edu:valor_cuartos))),
         nivel = ifelse(calificacion >= 202, "A/B",
                        ifelse(calificacion >= 168 & calificacion <= 201, "C+",
                               ifelse(calificacion >=141 & calificacion <=167, "C",
                                      ifelse(calificacion >= 116 & calificacion <=140,"C-",
                                             ifelse(calificacion >= 95 & calificacion <= 115, "D+",
                                                    ifelse(calificacion >=48 & calificacion <=94, "D",
                                                           "E")))))))%>%
  left_join(select(censo, c(DISTRITO, MUNICIPIO,SECCION)), by = "SECCION")



seccion_nse_15 <- select(seccion_nse,DISTRITO, MUNICIPIO,SECCION,nivel)


nulos = seccion_nse%>%
  filter(is.na(valor_edu))








##------------------------- DEMOGRAFICOS -------------------------------------------------
seccion_nse_15 <- read_csv("/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_dos/nse_seccion.csv")%>%
  janitor::clean_names()

inegi_secciones <- read_csv(paste0(dir,"alexa/2022/04_analisis_MC/descargas/escala_geoelectoral2020/conjunto_de_datos/INE_SECCION_2020.csv"))%>%
  janitor::clean_names()

inegi_secciones = inegi_secciones%>%
  filter(entidad == 15)%>%
  select(seccion,p_18ymas,p_18ymas_f,p_18ymas_m,graproes,graproes_f,graproes_m,pea,pea_f,pea_m)%>%
  left_join(seccion_nse_15, by= "seccion")


# Agregar datos de participacion 
participacion <- read.csv("/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_dos/participacion.csv")%>%
  janitor::clean_names()


datos_final <- full_join(inegi_secciones,participacion, by = "seccion")

