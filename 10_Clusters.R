#Librerias 
library(tidyverse)
library(gtools)

dir <- 'C:/Users/Alexa/OneDrive - Next Gate Research/'
#dir <- 'C:/Users/peyo_/OneDrive - Next Gate Research/Y.Respaldo/alfredo/NGR_ANT/Proyectos/'

# Datos de resultados de la eleccion
#load(paste0(dir,"alexa/2022/10_Plataforma/Datos/Estado de Mexico/Datos_indices/resultados_indices.RData"))

# Paso 1. Obtener el partido para el que es seccion dura cada seccion 
 # Se hace para cada tipo de eleccion 

# A_pan = a_pan%>%
#   select(id_seccion,estrato)%>%
#   rename(PAN = estrato)
# 
# A_pri = a_pri%>%
#   select(id_seccion,estrato)%>%
#   rename(PRI = estrato)
# 
# A_prd = a_prd%>%
#   select(id_seccion,estrato)%>%
#   rename(PRD = estrato)
# 
# A_pvem = a_pvem%>%
#   select(id_seccion,estrato)%>%
#   rename(PVEM = estrato)
# 
# A_pt = a_pt%>%
#   select(id_seccion,estrato)%>%
#   rename(PT = estrato)
# 
# A_mc = a_mc%>%
#   select(id_seccion,estrato)%>%
#   rename(MC = estrato)
# 
# A_morena = a_morena%>%
#   select(id_seccion,estrato)%>%
#   rename(MORENA = estrato)
# 
# # Dejamos el valor de todas. Para agregar varianza 
# 
# ayuntamientos_all = full_join(A_pan,A_pri,by="id_seccion")%>%
#   full_join(A_prd,by="id_seccion")%>%
#   full_join(A_pvem,by="id_seccion")%>%
#   full_join(A_pt,by="id_seccion")%>%
#   full_join(A_mc,by="id_seccion")%>%
#   full_join(A_morena,by="id_seccion")
#   
                            
# Paso 2.
 # Porcentaje por grupos de edad (conformacion de las edades)
 # NSE
 # NPE
 # Porcentaje de participacion de la poblacion 
 # % de hombres y mujeres 
 # % de la pea hombres y mujeres 

demo <- read_csv(paste0(dir,"alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_dos/demograficos_15.csv"))

agrupado <- read_csv(paste0(dir,"alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_dos/agrupados_15.csv"))

agrupado = agrupado%>%
   rename(id_seccion = seccion)

# Obtener porcentaje de participacion por rangos de edad  
edades = agrupado%>%
  select(id_seccion,nombre_rango,ln)%>%
  group_by(id_seccion,nombre_rango)%>%
  summarise(total = sum(ln))%>%
  pivot_wider(names_from = nombre_rango,values_from = total)%>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  mutate(total = rowSums(across(`18 a 29 años`:`60 y mas años`)),
         porcentaje_1 = `18 a 29 años`/total,
         porcentaje_2 = `30 a 39 años`/total,
         porcentaje_3 = `40 a 49 años`/total,
         porcentaje_4 = `50 a 59 años`/total,
         porcentaje_5 = `60 y mas años`/total)%>%
  select(id_seccion,porcentaje_1,porcentaje_2,porcentaje_3,porcentaje_4,porcentaje_5)

#sum(is.na(edades))

#nulos = edades%>%filter(is.na(porcentaje_1))



# Porcentaje de participacion general 
# participacion_all = agrupado%>%
#   group_by(id_seccion)%>%
#   summarise(ln_t = sum(ln),
#             sv_t = sum(sv),
#             porcentaje_p = sv_t/ln_t)%>%
#   select(id_seccion,porcentaje_p)

#sum(is.na(participacion_all))

# Grado promedio de escolaridad, NSE, porcentaje de hombres y mujeres , porcentaje pea
demograficos_all = demo%>%
  filter(!is.na(nivel))%>%
  #mutate_if(is.numeric , replace_na, replace = 0) %>%
  mutate(porcentaje_hombres = P_18YMAS_M/P_18YMAS,
         porcentaje_mujeres = P_18YMAS_F/P_18YMAS,
         porcentaje_PEA_H = PEA_M/PEA,
         porcentaje_PEA_M = PEA_F/PEA,
         niveles = case_when(
           nivel == "A/B" ~ 1,
           nivel == "C+" ~ 2,
           nivel == "C" ~ 3,
           nivel == "C-" ~ 4,
           nivel == "D+" ~ 5,
           nivel == "D" ~ 6,
           nivel == "E" ~ 7))%>%
  select(id_seccion,GRAPROES,niveles,
         porcentaje_hombres,porcentaje_mujeres,
         porcentaje_PEA_H,porcentaje_PEA_M)

sum(is.na(demograficos_all))  
sum(is.na(edades))

all = inner_join(demograficos_all,edades, by = "id_seccion")
all_id = all
# Convertir id_seccion como nombre de las filas 
rownames(all) <- all$id_seccion
all[,1] <- NULL

sum(is.na(all))
#----------------------------- Clusters -------------------------------------------

# Proceso para hacer clasificación por medio de KNN 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr")
ipak(packages)

# Estandarizar los datos 
df <- scale(all)
sum(is.na(df))

#calcular la matriz de distacias
m.distancia <- get_dist(df, method = "euclidean") #el método aceptado también puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
#fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))


#estimar el número de clústers
#Elbow, silhouette o gap_stat  method
fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "silhouette")
fviz_nbclust(df, kmeans, method = "gap_stat")


#con esta función se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).

#resnumclust<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
#fviz_nbclust(resnumclust)


#calculamos los dos clústers
k2 <- kmeans(df, centers = 4, nstart = 25)
k2
str(k2)

#plotear los cluster
fviz_cluster(k2, data = df)
fviz_cluster(k2, data = df, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k2, data = df, ellipse.type = "norm")
fviz_cluster(k2, data = df, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

# res2 <- hcut(df, k = 2, stand = TRUE)
# fviz_dend(res2, rect = TRUE, cex = 0.5,
#           k_colors = c("red","#2E9FDF"))
# 
# res4 <- hcut(df, k = 4, stand = TRUE)
# fviz_dend(res4, rect = TRUE, cex = 0.5,
#           k_colors = c("red","#2E9FDF","green","black",))



#pasar los cluster a mi df inicial para trabajar con ellos
all %>%
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


df <- all_id
df
df$clus<-as.factor(k2$cluster)
df

sum(is.na(df))

df = df%>%
  select(id_seccion,clus)

#write.csv(df, paste0(dir,"alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_tres/","cluster_seccion.csv"),row.names = FALSE)
# Escalar de nuevo para hacer visualizacion 
df <- all
df <- scale(df)
df<- as.data.frame(df)
df$clus<-as.factor(k2$cluster)
df

df$clus<-factor(df$clus)
data_long <- gather(df, caracteristica, valor, GRAPROES:porcentaje_5, factor_key=TRUE)
data_long


ggplot(data_long, aes(as.factor(x = caracteristica), y = valor,group=clus, colour = clus)) + 
  stat_summary(fun = mean, geom="pointrange", size = 1)+
  stat_summary(geom="line")
  #geom_point(aes(shape=clus))



### Hacer base de datos con cluster, demograficos, participacion y votos 
clusters <- read_csv(paste0(dir,"alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_tres/","cluster_seccion.csv"))

# Participacion por rangos de edad y por sexo 
rangos_seccion = agrupado%>%
  #select(id_seccion,nombre_rango,sexo)%>%
  group_by(id_seccion,nombre_rango,sexo)%>%
  summarise(ln = sum(ln),
            sv = sum(sv))

# Demograficos se van a repetir
demo 

save(clusters,demo,rangos_seccion,
     file = paste0(dir,"alexa/2022/10_Plataforma/Datos/Estado de Mexico/Tab_tres/","data_clusters.RData"))





