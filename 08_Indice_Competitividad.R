library(tidyverse)
library(p2distance)
library(stratification)
library(gtools)
#install.packages("p2distance")


# Ayuntamientos ------------------------------------------------------------------------

ayuntamientos <- smartbind(as.data.frame(a_pan),
                           as.data.frame(a_pri),
                           as.data.frame(a_prd),
                           as.data.frame(a_pt),
                           as.data.frame(a_pvem),
                           as.data.frame(a_mc),
                           as.data.frame(a_panal),
                           as.data.frame(a_morena),
                           as.data.frame(a_pes)
                           )

minimos <- rep(1,3)

indice <- p2distance(matriz = as.matrix(ayuntamientos[,c(3:5)]),
                     reference_vector = minimos,
                     iterations = 50)

ayuntamientos <- cbind(ayuntamientos,indice[["p2distance"]])

estratos <- strata.cumrootf(ayuntamientos$p2distance.2,
                            n = length(ayuntamientos$p2distance.2),
                            Ls = 5)


assign(paste0("ayuntamientos"), data.frame(ayuntamientos, estratos[["stratumID"]])) 

ayuntamientos_index = ayuntamientos%>%
  rename(p2_index = p2distance.2,
         estrato = "estratos...stratumID...")%>%
  mutate(estratos = case_when(
    estrato == 1 ~ "Imposibles",
    estrato == 2 ~ "Dificiles",
    estrato == 3 ~ "Posibles",
    estrato == 4 ~ "Blandas",
    estrato == 5 ~ "Duras")
  )


# Diputados ------------------------------------------------------ 

diputados <- smartbind(as.data.frame(d_pan),
                           as.data.frame(d_pri),
                           as.data.frame(d_prd),
                           as.data.frame(d_pt),
                           as.data.frame(d_pvem),
                           as.data.frame(d_mc),
                           as.data.frame(d_panal),
                           as.data.frame(d_morena),
                           as.data.frame(d_pes)
)

minimos <- rep(1,3)

indice <- p2distance(matriz = as.matrix(diputados[,c(3:5)]),
                     reference_vector = minimos,
                     iterations = 50)

diputados <- cbind(diputados,indice[["p2distance"]])

estratos <- strata.cumrootf(diputados$p2distance.2,
                            n = length(diputados$p2distance.2),
                            Ls = 5)


assign(paste0("diputados"), data.frame(diputados, estratos[["stratumID"]])) 

diputados_index = diputados%>%
  rename(p2_index = p2distance.2,
         estrato = "estratos...stratumID...")%>%
  mutate(estratos = case_when(
    estrato == 1 ~ "Imposibles",
    estrato == 2 ~ "Dificiles",
    estrato == 3 ~ "Posibles",
    estrato == 4 ~ "Blandas",
    estrato == 5 ~ "Duras")
  )



# Gobernador ------------------------------------------------------ 

gobernador <- smartbind(as.data.frame(g_pan),
                       as.data.frame(g_pri),
                       as.data.frame(g_prd),
                       as.data.frame(g_pt),
                       as.data.frame(g_pvem),
                       #as.data.frame(d_mc),
                       as.data.frame(g_panal),
                       as.data.frame(g_morena)
                       #as.data.frame(d_pes)
)

minimos <- rep(1,3)

indice <- p2distance(matriz = as.matrix(gobernador[,c(3:5)]),
                     reference_vector = minimos,
                     iterations = 50)

gobernador <- cbind(gobernador,indice[["p2distance"]])

estratos <- strata.cumrootf(gobernador$p2distance.2,
                            n = length(gobernador$p2distance.2),
                            Ls = 5)


assign(paste0("gobernador"), data.frame(gobernador, estratos[["stratumID"]])) 

gobernador_index = gobernador%>%
  rename(p2_index = p2distance.2,
         estrato = "estratos...stratumID...")%>%
  mutate(estratos = case_when(
    estrato == 1 ~ "Imposibles",
    estrato == 2 ~ "Dificiles",
    estrato == 3 ~ "Posibles",
    estrato == 4 ~ "Blandas",
    estrato == 5 ~ "Duras")
  )


save(ayuntamientos_index,diputados_index,gobernador_index,file = "~/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Datos_indices/resultados_indices.RData")

