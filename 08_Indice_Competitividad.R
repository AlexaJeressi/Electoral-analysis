library(tidyverse)
library(p2distance)
library(stratification)
library(gtools)
#install.packages("p2distance")

load("/Users/alexa/Library/CloudStorage/OneDrive-NextGateResearch/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Datos_indices/datos_indices.RData")

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



#------------------------------- AYUNTAMIENTOS -------------------------------------
## PAN---------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(a_pan[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# a_pan <- cbind(a_pan,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(a_pan$p2distance.2,
#                             n = length(a_pan$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("a_pan"), data.frame(a_pan, estratos[["stratumID"]])) 
# 
# a_pan = a_pan%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# ## PRI -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(a_pri[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# a_pri <- cbind(a_pri,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(a_pri$p2distance.2,
#                             n = length(a_pri$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("a_pri"), data.frame(a_pri, estratos[["stratumID"]])) 
# 
# a_pri = a_pri%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#     )
# 
# ## PRD -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(a_prd[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# a_prd <- cbind(a_prd,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(a_prd$p2distance.2,
#                             n = length(a_prd$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("a_prd"), data.frame(a_prd, estratos[["stratumID"]])) 
# 
# a_prd = a_prd%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# ## PT -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(a_pt[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# a_pt <- cbind(a_pt,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(a_pt$p2distance.2,
#                             n = length(a_pt$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("a_pt"), data.frame(a_pt, estratos[["stratumID"]])) 
# 
# a_pt = a_pt%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# ## PVEM -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(a_pvem[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# a_pvem <- cbind(a_pvem,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(a_pvem$p2distance.2,
#                             n = length(a_pvem$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("a_pvem"), data.frame(a_pvem, estratos[["stratumID"]])) 
# 
# a_pvem = a_pvem%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# ## MC -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(a_mc[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# a_mc <- cbind(a_mc,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(a_mc$p2distance.2,
#                             n = length(a_mc$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("a_mc"), data.frame(a_mc, estratos[["stratumID"]])) 
# 
# a_mc = a_mc%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# ## PANAL -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(a_panal[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# a_panal <- cbind(a_panal,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(a_panal$p2distance.2,
#                             n = length(a_panal$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("a_panal"), data.frame(a_panal, estratos[["stratumID"]])) 
# 
# a_panal = a_panal%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# 
# ## MORENA -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(a_morena[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# a_morena <- cbind(a_morena,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(a_morena$p2distance.2,
#                             n = length(a_morena$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("a_morena"), data.frame(a_morena, estratos[["stratumID"]])) 
# 
# a_morena = a_morena%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# ## PES -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(a_pes[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# a_pes <- cbind(a_pes,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(a_pes$p2distance.2,
#                             n = length(a_pes$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("a_pes"), data.frame(a_pes, estratos[["stratumID"]])) 
# 
# a_pes = a_pes%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# 
# 
# #------------------------------- DIPUTACIONES -------------------------------------
# ## PAN---------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(d_pan[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# d_pan <- cbind(d_pan,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(d_pan$p2distance.2,
#                             n = length(d_pan$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("d_pan"), data.frame(d_pan, estratos[["stratumID"]])) 
# 
# d_pan = d_pan%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# ## PRI -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(d_pri[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# d_pri <- cbind(d_pri,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(d_pri$p2distance.2,
#                             n = length(d_pri$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("d_pri"), data.frame(d_pri, estratos[["stratumID"]])) 
# 
# d_pri = d_pri%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# ## PRD -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(d_prd[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# d_prd <- cbind(d_prd,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(d_prd$p2distance.2,
#                             n = length(d_prd$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("d_prd"), data.frame(d_prd, estratos[["stratumID"]])) 
# 
# d_prd = d_prd%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# ## PT -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(d_pt[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# d_pt <- cbind(d_pt,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(d_pt$p2distance.2,
#                             n = length(d_pt$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("d_pt"), data.frame(d_pt, estratos[["stratumID"]])) 
# 
# d_pt = d_pt%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# ## PVEM -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(d_pvem[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# d_pvem <- cbind(d_pvem,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(d_pvem$p2distance.2,
#                             n = length(d_pvem$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("d_pvem"), data.frame(d_pvem, estratos[["stratumID"]])) 
# 
# d_pvem = d_pvem%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# ## MC -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(d_mc[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# d_mc <- cbind(d_mc,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(d_mc$p2distance.2,
#                             n = length(d_mc$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("d_mc"), data.frame(d_mc, estratos[["stratumID"]])) 
# 
# d_mc = d_mc%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# ## PANAL -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(d_panal[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# d_panal <- cbind(d_panal,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(d_panal$p2distance.2,
#                             n = length(d_panal$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("d_panal"), data.frame(d_panal, estratos[["stratumID"]])) 
# 
# d_panal = d_panal%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# 
# ## MORENA -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(d_morena[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# d_morena <- cbind(d_morena,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(d_morena$p2distance.2,
#                             n = length(d_morena$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("d_morena"), data.frame(d_morena, estratos[["stratumID"]])) 
# 
# d_morena = d_morena%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# ## PES  -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(d_pes[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# d_pes <- cbind(d_pes,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(d_pes$p2distance.2,
#                             n = length(d_pes$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("d_pes"), data.frame(d_pes, estratos[["stratumID"]])) 
# 
# d_pes = d_pes%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# 
# #------------------------------- GOBERNADOR -------------------------------------
# ## PAN---------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(g_pan[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# g_pan <- cbind(g_pan,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(g_pan$p2distance.2,
#                             n = length(g_pan$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("g_pan"), data.frame(g_pan, estratos[["stratumID"]])) 
# 
# g_pan = g_pan%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# ## PRI -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(g_pri[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# g_pri <- cbind(g_pri,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(g_pri$p2distance.2,
#                             n = length(g_pri$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("g_pri"), data.frame(g_pri, estratos[["stratumID"]])) 
# 
# g_pri = g_pri%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# ## PRD -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(g_prd[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# g_prd <- cbind(g_prd,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(g_prd$p2distance.2,
#                             n = length(g_prd$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("g_prd"), data.frame(g_prd, estratos[["stratumID"]])) 
# 
# g_prd = g_prd%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# ## PT -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(g_pt[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# g_pt <- cbind(g_pt,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(g_pt$p2distance.2,
#                             n = length(g_pt$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("g_pt"), data.frame(g_pt, estratos[["stratumID"]])) 
# 
# g_pt = g_pt%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# ## PVEM -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(g_pvem[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# g_pvem <- cbind(g_pvem,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(g_pvem$p2distance.2,
#                             n = length(g_pvem$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("g_pvem"), data.frame(g_pvem, estratos[["stratumID"]])) 
# 
# g_pvem = g_pvem%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )
# 
# 
# ## PANAL -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(g_panal[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 60)
# 
# g_panal <- cbind(g_panal,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(g_panal$p2distance.2,
#                             n = length(g_panal$p2distance.2),
#                             Ls = 2)
# 
# 
# assign(paste0("g_panal"), data.frame(g_panal, estratos[["stratumID"]])) 
# 
# g_panal = g_panal%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     #estrato == 2 ~ "Dificiles",
#     #estrato == 3 ~ "Posibles",
#     estrato == 2 ~ "Blandas",
#     #estrato == 5 ~ "Duras"
#     )
#   )
# 
# 
# ## MORENA -------------------------------------------
# minimos <- rep(1,3)
# 
# indice <- p2distance(matriz = as.matrix(g_morena[,c(3:5)]),
#                      reference_vector = minimos,
#                      iterations = 50)
# 
# g_morena <- cbind(g_morena,indice[["p2distance"]])
# 
# estratos <- strata.cumrootf(g_morena$p2distance.2,
#                             n = length(g_morena$p2distance.2),
#                             Ls = 5)
# 
# 
# assign(paste0("g_morena"), data.frame(g_morena, estratos[["stratumID"]])) 
# 
# g_morena = g_morena%>%
#   rename(p2_index = p2distance.2,
#          estrato = "estratos...stratumID...")%>%
#   mutate(estratos = case_when(
#     estrato == 1 ~ "Imposibles",
#     estrato == 2 ~ "Dificiles",
#     estrato == 3 ~ "Posibles",
#     estrato == 4 ~ "Blandas",
#     estrato == 5 ~ "Duras")
#   )


save(ayuntamientos_index,diputados_index,gobernador_index,file = "C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Datos/Estado de Mexico/Datos_indices/resultados_indices.RData")

