
# Cambiar el partido para obtener el modelo que le corresponde 
# Los siete modelos de los partidos por los 7 partidos
# En los modelos se incluyen todos los alcances
# Se harán 48 modelos en total 
 
library(tidyverse)

general_pf <- general_pf%>%
  filter(tipo == "Ayuntamientos")

partidos_base <- c("MC","MORENA","PAN","PANAL","PRD","PRI","PT","PVEM")
  
partido <- NULL

for(partidon in partidos_base){
 
   data_base <- general_pf%>%
    filter(partido_base == partidon)
  
   
  # Organizar como clumnas a la los partidos de las primeras fuerzas 
  data_base_reg <- data_base%>%
    select(id_seccion,partidos_nuevo,votos,"graproes" ,"nivel","Edad1_1",
           "Edad2_1","Edad3_1","porcentaje_m")%>%
    unique()%>%
    pivot_wider(names_from = partidos_nuevo,values_from = votos)
  

  base_est <- data_base_reg%>%
    mutate_if(is.numeric, scale)%>%
    mutate_at(vars(!matches("nivel")), as.numeric)%>%
    as.data.frame()
  
  
  
 
  assign(paste0("model_pan","_",partidon),lm(PAN ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                            + nivel, 
                            data = base_est)
         )
 
 
  assign(paste0("model_pri","_",partidon),lm(PRI ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                           + nivel, 
                           data = base_est))
  
  
 
   assign(paste0("model_prd","_",partidon),lm(PRD ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                            + nivel, 
                            data = base_est))
  
  
   assign(paste0("model_pt","_",partidon),lm(PT ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                           + nivel, 
                           data = base_est))
 
   assign(paste0("model_pvem","_",partidon),lm(PVEM ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                             + nivel, 
                             data = base_est))
 
  
  assign(paste0("model_mc","_",partidon),lm(MC ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                             + nivel, 
                             data = base_est))
   
  
   
 assign(paste0("model_morena","_",partidon),lm(MORENA ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                               + nivel, 
                               data = base_est)) 
 
  
  assign(paste0("model_panal","_",partidon),lm(PANAL ~  Edad1_1 + Edad2_1 + Edad3_1 + porcentaje_m + graproes 
                              + nivel, 
                              data = base_est)) 

  
  
  
  
}


ls()

save("model_mc_MC" ,"model_mc_MORENA","model_mc_PAN","model_mc_PANAL",           
"model_mc_PRD","model_mc_PRI", "model_mc_PT" ,"model_mc_PVEM" ,            
 "model_morena_MC","model_morena_MORENA","model_morena_PAN" ,"model_morena_PANAL"  ,      
"model_morena_PRD","model_morena_PRI","model_morena_PT" ,"model_morena_PVEM" ,        
"model_pan_MC","model_pan_MORENA","model_pan_PAN","model_pan_PANAL" ,"model_pan_PRD",
"model_pan_PRI","model_pan_PT","model_pan_PVEM" ,"model_panal_MC" , "model_panal_MORENA"  ,      
"model_panal_PAN" ,"model_panal_PANAL","model_panal_PRD"  ,"model_panal_PRI",           
"model_panal_PT","model_panal_PVEM" ,"model_prd_MC","model_prd_MORENA",          
"model_prd_PAN","model_prd_PANAL","model_prd_PRD","model_prd_PRI",             
"model_prd_PT","model_prd_PVEM","model_pri_MC", "model_pri_MORENA",          
"model_pri_PAN" , "model_pri_PANAL","model_pri_PRD","model_pri_PRI",             
 "model_pri_PT", "model_pri_PVEM", "model_pt_MC","model_pt_MORENA",           
"model_pt_PAN","model_pt_PANAL", "model_pt_PRD","model_pt_PRI",              
"model_pt_PT",  "model_pt_PVEM","model_pvem_MC","model_pvem_MORENA",         
"model_pvem_PAN","model_pvem_PANAL","model_pvem_PRD", "model_pvem_PRI",            
"model_pvem_PT","model_pvem_PVEM",
file = "C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Scripts/elecciones/data/data_modelos.Rda"
)
