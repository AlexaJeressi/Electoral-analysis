# Complemento del manejo de datos 

# Leer el RData original para solo guardar la informacion de la grafica de sanky 
# La modificaré con los nuevos partidos, despues 

load("C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Scripts/elecciones/data/data_all.RData")

save("grupos_sandy_ayuntamientos","nodos_name_ayuntamientos","grupos_sandy_diputados","nodos_name_diputados",
     "grupos_sandy_gobernador","nodos_name_gobernador",
     file = "C:/Users/Alexa/OneDrive - Next Gate Research/alexa/2022/10_Plataforma/Scripts/elecciones/data/data_sanky.RData")
