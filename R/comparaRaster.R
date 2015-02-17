# comparaRaster function  
# Esta funcion compara dos raster y obtiene la matriz de transicion 

# version 1 
# Author: Perez-Luque AJ (@ajpelu) & Moreno-Llorca, R. 
# Date: 2015 Feb 

comparaRaster <- function(x, y, nclases){ 
  # x es el raster inicial 
  # y es el raster final
  # nclases es el numero de clases 
  require(raster)
  
  # Resta Inicial - Final 
  resta <- x - y 
  
  # contador 
  nclases <- rep(1:nclases)
  histo <- data.frame() 
  
  for (i in nclases){ 
    # Selecciona clase inicial 
    sel_inicial <- ifelse(x[]!=i, 99999, 1)
    sel_inicial[sel_inicial[]> 100] <- NA
    # aux[aux[]<= -100] <- NA
    
    aux <- sel_inicial*resta 
    # aux[aux[]>100] <- NA
    # aux[aux[]<= -100] <- NA
    
    # Calcula n celdas por categoria 
    auxdf <- as.data.frame(table(aux[]))
    # Crea variable con clase inicial+
    auxdf$inicial <- rep(i,nrow(auxdf))
    
    # Crea variable con clase final
    auxdf$final <- ((as.numeric(levels(auxdf$Var1))[auxdf$Var1])*-1)+auxdf$inicial
    
    histo <- rbind(histo, auxdf)
  }
  print(histo)
}