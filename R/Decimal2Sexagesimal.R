# Convert decimal lat/long data to sexagesimal 

# version 1 
# Author: Perez-Luque AJ (@ajpelu)
# Date: 2015 April 

d2hms <- function(y){
  # Argument format 36.937/N 
  
  # Extract numeric value and put absolute value 
  x <- abs(as.numeric(do.call(rbind, strsplit(as.character(y), "/"))[1]))
  
  # Extract hour data
  hora <- floor(x)
  
  # Compute minute data
  minu_aux <- (x - hora) * 60
  minu <- floor(minu_aux)
  
  # Compute seconds data
  sec <- round((minu_aux - minu)*60, 2) 
  
  # Extract orientation
  or <- do.call(rbind, strsplit(as.character(y), "/"))[,2]
  or_note <- ifelse(as.numeric(do.call(rbind, strsplit(as.character(y), "/"))[1]) < 0, 'err.', or)
  
  out <- data.frame(cbind(coord=y,hora,minu,sec,or, or_note))
  out
}

