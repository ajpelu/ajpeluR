# dia.juliano 

# This function extract a julian day from a date
# version 1 
# Author: Perez-Luque AJ (@ajpelu)
# Date: 2012 Dec 
## http://ajperezluque.com/calcular-dias-julianos-en-r


dia.juliano <- function(x){
  ## función que devuelve el día juliano del año
  ## formato de fecha de entrada: dd/mm/año
  ## version: 1.0
  ## autor: Pérez-Luque, A.J.
  ## http://ajperezluque.com/calcular-dias-julianos-en-r
  # a # Transformo en formato Date
  date.text <- as.Date(x,"%d/%m/%Y")  
  # b # Extraigo el año
  year = as.numeric(format(date.text, format = "%Y"))
  # c # Calculo el dia juliano bruto de la fecha
  juliano.bruto <- as.numeric(julian(date.text))
  # d # Calculo el dia juliano para el primer dia del año de esa fecha
  # d1 # Creo el primer dia del año de la fecha dada
  dia.referencia.year <- paste("01/01/",year, sep='') 
  # d2 # Calculo el dia juliano para esa fecha
  juliano.ref <- as.numeric(julian(as.Date(dia.referencia.year,"%d/%m/%Y")))
  # e # Restar día juliano fecha - dia juliano primer dia del año de la fecha
  juliano <- (juliano.bruto - juliano.ref)
  print(juliano)}
