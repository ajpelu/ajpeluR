# Title: Simulaciones_Futuro.R
# Script to analyze the Sierra Nevada climate data 
# version: 1.0 
# date: Nov 2014
# Authors: Perez-Luque, AJ @ajpelu <ajperez@ugr.es>
#          Perez-Perez R @rperezperez 
#          Bonet, FJ @fjbonet 
# -----------------------------------------------------------

# -----------------------------------------------------------
library('plyr')
library('reshape')
library('ggplot2')
library('devtools')
source_url('https://raw.githubusercontent.com/ajpelu/ajpeluR/master/R/summarySE.R') # Summary function
# -----------------------------------------------------------

# -----------------------------------------------------------
# Prepare data
## Simulated data 
# set directory 
di <- '/Users/ajpelu/Dropbox/MS/DOSSIER2013_FICHAS/CLIMA/clima_dossier'

# Read data of simulation for tmin, tmax and precip for all SN
sim <- read.csv(file=paste(di,'/data/sim_2011_2100.csv', sep=''), head=TRUE, sep=";", dec=".")
sim <- sim[,-3]
# -----------------------------------------------------------

# -----------------------------------------------------------
## Precipitation analysis 
precip <- sim[sim$codigo == 'precip',]

for (i in c('echam4a2','echam4b2', 'cgcm2a2', 'cgcm2b2')) {
  df <- subset(precip, escenario_clima==paste(i))
  aux <- cast(df, cli_celda_id ~ ano, value='valor')
  aux <- as.data.frame(aux)
  names(aux) <- c('cli_celda_id', 'y2011', 'y2100') 
  aux$dif <- (aux$y2100 - aux$y2011)
  aux$escenario_clima <- rep(i, nrow(aux))
  assign(paste0("precip", i), aux)} 
  
dif.precip <- rbind(precipcgcm2a2,precipcgcm2b2, precipecham4a2, precipecham4b2)  

ggplot(dif.precip, aes(x=dif)) + 
  geom_histogram(fill='darkgreen') + 
  facet_wrap(~escenario_clima) + theme_bw() + 
  xlab('Diferencia precip 2100-2011')

dif.precip.summary <- summarySE(dif.precip, measurevar = 'dif', groupvars='escenario_clima')
dif.precip.summary$variable <- rep('precip', nrow(dif.precip.summary))
dif.precip.summary
# -----------------------------------------------------------


# -----------------------------------------------------------
## Tmax analysis 
tmax <- sim[sim$codigo == 'tmax',]
tmax$valor <- (tmax$valor)/10

for (i in c('echam4a2','echam4b2', 'cgcm2a2', 'cgcm2b2')) {
  df <- subset(tmax, escenario_clima==paste(i))
  aux <- cast(df, cli_celda_id ~ ano, value='valor')
  aux <- as.data.frame(aux)
  names(aux) <- c('cli_celda_id', 'y2011', 'y2100') 
  aux$dif <- (aux$y2100 - aux$y2011)
  aux$escenario_clima <- rep(i, nrow(aux))
  assign(paste0("tmax", i), aux)} 

dif.tmax <- rbind(tmaxcgcm2a2,tmaxcgcm2b2, tmaxecham4a2, tmaxecham4b2) 

ggplot(dif.tmax, aes(x=dif)) + 
  geom_histogram(fill='darkgreen') + 
  facet_wrap(~escenario_clima) + theme_bw() + 
  xlab('Diferencia tmax 2100-2011')

dif.tmax.summary <- summarySE(dif.tmax, measurevar = 'dif', groupvars='escenario_clima')
dif.tmax.summary$variable <- rep('tmax', nrow(dif.tmax.summary))
dif.tmax.summary
# -----------------------------------------------------------


# -----------------------------------------------------------
## tmin analysis 
tmin <- sim[sim$codigo == 'tmin',]
tmin$valor <- (tmin$valor)/10

for (i in c('echam4a2','echam4b2', 'cgcm2a2', 'cgcm2b2')) {
  df <- subset(tmin, escenario_clima==paste(i))
  aux <- cast(df, cli_celda_id ~ ano, value='valor')
  aux <- as.data.frame(aux)
  names(aux) <- c('cli_celda_id', 'y2011', 'y2100') 
  aux$dif <- (aux$y2100 - aux$y2011)
  aux$escenario_clima <- rep(i, nrow(aux))
  assign(paste0("tmin", i), aux)} 

dif.tmin <- rbind(tmincgcm2a2,tmincgcm2b2, tminecham4a2, tminecham4b2) 

ggplot(dif.tmin, aes(x=dif)) + 
  geom_histogram(fill='darkgreen') + 
  facet_wrap(~escenario_clima) + theme_bw() + 
  xlab('Diferencia tmin 2100-2011')

dif.tmin.summary <- summarySE(dif.tmin, measurevar = 'dif', groupvars='escenario_clima')
dif.tmin.summary$variable <- rep('tmin', nrow(dif.tmin.summary))
dif.tmin.summary
# -----------------------------------------------------------

# -----------------------------------------------------------
# Join the summary
dif.summary <- rbind(dif.precip.summary, dif.tmax.summary, dif.tmin.summary)
dif.summary
# -----------------------------------------------------------






  
  