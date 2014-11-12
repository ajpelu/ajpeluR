# Title: climateDataDossier.R  
# Script to analyze the Sierra Nevada climate data 
# version: 1.0 
# date: Nov 2014
# Authors: Perez-Luque, AJ @ajpelu <ajperez@ugr.es>
#          Perez-Perez R @rperezperez 
#          Bonet, FJ @fjbonet 
# -----------------------------------------------------------

# -----------------------------------------------------------
library('ggplot2') # To plot maps
library('plyr')
library('dplyr')
library('devtools')
library('gdata')
library('gplots')
library('gridExtra')
library('party') #cart 
source_url('https://raw.githubusercontent.com/ajpelu/ajpeluR/master/R/summarySE.R') # Summary function
source_url('https://raw.githubusercontent.com/ajpelu/ajpeluR/master/R/exportggplot.R') # Export function  
# -----------------------------------------------------------



# -----------------------------------------------------------
# Prepare Data 
# Set directory 
di <- '/Users/ajpelu/Dropbox/MS/DOSSIER2013_FICHAS/CLIMA/clima_dossier'
setwd(di)

# Read data of climate variables, elevation and natural park. 
past_precip <- read.csv(file=paste(di,'/data/past_precip.csv', sep=''), head=TRUE, sep=";", dec=".")
past_tmax <- read.csv(file=paste(di,'/data/past_tmax.csv', sep=''), head=TRUE, sep=";", dec=".")
past_tmin <- read.csv(file=paste(di,'/data/past_tmin.csv', sep=''), head=TRUE, sep=";", dec=".")

en <- read.csv(file=paste(di,'/data/espacio_natural.csv', sep=''), head=TRUE, sep=";", dec=".")
elev <- read.csv(file=paste(di,'/data/elevation.csv', sep=''), head=TRUE, sep=",", dec=".")

# Join elevation and natural park data 
eleven <- join(elev, en, type='full', by='cli_celda_id')
names(eleven) <- c('cli_celda_id','elev','pn')

# Change NA by 0, and 13 by 1 in 'pn' field
eleven[is.na(eleven)] <- 0
eleven$pn <- ifelse(eleven$pn == 13, 1, eleven$pn)

# Join elevation and pn data with climatic variables 
p_precip <- join(past_precip, eleven, type='inner', by='cli_celda_id')
p_tmax <- join(past_tmax, eleven, type='inner', by='cli_celda_id')
p_tmin <- join(past_tmin, eleven, type='inner', by='cli_celda_id')
# -----------------------------------------------------------



# -----------------------------------------------------------
# Analisis de clima pasado (vamos a centrarnos solo en sn)

#### Precipitacion 
p_precip_sn <- filter(p_precip, pn > 0) 

# 1 # ??Como es la tendencia en la precipitaci??n para SN? 

# 1 a # Tau positivo
# How many pixels have a positive trend? (positive tau values = have increased the precipitation value) 
nrow(p_precip_sn[p_precip_sn$tau>0,])
# percentage 
(nrow(p_precip_sn[p_precip_sn$tau>0,])/nrow(p_precip_sn))*100 

# Cuantos de ellos son significativos 
nrow(filter(p_precip_sn, tau > 0, p_value < 0.05)) 
#percentage (del total de taus positivos)
(nrow(filter(p_precip_sn, tau > 0, p_value < 0.05))/nrow(filter(p_precip_sn, tau > 0)))*100
#percentage (del total de pixeles de SN)
(nrow(filter(p_precip_sn, tau > 0, p_value < 0.05))/nrow(p_precip_sn))*100


# 1 b # Tau negativo
# How many pixels have a negative trend? (negative tau values = have decreased the precipitation value) 
nrow(p_precip_sn[p_precip_sn$tau<0,])
# percentage 
(nrow(p_precip_sn[p_precip_sn$tau<0,])/nrow(p_precip_sn))*100 

# Cuantos de ellos son significativos 
nrow(filter(p_precip_sn, tau < 0, p_value < 0.05)) 
#percentage (del total de taus negativos)
(nrow(filter(p_precip_sn, tau < 0, p_value < 0.05))/nrow(filter(p_precip_sn, tau < 0)))*100
#percentage (del total de pixeles de SN)
(nrow(filter(p_precip_sn, tau < 0, p_value < 0.05))/nrow(p_precip_sn))*100


# 2 # Categorizacion de las tendencias 
set.seed(0) 
# Add a variable to categorize significances 
p_precip_sn$sig <- ifelse(p_precip_sn$p_value < 0.05, 'sig', 'no sig') 

ggplot(p_precip_sn, aes(x=tau)) + geom_histogram(stat='bin', bindwidth=.1, fill='grey') + 
  facet_wrap(~sig) + theme_bw() + ggtitle('Precip')


# 3 # Relaci??n de las tendencias con la elevaci??n (CART)
fit <- ctree(tau~elev, data=p_precip_sn)
fit 
plot(fit)

# 4 # Categorizacion de las elevaciones 
# Clasificacion elevaciones
p_precip_sn$elevC <- as.factor(ifelse(p_precip_sn$elev > 3001, '3001-3500',
                            ifelse(p_precip_sn$elev > 2501, '2501-3000',
                            ifelse(p_precip_sn$elev > 2001, '2001-2500',
                            ifelse(p_precip_sn$elev > 1501, '1501-2000',        
                            ifelse(p_precip_sn$elev > 1001, '1001-1500', 
                            ifelse(p_precip_sn$elev > 501, '501-1000', '0-500')))))))
                                
# Reorder las elevaciones
p_precip_sn$elevC <- reorder.factor(p_precip_sn$elevC, new.order=c("0-500","501-1000","1001-1500","1501-2000","2001-2500","2501-3000","3001-3500"))

# Obtener los summary datos de los taus
aux.prec <- ddply(p_precip_sn, c('elevC', 'sig'), summarise,
            n= length(tau),
            mean= mean(tau),
            sd= sd(tau),
            se= sd / sqrt (n),
            .drop=FALSE)

aux1.prec <- ddply(p_precip_sn, c('elevC'), summarise,
            n.group.elev = length(tau),
            mean.group.elev = mean(tau),
            sd.group.elev = sd(tau),
            se.group.elev = sd.group.elev / sqrt (n.group.elev))

summa_p_precip_sn <- join(aux.prec, aux1.prec, type='full', by='elevC', match='all')
summa_p_precip_sn$per.sig <- ( summa_p_precip_sn$n / summa_p_precip_sn$n.group.elev )*100

# 5 # Plot combinado (Ojo solo los significativos). 
df.precip <- summa_p_precip_sn[summa_p_precip_sn$sig == 'sig',]
g.top.precip <- ggplot(df.precip, aes(x = elevC, y = per.sig)) +
  geom_bar(stat='identity') +
  theme_bw() + ylab('% sig. pixels (<0.05)') + 
  theme(plot.margin = unit(c(1,5,-30,6),units="points"),
        axis.title.y = element_text(vjust =0.25)) +
  ggtitle('Precip')
# g.top.precip 

g.bottom.precip <- ggplot(df.precip, aes(x=elevC, y=mean.group.elev, group=1)) + geom_line() + 
  geom_errorbar(aes(ymax = mean.group.elev + se.group.elev, ymin= mean.group.elev - se.group.elev), width=.15) + 
  geom_line(col='grey') + geom_point(size=3, shape=21, fill="white") + 
  theme_bw() + xlab('elevation') + ylab('tau (average value)')+ 
  theme(plot.margin = unit(c(0,5,1,1),units="points"),
        axis.text.x= element_text(angle=90)) 

# g.bottom.precip  

grid.arrange(arrangeGrob(g.top.precip, g.bottom.precip, ncol=1, nrow=2, heights = c(1/5, 4/5)))
# http://steffi.ca/thinkR/?p=91 
# -----------------------------------------------------------


# -----------------------------------------------------------
## Tmax 
p_tmax_sn <- filter(p_tmax, pn > 0) 

# 1 # ??Como es la tendencia en la tmax para SN? 

# 1 a # Tau positivo
# How many pixels have a positive trend? (positive tau values = have increased the tmax value) 
nrow(p_tmax_sn[p_tmax_sn$tau>0,])
# percentage 
(nrow(p_tmax_sn[p_tmax_sn$tau>0,])/nrow(p_tmax_sn))*100 

# Cuantos de ellos son significativos 
nrow(filter(p_tmax_sn, tau > 0, p_value < 0.05)) 
#percentage (del total de taus positivos)
(nrow(filter(p_tmax_sn, tau > 0, p_value < 0.05))/nrow(filter(p_tmax_sn, tau > 0)))*100
#percentage (del total de pixeles de SN)
(nrow(filter(p_tmax_sn, tau > 0, p_value < 0.05))/nrow(p_tmax_sn))*100


# 1 b # Tau negativo
# How many pixels have a negative trend? (negative tau values = have decreased the tmax value) 
nrow(p_tmax_sn[p_tmax_sn$tau<0,])
# percentage 
(nrow(p_tmax_sn[p_tmax_sn$tau<0,])/nrow(p_tmax_sn))*100 

# Cuantos de ellos son significativos 
nrow(filter(p_tmax_sn, tau < 0, p_value < 0.05)) 
#percentage (del total de taus negativos)
(nrow(filter(p_tmax_sn, tau < 0, p_value < 0.05))/nrow(filter(p_tmax_sn, tau < 0)))*100
#percentage (del total de pixeles de SN)
(nrow(filter(p_tmax_sn, tau < 0, p_value < 0.05))/nrow(p_tmax_sn))*100


# 2 # Categorizacion de las tendencias 
set.seed(0) 
# Add a variable to categorize significances 
p_tmax_sn$sig <- ifelse(p_tmax_sn$p_value < 0.05, 'sig', 'no sig') 

ggplot(p_tmax_sn, aes(x=tau)) + geom_histogram(stat='bin', bindwidth=.1, fill='grey') + 
  facet_wrap(~sig) + theme_bw() + ggtitle('Tmax')


# 3 # Relaci??n de las tendencias con la elevaci??n (CART)
fit <- ctree(tau~elev, data=p_tmax_sn)
fit 
plot(fit)

# 4 # Categorizacion de las elevaciones 
# Clasificacion elevaciones
p_tmax_sn$elevC <- as.factor(ifelse(p_tmax_sn$elev > 3001, '3001-3500',
                             ifelse(p_tmax_sn$elev > 2501, '2501-3000',
                             ifelse(p_tmax_sn$elev > 2001, '2001-2500',
                             ifelse(p_tmax_sn$elev > 1501, '1501-2000',        
                             ifelse(p_tmax_sn$elev > 1001, '1001-1500', 
                             ifelse(p_tmax_sn$elev > 501, '501-1000', '0-500')))))))

# Reorder las elevaciones
p_tmax_sn$elevC <- reorder.factor(p_tmax_sn$elevC, new.order=c("0-500","501-1000","1001-1500","1501-2000","2001-2500","2501-3000","3001-3500"))

# Obtener los summary datos de los taus
aux.tmax <- ddply(p_tmax_sn, c('elevC', 'sig'), summarise,
             n= length(tau),
             mean= mean(tau),
             sd= sd(tau),
             se= sd / sqrt (n),
             .drop=FALSE)

aux1.tmax <- ddply(p_tmax_sn, c('elevC'), summarise,
                   n.group.elev = length(tau),
                   mean.group.elev = mean(tau),
                   sd.group.elev = sd(tau),
                   se.group.elev = sd.group.elev / sqrt (n.group.elev))

summa_p_tmax_sn <- join(aux.tmax, aux1.tmax, type='full', by='elevC', match='all')
summa_p_tmax_sn$per.sig <- ( summa_p_tmax_sn$n / summa_p_tmax_sn$n.group.elev)*100

# 5 # Plot combinado (Ojo solo los significativos). 
df.tmax <- summa_p_tmax_sn[summa_p_tmax_sn$sig == 'sig',]
g.top.tmax <- ggplot(df.tmax, aes(x = elevC, y = per.sig)) +
  geom_bar(stat='identity') +
  theme_bw() + ylab('% sig. pixels (<0.05)') +
  theme(plot.margin = unit(c(1,5,-30,6),units="points"),
        axis.title.y = element_text(vjust =0.25)) +
  ggtitle('Tmax')
# g.top.tmax 

g.bottom.tmax <- ggplot(df.tmax, aes(x=elevC, y=mean.group.elev, group=1)) + 
  geom_errorbar(aes(ymax = mean.group.elev + se.group.elev, ymin=mean.group.elev - se.group.elev), width=.15) + 
  geom_line(col='grey') + 
  geom_point(size=3, shape=21, fill="white") + 
  theme_bw() + xlab('elevation') + ylab('tau (average value)')+ 
  theme(plot.margin = unit(c(0,5,1,1),units="points"),
        axis.text.x= element_text(angle=90)) 

# g.bottom.tmax 

grid.arrange(arrangeGrob(g.top.tmax, g.bottom.tmax, ncol=1, nrow=2, heights = c(1/5, 4/5)))
# http://steffi.ca/thinkR/?p=91 
# -----------------------------------------------------------

# -----------------------------------------------------------
# Analisis de clima pasado (vamos a centrarnos solo en sn)

## Tmin
p_tmin_sn <- filter(p_tmin, pn > 0) 

# 1 # ??Como es la tendencia en la tmax para SN? 

# 1 a # Tau positivo
# How many pixels have a positive trend? (positive tau values = have increased the tmax value) 
nrow(p_tmin_sn[p_tmin_sn$tau>0,])
# percentage 
(nrow(p_tmin_sn[p_tmin_sn$tau>0,])/nrow(p_tmin_sn))*100 

# Cuantos de ellos son significativos 
nrow(filter(p_tmin_sn, tau > 0, p_value < 0.05)) 
#percentage (del total de taus positivos)
(nrow(filter(p_tmin_sn, tau > 0, p_value < 0.05))/nrow(filter(p_tmin_sn, tau > 0)))*100
#percentage (del total de pixeles de SN)
(nrow(filter(p_tmin_sn, tau > 0, p_value < 0.05))/nrow(p_tmin_sn))*100


# 1 b # Tau negativo
# How many pixels have a negative trend? (negative tau values = have decreased the tmax value) 
nrow(p_tmin_sn[p_tmin_sn$tau<0,])
# percentage 
(nrow(p_tmin_sn[p_tmin_sn$tau<0,])/nrow(p_tmin_sn))*100 

# Cuantos de ellos son significativos 
nrow(filter(p_tmin_sn, tau < 0, p_value < 0.05)) 
#percentage (del total de taus negativos)
(nrow(filter(p_tmin_sn, tau < 0, p_value < 0.05))/nrow(filter(p_tmin_sn, tau < 0)))*100
#percentage (del total de pixeles de SN)
(nrow(filter(p_tmin_sn, tau < 0, p_value < 0.05))/nrow(p_tmin_sn))*100

# 2 # Categorizacion de las tendencias 
set.seed(0) 
# Add a variable to categorize significances 
p_tmin_sn$sig <- ifelse(p_tmin_sn$p_value < 0.05, 'sig', 'no sig') 

ggplot(p_tmin_sn, aes(x=tau)) + geom_histogram(stat='bin', bindwidth=.1, fill='grey') + 
  facet_wrap(~sig) + theme_bw() + ggtitle('Tmin')


# 3 # Relacion de las tendencias con la elevaci??n (CART)
fit <- ctree(tau~elev, data=p_tmin_sn)
fit 
plot(fit)

# 4 # Categorizacion de las elevaciones 
# Clasificacion elevaciones
p_tmin_sn$elevC <- as.factor(ifelse(p_tmin_sn$elev > 3001, '3001-3500',
                             ifelse(p_tmin_sn$elev > 2501, '2501-3000',
                             ifelse(p_tmin_sn$elev > 2001, '2001-2500',
                             ifelse(p_tmin_sn$elev > 1501, '1501-2000',        
                             ifelse(p_tmin_sn$elev > 1001, '1001-1500', 
                             ifelse(p_tmin_sn$elev > 501, '501-1000', '0-500')))))))

# Reorder las elevaciones
p_tmin_sn$elevC <- reorder.factor(p_tmin_sn$elevC, new.order=c("0-500","501-1000","1001-1500","1501-2000","2001-2500","2501-3000","3001-3500"))

# Obtener los summary datos de los taus
aux.tmin <- ddply(p_tmin_sn, c('elevC', 'sig'), summarise,
             n= length(tau),
             mean= mean(tau),
             sd= sd(tau),
             se= sd / sqrt (n),
             .drop=FALSE)

aux1.tmin <- ddply(p_tmin_sn, c('elevC'), summarise,
                   n.group.elev = length(tau),
                   mean.group.elev = mean(tau),
                   sd.group.elev = sd(tau),
                   se.group.elev = sd.group.elev / sqrt (n.group.elev))

summa_p_tmin_sn <- join(aux.tmin, aux1.tmin, type='full', by='elevC', match='all')
summa_p_tmin_sn$per.sig <- ( summa_p_tmin_sn$n / summa_p_tmin_sn$n.group.elev )*100

# 5 # Plot combinado (Ojo solo los significativos). 
df.tmin <- summa_p_tmin_sn[summa_p_tmin_sn$sig == 'sig',]
g.top.tmin <- ggplot(df.tmin, aes(x = elevC, y = per.sig)) +
  geom_bar(stat='identity') +
  theme_bw() + ylab('% sig. pixels (<0.05)') + 
  theme(plot.margin = unit(c(1,5,-30,6),units="points"),
        axis.title.y = element_text(vjust =0.25)) + 
  ggtitle('Tmin') + ylim(0, 1)
# g.top.tmin 

g.bottom.tmin <- ggplot(df.tmin, aes(x=elevC, y=mean.group.elev, group=1)) + 
  geom_errorbar(aes(ymax = mean.group.elev + se.group.elev, ymin=mean.group.elev - se.group.elev), width=.15) + 
  geom_line(col='grey') + 
  geom_point(size=3, shape=21, fill="white") + 
  theme_bw() + xlab('elevation') + ylab('tau (average value)')+ 
  theme(plot.margin = unit(c(0,5,1,1),units="points"),
        axis.text.x= element_text(angle=90))  

# g.bottom 

grid.arrange(arrangeGrob(g.top.tmin, g.bottom.tmin, ncol=1, nrow=2, heights = c(1/5, 4/5)))
# http://steffi.ca/thinkR/?p=91 


# Ahora todos los graficos combinados
gbtmax <- g.bottom.tmax + ylim(-0.3,.3)
gbtmin <- g.bottom.tmin + ylim(-0.3,.3)
gbprecip <- g.bottom.precip + ylim(-0.3,.3)

grid.arrange(arrangeGrob(g.top.precip, g.top.tmax, g.top.tmin, gbprecip, gbtmax, gbtmin, ncol=3, nrow=2, heights = c(1/5, 4/5)))

