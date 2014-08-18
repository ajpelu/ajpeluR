effectSize <- function(g1, g2){
  # Funcion para calcular se 
  se <- function(x) {sd(x)/sqrt(length(x))}
  
  # Calculo varianza y tamanio muestral de cada grupo
  var1 <- var(g1)
  n1 <- length(g1)
  var2 <- var(g2)
  n2 <- length(g2)
  
  # Calculo los efectos
  s.pooled <- sqrt((((n1 - 1)*var1) + ((n2 - 1)*var2))/((n1+n2)-2))
  dC = ((mean(g1)-mean(g2))/s.pooled)
  dH = dC*(1 - (3/((4*((n1+n2)-2))-1)))
  
  # Interpretamos el efecto 
  EffectSize <- ifelse(dH > 1.30, 'Very Large', 
                       ifelse(dH > 0.80, 'Large', 
                              ifelse(dH > 0.50, 'Medium',
                                     ifelse(dH > 0.20, 'Small','Trivial')))) 
  
  # Estadisticas por grupo
  statistics <- data.frame(group=c(deparse(substitute(g1)),
                                   deparse(substitute(g2))),
                           N=c(n1,n2),
                           MEAN=c(mean(g1),mean(g2)),
                           VAR=c(var1,var2),
                           SD=c(sd(g1),sd(g2)),
                           SE=c(se(g1),se(g2)))
  
  # Calculamos amplitud intervalo de confianza (apx)
  n.tot <- (n1+n2)
  se.d = sqrt(((n.tot -1)/(n.tot -3))*((4/(n.tot))*(1 + ((dC*dC)/8))))
  se.g = sqrt(((n.tot)/(n1*n2))+((dH*dH)/(2*(n.tot - 2))))
  ci.dC=(1.96)*(se.d)
  ci.dH=(1.96)*(se.g)  					 
  
  # Resumen Efectos 
  res.Efectos <- data.frame(Type=c("Cohen's d", "Hedge's g"),
                            d=c(dC, dH), 
                            CI.low=c((dC - ci.dC), (dH - ci.dH)),
                            CI.upp=c((dC + ci.dC), (dH + ci.dH)),
                            Interp.=c(EffectSize,EffectSize))				 
  
  x <- list(d.Cohen = dC, g.Hedge=dH, Effect=EffectSize, summ=res.Efectos, stats=statistics)
  # print(x) 
}