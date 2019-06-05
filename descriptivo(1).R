rm(list=ls())
library(readr)
residuales_2018 <- read_csv("spi 2019/Heats_2018 - heats_2018.csv")

library(plyr)
library(dplyr)
library(knitr)
library(kableExtra)
library(lme4)
library(optimx)

#Eliminando observaciones con Pourback distinto de cero
residuales_2018<-residuales_2018 %>% filter(POURBACK==0)

#Eliminando observaciones con numero de muestras distintas a 1 
residuales_2018<-residuales_2018 %>% filter(NUM_EAF_SAMPLES==1)

#Eliminando observaciones sin valor en heel y completo
residuales_2018<-residuales_2018 %>% filter(!is.na(PREV_ELEM_CU))
residuales_2018<-residuales_2018 %>% filter(!is.na(ELEM_CU))

#Eliminando valores de charge-manual distinto de cero
residuales_2018<-residuales_2018 %>% filter(CHARGE_MANUAL_WT==0)

#Eliminando valores de charge-bucket distinto de cero
residuales_2018<-residuales_2018 %>% filter(CHARGE_BUCKET_WT>0)

#Removemos el caso en que existe analisis pero no hay datos de scraps
#residuales_2018<-residuales_2018[-match(1816500,residuales_2018$HEAT_ID),]

#Eliminando valores con tiempo de "espera" muy alto
#residuales_2018<-residuales_2018 %>% filter(IDLE_TIME>12)

#Revisando conteos de valores por tipo de scrap
scraps<- residuales_2018[,19:66]>0
frecuencias<-apply(scraps, 2, sum)

#Reasignando los valores de scraps con valores no nulos
scraps<- residuales_2018[,19:66]
frecuencias<-frecuencias[frecuencias>0]
scraps<-scraps[,names(frecuencias)]
frecuencias2<-apply(scraps>0, 2, sum)   #Variables efectivas
kable(sort(frecuencias2, decreasing = TRUE))

#Contando frecuencias por grado (GRADE)
kable(table(residuales_2018$GRADE))

#Comparando Charge_bucket vs suma de los scraps
suma_scraps<-apply(scraps,1, sum)
table(residuales_2018$CHARGE_BUCKET_WT==suma_scraps)

#Revisando numero de scraps por GRADE
numero_scraps_usados<-apply(scraps>0,1, sum)
table(residuales_2018$GRADE, numero_scraps_usados)

#Comparando charge_Bucket_wt vs Tap_wt
table(residuales_2018$CHARGE_BUCKET_WT<=residuales_2018$TAP_WT)

#Resumen de residuales por grado (cobre)
residuales2<-residuales_2018 %>% select(GRADE, ELEM_CU) %>% group_by(GRADE) %>% summarise(minimo=min(ELEM_CU), media=mean(ELEM_CU), mediana=median(ELEM_CU), maximo=max(ELEM_CU), dev_std=sd(ELEM_CU))
kable(residuales2)

#Generando las graficas de recetas por GRADO y número de scraps usados
scraps_escalado<-as.data.frame(t(apply(scraps, 1, function(x) x/sum(x))))
scraps_escalado["Grado"]<-residuales_2018$GRADE

par(mfrow=c(1,1))      #Se pueden modificar estos valores y los de la linea de abajo para obtener grupos de graficas de barras apiladas
for (j in unique(scraps_escalado$Grado)[c(21)]){
  data<-matrix(0L, ncol=9, nrow=23)
  x<-NULL                                           #
  for (i in 1:9){
    a<-scraps_escalado[numero_scraps_usados==i,]
    if (sum(a$Grado==j)>0){
      x<-c(x,i)                                     #
      a<-a[a$Grado==j,]
      data[,i]<-sapply(a[,-24], sum)
      data[,i]<-data[,i]/sum(data[,i])
    }
  }
    data2<-data[,x]                                 #
    colnames(data2)<-paste(x)                       #
    rownames(data2)<-names(a[,-24])
    barplot(data2, col=colors()[c(6,10,11,12,17,19,24,26,124,32,33,44,50,51,53,56,62,75,79,83,31,100,116)], border="white", xlab="Número de scraps usados", ylab="Proporción de la mediana", font.axis=2, main=paste("Receta, Grado:",j))
}


#Leyenda de los colores
par(mfrow=c(1,1))
x <- 1:23
MyLab <- paste("Sc",x)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", MyLab, pch=16, pt.cex=2, cex=1.5, bty='n',
       col = colors()[c(6,10,11,12,17,19,24,26,124,32,33,44,50,51,53,56,62,75,79,83,31,100,116)], ncol=3)
mtext("Tipo de scrap", at=0.2, cex=2)

#tabla de identificadores
data.frame(Scraps=names(a[,-24]), identificador=MyLab)

#posibles Variables a eliminar usando un criterio de aportacion en general a los procesos
sort(apply(scraps_escalado[,-24],2,sum))

