rm(list=ls())
library(readr)
residuales_2018 <- read_csv("C:/Users/Daniel/Downloads/Heats_2018 - heats_2018.csv.csv")

library(MuMIn)
library(dplyr)
library(knitr)
library(kableExtra)
library(tsoutliers)
library(lme4)
library(lmerTest)
library(TSdist)
library(optimx)
library(nmle)
library(sjPlot)

#Eliminando observaciones con Pourback distinto de cero
residuales_2018<-residuales_2018 %>% filter(POURBACK==0)

#Eliminando observaciones con numero de muestras distintas a 1 
residuales_2018<-residuales_2018 %>% filter(NUM_EAF_SAMPLES==1)

#Eliminando observaciones sin valor en heel y completo
residuales_2018<-residuales_2018 %>% filter(!is.na(PREV_ELEM_CU))
residuales_2018<-residuales_2018 %>% filter(!is.na(ELEM_CU))

#Eliminando valores de charge-manual distinto de cero
residuales_2018<-residuales_2018 %>% filter(CHARGE_MANUAL_WT==0)

#Eliminando valores con tiempo de "espera" muy alto
#residuales_2018<-residuales_2018 %>% filter(IDLE_TIME>12)


### Borra las filas cuya suma de scraps sea 0 (signfica que no tiene ningun registro en esa colada)
residuales_2018<-residuales_2018[-which(rowSums(residuales_2018[19:66])==0),]

#Revisando conteos de valores por tipo de scrap
scraps<- residuales_2018[,19:66]>0
frecuencias<-apply(scraps, 2, sum)

#Reasignando los valores de scraps con valores no nulos
scraps<-residuales_2018[,19:66]
frecuencias<-frecuencias[frecuencias>0]
scraps<-scraps[names(frecuencias)]
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

###se Crea scraps_relativo diviendo cada entrada entre la suma de su respectiva fila(colada) 
scraps_relativo<-data.frame(t(apply(scraps, 1, function(i) i/sum(i))))

### Se calcula la variable respuesta "y" 
y<-residuales_2018$ELEM_CU-0.30*residuales_2018$PREV_ELEM_CU
### se agrega "y" y "GRADE" al data.frame de scraps_relativo
scraps_relativo["y"]<-y
scraps_relativo["GRADE"]<-as.factor(residuales_2018$GRADE)
### se crea una uestra de 10 datos aleatorios para la semilla
semilla<-sample(1:100,10,replace = F)
### se inicializa el array de Errores
Error<-rep(0,10)

### Este for tiene como finalidad  hacer una validaci?n cruzada 
for (i in 1:10) {
  ### se define la semilla i
  set.seed(semilla[i])
  ### se hace la muestra del 80% para Train y 20% Test
  aux<-sample(1:nrow(scraps_relativo),size = 0.80*nrow(scraps_relativo))
  scraps_relativo_train<-scraps_relativo[aux,]
  scraps_relativo_test<-scraps_relativo[-aux,]
  
  ### Se utiliza el modelo de Regresi?n de Coeficientes aleatorios para las 13 variables seleccionadas
  re.lm <-lmerTest::lmer(formula = y ~ SCRAP_BILLETS + SCRAP_N2_FRAG + SCRAP_P_S + SCRAP_N1_RR +
                          SCRAP_CLIP + SCRAP_A_SCRAP + SCRAP_BROKEN_RAILS + SCRAP_PMS +
                          SCRAP_N2_HEAVY_MELT + SCRAP_TURNING_BRIQUETTES + SCRAP_N1_FRAG +
                          SCRAP_TUNDISH_SKULL + SCRAP_LR_SPECIAL_BUSHLING + 0 + (SCRAP_BILLETS +
                                                                                   SCRAP_N2_FRAG + SCRAP_P_S + SCRAP_N1_RR + SCRAP_CLIP + SCRAP_A_SCRAP +
                                                                                   SCRAP_BROKEN_RAILS + SCRAP_PMS + SCRAP_N2_HEAVY_MELT + SCRAP_TURNING_BRIQUETTES +
                                                                                   SCRAP_N1_FRAG + SCRAP_TUNDISH_SKULL + SCRAP_LR_SPECIAL_BUSHLING +      0 | GRADE),data = scraps_relativo_train)

  ### Se hace el predict para los datos de Test
  re.pred<-predict(object = re.lm,newdata  =  scraps_relativo_test,allow.new.levels = TRUE)
  ### se calcula 1 -  el error absoluto relativo
  Error[i]<- 1 - sum(abs(scraps_relativo_test$y-re.pred)/sum(scraps_relativo_test$y))
  print(i)
}

#Error
plot(Error,type = "p",ylim = c(0,1))

plot(Error,type = "p", col="dodgerblue4", cex=1.5, ylab="Precisi?n", xlab="Fold", main="Precisi?n del modelo: residual Cu",pch =19)
lines(rep(mean(Error), 10),lwd = 5)


