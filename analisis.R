#==================================================================================
#Análisis de datos
#==================================================================================

library(stargazer);library(tidyverse);library(readxl);library(car);library(dplyr)
library(lmtest);library(sandwich);library(tables); library(corrplot)
base<-read_excel("../Econometria/base_limpia.xlsx")

# **********************************************************************
#Estadísticas descriptivas
# **********************************************************************

#Convertir en factor la variable tratamiento
base$tratamiento<-as.factor(base$tratamiento) #Categoría base: control

#Convertir en factor las variables categóricas sociodemográficas
base$sexo<-as.factor(base$sexo) #Categoría base: hombre
base$laboral<-as.factor(base$laboral) #Categoría base: desempleado(a)
base$nivel_edu<-as.factor(base$nivel_edu)
base$nivel_edu<-relevel(base$nivel_edu, ref="Primaria") #Categoría base: primaria
base$edad<-as.factor(base$edad)
base$edad<-relevel(base$edad, ref="18 - 25 años") #Categoría base: 18-25 años

#Convertir en factor las variables políticas
base$elecciones_primera<-as.factor(base$elecciones_primera)
base$elecciones_primera<-relevel(base$elecciones_primera, ref="Iván Duque Márquez") #Categoría base: Duque
base$perc_gobierno<-as.factor(base$perc_gobierno) #Categoría base: negativa
base$perc_vacunacion<-as.factor(base$perc_vacunacion) #Categoría base: negativa
base$dispuesto_vacunarse<-as.factor(base$dispuesto_vacunarse) #Categoría base: No
base$perc_vacunas<-as.factor(base$perc_vacunas) #Categoría base:negativa
base$tipo_vacuna<-as.factor(base$tipo_vacuna) #Categoría base: No

#Presición, positivas, negativas y neutrales según tratamiento
est<-tabular(tratamiento~
                    (precision+num_positiva+num_negativa+num_neutral)*
                    (mean+sd+min+max)+
                    (n=1),
                    data=base)

# **********************************************************************
#Análisis teniendo solo la variable de tratamiento
# **********************************************************************

#Regresión con variable independiente únicamente: tratamiento
reg1<-lm(precision~tratamiento, data=base)
clas1<-coeftest(reg1)
rob1<-coeftest(reg1, vcov = vcovHC(reg1, "HC1"))
stargazer(reg1, type="text", se=list(rob1[,"Std. Error"]))

# **********************************************************************
#Análisis teniendo como controles las variables sociodemográficas
# **********************************************************************

#Regresión con variables independientes: sociodemográficas y tratamiento 
reg2<-lm(precision~tratamiento + sexo + laboral + nivel_edu + edad + 
        superior + sexo*tratamiento + laboral*tratamiento + 
        tratamiento*nivel_edu + tratamiento*edad, data=base)     
rob2<-coeftest(reg2, vcov = vcovHC(reg2, "HC1"))

stargazer(reg2,type="text",se=list(rob2[,"Std. Error"]))

# **********************************************************************
#Análisis teniendo como controles las variables de política
# **********************************************************************

#Regresión con variables de política
reg3<-lm(precision~tratamiento + tratamiento*espectro_1 + tratamiento*perc_gobierno + 
         tratamiento*perc_vacunacion + dispuesto_vacunarse + perc_vacunas + 
         tipo_vacuna,data=base)
rob3<-coeftest(reg3,vcov=vcovHC(reg3,"HC1"))
stargazer(reg3,type="text",se=list(rob3[,"Std. Error"]))

# **********************************************************************
#Análisis teniendo como variables independientes la percepción
# **********************************************************************

#Regresión con variables independientes: interacción de percepción y tratamiento
reg4<-lm(precision~ tratamiento*num_positiva + tratamiento*num_negativa, data=base) 
rob4<-coeftest(reg4, vcov = vcovHC(reg4, "HC1"))
stargazer(reg4, type="text", se=list(rob4[,"Std. Error"]))

# **********************************************************************
#Análisis teniendo como variables independientes todas las anteriores
# **********************************************************************

#Regresión con variables independientes: sociodemográficas y política
reg5<-lm(precision~tratamiento + sexo + laboral + nivel_edu + edad +
         sexo*tratamiento + laboral*tratamiento + tratamiento*nivel_edu +
         tratamiento*edad+ tratamiento*espectro_1 + tratamiento*perc_gobierno + 
         tratamiento*perc_vacunacion + dispuesto_vacunarse + perc_vacunas + 
         tipo_vacuna, data=base)
rob5<-coeftest(reg5,vcov = vcovHC(reg6,"HC1"))
stargazer(reg5, type="text", se=list(rob5[,"Std. Error"]))

#Regresión con variables independiendientes: sociodemográficas, política y percepeciones
reg6<-lm(precision~tratamiento + sexo + laboral + nivel_edu + edad +
                 sexo*tratamiento + laboral*tratamiento + tratamiento*nivel_edu +
                 tratamiento*edad+ tratamiento*espectro_1 + tratamiento*perc_gobierno + 
                 tratamiento*perc_vacunacion + dispuesto_vacunarse + perc_vacunas + 
                 tipo_vacuna + tratamiento* num_positiva + tratamiento*num_negativa,
                 data=base)
rob6<-coeftest(reg6,vcov = vcovHC(reg6,"HC1"))
stargazer(reg6, type="text", se=list(rob6[,"Std. Error"]))

#Mostrar todas las regresiones realizadas
stargazer(reg1,reg2,reg3,reg4,reg5,reg6,type="text",se=list(rob1[,"Std. Error"],
                                                            rob2[,"Std. Error"],
                                                            rob3[,"Std. Error"],
                                                            rob4[,"Std. Error"],
                                                            rob5[,"Std. Error"],
                                                            rob6[,"Std. Error"]))
