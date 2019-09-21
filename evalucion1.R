library(tidyverse)
library(readr)
library(dplyr)
#Cargar los datos csv
all.data1 <- read.csv ("tabla2.csv", sep=";", dec = ",", header=TRUE)

data1<-all.data1
colnames(data1)

data2 <- data1 %>% select(MSW05_Family, MSW05_Genus, MSW05_Species,X5.1_AdultBodyMass_g,X15.1_LitterSize, X9.1_GestationLen_d,X17.1_MaxLongevity_m,X16.1_LittersPerYear, X23.1_SexualMaturityAge_d, X6.2_TrophicLevel, X13.1_AdultHeadBodyLen_mm)

data3<-data2 %>% rename("Familia"=MSW05_Family, "Genero"=MSW05_Genus, "Especie"=MSW05_Species, "Peso"=X5.1_AdultBodyMass_g,"Crias"=X15.1_LitterSize, "Gestacion"=X9.1_GestationLen_d,"Longevidad"=X17.1_MaxLongevity_m, "CriasXano"= X16.1_LittersPerYear,"Madurez_sexual"=X23.1_SexualMaturityAge_d, "Nivel_trofico"=X6.2_TrophicLevel,"Tamano"=X13.1_AdultHeadBodyLen_mm) %>% filter(Tamano!=-999,Madurez_sexual!=-999, Peso!=-999,Longevidad!=-999,Gestacion!=-999, Crias!=-999, CriasXano!=-999) 

data4<-data3 %>% filter (Familia=="Sciuridae")
data6<-filter(data3, Familia %in% c("Sciuridae", "Cricetidae","Gliridae"))
##plots

ggplot(data4,aes(Tamano, Peso, colour=Genero)) + geom_point()+ theme(panel.background = element_rect(fill = "transparent"),axis.line = element_line(colour="black")) + ggtitle("Familia Sciuridae") + labs(x = "Tamaño (cm)", y= "Peso (g)") 

ggplot(data4,aes(Longevidad, Gestacion, colour=Genero)) + geom_point()

ggplot(data6,aes(Familia, Longevidad)) + geom_boxplot()


Tabla1<- data4 %>% group_by(Genero) %>% summarize(Tamano=mean(Tamano),Gestacion=mean(Gestacion))
