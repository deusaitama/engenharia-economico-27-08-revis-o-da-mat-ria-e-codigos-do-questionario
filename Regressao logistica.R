
install.packages("caret")
library(caret)

glimpse(titanic2)

titanic2$Sobreviveu <- factor((titanic2$Sobreviveu))

glimpse(titanic2)

titanic2$Classe <- factor(titanic2$Classe)

glimpse(titanic2)

###################################################
# Análise da associação entre sexo e sobrivência

esquisser(titanic2)

#Análise da associação entre Tarifa e Sobrevivência

titanic2 %>% group_by(Sobreviveu) %>% summarise(media=mean(Tarifa))

# Análise da associação entre Idade e Sobrevivencia

titanic2 %>% group_by(Sobreviveu) %>% summarise(media=mean(Idade))

# Analise da associação entre sobrevivencia e classe

esquisser(titanic2)

#################################################
#treino(+- 80%): ajuste do modelo
#teste(+-20%): verificar a qualidade do modelo

indices<-sample(1:891,712,replace=FALSE)

treino<-titanic2[indices,]
teste<-titanic2[-indices,]

modelo<-glm(formula = Sobreviveu~.,family = "binomial",data = treino)
summary(modelo)

modelo_2<-glm(formula = Sobreviveu~Classe+Sexo+Idade+IrmEsp,family = "binomial",data = treino)
summary(modelo_2)

#Predições na amostra de teste

predicoes<-predict(modelo_2,newdata = teste,type = 'response')
predicoes<-ifelse(predicoes>0.5,1,0)

confusionMatrix(factor(predicoes),factor(teste$Sobreviveu))

# Eu morreria?

eu<-data.frame(Classe="3",Sexo="male",Idade=22,IrmEsp=1,PaiFilho=0,Tarifa=15,Embarque="C")

eu_morreria<-predict(modelo_2,newdata=eu,type='response')
ifelse(eu_morreria>0.5,'sobrevivi','morri')
























































