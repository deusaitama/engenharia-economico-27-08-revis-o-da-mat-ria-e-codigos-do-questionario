#TV: investimento em progaganda na TV
# Radio: investimento em propaganda na radio
#Newspaper: investimento em propganda no jornal impresso
#Sales: milhares de unidades vendidas de um produto

# Quero usar um modelo de regressão para vendas Sales como y e Tv como x

library(corrplot)

#1) Matriz de correlações

M<-cor(concrete_data)
corrplot(M,method='number')


cement, superplastic,age,water,fineagg,coarseagg, slag,ash.

#2) Analise individualpara tv

ggplot(data=concrete_data,aes(x=age,y=strength))+
  geom_point(size=4)+
  xlab("age")+
  ylab("strength")+
  theme_classic()


#Bonus: grafico de perfis
plot(advertising)

#3) Analie de regressão

#1 usando todas as variaveis

modelo_1<-lm(formula=strength~.,data=concrete_data)
summary(modelo_1)

#2 tirando as variaveis nao significativas

modelo_2<-lm(formula = Sales~TV+Radio,data = advertising)
summary(modelo_2)

# Diagnostico dos residuos

#1 - Verificação da média(testar se média = 0)
t.test(modelo_1$residuals)

#vamlor-p=1.Como e maior que 0,05, não há evidencias para rejeitar a hipótese de media 0.

#2- Verificação de normalidade

qqnorm(modelo_2$residuals)
qqline(modelo_2$residuals)

shapiro.test(modelo_1$residuals) # valor-p<0,05(há elementos para descartar a hipotese de normalidade)

# por conta de dois outlier, o teste de shapiro rejeita normalidade

hist(modelo_2$residuals)

#3 Homocedasticidade

plot(modelo_2$fitted.values,modelo_2$residuals)
abline(0,0)

#verificam-e indicios de homocedasticidade