df=read.csv("Cap5Prob20.csv")
df

str(df)
df$Pegamento=factor(df$Pegamento)
df$Temperatura=factor(df$Temperatura)
df$Y=as.double(df$Y)

modelo=aov(Y~Pegamento*Temperatura,data=df)
summary(modelo)


boxplot(Y~Pegamento,data=df,main="Graficos de los pegamento")
boxplot(Y~Temperatura,data=df,main="Resistencia a la torsión de las adhesiones")
boxplot(Y~Pegamento*Temperatura,data=df,main="Graficos de las variables")
      
tk=TukeyHSD(modelo)
tk

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

require(car)
leveneTest(Y~Temperatura,data=df)
leveneTest(Y~Pegamento,data=df)

plot(modelo$residuals)
abline(h=0)

interaction.plot(df$Pegamento,df$Temperatura,df$Y,main="Interaccion entre las variables")



