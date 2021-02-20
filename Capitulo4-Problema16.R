df=read.csv("CATALIZADOR16.csv",sep=";")
df

str(df)
df$DIA=factor(df$DIA)
df$LOTE=factor(df$LOTE)
df$CATALIZADOR=factor(df$CATALIZADOR)
df$Y=as.double(df$Y)

modelo=aov(Y~DIA+LOTE+CATALIZADOR,data=df)
summary(modelo)

boxplot(Y~CATALIZADOR,data=df)
tk=TukeyHSD(modelo)
tk

boxplot(Y~LOTE,data=df)
tk=TukeyHSD(modelo)
tk

boxplot(Y~DIA,data=df)
tk=TukeyHSD(modelo)
tk


qqnorm(modelo$residuals)
qqline(modelo$residuals)
shapiro.test(modelo$residuals)

library(car)
leveneTest(Y~CATALIZADOR,data=df)
