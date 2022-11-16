# Paulo Vitor Silva- Estatistica UFOP

# Limpa a mem�ria do software R
rm(list = ls())

# Utiliza virgula para demarcar casas decimais (padrao portugues)
options(OutDec = ",")

# Carrega os pacotes que serao utilizados
require(ggplot2)
require(MASS)
require(car)
require('gridExtra')
require(hnp)

# ------------------------------------------------------------------------------
# Primeiro Passo: Realizar a Leitura dos Dados

# Leitura dos dados

dadosvereadores <- read.csv2("candidatosBh.csv",sep = ";")
head(dadosvereadores)
summary(dadosvereadores)

resultados <- read.csv2("resultadoVereadoresBH.csv",sep = ",")
head(resultados)
summary(resultados)

dados= merge(dadosvereadores, resultados, by.x = "Candidato...nome.de.urna")
dim(dados)
head(dados)

dados=dados[,-c(2,3,10,12)] #excluindo coluna partido pois se repete , a variavel nacionalidade e a variavel porcentagem de votos 
dim(dados)
colnames(dados)= c("candidato","idade","genero", #renomeando variaveis
                   "escolaridade","estadoCivil",
                   "cor_ra�a","possuiBens","votos")
summary(dados)

dados[dados$votos==max(dados$votos),] # consultando linhas ( candidato mais votado)

### Tranformando vetores em fatores ordenados

dados$genero=factor(dados$genero)
dados$genero= factor(dados$genero,levels(dados$genero)[c(2, 1)])
levels(dados$genero)
dados$escolaridade=factor(dados$escolaridade)
dados$escolaridade=factor(dados$escolaridade, levels(dados$escolaridade)[c(5,2,1,4,3,7,6)])
levels(dados$escolaridade)
dados$estadoCivil=factor(dados$estadoCivil)
dados$estadoCivil=factor(dados$estadoCivil , levels(dados$estadoCivil)[c(4,1,2,3,5)])
levels(dados$estadoCivil)
dados$cor_ra�a=factor(dados$cor_ra�a)
dados$cor_ra�a=factor(dados$cor_ra�a , levels(dados$cor_ra�a)[c(2,5,4,1,3,6)])
levels(dados$cor_ra�a)
dados$possuiBens=factor(dados$possuiBens)
dados$possuiBens=factor(dados$possuiBens , levels(dados$possuiBens)[c(2,1)])
levels(dados$possuiBens)=c("Sim","N�o")


dados1=dados
dados1$votos=(dados$votos)^(1/3) # aplicando ra�z cubica
# para uma melhor visualiza�ao gr�fica foi aplicado a ra�z c�bica na variavel resposta.

dim(dados1)
head(dados1)
# ------------------------------------------------------------------------------
## Análise descritiva dos Dados

# 1) Se X for cont�?nua, utilize um gráfico de Dispersão

# Gráfico de Dispersão  utilizando o pacote GGPLOT 
g1 = ggplot(dados1, aes(y=`votos`, x=`idade`)) + 
  geom_point(size = 1) + 
  geom_smooth(method=lm, se=F)+
  theme_bw()+
  labs(y="Raiz c�bica dos votos ", x="Idade",  title = "(a)")+
  theme(axis.text.x = element_text(hjust = 1, size=12),
        axis.text.y = element_text(hjust = 1, size=12),
        plot.title = element_text(hjust = 0.5),
        axis.title=element_text(size=12))

cor.test(dados1$votos,dados$idade)

# Se X for uma vari�vel categ�rica, uma op��o � utilizar Boxplot

# Variavel Sexo
g2 = ggplot(dados1, aes(x=factor(`genero`), y=`votos`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Raiz c�bica dos votos", x="G�nero",  title = "(b)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5))

# escolaridade
g3 = ggplot(dados1, aes(x=factor(`escolaridade`), y=`votos`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Raiz c�bica dos votos", x=" Escolaridade ",  title = "(a)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))
 #estado civil
g4 = ggplot(dados1, aes(x=factor(`estadoCivil`), y=`votos`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Raiz c�bica dos votos", x="Estado Civil",  title = "(b)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))
 # cor_ra�a
g5 = ggplot(dados, aes(x=factor(`cor_ra�a`), y=`votos`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Raiz c�bica dos votos", x="Cor/Ra�a",  title = "(a)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))
# possui bens
g6 = ggplot(dados1, aes(x=factor(`possuiBens`), y=`votos`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Raiz c�bica dos votos", x=" Possui bens em seu nome",  title = "(b)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5))

# Partido
# g7 = ggplot(dados1, aes(x=factor(`partido`), y=`votos`)) +
#   geom_boxplot(fill = "lightblue")+
#   labs(y="Raiz c�bica dos votos", x="Partido")+
#   theme_bw(base_size = 12)+
#   theme(plot.title = element_text(hjust = 0.5),
#         axis.text.x = element_text(angle = 90))


#Organiza todos os gráficos em uma �nica figura
windows()
grid.arrange(arrangeGrob(g1, g2,ncol = 2, nrow =1))
grid.arrange(arrangeGrob(g3, g4,ncol = 2, nrow =1))
grid.arrange(arrangeGrob(g5, g6,ncol = 2, nrow =1))
#g7
##------------------------------------------------------------------------------------------

## Ajuste do Modelo
head(dados)
dados=dados[,-1]  
## Ajuste do modelo poisson --------------------------------------------------------
modelo.poisson = glm( votos ~ idade + genero + escolaridade + estadoCivil + 
                        cor_ra�a + possuiBens, 
             family = poisson, data = dados)

summary(modelo.poisson)
step(modelo.poisson)

deviance = with(modelo.poisson, cbind(Deviance = deviance, 
                              "Graus de Liberdade" = df.residual,
                              "P-valor" = pchisq(deviance, df.residual, 
                                                 lower.tail=FALSE)))
deviance

confint.default(modelo.poisson)

#O valor do Coeficiente de Determina��o Ajustado
modelo.poisson$adj.r.squared

#Gr�fico de res�duos
par(mgp=c(1.2,0.5,0),mar=c(2,2,2,2))
par(mfrow=c(2,2))
res <- residuals(modelo.poisson, type="deviance");
y_ajustado = modelo.poisson$fitted
plot(y_ajustado,res,xlab="Valores ajustados",
     ylab="Desvio Residual",pch=19,col="black", cex.axis = 0.6, cex.lab = 0.6, cex = 0.6)
abline(h=c(-3,0,3), col = 'red', lty =2)
plot(res, ylab="Desvio Residual",xlab="Ordem",cex = 0.6,
     pch=19,col="black", cex.axis = 0.6, cex.lab = 0.6); abline(h=c(-3,0,3), col = 'red', lty =2)
hist(res, main = "", ylab = "Frequ�ncia", 
     xlab = "Desvio Residual", cex.axis = 0.6, cex.lab = 0.6)
qqnorm(res, cex.axis = 0.6, cex.lab = 0.6, cex.main = 0.6, cex = 0.6)

# Gr�fico de envelope
par(mfrow=c(1,1))
hnp(modelo.poisson, xlab = 'Percentil da N(0,1)', ylab = 'Res�duos', main = '', cex.axis = 0.7, cex.lab = 0.7)


# Modelo Binomial Negativa --------------------------------------------------------------------------

modelo.bn = glm.nb(votos ~ idade + genero + escolaridade + estadoCivil + cor_ra�a + possuiBens, data = dados, link = log)
summary(modelo.bn) # modelo com fun�ao de liga�ao sqrt
deviance = with(modelo.bn, cbind(Deviance = deviance, 
                              "Graus de Liberdade" = df.residual,
                              "P-valor" = pchisq(deviance, df.residual, 
                                                 lower.tail=FALSE)))
deviance
# Novo modelo sem variavel idade qaundo funcoa ligacao for sqrt

modelo.bn = glm.nb(votos ~ genero + escolaridade + estadoCivil + cor_ra�a + possuiBens, data = dados,link = log) # sem a variavel idade
summary(modelo.bn)
deviance = with(modelo.bn, cbind(Deviance = deviance, 
                              "Graus de Liberdade" = df.residual,
                              "P-valor" = pchisq(deviance, df.residual, 
                                                 lower.tail=FALSE)))
deviance

# Res�duos
par(mfrow=c(2,2))
res <- residuals(modelo.bn, type="deviance");
y_ajustado = modelo.bn$fitted
min.y = min(res, -3)
max.y = max(res, 3)

# Gr�fico de res�duos
plot(y_ajustado,res,xlab="Valores ajustados",
     ylab="Desvio Residual",pch=19,col="black", cex.axis = 0.6, cex.lab = 0.6, cex = 0.6, ylim = c(min.y,max.y))
abline(h=c(-3,0,3), col = 'red', lty =2)
plot(res, ylab="Desvio Residual",xlab="Ordem",cex = 0.6,
     pch=19,col="black", cex.axis = 0.6, cex.lab = 0.6, ylim = c(min.y,max.y)); abline(h=c(-3,0,3), col = 'red', lty =2)
hist(res, main = "", ylab = "Frequ�ncia", 
     xlab = "Desvio Residual", cex.axis = 0.6, cex.lab = 0.6)
qqnorm(res, cex.axis = 0.6, cex.lab = 0.6, cex.main = 0.6, cex = 0.6)

# Gr�fico de envelope
par(mfrow=c(1,1))
hnp(modelo.bn, xlab = 'Percentil da N(0,1)', ylab = 'Res�duos', main = '', cex.axis = 0.7, cex.lab = 1)

# Modelo Quase-Verossimilhan�a --------------------------------------------------------------------------

modelo.quasi = glm(votos ~  idade + genero + escolaridade + estadoCivil + cor_ra�a + possuiBens , 
                   family = quasipoisson, data = dados)

summary(modelo.quasi)
summary(modelo.quasi)$dispersion
deviance = with(modelo.quasi, cbind(Deviance = deviance, 
                                     "Graus de Liberdade" = df.residual,
                                     "P-valor" = pchisq(deviance, df.residual, 
                                                        lower.tail=FALSE)))
deviance

# retirando variaveis n�o significativas
modelo.quasi2 = glm(votos ~ . -possuiBens, 
                   family = quasipoisson, data = dados)

summary(modelo.quasi2)
deviance = with(modelo.quasi2, cbind(Deviance = deviance, 
                                 "Graus de Liberdade" = df.residual,
                                 "P-valor" = pchisq(deviance, df.residual, 
                                                    lower.tail=FALSE)))
deviance
modelo.quasi=modelo.quasi2
#Gr�fico de res�duos
par(mgp=c(1.2,0.5,0),mar=c(2,2,2,2))
par(mfrow=c(2,2))
res <- residuals(modelo.quasi, type="deviance");
y_ajustado = modelo.quasi$fitted
plot(y_ajustado,res,xlab="Valores ajustados",
     ylab="Desvio Residual",pch=19,col="black", cex.axis = 0.6, cex.lab = 0.6, cex = 0.6)
abline(h=c(-3,0,3), col = 'red', lty =2)
plot(res, ylab="Desvio Residual",xlab="Ordem",cex = 0.6,
     pch=19,col="black", cex.axis = 0.6, cex.lab = 0.6); abline(h=c(-3,0,3), col = 'red', lty =2)
hist(res, main = "", ylab = "Frequ�ncia", 
     xlab = "Desvio Residual", cex.axis = 0.6, cex.lab = 0.6)
qqnorm(res, cex.axis = 0.6, cex.lab = 0.6, cex.main = 0.6, cex = 0.6)

par(mfrow=c(1,1))
hnp(modelo.quasi, xlab = 'Percentil da N(0,1)', ylab = 'Res�duos', main = '', cex.axis = 0.7, cex.lab = 0.7)

# escolhendo o melhor modelo
ajuste = c('Modelo Poisson', 'Modelo Binomial Negativa ', 'Quasi-Verossimilhan�a')

aic    = c(AIC(modelo.poisson), AIC(modelo.bn), AIC(modelo.quasi))

deviance = c(deviance(modelo.poisson),deviance(modelo.bn),deviance(modelo.quasi))

data.frame(ajuste, aic, deviance)

