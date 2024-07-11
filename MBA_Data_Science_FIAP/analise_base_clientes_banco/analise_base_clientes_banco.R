#ANALISE EXPLORATORIA DE DUAS BASE DE DADOS DE UM BANCO DE VAREJO
#primeira (CADASTRAL) contendo informações dos clientes e a
#segunda (TRANSACIONAL) contendo informações emprestimos realizados pelos clientes.

############ PRIMEIRA PARTE ##################

#importação e visualização da base CADASTRAL
library(readxl)
Cadastral <- read_excel("C:/Cadastral.xlsx")
View(Cadastral)

install.packages("summarytools")
library(summarytools)

#duplicando dataframe e analisando dados da coluna Sexo
A <- unique(Cadastral)

freq(A$Sexo)

#incluindo nova coluna no dataframe
A$DataAtual <- Sys.Date()

A

#verificando tipo de dado da coluna Salario
class(A$salario)

#verificando max e min da coluna salario
min(A$salario)
max(A$salario)

#verificando todos os dados estatistico da coluna salario
descr(A$salario)

#criando faixa salarial com a função CUT e gravando no data frame
A$fx_salario <-cut(A$salario,
                       breaks = c(1574,3000,5000,7000,13500),
                       labels=c("1574 - 3000","3001 - 5000",
                                "5001 - 7000","7001 - 13500"))
A

#importação e visualização da base TRANSACIONAL
library(readxl)
B <- read_excel("C:/Transacional.xlsx")
View(B)

#unindo os data frame com função MERGE
Cadastral_Transac <- merge(x=B, y=A, by="ID", all.x=TRUE)
Cadastral_Transac

#criando coluna "Comprometimento da Renda" com resultado da divisão Valor Emprestimentimo e Salario
Cadastral_Transac$CrompRenda <- (Cadastral_Transac$ValorEmprestimo / Cadastral_Transac$salario)

Cadastral_Transac


install.packages("gmodels")
library(gmodels)


#analise quantidade de pessoas na base por número de filhos e sexo
CrossTable(Cadastral_Transac$Sexo, Cadastral_Transac$NumerodeFilhos, chisq = TRUE)


install.packages("forecast")
library(forecast)

install.packages("ggplot2","plotly")
library(ggplot2)
library(plotly)


#analise quantiade de clientes por sexo e classificação
grafico_barras <- ggplot2(Cadastral_Transac, aes(x=fx_salario, fill=Sexo))+
  geom_bar(position = 'dodge')+
  xlab("Grupos definido de faixa salarial")+
  ylab("Numeros de clientes")+
  ggtitle("Gráfico de Barra")

grafico_barras


############ SEGUNDA PARTE ##################

install.packages(c("caret","rpart","rattle"))
library(caret)
library(rpart)
library(rattle)

rnorm(20)
rnorm(20)
set.seed(12345);rnorm(20)
set.seed(12345);rnorm(20)


#arvore de decisão 1
mytree <- rpart(default ~  QtdaParcelas + Atraso + ValorEmprestimo + QtdaPagas,
                data = Cadastral_Transac,
                method = "class",
                parms = list(split = 'gini'),
                minsplit = 2,
                minbucket = 1)

mytree

table(Cadastral_Transac$default)
table(Cadastral_Transac$QtdaParcelas)
table(Cadastral_Transac$Atraso)
table(Cadastral_Transac$ValorEmprestimo)
table(Cadastral_Transac$QtdaPagas)

fancyRpartPlot(mytree)

mytree$variable.importance

Cadastral_Transac$prob <-predict(mytree, newdata = Cadastral_Transac, type="prob")
Cadastral_Transac$classificacao <-predict(mytree, newdata = Cadastral_Transac, type="class")

tabela <-table(Cadastral_Transac$default, Cadastral_Transac$classificacao)
tabela

acuracia <- (tabela[1]+tabela[4])/sum(tabela)
acuracia

confusionMatrix(factor(Cadastral_Transac$default),
                factor(Cadastral_Transac$classificacao))


#arvore de decisão 2
mytree2 <- rpart(default ~  Sexo + TempodeServiço + EstadoCivil + NumerodeFilhos,
                data = Cadastral_Transac,
                method = "class",
                parms = list(split = 'gini'),
                minsplit = 2,
                minbucket = 1)

mytree2

names(Cadastral_Transac)

table(Cadastral_Transac$default)
table(Cadastral_Transac$Sexo)
table(Cadastral_Transac$TempodeServiço)
table(Cadastral_Transac$EstadoCivil)
table(Cadastral_Transac$NumerodeFilhos)

fancyRpartPlot(mytree2)

mytree2$variable.importance

Cadastral_Transac$prob <-predict(mytree2, newdata = Cadastral_Transac, type="prob")
Cadastral_Transac$classificacao <-predict(mytree2, newdata = Cadastral_Transac, type="class")

tabela2 <-table(Cadastral_Transac$default, Cadastral_Transac$classificacao)
tabela2

acuracia2 <- (tabela2[1]+tabela2[4])/sum(tabela2)
acuracia2

confusionMatrix(factor(Cadastral_Transac$default),
                factor(Cadastral_Transac$classificacao))


#arvore de decisão 3
mytree3 <- rpart(default ~  TempodeResidencia + Conta + fx_salario + CrompRenda,
                 data = Cadastral_Transac,
                 method = "class",
                 parms = list(split = 'gini'),
                 minsplit = 2,
                 minbucket = 1)

mytree3

table(Cadastral_Transac$default)
table(Cadastral_Transac$TempodeResidencia)
table(Cadastral_Transac$Conta)
table(Cadastral_Transac$fx_salario)
table(Cadastral_Transac$CrompRenda)

fancyRpartPlot(mytree3)

mytree3$variable.importance

Cadastral_Transac$prob <-predict(mytree3, newdata = Cadastral_Transac, type="prob")
Cadastral_Transac$classificacao <-predict(mytree3, newdata = Cadastral_Transac, type="class")

tabela3 <-table(Cadastral_Transac$default, Cadastral_Transac$classificacao)
tabela3

acuracia3 <- (tabela3[1]+tabela3[4])/sum(tabela3)
acuracia3

confusionMatrix(factor(Cadastral_Transac$default),
                factor(Cadastral_Transac$classificacao))


#arvore de decisão 4
mytree4 <- rpart(default ~  classificacao + QtdaParcelas + Atraso + TempodeResidencia,
                 data = Cadastral_Transac,
                 method = "class",
                 parms = list(split = 'gini'),
                 minsplit = 2,
                 minbucket = 1)

mytree4

table(Cadastral_Transac$default)
table(Cadastral_Transac$classificacao)
table(Cadastral_Transac$QtdaParcelas)
table(Cadastral_Transac$Atraso)
table(Cadastral_Transac$TempodeResidencia)

fancyRpartPlot(mytree4)

mytree4$variable.importance

Cadastral_Transac$prob <-predict(mytree4, newdata = Cadastral_Transac, type="prob")
Cadastral_Transac$classificacao <-predict(mytree4, newdata = Cadastral_Transac, type="class")

tabela4 <-table(Cadastral_Transac$default, Cadastral_Transac$classificacao)
tabela4

acuracia4 <- (tabela4[1]+tabela4[4])/sum(tabela4)
acuracia4

confusionMatrix(factor(Cadastral_Transac$default),
                factor(Cadastral_Transac$classificacao))

#arvore de decisão 5
mytree5 <- rpart(default ~  Atraso + CrompRenda + NumerodeFilhos + ValorEmprestimo,
                 data = Cadastral_Transac,
                 method = "class",
                 parms = list(split = 'gini'),
                 minsplit = 2,
                 minbucket = 1)

mytree5

table(Cadastral_Transac$default)
table(Cadastral_Transac$Atraso)
table(Cadastral_Transac$CrompRenda)
table(Cadastral_Transac$NumerodeFilhos)
table(Cadastral_Transac$ValorEmprestimo)

fancyRpartPlot(mytree5)


mytree5$variable.importance

Cadastral_Transac$prob <-predict(mytree5, newdata = Cadastral_Transac, type="prob")
Cadastral_Transac$classificacao <-predict(mytree5, newdata = Cadastral_Transac, type="class")

tabela5 <-table(Cadastral_Transac$default, Cadastral_Transac$classificacao)
tabela5

acuracia5 <- (tabela5[1]+tabela5[4])/sum(tabela5)
acuracia5

confusionMatrix(factor(Cadastral_Transac$default),
                factor(Cadastral_Transac$classificacao))
