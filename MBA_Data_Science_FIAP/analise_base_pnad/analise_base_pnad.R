install.packages("sqldf")
install.packages("tibble")
install.packages("plotly")
library(tibble)
library(dplyr)
library(sqldf)
library(plotly)

library(readxl)
pnad_0 <- read_excel("C:/Users/diego.plima/Downloads/pnad_0.xlsx")
View(pnad_0)

#analisando base de dados
summary(pnad_0)

#consultando nome das colunas
names(pnad_0)

#porcentagem de missings da base
dim(pnad_0)

quanttotalvalores <- 22451 * 218

quantmissing <- sum(is.na(pnad_0))

porcentagemmissing <- (quantmissing / quanttotalvalores) * 100
porcentagemmissing

quantvaloresnmissing <- quanttotalvalores - quantmissing

porcvaloresnmissing <- (quantvaloresnmissing / quanttotalvalores) * 100
porcvaloresnmissing

valores <- list(porcentagemmissing,porcvaloresnmissing)
valores

valores2 <- unlist(valores)
valores2


#verificando se existe diferença de região do pais em relação a variavel rendim.efetivoqqtrabalho

#NORMALIZAÇÃO DOS DADOS
#regularizando o nome de alguns estados
pnad_0$UF[pnad_0$UF == "Santa Catari"] <- "Santa Catarina"
pnad_0$UF[pnad_0$UF == "Mis Gerais"] <- "Minas Gerais"
pnad_0$UF[pnad_0$UF == "Amazos"] <- "Amazonas"

#regularizando NA da coluna Rendim.efetivoqqtrabalho
for (i in 1: nrow(pnad_0)){
  if (is.na(pnad_0[i, "Rendim.efetivoqqtrabalho"])){
    pnad_0[i, "Rendim.efetivoqqtrabalho"] <- 0
  }
}

pnad_0["Rendim.efetivoqqtrabalho"]

#incluindo coluna REGIAODOPAIS
pnad_0$regiaodopais <- sqldf("select
                              case 
                                when UF in ('Amazonas','Roraima','Amapá','Pará','Tocantins','Rondônia','Acre') then
                                  'Norte'
                                when UF in ('Maranhão','Piauí','Ceará','Rio Grande do Norte','Pernambuco', 'Paraíba', 'Sergipe', 'Alagoas', 'Bahia') then
                                  'Nordeste'
                                when UF in ('Mato Grosso','Mato Grosso do Sul','Goiás') then
                                  'Centro-Oeste'
                                when UF in ('São Paulo','Rio de Janeiro','Espírito Santo','Minas Gerais') then
                                  'Sudeste'
                                else
                                  'Sul'
                                end regiao
                             from pnad_0")

#agrupando Rendim.efetivoqqtrabalho por região
resultado <- group_by(pnad_0,regiaodopais)%>%summarise(Total=sum(Rendim.efetivoqqtrabalho))
resultado

totalresultado <- sum(resultado$Total)
totalresultado


barplot((resultado$Total*100)/totalresultado, names.arg = resultado$regiaodopais)
title(xlab = "Regiões do pais", ylab="Porcentagem %")


#verificando qual estado tem maior porcentagem de homens e mulheres

df <- data.frame(sqldf("select
              UF,
              sum(case when Sexo = 'Mulher' then 1 else 0 end) Mulher,
              sum(case when Sexo = 'Homem' then 1 else 0 end) Homem
            from pnad_0
            group by UF
            order by Mulher desc, Homem desc"))

df

df2 <- read.table(text = "SãoPaulo MinasGerais RiodeJaneiro
                  890 842 840
                  881 828 748", header = TRUE)

barplot(as.matrix(df2),col=c("red","blue"))


#analisando o perfil dessa base

#criando novo data frame
df1 <- data.frame(pnad_0[,c('Sexo', 'Idadedatadereferência', 'Corouraça', 
                 'Frequentaescola','Sabelereescrever',
                 'Rendim.efetivoqqtrabalho','regiaodopais')])

install.packages(c("caret","rpart","rattle"))
library(caret)
library(rpart)
library(rattle)



#verificando se há diferença estatisticamente para quem frequenta escola comparando com corouraça e idade

#criando novo data frame
quest5 <- data.frame(pnad_0[,c('Corouraça', 'Frequentaescola')])

#retirando registros com valores missing
quest5 <- na.omit(quest5)

dim(quest5)

#narmalizando dados
quest5_2 <- sqldf("select
                    Corouraça corouraca,
                    sum(case
                      when Frequentaescola = 'Não' then 0
                      when Frequentaescola = 'Sim' then 1
                    end) frequentaescola
                  from quest5
                  group by Corouraça")

quest5_2

plot_ly(x=quest5_2$corouraca, y=quest5_2$frequentaescola, type ="bar")

#estudo usando a variável rendim.efetivoqqtrabalho como target e busca alguma correlação 
#linear com as outras variáveis da base. Aqui você pode usar uma regressão linear usando python 
#(disciplina Machine learning ou buscar apenas gráficos de dispersão


#criando novo data frame, somente com as variveis numericas
quest6 <- data.frame(pnad_0[,c('Rendim.efetivoqqtrabalho',
    'Númerodepessoasnodomicílio', 'Idadedatadereferência',
    'Tempodeafastamenentoaté1ano', 'Tempodeafastamen.maisd2anos', 'Ocupaçãonotrab.principal',
    'Atividadenotrab.principal',  'Hrshabituaisnotrab.princ', 'Hrsefetivasnotrab.princ',
    'Ocupaçãonotrab.secundário', 'Atividadenotrab.secundário', 'Valordorend.hab.emdinheiro_A', 
    'Valordorend.efe.emdinheiro_A', 'Hrshabituaisnotrab.secun', 'Hrsefetivasnotrab.secun',
    'Valordorend.hab.emdinheiro_B', 'Valordorend.efe.emdinheiro_B', 'Hrshabituaisnosoutros',
    'Hrsefetivasnosoutrostrab', 'Tempotentandotrab.1mes1ano', 'Tempotentandotrab.1ano2anos',
    'Tempotentandotrab.de2anos', 'Númerodecomponentesdodomic', 'Rendim.habitualtrab.princ',
    'Rendim.efetivotrab.princ', 'Rendim.habitualqqtrabalho', 'Hrshabituaisemtodostrab', 
    'Hrsefetivasemtodostrab')])


#retirando registros com valores missing
for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Rendim.efetivoqqtrabalho"])){
    quest6[i, "Rendim.efetivoqqtrabalho"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Tempodeafastamenentoaté1ano"])){
    quest6[i, "Tempodeafastamenentoaté1ano"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Tempodeafastamen.maisd2anos"])){
    quest6[i, "Tempodeafastamen.maisd2anos"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Ocupaçãonotrab.principal"])){
    quest6[i, "Ocupaçãonotrab.principal"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Atividadenotrab.principal"])){
    quest6[i, "Atividadenotrab.principal"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Hrsefetivasnotrab.princ"])){
    quest6[i, "Hrsefetivasnotrab.princ"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Ocupaçãonotrab.secundário"])){
    quest6[i, "Ocupaçãonotrab.secundário"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Atividadenotrab.secundário"])){
    quest6[i, "Atividadenotrab.secundário"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Valordorend.hab.emdinheiro_A"])){
    quest6[i, "Valordorend.hab.emdinheiro_A"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Valordorend.efe.emdinheiro_A"])){
    quest6[i, "Valordorend.efe.emdinheiro_A"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Hrshabituaisnotrab.secun"])){
    quest6[i, "Hrshabituaisnotrab.secun"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Hrsefetivasnotrab.secun"])){
    quest6[i, "Hrsefetivasnotrab.secun"] <- 0
  }
}
for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Valordorend.hab.emdinheiro_B"])){
    quest6[i, "Valordorend.hab.emdinheiro_B"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Valordorend.efe.emdinheiro_B"])){
    quest6[i, "Valordorend.efe.emdinheiro_B"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Hrshabituaisnosoutros"])){
    quest6[i, "Hrshabituaisnosoutros"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Hrsefetivasnosoutrostrab"])){
    quest6[i, "Hrsefetivasnosoutrostrab"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Tempotentandotrab.1mes1ano"])){
    quest6[i, "Tempotentandotrab.1mes1ano"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Tempotentandotrab.1ano2anos"])){
    quest6[i, "Tempotentandotrab.1ano2anos"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Tempotentandotrab.de2anos"])){
    quest6[i, "Tempotentandotrab.de2anos"] <- 0
  }
}

for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Rendim.habitualtrab.princ"])){
    quest6[i, "Rendim.habitualtrab.princ"] <- 0
  }
}


for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Rendim.efetivotrab.princ"])){
    quest6[i, "Rendim.efetivotrab.princ"] <- 0
  }
}


for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Rendim.habitualqqtrabalho"])){
    quest6[i, "Rendim.habitualqqtrabalho"] <- 0
  }
}


for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Hrshabituaisemtodostrab"])){
    quest6[i, "Hrshabituaisemtodostrab"] <- 0
  }
}


for (i in 1: nrow(quest6)){
  if (is.na(quest6[i, "Hrsefetivasemtodostrab"])){
    quest6[i, "Hrsefetivasemtodostrab"] <- 0
  }
}

#criando matriz de correlação

quest6_cor <- cor(quest6)
quest6_cor

dim(quest6_cor)


