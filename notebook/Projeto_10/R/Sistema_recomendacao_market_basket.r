### Projeto com Feedback 10 - Sistema de Recomendação Para Rede de Varejo Usando Market Basket Analysis

# Quer você faça compras com listas de compras meticulosamente
# planejadas ou deixe que o capricho guie seus passos, nossos rituais únicos de
# compra definem quem somos. XPTO, um aplicativo de pedido e entrega de
# supermercado, tem como objetivo facilitar o preenchimento de sua geladeira e
# despensa com seus itens pessoais favoritos e itens básicos quando você precisar
# deles.

# Depois de selecionar produtos por meio do aplicativo XPTO, os
# compradores revisam seus pedidos, fazem compras e a entrega é feita na loja
# mais próxima a você. A equipe de ciência de dados da XPTO desempenha um papel importante
# no fornecimento dessa experiência de compra agradável. Atualmente, eles usam
# dados transacionais para desenvolver modelos que preveem quais produtos um
# usuário comprará novamente, quais tentará pela primeira vez ou quais
# adicionaráao carrinho durante uma sessão. Recentemente, a XPTO disponibilizou
# esses dados de forma aberta e o link para download você encontra logo abaixo
# Neste projeto de ciência de dados, você usará esses dados anônimos nos
# pedidos dos clientes ao longo do tempo para prever quais produtos adquiridos
# anteriormente estarão no próximo pedido de um usuário.

## Objetivo
# Nossa recomendação é que você utilize linguagem R e os pacotes de Market Basket Analysis oferecidos em R.





# Instalando pacote dados
install.packages("arules")
install.packages("arulesViz")
install.packages("recommenderlab")

### Importando biblioteca
library(readr)
library(arules)
library(arulesViz)
library(dplyr)
library(reshape2)
library(tidyverse)
library(knitr)
library(gridExtra)
library(lubridate)

### Base dados treinamento produtos
data_train <- read.csv("ROTATION_of_products01.01.2018-09.01.2019.csv", sep = ";", header = TRUE)
head(data_train)

# Verifique os nomes das colunas no dataframe
names(data_train)

# Criar um novo conjunto de dados apenas com produtos únicos e um identificador único
produtos_unicos <- data_train %>%
  select(Pname) %>%
  distinct() %>%
  mutate(id = seq_along(Pname))

# Juntar os dados originais com o identificador único
dados_com_id <- left_join(data_train, produtos_unicos, by = "Pname") %>%
  select(id, Pname)

# Transformar os dados para o formato de transações
transacoes <- as(dados_com_id, "transactions")
transacoes

## Modelo 1 - Apriori
modelo <- apriori(transacoes,
                  parameter = list(supp = 0.1, conf = 0.8, maxlen = 3))

# Visualizando modelo
modelo

# Visualização das regras (opcional)
inspect(modelo)

# Fazer recomendações
library(recommenderlab)

# Criar um objeto "transactions" a partir do dataset
transacoes <- as(dados_com_id, "transactions")

# Criar um objeto "binaryRatingMatrix" a partir do dataset
matriz_avaliacoes <- as(transacoes, "binaryRatingMatrix")

# Criar um objeto "recommender" usando as regras de associação encontradas
rec_modelo <- Recommender(matriz_avaliacoes,
                          method = "IBCF",
                          param = list(k = 5, threshold = 0.001))
rec_modelo

# Fazer recomendações para um usuário fictício (substitua pelos produtos que o usuário comprou)
usuario_ficticio <- as(transacoes[1:2, ], "binaryRatingMatrix")
recomendacoes <- predict(rec_modelo, usuario_ficticio, n = 15)
recomendacoes

# Plot 1
# Criar o gráfico do grafo das regras
plot(modelo, method = "graph")
plot(modelo, method = "graph", control = list(type = "items"))

# Plot 2
# gráfico de matriz (heatmap) das regras:
plot(modelo, method = "matrix", measure = "lift")
plot(modelo, method = "matrix", measure = "confidence", shading = "confidence")

modelo <- apriori(transacoes,
                  parameter=list(sup=supportLevels[4],
                                 conf=confidenceLevels[9],
                                 target="rules"))

### Modelo 02

# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.01, 0.005)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Empty integers
rules_sup10 <- integer(length=9)
rules_sup5 <- integer(length=9)
rules_sup1 <- integer(length=9)
rules_sup0.5 <- integer(length=9)

# Apriori
for (i in 1:length(confidenceLevels)) {
  rules_sup10[i] <- length(apriori(transacoes,
                                   parameter=list(sup=supportLevels[1],
                                                  conf=confidenceLevels[i], target="rules")))
}

# Apriori
for (i in 1:length(confidenceLevels)){
  rules_sup5[i] <- length(apriori(transacoes,
                                  parameter=list(sup=supportLevels[2],
                                                 conf=confidenceLevels[i], target="rules")))
}

# Apriori
for (i in 1:length(confidenceLevels)){
  rules_sup1[i] <- length(apriori(transacoes, parameter=list(sup=supportLevels[3],
                                                        conf=confidenceLevels[i], target="rules")))
}

# Apriori
for (i in 1:length(confidenceLevels)){
  rules_sup0.5[i] <- length(apriori(transacoes, parameter=list(sup=supportLevels[4],
                                                          conf=confidenceLevels[i], target="rules")))
}

# Number
plot1 <- qplot(confidenceLevels, rules_sup10, geom=c("point", "line"),
               xlab="Confidence level", ylab="Number of rules found",
               main="Apriori with a support level of 10%") +
  theme_bw()

# Number
plot2 <- qplot(confidenceLevels, rules_sup5, geom=c("point", "line"),
               xlab="Confidence level", ylab="Number of rules found",
               main="Apriori with a support level of 5%") +
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  theme_bw()

# Number
plot3 <- qplot(confidenceLevels, rules_sup1, geom=c("point", "line"),
               xlab="Confidence level", ylab="Number of rules found",
               main="Apriori with a support level of 1%") +
  scale_y_continuous(breaks=seq(0, 50, 10)) +
  theme_bw()

# Number
plot4 <- qplot(confidenceLevels, rules_sup0.5, geom=c("point", "line"),
               xlab="Confidence level", ylab="Number of rules found",
               main="Apriori with a support level of 0.5%") +
  scale_y_continuous(breaks=seq(0, 130, 20)) +
  theme_bw()

# Subplot
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

# Data frame
num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_rules, aes(x=confidenceLevels)) +

  geom_line(aes(y=rules_sup10, colour="Support level of 10%")) +
  geom_point(aes(y=rules_sup10, colour="Support level of 10%")) +

  geom_line(aes(y=rules_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=rules_sup5, colour="Support level of 5%")) +

  geom_line(aes(y=rules_sup1, colour="Support level of 1%")) +
  geom_point(aes(y=rules_sup1, colour="Support level of 1%")) +

  geom_line(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +

  labs(x="Confidence levels", y="Number of rules found",
       title="Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())

# Modelo 2 - Apriori
rules_sup1_conf50 <- apriori(transacoes, parameter=list(sup=supportLevels[3],
                                                   conf=confidenceLevels[5], target="rules"))

# Inspect
inspect(rules_sup1_conf50)

# Graph
plot(rules_sup1_conf50, method="graph")

# Graph
plot(rules_sup1_conf50, method="graph", control=list(layout=igraph::in_circle()))

# Grouped
plot(rules_sup1_conf50, method="grouped")

# Apriori
rules_sup0.5_conf10 <- apriori(trans, parameter=list(sup=supportLevels[4], conf=confidenceLevels[9], target="rules"))
