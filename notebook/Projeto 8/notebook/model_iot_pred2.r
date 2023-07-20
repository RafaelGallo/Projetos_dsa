# Modelo Machine learning - Modelagem Preditiva em IoT - Previsão de Uso de Energia.

# Este projeto de IoT tem como objetivo a criação de modelos preditivos para a previsão de consumo de energia de eletrodomésticos. Os dados utilizados
# incluem medições de sensores de temperatura e umidade de uma rede sem fio, previsão do tempo de uma estação de um aeroporto e uso de energia utilizada por
# luminárias. Nesse projeto de aprendizado de máquina você deve realizar a filtragem de
# dados para remover parâmetros não-preditivos e selecionar os melhores recursos
# (melhores features) para previsão. O conjunto de dados foi coletado por um
# período de 10 minutos por cerca de 5 meses. As condições de temperatura e
# umidade da casa foram monitoradas com uma rede de sensores sem fio ZigBee.

# Cada nó sem fio transmitia as condições de temperatura e umidade em torno
# de 3 min. Em seguida, a média dos dados foi calculada para períodos de 10 minutos.
# Os dados de energia foram registrados a cada 10 minutos com medidores de
# energia de barramento m. O tempo da estação meteorológica mais próxima do
# aeroporto (Aeroporto de Chievres, Bélgica) foi baixado de um conjunto de dados
# públicos do Reliable Prognosis (rp5.ru) e mesclado com os conjuntos de dados
# experimentais usando a coluna de data e hora. Duas variáveis aleatórias foram
# incluídas no conjunto de dados para testar os modelos de regressão e filtrar os
# atributos não preditivos (parâmetros). Seu trabalho agora é construir um modelo preditivo que possa prever o
# consumo de energia com base nos dados de sensores IoT coletados.

# Objetivo
# Recomendamos usar RandomForest para a seleção de atributos e SVM, Regressão, Logística Multilinear ou Gradient Boosting para o modelo preditivo.
# Recomendamos ainda o uso da linguagem R.

# Coluna de previsão e coluna "Appliances" e nosso alvo

### Dicionario dados
# Descricao das variáveis

# date: tempo de coleta dos dados pelos sensores (year-month-day hour:minute)
# Appliances: uso de energia (em W)
# lights: potencia de energia de eletrodomesticos na casa (em W)
# TXX: Temperatura em um lugar da casa (em Celsius)
# RH_XX: umidade em um lugar da casa (em %)
# T_out: temperatura externa (em Celsius) in Celsius
# Pressure: pressão externa (em mm Hg)
# RH_out: umidade externa (em %)
# Wind speed: velocidade do vento (em m/s)
# Visibility; visibilidade (em km)
# Tdewpoint: nao descobri o que significa mas acredito que dados de algum sensor
# rv1: variavel randomica adicional
# rv2, variavel randomica adicional
# WeekStatus: indica se é dia de semana ou final de semana (weekend ou weekday)
# Day_of_week: dia da semana
# NSM: medida do tempo em segundos

# Instalando pacotes
install.packages("Hmisc")
install.packages("corrgram")
install.packages("dplyr")

### Parte 1 - Carregando bibliotecas
library(dplyr)
library(Hmisc)
library(ggplot2)
library(PerformanceAnalytics)
library(corrgram)
library(zoo)
library(readr)
library(caret)
library(scales)


### Parte 2 - Carregando base dados

# Base treino
data_train <- read_csv("projeto8-training.csv")
head(data_train)
View(data_train)

# Base teste
data_test <- read_csv("projeto8-testing.csv")
head(data_test)
View(data_test)

### Parte 3 - Ajutando base dados
data <- rbind(data_train, data_test)

# Visualizando base dados nova
head(data)

# Visualizando nomes da coluna
names(data)

### Parte 4 - Engenharia de Atributos - Feature Engineering

## Transformação dados para data
data$date <- strptime(as.character(data$date),format="%Y-%m-%d %H:%M")
data$date <- as.POSIXct(data$date , tz="UTC")
data$day   <- as.integer(format(data$date, "%d"))
data$month <- as.factor(format(data$date, "%m"))
data$hour <- as.integer(format(data$date, "%H"))

# Transformação em dados númericas para variáveis categóricas
data$lights <- as.factor(data$lights)

### Parte 5 - Analise Exploratoria de Dados

# Valores ausentes
any(is.na(data))

# Dados estatisticos numéricas
describe(data)

## Análise Estatística dos Dados

# O presente texto traz observações estatísticas relacionadas a diferentes variáveis, como temperaturas internas e externas, umidade interna e externa, consumo de energia, luzes e status da semana.
# Vamos detalhar cada um desses pontos:

# Temperaturas

# Temperaturas internas variaram entre 14.89°C a 29.95°C.
# Temperaturas externas (T6 e T_out) variaram entre -6.06°C a 28.29°C.

# Umidade
# A umidade interna variou entre 20.60% a 63.36%, exceto para o ponto RH_5, cujo valor não foi mencionado.
# A umidade externa (RH_6 e RH_out) apresentou variação entre 1% a 100%.

# Consumo de Energia
# A análise revelou que 75% do consumo de energia está abaixo de 100W, enquanto o maior consumo foi de 1080W, identificado como um outlier no dataset.

# Luzes
# Foram encontrados 15.252 valores iguais a zero (0) em um total de 19.735 observações.
# É necessário investigar se a presença desses valores nulos (zeros) possui significância para a performance do modelo em análise.

# WeekStatus (Status da Semana)
# Cerca de 72,3% das observações ocorreram durante a semana, enquanto 27,7% ocorreram nos finais de semana.

# Essas observações estatísticas fornecem uma visão geral dos dados coletados, mostrando algumas características importantes do conjunto de dados em questão.
# A partir dessas informações, é possível realizar análises mais detalhadas, investigar possíveis padrões e, se aplicável, preparar o conjunto de dados para o treinamento de um modelo ou para outras aplicações específicas. É importante lembrar que a análise estatística é uma etapa essencial
# para compreender os dados e obter insights relevantes para tomadas de decisão mais embasadas.

# Análise Estatística - Correlação
data_nub <- numeric.vars <- c('Appliances','T1','RH_1','T2',
                              'RH_2','T3','RH_3','T4','RH_4',
                              'T5','RH_5','T6','RH_6','T7',
                              'RH_7','T8','RH_8','T9','RH_9',
                              'T_out','Press_mm_hg','RH_out','Windspeed',
                              'Visibility','Tdewpoint',
                              'rv1','rv2','NSM')
data_corr <- cor(data[,data_nub])

# Visualizando correlação com "Spearman"
chart.Correlation(data_corr,
                  method="spearman",
                  histogram=TRUE,
                  pch=16)

# Visualizando correlação com dados númericos
data_corr <- corrgram(data_corr, order=TRUE,
                      lower.panel = panel.shade,
                      upper.panel = panel.pie,
                      text.panel = panel.txt)

# Observações da Correlação
# Neste texto, serão apresentadas algumas observações relevantes sobre as correlações existentes entre as variáveis do conjunto de dados, especialmente em relação ao atributo-alvo "Appliances":

# Temperaturas:
# Todas as características relacionadas às temperaturas apresentam uma correlação positiva com o atributo-alvo "Appliances". Isso significa que, à medida que as temperaturas aumentam, é provável que o consumo de energia dos eletrodomésticos também aumente, e vice-versa.

# Atributos do Tempo:
# Algumas características relacionadas ao tempo, como Visibility, Tdewpoint e Press_mm_hg, mostraram uma correlação baixa com o atributo-alvo "Appliances". Isso indica que essas variáveis têm uma influência limitada ou pouco significativa sobre o consumo de energia dos eletrodomésticos.

# Umidade:
# A análise revelou que as variáveis relacionadas à umidade não possuem correlação significante com o atributo-alvo "Appliances". Ou seja, o nível de umidade do ambiente não parece ter uma influência direta sobre o consumo de energia dos eletrodomésticos. Valores próximos a 0.9 são frequentemente considerados como um limiar para correlações significativas, e como não atingem esse valor, a umidade não é um fator determinante para o consumo de energia.

# Variáveis Randômicas:
# Foi constatado que as variáveis aleatórias não apresentam influência ou correlação significativa com o atributo-alvo "Appliances". Essas variáveis podem ser consideradas como ruído ou informações irrelevantes para a previsão do consumo de energia dos eletrodomésticos.
# Essas observações sobre a correlação entre as variáveis são fundamentais para a construção e interpretação de modelos preditivos ou análises mais complexas. Ao identificar quais características têm maior ou menor impacto no atributo-alvo, é possível direcionar esforços para aprimorar modelos ou entender melhor os fatores que influenciam o consumo de energia, auxiliando em tomadas de decisão mais informadas.

# Gráfico variavel target "Appliances"
ggplot(data, aes(x = Appliances)) +
  geom_histogram(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Variavel target - Appliances",
       x = "Categorias",
       y = "Contagem")

## Análise série temporal

# Visualizando o consumo de energia por dia x mes
ggplot(data)+
  geom_bar(aes(x=day, y=Appliances, color = "steelblue"), stat="identity")+
  scale_y_continuous(name="Consumo Energia")+
  facet_wrap(~month, scale="free")+
  theme_bw()

# Visualizando o consumo de energia por dia x semana e final de semana
ggplot(data)+
  geom_bar(aes(x=day, y=Appliances), stat="identity", color = "steelblue")+
  scale_y_continuous(name="Consumo Energia")+
  facet_wrap(~WeekStatus, scale="free")+
  theme_bw()

# Observações a partir dos gráficos
# 1: Com base nos gráficos fornecidos, podemos notar um pico de consumo de energia no mês de janeiro,
# seguido por alguns períodos com baixa frequência de consumo, especialmente no final de janeiro e início de abril.

# 2: É possível perceber que o consumo de energia nos meses de março, abril e maio é menor em comparação aos meses de janeiro e fevereiro.
# Essa redução pode ser atribuída a um período de férias ou devido ao verão, quando a demanda tende a diminuir.

# Parte 6 - Seleção de variaveis

# Parte 6.0 Normalização dados

scale.features <- function(data, variables){
  for (variable in variables){
    data[[variable]] <- scale(data[[variable]], center=T, scale=T)
  }
  return(data)
}

# Variaveis para normalização dados
norml_data <- numeric.vars <- c('T1','RH_1','T2','RH_2','T3','RH_3','T4','RH_4','T5','RH_5','T6','RH_6','T7','RH_7','T8','RH_8','T9','RH_9',
                                'T_out','Press_mm_hg','RH_out','Windspeed','Visibility','Tdewpoint','rv1','rv2','NSM')
# Normalização dados
data <- scale.features(data, norml_data)
data

# Transformando data index
rownames(data) <- data$date
data$date <- NULL

# Treino teste modelo
data_splits <- createDataPartition(data$Appliances,
                                   p=0.7,
                                   list = FALSE)

# Dados treino e teste
train <- data[data_splits,]
test <- data[-data_splits,]

# Verificando dados treino e teste
nrow(train)
nrow(test)

# Parte 6.1 - Limpeza de dados

train <- na.omit(train)

mean_value <- mean(train$Appliances, na.rm = TRUE)
train$Appliances[is.na(train$Appliances)] <- mean_value

# Parte 7 - Modelo machine learning
# Modelo 01 - Regressão Logística Multinomial
library(nnet)

# Treine o modelo de Regressão Logística Multinomial
modelo_logistica <- multinom(Appliances ~ .,
                             data = train,
                             MaxNWts = 1000)

# Faça previsões no conjunto de teste
previsoes_logistica <- predict(modelo_logistica, newdata = dados_teste)
previsoes_logistica

# Avalie a precisão do modelo
precisao <- mean(previsoes_logistica == dados_teste$Classe)
cat("Precisão do modelo de Regressão Logística Multinomial:", precisao, "\n")

# Treine o modelo SVM para classificação
# Modelo 02 - SVM
library(e1071)
modelo_svm <- svm(Appliances ~ .,
                  data = train)

modelo_svm

# Sumario do modelo
summary(modelo_svm)

# Faça previsões no conjunto de teste
previsoes_svm <- predict(modelo_svm, newdata = test)
previsoes_svm

## Modelo 03 - Gradient Boosting - XGBoost
library(xgboost)

# Treine o modelo Gradient Boosting usando xgboost
modelo_xgboost <- xgboost(data = as.matrix(train[, -3]),
                          label = train$Appliances,
                          nrounds = 100,
                          objective = "binary:logistic")
modelo_xgboost

# Faça previsões no conjunto de teste
previsoes_xgboost <- predict(modelo_xgboost, as.matrix(dados_teste[, -3]))
previsoes_xgboost

# Converta as previsões em classes (0 e 1)
previsoes_xgboost <- ifelse(previsoes_xgboost >= 0.5, 1, 0)
previsoes_xgboost

# Avalie a precisão do modelo Gradient Boosting
precisao <- mean(previsoes_xgboost == dados_teste$Classe)
cat("Precisão do modelo Gradient Boosting:", precisao, "\n")

# Modelo 04 - Random forest
library(randomForest)

# Treine o modelo Random Forest
modelo_rf <- randomForest(Classe ~ .,
                          data = dados_treino,
                          ntree = 100)
modelo_rf

# Sumario
summary(modelo_rf)

# Plot do modelo Random Forest
plot(modelo_rf)

# Faça previsões no conjunto de teste
previsoes_rf <- predict(modelo_rf,
                        newdata = dados_teste)
previsoes_rf

# Avalie a precisão do modelo Random Forest
precisao <- mean(previsoes_rf == dados_teste$Classe)
cat("Precisão do modelo Random Forest:", precisao, "\n")
