### Formação Cientista de Dados - Projeto com Feedback 8
# Modelagem Preditiva em IoT - Previsão de Uso de Energia

# Este projeto de IoT tem como objetivo a criação de modelos preditivos para
# a previsão de consumo de energia de eletrodomésticos. Os dados utilizados
# incluem medições de sensores de temperatura e umidade de uma rede sem fio,previsão do tempo de uma estação de um aeroporto e uso de energia utilizada por luminárias

## Nesse projeto de aprendizado de máquina você deve realizar a filtragem de
# dados para remover parâmetros não-preditivos e selecionar os melhores recursos
# (melhores features) para previsão. O conjunto de dados foi coletado por um
# período de 10 minutos por cerca de 5 meses. As condições de temperatura e
# umidade da casa foram monitoradas com uma rede de sensores sem fio ZigBee.

# Cada nó sem fio transmitia as condições de temperatura e umidade em torno
# de 3 min. Em seguida, a média dos dados foi calculada para períodos de 10 minutos.
# Os dados de energia foram registrados a cada 10 minutos com medidores de energia de barramento m. O tempo da estação meteorológica mais próxima do
# aeroporto (Aeroporto de Chievres, Bélgica) foi baixado de um conjunto de dados públicos do Reliable Prognosis (rp5.ru) e mesclado com os conjuntos de dados
# experimentais usando a coluna de data e hora. Duas variáveis aleatórias foram
# incluídas no conjunto de dados para testar os modelos de regressão e filtrar os
# atributos não preditivos (parâmetros).

## Objetivo
# Seu trabalho agora é construir um modelo preditivo que possa prever o
# consumo de energia com base nos dados de sensores IoT coletados.
# Recomendamos usar Random Forest para a seleção de atributos e SVM, Regressão Logística Multilinear ou
# Gradient Boosting para o modelo preditivo.

# Recomendamos ainda o uso da linguagem R.


# Dicionario dados

# date time = ano-mês-dia hora:minuto:segundo
# Appliances = uso de energia em Wh (variável alvo para previsão)
# lights = uso de energia de luminárias na casa em Wh
# T1 = Temperatura na área da cozinha, em Celsius
# RH_1 = Umidade na área da cozinha, em %
# T2 = Temperatura na zona da sala, em Celsius
# RH_2 = Umidade na área da sala, em %
# T3 = Temperatura na zona da lavandaria
# RH_3 = Umidade na área da lavanderia, em %
# T4 = Temperatura em sala de escritório, em Celsius
# RH_4 = Umidade na sala do escritório, em %
# T5 = Temperatura no banheiro, em Celsius
# RH_5 = Umidade no banheiro, em %
# T6 = Temperatura exterior ao edifício (lado norte), em Celsius
# RH_6 = Umidade externa ao edifício (lado norte), em %
# T7 = Temperatura na sala de passar roupa, em Celsius
# RH_7 = Umidade na sala de passar roupa, em %
# T8 = Temperatura no quarto do adolescente 2, em Celsius
# RH_8 = Umidade no quarto do adolescente 2, em %
# T9 = Temperatura no quarto dos pais, em Celsius
# RH_9 = Umidade no quarto dos pais, em %
# TO = Temperatura externa (da estação meteorológica de Chievres), em Celsius
# Pressure = (da estação meteorológica de Chievres), em mm Hg
# RH_out = umidade externa (da estação meteorológica de Chievres), em%
# Wind speed = Velocidade do vento (da estação meteorológica de Chievres), em m/s
# Visibilidade = (da estação meteorológica de Chievres), em km
# Tdewpoint = (da estação meteorológica de Chievres), °C
# rv1 = variável aleatória 1, adimensional
# rv2 = variável aleatória 2, adimensional

## Importando as bibliotecas
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(lubridate)
library(lubridate)
library(caret)
library(randomForest)
library(xgboost)
library(e1071)
library(class)

## Dataset
data_train <- read_csv("projeto8-training.csv")
data_test <- read_csv("projeto8-testing.csv")

# Visualizandos dados
head(data_train)
head(data_test)

tail(data_train)

# View dados
View(data_train)
View(data_test)

# Verifique as primeiras linhas dos dados
head(data_train)

# Verifique a estrutura dos dados
str(data_train)

# Verifique se há valores ausentes
sum(is.na(data_train))

## Análise dados

# Resumo estatístico das variáveis numéricas:
summary(data_train)

# Gráfico váriavel alvo - rv1
ggplot(data_train, aes(x = rv1)) +
  geom_histogram(binwidth = 1, fill = "red", alpha = 0.5) +
  labs(x = "rv1", y = "Contagem", title = "Histograma: Distribuição de rv1") +
  theme_minimal()

# Gráfico váriavel alvo - rv2
ggplot(data_train, aes(x = rv2)) +
  geom_histogram(binwidth = 1, fill = "green", alpha = 0.5) +
  labs(x = "rv2", y = "Contagem", title = "Histograma: Distribuição de rv2") +
  theme_minimal()

# Gráfico Appliances
ggplot(data_train, aes(x = Appliances)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Histograma de Appliances", x = "Valor", y = "Frequência")

# Gráfico Scatterplot Appliances
ggplot(data_train, aes(x = T_out, y = Appliances)) +
  geom_point() +
  labs(title = "Scatterplot de Appliances", x = "T_out", y = "Appliances")

# Gráfico scatterplot com regressão
ggplot(data_train, aes(x = T1, y = Appliances)) +
  geom_point(color = "blue") +                    # Adicione os pontos
  labs(title = "Gráfico de Regressão Linear",
       x = "T_out",
       y = "Appliances")

# Crie um gráfico de barras
ggplot(data_train, aes(x = WeekStatus, y = Appliances, fill = WeekStatus)) +
  geom_bar(stat = "identity") +
  labs(title = "Eletrodomésticos por Status da Semana",
       x = "WeekStatus",
       y = "Appliances")

# Gráfico (boxplot)
ggplot(data_train, aes(y = Appliances)) +
  geom_boxplot() +
  ylab("Consumo de Energia (Appliances)") +
  xlab("Total") +
  ggtitle("Gráfico váriavel alvo com outliers")

# Gráfico boxplot
ggplot(data_train, aes(x = WeekStatus , y = Appliances)) +
  geom_boxplot() +
  labs(title = "Boxplot Eletrodomésticas por Status da Semana", x = "WeekStatus", y = "Appliances")

# Gráfico boxplot
ggplot(data_train, aes(x = Day_of_week, y = Appliances)) +
  geom_boxplot() +
  labs(title = "Boxplot Eletrodomésticas por Dia da semana", x = "WeekStatus", y = "Appliances")

# Crie um gráfico de barras
ggplot(data_train, aes(x = Day_of_week, y = Appliances, fill = Day_of_week)) +
  geom_bar(stat = "identity") +
  labs(title = "Gráfico barras eletrodomésticos por Dia da Semana",
       x = "Dia da Semana",
       y = "Appliances")

# Converta a coluna "date" em um objeto de data
# Supondo que a coluna "date" esteja no formato correto
data_train$date <- ymd_hms(data_train$date)

# Crie um gráfico de série temporal
ggplot(data_train, aes(x = date, y = Appliances, color = WeekStatus)) +
  geom_line() +
  labs(title = "Gráfico barras eletrodomésticos por Dia da Semana",
       x = "Data",
       y = "Appliances")

# Gráfico eletrodomésticos por Dia da Semana
ggplot(data_train, aes(x = date, y = T1, color = WeekStatus)) +
  geom_line() +
  labs(title = "Gráfico barras eletrodomésticos por Dia da Semana",
       x = "Data",
       y = "Appliances")

# Gráfico de séries temporais para T1 e RH_1
ggplot(data_train, aes(x = date)) +
  geom_line(aes(y = T1, color = "T1")) +
  geom_line(aes(y = RH_1, color = "RH_1")) +
  labs(title = "Séries Temporais de T1 e RH_1",
       x = "Data",
       y = "Valor") +
  scale_color_manual(values = c("T1" = "blue", "RH_1" = "red")) +
  theme_minimal()


# Crie a matriz de correlação (substitua as variáveis pelas desejadas)
correlation_matrix <- cor(data_train[, c("T_out", "Appliances", "RH_1")])
corrplot(correlation_matrix, method = "circle", type = "full", tl.cex = 0.7, tl.col = "black") # Plot da matriz de correlação

## Verificação de outliers
# Trate os valores ausentes, por exemplo, preenchendo-os com a média
data_train$Appliances[is.na(data_train$Appliances)] <- mean(data_train$Appliances, na.rm = TRUE)

# 1) Verifique a presença de outliers (boxplot)
ggplot(data_train, aes(y = Appliances)) +
  geom_boxplot() +
  ylab("Consumo de Energia (Appliances)") +
  xlab("Total") +
  ggtitle("Gráfico váriavel alvao com outliers")

# Trate os outliers, se necessário
Q1 <- quantile(data_train$Appliances, 0.25)
Q3 <- quantile(data_train$Appliances, 0.75)
IQR <- Q3 - Q1
limite_superior <- Q3 + 1.5 * IQR

# Verificação outliers
out <- data_train$Appliances[data_train$Appliances > limite_superior] <- limite_superior
out

## Limpeza de dados
colunas_com_valores_ausentes <- colnames(data_train)[apply(data_train, 2, function(x) any(is.na(x)))]
data_sem_valores_ausentes <- na.omit(data_train)
data_train$Appliances[is.na(data_train$Appliances)] <- mean(data_train$Appliances, na.rm = TRUE)
colunas_com_valores_ausentes <- colnames(data_train)[apply(data_train, 2, function(x) any(is.na(x)))]

# Gráfico sem outliers
ggplot(data_train, aes(y = Appliances)) +
  geom_boxplot() +
  ylab("Consumo de Energia (Appliances)") +
  xlab("Total") +
  ggtitle("Gráfico váriavel alvo sem outliers")

# 2) Verifique a presença de outliers (boxplot)
boxplot(data_train$Appliances)
title("Gráfico váriavel alvo sem outlier")

## Treino e teste

#OBS: Colunas alvos são RV1 RV2


# Divisão dos dados em conjuntos de treinamento e teste (por exemplo, 80% treinamento, 20% teste)
set.seed(123)  # Para garantir a reprodutibilidade
sample_indices <- sample(nrow(data_train), 0.8 * nrow(data_train))
x_train <- data_train[sample_indices, ]
y_test <- data_train[-sample_indices, ]

# Visualizando os dados treino
dim(x_train)

# Visualizando os dados teste
dim(y_test)

######### Modelo machine learning #########

# Modelo de regressão linear múltipla
modelo_regressao <- lm(Appliances ~ rv1 + rv2, data = x_train)
modelo_regressao

# Visualize os coeficientes do modelo
summary(modelo_regressao)

# Previsão
predictions_modelo_regressao <- predict(modelo_regressao, newdata = y_test)
predictions_modelo_regressao

# SVM
svm_model <- svm(Appliances ~ ., data = x_train, kernel = "radial")
svm_model

# Visualize os coeficientes do modelo
summary(svm_model)

# Previsão
predictions_svm <- predict(svm_model, newdata = y_test)
predictions_svm

# Regressão Logística Multilinear
lm_model <- lm(Appliances ~ ., data = x_train)
lm_model

# Visualize os coeficientes do modelo
summary(lm_model)

# Previsão
predictions_lm <- predict(lm_model, newdata = y_test)
predictions_lm
View(predictions_lm)


######### Segunda parte 2 - Análise série temporal #########
#########

## Gráfico de série temporal

# Gráfico de série temporal para a variável T1
ggplot(data_train, aes(x = NSM, y = T1)) +
  geom_line() +
  labs(x = "Tempo", y = "T1", title = "Análise de Série Temporal para T1") +
  theme_minimal()

ggplot(data_train, aes(x = date, y = T1, color = WeekStatus)) +
  geom_line() +
  labs(title = "Gráfico de Série Temporal de Appliances por WeekStatus",
       x = "Data",
       y = "Appliances")

ggplot(data_train, aes(x = date, y = T1, color = Day_of_week)) +
  geom_line() +
  labs(title = "Gráfico de Série Temporal de Appliances por WeekStatus",
       x = "Data",
       y = "Appliances")

ggplot(data_train, aes(x = date, y = Tdewpoint, color = Day_of_week)) +
  geom_line() +
  labs(title = "Gráfico de Série Temporal de Appliances por WeekStatus",
       x = "Data",
       y = "Appliances")

# Crie um gráfico de séries temporais para T1 e RH_1
ggplot(data_train, aes(x = date)) +
  geom_line(aes(y = T1, color = "T1")) +
  geom_line(aes(y = RH_1, color = "RH_1")) +
  labs(title = "Séries Temporais de T1 e RH_1",
       x = "Data",
       y = "Valor") +
  scale_color_manual(values = c("T1" = "blue", "RH_1" = "black")) +
  theme_minimal()

# Gráfico de série temporal para Wind Speed (Velocidade do Vento) e T1 (Temperatura)
ggplot(data_train, aes(x = date, y = Windspeed, color = Day_of_week)) +
  geom_line() +
  labs(title = "Gráfico de Série Temporal de Appliances por WeekStatus",
       x = "Data",
       y = "Appliances")

######### Terceira parte - Modelo série temporal ARIMA #########
#########

# Converta para um objeto xts (séries temporais)
library(forecast)

# Converter a coluna "date" para o formato correto
data_train$date <- as.Date(data_train$date)

# série temporal a partir dos dados
ts_data <- ts(data_train$rv1, frequency = 20)  # Suponhamos que seja uma série semanal (frequency = 7)

## Modelo ARIMA
arima_model <- auto.arima(ts_data)
arima_model

# Summary modelo
summary(arima_model)


# Previsões
forecast_values <- forecast(arima_model, h = 20)  # Substitua 10 pelo número de períodos que deseja prever
forecast_values


######### Quarta parte - Modelo série temporal SARIMA #########
#########

# Converter a coluna "date" para o formato correto
ts_data <- ts(data_train$Appliances, frequency = 7)  # Suponhamos que seja uma série semanal (frequency = 7)

# Modelo SARIMA
sarima_model <- auto.arima(ts_data, seasonal = TRUE)
sarima_model

# Summary dados
summary(sarima_model)

# Previsão dados
forecast_values <- forecast(sarima_model, h = 120)  # Substitua 10 pelo número de períodos que deseja prever
forecast_values


######### Conclusão


######### Referência
