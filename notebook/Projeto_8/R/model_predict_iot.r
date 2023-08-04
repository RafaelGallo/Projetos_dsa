# Etapa 1: Carregar os dados

library(readr)
data_train <- read_csv("projeto8-training.csv")
head(data_train)
View(data_train)

data_test <- read_csv("projeto8-testing.csv")
head(data_test)
View(data_test)

# Etapa 2: Pré-processamento dos dados (por exemplo, supondo que você tem a coluna "consumo" que é a variável alvo)
# Verificando valores ausentes
sum(is.na(data_train))

# Normalização/escalonamento dos atributos (caso necessário)
# Exemplo usando a função scale():
data_train~Appliances <- scale(data_train ~ Appliances)

# Dividir os dados em conjunto de treinamento e teste (por exemplo, usando 70% dos dados para treinamento e 30% para teste)
library(caTools)
set.seed(42) # Define uma semente para reproducibilidade dos resultados
split <- sample.split(data_train$Appliances, SplitRatio = 0.7)
dados_treinamento <- subset(data_train, split == TRUE)
dados_teste <- subset(data_train, split == FALSE)

# Visualizando dados treino
dim(dados_treinamento)

# Visualizando dados teste
dim(dados_teste)

# Etapa 3: Seleção de atributos com RandomForest
library(randomForest)

# Modelo Random forest
modelo_rf <- randomForest(Appliances ~ .,
                          data = dados_treinamento,
                          ntree = 100)

# Visualizando modelo RF
summary(modelo_rf)

# Ver os atributos mais importantes
importance(modelo_rf)

# Etapa 5: Avaliação do modelo
# Fazendo previsões com o modelo
previsoes <- predict(modelo_rf,
                     newdata = dados_teste,
                     n.trees = 100)
previsoes

# Calculando o RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((previsoes - dados_teste$Appliances)^2))
rmse

# Calculando o MAE (Mean Absolute Error)
mae <- mean(abs(previsoes - dados_teste$Appliances))
mae

# Calculando o R²
ssr <- sum((previsoes - mean(dados_teste$Appliances))^2)
sst <- sum((dados_teste$Appliances - mean(dados_teste$Appliances))^2)
r2 <- ssr/sst
r2

# Modelo 02 - SVM
# Treinar o modelo SVM

library(readr)
library(e1071)
library(caret)

# Modelo SVM
modelo_svm <- svm(Appliances ~ .,
                  data = dados_treinamento,
                  kernel = "linear")

# Visualizando modelo SVM
modelo_svm

# Obter os atributos selecionados pelo modelo SVM
atributos_selecionados <- colnames(dados_treinamento)[modelo_svm$index]

# Filtrar os atributos selecionados nos conjuntos de treinamento e teste
dados_treino_filtrados <- dados_treinamento[, c("Appliances", atributos_selecionados)]
dados_teste_filtrados <- dados_teste[, c("consumo_energia", atributos_selecionados)]

# Treinar o modelo de Regressão Logística Multilinear
modelo_reg_multilinear <- lm(Appliances ~ .,
                             data = dados_treinamento)

# Fazer previsões no conjunto de teste
previsoes <- predict(modelo_reg_multilinear,
                     newdata = dados_teste,
                     n.trees = 100)
previsoes

# Calcular o RMSE
rmse <- sqrt(mean((previsoes - dados_teste$Appliances - previsoes)^2))
rmse

# Calcular o R²
r2 <- cor(dados_teste$Appliances, previsoes)^2
r2

# Exibir os resultados
print(paste("RMSE:", rmse))
print(paste("R²:", r2))

### Modelo 03 - Regressão Logística Multilinear

# Filtrar os atributos selecionados pelo SVM nos conjuntos de treinamento e teste
dados_treino_filtrados <- dados_treino[, c("consumo_energia", atributos_selecionados)]
dados_teste_filtrados <- dados_teste[, c("consumo_energia", atributos_selecionados)]

# Treinar o modelo de Regressão Linear Multivariada
modelo_reg_multivariada <- lm(consumo_energia ~ ., data = dados_treino_filtrados)
modelo_reg_multivariada

# Fazer previsões no conjunto de teste
previsoes <- predict(modelo_reg_multivariada, newdata = dados_teste_filtrados)
previsoes

# Calcular o RMSE
rmse <- sqrt(mean((dados_teste_filtrados$consumo_energia - previsoes)^2))
rmse

# Calcular o R²
r2 <- summary(modelo_reg_multivariada)$r.squared
r2

# Exibir os resultados
print(paste("RMSE:", rmse))
print(paste("R²:", r2))




### Segundo modelo - Forecast

install.packages("forecast")
library(forecast)


# Exemplo de leitura de dados CSV, ajuste conforme o formato do seu arquivo
dados <- read_csv("projeto8-training.csv")
dados

# Converta a coluna de data/hora para o tipo apropriado
dados$date <- as.POSIXct(dados$date)

# Supondo que você já carregou os pacotes e leu os dados conforme mencionado anteriormente...

# Crie a série temporal (defina a frequência apropriada, conforme a periodicidade dos dados)
serie_temporal <- ts(dados$lights, frequency = 24) # Supondo que a frequência dos dados seja diária (24 observações por dia)

# Divida a série temporal em conjunto de treinamento e teste
tamanho_treinamento <- floor(0.7 * length(serie_temporal))
conjunto_treinamento <- serie_temporal[1:tamanho_treinamento]
conjunto_teste <- serie_temporal[(tamanho_treinamento + 1):length(serie_temporal)]

# Instale e carregue o pacote forecast
install.packages("forecast")
library(forecast)

# Modelo ARIMA
modelo_arima <- auto.arima(conjunto_treinamento)
modelo_arima

# Summary modelo ARIMA
summary(modelo_arima)

# Previsoes
previsoes <- forecast(modelo_arima, h = length(conjunto_teste))
previsoes

# Gráfico previsão
library(ggplot2)

# Crie um gráfico de previsão com ggplot2
grafico_previsao <- autoplot(previsoes) +
  labs(title = "Previsão de Consumo de Energia",
       x = "Data",
       y = "Consumo") +
  theme_minimal()

# Exiba o gráfico
print(grafico_previsao)

# RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((previsoes$mean - conjunto_teste)^2))
rmse

## Modelo SARIMA
# Ajuste o modelo SARIMA usando a função auto.arima
modelo_sarima <- auto.arima(conjunto_treinamento, seasonal = TRUE)
modelo_sarima

# Summario
summary(modelo_sarima)

# Supondo que você já ajustou o modelo SARIMA e fez as previsões conforme mencionado anteriormente...

# Extrair os valores observados do conjunto de teste
valores_observados <- as.vector(conjunto_teste)
valores_observados

# Extrair as previsões do modelo SARIMA
valores_previsoes <- as.vector(previsoes_sarima$mean)
valores_previsoes

# Calcular o R²
ssr <- sum((valores_previsoes - mean(valores_observados))^2)
sst <- sum((valores_observados - mean(valores_observados))^2)
r2 <- ssr / sst

# Exibir o valor de R²
print(paste("R² do modelo SARIMA:", round(r2, 4)))


# Faça as previsões usando o modelo ajustado
previsoes_sarima <- forecast(modelo_sarima, h = length(conjunto_teste))
previsoes_sarima

# Crie um gráfico de previsão com ggplot2
grafico_previsao_sarima <- autoplot(previsoes_sarima) +
  labs(title = "Previsão de Consumo de Energia com SARIMA",
       x = "Data",
       y = "Consumo") +
  theme_minimal()

# Exiba o gráfico
print(grafico_previsao_sarima)

# RMSE SARIMA
rmse2 <- sqrt(mean((previsoes_sarima$mean - conjunto_teste)^2))
rmse2

### Terceiro modelo forecast - Prophet

# Baixando biblioteca prophet
install.packages("prophet")

# Carregando biblioteca
library(prophet)

dados

# Prepare os dados no formato de dataframe esperado pelo prophet
dados_prophet <- data.frame(ds = dados$lights, y = dados$Appliances)
dados_prophet

# Modelo série temporal
# Ajuste o modelo Prophet
modelo_prophet <- prophet(dados_prophet)
modelo_prophet

# Criar um dataframe com as datas para fazer as previsões
periodo_previsao <- make_future_dataframe(modelo_prophet, periods = length(conjunto_teste))
periodo_previsao

# Faça as previsões
previsoes_prophet <- predict(modelo_prophet, periodo_previsao)
previsoes_prophet

# Gráfico das previsões
plot(modelo_prophet, previsoes_prophet)

# Gráfico dos componentes do modelo (tendência, sazonalidade, feriados, etc.)
prophet_plot_components(modelo_prophet, previsoes_prophet)


