# Projeto com Feedback 2 02 - Machine Learning na Segurança do Trabalho Prevendo a Eficiência de Extintores de Incêndio

# O teste hidrostático extintor é um procedimento estabelecido pelas normas da ABNT NBR 12962/2016, que determinam que todos os extintores devem ser testados a cada cinco anos,
# com a finalidade de identificar eventuais vazamentos, além de também verificar a resistência do material do extintor.
# Com isso, o teste hidrostático extintor pode ser realizado em baixa e alta pressão, de acordo com estas normas em questão.
# O procedimento é realizado por profissionais técnicos da área e com a utilização de aparelhos específicos e apropriados para o teste, visto que eles devem fornecer resultados com exatidão.
# Seria possível usar Machine Learning para prever o funcionamento de um extintor de incêndio com base em simulações feitas em computador e assim incluir uma
# camada adicional de segurança nas operações de uma empresa?
# Esse é o objetivo do Projeto com Feedback 2.
# Usando dados reais disponíveis publicamente, seu trabalho é desenvolver um modelo de Machine Learning capaz de prever a eficiência de extintores de incêndio.

# Base dados
# No link abaixo você encontra os dados necessários para o seu trabalho:
# https://www.muratkoklu.com/datasets/vtdhnd07.php

# O conjunto de dados foi obtido como resultado dos testes de extinção de quatro chamas de combustíveis diferentes com um sistema de extinção de ondas sonoras.
# O sistema de extinção de incêndio por ondas sonoras consiste em 4 subwoofers com uma potência total de 4.000 Watts.
# Existem dois amplificadores que permitem que o som chegue a esses subwoofers como amplificado.
# A fonte de alimentação que alimenta o sistema e o circuito do filtro garantindo que as frequências de som sejam transmitidas adequadamente para o sistema está localizada dentro da unidade de controle.
# Enquanto o computador é usado como fonte de frequência, o anemômetro foi usado para medir o fluxo de ar resultante das ondas sonoras durante a fase de extinção da chama e um decibelímetro para medir a intensidade do som.
# Um termômetro infravermelho foi utilizado para medir a temperatura da chama e da lata de combustível, e uma câmera é instalada para detectar o tempo de extinção da chama.
# Um total de 17.442 testes foram realizados com esta configuração experimental.
# Os experimentos foram planejados da seguinte forma:

# Problema négocio
# Três diferentes combustíveis líquidos e combustível GLP foram usados para criar a chama.
# 5 tamanhos diferentes de latas de combustível líquido foram usados para atingir diferentes tamanhos de chamas.
# O ajuste de meio e cheio de gás foi usado para combustível GLP.
# Durante a realização de cada experimento, o recipiente de combustível, a 10 cm de distância, foi movido para frente até 190 cm, aumentando a distância em 10 cm a cada vez.
# Junto com o recipiente de combustível, o anemômetro e o decibelímetro foram movidos para frente nas mesmas dimensões.
# Experimentos de extinção de incêndio foram conduzidos com 54 ondas sonoras de frequências diferentes em cada distância e tamanho de chama.

## Objetivo
# Ao longo dos experimentos de extinção de chama, os dados obtidos de cada dispositivo de medição foram registrados e um conjunto de dados foi criado.
# O conjunto de dados inclui as características do tamanho do recipiente de combustível representando o tamanho da chama, tipo de combustível, frequência, decibéis, distância,
# fluxo de ar e extinção da chama. Assim, 6 recursos de entrada e 1 recurso de saída serão usados no modelo que você vai construir.

# A coluna de status (extinção de chama ou não extinção da chama) pode ser prevista usando os seis recursos de entrada no conjunto de dados.
# Os recursos de status e combustível são categóricos, enquanto outros recursos são numéricos.
# Seu trabalho é construir um modelo de Machine Learning capaz de prever, com base em novos dados, se a chama será extinta ou não ao usar um extintor de incêndio.

## Descrição projeto

# Definição do projeto: Nesse projeto, serão abordados diversos tópicos fundamentais para a realização de uma análise de dados robusta e a construção de modelos de machine learning eficazes.
# A seguir, apresento os detalhes das etapas que serão desenvolvidas.

# A) Limpeza dos dados: O primeiro passo é realizar a limpeza dos dados, que consiste em identificar e corrigir possíveis erros, valores ausentes ou inconsistentes no conjunto de dados.
# Essa etapa é crucial para garantir a qualidade dos resultados finais.

# B) Verificação de outliers: Em seguida, faremos uma análise cuidadosa para identificar e lidar com possíveis outliers (valores discrepantes).
# A presença de outliers pode afetar significativamente os resultados dos modelos, por isso é importante tratá-los adequadamente.

# C) Análise exploratória dos dados: Antes de construir os modelos de machine learning, realizaremos uma análise exploratória dos dados.
# Isso nos ajudará a compreender melhor as relações entre as variáveis, identificar padrões e insights relevantes para o projeto.

# D) Construção dos modelos de machine learning: Essa é uma das etapas centrais do projeto, onde iremos implementar seis algoritmos de machine learning diferentes.
# Os modelos que serão construídos são: Naive Bayes, Regressão Logística, Random Forest, KNN (K-Nearest Neighbors), SVM (Support Vector Machine) e Decision Tree.

# E) Comparação dos modelos: Após construir os modelos, realizaremos uma análise comparativa para determinar quais algoritmos apresentam o melhor desempenho em relação aos nossos critérios de avaliação.
# Essa avaliação nos permitirá selecionar os melhores modelos para prosseguir no projeto.

# F) Definição do modelo final: Com base nos resultados da etapa anterior, escolheremos o modelo (ou modelos) que melhor se adequam ao problema em questão.
# A seleção do modelo final é crucial para garantir uma previsão precisa e confiável.

# G) Fazer previsões com os dados treinados: Na última etapa do projeto, utilizaremos os dados previamente treinados pelo modelo selecionado para fazer previsões em novos conjuntos de dados.
# Essa fase é a aplicação prática dos resultados obtidos e demonstrará a eficácia do modelo escolhido.

# Por meio deste projeto abrangente, esperamos alcançar uma análise de dados mais confiável e obter resultados precisos por meio dos modelos de machine learning desenvolvidos.
# O processo de limpeza, análise e seleção dos melhores algoritmos é fundamental para tomar decisões informadas e auxiliar em problemas complexos que envolvam análise e previsão de dados.

install.packages("pROC")
install.packages("randomForest")
install.packages("randomForestExplainer")
install.packages("rpart.plot")

# Importando bibliotecas
library(pROC)
library(dplyr)
library(ggplot2)
library(readxl)
library(corrplot)
library(caret)
library(e1071)
library(randomForest)
library(xgboost)
library(class)
library(rpart)
library(rpart.plot)

# Base dados
data <- read_excel("Acoustic_Extinguisher_Fire_Dataset.xlsx")
data

# Visualizando os 5 primeiros dados
head(data)

# Visualizando os 5 últimos dados
tail(data)

# Linhas e colunas
dim(data)

# Linhas
nrow(data)

str(data)

summary(data)

## Limpeza dados

# Valores ausentes
sum(is.na(data))

# Excluir linhas com valores ausentes
data <- data[complete.cases(data),]
data

# Remover duplicatas
sum(duplicated(data))

# Remover linhas duplicadas
data <- unique(data)

## Análise exploratória

# Gráfico Combustíveis
ggplot(data, aes(x = FUEL)) +
  xlab("Combustíveis") +
  ylab("Total") +
  ggtitle("Combustíveis ")+
  geom_bar()

# Gráfico Váriavel alvo
ggplot(data, aes(x = STATUS)) +
  xlab("Status - Variavel alvo") +
  ylab("Total") +
  ggtitle("Variavel alvo")+
  geom_bar()

# Gráfico barras - Frêquencia por combustíveis
ggplot(data, aes(x = FUEL, y = STATUS)) +
  geom_bar(stat = "identity") +
  xlab("Combustíveis") +
  ylab("Total") +
  ggtitle("Frêquencia por combustíveis") +
  theme_minimal()

# Gráfico de histrograma - Frêquencia por váriavel alvo
ggplot(data, aes(x = FREQUENCY, y = STATUS)) +
  geom_histogram(stat = "identity") +
  xlab("Valores") +
  ylab("Total") +
  ggtitle("Total fequencia") +
  theme_minimal()

# Gráfico de boxplot - verificação tem outliers
ggplot(data, aes(x = FUEL, y = FREQUENCY)) +
  geom_boxplot() +
  xlab("Combustíveis") +
  ylab("Total") +
  ggtitle("Gráfico Boxplot verificação tem outliers") +
  theme_minimal()

# Criar o gráfico de histograma - Frquência por fluxo do ar
ggplot(data, aes(x = AIRFLOW)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  xlab("Valores") +
  ylab("AIRFLOW") +
  ggtitle("Frquência por fluxo do ar") +
  theme_minimal()

# Criar o gráfico de barras com legenda
ggplot(data, aes(x = FUEL, y = AIRFLOW, fill = STATUS)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Combustíveis") +
  ylab("Total") +
  ggtitle("Gráfico - Combustíveis por Frquência por fluxo do ar") +
  theme_minimal() +
  labs(fill = "Grupos")

# Gráfico de Dispersão - Frquência por fluxo do ar por status
ggplot(data, aes(x = FREQUENCY, y = AIRFLOW, color = STATUS)) +
  geom_point() +
  xlab("Frquência") +
  ylab("Total") +
  ggtitle("Gráfico de Dispersão - Frquência por fluxo do ar") +
  theme_minimal() +
  labs(color = "Total")


# Cálculo de correlação
corr <- data[, c("SIZE", "DISTANCE", "DESIBEL", "AIRFLOW", "FREQUENCY")]
corr <- cor(corr)
corrplot(corr, method = "circle", type = "upper", tl.cex = 0.7)


## Pré-processamento

# Divida os dados em treinamento e teste (80% treinamento, 20% teste)
set.seed(42)
x <- createDataPartition(data$STATUS, p = 0.7, list = FALSE)
train_x <- data[x, ]
test_y <- data[-x, ]

# Verifique as dimensões dos conjuntos de treino e teste
dim(train_x)
dim(test_y)

## Modelo machine learning

# Modelo Naive Bayes
modelo_naive_bayes <- naiveBayes(STATUS ~ .,
                                 data = train_x)

# Sumario dados
summary(modelo_naive_bayes)

# Previsão modelo
model_naive_pred <- predict(modelo_naive_bayes, test_y, type = "class")
model_naive_pred

# Acuracia modelo Naive bayes
acuracia_naive_bayes <- sum(model_naive_pred == test_y$STATUS) / nrow(test_y)
acuracia_naive_bayes

# Obtenha as probabilidades do modelo Naive Bayes nos dados de teste

# Matriz de confusão - Naive bayes
matriz_confusao_naive_bayes <- table(Real = test_y$STATUS, Previsto = model_naive_pred)
print(matriz_confusao_naive_bayes)

# Modelo 2 - Random forest

# Importtando biblioteca random forest
library(randomForest)

# Modelo Random Forest
modelo_rf <- randomForest(STATUS ~ ., data = train_x)

# Súmario modelo
summary(modelo_rf)

## Gráfico Random Forest
# Plotar uma das árvores do modelo
library(rpart)
library(rpart.plot)

# Extrair uma das árvores individuais do modelo (por exemplo, a primeira árvore)
arvore <- modelo_rf$forest[[1]]
arvore

# Converter o dataframe em matriz numérica
dados_treino_mat <- as.matrix(train_x[, -which(names(train_x) == "STATUS")])

# Ajustar um modelo de árvore de decisão individual usando rpart
modelo_arvore <- rpart(STATUS ~ .,
                       data = train_x,
                       method = "class",
                       control = rpart.control(minsplit = 1, cp = 0))

# Plotar a árvore de decisão
rpart.plot(modelo_arvore, main = "Árvore de Decisão - Random Forest")

# Previsões usando o modelo Random Forest
previsoes_rf <- predict(modelo_arvore, newdata = test_y)
previsoes_rf

# Calcular a acurácia das previsões
acuracia_rf <- sum(previsoes_rf == test_y$STATUS) / length(previsoes_rf)
print(paste("Acurácia Random Forest:", acuracia_rf))

# Matriz de confusão - Naive bayes
matriz_confusao_random_forest <- table(Real = test_y$STATUS, Previsto = previsoes_rf)
matriz_confusao_random_forest

# Modelo 3 - K-NN

# Encontrando o valor de K
# Encontrar o valor de k usando validação cruzada
set.seed(42)
ctrl <- trainControl(method = "cv", number = 5) # 5-fold cross-validation
modelo_knn <- train(STATUS ~ .,
                    data = train_x,
                    method = "knn",
                    trControl = ctrl,
                    tuneLength = 10)

# Mostrar os resultados da busca pelo valor de k
print(modelo_knn)

# Escolha o melhor valor de k encontrado
melhor_k <- modelo_knn$bestTune$k
melhor_k

# Gráfico valor K
# Criar um grid de valores de k para testar
k_values <- seq(1, 20, by = 2)

# Lista para armazenar as acurácias calculadas para cada valor de k
accuracies <- vector("list", length(k_values))

# Calcular a acurácia para cada valor de k usando validação cruzada
for (i in seq_along(k_values)) {
  set.seed(42)
  ctrl <- trainControl(method = "cv", number = 5)
  modelo_knn <- train(STATUS ~ .,
                        data = train_x,
                        method = "knn",
                        trControl = ctrl,
                        tuneGrid = data.frame(k = k_values[i]))
  accuracies[[i]] <- modelo_knn$results$Accuracy
}

# Criar um data frame com os valores de k e as acurácias correspondentes
dados_acuracia <- data.frame(k = k_values, Accuracy = unlist(accuracies))

# Criar o gráfico de linha
ggplot(dados_acuracia, aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  labs(x = "Valor de k", y = "Acurácia", title = "Acurácia em função do valor de k no modelo K-NN") +
  theme_minimal()

# Valor k
melhor_k

# Modelo K-NN
modelo_knn <- train(STATUS ~ .,
                    data = train_x,
                    method = "knn",
                    trControl = ctrl,
                    tuneGrid = data.frame(k = melhor_k))
modelo_knn

# Súmario modelo
summary(modelo_knn)

# Previsões usando o modelo knn
previsoes_knn <- predict(modelo_rf, newdata = test_y)
previsoes_knn

# Calcular a acurácia das previsões
acuracia_knn <- sum(previsoes_knn == test_y$STATUS) / length(previsoes_knn)
print(paste("Acurácia K-NN:", acuracia_knn))

# Matriz de confusão - knn
matriz_confusao_knn <- table(Real = test_y$STATUS, Previsto = previsoes_knn)
matriz_confusao_knn

# Modelo 4 - SVM
modelo_svm <- svm(STATUS ~ ., data = train_x)
modelo_svm

# Súmario modelo
summary(modelo_svm)

# Previsões usando o modelo SVM
previsoes_svm <- predict(modelo_svm, newdata = test_y)
previsoes_svm

# Calcular a acurácia das previsões
acuracia_svm <- sum(previsoes_svm == test_y$STATUS) / length(previsoes_svm)
print(paste("Acurácia SVM:", acuracia_svm))

# Matriz de confusão - svm
matriz_confusao_svm <- table(Real = test_y$STATUS, Previsto = previsoes_svm)
matriz_confusao_svm

# Modelo 5 - Decision tree

# Criando modelo decision tree
modelo_dt <- rpart(STATUS ~ .,
                   data = train_x)
modelo_dt

# Súmario modelo
summary(modelo_dt)

# Ajustar um modelo de árvore de decisão com rpart
modelo_arvore <- rpart(STATUS ~ ., data = train_x, method = "class")

# Plotar a árvore de decisão
rpart.plot(modelo_arvore, main = "Árvore de Decisão")

# Previsões usando o modelo dt
previsoes_dt <- predict(modelo_svm, newdata = test_y)
previsoes_dt

# Calcular a acurácia das previsões
acuracia_dt <- sum(previsoes_dt == test_y$STATUS) / length(previsoes_dt)
print(paste("Acurácia Árvore de decisão:", acuracia_dt))

# Matriz de confusão - dt
matriz_confusao_svm <- table(Real = test_y$STATUS, Previsto = previsoes_dt)
matriz_confusao_svm

# Modelo 6 - Regressão logistica
modelo <- glm(STATUS~.,
              data = train_x)


# Visualizar um resumo do modelo
summary(modelo)

# Prever os valores do conjunto de teste
previsoes <- predict(modelo, newdata = test_y, type = "response")
previsoes

# Transformar as previsões em classes (0 ou 1) usando um limiar de decisão (0.5 neste caso)
previsoes_classes <- ifelse(previsoes > 0.5, 1, 0)

# Certifique-se de que as classes reais estão como fatores
test_y$STATUS <- as.factor(test_y$STATUS)

# Calcular a matriz de confusão
matriz_confusao <- confusionMatrix(as.factor(previsoes_classes), test_y$STATUS)
matriz_confusao

# Exibir a matriz de confusão
print(matriz_confusao)

# Extrair a acurácia do modelo
accuracy_lr <- matriz_confusao$overall["Accuracy"]

# Exibir a acurácia do modelo
cat("Acurácia do modelo:", accuracy_lr, "\n")

# Resultados modelos
# Comparar a acurácia dos modelos

resultados <- data.frame(
  Modelo = c("Naive Bayes",
             "Regressão Logística",
             "Random Forest",
             "KNN",
             "SVM",
             "Decision Tree"),

  Acuracia = c(acuracia_naive_bayes,
               accuracy_lr,
               acuracia_rf,
               acuracia_knn,
               acuracia_svm,
               acuracia_dt)
)

print(resultados)

## Salvando modelo machine learning

# Salvando o modelo em um arquivo
saveRDS(modelo_naive_bayes, file = "Modelo_naive_bayes.rds")
saveRDS(modelo, file = "modelo_logistico.rds")
saveRDS(modelo_dt, file = "modelo_dt.rds")


## Conclusão

# Projeto "Aprimorando a Análise de Extintores de Incêndio através de Machine Learning"

# Neste projeto de Machine Learning, o objetivo foi avaliar a eficiência dos extintores de incêndio.
# Para isso, foi conduzida uma abrangente análise de dados com várias etapas importantes, visando extrair insights relevantes para aprimorar a segurança e desempenho desses dispositivos.
# A primeira etapa consistiu na coleta e análise dos dados brutos, onde informações cruciais foram extraídas, como o total de fluxo de ar associado ao funcionamento dos extintores.
# Na sequência, passou-se para a limpeza dos dados, um processo essencial para garantir a qualidade e confiabilidade dos resultados.
# Foram tratados dados inválidos, inconsistentes e duplicados, a fim de eliminar qualquer interferência negativa na análise.
# A terceira etapa consistiu na verificação e tratamento de valores nulos, NaN e outliers.
# Esses dados faltantes ou extremos poderiam prejudicar o desempenho dos modelos, portanto, foram cuidadosamente tratados para não comprometer o resultado final.
# Uma das partes mais importantes desse projeto foi a criação de modelos de Machine Learning. No total, foram desenvolvidos seis modelos diferentes, cada um com sua abordagem e características específicas.
# Após rigorosas avaliações e comparações, o modelo Decision Tree se destacou como o mais eficiente em fazer previsões com base nos dados de treinamento.
# Sua capacidade de dividir os dados em árvores de decisão levou a resultados precisos e confiáveis. Embora o modelo Decision Tree tenha obtido o melhor desempenho, é essencial mencionar que outros dois modelos
# também se mostraram promissores. O SVM (Support Vector Machine) e o KNN (K-Nearest Neighbors) também demonstraram resultados sólidos, embora ligeiramente inferiores ao Decision Tree.
# Em suma, com base nos resultados obtidos, podemos afirmar que o modelo Decision Tree foi o mais adequado para fazer previsões com base nos dados de treinamento.
# No entanto, é importante continuar a aprimorar e refinar os modelos, bem como realizar testes adicionais em dados de validação e teste para garantir a e ficácia do sistema em situações reais de combate a incêndios.
# Essa abordagem meticulosa e abrangente proporcionará uma melhoria significativa na segurança e eficiência dos extintores de incêndio, contribuindo para a proteção das vidas e propriedades.

## Refêrencia

# Link 1 - https://www.r-bloggers.com/2021/04/naive-bayes-classification-in-r/
# Link 2 - https://cienciadedados.uff.br/aprendizado-de-maquinas-com-r/
# Link 3 - https://caiquecoelho.medium.com/um-guia-completo-para-o-pr%C3%A9-processamento-de-dados-em-machine-learning-f860fbadabe1
# Link 4
