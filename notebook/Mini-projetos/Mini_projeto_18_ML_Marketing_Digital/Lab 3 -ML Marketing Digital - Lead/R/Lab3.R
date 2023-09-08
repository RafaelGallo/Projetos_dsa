# Lab 3 - Machine Learning em Marketing Digital - Prevendo a Probabilidade de Conversão do Lead

# Pergunta de Negócio do Lab 2
# Qual o número provável de usuários convertidos? (Regressão)

# Pergunta de Negócio do Lab 3
# Um Lead será convertido? Sim ou Não? Qual a probabilidade? (Classificação)

# Lembrete: Ajuste sua pasta de trabalho conforme necessário!

# Carregando os pacotes necessários
library(caret)
library(dplyr)
library(ggplot2)
library(readr)

# Carregando os dados
#dados_dsa <- read.csv("dataset.csv")
dados_dsa <- read_csv("Lab 3 -Machine Learning em Marketing Digital - Lead/dataset.csv")
dados_dsa

# Visualizando 5 primeiros dados
head(dados_dsa)

# Visualizando 5 últimos dados
tail(dados_dsa)

# Tipos de dados
str(dados_dsa)

# Visualizando os dados
View(dados_dsa)

# O que fazer neste caso?

# https://www.planalto.gov.br/ccivil_03/_ato2015-2018/2018/lei/l13709.htm
# https://www.gov.br/governodigital/pt-br/seguranca-e-protecao-de-dados/guias/guia_lgpd.pdf

# Remove a variável
dados_dsa$cor_da_pele <- NULL

# Visualizando os dados
View(dados_dsa)

##### Análise Exploratória #####

# Gráfico de barras
ggplot(dados_dsa, aes(x=converteu)) +
  geom_bar(aes(fill=converteu), alpha=0.7) +
  ggtitle("Distribuição da Variável 'Converteu'") +
  xlab("Converteu") +
  ylab("Contagem")

# Boxplot
ggplot(dados_dsa, aes(x=converteu, y=numero_cliques, fill=converteu)) +
  geom_boxplot() +
  ggtitle("Boxplot - Número de Cliques por Conversão") +
  xlab("Converteu") +
  ylab("Número de Cliques")

# Gráfico de barras
ggplot(dados_dsa, aes(x=faixa_etaria)) +
  geom_bar(fill="orangered3", alpha=0.7) +
  ggtitle("Distribuição de Faixa Etária") +
  xlab("Faixa Etária") +
  ylab("Contagem")

# Gráfico de dispersão
ggplot(dados_dsa, aes(x=numero_acessos, y=numero_cliques)) +
  geom_point(aes(color=converteu), alpha=0.6) +
  geom_smooth(method='lm') +
  ggtitle("Relação entre Número de Acessos e Número de Cliques") +
  xlab("Número de Acessos") +
  ylab("Número de Cliques")

# Sumarizar dados para obter a média do número de acessos por cidade
dados_sumarizados <- aggregate(numero_acessos ~ cidade, data = dados_dsa, FUN = mean)

# Gráfico de barras
ggplot(dados_sumarizados, aes(x=reorder(cidade, -numero_acessos), y=numero_acessos)) +
  geom_bar(stat="identity", aes(fill=cidade), alpha=0.7) +
  ggtitle("Gráfico de Barras - Média do Número de Acessos por Cidade") +
  xlab("Cidade") +
  ylab("Média do Número de Acessos") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

##### Pré-Processamento e Encoding de Variáveis Categóricas #####

# Visualizando os dados
View(dados_dsa)

# Dividindo os dados em treino e teste
set.seed(42)  # para reprodutibilidade
?createDataPartition
indices <- createDataPartition(dados_dsa$converteu, p = 0.75, list = FALSE)
treino <- dados_dsa[indices,]
teste <- dados_dsa[-indices,]

# Visualizando os dados e tipos de dados
View(treino)
str(treino)
str(teste)

# Aplicando label encoding à variável alvo
?as.factor
treino$converteu <- as.factor(treino$converteu)
teste$converteu <- as.factor(teste$converteu)

# Visualizando os dados e tipos de dados
View(treino)
View(teste)
str(treino)
str(teste)

# Aplicando label encoding às variáveis categóricas
treino$faixa_etaria <- as.factor(treino$faixa_etaria)
treino$cidade <- as.factor(treino$cidade)
treino$navegador_web <- as.factor(treino$navegador_web)

teste$faixa_etaria <- as.factor(teste$faixa_etaria)
teste$cidade <- as.factor(teste$cidade)
teste$navegador_web <- as.factor(teste$navegador_web)

# Visualizando os dados e tipos de dados
View(treino)
str(treino)
str(teste)

##### Modelagem Preditiva #####

# Treinando o modelo de regressão logística
# Versão 1 do modelo
?glm
modelo_v1 <- glm(converteu ~ .,
                 family = binomial(link = 'logit'),
                 data = treino)

# Sumário do modelo
summary(modelo_v1)

# Fazendo previsões no conjunto de teste
previsoes_prob <- predict(modelo_v1, newdata = teste, type = 'response')
print(previsoes_prob)
previsoes_classe <- ifelse(previsoes_prob > 0.5, 'sim', 'não') #cutoff
print(previsoes_classe)

# Matriz de confusão
matriz_confusao <- confusionMatrix(as.factor(previsoes_classe), teste$converteu)
print(matriz_confusao)

# No Generalized Linear Model (GLM) para regressão logística em R, a referência da categoria (ou classe "positiva")
# é definida com base nos níveis do fator da variável resposta. A primeira categoria (ou nível) da variável fator é usada
# como referência por padrão. No entanto, é possível reordenar os níveis do fator para definir qual classe deve ser tratada
# como a "positiva" ou referência.
#treino$converteu <- relevel(treino$converteu, ref = "sim")
#teste$converteu <- relevel(teste$converteu, ref = "sim")
# Essa classe "positiva" é apenas referência e não significa coisa boa ou ruim.

# Métricas de avaliação
acuracia <- sum(diag(matriz_confusao$table)) / sum(matriz_confusao$table)
cat("Acurácia:", acuracia, "\n")

# Mantemos a faixa-etaria no dataset?
summary(modelo_v1)

# Versão 2 do modelo
modelo_v2 <- glm(converteu ~
                   numero_acessos +
                   numero_cliques,
                 family = binomial(link='logit'),
                 data = treino)

# Sumário do modelo
summary(modelo_v2)

# Fazendo previsões no conjunto de teste
previsoes_prob <- predict(modelo_v2, newdata = teste, type = 'response')
previsoes_classe <- ifelse(previsoes_prob > 0.5, 'sim', 'não')

# Matriz de confusão
matriz_confusao <- confusionMatrix(as.factor(previsoes_classe), teste$converteu)
print(matriz_confusao)

# Métricas de avaliação
acuracia <- sum(diag(matriz_confusao$table)) / sum(matriz_confusao$table)
cat("Acurácia:", acuracia, "\n")

##### Deploy #####

# Salva o modelo treinado em disco
save(modelo_v2, file = "modelo_v2.RData")

# Carrega o modelo do disco
load("modelo_v2.RData")

# Novos dados
novos_dados <- data.frame(numero_acessos = c(60), numero_cliques = c(20))
print(novos_dados)

# Previsões com novos dados
previsoes_novos_dados_prob <- predict(modelo_v2, newdata = novos_dados, type = 'response')
previsoes_novos_dados_classe <- ifelse(previsoes_novos_dados_prob > 0.5, 'sim', 'não')

# Mostrando as previsões de classe e probabilidade
novos_dados$Lead_Convertido <-previsoes_novos_dados_classe
novos_dados$Probabilidade <-previsoes_novos_dados_prob * 100
print(novos_dados)

# Fim




