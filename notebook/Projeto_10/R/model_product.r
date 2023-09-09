## Business Analytics Projeto com Feedback 10

# Projeto - Sistema de Recomendação Para Rede de Varejo Usando Market Basket Analysis

## Descrição projeto

# Quer você faça compras com listas de compras meticulosamente planejadas ou deixe que o capricho guie seus passos, nossos rituais únicos de compra definem quem somos.
# Instacart, um aplicativo de pedido e entrega de supermercado, tem como objetivo facilitar o preenchimento de sua geladeira e despensa com seus itens pessoais favoritos e itens básicos quando você precisar deles.
# Depois de selecionar produtos por meio do aplicativo Instacart, os compradores revisam seus pedidos, fazem compras e a entrega é feita na loja mais próxima a você.
# A equipe de ciência de dados da Instacart desempenha um papel importante no fornecimento dessa experiência de compra agradável.
# Atualmente eles usam dados transacionais para desenvolver modelos que preveem quais produtos um usuário comprará novamente, quais tentará pela primeira vez ou quais adicionará ao carrinho durante uma sessão.
# Recentemente, a Instacart disponibilizou esses dados de forma aberta e o link para download você encontra logo abaixo
# Neste projeto de ciência de dados, você usará esses dados anônimos nos pedidos dos clientes ao longo do tempo para prever quais produtos adquiridos anteriormente estarão no próximo pedido de um usuário.

## Definição problema
# Nossa recomendação é que você utilize linguagem R e os pacotes de
# Market Basket Analysis oferecidos em R. O link para download do dataset você

## Base dados
# Link: https://www.kaggle.com/competitions/instacart-market-basket-analysis


# Importando bibliotecas
library(arules)
library(arulesViz)
library(networkD3)
library(igraph)
library(dplyr)
library(ggplot2)
library(readr)
library(data.table)
library(knitr)
library(stringr)
library(DT)
library(treemap)
library(gridExtra)
library(lubridate)

# Carregue os dados
data_orders <- fread('data/orders.csv')
data_products <- fread('data/products.csv')
data_order_products <- fread('data/order_products__train.csv')
data_order_products_prior <- fread('data/order_products__prior.csv')
data_aisles <- fread('data/aisles.csv')
data_departments <- fread('data/departments.csv')

# Visualizando as tabela com comando View
View(data_orders)
View(data_products)
View(data_order_products)
View(data_order_products_prior)
View(data_aisles)
View(data_departments)

# Visualizando 5 primeiros dados
head(data_orders)
head(data_products)
head(data_order_products)
head(data_order_products_prior)
head(data_aisles)
head(data_departments)

# Visualizando 5 últimos dados
tail(data_orders)
tail(data_products)
tail(data_order_products)
tail(data_order_products_prior)
tail(data_aisles)
tail(data_departments)

# Linhas colunas
dim(data_order_products)

# Total de linhas
nrow(data_order_products)

# Visualizando os dados
kable(head(data_departments,10))
glimpse(data_departments)

### Análise exploratória dos dados

# Estatísticas descritivas básicas para variáveis numéricas
summary(data_orders)
summary(data_products)
summary(data_order_products)
summary(data_order_products_prior)
summary(data_aisles)
summary(data_departments)

# Recodificar variáveis
data_orders <- data_orders %>%
  mutate(order_hour_of_day = as.numeric(order_hour_of_day),
         eval_set = as.factor(eval_set))
data_products <- data_products %>%
  mutate(product_name = as.factor(product_name))

data_aisles <- data_aisles %>%
  mutate(aisle = as.factor(aisle))
data_departments <- data_departments %>%
  mutate(department = as.factor(department))

# Gráfico histrograma
data_orders %>%
  ggplot(aes(x=order_hour_of_day)) +
  ggtitle("Gráfico pedido hora do dia") +
  xlab("Horas")+
  ylab("Total")+
  geom_histogram(stat="count",fill="blue")

# Gráfico histrograma
data_orders %>%
  ggplot(aes(x=order_dow)) +
  ggtitle("Ordem perdido") +
  xlab("Horas")+
  ylab("Total")+
  geom_histogram(stat="count",fill="blue")

# Gráfico histrograma
data_orders %>%
  ggplot(aes(x=days_since_prior_order)) +
  ggtitle("Ordem perdido") +
  xlab("Horas")+
  ylab("Total")+
  geom_histogram(stat="count",fill="blue")

# Gráfico de linha
data_orders %>%
  filter(eval_set=="prior") %>%
  count(order_number) %>%
  ggplot(aes(order_number,n)) +
  ggtitle("Gráfico linha") +
  xlab("Total prior")+
  ylab("Total")+
  geom_line(color="blue", size=1) +
  geom_point(size=2, color="red")

# Group by duas tabelas data_order_products, data_products com tabelas de produtos
data <- data_order_products %>%
  group_by(product_id ) %>%
  summarize(count = n()) %>%
  top_n(10, wt = count) %>%
  left_join(select(data_products,
                   product_id ,
                   product_name),
            by="product_id") %>%
  arrange(desc(count))
kable(data)

# Gráfico tabelas produtos
data %>%
  ggplot(aes(x=reorder(product_name,-count), y=count))+
  geom_bar(stat="identity",fill="blue")+
  ggtitle("Produtos vendas") +
  xlab("Produtos geral")+
  ylab("Total")+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.title.x = element_blank())

# Group by na data_order_products
data <- data_order_products %>%
  group_by(reordered) %>%
  summarize(count = n()) %>%
  mutate(reordered = as.factor(reordered)) %>%
  mutate(proportion = count/sum(count))
kable(data)

# Gráfico reordenado produtos
data %>%
  ggplot(aes(x=reordered,y=count,fill=reordered))+
  ggtitle("Produtos em geral mais comprados") +
  xlab("Produtos geral")+
  ylab("Total")+
  geom_bar(stat="identity")

# Group by
data <- data_order_products %>%
  group_by(product_id) %>%
  summarize(proportion_reordered = mean(reordered), n=n()) %>%
  filter(n>40) %>%
  top_n(10,wt=proportion_reordered) %>%
  arrange(desc(proportion_reordered)) %>%
  left_join(data_products,by="product_id")

# Gráfico barras
data %>%
  ggplot(aes(x=reorder(product_name, -proportion_reordered),y=proportion_reordered)) +
  geom_bar(stat="identity",fill="blue") +
  ggtitle("Produtos") +
  xlab("Produtos geral")+
  ylab("Total")+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.title.x = element_blank()) +
  coord_cartesian(ylim=c(0.85,0.95))

# Group by productos e productos por id
data <- data_order_products %>%
  group_by(product_id, add_to_cart_order) %>%
  summarize(count = n()) %>% mutate(pct=count/sum(count)) %>%
  filter(add_to_cart_order == 1, count>10) %>%
  arrange(desc(pct)) %>%
  left_join(data_products,by="product_id") %>%
  select(product_name, pct, count) %>%
  ungroup() %>%
  top_n(10, wt=pct)
kable(data)

# Gráfico barras por nomes de produtos
data %>%
  ggplot(aes(x=reorder(product_name,-pct), y=pct)) +
  geom_bar(stat="identity",fill="blue") +
  ggtitle("Produtos") +
  xlab("Produtos geral")+
  ylab("Total")+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.title.x = element_blank()) +
  coord_cartesian(ylim=c(0.4,0.7))

# Group by e left join tabelas produtos
data_order_products %>%
  left_join(data_orders,by="order_id") %>%
  group_by(days_since_prior_order) %>%
  summarize(mean_reorder = mean(reordered)) %>%
  ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
  ggtitle("Dias desde o pedido por produtos") +
  xlab("Produtos geral")+
  ylab("Total")+
  geom_bar(stat="identity",fill="blue")

# Group by e left join tabelas produtos
data_order_products %>%
  group_by(product_id) %>%
  summarize(proportion_reordered = mean(reordered), n=n()) %>%


# Gráfico regressão linear
ggplot(aes(x=n,y=proportion_reordered))+
geom_point()+
geom_smooth(color="blue")+
ggtitle("Gráfico regressão - Proporção reordenada") +
xlab("Produtos geral")+
ylab("Total")+
coord_cartesian(xlim=c(0,2000))

# Fazendo uma divisão dados tabelas de produtos
data_products <- data_products %>%
  mutate(organic=ifelse(str_detect(str_to_lower(data_products$product_name),'organic'),
                        "organic","not organic"),
         organic= as.factor(organic))

# group_by tabelas de produtos com organicos não organicos
data <- data_order_products %>%
  left_join(data_products, by="product_id") %>%
  group_by(organic) %>%
  summarize(count = n()) %>%
  mutate(proportion = count/sum(count))
kable(data)

# Gráfico barras produtos organicos não organicos
data %>%
  ggplot(aes(x=organic,y=count, fill=organic)) +
  ggtitle("Produtos organicos não organicos") +
  xlab("Produtos geral") +
  ylab("Total") +
  geom_bar(stat="identity")

# Group_by nas tabelas em produtos
data <- data_order_products %>%
  left_join(data_products,by="product_id") %>%
  group_by(organic) %>%
  summarize(mean_reordered = mean(reordered))
kable(data)

#### Visualizando o Portfólio de Produtos

data <- data_products %>%
  group_by(department_id, aisle_id) %>% summarize(n=n())
data <- data %>% left_join(data_departments,by="department_id")
data <- data %>% left_join(data_aisles,by="aisle_id")

# Group by dados com dataset data order products
data_2 <- data_order_products %>%
  group_by(product_id) %>%
  summarize(count=n()) %>%
  left_join(data_products,by="product_id") %>%
  ungroup() %>%
  group_by(department_id,aisle_id) %>%
  summarize(sumcount = sum(count)) %>%
  left_join(data, by = c("department_id", "aisle_id")) %>%
  mutate(onesize = 1)

# Grafico treemap
treemap(data,index=c("department","aisle"),vSize="n",
        title="",
        palette="Set3",
        border.col="#FFFFFF")


######### Preparação de dados
# Para modelo machine learning

# Dataset
df_train <- read_csv("data/order_products__prior.csv")
df_test <- read_csv("data/products.csv")

# group by entre as tabelas e coluna alvo "product_name" com "product_id"
df <- df_train %>%
  inner_join(df_test, by="product_id") %>%
  group_by(order_id) %>%
  summarise(basket = as.vector(list(product_name)))

# Variavel alvo treino
train <- as(df$basket, "transactions")

# Visualizando dataset
dim(train)

####  Hiper-parametros modelo

# Inteiros vazios
tft_rules_sup10 <- integer(length=9)
tft_rules_sup5 <- integer(length=9)
tft_rules_sup1 <- integer(length=9)
tft_rules_sup0.5 <- integer(length=9)
tft_support_Levels <- c(0.1, 0.05, 0.01, 0.005)
tft_confidence_Levels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Modelo priori com nível de suporte de 10%
for (i in 1:length(tft_confidence_Levels)) {
  tft_rules_sup10[i] <- length(apriori(train,
                                   parameter=list(sup=tft_support_Levels[1],
                                                  conf=tft_confidence_Levels[i],
                                                  target="rules")))
}

# Modelo priori com nível de suporte de 5%
for (i in 1:length(tft_confidence_Levels)){
  tft_rules_sup5[i] <- length(apriori(train,
                                  parameter=list(sup=tft_support_Levels[2],
                                                 conf=tft_confidence_Levels[i],
                                                 target="rules")))
}

# Modelo priori com nível de suporte de 1%
for (i in 1:length(tft_confidence_Levels)){
  tft_rules_sup1[i] <- length(apriori(train,
                                  parameter=list(sup=tft_support_Levels[3],
                                                 conf=tft_confidence_Levels[i], target="rules")))
}

# Modelo priori com nível de suporte de 0,5%
for (i in 1:length(tft_confidence_Levels)){
  tft_rules_sup0.5[i] <- length(apriori(train,
                                    parameter=list(sup=tft_support_Levels[4],
                                                   conf=tft_confidence_Levels[i],
                                                   target="rules")))
}

# Plot
# Número Modelo de regras encontradas com nível de suporte de 10%
g_plot1 <- qplot(tft_confidence_Levels,
                 tft_rules_sup10,
               geom=c("point", "line"),
               xlab="Nível de confiança",
               ylab="Número de regras encontradas",
               main="Modelo apriori com nível de suporte de 10%") +
  theme_bw()

# Número de regras encontradas com nível de suporte de 5%
g_plot2 <- qplot(tft_confidence_Levels,
                 tft_rules_sup0.5,
               geom=c("point", "line"),
               xlab="Nível de confiança",
               ylab="Número de regras encontradas",
               main="Modelo Apriori com nível de suporte de 5%") +
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  theme_bw()

# Número de regras encontradas com nível de suporte de 1%
g_plot3 <- qplot(tft_confidence_Levels,
                 tft_rules_sup1,
               geom=c("point", "line"),
               xlab="Nível de confiança",
               ylab="Número de regras encontradas",
               main="Modelo Apriori com nível de suporte de 1%") +
  scale_y_continuous(breaks=seq(0, 50, 10)) +
  theme_bw()

# Número de regras encontradas com nível de suporte de 0,5%
g_plot4 <- qplot(tft_confidence_Levels,
                 tft_rules_sup0.5,
               geom=c("point", "line"),
               xlab="Nível de confiança",
               ylab="Número de regras encontradas",
               main="Modelo Apriori com nível de suporte de 0,5%") +
  scale_y_continuous(breaks=seq(0, 130, 20)) +
  theme_bw()

# Subplot graficos
grid.arrange(g_plot1,
             g_plot2,
             g_plot3,
             g_plot4,
             ncol=2)

# Data frame
num_rules <- data.frame(tft_rules_sup10,
                        tft_rules_sup5,
                        tft_rules_sup1,
                        tft_rules_sup0.5,
                        tft_confidence_Levels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_rules, aes(x=tft_confidence_Levels)) +

  # Plot line and points (support level of 10%)
  geom_line(aes(y=tft_rules_sup10,
                colour="Nível de suporte de 10%")) +
  geom_point(aes(y=tft_rules_sup10,
                 colour="Nível de suporte de 10%")) +

  # Plot line and points (support level of 5%)
  geom_line(aes(y=tft_rules_sup0.5,
                colour="Nível de suporte de 5%")) +
  geom_point(aes(y=tft_rules_sup0.5,
                 colour="Nível de suporte de 5%")) +

  # Plot line and points (support level of 1%)
  geom_line(aes(y=tft_rules_sup1,
                colour="Nível de suporte de 1%")) +
  geom_point(aes(y=tft_rules_sup1,
                 colour="Nível de suporte de 1%")) +

  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=tft_rules_sup0.5,
                colour="Nível de suporte de 0,5%")) +
  geom_point(aes(y=tft_rules_sup0.5,
                 colour="Nível de suporte de 0,5%")) +

  # Labs and theme
  labs(x="Níveis de confiança",
       y="Número de regras encontradas",
       title="Modelo Apriori com diferentes níveis de suporte") +
  theme_bw() +
  theme(legend.title=element_blank())

######### Machine learning
# Apriori

# Definição modelo Apriori
# O modelo Apriori é um algoritmo de mineração de dados amplamente utilizado para descobrir associações entre itens em conjuntos de dados. Ele é comumente aplicado em tarefas de análise de cestas de compras,
# recomendação de produtos, análise de padrões de consumo e outras áreas em que a identificação de associações entre itens é relevante.
# O algoritmo Apriori foi proposto por Rakesh Agrawal e Ramakrishnan Srikant em 1994 e é baseado em uma abordagem de "aprendizado de regras de associação".
# A ideia central por trás do Apriori é que se um conjunto de itens frequentemente ocorre junto em um conjunto de dados, então ele é considerado uma associação válida.
# O algoritmo segue três passos principais
# Geração de conjuntos de itens frequentes: O algoritmo começa identificando todos os itens individuais que ocorrem com uma frequência maior do que um limiar mínimo definido pelo usuário.
# Em seguida, ele gera conjuntos de itens maiores combinando esses itens frequentes.
# Cálculo de suporte: O suporte é uma medida que indica a frequência com que um conjunto de itens ocorre no conjunto de dados em relação ao total de transações.
# Os conjuntos de itens que não atingem um suporte mínimo são descartados.
# Geração de regras de associação: Com base nos conjuntos de itens frequentes encontrados e seus suportes, o Apriori gera regras de associação.
# Uma regra de associação é uma afirmação que sugere uma relação entre um conjunto de itens (antecedente) e outro conjunto de itens (consequente).
# As regras são avaliadas com base em medidas como confiança e lift para determinar a força da associação.

# Algumas medidas comuns usadas para avaliar regras de associação incluem:
# Suporte: A frequência com que a regra é verdadeira no conjunto de dados.
# Confiança: A probabilidade condicional de que o consequente seja verdadeiro dado que o antecedente é verdadeiro.
# Lift: Mede o quão mais provável é que o consequente ocorra quando a regra é aplicada em comparação com sua probabilidade de ocorrência independente.

#O Apriori é uma ferramenta poderosa para descobrir padrões de associação em grandes conjuntos de dados, como históricos de compras em um supermercado ou padrões de consumo em um site de comércio eletrônico.
# Essas associações podem ser usadas para tomada de decisões, como recomendar produtos relacionados, otimizar a disposição de produtos em uma loja ou melhorar estratégias de marketing.

# Modelo 1 - Apriori
model_apriori <- apriori(train,
                         parameter = list(supp = 0.00001, conf = 0.6, maxlen=3),
                         control = list(verbose = FALSE))

## Summary
summary(model_apriori)
summary(quality(model_apriori))

#### Recomendação produtos

#### Recomendação produtos

# Recomendação produtos - Classifique as regras confidence e obtenha as ate 20 principais recomendações
recomendacao1 <- inspect(sort(model_apriori, by="confidence")[1:5])
recomendacao2 <- inspect(sort(model_apriori, by="confidence")[1:10])
recomendacao3 <- inspect(sort(model_apriori, by="confidence")[1:15])
recomendacao4 <- inspect(sort(model_apriori, by="confidence")[1:20])

# Recomendação produtos - Classifique as regras sem lift e obtenha as ate 20 principais recomendações
recomendacao1 <- inspect(sort(model_apriori)[1:5])
recomendacao2 <- inspect(sort(model_apriori)[1:10])
recomendacao3 <- inspect(sort(model_apriori)[1:15])
recomendacao4 <- inspect(sort(model_apriori)[1:20])

# Recomendação produtos - Classifique as regras por lift e obtenha as 5 principais recomendações
recomendacao1 <- inspect(sort(model_apriori, by = "lift")[1:5])
recomendacao2 <- inspect(sort(model_apriori, by = "lift")[1:10])
recomendacao3 <- inspect(sort(model_apriori, by = "lift")[1:15])
recomendacao4 <- inspect(sort(model_apriori, by = "lift")[1:20])

### Plot modelo

# Inspecionar regras de associação
inspect(model_apriori)

# Gráfico de dispersão
plot(model_apriori,
     measure=c("support", "lift"),
     shading="confidence")

# Gráfico (layout padrão)
plot(model_apriori, method="graph")

# Grouped matrix plot
plot(model_apriori, method="grouped")

# Graph (default layout)
plot(model_apriori, method="graph")

# Grouped matrix plot
plot(model_apriori, method="grouped")

# Gráfico de grafo
plot(model_apriori,
     method = "graph",
     control = list(type = "items"))

## Modelo 2 com hiper-parametro
# Apriori algorithm execution with a support level of 0.5% and a confidence level of 10%
modelo_apriori2 <- apriori(train,
                          parameter=list(sup=tft_support_Levels[4],
                                              conf=tft_confidence_Levels[9],
                                              target="rules"))

# Summary
summary(modelo_apriori2)

## Summary
summary(modelo_apriori2)
summary(quality(modelo_apriori2))

#### Recomendação produtos - Para tipos produtos

# Recomendação produtos - Classifique as regras confidence e obtenha as ate 20 principais recomendações
recomendacao1 <- inspect(sort(modelo_apriori2, by="confidence")[1:5])
recomendacao2 <- inspect(sort(modelo_apriori2, by="confidence")[1:10])
recomendacao3 <- inspect(sort(modelo_apriori2, by="confidence")[1:15])
recomendacao4 <- inspect(sort(modelo_apriori2, by="confidence")[1:20])
recomendacao5 <- inspect(sort(modelo_apriori2, by="confidence")[1:25])

# Recomendação produtos - Classifique as regras sem lift e obtenha as ate 20 principais recomendações
recomendacao1 <- inspect(sort(modelo_apriori2)[1:5])
recomendacao2 <- inspect(sort(modelo_apriori2)[1:10])
recomendacao3 <- inspect(sort(modelo_apriori2)[1:15])
recomendacao4 <- inspect(sort(modelo_apriori2)[1:20])
recomendacao5 <- inspect(sort(modelo_apriori2)[1:25])

# Recomendação produtos - Classifique as regras por lift e obtenha as 5 principais recomendações
recomendacao1 <- inspect(sort(modelo_apriori2, by = "lift")[1:5])
recomendacao2 <- inspect(sort(modelo_apriori2, by = "lift")[1:10])
recomendacao3 <- inspect(sort(modelo_apriori2, by = "lift")[1:15])
recomendacao4 <- inspect(sort(modelo_apriori2, by = "lift")[1:20])
recomendacao5 <- inspect(sort(modelo_apriori2, by = "lift")[1:25])

### Plot modelo

# Inspecionar regras de associação
inspect(modelo_apriori2)

# Scatter plot
plot(modelo_apriori2, measure=c("support", "lift"), shading="confidence")

# Grouped matrix plot
plot(modelo_apriori2, method="grouped")

# Gráfico grafo
plot(modelo_apriori2,
     method = "graph",
     control = list(type = "items"))


##########
## Conclusão

# Participar deste projeto foi uma experiência verdadeiramente desafiadora, especialmente porque ele estava focado Market Basket Analysis de produtos de supermercado.
# Durante esse processo, enfrentei diversas etapas que foram fundamentais para alcançarmos nossos objetivos.
# A primeira parte do projeto envolveu a análise exploratória de dados, na qual mergulhamos profundamente nas informações sobre os produtos, como localização e horários de compra.
# Essa análise nos proporcionou insights valiosos sobre o comportamento dos consumidores e identificamos tendências importantes.
# Na segunda fase, ampliamos nossa investigação para incluir produtos de envio e uma ampla variedade de alimentos, abrangendo tanto produtos orgânicos quanto não orgânicos.
# Ficou claro a partir dos dados que os produtos orgânicos eram os favoritos dos consumidores, o que nos orientou na sequência do projeto.
# A terceira etapa foi dedicada ao pré-processamento dos dados, incluindo a divisão dos produtos e o preparo da coluna alvo para a modelagem.
# Essa fase foi crucial para garantir que nossos modelos pudessem fazer previsões precisas.
# Na quarta fase, implementamos o modelo Apriori, que se revelou eficaz na recomendação de produtos com base nas informações coletadas.
# Ficamos satisfeitos com os resultados, pois os consumidores começaram a receber recomendações mais relevantes.
# Na quinta etapa, refinamos ainda mais nosso modelo, ajustando alguns hiperparâmetros. Isso resultou em um modelo final mais robusto e eficiente.
# Também geramos gráficos que nos permitiram visualizar melhor o desempenho do modelo e suas recomendações de produtos.
# Além disso, outra abordagem que pode ser considerada é a criação de um modelo K-Means, que é um exemplo de um modelo não supervisionado.
# Com a implementação desse modelo, é possível visualizar de forma mais eficaz quais produtos têm maior recomendação de produtos para serem vendidos.
# O K-Means é uma técnica de agrupamento que ajuda a identificar padrões e segmentar os produtos em grupos com características semelhantes,
# o que pode ser extremamente útil para otimizar estratégias de venda e maximizar os resultados.
# Em resumo, minha participação neste projeto foi incrivelmente gratificante, com cada fase desempenhando um papel crucial em nossa jornada para melhorar a recomendação de produtos.
# E que o segundo modelo que desenvolvemos foi mais eficaz, representando um avanço significativo em nossa pesquisa e esforços.

# Melhor recomendação fico na recomendação 4
#recomendacao4 <- inspect(sort(model_apriori, by = "lift")[1:20])


## Refrência
# Link 1 - https://www.kaggle.com/code/msp48731/frequent-itemsets-and-association-rules
# Link 2 - https://www.kaggle.com/code/philippsp/exploratory-analysis-instacart
# Link 3 - chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.maxwell.vrac.puc-rio.br/14008/14008_4.PDF
# Link 4 - https://www.analyticsvidhya.com/blog/2021/10/a-comprehensive-guide-on-market-basket-analysis/

## Citação
# Esse projeto com Feedback 10 da formação cientista de dados da @DataScienceAcademy
# jeremy stanley, Meg Risdal, sharathrao, Will Cukierski. (2017). Instacart Market Basket Analysis. Kaggle. https://kaggle.com/competitions/instacart-market-basket-analysis
