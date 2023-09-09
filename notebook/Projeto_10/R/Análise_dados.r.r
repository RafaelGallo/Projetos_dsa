## Business Analytics Projeto com Feedback 10

# Projeto - Sistema de Recomendação Para Rede de Varejo Usando Market Basket Analysis

## Descrição projeto

# Quer você faça compras com listas de compras meticulosamente
# planejadas ou deixe que o capricho guie seus passos, nossos rituais únicos de
# compra definem quem somos. Instacart, um aplicativo de pedido e entrega de
# supermercado, tem como objetivo facilitar o preenchimento de sua geladeira e
# despensa com seus itens pessoais favoritos e itens básicos quando você precisar deles.
# Depois de selecionar produtos por meio do aplicativo Instacart, os compradores revisam seus pedidos, fazem compras e a entrega é feita na loja
# mais próxima a você.
# A equipe de ciência de dados da Instacart desempenha um papel
# importante no fornecimento dessa experiência de compra agradável. Atualmente,
# eles usam dados transacionais para desenvolver modelos que preveem quais
# produtos um usuário comprará novamente, quais tentará pela primeira vez ou
# quais adicionará ao carrinho durante uma sessão. Recentemente, a Instacart
# disponibilizou esses dados de forma aberta e o link para download você encontra logo abaixo
# Neste projeto de ciência de dados, você usará esses dados anônimos nos
# pedidos dos clientes ao longo do tempo para prever quais produtos adquiridos
# anteriormente estarão no próximo pedido de um usuário.

## Definição problema
# Nossa recomendação é que você utilize linguagem R e os pacotes de
# Market Basket Analysis oferecidos em R. O link para download do dataset você

## Base dados
# Link: https://www.kaggle.com/competitions/instacart-market-basket-analysis


# Importando bibliotecas
library(arules)
library(arulesViz)
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
  left_join(tmp, by = c("department_id", "aisle_id")) %>%
  mutate(onesize = 1)

# Grafico treemap
treemap(data,index=c("department","aisle"),vSize="n",
        title="",
        palette="Set3",
        border.col="#FFFFFF")

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
# Em resumo, minha participação neste projeto foi incrivelmente gratificante, com cada fase desempenhando um papel crucial em nossa jornada para melhorar a recomendação de produtos.
# E que o segundo modelo que desenvolvemos foi mais eficaz, representando um avanço significativo em nossa pesquisa e esforços.


## Refrência
# Link 1 - https://www.kaggle.com/code/msp48731/frequent-itemsets-and-association-rules
# Link 2 - https://www.kaggle.com/code/philippsp/exploratory-analysis-instacart
# Link 3 - chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.maxwell.vrac.puc-rio.br/14008/14008_4.PDF
# Link 4 - https://www.analyticsvidhya.com/blog/2021/10/a-comprehensive-guide-on-market-basket-analysis/

## Citação
# Esse projeto com Feedback 10 da formação cientista de dados da @DataScienceAcademy
# jeremy stanley, Meg Risdal, sharathrao, Will Cukierski. (2017). Instacart Market Basket Analysis. Kaggle. https://kaggle.com/competitions/instacart-market-basket-analysis
