library(tidyverse)
library(readxl)
library(lubridate)
library(cluster)
library(arules)
library(RecordLinkage)

library(tidyr)
library(purrr)
library(weights)


#==========================#
#    Limpieza de datos     #
#==========================#

df <- read_excel("Online Retail.xlsx") %>%
  mutate(StockCode = toupper(StockCode)) %>%
  filter(Quantity > 0, UnitPrice > 0)

#Tablas normalizadas BCNF
invoices <- df %>%
  distinct(InvoiceNo, InvoiceDate, CustomerID, Country) %>%
  distinct(InvoiceNo, .keep_all=TRUE)

invoice_lines <- df %>%
  select(StockCode, Quantity, UnitPrice, InvoiceNo) %>%
  mutate(Total = UnitPrice * Quantity)

products <- df %>%
  distinct(StockCode, Description) %>%
  distinct(StockCode, .keep_all=TRUE)

#Procedimiento para encontrar las inconsistencias:
#View(products %>% count(Description) %>% filter(n > 1) %>%
#inner_join(products, by="Description"))
#View(products %>% count(StockCode) %>% filter(n > 1) %>%
#inner_join(products, by="StockCode"))
#View(df %>% filter(Description == "damaged"))
#View(df %>% filter(Quantity <= 0))

#Dataframe completo sin inconsitencias
df <- invoice_lines %>%
  left_join(invoices, by="InvoiceNo") %>%
  left_join(products, by="StockCode")


#==========================#
#   Exploracion de datos   #
#==========================#

#???Los clientes son recurrentes o solo compran en una ocasion?
ggplot(invoices, aes(x=InvoiceDate, fill=Country)) +
  geom_histogram()
invoices %>% distinct(Country) %>% print(n=38)
invoices %>% count(CustomerID) %>% summarize(mean(n))
#el cliente tipico como entre 4 a 5 veces en el periodo analizado

#???Cuanto se vende en total por cada region?
#Regiones
WesternEurope = c('United Kingdom', 'France', 'Switzerland', 'Netherlands',
                  'EIRE', 'Belgium', 'Channel Islands')
NorthernEurope = c('Norway', 'Sweden', 'Finland', 'Iceland', 'Denmark')
CentralEurope = c('Germany', 'Austria', 'Czech Republic')
SouthernEurope = c('Italy', 'Greece', 'Cyprus', 'Malta', 'Spain', 'Portugal')
EasternEurope = c('Poland', 'Lithuania')
MiddleEast = c('Bahrain', 'Israel', 'Lebanon', 'United Arab Emirates',
               'Saudi Arabia')
EastAsia = c('Japan', 'Hong Kong', 'Singapore')
America = c('Canada', 'USA', 'Brazil')
Other = c('Unspecified', 'European Community', 'Australia', 'RSA')

ggplot(invoices, aes(x=InvoiceDate, fill=Region)) + geom_histogram()

df  <- df  %>% mutate (Region = case_when(
  Country %in% NorthernEurope ~ 'Northern Europe',
  Country %in% WesternEurope ~ 'Western Europe',
  Country  %in% CentralEurope ~ 'Central Europe',
  Country  %in% SouthernEurope ~ 'Southern Europe',
  Country %in% EasternEurope ~ 'Eastern Europe',
  Country %in% MiddleEast ~ 'Middle East',
  Country %in% EastAsia ~ 'East Asia',
  Country %in% America ~ 'America',
  Country %in% Other ~ 'Other'
))

df$Subtotal = df$UnitPrice * df$Quantity

ventas_region <- as.data.frame(
  df %>% group_by(Region) %>% summarize (total = sum(Subtotal)) %>% arrange(desc(total)) %>% 
    mutate(porcentage = (total/sum(total)))
)
ventas_pais <- as.data.frame(
  df %>% group_by(Country) %>% summarize (total = sum(Subtotal)) %>% arrange(desc(total)) %>% 
    mutate(porcentage = (total/sum(total)))
)
#93% ventas son de Europa Occidental
#84.6% son de UK

#???Los precios se comportan diferentes segun la region?
df %>% group_by(Region, StockCode) %>% summarize(precio_promedio = mean(UnitPrice), 
                                                 unidades = n()) %>% arrange(desc(unidades)) %>%
  filter(StockCode %in% c('85123A', '85099B','22423')) %>% print(n=22)

#???La cantidad de productos diferentes que compran los clientes varian por region?
df %>% group_by(Region) %>% distinct(StockCode) %>% count() %>% arrange(desc(n))


#???Cuales son los articulos que se compran en mayor cantidad?
df %>% 
  group_by(StockCode, Description)%>% 
  summarise(Quantity=sum(Quantity))%>% 
  arrange(desc(Quantity))

df %>% group_by(StockCode, Description) %>% count() %>% arrange(desc(n))


#???Como varian las transacciones por dia de la semana?
df %>%
  mutate(weekday = wday(InvoiceDate)) %>%
  mutate(weekday = factor(weekday, levels = c(1, 2, 3, 4, 5, 6, 7), 
                          labels = c("Domingo", "Lunes", "Martes", "Miercoles", 
                                     "Jueves", "Viernes", "Sabado"))) %>%
  group_by(weekday) %>%
  distinct(InvoiceNo) %>%
  count()%>% 
  arrange(desc(n))

#???Cambian los dias de mayor transaccion segun la region?
df %>%
  mutate(weekday = wday(InvoiceDate)) %>%
  mutate(weekday = factor(weekday, levels = c(1, 2, 3, 4, 5, 6, 7), 
                          labels = c("Domingo", "Lunes", "Martes", "Miercoles", 
                                     "Jueves", "Viernes", "Sabado"))) %>%
  filter(Region=="Northern Europe") %>%
  group_by(weekday) %>%
  distinct(InvoiceNo) %>%
  count()%>% 
  arrange(desc(n))
#Para todas las regiones 


#???Las ventas presentan alguna estacionalidad por mes?
df %>%
  mutate(Month = month(InvoiceDate)) %>%
  group_by(Month) %>%
  summarize(TotalQuantity = sum(Quantity))

df %>%
  mutate(Month = month(InvoiceDate)) %>%
  group_by(Month) %>%
  summarize(TotalSales = sum(Total))

#???Cuales son los dias de mayores ventas?
df %>%
  mutate(Month = month(InvoiceDate), Day = day(InvoiceDate)) %>%
  group_by(Month, Day) %>%
  summarize(TotalQuantity = sum(Quantity)) %>%
  
  df %>%
  mutate(Month = month(InvoiceDate), Day = day(InvoiceDate)) %>%
  group_by(Month, Day) %>%
  summarize(TotalSales = sum(Total)) %>%
  arrange(desc(TotalSales)) %>%
  top_n(10)

#Top 10 productos mas vendidos
invoice_lines %>%
  group_by(StockCode) %>%
  summarize(TotalQuantity = sum(Quantity)) %>%
  arrange(desc(TotalQuantity)) %>%
  top_n(10) %>%
  left_join(products, by = "StockCode")

invoice_lines %>%
  count(StockCode) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  left_join(products, by = "StockCode")

invoice_lines %>%
  group_by(StockCode) %>%
  summarize(TotalSales = sum(Total)) %>%
  arrange(desc(TotalSales)) %>%
  top_n(10) %>%
  left_join(products, by = "StockCode")

#???Cambia el top 10 de productos mas vendidos por region?
df %>%
  filter(Country %in% c('Germany', 'Australia')) %>%
  group_by(Country, StockCode) %>%
  summarize(TotalSales = sum(Total)) %>%
  top_n(10) %>%
  left_join(products, by = "StockCode") %>%
  arrange(Country, desc(TotalSales)) %>%
  ggplot(aes(x = TotalSales, y = Description, fill = Country)) +
  geom_col() +
  facet_grid(cols = vars(Country))


#==========================#
#        Clustering        #
#==========================#

clients <- df %>%
  group_by(CustomerID) %>%
  summarize(AvgPrice = mean(UnitPrice), AvgQuantity = mean(Quantity),
            Total = sum(Total))

clients <- df %>%
  group_by(InvoiceNo) %>%
  summarize(TotalInvoice = sum(Total)) %>%
  left_join(invoices, by = "InvoiceNo") %>%
  mutate(Weekend = wday(InvoiceDate, week_start = 1) < 6,
         UK = as.integer(Country == "United Kingdom")) %>%
  group_by(CustomerID) %>%
  summarize(Invoices = n(), AvgInvoice = mean(TotalInvoice),
            Weekend = mean(Weekend), UK = mean(UK),
            PerWeek = n() / as.numeric(difftime(max(InvoiceDate),
                                                min(InvoiceDate),
                                                units = "weeks"))) %>%
  mutate(PerWeek = ifelse(PerWeek == Inf, 0, PerWeek)) %>%
  left_join(clients, by = "CustomerID")

#Se usa scale por las escalan varían mucho entre ellas 
colSums(is.na(clients))
colSums(is.na(df))
clients <- na.omit(clients)
clients_scaled= scale(clients)
#hay 1 NA en customer ID y varios en DF 



# Uso map_dbl para correr varios modelos variando la k (los centroides)
tot_withinss <- map_dbl(2:10,  function(k){
  model <- kmeans(x = clients_scaled, centers = k)
  model$tot.withinss
})

# Genero un dataframe con el valor de k y de tot_withinss
elbow_df <- data.frame(
  k = 2:10 ,
  tot_withinss = tot_withinss
)

# Muestro la gráfica de codo
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)

# Uso map_dbl para correr varios modelos cambiando el valor de k
sil_width <- map_dbl(2:10,  function(k){
  model <- pam(x = clients_scaled, k = k)
  model$silinfo$avg.width
})
# Genero un dataframe que tenga el valor k y la silueta
sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)
# Grafico la relación entre k y la silueta
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)

set.seed(2)

# Construir un modelo k-means model con los datos de customers_spend con k=4
model_customers <- kmeans(clients_scaled, centers = 3)

# Extraer el vactor con los valores de clusters asignados
cluster_k <- as.data.frame(model_customers$cluster)
colnames(cluster_k) = "cluster_k"

# Generar un dataframe con los datos de los clientes y el cluster
segment_customers_k = bind_cols(clients, cluster_k) %>%
  mutate(cluster_k = as.factor(cluster_k))

# Calculamos el tamaño de cada cluster
Conteo_clusters1 = segment_customers_k %>%
  dplyr::group_by(cluster_k) %>%
  dplyr::summarise(conteo = n())

# Graficamos el resultado de los 3 clusters
clusplot(segment_customers_k, 
         segment_customers_k$cluster_k, 
         shape=TRUE, color=TRUE, labels=2, shade = T)


#==========================#
#    Association Rules     #
#==========================#

df$InvoiceNo = factor(df$InvoiceNo)
data_list = split(df$Description, df$InvoiceNo)
sales_transac = as(data_list, "transactions")

#modelo 1
sales_rules <- apriori(sales_transac,parameter = list(support = 0.01,
                                                      confidence = 0.15,
                                                      minlen = 2))
rules_df <- DATAFRAME(sales_rules)
View(rules_df %>%
       mutate(similarity = levenshteinSim(as.character(LHS), as.character(RHS))) %>%
       filter(similarity < 0.15) %>%
       arrange(desc(lift)))

