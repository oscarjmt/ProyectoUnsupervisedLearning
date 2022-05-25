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

#---------------------Juan Fer--------------------------------------------------

#???Cuales son los articulos que se compran en mayor cantidad?
en_mayor_cantidad<-df %>% 
  group_by(StockCode, Description)%>% 
  summarise(Quantity=sum(Quantity))%>% 
  arrange(desc(Quantity))
en_mayor_cantidad<-head(en_mayor_cantidad,10)

ggplot(en_mayor_cantidad) +
  geom_bar(aes(y=Quantity, x=reorder(Description, Quantity)),stat="identity", fill="steelblue")+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5))


df %>% group_by(StockCode, Description) %>% count() %>% arrange(desc(n))


#???Como varian las transacciones por dia de la semana?
trans_dia<-df %>%
  mutate(weekday = wday(InvoiceDate)) %>%
  mutate(weekday = factor(weekday, levels = c(1, 2, 3, 4, 5, 6, 7), 
                          labels = c("Domingo", "Lunes", "Martes", "Miercoles", 
                                     "Jueves", "Viernes", "Sabado"))) %>%
  group_by(weekday) %>%
  distinct(InvoiceNo)
           
ggplot(trans_dia, aes(weekday))+
  geom_bar(fill ="steelblue")+
  labs(title= "¿Cómo varían las transacciones por día de la semana?", x="Dia", y="Conteo")
# %>%count()%>% arrange(desc(n))           
  
df

#???Cambian los dias de mayor transaccion segun la region?

regiones<-c('Northern Europe','Western Europe','Central Europe', 'Southern Europe',
  'Eastern Europe','Middle East','East Asia','America','Other'
)
regiones

for(i in regiones){
  temp<-df %>%
    mutate(weekday = wday(InvoiceDate)) %>%
    mutate(weekday = factor(weekday, levels = c(1, 2, 3, 4, 5, 6, 7), 
                            labels = c("Domingo", "Lunes", "Martes", "Miercoles", 
                                       "Jueves", "Viernes", "Sabado"))) %>%
    filter(Region==i) %>%
    group_by(weekday) %>%
    distinct(InvoiceNo)
  print(ggplot(temp, aes(weekday))+
    geom_bar(fill ="steelblue")+
    labs(title= i, x="Dia", y="Conteo"))
  Sys.sleep(2)
}


#-------------------------------------------------------------------------------


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
  summarise(AvgPrice = mean(UnitPrice), AvgQuantity = mean(Quantity),
            Total = sum(Total))

clients <- df %>%
  group_by(InvoiceNo) %>%
  summarise(TotalInvoice = sum(Total)) %>%
  left_join(invoices, by = "InvoiceNo") %>%
  mutate(Weekend = wday(InvoiceDate, week_start = 1) < 6,
         UK = as.integer(Country == "United Kingdom")) %>%
  group_by(CustomerID) %>%
  summarise(Invoices = n(), AvgInvoice = mean(TotalInvoice),
            Weekend = mean(Weekend), UK = mean(UK),
            PerWeek = 1000*n() / as.numeric(difftime(max(InvoiceDate),
                                                min(InvoiceDate),
                                                units = "weeks"))) %>%
  mutate(PerWeek = ifelse(PerWeek == Inf, 0, PerWeek)) %>%
  left_join(clients, by = "CustomerID")

#Exploracion para observar datos atipicos y eliminarlos 
ggplot(clients, aes(y=Invoices))+
  geom_boxplot()+
  scale_y_log10()

ggplot(clients, aes(y=AvgInvoice))+
  geom_boxplot()+
  scale_y_log10()


ggplot(clients, aes(y=Weekend))+
  geom_boxplot()

ggplot(clients, aes(y=UK))+
  geom_boxplot()

ggplot(clients, aes(y=PerWeek))+
  geom_boxplot()+
  scale_y_log10()


ggplot(clients, aes(y=AvgPrice))+
  geom_boxplot()+
  scale_y_log10()

ggplot(clients, aes(y=AvgQuantity))+
  geom_boxplot()+
  scale_y_log10()

ggplot(clients, aes(y=Total))+
  geom_boxplot()+
  scale_y_log10()

#Se crean variables logaritmicas para las variables que necesitan esta escala 

clients$log_Invoices<-log(clients$Invoices)
clients$log_AvgInvoice<-log(clients$AvgInvoice)
clients$log_AvgPrice<-log(clients$AvgPrice)
clients$log_AvgQuantity<-log(clients$AvgQuantity)
clients$log_Total<-log(clients$Total)

#Se eliminan los outliers a traves de las variables creadas anteriormente 
clients<-clients[!clients$log_Invoices %in% boxplot.stats(clients$log_Invoices)$out,]
clients<-clients[!clients$log_AvgInvoice %in% boxplot.stats(clients$log_AvgInvoice)$out,]
clients<-clients[!clients$log_AvgPrice %in% boxplot.stats(clients$log_AvgPrice)$out,]
clients<-clients[!clients$log_AvgQuantity %in% boxplot.stats(clients$log_AvgQuantity)$out,]
clients<-clients[!clients$log_Total %in% boxplot.stats(clients$log_Total)$out,]
clients<-clients[!clients$Weekend %in% boxplot.stats(clients$Weekend)$out,]


#Se eliminan las variables log 
clients$log_AvgInvoice=NULL
clients$log_AvgPrice=NULL
clients$log_AvgQuantity=NULL
clients$log_Invoices=NULL
clients$log_Total=NULL

#comprobar que se estan eliminando
'''
length(boxplot(log(clients$Invoices))$out)
length(boxplot(log(clients$AvgInvoice))$out)
length(boxplot(log(clients$AvgPrice))$out)
length(boxplot(log(clients$AvgQuantity))$out)
length(boxplot(log(clients$Total))$out)
lenght(boxplot(log(clients$PerWeek))$out)
'''

#Se crea el df para la clusterización
clients_cluster<-clients%>%
  select(-CustomerID)



#Se usa scale por las escalan varían mucho entre ellas 


clients_scaled= scale(clients_cluster)




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
cluster_k%>%count(cluster_k)
# Generar un dataframe con los datos de los clientes y el cluster
segment_customers_k = bind_cols(clients_cluster, cluster_k) %>%
  mutate(cluster_k = as.factor(cluster_k))



# Calculamos el tamaño de cada cluster
Conteo_clusters1 = segment_customers_k %>%
  dplyr::group_by(cluster_k) %>%
  dplyr::summarise(conteo = n())

# Graficamos el resultado de los 3 clusters
clusplot(segment_customers_k, 
         segment_customers_k$cluster_k, 
         shape=TRUE, color=TRUE, labels=2, shade = T)

Resumen_K = segment_customers_k %>% 
  group_by(cluster_k) %>% 
  summarise_all(funs(mean(.)))

Resumen_K
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

