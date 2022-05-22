library(tidyverse)
library(readxl)
library(lubridate)

setwd("D:/Google Drive/UVG/V Semestre/Data Mining/ProyectoUnsupervisedLearning")
df <- read_excel("Online Retail.xlsx") %>%
  mutate(StockCode = toupper(StockCode)) %>%
  filter(Quantity > 0, UnitPrice > 0)

#Tablas normalizadas BCNF
invoices <- df %>%
  distinct(InvoiceNo, InvoiceDate, CustomerID, Country) %>%
  distinct(InvoiceNo, .keep_all=TRUE)

invoice_lines <- df %>%
  select(StockCode, Quantity, UnitPrice, InvoiceNo)

products <- df %>%
  distinct(StockCode, Description) %>%
  distinct(StockCode, .keep_all=TRUE)

#Dataframe completo sin inconsitencias
df <- invoice_lines %>%
  left_join(invoices, on="InvoiceNo") %>%
  left_join(products, on="StockCode")

#¿Los clientes son recurrentes o solo compran en una ocasión?
ggplot(invoices, aes(x=InvoiceDate, fill=Country)) + geom_histogram()
invoices %>% distinct(Country) %>% print(n=38)
invoices %>% count(CustomerID) %>% summarize(mean(n))
# el cliente tipico como entre 4 a 5 veces en el periodo analizado

#¿Cuanto se vende en total por cada región?
#Regiones
WesternEurope = c('United Kingdom','France','Switzerland','Netherlands','EIRE', 'Belgium', 
                  'Channel Islands')
NorthernEurope = c('Norway','Sweden','Finland', 'Iceland', 'Denmark')
CentralEurope = c('Germany', 'Austria', 'Czech Republic')
SouthernEurope = c('Italy', 'Greece', 'Cyprus', 'Malta', 'Spain', 'Portugal')
EasternEurope = c('Poland', 'Lithuania')
MiddleEast = c('Bahrain', 'Israel', 'Lebanon', 'United Arab Emirates', 'Saudi Arabia')
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
# 93% ventas son de Europa Occidental
# 84.6% son de UK

#¿Los precios se comportan diferentes según la región?
df %>% group_by(Region, StockCode) %>% summarize(precio_promedio = mean(UnitPrice), 
                                                 unidades = n()) %>% arrange(desc(unidades)) %>%
  filter(StockCode %in% c('85123A', '85099B','22423')) %>% print(n=22)

#Los precios si varian por lo general mas caros en Europa Occidental

#¿La cantidad de productos diferentes que compran los clientes varían por región?
df %>% group_by(Region) %>% distinct(StockCode) %>% count() %>% arrange(desc(n))
#Existe una mayor variacion de productos en Europa Occidental
