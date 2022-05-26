library(tidyverse)
library(readxl)
library(lubridate)
library(maps)

#setwd("D:/Google Drive/UVG/V Semestre/Data Mining/ProyectoUnsupervisedLearning")
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


ggplot(df, aes(x=InvoiceDate, fill=Region)) + geom_histogram()

df$Total = df$UnitPrice * df$Quantity

ventas_region <- as.data.frame(
  df %>% group_by(Region) %>% summarize (total_invoice = sum(Total)) %>% 
    arrange(desc(total_invoice)) %>% 
    mutate(porcentage = (total_invoice/sum(total_invoice))))

ventas_pais <- as.data.frame(
  df %>% group_by(Country) %>% summarize (total_invoice = sum(Total)) %>% 
    arrange(desc(total_invoice)) %>% 
    mutate(porcentage = (total_invoice/sum(total_invoice))))

# 93% ventas son de Europa Occidental
# 84.6% son de UK

#¿Los precios se comportan diferentes según la región?
df %>% group_by(Region, StockCode) %>% summarize(precio_promedio = mean(UnitPrice), 
                                                 unidades = n()) %>% arrange(desc(unidades)) %>%
  filter(StockCode %in% c('85123A', '85099B','22423')) %>% print(n=22)

#Los precios si varian por lo general mas caros en Europa Occidental

#¿La cantidad de productos diferentes que compran los clientes varían por región?
df %>% group_by(Region) %>% distinct(StockCode) %>% count() %>% arrange(desc(n))
df %>% group_by(Country) %>% distinct(StockCode) %>% count() %>% arrange(desc(n))

#Existe una mayor disponibilidad  de productos en Europa Occidental
#La cantidad de SKU en UK es significativa


#3a Ventas por pais
dfgeo <- df
dfgeo$Country[df$Country == "United Kingdom"] <- "UK"
dfgeo$Country[df$Country == "Channel Islands"] <- "UK"
dfgeo$Country[df$Country == "EIRE"] <- "Ireland"
dfgeo$Country[df$Country == "RSA"] <- "South Africa"
dfgeo$Country[df$Country == "Hong Kong"] <- "China"


mapdata <- map_data("world") ##ggplot2
geo_ventas <- as.data.frame(dfgeo %>% group_by(Country) %>% 
                              summarize (total_invoice =sum(Total)) %>% 
                              arrange(desc(total_invoice)))
mapdata <- left_join(mapdata, geo_ventas, by=c("region"="Country"))
mapdata1 <- mapdata %>% filter (!is.na(mapdata$total))
map1 <- ggplot(mapdata1, aes(x=long, y = lat, group=group)) + 
  geom_polygon(aes(fill = total_invoice), color = "black") + 
  ggtitle("Ventas Totales") + labs(fill="Ventas Totales GPB")
map1

#3b Precio de compra promedio
geo_venta_promedio <- as.data.frame(dfgeo %>% group_by(InvoiceNo, Country) %>% 
                                      summarise(venta = sum(Total)) %>%
                                      group_by(Country) %>%
                                      summarize(venta_promedio = mean(venta)) %>%
                                    arrange(desc(venta_promedio)))
#geo_venta_promedio$venta_promedio <- round(geo_venta_promedio$venta_promedio,0)

mapdata2 <- map_data("world")
mapdata2 <- left_join(mapdata2, geo_venta_promedio, by=c("region" = "Country")) %>% 
  filter (!is.na(mapdata$total))
map2 <- ggplot(mapdata2, aes(x=long, y = lat, group=group)) + 
  geom_polygon(aes(fill = venta_promedio), color = "black") + 
  ggtitle("Ventas Promedio") + labs(fill = "Venta Promedio GBP") +
  scale_fill_gradient( low = 'green', high='red') + theme_dark()
map2

#3c Cantidad de Clientes
geo_clientes <- as.data.frame(dfgeo %>% distinct(CustomerID, Country) %>%
                                group_by(Country) %>% count() %>%
                                arrange(desc(n)))

mapdata3 <- map_data("world")
mapdata3 <- left_join(mapdata3, geo_clientes, by=c("region" = "Country")) %>% 
  filter (!is.na(mapdata$total))
map3 <- ggplot(mapdata3, aes(x=long, y = lat, group=group)) + 
  geom_polygon(aes(fill = factor(n)), color = "black") + ggtitle("Total Clientes") +
  theme_light() + labs(fill="No. Clientes")
map3


#3d Cantidad de Productos
geo_sku <- as.data.frame(dfgeo %>% distinct(StockCode, Country) %>%
                                group_by(Country) %>% count() %>%
                                arrange(desc(n)))

mapdata4 <- map_data("world")
mapdata4 <- left_join(mapdata4, geo_sku, by=c("region" = "Country")) %>% 
  filter (!is.na(mapdata$total))
map4 <- ggplot(mapdata4, aes(x=long, y = lat, group=group)) + 
  geom_polygon(aes(fill = factor(n)), color = "black") +
  ggtitle("Total de Productos") + labs(fill="Productos") +
  theme_classic()
map4
