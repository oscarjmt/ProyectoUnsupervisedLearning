library(tidyverse)
library(readxl)
library(lubridate)

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

#View(products %>% count(Description) %>% filter(n > 1) %>% inner_join(products, by="Description"))
#View(products %>% count(StockCode) %>% filter(n > 1) %>% inner_join(products, by="StockCode"))
#View(df %>% filter(Description == "damaged"))
#View(df %>% filter(Quantity <= 0))

#¿Los clientes son recurrentes o solo compran en una ocasión?
#¿Cuanto se vende en total por cada región?
#¿Los precios se comportan diferentes según la región?
#¿La cantidad de productos diferentes que compran los clientes varían por región?

#¿Cuales son los articulos que se compran en mayor cantidad?
#¿Como varian las transacciones por dia de la semana?
#¿Cambian los días de mayor transacción según la región?

#¿Las ventas presentan alguna estacionalidad por mes?    
#¿Cuales son los días de mayores ventas?
#Top 10 productos más vendidos
#¿Cambia el 10 de productos más vendidos por región?

