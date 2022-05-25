library(tidyverse)
library(readxl)
library(lubridate)
library(cluster)
library(arules)
library(RecordLinkage)


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
    select(StockCode, Quantity, UsnitPrice, InvoiceNo) %>%
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

#¿Los clientes son recurrentes o solo compran en una ocasion?
ggplot(invoices, aes(x=InvoiceDate, fill=Country)) +
    geom_histogram()
invoices %>% distinct(Country) %>% print(n=38)
invoices %>% count(CustomerID) %>% summarize(mean(n))
    #el cliente tipico como entre 4 a 5 veces en el periodo analizado

#¿Cuanto se vende en total por cada region?
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

#¿Los precios se comportan diferentes segun la region?
df %>% group_by(Region, StockCode) %>% summarize(precio_promedio = mean(UnitPrice), 
                                                 unidades = n()) %>% arrange(desc(unidades)) %>%
    filter(StockCode %in% c('85123A', '85099B','22423')) %>% print(n=22)

#¿La cantidad de productos diferentes que compran los clientes varian por region?
df %>% group_by(Region) %>% distinct(StockCode) %>% count() %>% arrange(desc(n))


#¿Cuales son los articulos que se compran en mayor cantidad?
#¿Como varian las transacciones por dia de la semana?
#¿Cambian los dias de mayor transaccion segun la region?

#¿Las ventas presentan alguna estacionalidad por mes?
df %>%
    mutate(Month = month(InvoiceDate)) %>%
    group_by(Month) %>%
    summarize(TotalQuantity = sum(Quantity))

df %>%
    mutate(Month = month(InvoiceDate)) %>%
    group_by(Month) %>%
    summarize(TotalSales = sum(Total))

#¿Cuales son los dias de mayores ventas?
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

#¿Cambia el top 10 de productos mas vendidos por region?
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

