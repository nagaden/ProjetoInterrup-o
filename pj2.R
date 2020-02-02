#AIzaSyBqFVn9Q--2UIqF8VybhX5ROmbZg3c4kTs
register_google(key = "AIzaSyBqFVn9Q--2UIqF8VybhX5ROmbZg3c4kTs") 
key = "AIzaSyBqFVn9Q--2UIqF8VybhX5ROmbZg3c4kTs"
register_google(key, account_type, client, signature, second_limit,
                day_limit, write = FALSE)

showing_key()
google_key()
library(ggplot2)
has_google_key()
ggmap_show_api_key()

install.packages("mapview")
library(mapview)
library(ggmap)
install.packages("devtools")
library(devtools)
devtools::install_github("dkahle/ggmap")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
install.packages("rlang")


#### UTILIZAR A PARTIR DESSE PONTO QUANDO TIVER OS DADOS DE 2016 ########
# 1 AGRUPAR OS DADOS DO ANO CORRENTE
# 2 MESCLAR OS DADOS UTILIZANDO RBIND
# 3 GERAR O TS A PARTIR DESSE MERGE

m = inter

str(m)

tail(m)
str(m)


mano = table(m$ANO,m$MES )
mano = as.data.frame(mano)
mano = select(mano,-c(Var1))

#Duplicando apenas para printar o decompose. Quando obter os dados reais de 2016 não sera necessário isso
mano2 = mano
tm = rbind(mano, mano2)
names(tm)[1:2] = c("Mes", "Qtd")

#Gerando a série temporal
x5 <-ts(tm$Qtd,start=c(2015),freq=12)

#Decompose da série temporal
plot(decompose(x5))

#Plot da previsão
plot(forecast(x5))

x6 = forecast(x5)

summary(x6)

plot(x6)

m$NOME_MUNICIPIO
bairro

bairro = m %>%
  group_by(NOME_BAIRRO) %>%
  summarise(qtd_call = n())

bairro[order(bairro$qtd_call),]

str(inter)
  head(dplyr::filter(inter, NOME_MUNICIPIO == "GUARULHOS"))
  
  
 get_map(location = "Europe")

#Agrupando as reclamacoes dos bairros do municipio de guarulhos
qtd = m %>%
  group_by(NOME_BAIRRO, MES) %>%
  dplyr::filter(NOME_MUNICIPIO == "GUARULHOS") %>%
  summarise(qtd_call = n())

#Agrupando todas reclamacoes de Guarulhos
qtd = m %>%
  group_by(MES) %>%
  dplyr::filter(NOME_MUNICIPIO == "GUARULHOS") %>%
  summarise(qtd_call = n())
  
qtd[order(-qtd$qtd_call),]

qtd

qtd[order(qtd$MES),]

str(m)

head(m)
lista = select(y,CLIENTE_LONG,CLIENTE_LAT)


head(y)
str(y)

head(select(y, CLIENTE_LAT,CLIENTE_LONG,V15,V16))
y = m

#Tratamento para concatenar as colunas e formar de forma correta lat/long
y$CLIENTE_LAT = paste(m$CLIENTE_LAT,".",m$CLIENTE_LONG, sep="")

head(y$CLIENTE_LONG)

capture.output(cat('hi',1))

#PEGANDO O BAIRRO COM MAIOR QTD DE RECLAMACOES DO MUNICIPIO DE GUARULHOS
cs = qtd %>%
dplyr::filter(NOME_BAIRRO == "CIDADE SOBERANA")

plot(qtd)

qts = cs[,-1]

qts = as.data.frame(qts)

qts2 = qts
qm = rbind(qts, qts2)
qm

qs <-ts(qm$qtd_call,start=c(2015),freq=12)

qs

#Metodos alternativos para previsao

n5 = naive(x5, h = 12)

summary(n5)

s5 = ses(x5, h = 12)

summary(s5)

h5 = holt(x5, h = 12)

summary(h5)

ar5 = auto.arima(x5)

summary(ar5)

tb5 = tbats(x5)

summary(tb5)

x6$mean
x6$level
x6$x
tm


summary(x6)


ano = table(inter$ANO,inter$MES )
ano = as.data.frame(ano)
ano = select(ano,-c(Var1))
str(ano)

ano


names(ano)[1:2] = c("Mes", "Qtd")


ano2 = ano
a4 = rbind(ano,ano2)

x4 <-ts(a4$Qtd,start=c(2015),freq=12)

plot(x4)

x4

dc = decompose(x4)
plot(dc, col = "red")


hw = HoltWinters(x4, beta = FALSE, gamma = FALSE)

hw$coefficients

hw$fitted

x = forecast(x4)

x$fitted
x$residuals

summary(x)
plot(forecast(x4))

summary(x4)

hw$SSE

plot(hw)

plot.forecast(hw)



head(m)

#Agrupando todas reclamacoes do ano por bairro
f = table(m$NOME_BAIRRO, m$ANO)
#transformando para df
f = as.data.frame(f)
#ordenando pela quantidade
f = f[order(-f$Freq),]

loc = read2.csv("data_interrupt2.csv")

l = read.csv2("data_interrupt2.csv")
l
plot_brmap(edp)


head(edp)

names(edp)



municipios <- get_brmap(geo = "City", geo.filter = list(Region = 5),
                        class = "SpatialPolygonsDataFrame")
municipios <- join_data(municipios, pop2017, by = c("City" = "mun"))
plot_brmap(municipios)

plot_brmap(map, data_to_join = data.frame(), join_by = NULL,
           var = "values", theme = theme_map())

map_sul <- get_brmap(geo = "City", geo.filter = list(Region = 4))
mapa1 <- plot_brmap(map_sul,
                    data_to_join = pop2017,
                    join_by = c("City" = "mun"),
                    var = "pop2017")

mapa1 +
  labs(title = "População Municipal 2017 - Região Sul")

head(map_sul)
head(pop2017)

rio_map <- get_brmap(geo = "City",
                     geo.filter = list(State = 35),
                     class = "sf")
plot_brmap(rio_map)

gr = rio_map[rio_map$nome == "GUARULHOS",]

plot_brmap(gr)

str(rio_map$nome)

ab = rio_map[rio_map$nome == "ABADIA DE GOIÁS",]
ab


d1 = st_read("LAYER_DISTRITO", stringsAsFactors = FALSE)
head(d1)
tail(d1)

plot(d1)
plot_brmap(d1)
