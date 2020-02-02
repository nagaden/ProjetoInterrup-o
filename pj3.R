library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
(sites <- data.frame(longitude = c(-80.144005, -80.109), latitude = c(26.479005, 26.83)))

#Mapa final da EDP concessao
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = edp2, fill = NA) + 
  geom_point(data = lista, aes(x = longitude, y = latitude), size = 1.5, 
             shape = 22, fill = "darkred") +
  coord_sf(xlim = c(-46.7, -44.8), ylim = c(-24, -22.4), expand = FALSE)

(sites <- data.frame(longitude = c(-46), latitude = c(-23.5)))

(sites1 <- data.frame(longitude = c(-46.523579), latitude = c(-23.459437)))
sites
lista

ggplot(lista) +
  geom_sf(data = edp2, fill = NA) + 
geom_point(aes(x = longitude, y = latitude), size = 1.5, 
           shape = 22, fill = "darkred") +
  coord_sf(xlim = c(-46.7, -44.8), ylim = c(-24, -22.4), expand = FALSE)

lista = rbind(sites, lista)

str(m)

#Agrupamento de reclamacoes por bairros
br =m %>%
  group_by(NOME_BAIRRO, NOME_MUNICIPIO) %>%
  summarise(qtd_call = n())

br = as.data.frame(br)

head(br)

#Filtro para retorno do primeiro registro de cada bairro, para obter apenas as coordenadas lat long
m %>%
select(CLIENTE_LAT,CLIENTE_LONG) %>%
dplyr::filter(m$NOME_BAIRRO == "AEROPORTO INTER GUARULHOS") %>%
  slice(1:1)

m %>%
  select(CLIENTE_LAT,CLIENTE_LONG) %>%
  dplyr::filter(m$NOME_BAIRRO == toString(nome[1,])) %>%
  slice(1:1)

toString(nome[1,])
nome

nrow(br)

brs = head(br)

head(brs)

br2 = dplyr::filter(br1, qtd_call > 300)
br2

nrow(br2)

nrow(br)

brs[,3:4]
names(brs)[3:4] = c("longitude", "latitude")



brs$CLIENTE_LAT  = cl[1,]

brs[1,]$CLIENTE_LAT

brs = select(brs, NOME_BAIRRO,qtd_call,CLIENTE_LONG,CLIENTE_LAT)

str(brs)


l1$longitude = as.numeric(l1$longitude)
l1$latitude = as.numeric(l1$latitude)

brs$longitude = as.numeric(brs$longitude)
brs$latitude = as.numeric(brs$latitude)

str(brs)

br2[order(br2$qtd_call),]

m %>%
dplyr::filter(m$NOME_BAIRRO == "CENTRO")

m %>%
  select(CLIENTE_LAT) %>%
  dplyr::filter(m$NOME_BAIRRO == "CENTRO" & m$NOME_MUNICIPIO == "SAO JOSE DOS CAMPOS") %>%
  slice(1:1)

#Plotar qtd de reclamacoes por bairros
ggplot(br2) +
  geom_sf(data = edp2, fill = NA) + 
  geom_point(aes(x = longitude, y = latitude, size = qtd_call, colour = factor(NOME_BAIRRO)),
             alpha=0.5) +
  scale_size(range = c(.1, 10), name="Qtd de reclamações")
 # coord_sf(xlim = c(-46.7, -44.8), ylim = c(-24, -22.4), expand = FALSE)

ggplot(br2) +
  geom_sf(data = edp2, fill = NA) + 
  geom_point(aes(x = longitude, y = latitude, size = qtd_call),
             alpha=0.5) +
  scale_size(range = c(.1, 10), name="Qtd de reclamações")


flcities <- data.frame(state = rep("Florida", 5), city = c("Miami", 
                                                           "Tampa", "Orlando", "Jacksonville", "Sarasota"))
flcities

br3 %>%
dplyr::filter(br3$NOME_MUNICIPIO == "PINDAMONHANGABA")

head(br3)

cl  = m %>%
  select(CLIENTE_LAT) %>%
  dplyr::filter(m$NOME_BAIRRO == toString(nome[1,]) & m$NOME_MUNICIPIO == toString(nome[1,])) %>%
  slice(1:1)

cl[1,]
br1[1,]

br1[1,]$latitude = cl[1,]

cl[1,]

br1$latitude = 0
br1$longitude = 0

br1[1,]$longitude = cl[1,]


head(br1)

head(br)

br1 = br

br1$longitude = 0

head(br1)

br3 = dplyr::filter(br1, qtd_call < 300)

nrow(br2)

br2

for(i in 1:nrow(br1)){
  
nb =  br1[i,] %>%
    select(NOME_BAIRRO)

nm =  br1[i,] %>%
  select(NOME_MUNICIPIO)
  
cl  = m %>%
  select(CLIENTE_LAT) %>%
  dplyr::filter(m$NOME_BAIRRO == toString(nb[1,])  & m$NOME_MUNICIPIO == toString(nm[1,]) ) %>%
  slice(1:1)

br1[i,]$latitude = as.numeric(cl[1,])

co  = m %>%
  select(CLIENTE_LONG) %>%
  dplyr::filter(m$NOME_BAIRRO == toString(nb[1,]) & m$NOME_MUNICIPIO == toString(nm[1,]) ) %>%
  slice(1:1)

br1[i,]$longitude = as.numeric(co[1,])
  
}

warnings()

head(br1)


c = m %>%
  select(CLIENTE_LAT) %>%
  dplyr::filter(m$NOME_BAIRRO == toString(nome[1,])) %>%
  slice(1:1)

c[1,]

ggplot(lista) +
geom_point(aes(x = longitude, y = latitude))


l = as_tibble(lista)

str(lista)

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
states
states <- cbind(states, st_coordinates(st_centroid(lista)))

lista

head(lista)

lista = rbind(sites, sites1)
lista

str(lista)

st_centroid(lista)

library("maps")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)

head(edp2$geometry, edp2$NOMEMUNICP)

 d = select(edp2, geometry, NOMEMUNICP)
head(d)

head(lista)
lista = as.data.frame(lista)
str(lista)

head(m)

m$CLIENTE_LONG =  paste(m$V15, ".", m$V16, sep = "")

head(m)

lista = select(lista, CLIENTE_LONG, CLIENTE_LAT)
names(lista)[1:2] = c("longitude", "latitude")


str(lista)
head(lista)
