library(tidyverse)
library(mFilter)
library(countrycode)
library(lmtest)
library(sandwich)
library(car)
library(stargazer)
library(changepoint)
library(forecast)
library(fpp3)
library(directlabels)
library(patchwork)
library(ggrepel)
library(GGally)
library(fixest)
library(ggResidpanel)
library(nlme)
library(frontier)
library(patchwork)
theme_set(theme_bw(base_size = 21))
options(scipen = 999)



# =========== ANÁLISIS PIB POTENCIAL Y PIB REAL =================

# todo esto son puras limpiezas
pibbc <- readxl::read_excel("PIB chained.xlsx", skip = 1)
colnames(pibbc) <- c("anio", "pib")
pibbc$anio <- str_sub(pibbc$anio, 1, 4) %>% as.numeric()
pibbc$pib <- pibbc$pib * 1000000000
pobla <- readxl::read_excel("pobla.xlsx", skip = 1)
colnames(pobla) <- c("anio", "poblacion")
pobla$anio <-  str_sub(pobla$anio, 1, 4) %>% as.numeric()
pibbcpc <- pibbc %>% 
  left_join(pobla) %>% 
  as.data.frame()
pibbcpc$pc <- pibbcpc$pib/pibbcpc$poblacion
pibbcpc$deltapc <- (pibbcpc$pc - lag(pibbcpc$pc))/lag(pibbcpc$pc)


# desde acá el análisis
pibts <- ts(pibbcpc$pc, start = 1989, end = 2022)
base <- tslm(pibts ~ trend)
model <- lm(pc ~ lag(pc)+lag(pc, 2), pibbcpc)

# explorando calidad del modelo por regresión
coeftest(model, vcov = vcovHC, type = "HC3")
acf(model$residuals)
dwt(model, lag.max = 1)

# sacando los Pib potenciales
pibbcpc$base <- fitted(base)
pibbcpc$potencial1[3:34] <- fitted(model)
pibbcpc$hp1 <- hpfilter(pibbcpc$pc, freq = 100)[[2]]

# gráfico de pib potencial
ggplot() +
  aes(x = pibbcpc$anio) +
  geom_line(aes(y = pibbcpc$base/1000, color = "Potencial 1")) +
  geom_line(aes(y = pibbcpc$potencial1/1000, color = "Potencial 2")) +
  geom_line(aes(y = pibbcpc$hp1/1000, color = "HP")) +
  geom_line(aes(y = pibbcpc$pc/1000, color = "Real"), size = 1.2) +
  scale_x_continuous(breaks = seq(1990, 2022, 1),
                     expand = c(0,0)) +
  scale_y_continuous(labels = scales::dollar,
                     breaks = seq(3500, 11000, 500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8, .3),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) +
  labs(title = "Crecimiento real y Potencial",
       subtitle = "PIBs per cápita a precios encadenados, en millones de pesos")
scales::dollar(10339757)


# esto que está comentado es para obtener una gráfica del "efecto garfio"
# c1 <- pibbcpc %>% 
#   filter(anio >= 1990, anio <= 1999) %>% 
#   pull(pc)
# 
# c2 <- pibbcpc %>% 
#   filter(anio > 1999, anio<=2009) %>% 
#   pull(pc)
# 
# c3 <- pibbcpc %>% 
#   filter(anio > 2009, anio <= 2019) %>% 
#   pull(pc)
# 
# d <- data.frame(i = rep(1:10, 3),
#                 pib = c(c1, c2, c3),
#                 tp = c(rep("1990-1999", 10),
#                        rep("2000-2009", 10),
#                        rep("2009-2019", 10)))
# 
# 
# d %>% 
#   ggplot() +
#   aes(x=i, pib, color = tp) +
#   guides(color = "none") +
#   geom_line(linewidth = 1.2) +
#   geom_dl(aes(label = tp), 
#           method = list(dl.trans(x = x + -4, y = y + .9), 
#                         "last.points", cex = 1.5)) +
#   scale_y_continuous(labels = scales::dollar,
#                      limits = c(3500000, 10200000),
#                      breaks = seq(3500000, 10200000, 500000)) +
#   scale_x_continuous(breaks = 1:10) +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.title = element_blank(),
#         legend.position = c(.8, .3)) +
#   labs(title = "PIB per cápita chileno desde 1990")



# estimando los quiebres de la serie
ts.rate <- ts(pibbcpc$deltapc[2:34], start = 1990, end = 2022)
quiebre <- cpt.mean(ts.rate*100, penalty = "BIC", method = "BinSeg")

1990+24

# asignar los quiebres, se obtienen del objeto "quiebre", solo basta
# sumar los números que da al año base, y se tiene el año de quiebre
pibbcpc$quiebres <- case_when(
  pibbcpc$anio <= 1998 ~ 1,
  pibbcpc$anio <= 2000 ~ 2,
  pibbcpc$anio <= 2014 ~ 3,
  T~ 4
)

# agrupar por quiebres
pibbcpc <- pibbcpc %>% 
  group_by(quiebres) %>% 
  mutate(avgdelta = mean(deltapc, na.rm = T))


# gráfica del ritmo de la tasa de crecimiento
ggplot() +
  aes(x = pibbcpc$anio) +
  geom_line(aes(y = (pibbcpc$hp1-lag(pibbcpc$hp1))/pibbcpc$hp1, 
                color = "HP")) +
  geom_line(aes(y = pibbcpc$deltapc, color = "Real"), size = 1.2) +
  geom_line(data = pibbcpc %>% filter(anio <= 1998),
            aes(x = anio, y = avgdelta, color = "Quiebres"), 
            color = "black", size = 1) +
  geom_line(data = pibbcpc %>% filter(anio > 1998 & anio <= 2000),
            aes(x = anio, y = avgdelta, color = "Quiebres"), 
            color = "black", size = 1) +
  geom_line(data = pibbcpc %>% filter(anio > 2000 & anio <= 2014),
            aes(x = anio, y = avgdelta, color = "Quiebres"), 
            color = "black", size = 1) +
  geom_line(data = pibbcpc %>% filter(anio > 2014),
            aes(x = anio, y = avgdelta, color = "Quiebres"), 
            color = "black", size = 1) +
  scale_x_continuous(breaks = seq(1990, 2022, 1),
                     expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8, .85),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) +
  labs(title = "Tasa crecimiento real y potencial",
       subtitle = "Tasa de variación YoY, y quiebre estructural (linea recta)",
       caption = "Quiebre estructural obtenido por segmentación binaria, HP es filtro Hodrick-Prescott λ=100.\n@sientifiko1")


# ========== COMPARACIÓN INTERNACIONAL ===================

# leer el archivo
bm <- readxl::read_excel("API_NY.GDP.PCAP.PP.KD_DS2_en_excel_v2_6299184.xls") %>% 
  gather("año", "pibpc", 3:35) %>% 
  mutate(año = as.numeric(año))

# asignar una región
bm$region <- countrycode(bm$iso,
                         origin = "iso3c",
                         destination = "region23")

# filtro remover no países
bm <- bm %>% 
  filter(!is.na(region))

# DESDE AQUÍ LAS GRÁFICAS

# Tendencia del PIB global
bm %>% 
  group_by(año) %>% 
  summarise(q2 = quantile(pibpc, .25,na.rm = T),
            q3 = quantile(pibpc, .5,na.rm = T),
            q4 = quantile(pibpc, .75,na.rm = T)) %>% 
  left_join(
    bm %>% 
      filter(iso == "CHL") %>% 
      select(año, pibchile = pibpc),
    by = "año"
  ) %>% 
  ggplot() +
  aes(x= año) +
  geom_line(aes(y = q3), linewidth = 1.2, color = "black") +
  geom_ribbon(aes(ymax = q4, ymin = q2),alpha = .3, color = "grey") +
  geom_line(aes(y = pibchile), color = "red", alpha = .3, linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1990, 2022, 1),
                     expand = c(0,0)) +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8, .85),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) +
  labs(title = "Tendencia del PIB global",
       subtitle = "PIB per cápita PPC, USD constantes del 2017",
       caption = "Línea recta es PIB mediano global, bandas representan percentiles .25 y .75 respectivamente, en rojo el PIB de Chile\n@sientifiko1")

# Filtro para identificar a los más competitivos del mundo
temp <- bm %>% 
  left_join(
    bm %>% 
      group_by(año) %>% 
      summarise(q4 = quantile(pibpc, .75,na.rm = T)),
    by = "año"
  ) %>% 
  filter(pibpc >= q4) %>% 
  arrange(iso, año)

# PIB global de sudamérica
bm %>% 
  filter(region == "South America") %>% 
  group_by(año) %>% 
  summarise(q2 = quantile(pibpc, .25,na.rm = T),
            q3 = quantile(pibpc, .5,na.rm = T),
            q4 = quantile(pibpc, .75,na.rm = T)) %>% 
  left_join(
    bm %>% 
      filter(iso == "CHL") %>% 
      select(año, pibchile = pibpc),
    by = "año"
  ) %>% 
  ggplot() +
  aes(x= año) +
  geom_line(aes(y = q3), linewidth = 1.2, color = "black") +
  geom_ribbon(aes(ymax = q4, ymin = q2),alpha = .3, color = "grey") +
  geom_line(aes(y = pibchile), color = "red", alpha = .3, linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1990, 2022, 1),
                     expand = c(0,0)) +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8, .85),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) +
  labs(title = "Tendencia del PIB sudaméricano",
       subtitle = "PIB per cápita PPC, USD constantes del 2017",
       caption = "Línea recta es PIB mediano global, bandas representan percentiles .25 y .75 respectivamente, en rojo el PIB de Chile\n@sientifiko1")

# filtro para seleccionar a los más competitivos de sudamérica
temp <- bm %>% 
  filter(region == "South America") %>% 
  left_join(
    bm %>% 
      filter(region == "South America") %>% 
      group_by(año) %>% 
      summarise(q4 = quantile(pibpc, .75,na.rm = T)),
    by = "año"
  ) %>% 
  filter(pibpc >= q4) %>% 
  arrange(iso, año)


# PIB global de américa latina y el caribe
bm %>% 
  filter(region%in%c("South America", 
                     "Central America", "Caribbean")) %>% 
  group_by(año) %>% 
  summarise(q2 = quantile(pibpc, .25,na.rm = T),
            q3 = quantile(pibpc, .5,na.rm = T),
            q4 = quantile(pibpc, .75,na.rm = T)) %>% 
  left_join(
    bm %>% 
      filter(iso == "CHL") %>% 
      select(año, pibchile = pibpc),
    by = "año"
  ) %>% 
  ggplot() +
  aes(x= año) +
  geom_line(aes(y = q3), linewidth = 1.2, color = "black") +
  geom_ribbon(aes(ymax = q4, ymin = q2),alpha = .3, color = "grey") +
  geom_line(aes(y = pibchile), color = "red", alpha = .3, linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1990, 2022, 1),
                     expand = c(0,0)) +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8, .85),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) +
  labs(title = "Tendencia del PIB LATAM y el caribe",
       subtitle = "PIB per cápita PPC, USD constantes del 2017",
       caption = "Línea recta es PIB mediano global, bandas representan percentiles .25 y .75 respectivamente, en rojo el PIB de Chile\n@sientifiko1")


# identificar países más competitivos de estas regiones
temp <- bm %>% 
  filter(region%in%c("South America", 
                     "Central America", "Caribbean")) %>% 
  left_join(
    bm %>% 
      filter(region%in%c("South America", 
                         "Central America", "Caribbean")) %>% 
      group_by(año) %>% 
      summarise(q4 = quantile(pibpc, .75,na.rm = T)),
    by = "año"
  ) %>% 
  filter(pibpc >= q4) %>% 
  arrange(iso, año)


# inspección gráfica de por qué el vecindario electo
bm %>% 
  filter(año == 1990,
         pibpc <= 15000) %>% 
  ggplot() +
  aes(reorder(iso, pibpc),
      pibpc, 
      color = ifelse(iso=="CHL", "es chile","no es chile")) +
  guides(color = "none") +
  geom_point()+
  scale_y_continuous(breaks = seq(0, 100000, 500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8, .85),
        axis.text.x = element_text(angle = 90, 
                                   hjust = 0, vjust = .5,
                                   size = 1))

# seleccionando la banda del vecindario
tempvecindario <- bm %>% 
  filter(año == 1990,
         pibpc >= 8000,
         pibpc <= 11500) %>% 
  pull(iso)

# serie de tiempo del vecindario de chile
bm %>% 
  filter(iso %in% tempvecindario) %>% 
  ggplot() +
  aes(año, pibpc, color = name) +
  guides(color = "none") +
  geom_line(linewidth = 1.2) +
  geom_dl(aes(label = iso), 
          method = list(dl.trans(x = x + -1, y = y), 
                        "last.points", cex = .9)) +
  scale_x_continuous(breaks = seq(1990, 2022, 1),
                     expand = c(0,0)) +
  # scale_y_continuous(trans = "log10") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8, .85),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5))


# Crecimiento de países del "vecindario" de Chile en los 90
(bm %>% 
  filter(iso %in% tempvecindario,
         año %in% c(1990, 2015)) %>% 
  spread(año, pibpc) %>% 
  mutate(tasa = (`2015` - `1990`)/`1990`) %>% 
  ggplot() +
  aes(`1990`, tasa, color = (iso == "CHL")) +
  guides(color = "none") +
  geom_jitter() +
  geom_text_repel(aes(label = iso)) +
  scale_x_continuous(breaks = seq(8000, 11500, 250)) +
  scale_y_continuous(labels =scales::percent,
                     limits = c(0, 2.25)) +
  theme(axis.text.x = element_text(size =11)) +
  labs(x = "PIB pc en 1990",
       y = "Tasa de crecimiento",
       subtitle = "Tasa calculada entre 1990 a 2015")) +
(bm %>% 
  filter(iso %in% tempvecindario,
         año %in% c(1990, 2022)) %>% 
  spread(año, pibpc) %>% 
  mutate(tasa = (`2022` - `1990`)/`1990`) %>% 
  ggplot() +
  aes(`1990`, tasa, color = (iso == "CHL")) +
  guides(color = "none") +
  geom_jitter() +
  geom_text_repel(aes(label = iso)) +
  scale_x_continuous(breaks = seq(8000, 11500, 250)) +
  scale_y_continuous(labels =scales::percent,
                     limits = c(0, 2.25)) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size =11)) +
  labs(x = "PIB pc en 1990",
       subtitle = "Tasa calculada entre 1990 a 2022")) +
  plot_annotation(title = "Convergencia países de PIB similar a Chile")



# crecimiento Malaisia vs Chile
bm %>% 
  filter(iso %in% c("MYS", "CHL")) %>% 
  ggplot() +
  aes(año, pibpc, color = name) +
  guides(color = "none") +
  geom_line(linewidth = 1.2) +
  geom_dl(aes(label = iso), 
          method = list(dl.trans(x = x+1, y = y+.75), 
                        "first.points", cex = .9)) +
  scale_x_continuous(breaks = seq(1990, 2022, 1),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(9000, 29000, 1000),
                     labels = scales::dollar) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8, .85),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) +
  labs(title = "Malasia vs Chile",
       subtitle = "PIB per cápita PPC a USD constantes del 2017")


# cierre brecha con respecto a países en desarrollo más competitivos
bm %>% 
  filter(name %in% c("Chile", "Panama", "Puerto Rico",
                     "Malaysia", "Uruguay", "Trinidad and Tobago")) %>% 
  left_join(
    bm %>% 
      filter(iso == "USA") %>% 
      select(año, pibusa = pibpc)
  ) %>% 
  mutate(frontera = pibpc/pibusa) %>% 
  ggplot() +
  aes(año, frontera, color = iso) +
  guides(color = "none") +
  geom_line(linewidth = 1.2)+
  geom_dl(aes(label = iso), 
          method = list(dl.trans(x = x-1, y = y), 
                        "last.points", cex = .9), 
          color = "black") +
  scale_x_continuous(breaks = seq(1990, 2022, 1),
                     expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8, .85),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) +
  labs(title = "Catch up con Estados Unidos",
       subtitle = "Ratio de PIB pc con respecto al PIB pc de Estados Unidos")


# cierre brecha con respecto a UK
bm %>% 
  filter(name %in% c("Chile", "Panama", "Puerto Rico",
                     "Malaysia", "Uruguay", "Trinidad and Tobago")) %>% 
  left_join(
    bm %>% 
      filter(iso == "GBR") %>% # cambiar el país acá
      select(año, pibrico = pibpc)
  ) %>% 
  mutate(frontera = pibpc/pibrico) %>% 
  ggplot() +
  aes(año, frontera, color = iso) +
  guides(color = "none") +
  geom_line(linewidth = 1.2)+
  geom_dl(aes(label = iso), 
          method = list(dl.trans(x = x-1, y = y), 
                        "last.points", cex = .9), 
          color = "black") +
  scale_x_continuous(breaks = seq(1990, 2022, 1),
                     expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8, .85),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) +
  labs(title = "Catch up con Reino Unido",
       subtitle = "Ratio de PIB pc con respecto al PIB pc de Reino Unido")


# cierre brecha cono sur
bm %>% 
  filter(name %in% c("Chile", "Uruguay", "Argentina",
                     "Bolivia", "Peru", "Paraguay")) %>% 
  left_join(
    bm %>% 
      filter(iso == "USA") %>% 
      select(año, pibrico = pibpc)
  ) %>% 
  mutate(frontera = pibpc/pibrico) %>% 
  ggplot() +
  aes(año, frontera, color = iso) +
  guides(color = "none") +
  geom_line(linewidth = 1.2)+
  geom_dl(aes(label = iso), 
          method = list(dl.trans(x = x-1, y = y), 
                        "last.points", cex = .9), 
          color = "black") +
  scale_x_continuous(breaks = seq(1990, 2022, 1),
                     expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8, .85),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) +
  labs(title = "Catch up con Estados Unidos",
       subtitle = "Ratio de PIB pc con respecto al PIB pc de Estados Unidos")



# ======== CHINA ====================

bm <- readxl::read_excel("API_NY.GDP.PCAP.PP.KD_DS2_en_excel_v2_6299184.xls") %>% 
  gather("año", "pibpc", 3:35) %>% 
  mutate(año = as.numeric(año))

# CORRELACIÓN SUPER CHINA
bm %>% 
  filter(name %in% c("Chile", "Uruguay", "Argentina",
                     "Bolivia", "Peru", "Paraguay", "China"),
         año < 2012) %>% 
  select(año, name, pibpc) %>% 
  spread(name, pibpc) %>% 
  select(2:ncol(.)) %>% 
  ggpairs(diag = list(continuous = "blankDiag")) +
  theme(strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 7),
        axis.text.y = element_text(size = 7))

# correlación china bajo
bm %>% 
  filter(name %in% c("Chile", "Uruguay", "Argentina",
                     "Bolivia", "Peru", "Paraguay", "China"),
         año > 2014) %>% 
  select(año, name, pibpc) %>% 
  spread(name, pibpc) %>% 
  select(2:ncol(.)) %>% 
  ggpairs(diag = list(continuous = "blankDiag")) +
  theme(strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 7),
        axis.text.y = element_text(size = 7))

  labs(title = "Asociación PIB China y Conosur",
       subtitle = "Desde 2014 en adelante")


# Pibs de Chile y China
bm %>% 
  filter(name %in% c("Chile", "China")) %>% 
  group_by(name) %>% 
  mutate(rate = log(pibpc/lag(pibpc))) %>% 
  ggplot() +
  aes(año, rate, color = name) +
  guides(color = "none") +
  geom_line(linewidth = 1.2) +
  geom_dl(aes(label = name), 
          method = list(dl.trans(x = x+2.5, y = y+2.7), 
                        "first.points", cex = 1.5)) +
  scale_x_continuous(breaks = seq(1990, 2022, 1),
                     expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) 


# ROL DE IMPORTACIONES
chinam <- readxl::read_excel("importaciones.xls", skip = 2) %>% 
  filter(`Country Code` == "CHN") %>% 
  gather("anio","imports",5:ncol(.)) %>% 
  mutate(anio = as.numeric(anio)) %>% 
  filter(anio >= 1990) %>% 
  select(anio, imports)

# asociación importaciones y crecimiento
bm %>% 
  filter(name %in% c("Chile")) %>% 
  group_by(name) %>% 
  mutate(rate = log(pibpc/lag(pibpc))) %>% 
  left_join(
    chinam, by = c("año" = "anio")
  ) %>% 
  mutate(ratem = log(imports/lag(imports)),
         id = row_number()) %>% 
  mutate(lagratem = lag(ratem),
         lagpib = lag(rate))-> tempimpocl

tempimpocl %>% 
  ggplot() +
  aes(x = año) +
  # guides(color = "none") +
  geom_line(aes(y=rate, color = "PIB Chile"),linewidth = 1.2) +
  geom_line(aes(y=ratem, color = "Impo China"),linewidth = 1.2) +
  # geom_dl(aes(label = name), 
  #         method = list(dl.trans(x = x+1.7, y = y+2.7), 
  #                       "first.points", cex = .9)) +
  scale_x_continuous(breaks = seq(1990, 2022, 1),
                     expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5),
        legend.position = c(.8, .8),
        legend.title = element_blank()) 

# MODELO Y DIAGNÓSTICO
model1 <- feols(rate ~ ratem, data = tempimpocl)
model2 <- feols(rate ~ ratem + lagratem, data = tempimpocl)
model3 <- feols(rate ~ ratem + lagratem + lagpib, data = tempimpocl)

model1lm <- lm(rate ~ ratem, data = tempimpocl)
model2lm <- lm(rate ~ ratem + lagratem, data = tempimpocl)
model3lm <- lm(rate ~ ratem + lagratem + lagpib, data = tempimpocl)

etable(model1, model2, model3, vcov = vcovHAC)

resid_panel(model1lm)
resid_panel(model2lm)

ggPacf(model1$residuals)
ggAcf(model1$residuals)
ggPacf(model2$residuals)
ggAcf(model2$residuals)

ggplot() +
  aes(x= 1991:2022) +
  geom_line(aes(y = tempimpocl$rate[2:33], color = "Observado")) +
  geom_line(aes(y = model1$fitted.values, color = "Predicho"))


# probando la misma asociación antes de 1990
chinam2 <- readxl::read_excel("importaciones.xls", skip = 2) %>% 
  filter(`Country Code` == "CHN") %>% 
  gather("anio","imports",5:ncol(.)) %>% 
  mutate(anio = as.numeric(anio)) %>% 
  select(anio, imports)

datpib <- readxl::read_excel("pwt1001.xlsx", sheet = 3) %>% 
  filter(country == "Chile") %>% 
  select(4, 5, 7) %>% 
  mutate(pibpc = rgdpe/pop) %>% 
  mutate(rate = log(pibpc/lag(pibpc)))

datpib %>% 
  left_join(chinam2, 
            by = c("year"="anio")) %>% 
  filter(year >= 1960, year <= 1990) %>% 
  mutate(rate = log(pibpc/lag(pibpc)),
         ratem = log(imports/lag(imports))) %>% 
  mutate(lagratem = lag(ratem),
         lagpib = lag(rate))-> temp2

model1alt <- feols(rate ~ ratem, data = temp2)
model2alt <- feols(rate ~ ratem + lagratem, data = temp2)
model3alt <- feols(rate ~ ratem + lagratem + lagpib, data = temp2)

etable(model1alt, model2alt, model3alt, vcov = vcovHAC)


# ================ PRODUCTIVIDAD ===============

pwtdat <- readxl::read_excel("pwt1001.xlsx", sheet = 3)
pwtdat$prod <- (pwtdat$rgdpe/pwtdat$avh)/pwtdat$emp
# pwtdat$prod <- (pwtdat$rgdpe/pwtdat$emp)/pwtdat$avh
# pwtdat$prod <- pwtdat$rgdpe/(pwtdat$emp*pwtdat$avh)
pwtdat$region <- countrycode(pwtdat$countrycode,
                             origin = "iso3c",
                             destination = "region23")

# productividad global
pwtdat %>% 
  filter(!(country == "Chile")) %>% 
  group_by(year) %>% 
  summarise(q2 = quantile(prod, .25,na.rm = T),
            q3 = quantile(prod, .5,na.rm = T),
            q4 = quantile(prod, .75,na.rm = T)) %>% 
  left_join(
    pwtdat %>% 
      filter(country == "Chile") %>% 
      select(year, prodchile = prod),
    by = "year"
  ) %>% 
  ggplot() +
  aes(x= year) +
  geom_line(aes(y = q3), linewidth = 1.2, color = "black") +
  geom_ribbon(aes(ymax = q4, ymin = q2),alpha = .3, color = "grey") +
  geom_line(aes(y = prodchile), color = "red", alpha = .3, linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1990, 2019, 1),
                     limits = c(1990, 2019),
                     expand = c(0,0)) +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8, .85),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5),
        plot.caption = element_text(size = 15)) +
  labs(title = "Tendencia productividad global",
       subtitle = "PIB por hora media de trabajo anual por trabajador",
       caption = "Línea negra es productividad mediana global, bandas representan percentiles .25 y .75 respectivamente, en rojo productividad de Chile\n@sientifiko1")

# productividad de américa
pwtdat %>% 
    filter(region%in%c("South America", "Central America", "Caribbean"),
           !(country == "Chile")) %>% 
    group_by(year) %>% 
    summarise(q2 = quantile(prod, .25,na.rm = T),
              q3 = quantile(prod, .5,na.rm = T),
              q4 = quantile(prod, .75,na.rm = T)) %>% 
    left_join(
      pwtdat %>% 
        filter(country == "Chile") %>% 
        select(year, prodchile = prod),
      by = "year"
    ) %>% 
  ggplot() +
    aes(x= year) +
    geom_line(aes(y = q3), linewidth = 1.2, color = "black") +
    geom_ribbon(aes(ymax = q4, ymin = q2),alpha = .3, color = "grey") +
    geom_line(aes(y = prodchile), color = "red", alpha = .3, linewidth = 1.2) +
    scale_x_continuous(breaks = seq(1990, 2019, 1),
                       limits = c(1990, 2019),
                       expand = c(0,0)) +
    scale_y_continuous(labels = scales::dollar) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.8, .85),
          axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5),
          plot.caption = element_text(size = 15)) +
    labs(title = "Tendencia productividad LATAM y el Caribe",
         subtitle = "PIB por hora media de trabajo anual por trabajador",
         caption = "Línea negra es productividad mediana latina, bandas representan percentiles .25 y .75 respectivamente, en rojo productividad de Chile\n@sientifiko1")
  


 # productividad a los más competitivos
 pwtdat %>% 
   filter(country %in% c("Chile", "Panama", "Puerto Rico",
                         "Uruguay", "Trinidad and Tobago")) %>% 
   select(year, country, countrycode, prod) %>% 
    ggplot() +
    aes(year, prod, color = country) +
    guides(color = "none") +
    geom_line(linewidth = 1.2) +
    geom_dl(aes(label = countrycode), 
             method = list(dl.trans(x = x-1, y = y), 
                           "last.points", cex = .9), 
             color = "black") +
    scale_x_continuous(breaks = seq(1990, 2019, 1),
                       limits = c(1990, 2019),
                       expand = c(0,0)) +
    scale_y_continuous(labels = scales::dollar) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.8, .85),
          axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5),
          plot.caption = element_text(size = 15)) +
    labs(title = "Productividad de los latinos más competitivos",
         subtitle = "PIB por hora media de trabajo anual por trabajador",
         caption = "@sientifiko1")


# output por trabajador
pwtdat %>% 
      filter(country %in% c("Chile", "Panama", "Puerto Rico",
                            "Uruguay", "Trinidad and Tobago")) %>% 
    ggplot() +
      aes(year, rgdpe/emp, color = country) +
      guides(color = "none") +
      geom_line(linewidth = 1.2) +
      geom_dl(aes(label = countrycode), 
              method = list(dl.trans(x = x-1, y = y), 
                            "last.points", cex = .9), 
              color = "black") +
      scale_x_continuous(breaks = seq(1990, 2019, 1),
                         limits = c(1990, 2019),
                         expand = c(0,0)) +
      scale_y_continuous(labels = scales::dollar) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank(),
            legend.position = c(.8, .85),
            axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5),
            plot.caption = element_text(size = 15)) +
      labs(title = "Productividad de los latinos más competitivos",
           subtitle = "PIB por trabajador",
           caption = "@sientifiko1")
    
countrycode("Costa Rica",
            "country.name",
            "iso3c")

  
# cierre brecha productividad con respecto a USA 
pwtdat %>% 
  select(countrycode, country, year, prod) %>% 
  filter(countrycode %in% c("CHL", "URY", "MYS", "SGP", "KOR", 
                            "CHN", "CRI")) %>% 
  left_join(
    pwtdat %>% 
      filter(countrycode == "USA") %>% 
      select(year, produsa = prod)
  ) %>% 
  mutate(frontera = prod/produsa) %>% 
  ggplot() +
  aes(year, frontera, color = countrycode) +
  guides(color = "none") +
  geom_line(linewidth = 1.2)+
  geom_dl(aes(label = countrycode), 
          method = list(dl.trans(x = x-1, y = y), 
                        "last.points", cex = .9), 
          color = "black") +
  scale_x_continuous(breaks = seq(1990, 2019, 1),
                     expand = c(0,0),
                     limits = c(1990, 2019)) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8, .85),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) +
  labs(title = "Catch up con Estados Unidos",
       subtitle = "Ratio de productividad con respecto a EEUU")
  



# =============== LA PANDEMIA ==============


# leer el archivo
bm <- readxl::read_excel("API_NY.GDP.PCAP.PP.KD_DS2_en_excel_v2_6299184.xls") %>% 
  gather("año", "pibpc", 3:35) %>% 
  mutate(año = as.numeric(año))

# asignar una región
bm$region <- countrycode(bm$iso,
                         origin = "iso3c",
                         destination = "region23")

# filtro remover no países
bm <- bm %>% 
  filter(!is.na(region))

# DESDE AQUÍ LAS GRÁFICAS

# Tendencia del PIB global por pandemia
bm %>% 
  group_by(año) %>% 
  summarise(q2 = quantile(pibpc, .25,na.rm = T),
            q3 = quantile(pibpc, .5,na.rm = T),
            q4 = quantile(pibpc, .75,na.rm = T)) %>% 
  left_join(
    bm %>% 
      filter(iso == "CHL") %>% 
      select(año, pibchile = pibpc),
    by = "año"
  ) %>% 
  ggplot() +
  aes(x= año) +
  geom_line(aes(y = q3), linewidth = 1.2, color = "black") +
  geom_ribbon(aes(ymax = q4, ymin = q2),alpha = .3, color = "grey") +
  geom_line(aes(y = pibchile), color = "red", alpha = .3, linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1990, 2022, 1),
                     expand = c(0,0), 
                     limits = c(2015, 2022)) +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8, .85),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) +
  labs(title = "El efecto global de la Pandemia",
       subtitle = "PIB per cápita PPC, USD constantes del 2017",
       caption = "Línea recta es PIB mediano global, bandas representan percentiles .25 y .75 respectivamente, en rojo el PIB de Chile\n@sientifiko1")

# calculando los que más caen y mejor se recuperan
bm %>% 
  filter(año %in% c(2018, 2019, 2020, 2022)) %>% 
  mutate(año = paste0("y", año)) %>% 
  spread(año, pibpc) -> recuperacion

recuperacion$cae <- (recuperacion$y2020 - recuperacion$y2019)/recuperacion$y2019
recuperacion$sube <- (recuperacion$y2022-recuperacion$y2020)/recuperacion$y2020

# los que más caen
recuperacion %>% 
  na.omit() %>% 
  arrange(cae) %>% 
  head(20) %>% 
  ggplot() +
  aes(reorder(name, -cae), cae, fill = name) +
  guides(fill = "none") +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(subtitle = str_wrap("Top 20 países más afectados por la Pandemia", 25),
       caption = "Tasa variación 2019-2020") +
  # los que mejor se recuperan
  recuperacion %>% 
  na.omit() %>% 
  arrange(desc(sube)) %>% 
  head(20) %>% 
  ggplot() +
  aes(reorder(name, sube), sube, fill = name) +
  guides(fill = "none") +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(subtitle = str_wrap("Top 20 países que mejor se recuperan post Pandemia", 25),
       caption = "Tasa variación 2020-2022")


recuperacion %>% 
  filter(name == "Chile")

nrow(recuperacion %>% filter(cae < -0.0741))
nrow(recuperacion %>% filter(sube > 0.127))



recuperacion %>% 
  ggplot() +
  aes(cae, sube) +
  guides(color = "none") +
  geom_point() +
  geom_point(data = recuperacion %>% filter(name == "Chile"),
              mapping = aes(color = (name == "Chile")), size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  geom_text_repel(data = recuperacion %>% filter(name == "Chile"),
                  mapping = aes(label = name),
                  max.overlaps = 1000,
                  nudge_y = .3) +
  labs(x="Tasa caída 2019-2020",
       y="Tasa recuperación 2020-2022",
       title = "Derrumbe y recuperación mundial pandémica")

# sacando la esperanza condicional
set <- recuperacion %>% na.omit() 

esperada1 <- feols(sube ~ log(y2019) + cae, data = set,
                   panel.id = ~region)
esperada2 <- feols(sube ~ log(y2019) + cae + region, 
                   data = set,
                  panel.id = ~region)

etable(esperada1, esperada2, vcov = "cluster")

# esto de acá es pa estudiar los residuales
# esperada <- lm(sube ~ log(y2019) + cae, data = set)
# resid_panel(esperada) vcovHC(esperada, "HC1")


set$pred <- fitted(esperada2)
set$worspred <- fitted(esperada1)
# set$predlow <- predict(esperada1, vcov = vcovHC(esperada, "HC3"),
#                        interval = "confidence")$ci_low  
# set$predhigh <- predict(esperada1, vcov = vcovHC(esperada, "HC3"), 
#                         interval = "confidence")$ci_high 
set$predlow <- predict(esperada2, vcov = "cluster",
                       interval = "confidence")$ci_low
set$predhigh <- predict(esperada2, vcov = "cluster",
                        interval = "confidence")$ci_high

# Sacando el error cuadrado medio de cada modelo
mean((set$sube - set$pred)**2)
mean((set$sube - set$worspred)**2)

# GRAFICANDO la esperanza condicional
set %>% 
  ggplot() +
  aes(pred, sube, ymax = pred+predhigh, ymin = pred-predlow) +
  guides(color = "none") +
  geom_point() +
  geom_point(data = set %>% filter(name == "Chile"),
             mapping = aes(color = (name == "Chile")), size = 3) +
  geom_ribbon(alpha = .3) +
  # geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(labels = scales::percent,
                     limits = c(-.069, .21),
                     breaks = seq(-.069, .21, .02)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(-.179, 1),
                     breaks = seq(-.179, 1, .1)) +
  # scale_x_continuous(labels = scales::percent) +
  # scale_y_continuous(labels = scales::percent) +
  geom_text_repel(data = set %>% filter(name == "Chile"),
                  mapping = aes(label = name),
                  max.overlaps = 1000,
                  nudge_y = .3) +
  labs(x = "Crecimiento predicho",
       y="Crecimiento observado",
       title = "Potencial de recuperación post pandemia")


# explorando que pasa en América del Sur
set <- recuperacion %>% na.omit() 

set %>% 
  filter(region == "South America") %>% 
  ggplot() +
  aes(reorder(name, sube), sube) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(.01, 1, .05)) +
  theme(axis.title.y = element_blank()) +
  labs(y = "Tasa de recuperación 2020-2022",
       title = "Recuperación en América del Sur")

# set %>% 
#   filter(region == "South America") %>% 
#   ggplot() +
#   aes(log(y2019), sube, color = (name == "Chile")) +
#   guides(color = "none") +
#   geom_point()

# Re estimar recuperación sin guyana
set2 <- set %>% filter(name != "Guyana")

temp <- feols(sube ~ log(y2019) + cae + region, 
                   data = set2,
                   panel.id = ~region)

set2$pred <- temp$fitted.values

# obtener la media regional de recuperación sin Guyana
set2 %>% 
  filter(region == "South America") %>% 
  pull(sube) %>% 
  mean()

# =========== OTRAS GRÁFICAS Y EXPLORACIONES =============

# IMPORTACIÓN DE COBRE REFINADO
impocobre <- readxl::read_excel("datos valor importaciones Cobre China de Chile.xlsx")

bm %>% 
  filter(name %in% c("Chile")) %>% 
  group_by(name) %>% 
  mutate(rate = log(pibpc/lag(pibpc))) %>% 
  left_join(
    impocobre %>% 
      select(Period, tradevalue = `Trade Value (US$)`),
    by = c("año" = "Period")
  ) %>% 
  # group_by(año) %>% 
  mutate(ratem = log(tradevalue/lag(tradevalue)),
         ratem2 = (tradevalue-lag(tradevalue))/lag(tradevalue)) -> temp

temp %>% 
  ggplot() +
  aes(x = año) +
  geom_line(aes(y = ratem2, color = "Impo"), linewidth = 1.2)


# vinculo cobre refinado y crecimiento
model <- feols(rate ~ ratem, data = temp)
etable(model, vcov = vcovHAC)


# quiebre estructural de las importaciones chinas
chinam <- readxl::read_excel("importaciones.xls", skip = 2) %>% 
  filter(`Country Code` == "CHN") %>% 
  gather("anio","imports",5:ncol(.)) %>% 
  mutate(anio = as.numeric(anio)) %>% 
  filter(anio >= 1989)


chinam$rate <- (chinam$imports - lag(chinam$imports))/lag(chinam$imports)
chinam$rate2 <- log(chinam$imports/lag(chinam$imports))

tsratechn <- ts(chinam$rate[2:34], start = 1990, end = 2022)
tslogchn <- ts(log(chinam$imports), start = 1989, end = 2022)
tsretchn <- ts(chinam$rate2[2:34], start = 1990, end = 2022)

br <- cpt.mean(tsratechn*100, method = "BinSeg", penalty = "BIC", Q=4)
br2 <- cpt.mean(tslogchn, method = "BinSeg", penalty = "BIC", Q=5)
br3 <- cpt.mean(tsretchn*100, method = "BinSeg", penalty = "BIC", Q=4)

br
br2
br3


plot(br)
plot(br2)
plot(br3)

1989+15


# PRECIO DEL COBRE
cooper <- readxl::read_excel("1.3_Price-of-refined-copper-Yearly.xlsx",
                             sheet = 2)

# pibcoop <- pibbcpc %>% 
#   left_join(cooper)
# pibcoop$precio
# 
# pibcoop %>% 
#   filter(anio >= 1990) %>% 
#   ggplot() +
#   aes(x = anio) +
#   geom_line(aes(y = pc/first(pc), color = "PIBpc")) +
#   geom_line(aes(y = precio/first(precio), color = "P. cobre"))


# Extensión horas de trabajo de Korea
pwtdat <- readxl::read_excel("pwt1001.xlsx", sheet = 3)

pwtdat %>% 
  filter(country %in% c("Republic of Korea", "Singapore",
                       "Japan", "China", "Taiwan")) %>% 
  ggplot() +
  aes(year, avh, color = country) +
  geom_line(linewidth = 1.2)


pwtdat %>% 
  filter(country %in% c("Republic of Korea", "Singapore",
                        "Japan", "China", "Taiwan")) %>% 
  ggplot() +
  aes(year, rgdpe/pop, color = country) +
  geom_line(linewidth = 1.2)


pwtdat %>% 
  filter(year == 2019) %>% 
  ggplot() +
  aes(rgdpe/pop, avh, color = (country == "Republic of Korea")) + 
  guides(color = "none") +
  geom_jitter()


# probando un modelo de estimación de eficiencia productiva
setreg <- pwtdat %>% 
  filter(year >= 1990,
         !is.na(rgdpna/pop),
         !is.na(avh),
         !is.na(hc),
         !is.na(rdana),
         !is.na(rnna),
         !is.na(labsh)) %>% 
  select(countrycode, year, rgdpna, pop,
         avh, hc, rdana, rnna, labsh)


sf_model <- sfa(log(rgdpna/pop) ~ log(avh) + log(hc) + log(rdana/pop) +
                  log(rnna/pop) + log(labsh) | countrycode + year, 
                data = setreg)


summary(sf_model)

setreg$eff <- efficiencies(sf_model)
