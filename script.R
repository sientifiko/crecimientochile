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
theme_set(theme_bw(base_size = 21))
options(scipen = 999)


# ========================================
# ANÁLISIS PIB POTENCIAL Y PIB REAL

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


# =======================================
# COMPARACIÓN INTERNACIONAL

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

