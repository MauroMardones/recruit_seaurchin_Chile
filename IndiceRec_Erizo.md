---
title: "Indice de Reclutamiento  Erizo X y XI regiones "
subtitle: "Análisis complementario Seguimiento Bentónico 1996-2022"
author: "Mauricio Mardones. Inv. Depto Ev. Recursos. IFOP"
date:  "06 October, 2023"
#bibliography: LBSPR.bib
#csl: apa.csl
link-citations: yes
linkcolor: blue
output:
  bookdown::html_document2:
    fig_caption: yes
    keep_md: true
    toc: true
    toc_deep: 3
    toc_float:
      collapsed: false
      smooth_scroll: false
    theme: cosmo
    fontsize: 0.9em
    linestretch: 1.7
    html-math-method: katex
    self-contained: true
    code-tools: true
editor_options: 
  markdown: 
    wrap: 72
---


# Set de trabajo y librerías utilizadas


```r
rm(list = ls())
knitr::opts_chunk$set(message = FALSE,
                      warning = FALSE,
                      fig.align = 'center',
                      fig.pos = "H",
                      dev = 'jpeg',
                      dpi = 300,
                      fig.path="Figuras/")
#XQuartz is a mess, put this in your onload to default to cairo instead
options(bitmapType = "cairo") 
# (https://github.com/tidyverse/ggplot2/issues/2655)
# Lo mapas se hacen mas rapido
```



```r
options(bitmapType = "cairo") 
#XQuartz is a mess, put this in your onload to default to cairo instead (https://github.com/tidyverse/ggplot2/issues/2655)
# Lo mapas se hacen mas rapido
# solo para IOs
```



```r
library(GGally)
library(knitr)
library(tidyverse)
library(patchwork)
library(marmap)
library(mapproj)
library(maps)
library(raster)
library(knitr)
library(ggrepel)
library(sf)
library(ggthemes) # nuevos fondos
library(readxl)
library(performance)
library(ggridges)
library(see)
```




# Tallas estaciones fijas


```r
EstFij <- read_csv2("Talla_EstFi_20011_2018.csv")
EstFij2 <- as.data.frame(EstFij)
```

## Exploración


```r
jz3 <- ggplot(EstFij2 %>% 
                drop_na(), aes(x=talla_mm, y = as.factor(year), 
                      fill = factor(stat(quantile))))+
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
     quantiles = c(0.10, 0.90)) +
  scale_fill_manual(
    name = "Probabilidad", values = c("#de2d26", "#fee0d2", "#de2d26"),
    labels = c("[0, 0.10]", "[0.10, 0.90]", "[0.90, 1]"))+
  facet_wrap(.~sector, ncol=8) +   
  geom_vline(xintercept = 65, color = "red")+
  scale_x_continuous(breaks = seq(from = 40, to = 120, by = 10))+
  scale_y_discrete(breaks = seq(from = 1996, to = 2022, by = 5))+
  #scale_fill_viridis_d(name="SubArea")+
  theme_few()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlim(10,120)+
  xlab("Longitud (cm.)")+
  ylab("")
jz3
```

<img src="Figuras/unnamed-chunk-3-1.jpeg" style="display: block; margin: auto;" />
Lo mismo por poligono


```r
jz4 <- ggplot(EstFij2 %>% 
                drop_na(), aes(x=talla_mm, y = as.factor(year), 
                      fill = factor(stat(quantile))))+
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.10, 0.90),
    jittered_points = TRUE,
    alpha=0.1) +
  scale_fill_manual(
    name = "Probabilidad", values = c("#de2d26", "#fee0d2", "#de2d26"),
    labels = c("[0, 0.10]", "[0.10, 0.90]", "[0.90, 1]"))+
  facet_wrap(.~poligono, ncol=5) +   
  geom_vline(xintercept = 65, color = "red")+
  scale_x_continuous(breaks = seq(from = 40, to = 120, by = 10))+
  scale_y_discrete(breaks = seq(from = 1996, to = 2022, by = 5))+
  #scale_fill_viridis_d(name="SubArea")+
  theme_few()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlim(10,120)+
  xlab("Longitud (cm.)")+
  ylab("")
jz4
```

<img src="Figuras/unnamed-chunk-4-1.jpeg" style="display: block; margin: auto;" />
Lo mismo por zona


```r
jz5 <- ggplot(EstFij2 %>% 
                drop_na(), aes(x=talla_mm, y = as.factor(year), 
                      fill = factor(stat(quantile))))+
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.10, 0.90),
    jittered_points = TRUE,
    alpha=0.1,
    rel_min_height = 0.05) +
  scale_fill_manual(
    name = "Probabilidad", values = c("#de2d26", "#fee0d2", "#de2d26"),
    labels = c("[0, 0.10]", "[0.10, 0.90]", "[0.90, 1]"))+
  facet_wrap(.~macrozona, ncol=3) +   
  geom_vline(xintercept = 65, color = "red")+
  scale_x_continuous(breaks = seq(from = 40, to = 120, by = 10))+
  scale_y_discrete(breaks = seq(from = 1996, to = 2022, by = 1))+
  #scale_fill_viridis_d(name="SubArea")+
  theme_few()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlim(10,120)+
  xlab("Longitud (cm.)")+
  ylab("")
jz5
```

<img src="Figuras/unnamed-chunk-5-1.jpeg" style="display: block; margin: auto;" />
Calculo cuantiles por grupo


```r
# Calcular cuantiles por grupo
cuantiles_por_zona <- tapply(EstFij2$talla_mm, 
                              EstFij2$macrozona,
                              function(x) quantile(x, c(0.10, 0.5, 0.90)))
cuantiles_por_poligono <- tapply(EstFij2$talla_mm, 
                              EstFij2$poligono,
                              function(x) quantile(x, c(0.10, 0.5, 0.90)))
cuantiles_por_año <- tapply(EstFij2$talla_mm, 
                              EstFij2$year,
                              function(x) quantile(x, c(0.10, 0.5, 0.90)))

print(cuantiles_por_zona)
```

```
## $`X Norte`
## 10% 50% 90% 
##  22  45  63 
## 
## $`X Sur`
## 10% 50% 90% 
##  21  38  57 
## 
## $XI
## 10% 50% 90% 
##  23  46  66
```

```r
print(cuantiles_por_poligono)
```

```
## $`1`
## 10% 50% 90% 
##  20  34  53 
## 
## $`2`
## 10% 50% 90% 
##  33  55  66 
## 
## $`4`
##  10%  50%  90% 
## 16.0 28.5 54.0 
## 
## $`5`
## 10% 50% 90% 
##  18  38  57 
## 
## $`6`
## 10% 50% 90% 
##  24  39  57 
## 
## $`7`
## 10% 50% 90% 
##  26  50  67 
## 
## $`8`
## 10% 50% 90% 
##  24  41  63 
## 
## $`11`
##  10%  50%  90% 
## 21.0 32.0 62.4 
## 
## $`12`
## 10% 50% 90% 
##  19  39  65
```

```r
print(cuantiles_por_año)
```

```
## $`2011`
## 10% 50% 90% 
##  41  58  70 
## 
## $`2012`
## 10% 50% 90% 
##  23  32  51 
## 
## $`2013`
## 10% 50% 90% 
##  20  41  63 
## 
## $`2014`
## 10% 50% 90% 
##  15  31  57 
## 
## $`2015`
## 10% 50% 90% 
##  19  33  55 
## 
## $`2016`
## 10% 50% 90% 
##  24  46  63 
## 
## $`2017`
## 10% 50% 90% 
##  28  45  63 
## 
## $`2018`
## 10% 50% 90% 
##  22  40  61
```



Una vez definidos los quantiles de los muestreos poblacionales (Estaciones Fijas), se identifica que valores de entre `54 y 46 mm ` son identificados como un porcentaje de ingreso a la fraccion eplotable. 

Calculo el promedio del cuantil `90` por zona.


```r
mean(54, 38, 46)
```

```
## [1] 54
```

# Calculo Indice Reclutamiento

## Tallas Seguimiento


```r
load("talla2022.Rdata")
```



```r
talla2022exp <- talla2022 %>% 
  drop_na() %>% 
  type.convert(as.is = TRUE) %>% 
  uncount(FRECUENCIA)

colSums(is.na(talla2022exp))
```

```
##        PROCED      POLIGONO POLIGONO_IFOP             X        REGION 
##             0             0             0             0             0 
##        PUERTO       ANO_ARR       MES_ARR       DIA_ARR       FUNCION 
##             0             0             0             0             0 
##       CAPTURA       DESTINO        PESO_M      LONGITUD          ZONA 
##             0             0             0             0             0
```


```r
tallasegpo <- ggplot(talla2022exp %>% 
                drop_na(), aes(x=LONGITUD, y = as.factor(ANO_ARR), 
                      fill = factor(stat(quantile))))+
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
     quantiles = c(0.10, 0.90)) +
  scale_fill_manual(
    name = "Probabilidad", values = c("#3182bd", "#deebf7", "#3182bd"),
    labels = c("[0, 0.10]", "[0.10, 0.90]", "[0.90, 1]"))+
  facet_wrap(.~POLIGONO, ncol=6) +   
  geom_vline(xintercept = 65, color = "red")+
  scale_x_continuous(breaks = seq(from = 40, to = 120, by = 10))+
  scale_y_discrete(breaks = seq(from = 1996, to = 2022, by = 5))+
  #scale_fill_viridis_d(name="SubArea")+
  theme_few()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlim(10,120)+
  xlab("Longitud (cm.)")+
  ylab("")
tallasegpo
```

<img src="Figuras/unnamed-chunk-10-1.jpeg" style="display: block; margin: auto;" />


El promedio de un 10% de los quantiles de entrada a la fracción explotable es de `56 mm`.

Ahora identifico los distintos cuantiles de los datos de pesquería y estaciones


```r
cuantil_10pesq <- quantile(talla2022exp$LONGITUD, 0.10)
cuantil_10est <- quantile(EstFij2$talla_mm, 0.90)
```



```r
indice_reclutamiento <- talla2022exp %>%
  filter(LONGITUD<cuantil_10est) %>% 
  group_by(ANO_ARR, MES_ARR, POLIGONO, ZONA, POLIGONO_IFOP) %>%
  summarize(PROP = n() / nrow(talla2022exp)) %>% 
  mutate(PROPLOG =log(PROP))
# Crear gráficos en facet_wrap de barras para representar el índice de reclutamiento
```
Veo los datos crudos  con la linea como media del cuantil de los datos

```r
indseg <- ggplot(indice_reclutamiento , 
       aes(x = factor(ANO_ARR), 
           y = PROP)) +
  geom_boxplot() +
  facet_wrap(POLIGONO~., ncol=4) +
  scale_x_discrete(breaks = seq(from = 1996, to = 2022, by = 4))+
  theme_few()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
   labs(x = "ANO", 
        y = "Índice de Reclutamiento")+
  ylim(0, 0.001)
indseg
```

<img src="Figuras/unnamed-chunk-13-1.jpeg" style="display: block; margin: auto;" />

Veo los datos normalizados con la linea como media del cuantil de los datos

```r
indseg2 <- ggplot(indice_reclutamiento , 
       aes(x = factor(ANO_ARR), 
           y = PROPLOG)) +
  geom_boxplot() +
  facet_wrap(POLIGONO~., ncol=4) +
  geom_hline(yintercept = quantile(indice_reclutamiento$PROPLOG, 0.5), 
             color = "blue")+
  scale_x_discrete(breaks = seq(from = 1996, to = 2022, by = 4))+
  theme_few()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
   labs(x = "ANO", 
        y = "Índice de Reclutamiento")
indseg2
```

<img src="Figuras/unnamed-chunk-14-1.jpeg" style="display: block; margin: auto;" />

ahora estandarizo los datos entre -1 y 1.


```r
a <- -1  # Límite inferior del rango objetivo
b <- 1   # Límite superior del rango objetivo

# Calcular el valor mínimo y máximo de tus datos
min_x <- min(indice_reclutamiento$PROPLOG)
max_x <- max(indice_reclutamiento$PROPLOG)

# Aplicar la fórmula de normalización
indice_reclutamiento$PROPLOG2 <- ((indice_reclutamiento$PROPLOG- min_x) / (max_x - min_x)) * (b - a) + a
```



```r
indseg3 <- ggplot(indice_reclutamiento  %>% 
  group_by(ANO_ARR,POLIGONO) %>%
  summarise(PROPLOG3=mean(PROPLOG2)), 
       aes(x = factor(ANO_ARR), 
           y = PROPLOG3,
           fill=PROPLOG3 > 0)) +
  geom_bar(stat = "identity")  +
  scale_fill_manual(values = c("black", "red"),
                    labels = c("Negativo", "Positivo"),
                    name="IR Erizo") +
  facet_wrap(.~POLIGONO) +
  geom_hline(yintercept = 0, color = "red")+
  scale_x_discrete(breaks = seq(from = 1996, to = 2022, by = 4))+
  theme_few()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "ANO", 
        y = "Índice de Reclutamiento")+
  coord_flip()
indseg3
```

<img src="Figuras/unnamed-chunk-16-1.jpeg" style="display: block; margin: auto;" />

Grafico como Oscilación por ppoligono


```r
recosc <- ggplot(indice_reclutamiento %>% 
                     group_by(ANO_ARR, POLIGONO, ZONA ) %>%
                    summarise(PROPLOG3=mean(PROPLOG2)), 
       aes(x = ANO_ARR, y = PROPLOG3)) +
  geom_ribbon(aes(ymin = pmax(PROPLOG3, 0), 
                  ymax = 0), 
              fill = "#de2d26", 
              alpha = 0.8) +  # Área por encima de la línea
  geom_ribbon(aes(ymin = pmin(PROPLOG3, 0), 
                  ymax = 0), 
              fill = "black", 
              alpha = 0.8) +  # Área por debajo de la línea
  geom_hline(yintercept = 0, color = "red")+
  geom_line(color = "black") +  # Línea de anomalías
  geom_point( alpha=0.2,
              size= 0.9)+
  labs(x = "AÑO", y = "Indice Reclutamiento Erizo") +
   facet_wrap(.~POLIGONO, ncol = 4)+
  theme_few()+
  theme(axis.text.x = element_text(angle = 90, hjust = 2))
recosc
```

<img src="Figuras/unnamed-chunk-17-1.jpeg" style="display: block; margin: auto;" />

Grafico como Oscilación por Zona


```r
recosczo <- ggplot(indice_reclutamiento %>% 
                     group_by(ANO_ARR, ZONA, MES_ARR ) %>%
                    summarise(PROPLOG3=mean(PROPLOG2)), 
       aes(x = ANO_ARR, y = PROPLOG3)) +
  geom_ribbon(aes(ymin = pmax(PROPLOG3, 0), 
                  ymax = 0), 
              fill = "#de2d26", 
              alpha = 0.8) +  # Área por encima de la línea
  geom_ribbon(aes(ymin = pmin(PROPLOG3, 0), 
                  ymax = 0), 
              fill = "black", 
              alpha = 0.8) +  # Área por debajo de la línea
  geom_hline(yintercept = 0, color = "red")+
  geom_line(color = "black") +  # Línea de anomalías
  geom_point( alpha=0.2,
              size= 0.9)+
  labs(x = "AÑO", y = "Indice Reclutamiento Erizo") +
   facet_grid(MES_ARR~ZONA)+
  theme_few()+
  theme(axis.text.x = element_text(angle = 90, hjust = 2))
recosczo
```

<img src="Figuras/unnamed-chunk-18-1.jpeg" style="display: block; margin: auto;" />

## Tallas estaciones Fijas

# Indicadores:

- Seguimiento y evaluación:
  - Tallas medias por macrozona y poligono
  - Indice  de Reclutamiento por macrozona y poligono y por mes
  - CPUE por poligono y por procedencia
  - biomasa desovante por macrozona
  - F por macrozona
  
- Red Estaciones fijas
  - Tallas medias
  - Cobertura algal
  - Densidades medias por sector por macrozona
  
  
