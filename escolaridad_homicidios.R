## Distribución de homicidios por escolaridad en México
## Librerias
library(tidyverse)
library(readr)
library(foreign)
library(RColorBrewer)
library(ggsci)


## Función para filtrar homicidios, considerando:
## Año, Entidad, sexo de la víctima, edad agrupada por grupos quinquenales y escolaridad
fhom1 <- function(df){
  as.tibble(df) %>%
    filter(LISTA_MEX == 55) %>% ## 55 es la clave para homicidio
    select(ANIO_OCUR,
           ENT_OCURR,
           SEXO,
           EDAD_AGRU,
           ESCOLARIDA)}
## Bases de datos crudas
## Las bases de datos están muy pesadas, para cargarlas en github
## hay que descargarlas de https://www.inegi.org.mx/programas/mortalidad/?ps=microdatos y colocar la ubicación en el apartado file =
## Los archivos de trabajo son lo que llevan el nombre DEFUN ##
def19 <- read.dbf(file = )
def18 <- read.dbf(file = )
def17 <- read.dbf(file = )
def16 <- read.dbf(file = )
def15 <- read.dbf(file = )
def14 <- read.dbf(file = )
def13 <- read.dbf(file = )
def12 <- read.dbf(file = )

## Bases de datos filtradas
hom19 <- fhom1(def19)
hom18 <- fhom1(def18)
hom17 <- fhom1(def17)
hom16 <- fhom1(def16)
hom15 <- fhom1(def15)
hom14 <- fhom1(def14)
hom13 <- fhom1(def13)
hom12 <- fhom1(def12)

## Removemos bases de datos crudas
rm(def19,
   def18,
   def17,
   def16,
   def15,
   def14,
   def13,
   def12)

hom12_19 <- bind_rows(hom19,
                      hom18,
                      hom17,
                      hom16,
                      hom15,
                      hom14,
                      hom13,
                      hom12)

rm(hom19,
   hom18,
   hom17,
   hom16,
   hom15,
   hom14,
   hom13,
   hom12,
   fhom1)

## Convertimos los valores de la base de datos en variables categóricas
## Consideramos el último grado de escolaridad (entonces, si alguien tiene primaria incompleta lo consideramos como kinder, etc.)
ESCOLARIDA1 <- c("N_ESC",
                 "PREESC",
                 "PREESC",
                 "PRIMARIA",
                 "PRIMARIA",
                 "SECUNDARIA",
                 "SECUNDARIA",
                 "BACHILLERATO",
                 "LIC",
                 "POSGRADO",
                 "NO_APLICA",
                 "INDETERMINADO")
ESCOLARIDA <- tibble(distinct(hom12_19,
                              ESCOLARIDA)) %>% 
  arrange(ESCOLARIDA)
ESCOLARIDA <- tibble(ESCOLARIDA1,
                     ESCOLARIDA)
rm(ESCOLARIDA1)

SEXO <- tibble(SEXO = c(1,
                        2,
                        9),
               SEXO1 = c("HOMBRE",
                         "MUJER",
                         "NO IDENTIFICADO"))

hom12_19tr <- merge(hom12_19,
                    ESCOLARIDA)
hom12_19tr <- merge(hom12_19tr,
                    SEXO)

## Distribución de la escolaridad de las víctimas de homicidios en México
## Base de datos donde contamos los homicidios y vemos la distribución porcentual
escolaridad_homicidios<- hom12_19tr %>% 
  group_by(ANIO_OCUR,
           SEXO1,
           ESCOLARIDA1) %>%
  filter(ANIO_OCUR >= 2012,
         ANIO_OCUR <= 2019) %>%
  filter(SEXO1 %in% c("HOMBRE",
                      "MUJER")) %>%
  summarise(VICT = n()) %>%
  mutate(pct = VICT / sum(VICT) * 100)

##  Con esta base de datos, obtenemos los niveles educativos, de acuerdo a las etiquetas:
## N_ESC (Sin escolaridad)
## BASICO (Educación Básica: Preescolar, Primaria y Secundaria)
## MEDIA_SUPERIOR (Educación media superior: bachillerato)
## SUPERIOR (Educación superior: licenciatura y posgrado)
## NO_APLICA (No aplica la categoría)
nivel_edu <- escolaridad_homicidios%>%
  ungroup() %>%
  distinct(ESCOLARIDA1) %>%
  arrange() %>%
  mutate(nivel = as.factor(c("MEDIA_SUPERIOR",
                             "INDETERMINADO",
                             "SUPERIOR",
                             "N_ESC",
                             "NO_SE_APLICA",
                             "SUPERIOR",
                             "BASICO",
                             "BASICO",
                             "BASICO")))
## Modificamos la base de daatos para introducir niveles
escolaridad_homicidios <- merge(escolaridad_homicidios,
                                nivel_edu,
                                by = "ESCOLARIDA1") %>%
  rename(año = ANIO_OCUR)

## Ordenamos factores para que así aparezca en gráficas
escolaridad_homicidios$nivel <- factor(escolaridad_homicidios$nivel,
                                       levels = c("BASICO",
                                                  "MEDIA_SUPERIOR",
                                                  "SUPERIOR",
                                                  "N_ESC",
                                                  "INDETERMINADO",
                                                  "NO_APLICA"))

## Gráfica por la distribución de la escolaridad de víctima de homicidio, por año
ggplot(escolaridad_homicidios,
       aes(x = año,
           y = pct,
           fill = nivel))+
  theme_bw()+
  geom_bar(stat="identity",
           position = "stack")+
  scale_fill_simpsons(labels = c("Básica",
                                 "Media superior",
                                 "Superior",
                                 "Sin escolaridad", 
                                 "Indeterminado",
                                 "No se aplica"))+
  facet_wrap(~ SEXO1,
             ncol = 1) +
  scale_x_continuous(breaks = c(2012,
                                2013,
                                2014,
                                2015,
                                2016,
                                2017,
                                2018,
                                2019),
                     labels = c(2012,
                                2013,
                                2014,
                                2015,
                                2016,
                                2017,
                                2018,
                                2019))+
  labs(title="Homicidios en México, 2012 - 2019", 
       subtitle="Porcentaje de homicidios por escolaridad de víctima",
       x = "Año",
       y = "Porcentaje",
       fill = "Nivel de Escolaridad",
       caption = "Fuente: Microdatos de Mortalidad 2012 - 2019, INEGI")


## Comparación con distribución poblacional de educación
## Para esta comparación, solo consideraremos las distribuciones de 2019 en defunciones y 2020 en el censo
## No agrupamos por sexo para hacerlo comparable con datos del censo 2020
escolaridad_homicidios_comparacion <- hom12_19tr %>% 
  group_by(ANIO_OCUR,
           ESCOLARIDA1) %>%
  filter(ANIO_OCUR == 2019) %>%
  ## Filtramos de la categoría 8 a la 29, para tener población en el rango de edad 15 y más
  filter(EDAD_AGRU %in% c("08",
                          "09",
                          "10",
                          "11",
                          "12",
                          "13",
                          "14",
                          "15",
                          "16",
                          "17",
                          "18",
                          "19",
                          "20",
                          "21",
                          "22",
                          "23",
                          "24",
                          "25",
                          "26",
                          "27",
                          "28",
                          "29")) %>% 
  summarise(VICT = n()) %>%
  mutate(pct = VICT / sum(VICT) * 100) %>% 
  rename(año = ANIO_OCUR) %>% 
  mutate(año = as.factor(año)) %>% 
  select(año, ESCOLARIDA1, pct)

nivel_edu <- escolaridad_homicidios_comparacion%>%
  ungroup() %>%
  distinct(ESCOLARIDA1) %>%
  arrange() %>%
  mutate(nivel = as.factor(c("MEDIA_SUPERIOR",
                             "INDETERMINADO",
                             "SUPERIOR",
                             "N_ESC",
                             "SUPERIOR",
                             "BASICO",
                             "BASICO",
                             "BASICO")))
## Modificamos la base de datos para introducir niveles
escolaridad_homicidios_comparacion<- merge(escolaridad_homicidios_comparacion,
                                nivel_edu,
                                by = "ESCOLARIDA1") %>% 
  mutate(categoria = "victimas de homicidio") %>% 
  select(año, categoria, nivel, pct) %>%
  group_by(año, categoria, nivel) %>% 
  summarise(pct = sum(pct))


## Escolaridad en México 2019
escolaridad_mexico <- data.frame(año = as.factor(2019),
                                 categoria = as.factor(c("escolaridad en mexico")),
                                 nivel = as.factor(c("N_ESC",
                                                     "BASICO",
                                                     "MEDIA_SUPERIOR",
                                                     "SUPERIOR",
                                                     "INDETERMINADO")),
                                 pct = c(4.9,
                                         49.3,
                                         24.0,
                                         21.6,
                                         0.2))
## Modificamos la base de datos para introducir la distribución de los niveles educativos
df_comparacion<- bind_rows(escolaridad_homicidios_comparacion,
                           escolaridad_mexico)
## Ordenamos factores para que así aparezca en gráficas
df_comparacion$nivel <- factor(df_comparacion$nivel,
                                       levels = c("BASICO",
                                                  "MEDIA_SUPERIOR",
                                                  "SUPERIOR",
                                                  "N_ESC",
                                                  "INDETERMINADO"))

## Gráfico de comparación entre distribución de educación de la población mexicana
## y distribución de la educación de las víctimas de homicidio
ggplot(df_comparacion,
       aes(x = categoria,
           y = pct,
           fill = nivel))+
  theme_bw()+
  geom_bar(stat="identity",
           position = "stack",
           width = 0.4)+
  scale_fill_simpsons(labels = c("Básica",
                                 "Media superior",
                                 "Superior",
                                 "Sin escolaridad", 
                                 "Indeterminado"))+
  labs(title="Homicidios en México, 2019", 
       subtitle="Comparación entre distribución de homicidios por nivel educativo, población 15 años y más",
       x = " ",
       y = "Porcentaje",
       fill = "Nivel de Escolaridad",
       caption = "Fuente: Microdatos de Mortalidad 2019, INEGI y Características educativas de la población según el censo 2020, INEGI")
