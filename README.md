# IVSL — Índice de Vulnerabilidad Social Local

> Pipeline en R para construir y visualizar un índice compuesto de vulnerabilidad social a escala de radio censal, con cartografía interactiva basada en Leaflet.

---

## Descripción

El diagnóstico de la vulnerabilidad social es central en los procesos de mitigación y adaptación del riesgo. Este repositorio implementa una metodología para construir un **Índice de Vulnerabilidad Social Local (IVSL)** a partir de variables censales, utilizando análisis de componentes principales (PCA) integrado en un sistema de información geográfica (SIG).

Como caso de aplicación se analizaron datos censales de la República Argentina. A partir de las variables incluidas en el censo nacional se construyeron **19 indicadores descriptivos**, sintetizados en un único índice que permite categorizar el territorio a nivel de radio censal.

El resultado principal es un **mapa interactivo** que facilita la exploración espacial de la distribución de vulnerabilidad social.

---

## Demo

Abrí directamente en el navegador:

```
index.html          # página de presentación del proyecto
map_IVSL_CABA.html  # mapa interactivo generado
```

---

## Estructura del repositorio

```
IVSL/
│
├── README.md
├── index.html                  # página de presentación
├── map_IVSL_CABA.html          # mapa interactivo exportado
│
├── src/
│   ├── INDICE.R                # construcción del índice (PCA)
│   └── visual_ILVS.R           # visualización cartográfica con Leaflet
│
├── shp/                        # shapefiles de unidades territoriales
│
└── map_IVSL_CABA_files/        # archivos auxiliares del mapa HTML
```

---

## Requisitos

- **R** ≥ 3.6
- Paquetes:

```r
install.packages(c("leaflet", "maptools", "mapview", "sp", "rgdal"))
```

---

## Cómo reproducir el análisis

### 1. Preparar los datos geográficos

Colocar los shapefiles de las unidades territoriales en la carpeta `/shp`. Estos archivos definen los radios censales utilizados para el análisis.

### 2. Construir el índice

```r
source("src/INDICE.R")
```

Este script procesa las variables socioeconómicas, aplica PCA y genera el índice de vulnerabilidad que alimenta la visualización.

### 3. Generar el mapa interactivo

```r
source("src/visual_ILVS.R")
```

Este script integra el índice con el shapefile, construye el mapa con Leaflet y exporta el resultado como archivo HTML.

**Salida:** `map_IVSL_CABA.html`

---

## Interpretación del índice

El IVSL sintetiza múltiples indicadores sociales en un único valor compuesto. **Valores más altos indican mayor vulnerabilidad relativa** dentro del territorio analizado.

Aplicaciones posibles:

- Análisis territorial y urbano
- Planificación de políticas públicas
- Investigación en geografía social
- Visualización exploratoria de desigualdades espaciales

### Limitaciones

- El índice depende de las variables disponibles en las fuentes censales.
- Los resultados deben interpretarse en relación con la escala territorial utilizada.
- El IVSL representa **vulnerabilidad relativa** dentro del área analizada, no niveles absolutos de pobreza o exclusión social.

---

## Hallazgos principales

El análisis sobre datos censales de Argentina identificó patrones territoriales claros:

- Mayor vulnerabilidad social en el **sector oeste y norte** del país.
- Menor vulnerabilidad en la **región pampeana, patagónica y la Ciudad Autónoma de Buenos Aires**.
- La clasificación resultó efectiva para discriminar radios censales en condiciones extremas.

---

## Publicación

Este trabajo fue publicado en:

> Represa, N. S., Sanchez, Y., & Porta, A. (2019). *Estudio de la Vulnerabilidad Social en Argentina Mediante el Uso de SIG. Construcción de un Índice de Aplicación Local.* **Anuário do Instituto de Geociências**, 41(2), 351–357.

---

## Cita

Si utilizás este repositorio en investigación o proyectos, por favor citar:

```
Represa, N. S. Índice de Vulnerabilidad Social Local (IVSL). GitHub. https://github.com/solrepresa/IVSL
```

---

## Autora

**Natacha Soledad Represa**  
Data Science · AI · Spatial Analysis  
[github.com/solrepresa](https://github.com/solrepresa)
