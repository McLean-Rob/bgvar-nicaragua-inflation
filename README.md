# El papel de los factores globales en la dinámica inflacionaria en Nicaragua
### Una aproximación de Vectores Autorregresivos Globales Bayesianos (BGVAR)

**Autor:** Lic. Roberto McLean  
**Programa:** Maestría en Economía y Finanzas — Universidad Nacional de Ingeniería (UNI), Nicaragua  
**Lugar y año:** Managua, Nicaragua · 2026

---

## Resumen

Este repositorio contiene el código R y los datos utilizados en la tesis de maestría que examina la influencia de los factores globales en la dinámica inflacionaria de Nicaragua mediante un modelo **BGVAR (Bayesian Global Vector Autoregression)**. El modelo cubre **21 países** e incorpora variables macroeconómicas clave para identificar los canales de transmisión de choques externos hacia la inflación nicaragüense.

Los principales hallazgos indican que, a corto plazo, la inflación está impulsada predominantemente por factores domésticos. Sin embargo, en horizontes de mediano y largo plazo, los choques externos —en particular los precios internacionales de petróleo y alimentos, y las condiciones macroeconómicas de los principales socios comerciales— explican una proporción sustancial de la varianza inflacionaria.

---

## Estructura del repositorio

```
bgvar-nicaragua-inflation/
│
├── README.md
│
├── data/
│   ├── Data.xlsx             # Base de datos macroeconómica principal (21 países)
│   ├── W.trade1619.xlsx      # Matriz de ponderación comercial 2016–2019
│   ├── W.trade1922.xlsx      # Matriz de ponderación comercial 2019–2022
│   └── W.trade0322.xlsx      # Matriz de ponderación comercial 2003–2022
│
└── code/
    ├── BGVAR_CAPRD_1.R             # Estimación principal del modelo BGVAR
    └── R_script_tesis_bgvar_new.R  # Script secundario: exploración de especificaciones
```

---

## Datos

### Base de datos macroeconómica (`Data.xlsx`)

Contiene series de tiempo trimestrales para los **21 países** del modelo. Las variables están transformadas en **primeras diferencias de logaritmos** para capturar tasas de crecimiento y mitigar problemas de no estacionariedad.

| Variable | Descripción |
|---|---|
| `y` | PIB real (tasa de crecimiento) |
| `dp` | Inflación (IPC, tasa de crecimiento) |
| `r` | Tasa de interés nominal |
| `reer` | Tipo de cambio real efectivo (variación) |
| `x` | Exportaciones (variación relativa) |
| `m` | Importaciones (variación relativa) |

### Países incluidos

| Código | País | Ponderación comercial (%) |
|---|---|---|
| NI | Nicaragua *(país focal)* | — |
| US | Estados Unidos | 41.92 |
| ME | México | 10.21 |
| GT | Guatemala | 5.63 |
| SL | El Salvador | 5.53 |
| CR | Costa Rica | 5.25 |
| UE | Unión Europea (UE27) | 5.15 |
| PA | Panamá | 3.47 |
| HN | Honduras | 2.07 |
| CA | Canadá | 1.02 |
| KO | Corea | 0.94 |
| JP | Japón | 0.76 |
| UK | Reino Unido | 0.58 |
| EC | Ecuador | 0.62 |
| IN | India | 0.63 |
| BR | Brasil | 0.80 |
| CN | China | 4.92 |
| CO | Colombia | 0.45 |
| CH | Chile | 0.30 |
| AR | Argentina | 0.34 |
| DR | República Dominicana | 0.38 |

### Matrices de ponderación comercial

Las matrices `W.trade*.xlsx` contienen los pesos bilaterales de comercio normalizados por fila, utilizados para construir las variables extranjeras débilmente exógenas del BGVAR. Se construyeron para tres períodos:

- `W.trade0322.xlsx` — período amplio 2003–2022
- `W.trade1619.xlsx` — período 2016–2019 (pre-pandemia)
- `W.trade1922.xlsx` — período 2019–2022 (post-pandemia)

**Fuente:** Datos de comercio bilateral del FMI (DOTS) y del Banco Central de Nicaragua (BCN).

---

## Código

### `BGVAR_CAPRD_1.R` — Estimación principal

Script principal que replica los resultados reportados en la tesis. Realiza:

1. Carga y preparación de datos (`excel_to_list()`)
2. Construcción y validación de las matrices de ponderación
3. Estimación del BGVAR con prior **Minnesota (MN)** — modelo base seleccionado por mejor desempeño predictivo (LPS = −4,112.24) y convergencia (8.57% de variables con |z| > 1.96 en diagnóstico de Geweke)
4. Estimación comparativa con priors **Normal-Gamma (NG)**, **SSVS** y **Horse-Shoe (HS)**
5. Cálculo de **Funciones Impulso-Respuesta Generalizadas (GIRF)**
6. **Descomposición Generalizada de la Varianza del Error de Pronóstico (GFEVD)** a 24 trimestres
7. Diagnósticos de convergencia MCMC

### `R_script_tesis_bgvar_new.R` — Exploración de especificaciones

Script auxiliar para comparar distintas configuraciones de rezagos y priors. Incluye:

- Pruebas con modelos `model.01` (defaults) y `model.1` (p=2)
- Análisis de sensibilidad del prior
- Evaluación fuera de muestra (Log Predictive Score)

---

## Especificación del modelo base

```
Modelo:     BGVAR con prior Minnesota (MN)
Rezagos:    p = 1 (endógenas y débilmente exógenas)
Draws:      5,000
Burn-in:    5,000
Horizonte GFEVD: 24 trimestres
Software:   R — paquete BGVAR (Boeck et al., 2022)
Seed:       set.seed(2521)
```

**Criterio de selección del modelo base:** el prior Minnesota fue seleccionado por su mejor desempeño predictivo (LPS = −4,112.24) y los mejores indicadores de convergencia de la cadena MCMC (diagnóstico de Geweke: 8.57% de variables con |z| > 1.96).

---

## Requisitos

### Software
- **R** ≥ 4.2.0
- **RStudio** (recomendado)

### Paquetes R

```r
install.packages("pacman")
pacman::p_load(BGVAR, ggplot2, zoo, openxlsx, readxl)
```

El paquete principal es [`BGVAR`](https://cran.r-project.org/package=BGVAR) (Boeck, Feldkircher & Huber, 2022).

---

## Instrucciones de replicación

1. Clonar o descargar este repositorio
2. Abrir R/RStudio y establecer el directorio de trabajo en la carpeta `data/`:
   ```r
   setwd("ruta/a/bgvar-nicaragua-inflation/data")
   ```
3. Ejecutar `BGVAR_CAPRD_1.R` para replicar los resultados principales de la tesis

> ⚠️ **Nota sobre tiempos de cómputo:** La estimación MCMC con 5,000 draws puede tomar entre 30 y 90 minutos dependiendo de las especificaciones del equipo. Se recomienda ejecutar primero con `draws = 100, burnin = 100` para verificar que el código corre correctamente antes de la estimación completa.

---

## Referencias principales

- Boeck, M., Feldkircher, M. & Huber, F. (2022). *BGVAR: Bayesian Global Vector Autoregressions with Common Factors in R*. Journal of Statistical Software.
- Pesaran, M.H., Schuermann, T. & Weiner, S.M. (2004). Modeling Regional Interdependencies Using a Global Error-Correcting Macroeconometric Model. *Journal of Business & Economic Statistics*, 22(2), 129–162.
- Cuaresma, J.C., Feldkircher, M. & Huber, F. (2016). Forecasting with Global Vector Autoregressive Models. *International Journal of Forecasting*.

---

## Licencia

Este repositorio es de acceso abierto con fines académicos. Si utilizas estos datos o código en tu investigación, por favor cita la tesis correspondiente.

---

*Tesis de Maestría en Economía y Finanzas — UNI Nicaragua, 2026*
