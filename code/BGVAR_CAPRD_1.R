
#setwd("D:/Documentos/Documents/RMP/BGVAR/DATOS")
setwd("D:/Documentos/Documents/Thesis/Thesis_MEF/BGVAR/DATOS")
setwd("D:/Documentos/Documents/RMP/Thesis/Thesis_MEF/BGVAR/DATOS")
#D:/Documentos/Documents/RMP/Thesis/Thesis_MEF

pacman::p_load(ggplot2, BGVAR, zoo, openxlsx, readxl)

Data <- excel_to_list(file = "./Data.xlsx", first_column_as_time=TRUE, skipsheet=NULL)

#data(eerData) datos de ejemplo del paquete

W.trade1619 <- read_xlsx("W.trade1619.xlsx") #cargar la matriz de comercio

head(W.trade1619)

#head(W.trade0012)

all(colnames(W.trade1619)==names(Data))

rowSums(W.trade1619)
diag(W.trade1619)

###
# Debo convertir el formato de los datos
###

# Leer el archivo de Excel
W.trade1619 <- read_xlsx("W.trade1619.xlsx")

# Obtener las filas y columnas sin incluir la primera fila que contiene los nombres de los países
country_names <- colnames(W.trade1619)[-1]
row_names <- W.trade1619[[1]]

# Crear una matriz con los datos de W.trade1619
W.trade1619 <- as.matrix(W.trade1619[-1])

# Asignar nombres de filas y columnas a la matriz
rownames(W.trade1619) <- row_names
colnames(W.trade1619) <- country_names


# Verificar la estructura del data frame
str(W.trade0012)
str(W.trade1619)

summary(Data)

# 
# names(Data)
# 
# head(Data)
# class(Data)
# str(Data)
# 
# head(Data$US)
# 
# head(W.trade1619)
# head(W.trade0012)

set.seed(2521)

###################################################
### code chunk number 4: model.1
###################################################
model.1 <- bgvar(Data = Data, W = W.trade1619)


###################################################
### code chunk number 5: model.1.summary
###################################################
summary(model.1)

# geweke: 13.55%


###################################

#> summary(model.1)
#---------------------------------------------------------------------------
#  Model Info:
#  Prior: Normal-Gamma prior (NG)
#Number of lags for endogenous variables: 1
# Number of lags for weakly exogenous variables: 1
# Number of posterior draws: 5000/1=5000
# Number of stable posterior draws: 3148
# Number of cross-sectional units: 21
# ---------------------------------------------------------------------------
#   Convergence diagnostics
# Geweke statistic:
#   1014 out of 7482 variables' z-values exceed the 1.96 threshold (13.55%).
# ---------------------------------------------------------------------------
# F-test, first order serial autocorrelation of cross-unit residuals
# Summary statistics:
# =========  ==========  ======
# \          # p-values  in %  
# =========  ==========  ======
# >0.1       26          30.23%
# 0.05-0.1   5           5.81% 
# 0.01-0.05  7           8.14% 
# <0.01      48          55.81%
# =========  ==========  ======
# ---------------------------------------------------------------------------
# Average pairwise cross-unit correlation of unit-model residuals
# Summary statistics:
# =======  ==========  ===========  ===========  ===========
# \        y           i            r            e          
# =======  ==========  ===========  ===========  ===========
# <0.1     7 (33.33%)  17 (80.95%)  15 (71.43%)  20 (95.24%)
# 0.1-0.2  1 (4.76%)   4 (19.05%)   6 (28.57%)   1 (4.76%)  
# 0.2-0.5  13 (61.9%)  0 (0%)       0 (0%)       0 (0%)     
# >0.5     0 (0%)      0 (0%)       0 (0%)       0 (0%)     
# =======  ==========  ===========  ===========  ===========
# ---------------------------------------------------------------------------
# ###################################
#THIS IS THE VERSION WITH THE SET.SEED 2521
# ---------------------------------------------------------------------------
#   Model Info:
#   Prior: Normal-Gamma prior (NG)
# Number of lags for endogenous variables: 1
# Number of lags for weakly exogenous variables: 1
# Number of posterior draws: 5000/1=5000
# Number of stable posterior draws: 2840
# Number of cross-sectional units: 21
# ---------------------------------------------------------------------------
#   Convergence diagnostics
# Geweke statistic:
#   1036 out of 7482 variables' z-values exceed the 1.96 threshold (13.85%).
# ---------------------------------------------------------------------------
# F-test, first order serial autocorrelation of cross-unit residuals
# Summary statistics:
# =========  ==========  ======
# \          # p-values  in %  
# =========  ==========  ======
# >0.1       26          30.23%
# 0.05-0.1   4           4.65% 
# 0.01-0.05  9           10.47%
# <0.01      47          54.65%
# =========  ==========  ======
# ---------------------------------------------------------------------------
# Average pairwise cross-unit correlation of unit-model residuals
# Summary statistics:
# =======  ==========  ===========  ===========  ===========
# \        y           i            r            e          
# =======  ==========  ===========  ===========  ===========
# <0.1     8 (38.1%)   17 (80.95%)  15 (71.43%)  20 (95.24%)
# 0.1-0.2  0 (0%)      4 (19.05%)   6 (28.57%)   1 (4.76%)  
# 0.2-0.5  13 (61.9%)  0 (0%)       0 (0%)       0 (0%)     
# >0.5     0 (0%)      0 (0%)       0 (0%)       0 (0%)     
# =======  ==========  ===========  ===========  ===========
# ---------------------------------------------------------------------------

conv.diag(model.1)


yfit <- fitted(model.1)
plot(model.1, global = FALSE, resp = "NI")
#plot(model.1, global = FALSE, resp = "NI", cex.axis=1.2)

###########################################

??bgvar

###################################################
### code chunk number 7: forecasting
###################################################
model1.ssvs <- bgvar(Data=Data,W=W.trade1619,plag=1,hold.out=8,thin=2,
                     draws=1000,burnin=1000, prior="SSVS")
model1.ng <- bgvar(Data=Data,W=W.trade1619,plag=1,hold.out=8,thin=2,
                   draws=1000,burnin=1000, prior="NG")
model1.mn <- bgvar(Data=Data,W=W.trade1619,plag=1,hold.out=8,thin=2,
                   draws=1000,burnin=1000, prior="MN")
model1.hs <- bgvar(Data=Data,W=W.trade1619,plag=1,hold.out=8,thin=2,
                   draws=1000,burnin=1000,prior="HS")
model2.ssvs <- bgvar(Data=Data,W=W.trade1619,plag=2,hold.out=8,thin=2,
                     draws=1000,burnin=1000,prior="SSVS")
model2.ng <- bgvar(Data=Data,W=W.trade1619,plag=2,hold.out=8,thin=2,
                   draws=1000,burnin=1000, prior="NG")
model2.mn <- bgvar(Data=Data,W=W.trade1619,plag=2,hold.out=8,thin=2,
                   draws=1000,burnin=1000, prior="MN")
model2.hs <- bgvar(Data=Data,W=W.trade1619,plag=2,hold.out=8,thin=2,
                   draws=1000,burnin=1000,prior="HS")

#############################################################

model3.ng <- bgvar(Data = Data, W = W.trade1619, hold.out = 8, thin = 2, prior = "NG")
model4.ng <- bgvar(Data = Data, W = W.trade1619, hold.out = 8, thin = 2, prior = "NG", plag = 2)

###################################################
### code chunk number 8: model.eval
###################################################
fcast <- predict(model1.ssvs, n.ahead=8)
lps.ssvs<-sum(lps(fcast))

###################################################
### code chunk number 9: model.eval2
###################################################
fcast1.ng <- predict(model1.ng, n.ahead=8);lps.ng<-sum(lps(fcast1.ng))
fcast1.mn <- predict(model1.mn, n.ahead=8);lps.mn<-sum(lps(fcast1.mn))
fcast1.hs <- predict(model1.hs, n.ahead=8);lps.hs<-sum(lps(fcast1.hs))
fcast2.ssvs <- predict(model2.ssvs, n.ahead=8);lps.ssvs2<-sum(lps(fcast2.ssvs))
fcast2.ng <- predict(model2.ng, n.ahead=8);lps.ng2<-sum(lps(fcast2.ng))
fcast2.mn <- predict(model2.mn, n.ahead=8);lps.mn2<-sum(lps(fcast2.mn))
fcast2.hs <- predict(model2.hs, n.ahead=8);lps.hs2<-sum(lps(fcast2.hs))
dic1.ng<-dic(model1.ng);dic2.ng<-dic(model2.ng)
dic1.mn<-dic(model1.mn);dic2.mn<-dic(model2.mn)
dic1.ssvs<-dic(model1.ng);dic2.ssvs<-dic(model2.ssvs)
dic1.hs<-dic(model1.hs);dic2.hs<-dic(model2.hs)

###################################################################

fcast3.ng <- predict(model3.ng, n.ahead = 8); lps.ng3 <- sum(lps(fcast3.ng))
fcast4.ng <- predict(model4.ng, n.ahead = 8); lps.ng4 <- sum(lps(fcast4.ng))
dic3.ng <- dic(model3.ng); dic4.ng <- dic(model4.ng)

###################################################
### code chunk number 10: summary.table1
###################################################
library(xtable)
summarize.lps<-rbind(c(lps.ssvs,lps.ng,lps.mn,lps.hs),
                     c(lps.ssvs2,lps.ng2,lps.mn2,lps.hs2)
)
colnames(summarize.lps)<-c("SSVS","NG","MN","HS")
rownames(summarize.lps)<-c("p=1","p=2")
xtable(summarize.lps, digits = 2, caption = "LPS scores",label="tbl:lps")

##################################################################

library(xtable)
summarize.lps2 <- rbind(c(lps.ng3), c(lps.ng4))
colnames(summarize.lps2)<-c("NG")
rownames(summarize.lps2)<-c("p=1","p=2")
xtable(summarize.lps2, digits = 2, caption = "LPS scores for NG",label="tbl:lps2")


##################################################################

lps
###################################################
### code chunk number 11: summary.table2
###################################################
summarize.dic<-rbind(c(dic1.ssvs,dic1.ng,dic1.mn,dic1.hs),
                     c(dic2.ssvs,dic2.ng,dic2.mn,dic2.hs)
)
colnames(summarize.dic)<-c("SSVS","NG","MN","HS")
rownames(summarize.dic)<-c("p=1","p=2")
rel.dic<-summarize.dic/summarize.dic["p=1","NG"]

xtable(rel.dic, digits = 2, caption = "DIC relative to NG (p=1)",label="tbl:dic")

#################################################################################

summarize.dic2<-rbind(c(dic3.ng),
                     c(dic4.ng))
colnames(summarize.dic2)<-c("NG")
rownames(summarize.dic2)<-c("p=1","p=2")
rel.dic2<-summarize.dic2/summarize.dic2["p=1","NG"]

xtable(rel.dic2, digits = 2, caption = "DIC relative to NG default (p=1)",label="tbl:dic2")

diag3.ng   <- conv.diag(model3.ng)
diag4.ng   <- conv.diag(model4.ng)

##############################################################
# Model eval by Geweke stats
###########################################################

# Cargar la librería xtable
library(xtable)

# Obtener los diagnósticos de convergencia para cada modelo
diag1.ssvs <- conv.diag(model1.ssvs)
diag1.ng   <- conv.diag(model1.ng)
diag1.mn   <- conv.diag(model1.mn)
diag1.hs   <- conv.diag(model1.hs)
diag2.ssvs <- conv.diag(model2.ssvs)
diag2.ng   <- conv.diag(model2.ng)
diag2.mn   <- conv.diag(model2.mn)
diag2.hs   <- conv.diag(model2.hs)

# Función para extraer el porcentaje de convergencia
extraer_porcentaje <- function(x) {
  # Supone que x es un character que contiene el mensaje con el porcentaje
  porcentaje <- gsub(".*\\((.*)%\\).*", "\\1", x)
  return(as.numeric(porcentaje))
}

# Extraer el porcentaje de variables con |z| > 1.96 para cada modelo
pct1.ssvs <- extraer_porcentaje(diag1.ssvs)
pct1.ng   <- extraer_porcentaje(diag1.ng)
pct1.mn   <- extraer_porcentaje(diag1.mn)
pct1.hs   <- extraer_porcentaje(diag1.hs)

pct2.ssvs <- extraer_porcentaje(diag2.ssvs)
pct2.ng   <- extraer_porcentaje(diag2.ng)
pct2.mn   <- extraer_porcentaje(diag2.mn)
pct2.hs   <- extraer_porcentaje(diag2.hs)

# Crear una matriz con los resultados
geweke_table <- rbind(
  "p = 1" = c(SSVS = pct1.ssvs, NG = pct1.ng, MN = pct1.mn, HS = pct1.hs),
  "p = 2" = c(SSVS = pct2.ssvs, NG = pct2.ng, MN = pct2.mn, HS = pct2.hs)
)

# Generar el código LaTeX de la tabla
xtable_geweke <- xtable(geweke_table, digits = 2,
                        caption = "Estadístico de Geweke: Porcentaje de variables con |z| > 1.96",
                        label = "tbl:geweke")
print(xtable_geweke, type = "latex")

###########################################################################




#####################################################################
#SECCIÓN REESTIMACIÓN DE LAS PROPIEDADES DE SELECCIÓN DEL PRIOR QUE
#MEJOR SER ADAPTE 
#
#

###################################################
### code chunk number 7: forecasting
###################################################
model1.ssvs <- bgvar(Data=Data, W=W.trade1619,plag=1,hold.out=8,thin=2, prior="SSVS")
model1.ng <- bgvar(Data=Data,W=W.trade1619,plag=1,hold.out=8,thin=2, prior="NG")
model1.mn <- bgvar(Data=Data,W=W.trade1619,plag=1,hold.out=8,thin=2, prior="MN")
model1.hs <- bgvar(Data=Data,W=W.trade1619,plag=1,hold.out=8,thin=2, prior="HS")
model2.ssvs <- bgvar(Data=Data,W=W.trade1619,plag=2,hold.out=8,thin=2, prior="SSVS")
model2.ng <- bgvar(Data=Data,W=W.trade1619,plag=2,hold.out=8,thin=2, prior="NG")
model2.mn <- bgvar(Data=Data,W=W.trade1619,plag=2,hold.out=8,thin=2, prior="MN")
model2.hs <- bgvar(Data=Data,W=W.trade1619,plag=2,hold.out=8,thin=2, prior="HS")

model1.mn <- bgvar(Data=Data,W=W.trade1619,plag=2, hold.out = 15, thin=2, prior="MN", trend=TRUE)
summary(model1.mn)
residuos <- BGVAR::resid.corr.test(model1.mn, lag.cor = 1)
class(residuos)

library(openxlsx)

# Ejecutar el test de correlación de residuos
residuos <- BGVAR::resid.corr.test(model1.mn, lag.cor = 1)

# Crear un nuevo workbook
wb <- createWorkbook()

# Iterar sobre cada elemento de la lista
for(nom in names(residuos)){
  # Agregar una hoja con el nombre del elemento
  addWorksheet(wb, sheetName = nom)
  
  data_element <- residuos[[nom]]
  
  # Intentar convertir a data frame
  try_df <- try(as.data.frame(data_element), silent = TRUE)
  
  if(inherits(try_df, "try-error")) {
    # Si no se puede convertir, usar capture.output para guardar la información en formato texto
    data_to_export <- data.frame(Text = capture.output(print(data_element)))
    writeData(wb, sheet = nom, x = data_to_export, rowNames = FALSE)
  } else {
    writeData(wb, sheet = nom, x = try_df, rowNames = TRUE)
  }
}

# Guardar el workbook en un archivo Excel
saveWorkbook(wb, file = "residuos.xlsx", overwrite = TRUE)


# example for class 'bgvar.resid'
res <- residuals(model1.mn)
plot(res, resp = NULL, global = TRUE)


# model1 <- bgvar(Data = pesaranData, W = W.8016)
# summary(model1)
# View(pesaranData)

# Ejecutar el test de correlación de residuos
residuos1 <- BGVAR::resid.corr.test(model1, lag.cor = 1)

# Crear un nuevo workbook
wb <- createWorkbook()

# Iterar sobre cada elemento de la lista
for(nom in names(residuos1)){
  # Agregar una hoja con el nombre del elemento
  addWorksheet(wb, sheetName = nom)
  
  data_element <- residuos1[[nom]]
  
  # Intentar convertir a data frame
  try_df <- try(as.data.frame(data_element), silent = TRUE)
  
  if(inherits(try_df, "try-error")) {
    # Si no se puede convertir, usar capture.output para guardar la información en formato texto
    data_to_export <- data.frame(Text = capture.output(print(data_element)))
    writeData(wb, sheet = nom, x = data_to_export, rowNames = FALSE)
  } else {
    writeData(wb, sheet = nom, x = try_df, rowNames = TRUE)
  }
}

# Guardar el workbook en un archivo Excel
saveWorkbook(wb, file = "residuos1.xlsx", overwrite = TRUE)

plot(pesaranData$US$y)
plot(Data$UE$y)


model.ssvs.1<-bgvar(Data=Data,
                    W=W.trade1619,
                    draws=30000,
                    burnin=30000,
                    plag=2,
                    prior="SSVS",
                    hyperpara=NULL, 
                    SV=TRUE,
                    thin=10,
                    Ex=NULL,
                    trend=TRUE,
                    expert=list(save.shrink.store=TRUE),
                    hold.out=0,
                    eigen=1,
                    verbose=TRUE
)

summary(model.ssvs.1)
residuos2 <- BGVAR::resid.corr.test(model.ssvs.1, lag.cor = 1)

# Crear un nuevo workbook
wb <- createWorkbook()

# Iterar sobre cada elemento de la lista
for(nom in names(residuos2)){
  # Agregar una hoja con el nombre del elemento
  addWorksheet(wb, sheetName = nom)
  
  data_element <- residuos2[[nom]]
  
  # Intentar convertir a data frame
  try_df <- try(as.data.frame(data_element), silent = TRUE)
  
  if(inherits(try_df, "try-error")) {
    # Si no se puede convertir, usar capture.output para guardar la información en formato texto
    data_to_export <- data.frame(Text = capture.output(print(data_element)))
    writeData(wb, sheet = nom, x = data_to_export, rowNames = FALSE)
  } else {
    writeData(wb, sheet = nom, x = try_df, rowNames = TRUE)
  }
}

# Guardar el workbook en un archivo Excel
saveWorkbook(wb, file = "residuos2.xlsx", overwrite = TRUE)


###################################################
#
#modelo con corrección de autocorrelación serial de primer orden
#
################################################################

# Start estimation of Bayesian Global Vector Autoregression.
# 
# Prior: Stochastic Search Variable Selection prior.
# Lag order: 2 (endo.), 2 (w. exog.)
# Stochastic volatility: enabled.
# Number of cores used: 1.
# Thinning factor: 10. This means every 10th draw is saved.
# No hyperparameters are chosen, default setting applied.
# 
# Estimation of country models starts...
# Estimation done and took 143 mins 51 seconds.
# Stacking of global model starts... 
# 
# Stacking finished.
# Computation of BGVAR yields 51 (2%) draws (active trimming).
# Needed time for estimation of bgvar: 146 mins 5 seconds.
# > 
#   > summary(model.ssvs.1)
# ---------------------------------------------------------------------------
#   Model Info:
#   Prior: Stochastic Search Variable Selection prior (SSVS)
# Number of lags for endogenous variables: 2
# Number of lags for weakly exogenous variables: 2
# Number of posterior draws: 30000/10=3000
# Number of stable posterior draws: 51
# Number of cross-sectional units: 21
# ---------------------------------------------------------------------------
#   Convergence diagnostics
# Geweke statistic:
#   1569 out of 14964 variables' z-values exceed the 1.96 threshold (10.49%).
# ---------------------------------------------------------------------------
# F-test, first order serial autocorrelation of cross-unit residuals
# Summary statistics:
# =========  ==========  ======
# \          # p-values  in %  
# =========  ==========  ======
# >0.1       45          52.33%
# 0.05-0.1   6           6.98% 
# 0.01-0.05  13          15.12%
# <0.01      22          25.58%
# =========  ==========  ======
# ---------------------------------------------------------------------------
# Average pairwise cross-unit correlation of unit-model residuals
# Summary statistics:
# =======  ===========  ===========  ===========  =========
# \        y            i            r            e        
# =======  ===========  ===========  ===========  =========
# <0.1     6 (28.57%)   20 (95.24%)  14 (66.67%)  21 (100%)
# 0.1-0.2  1 (4.76%)    1 (4.76%)    7 (33.33%)   0 (0%)   
# 0.2-0.5  14 (66.67%)  0 (0%)       0 (0%)       0 (0%)   
# >0.5     0 (0%)       0 (0%)       0 (0%)       0 (0%)   
# =======  ===========  ===========  ===========  =========
# ---------------------------------------------------------------------------
# 


yfit <- fitted(model.ssvs.1)
plot(model.ssvs.1, global = FALSE, resp = "NI")

gfevd.ssvs=gfevd(model.ssvs.1,n.ahead=24,running=TRUE)$FEVD

# get position of NI 
idx<-which(grepl("NI.",dimnames(gfevd.ssvs)[[2]]))
own<-colSums(gfevd.ssvs["NI.i",idx,])
foreign<-colSums(gfevd.ssvs["NI.i",-idx,])
barplot(t(cbind(own,foreign)),legend.text =c("Interno","Externo"))

# Crear data_gfevd con las sumas de contribuciones
data_gfevd <- t(cbind(own, foreign))

library(ggplot2)
library(reshape2)

# Convertir data_gfevd a formato largo
df <- data.frame(
  Trimestre = rep(1:ncol(data_gfevd), each = 2),
  Tipo = rep(c("Interno", "Externo"), times = ncol(data_gfevd)),
  Proporcion = as.vector(data_gfevd)
)

# Crear el gráfico apilado con ggplot2
p <- ggplot(df, aes(x = factor(Trimestre), y = Proporcion, fill = Tipo)) +
  geom_bar(stat = "identity") +  # Barras apiladas
  geom_text(aes(label = round(Proporcion, 2), color = Tipo), 
            position = position_stack(vjust = 0.5), 
            size = 4, show.legend = FALSE) +  # Etiquetas con color mapeado
  labs(title = "", 
       x = "Trimestre", 
       y = "Proporción") +
  scale_fill_manual(values = c("Interno" = "grey25", "Externo" = "grey80")) +
  scale_color_manual(values = c("Interno" = "white", "Externo" = "black")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",    # Ubicar leyenda en la parte superior
        legend.title = element_blank())

# Mostrar el gráfico
print(p)

#################################################
# LAS GFEVD PARA CENTROAMÉRICA

# LAS GFEVD PARA CENTROAMÉRICA

library(ggplot2)
library(reshape2)
library(gridExtra)

# Función para generar el gráfico de cada país
generar_grafico <- function(pais) {
  # Obtener índices del país actual
  idx <- which(grepl(paste0(pais, "."), dimnames(gfevd.1)[[2]]))
  own <- colSums(gfevd.1[paste0(pais, ".i"), idx, ])
  foreign <- colSums(gfevd.1[paste0(pais, ".i"), -idx, ])
  
  # Crear data_gfevd con las sumas de contribuciones
  data_gfevd <- t(cbind(own, foreign))
  
  # Convertir a formato largo
  df <- data.frame(
    Trimestre = rep(1:ncol(data_gfevd), each = 2),
    Tipo = rep(c("Interno", "Externo"), times = ncol(data_gfevd)),
    Proporcion = as.vector(data_gfevd)
  )
  
  # Crear gráfico
  p <- ggplot(df, aes(x = factor(Trimestre), y = Proporcion, fill = Tipo)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(Proporcion, 2), 
                  color = ifelse(Tipo == "Interno", "white", "black")),
              position = position_stack(vjust = 0.5),
              size = 2) +
    labs(title = pais, x = "Trimestre", y = "Proporción") +
    scale_fill_manual(values = c("Interno" = "grey25", "Externo" = "grey80"),
                      name = "Contribución") +
    scale_color_identity() +  # Para que use los colores definidos en aes()
    theme_minimal(base_size = 10) +
    theme(legend.position = "top")
  
  return(p)
}

# Lista de países
paises <- c("NI", "CR", "DR", "GT", "PA", "HN")

# Generar gráficos para cada país
graficos <- lapply(paises, generar_grafico)

# Organizar los gráficos en un panel
grid.arrange(grobs = graficos, ncol = 2)




##############################################################
# Extraer códigos de país únicos a partir de las variables que contienen ".i"
idx <- which(grepl(".i", dimnames(gfevd.ssvs)[[2]]))
xx_all <- dimnames(gfevd.ssvs)[[2]][idx]
xx <- unique(substr(xx_all, 12, 13))  # Conservar solo códigos únicos
shares <- matrix(0, ncol=2, nrow=length(xx))
colnames(shares) <- c("Impact", "Long-run")
rownames(shares) <- xx

for(i in 1:length(xx)){
  idx_country <- which(grepl(xx[i], dimnames(gfevd.ssvs)[[2]]))
  # Forzar la extracción a una matriz para evitar el descenso de dimensión
  temp <- as.matrix(gfevd.ssvs[paste0(xx[i], ".i"), idx_country, ])
  own <- colSums(temp)
  shares[i,] <- own[c(1, 24)]
}





# Set graphical parameters for a two-panel layout
par(mfrow=c(2,1), mar=c(3,3,2,2))

# Plot for "Impact"
impacts <- sort(shares[,"Impact"])
bp1 <- barplot(impacts, las=2, ylim=c(0,1.1), main="h=0")
# Add data labels above each bar
text(x = bp1, y = impacts + 0.05, labels = round(impacts, 2), cex = 0.8, col = "black")
box()

# Plot for "Long-run"
longrun <- sort(shares[,"Long-run"])
bp2 <- barplot(longrun, las=2, ylim=c(0,1.1), main="h=24")
# Add data labels above each bar
text(x = bp2, y = longrun + 0.05, labels = round(longrun, 2), cex = 0.8, col = "black")
box()








plot(Data$US$poil)
##############################################################
#SHOCKS 

#POIL

shockinfo_girf_oil <- get_shockinfo("girf")
shockinfo_girf_oil$shock <- 'US.poil'
shockinfo_girf_oil$scale <- 10 
shockinfo_girf_oil$global <- FALSE

irf.girf.oil <- BGVAR::irf(model.ssvs.1,
                           n.ahead = 24,
                           shockinfo = shockinfo_girf_oil,
                           expert=list(save.store=TRUE))

plot(irf.girf.oil, resp = c('DR.i', 'CR.i', 'HN.i',
                            'GT.i', 'NI.i', 'PA.i'), 
     shock = shockinfo_girf_oil$shock, cumulative = FALSE)

plot(irf.girf.oil, resp = "NI", shock = shockinfo_girf_oil$shock, cumulative = TRUE)

plot(irf.girf.oil, resp = 'US.i', 
     shock = shockinfo_girf_oil$shock)

names(irf.girf.oil)

irf_data <- irf.girf.oil$posterior

library(openxlsx)

# Convertir la matriz a data frame, conservando los nombres de filas
data_to_export <- as.data.frame(irf_data)
data_to_export$Variable <- rownames(irf_data)  # Agregar nombres de filas como una columna
data_to_export <- data_to_export[, c(ncol(data_to_export), 1:(ncol(data_to_export)-1))]  # Reordenar para que la columna de variables quede a la izquierda

# Exportar a Excel
write.xlsx(data_to_export, file = "irf_data.xlsx", rowNames = FALSE)


##############################################################
#SHOCKS 

#PAL

shockinfo_girf_pal <- get_shockinfo("girf")
shockinfo_girf_pal$shock <- 'US.pal'
shockinfo_girf_pal$scale <- 10 
shockinfo_girf_pal$global <- FALSE

irf.girf.pal <- BGVAR::irf(model.ssvs.1,
                           n.ahead = 24,
                           shockinfo = shockinfo_girf_pal,
                           expert=list(save.store=TRUE))

plot(irf.girf.pal, resp = c('DR.i', 'CR.i', 'HN.i',
                            'GT.i', 'NI.i', 'PA.i'), 
     shock = shockinfo_girf_pal$shock, cumulative = FALSE)

plot(irf.girf.pal, resp = "NI", 
     shock = shockinfo_girf_pal$shock, cumulative = FALSE,
     quantiles = c(0.1, 0.16, 0.5, 0.84, 0.9))

?plot.bgvar.irf

irf_data_pal <- irf.girf.oil$posterior

library(openxlsx)

# Convertir la matriz a data frame, conservando los nombres de filas
data_to_export <- as.data.frame(irf_data)
data_to_export$Variable <- rownames(irf_data)  # Agregar nombres de filas como una columna
data_to_export <- data_to_export[, c(ncol(data_to_export), 1:(ncol(data_to_export)-1))]  # Reordenar para que la columna de variables quede a la izquierda

# Exportar a Excel
write.xlsx(data_to_export, file = "irf_data.xlsx", rowNames = FALSE)


library(ggplot2)

# Crear un data frame con los cuantiles relevantes
df1 <- data.frame(
  Horizon = rep(0:(dim(irf.girf.pal$posterior)[2] - 1), times = 3),
  Response = c(irf.girf.pal$posterior["NI.i", , 1, "Q50"],
               irf.girf.pal$posterior["NI.i", , 1, "Q16"],
               irf.girf.pal$posterior["NI.i", , 1, "Q84"]),
  Quantile = rep(c("Q50", "Q16", "Q84"), each = dim(irf.girf.pal$posterior)[2])
)

# Reshape para graficar bandas de incertidumbre
df_wide <- reshape(df1, idvar = "Horizon", timevar = "Quantile", direction = "wide")

# Graficar en ggplot2
ggplot(df_wide, aes(x = Horizon)) +
  # Relleno del intervalo de confianza (Q16 a Q84)
  geom_ribbon(aes(ymin = Response.Q16, ymax = Response.Q84), fill = "blue", alpha = 0.3) +
  # Línea mediana posterior (Q50) más gruesa
  geom_line(aes(y = Response.Q50), color = "blue", size = 1.2) +
  # Estilo
  labs(title = "Impulse Response Function (IRF) - Nicaragua",
       x = "Horizon", y = "Response") +
  theme_minimal()

# str(irf.girf.pal$posterior)
# dimnames(irf.girf.pal$posterior)





##############################################################
#SHOCKS 

#US.i

shockinfo_girf_US.i <- get_shockinfo("girf")
shockinfo_girf_US.i$shock <- 'US.i'
shockinfo_girf_US.i$scale <- 1 
shockinfo_girf_US.i$global <- FALSE

irf.girf.us.i <- BGVAR::irf(model.ssvs.1,
                           n.ahead = 24,
                           shockinfo = shockinfo_girf_US.i,
                           expert=list(save.store=TRUE))

plot(irf.girf.us.i, resp = c('DR.i', 'CR.i', 'HN.i',
                            'GT.i', 'NI.i', 'PA.i'), 
     shock = shockinfo_girf_US.i$shock, cumulative = FALSE)

plot(irf.girf.us.i, resp = "NI", 
     shock = shockinfo_girf_US.i$shock, cumulative = FALSE,
     quantiles = c(0.1, 0.16, 0.5, 0.84, 0.9))


irf_data_us.i <- irf.girf.us.i$posterior

library(openxlsx)

# Convertir la matriz a data frame, conservando los nombres de filas
data_to_export_us.i <- as.data.frame(irf_data_us.i)
data_to_export_us.i$Variable <- rownames(irf_data_us.i)  # Agregar nombres de filas como una columna
data_to_export_us.i <- data_to_export_us.i[, c(ncol(data_to_export_us.i), 1:(ncol(data_to_export_us.i)-1))]  # Reordenar para que la columna de variables quede a la izquierda

# Exportar a Excel
write.xlsx(data_to_export_us.i, file = "irf_data_us.i.xlsx", rowNames = FALSE)


##############################################################
#SHOCKS 

#US.y

shockinfo_girf_US.y <- get_shockinfo("girf")
shockinfo_girf_US.y$shock <- 'US.y'
shockinfo_girf_US.y$scale <- 1 
shockinfo_girf_US.y$global <- FALSE

irf.girf.us.y <- BGVAR::irf(model.ssvs.1,
                            n.ahead = 24,
                            shockinfo = shockinfo_girf_US.y,
                            expert=list(save.store=TRUE))

plot(irf.girf.us.y, resp = c('DR.i', 'CR.i', 'HN.i',
                             'GT.i', 'NI.i', 'PA.i'), 
     shock = shockinfo_girf_US.y$shock, cumulative = FALSE)

plot(irf.girf.us.y, resp = "NI", 
     shock = shockinfo_girf_US.y$shock, cumulative = FALSE,
     quantiles = c(0.1, 0.16, 0.5, 0.84, 0.9))


irf_data_us.i <- irf.girf.us.i$posterior

library(openxlsx)

# Convertir la matriz a data frame, conservando los nombres de filas
data_to_export_us.i <- as.data.frame(irf_data_us.i)
data_to_export_us.i$Variable <- rownames(irf_data_us.i)  # Agregar nombres de filas como una columna
data_to_export_us.i <- data_to_export_us.i[, c(ncol(data_to_export_us.i), 1:(ncol(data_to_export_us.i)-1))]  # Reordenar para que la columna de variables quede a la izquierda

# Exportar a Excel
write.xlsx(data_to_export_us.i, file = "irf_data_us.i.xlsx", rowNames = FALSE)

##############################################################
#SHOCKS 

#US.r

shockinfo_girf_US.r <- get_shockinfo("girf")
shockinfo_girf_US.r$shock <- 'US.r'
shockinfo_girf_US.r$scale <- 1 
shockinfo_girf_US.r$global <- FALSE

irf.girf.us.r <- BGVAR::irf(model.ssvs.1,
                            n.ahead = 24,
                            shockinfo = shockinfo_girf_US.r,
                            expert=list(save.store=TRUE))

plot(irf.girf.us.r, resp = c('DR.i', 'CR.i', 'HN.i',
                             'GT.i', 'NI.i', 'PA.i'), 
     shock = shockinfo_girf_US.r$shock, cumulative = FALSE)

plot(irf.girf.us.r, resp = "US", 
     shock = shockinfo_girf_US.r$shock, cumulative = FALSE,
     quantiles = c(0.1, 0.16, 0.5, 0.84, 0.9))


irf_data_us.i <- irf.girf.us.i$posterior

library(openxlsx)

# Convertir la matriz a data frame, conservando los nombres de filas
data_to_export_us.i <- as.data.frame(irf_data_us.i)
data_to_export_us.i$Variable <- rownames(irf_data_us.i)  # Agregar nombres de filas como una columna
data_to_export_us.i <- data_to_export_us.i[, c(ncol(data_to_export_us.i), 1:(ncol(data_to_export_us.i)-1))]  # Reordenar para que la columna de variables quede a la izquierda

# Exportar a Excel
write.xlsx(data_to_export_us.i, file = "irf_data_us.i.xlsx", rowNames = FALSE)




##############################################################
#SHOCKS 

#CN.i

shockinfo_girf_CN.i <- get_shockinfo("girf")
shockinfo_girf_CN.i$shock <- 'CN.i'
shockinfo_girf_CN.i$scale <- 1 
shockinfo_girf_CN.i$global <- FALSE

irf.girf.cn.i <- BGVAR::irf(model.ssvs.1,
                            n.ahead = 24,
                            shockinfo = shockinfo_girf_CN.i,
                            expert=list(save.store=TRUE))

plot(irf.girf.cn.i, resp = c('DR.i', 'CR.i', 'HN.i',
                             'GT.i', 'NI.i', 'PA.i'), 
     shock = shockinfo_girf_CN.i$shock, cumulative = FALSE)

plot(irf.girf.cn.i, resp = "NI", 
     shock = shockinfo_girf_CN.i$shock, cumulative = FALSE,
     quantiles = c(0.1, 0.16, 0.5, 0.84, 0.9))


irf_data_cn.i <- irf.girf.cn.i$posterior

library(openxlsx)

# Convertir la matriz a data frame, conservando los nombres de filas
data_to_export_cn.i <- as.data.frame(irf_data_cn.i)
data_to_export_cn.i$Variable <- rownames(irf_data_cn.i)  # Agregar nombres de filas como una columna
data_to_export_cn.i <- data_to_export_cn.i[, c(ncol(data_to_export_cn.i), 1:(ncol(data_to_export_cn.i)-1))]  # Reordenar para que la columna de variables quede a la izquierda

# Exportar a Excel
write.xlsx(data_to_export_cn.i, file = "irf_data_cn.i.xlsx", rowNames = FALSE)


##############################################################
#SHOCKS 

#UE.i

shockinfo_girf_UE.i <- get_shockinfo("girf")
shockinfo_girf_UE.i$shock <- 'UE.i'
shockinfo_girf_UE.i$scale <- 1 
shockinfo_girf_UE.i$global <- FALSE

irf.girf.ue.i <- BGVAR::irf(model.ssvs.1,
                            n.ahead = 24,
                            shockinfo = shockinfo_girf_UE.i,
                            expert=list(save.store=TRUE))

plot(irf.girf.ue.i, resp = c('DR.i', 'CR.i', 'HN.i',
                             'GT.i', 'NI.i', 'PA.i'), 
     shock = shockinfo_girf_UE.i$shock, cumulative = FALSE)

plot(irf.girf.ue.i, resp = "NI", 
     shock = shockinfo_girf_UE.i$shock, cumulative = FALSE,
     quantiles = c(0.1, 0.16, 0.5, 0.84, 0.9))


irf_data_cn.i <- irf.girf.cn.i$posterior

library(openxlsx)

# Convertir la matriz a data frame, conservando los nombres de filas
data_to_export_cn.i <- as.data.frame(irf_data_cn.i)
data_to_export_cn.i$Variable <- rownames(irf_data_cn.i)  # Agregar nombres de filas como una columna
data_to_export_cn.i <- data_to_export_cn.i[, c(ncol(data_to_export_cn.i), 1:(ncol(data_to_export_cn.i)-1))]  # Reordenar para que la columna de variables quede a la izquierda

# Exportar a Excel
write.xlsx(data_to_export_cn.i, file = "irf_data_cn.i.xlsx", rowNames = FALSE)


##############################################################
#SHOCKS 

#GLOBAL.i

shockinfo_girf_global.i      <- get_shockinfo("girf", nr_rows = 4)
shockinfo_girf_global.i$shock <- c("US.i","UE.i", "CN.i", "UK.i")
shockinfo_girf_global.i$scale <- c(1,1,1,1) 
shockinfo_girf_global.i$global <- TRUE

irf.girf.global.i <- BGVAR::irf(model.ssvs.1,
                            n.ahead = 24,
                            shockinfo = shockinfo_girf_global.i,
                            expert=list(save.store=TRUE))

plot(irf.girf.global.i, resp = c('DR.i', 'CR.i', 'HN.i',
                             'GT.i', 'NI.i', 'PA.i'), 
     shock = "Global.i", cumulative = FALSE)

plot(irf.girf.global.i, resp = "NI", 
     shock = shockinfo_girf_global.i$shock, cumulative = FALSE,
     quantiles = c(0.1, 0.16, 0.5, 0.84, 0.9))


irf_data_global.i <- irf.girf.global.i$posterior

library(openxlsx)

# Convertir la matriz a data frame, conservando los nombres de filas
data_to_export_global.i <- as.data.frame(irf_data_global.i)
data_to_export_global.i$Variable <- rownames(irf_data_global.i)  # Agregar nombres de filas como una columna
data_to_export_global.i <- data_to_export_global.i[, c(ncol(data_to_export_global.i), 1:(ncol(data_to_export_global.i)-1))]  # Reordenar para que la columna de variables quede a la izquierda

# Exportar a Excel
write.xlsx(data_to_export_global.i, file = "irf_data_global.i.xlsx", rowNames = FALSE)


##############################################################
#SHOCKS 

#GLOBAL.y

shockinfo_girf_global.y      <- get_shockinfo("girf", nr_rows = 4)
shockinfo_girf_global.y$shock <- c("US.y","UE.y", "CN.y", "UK.y")
shockinfo_girf_global.y$scale <- c(1,1,1,1) 
shockinfo_girf_global.y$global <- TRUE

irf.girf.global.y <- BGVAR::irf(model.ssvs.1,
                                n.ahead = 24,
                                shockinfo = shockinfo_girf_global.y,
                                expert=list(save.store=TRUE))

plot(irf.girf.global.y, resp = c('DR.y', 'CR.y', 'HN.y',
                                 'GT.y', 'NI.y', 'PA.y'), 
     shock = "Global.y", cumulative = FALSE)

plot(irf.girf.global.i, resp = "NI", 
     shock = shockinfo_girf_global.i$shock, cumulative = FALSE,
     quantiles = c(0.1, 0.16, 0.5, 0.84, 0.9))


irf_data_global.i <- irf.girf.global.i$posterior

library(openxlsx)

# Convertir la matriz a data frame, conservando los nombres de filas
data_to_export_global.i <- as.data.frame(irf_data_global.i)
data_to_export_global.i$Variable <- rownames(irf_data_global.i)  # Agregar nombres de filas como una columna
data_to_export_global.i <- data_to_export_global.i[, c(ncol(data_to_export_global.i), 1:(ncol(data_to_export_global.i)-1))]  # Reordenar para que la columna de variables quede a la izquierda

# Exportar a Excel
write.xlsx(data_to_export_global.i, file = "irf_data_global.i.xlsx", rowNames = FALSE)




##########################################################################
#
# Modelling A global Aggregate demand and Aggregate Supply Shock
#
#########################################################################

# imposes sign restrictions on the cross-section and for a global shock
# Aggregate demand**

shockinfo<-get_shockinfo("sign") #IRF con restricción de signo para las grandes economías
for(cc in c("US","UE","CN")){
  shockinfo<-add_shockinfo(shockinfo, shock=paste0(cc,".y"),
                           restriction=paste0(cc,c(".y",".i")),
                           sign=c(">","<"), horizon=c(4,4), 
                           prob=c(0.5,0.5), scale=c(1,1),
                           global=TRUE)
}


shockinfo # corroborar la información del shock

irf.sign <-irf(model.ssvs.1, n.ahead=24, 
               shockinfo=shockinfo)

plot(irf.sign, resp=c("NI.i"), shock="Global.y", quantiles = c(0.1, 0.16, 0.5, 0.84, 0.9)) #verificar la respuesta de la inflación de Nicaragua ante un shock positivo global de demanda agregada
##############################################

shockinfo<-get_shockinfo("sign")
shockinfo<-add_shockinfo(shockinfo, shock="US.r", 
                         restriction="US.i", sign="<", horizon=4, prob=1, scale=1)


irf.sign<-irf(model.ssvs.1, n.ahead=24, shockinfo=shockinfo, 
              expert=list(MaxTries=100, save.store=FALSE, cores=NULL))


plot(irf.sign, resp=c("US.y","US.i"), shock="US.r", cumulative = FALSE, quantiles = c(0.16, 0.5, 0.84))
plot(irf.sign, resp=c("NI.y","NI.i"), shock="US.i", quantiles = c(0.16, 0.5, 0.84), cumulative = TRUE)

irf_data_global.i <- irf.girf.global.i$posterior

library(openxlsx)

# Convertir la matriz a data frame, conservando los nombres de filas
data_to_export_global.i <- as.data.frame(irf_data_global.i)
data_to_export_global.i$Variable <- rownames(irf_data_global.i)  # Agregar nombres de filas como una columna
data_to_export_global.i <- data_to_export_global.i[, c(ncol(data_to_export_global.i), 1:(ncol(data_to_export_global.i)-1))]  # Reordenar para que la columna de variables quede a la izquierda

# Exportar a Excel
write.xlsx(data_to_export_global.i, file = "irf_data_global.i.xlsx", rowNames = FALSE)






print(model.ssvs.1)


str(residuos_list)

lag.max <- 12
acfs_por_pais <- lapply(residuos_list, function(x) {
  # Convertir a numérico (por si es data frame de 1 columna o similar)
  x <- as.numeric(x)
  # Opcional: eliminar NA (si los hay)
  x <- na.omit(x)
  
  # Evitar acf() en caso de series con longitud < 2
  if(length(x) < 2) return(rep(NA, lag.max))
  
  acf(x, lag.max = lag.max, plot = FALSE)$acf[-1]
})


# 1) Extraer residuos del modelo BGVAR
#    'draws=FALSE' devuelve por defecto los residuos promedio (posterior mean).
residuos_list <- resid(model.ssvs.1, type = "residuals", draws = FALSE)

# 'residuos_list' es una lista, donde cada elemento corresponde
# a la serie de residuos de un país.

# 2) Calcular la ACF para cada país y cada rezago hasta 10
lag.max <- 12
acfs_por_pais <- lapply(residuos_list, function(x) {
  # Usamos la función acf de stats, con plot=FALSE para no graficar aún
  # Retornamos solo las autocorrelaciones en lags 1..lag.max (omitimos el lag 0)
  acf(x, lag.max = lag.max, plot = FALSE)$acf[-1]
})

# Ahora acfs_por_pais es una lista, cada elemento es un vector de long. 'lag.max'
# con las autocorrelaciones de 1..10 para cada país.

# 3) Crear una matriz donde cada fila sea un país y cada columna sea un lag
acfs_mat <- do.call(rbind, acfs_por_pais)
colnames(acfs_mat) <- paste0("lag", 1:lag.max)

# acfs_mat: filas = países, columnas = lags.
# Para el diagrama de cajas "por lag" (eje X = lag, eje Y = valores ACF),
# cada columna se vuelve una "variable" en el boxplot. Basta convertirla en data.frame:

acfs_df <- as.data.frame(acfs_mat)

# 4) Graficar el boxplot por lag
boxplot(acfs_df,
        main = "ACF de los residuos por lag",
        xlab = "Rezago (lag)",
        ylab = "Autocorrelación")
abline(h=0, col="red", lty=2)

############################################################
# Extrae el array 3D con dimensión [draws, tiempo, variables]
resid_country_array <- residuos_list$country  # dim: [51, 93, 86]

# Promedia sobre la dimensión de draws (la primera, '1')
# => obtienes una matriz [tiempo, variables]
resid_country_mean <- apply(resid_country_array, c(2,3), mean)
# dim(resid_country_mean) = [93, 86]
lag.max <- 12
acfs_by_var <- apply(resid_country_mean, 2, function(x) {
  # Asegurarnos de quitar NAs
  x <- na.omit(x)
  # Si hay muy pocos datos, devolvemos NA
  if(length(x) < 2) return(rep(NA, lag.max))
  
  # acf() retorna acf en lags 0..lag.max; tomamos lags 1..lag.max
  acf(x, lag.max=lag.max, plot=FALSE)$acf[-1]
})
# acfs_by_var es una matriz de dimensión [lag.max, nVariables=86].


# Transponemos para que cada columna sea un lag
acfs_df <- as.data.frame(t(acfs_by_var))  
# => ahora acfs_df tiene 86 filas (variables) y 10 columnas (lags)

par(mfrow=c(1,1))  # Asegura un solo gráfico en la ventana

boxplot(acfs_df,
        xlab="Rezago (lag)",
        ylab="Autocorrelación",
        col = "skyblue",       # Color de relleno de las cajas
        border = "darkblue")   # Color del borde de las cajas
abline(h=0, col="red", lty=2)
abline(h=0.2, col="black", lty=1)
abline(h=-0.2, col="black", lty=1)


###########################################################

# Calcular el diagnóstico de convergencia de Geweke
# Esto devuelve un objeto (por ejemplo, con elementos 'country' y 'global')
diagnostico <- conv.diag(model.ssvs.1)
diagnostico$geweke.z

# Revisar la estructura del diagnóstico, por ejemplo:
str(diagnostico)
# Supongamos que diagnostico$country es una matriz/data.frame
# donde cada columna corresponde a los Z-scores de un país

nombres_var <- dimnames(model.ssvs.1$stacked.results$A_large)[[2]]  
head(nombres_var)  # Inspeccionar los primeros elementos

str(model.ssvs.1$stacked.results)
length(nombres_var) == length(diagnostico$geweke.z)

length(nombres_var)  # Número de nombres de variables
length(diagnostico$geweke.z)  # Número de estadísticas Geweke

dim(diagnostico$geweke.z)

dim(diagnostico$geweke.z) <- c(174, length(diagnostico$geweke.z) / 174)
geweke_mean <- apply(diagnostico$geweke.z, 1, mean)
length(geweke_mean)  # ¿Ahora coincide con 174?
geweke_df <- data.frame(Variable = nombres_var, Geweke_Z = geweke_mean)
geweke_df_filtered <- subset(geweke_df, abs(Geweke_Z) > 1.96)
print(geweke_df_filtered)

hist(geweke_mean, breaks = 30, main = "Distribución de los valores de Geweke",
     xlab = "Geweke Z-score", col = "lightblue", border = "black")
abline(v = c(-1.96, 1.96), col = "red", lwd = 2, lty = 2)  # Líneas de referencia

boxplot(geweke_mean, main = "Boxplot de Geweke Z-scores", 
        ylab = "Geweke Z-score", col = "lightblue", border = "black")
abline(h = c(-1.96, 1.96), col = "red", lwd = 2, lty = 2)  # Líneas de referencia

paises <- sapply(strsplit(nombres_var, "\\."), `[`, 1)  # Extrae el país de cada variable

geweke_df <- data.frame(Pais = paises, Geweke_Z = geweke_mean)

geweke_df_filtrado <- subset(geweke_df, !Pais %in% c("cons", "trend"))


ggplot(geweke_df_filtrado, aes(x = Pais, y = Geweke_Z)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  geom_hline(yintercept = c(-1, 1.96), col = "red", linetype = "dashed") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = "País", y = "Geweke Z-score")


model.ssvs.1$stacked.results$trim.info
# look at serial correlation in residuals, 2 lags should be sufficient

Res<-model.ssvs.1$cc.results$res
str(Res)
dim(Res)
class(Res)
Res$CR
sapply(Res, ncol)  # Check number of columns for each element
Res$US <- Res$US[, 1:4]
sapply(Res, ncol)
acf(Res$CR)



all_cols <- unique(unlist(lapply(Res, colnames)))  # Get all unique column names

# Now, bind them together
Res_combined <- do.call(rbind, Res)

Res1 <- data

Res_combined <- do.call(rbind, Res)  # Combine all list elements into one matrix/data frame
colnames(Res_combined) <- colnames(model.ssvs.1$xglobal)
class(Res)
Res1 <- data

colnames(Res)<-colnames(model.ssvs.1$xglobal)
pdf("acf_modelng_infl_exp.pdf")
for(i in 1:ncol(Res)){
  acf(Res[,i],main=paste(colnames(Res)[i]))
}
dev.off()
save(model.ssvs.1,file="model_oil.rda")

test <- load("D:/Documentos/Documents/Thesis/Thesis_MEF/model_oil.rda")

acf(Res$CR)


# Define el máximo lag que deseas considerar
lag.max <- 12

# Calcula el ACF para cada serie de cada país y extrae los valores para lags 1 a 12.
acf_list <- lapply(Res, function(mat) {
  # Para cada columna (variable) en la matriz del país, calcula el acf
  acf_series <- apply(mat, 2, function(series) {
    acf_obj <- acf(series, lag.max = lag.max, plot = FALSE)
    # acf_obj$acf es un arreglo donde el primer elemento es lag 0,
    # extraemos de índice 2 a (lag.max+1) para obtener lags 1 a lag.max.
    as.vector(acf_obj$acf)[2:(lag.max+1)]
  })
  # La función apply devuelve una matriz con filas = lags y columnas = series,
  # la transponemos para que cada fila corresponda a una serie.
  t(acf_series)
})

# Combina todas las series de todos los países en una única matriz.
# Cada fila corresponde a una serie (de un país y una variable) y cada columna a un lag.
all_acf <- do.call(rbind, acf_list)

# Opcional: revisar dimensiones (debe ser (número total de series) x lag.max)
dim(all_acf)

# Genera un boxplot para cada lag (1 a 12).
boxplot(as.data.frame(all_acf), 
        xlab = "Lag", 
        ylab = "Valor de ACF", 
        main = "Boxplot de ACF por Lag (1 a 12)",
        names = paste("Lag", 1:lag.max))

####################################################################

# Calcular las correlaciones de pares entre los residuos de países
pair_cor <- avg.pair.cc(model.ssvs.1)

# Extraer la matriz de correlaciones de residuos (está como caracteres)
resid_cor_mat <- pair_cor$resid.cor

# Convertir la matriz a numérica:
resid_cor_numeric <- as.numeric(as.matrix(resid_cor_mat))

# Tomar el valor absoluto
abs_resid_cor <- abs(resid_cor_numeric)

# Graficar la ECDF de las correlaciones absolutas
plot(ecdf(abs_resid_cor),
     main = "ECDF de correlaciones cruzadas de residuos (por país)",
     xlab = "Correlación absoluta",
     ylab = "Distribución acumulada",
     col = "blue", lwd = 2)
abline(v = 0.30, col = "red", lty = 2)



########################################################

model.ssvs.2<-bgvar(Data=Data,
                    W=W.trade1619,
                    draws=30000,
                    burnin=30000,
                    plag=6,
                    prior="SSVS",
                    hyperpara=NULL, 
                    SV=TRUE,
                    thin=10,
                    Ex=NULL,
                    trend=TRUE,
                    expert=list(save.shrink.store=TRUE),
                    hold.out=0,
                    eigen=1,
                    verbose=TRUE
)


summary(model.ssvs.2)

BGVAR::resid.corr.test(model.ssvs.2, lag.cor = 1)



















# ggplot(geweke_df_filtrado, aes(x = Pais, y = Geweke_Z)) +
#   geom_boxplot(fill = "lightblue", color = "black") +
#   geom_hline(yintercept = c(-1, 1.96), col = "red", linetype = "dashed") + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   labs(title = "Distribución de Z-Scores de Geweke por País", 
#        x = "País", y = "Geweke Z-score")


# rm(model1.hs,model1.ng,model1.ssvs,model2.hs,model2.mn,model2.ng,model2.ssvs)
# rm(fcast,fcast1.ng,fcast1.hs,fcast2.ssvs,fcast2.ng,fcast2.hs,fcast2.mn)
# rm(dic1.hs,dic1.ng,dic1.ssvs,dic2.hs,dic2.mn,dic2.ng,dic2.ssvs)
# rm(lps.hs,lps.ng,lps.ssvs,lps.hs2,lps.ng2,lps.mn2,lps.ssvs2)
# rm(summarize.lps,summarize.dic)
# gc()
# 
# rm(model.1)
#rm(cr.i.adf, cr.y.adf, model.ssvs.1, model.ssvs.1_sum)


###################################################
### code chunk number 9: model.eval2
###################################################
fcast1.ng <- predict(model1.ng, n.ahead=8);lps.ng<-sum(lps(fcast1.ng))
fcast1.mn <- predict(model1.mn, n.ahead=8);lps.mn<-sum(lps(fcast1.mn))
fcast1.hs <- predict(model1.hs, n.ahead=8);lps.hs<-sum(lps(fcast1.hs))
fcast1.ssvs <- predict(model1.ssvs, n.ahead=8);lps.ssvs<-sum(lps(fcast1.ssvs))
fcast2.ssvs <- predict(model2.ssvs, n.ahead=8);lps.ssvs2<-sum(lps(fcast2.ssvs))
fcast2.ng <- predict(model2.ng, n.ahead=8);lps.ng2<-sum(lps(fcast2.ng))
fcast2.mn <- predict(model2.mn, n.ahead=8);lps.mn2<-sum(lps(fcast2.mn))
fcast2.hs <- predict(model2.hs, n.ahead=8);lps.hs2<-sum(lps(fcast2.hs))
dic1.ng<-dic(model1.ng);dic2.ng<-dic(model2.ng)
dic1.mn<-dic(model1.mn);dic2.mn<-dic(model2.mn)
dic1.ssvs<-dic(model1.ssvs);dic2.ssvs<-dic(model2.ssvs)
dic1.hs<-dic(model1.hs);dic2.hs<-dic(model2.hs)

###################################################################

fcast3.ng <- predict(model3.ng, n.ahead = 8); lps.ng3 <- sum(lps(fcast3.ng))
fcast4.ng <- predict(model4.ng, n.ahead = 8); lps.ng4 <- sum(lps(fcast4.ng))
dic3.ng <- dic(model3.ng); dic4.ng <- dic(model4.ng)

###################################################
### code chunk number 10: summary.table1
###################################################
library(xtable)
summarize.lps<-rbind(c(lps.ssvs,lps.ng,lps.mn,lps.hs),
                     c(lps.ssvs2,lps.ng2,lps.mn2,lps.hs2)
)
colnames(summarize.lps)<-c("SSVS","NG","MN","HS")
rownames(summarize.lps)<-c("p=1","p=2")
xtable(summarize.lps, digits = 2, caption = "LPS scores",label="tbl:lps")


###################################################
### code chunk number 11: summary.table2
###################################################
summarize.dic<-rbind(c(dic1.ssvs,dic1.ng,dic1.mn,dic1.hs),
                     c(dic2.ssvs,dic2.ng,dic2.mn,dic2.hs)
)
colnames(summarize.dic)<-c("SSVS","NG","MN","HS")
rownames(summarize.dic)<-c("p=1","p=2")
rel.dic<-summarize.dic/summarize.dic["p=1","NG"]

xtable(rel.dic, digits = 2, caption = "DIC relative to NG (p=1)",label="tbl:dic")

##############################################################
# Model eval by Geweke stats
###########################################################

# Cargar la librería xtable
#library(xtable)

# Obtener los diagnósticos de convergencia para cada modelo
diag1.ssvs <- conv.diag(model1.ssvs)
diag1.ng   <- conv.diag(model1.ng)
diag1.mn   <- conv.diag(model1.mn)
diag1.hs   <- conv.diag(model1.hs)
diag2.ssvs <- conv.diag(model2.ssvs)
diag2.ng   <- conv.diag(model2.ng)
diag2.mn   <- conv.diag(model2.mn)
diag2.hs   <- conv.diag(model2.hs)

# Función para extraer el porcentaje de convergencia
extraer_porcentaje <- function(x) {
  # Supone que x es un character que contiene el mensaje con el porcentaje
  porcentaje <- gsub(".*\\((.*)%\\).*", "\\1", x)
  return(as.numeric(porcentaje))
}

# Extraer el porcentaje de variables con |z| > 1.96 para cada modelo
pct1.ssvs <- extraer_porcentaje(diag1.ssvs)
pct1.ng   <- extraer_porcentaje(diag1.ng)
pct1.mn   <- extraer_porcentaje(diag1.mn)
pct1.hs   <- extraer_porcentaje(diag1.hs)

pct2.ssvs <- extraer_porcentaje(diag2.ssvs)
pct2.ng   <- extraer_porcentaje(diag2.ng)
pct2.mn   <- extraer_porcentaje(diag2.mn)
pct2.hs   <- extraer_porcentaje(diag2.hs)

# Crear una matriz con los resultados
geweke_table <- rbind(
  "p = 1" = c(SSVS = pct1.ssvs, NG = pct1.ng, MN = pct1.mn, HS = pct1.hs),
  "p = 2" = c(SSVS = pct2.ssvs, NG = pct2.ng, MN = pct2.mn, HS = pct2.hs)
)

# Generar el código LaTeX de la tabla
xtable_geweke <- xtable(geweke_table, digits = 2,
                        caption = "Estadístico de Geweke: Porcentaje de variables con |z| > 1.96",
                        label = "tbl:geweke")
print(xtable_geweke, type = "latex")

###########################################################################
# FITTING WITH THE MN PRIOR
#
yfit <- fitted(model1.mn)
plot(model1.mn, global = FALSE, resp = "NI")


#######################################################
#
#SUMMARY DEL MODELO CON PRIOR SELECCIONADO MN
#
###############################################

summary(model1.mn)

# > summary(model1.mn)
# ---------------------------------------------------------------------------
#   Model Info:
#   Prior: Minnesota prior (MN)
# Number of lags for endogenous variables: 1
# Number of lags for weakly exogenous variables: 1
# Number of posterior draws: 5000/2=2500
# Number of stable posterior draws: 1980
# Number of cross-sectional units: 21
# ---------------------------------------------------------------------------
#   Convergence diagnostics
# Geweke statistic:
#   641 out of 7482 variables' z-values exceed the 1.96 threshold (8.57%).
# ---------------------------------------------------------------------------
# F-test, first order serial autocorrelation of cross-unit residuals
# Summary statistics:
# =========  ==========  ======
# \          # p-values  in %  
# =========  ==========  ======
# >0.1       26          30.23%
# 0.05-0.1   6           6.98% 
# 0.01-0.05  6           6.98% 
# <0.01      48          55.81%
# =========  ==========  ======
# ---------------------------------------------------------------------------
# Average pairwise cross-unit correlation of unit-model residuals
# Summary statistics:
# =======  ==========  ===========  ===========  ===========
# \        y           i            r            e          
# =======  ==========  ===========  ===========  ===========
# <0.1     6 (28.57%)  19 (90.48%)  17 (80.95%)  20 (95.24%)
# 0.1-0.2  2 (9.52%)   2 (9.52%)    4 (19.05%)   1 (4.76%)  
# 0.2-0.5  13 (61.9%)  0 (0%)       0 (0%)       0 (0%)     
# >0.5     0 (0%)      0 (0%)       0 (0%)       0 (0%)     
# =======  ==========  ===========  ===========  ===========
# ---------------------------------------------------------------------------

################################################################

###################################################
### code chunk number 13: gfevd_allgemein
###################################################
gfevd.1=gfevd(model1.mn,n.ahead=24,running=TRUE)$FEVD

# get position of NI 
idx<-which(grepl("NI.",dimnames(gfevd.1)[[2]]))
own<-colSums(gfevd.1["NI.i",idx,])
foreign<-colSums(gfevd.1["NI.i",-idx,])
barplot(t(cbind(own,foreign)),legend.text =c("Interno","Externo"))

# Crear data_gfevd con las sumas de contribuciones
data_gfevd <- t(cbind(own, foreign))



library(ggplot2)
library(reshape2)

# Convertir data_gfevd a formato largo
df <- data.frame(
  Trimestre = rep(1:ncol(data_gfevd), each = 2),
  Tipo = rep(c("Interno", "Externo"), times = ncol(data_gfevd)),
  Proporcion = as.vector(data_gfevd)
)

# Crear el gráfico apilado con ggplot2
p <- ggplot(df, aes(x = factor(Trimestre), y = Proporcion, fill = Tipo)) +
  geom_bar(stat = "identity") +  # Barras apiladas
  geom_text(aes(label = round(Proporcion, 2), color = Tipo), 
            position = position_stack(vjust = 0.5), 
            size = 4, show.legend = FALSE) +  # Etiquetas con color mapeado
  labs(title = "", 
       x = "Trimestre", 
       y = "Proporción") +
  scale_fill_manual(values = c("Interno" = "grey25", "Externo" = "grey80")) +
  scale_color_manual(values = c("Interno" = "white", "Externo" = "black")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",    # Ubicar leyenda en la parte superior
        legend.title = element_blank())

# Mostrar el gráfico
print(p)



###################################################
### code chunk number 16: gfevd_CA
###################################################
idx<-which(grepl(".i",dimnames(gfevd.1)[[2]])) # get index of output variables
xx<-dimnames(gfevd.1)[[2]][idx]
xx<-substr(xx,12,13) # get country names
shares<-matrix(0,ncol=2,nrow=length(xx))
colnames(shares)<-c("Impact","Long-run");rownames(shares)<-xx

for(i in 1:length(xx)){
  idx<-which(grepl(xx[i],dimnames(gfevd.1)[[2]]))
  own<-colSums(gfevd.1[paste0(xx[i],".i"),idx,])
  shares[i,]<-own[c(1,24)]
}


###################################################
### code chunk number 17: Canada_longrun
###################################################
par(mfrow=c(2,1),mar=c(3,3,2,2))
barplot(sort(shares[,"Impact"]),las=2,ylim=c(0,1.1),main="h=0");box()
barplot(sort(shares[,"Long-run"]),las=2,ylim=c(0,1.1),main="h=24");box()


##############################################################
# Extraer códigos de país únicos a partir de las variables que contienen ".i"
idx <- which(grepl(".i", dimnames(gfevd.1)[[2]]))
xx_all <- dimnames(gfevd.1)[[2]][idx]
xx <- unique(substr(xx_all, 12, 13))  # Conservar solo códigos únicos
shares <- matrix(0, ncol=2, nrow=length(xx))
colnames(shares) <- c("Impact", "Long-run")
rownames(shares) <- xx

for(i in 1:length(xx)){
  idx_country <- which(grepl(xx[i], dimnames(gfevd.1)[[2]]))
  # Forzar la extracción a una matriz para evitar el descenso de dimensión
  temp <- as.matrix(gfevd.1[paste0(xx[i], ".i"), idx_country, ])
  own <- colSums(temp)
  shares[i,] <- own[c(1, 24)]
}





# Set graphical parameters for a two-panel layout
par(mfrow=c(2,1), mar=c(3,3,2,2))

# Plot for "Impact"
impacts <- sort(shares[,"Impact"])
bp1 <- barplot(impacts, las=2, ylim=c(0,1.1), main="h=0")
# Add data labels above each bar
text(x = bp1, y = impacts + 0.05, labels = round(impacts, 2), cex = 0.8, col = "black")
box()

# Plot for "Long-run"
longrun <- sort(shares[,"Long-run"])
bp2 <- barplot(longrun, las=2, ylim=c(0,1.1), main="h=24")
# Add data labels above each bar
text(x = bp2, y = longrun + 0.05, labels = round(longrun, 2), cex = 0.8, col = "black")
box()





################################################################################
#
# Análisis estructural Shocks
#
########################################################

# Choque del Petróleo

shock_size <- 100

# Oil - GIRF-----------------------------------------------------------------------------------
shockinfo_girf_oil <- get_shockinfo("girf")
shockinfo_girf_oil$shock <- 'US.poil'
shockinfo_girf_oil$scale <- shock_size 
shockinfo_girf_oil$global <- FALSE

irf.girf.oil <- BGVAR::irf(model1.mn,
                           n.ahead = 24,
                           shockinfo = shockinfo_girf_oil)

plot(irf.girf.oil, resp = 'NI.i', 
     shock = 'US.poil')

plot(irf.girf.oil, resp = 'NI.e', 
     shock = 'US.poil')

plot(irf.girf.oil, resp = 'NI.r', 
     shock = 'US.poil')

plot(irf.girf.oil, resp = 'NI.y', 
     shock = 'US.poil')

plot(irf.girf.oil, resp = c('DR.i', 'CR.i', 'HN.i',
                            'GT.i', 'NI.i', 'PA.i'), 
     shock = shockinfo_girf_oil$shock)






irf_data <- irf.girf.oil$posterior



library(openxlsx)

# Convertir la matriz a data frame, conservando los nombres de filas
data_to_export <- as.data.frame(irf_data)
data_to_export$Variable <- rownames(irf_data)  # Agregar nombres de filas como una columna
data_to_export <- data_to_export[, c(ncol(data_to_export), 1:(ncol(data_to_export)-1))]  # Reordenar para que la columna de variables quede a la izquierda

# Exportar a Excel
write.xlsx(data_to_export, file = "irf_data.xlsx", rowNames = FALSE)





# Definir la lista completa de variables de respuesta para los países
countries <- c("CR.i", "DR.i", "GT.i", "HN.i", "NI.i", "PA.i", 
               "SL.i", "AR.i", "BR.i", "CA.i", "CH.i", "CN.i", 
               "CO.i", "EC.i", "IN.i", "JP.i", "KO.i", "ME.i", 
               "UK.i", "US.i", "UE.i")

# Graficar las funciones impulso-respuesta (IRF) para las variables seleccionadas
plot(irf.girf.oil, resp = countries, shock = shockinfo_girf_oil$shock, )



########################################################################

model.ssvs.1<-bgvar(Data=Data,
                    W=W.trade1619,
                    draws=15000,
                    burnin=5000,
                    plag=2,
                    prior="SSVS",
                    hyperpara=NULL, 
                    SV=TRUE,
                    thin=1,
                    Ex=NULL,
                    trend=TRUE,
                    expert=list(save.shrink.store=TRUE),
                    hold.out=0,
                    eigen=1,
                    verbose=TRUE
)

pip <- model.ssvs.1$cc.results$PIP$PIP.avg

summary(model.ssvs.1)

Fmat <- coef(model.ssvs.1)
Smat <- vcov(model.ssvs.1)
lik  <- logLik(model.ssvs.1)

yfit <- fitted(model.ssvs.1)
plot(model.ssvs.1, global=FALSE, resp="NI")

irf.chol<-irf(model.ssvs.1, n.ahead=8, expert=list(save.store=FALSE))
plot(irf.chol)


shockinfo<-get_shockinfo(ident="chol")
?get_shockinfo

?add_shockinfo

shockinfo <-  shockinfo(shockinfo, shock=)

irf.sign.ssvs<-irf(model.ssvs.1, n.ahead=24, shockinfo=shockinfo, expert=list(MaxTries=500))

plot(irf.sign.ssvs, resp=c("AT.ip"), shock="Global.ltir")

gfevd.us.mp=gfevd(model.ssvs.1,n.ahead=24,running=TRUE,cores=4)$FEVD

# get position of EA 
idx<-which(grepl("NI.",dimnames(gfevd.us.mp)[[2]]))
own<-colSums(gfevd.us.mp["NI.i",idx,])
foreign<-colSums(gfevd.us.mp["NI.i",-idx,])
barplot(t(cbind(own,foreign)),legend.text =c("own","foreign"))

fevd.ni.i=fevd(irf.chol, var.slct=c("NI.i"))$FEVD
idx<-which(grepl("NI.",rownames(fevd.ni.i)))
barplot(fevd.ni.i[idx,1,])






















####################################################################

# ESTADÍSTICAS DESCRIPTIVAS

# Convertir cada columna xts a data.frame
data_frame <- as.data.frame(Data)


# Función para calcular estadísticas descriptivas
get_summary_stats <- function(df) {
  data.frame(
    mean = sapply(df, mean, na.rm = TRUE),
    sd = sapply(df, sd, na.rm = TRUE),
    min = sapply(df, min, na.rm = TRUE),
    max = sapply(df, max, na.rm = TRUE)
  )
}

# Calcular estadísticas descriptivas para cada país
summary_table <- get_summary_stats(data_frame)

# Añadir nombres de columnas y países para clarificar
summary_table$Country <- rownames(summary_table)
rownames(summary_table) <- NULL

# Mostrar el resultado
summary_table

library(knitr)
library(kableExtra)

# Presentar tabla en formato HTML o LaTeX
kable(summary_table, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

library(tidyr)

# Usamos pivot_wider para reestructurar la tabla y agrupar las variables por país
grouped_summary <- summary_table %>%
  pivot_wider(names_from = Variable, values_from = c(mean, sd, min, max))

# Mostrar la tabla reestructurada
grouped_summary


####################################################################
#Pruebas de estacionariedad

library(urca)

cr.y.adf <- ur.df(data_frame$CR.y, type = "drift", selectlags = "AIC")
summary(cr.y.adf)

cr.i.adf <- ur.df(data_frame$CR.i, type = "drift", lags = 4)
summary(cr.i.adf)

#######################################################


# Instala el paquete urca si no lo tienes
if(!require(urca)) install.packages("urca")

# Carga las librerías necesarias
library(urca)

# Supongamos que tu DataFrame se llama 'df'
resultados_df <- sapply(names(data_frame), function(col) {
  # Aplica la prueba de Dickey-Fuller aumentado (ADF)
  adf_test <- ur.df(data_frame[[col]], type="drift", selectlags = "AIC")  # Ajusta 'lags' si es necesario
  
  # Obtén el valor de la estadística
  test_stat <- adf_test@teststat[1]
  
  # Obtén los valores críticos
  critical_values <- adf_test@cval[1,]
  
  # Determina si la serie es I(1) a los niveles de significancia del 1%, 5% y 10%
  is_I1 <- ifelse(test_stat > critical_values["10pct"], "I(1)", "Estacionaria")
  
  return(is_I1)
})

# Muestra los resultados
resultados_df

kable(resultados_df, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))






#################################################################
# Luego de cambiar nombre y convertir a DataFrame se llama 'data_frame'
#Realizar prubeas de raiz unitaria


resultados_df <- sapply(names(data_frame), function(col) {
  # Aplica la prueba de Dickey-Fuller aumentado (ADF)
  adf_test <- ur.df(data_frame[[col]], type="drift", selectlags = "AIC")  # Ajusta 'lags' si es necesario
  
  # Obtén el valor p de la prueba
  valor_p <- adf_test@teststat[1]
  critical_values <- adf_test@cval[1,]  # Valores críticos
  
  # Clasificación según el valor p
  if (valor_p < critical_values["5pct"]) {
    return("< 0.05")
  } else if (valor_p >= critical_values["5pct"] & valor_p <= critical_values["10pct"]) {
    return("0.05 - 0.10")
  } else {
    return("> 0.10")
  }
})

# Convierte a DataFrame y cuenta frecuencias por categoría
resultados_df <- as.data.frame(table(resultados_df))
colnames(resultados_df) <- c("Valor p", "Total de variables")

# Agrega interpretaciones
resultados_df$Interpretación <- c(
  "Se rechaza H0 con alta confianza → la serie es estacionaria.",
  "Rechazo de H0 con menor confianza (marginalmente estacionaria). Puede requerir transformación adicional.",
  "No se puede rechazar H0 → la serie es no estacionaria. Se necesita diferenciación u otra transformación."
)

# Genera la tabla en formato HTML con estilo
kable(resultados_df, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

###################################################################



