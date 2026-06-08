
#####################
# PREPARAR EL ENTORNO
################

setwd("D:/Documentos/Documents/RMP/Thesis/Thesis_MEF/BGVAR/DATOS")
#setwd("G:/Otros ordenadores/Mi portátil/RMP/Thesis/Thesis_MEF/BGVAR/DATOS")

pacman::p_load(ggplot2, BGVAR, zoo, openxlsx, readxl) #Cargar paquetes
set.seed(2521)

#Cargar los datos y convertir a lista
Data <- excel_to_list(file = "./Data3.xlsx", first_column_as_time=TRUE, skipsheet=NULL)

#Cargar la matriz de ponderación comercial
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
str(W.trade1619)

model.01 <- bgvar(Data = Data, W = W.trade1619, burnin = 100, draws = 100)
summary(model.01)
###################################################
### code chunk number 1: model.1 probando el default
###################################################
model.1 <- bgvar(Data = Data, W = W.trade1619, plag = 2)
summary(model.1)

#CON DATOS ESTACIONARIOS

# Start estimation of Bayesian Global Vector Autoregression.
# 
# Prior: Normal-Gamma prior.
# Lag order: 2 (endo.), 2 (w. exog.)
# Stochastic volatility: enabled.
# Number of cores used: 1.
# Thinning factor: 1. This means every draw is saved.
# No hyperparameters are chosen, default setting applied.
# 
# Estimation of country models starts...
# Estimation done and took 16 mins 14 seconds.
# Stacking of global model starts... 
# 
# Stacking finished.
# Computation of BGVAR yields 2306 (46%) draws (active trimming).
# Needed time for estimation of bgvar: 26 mins 14 seconds.
# > summary(model.1)
# ---------------------------------------------------------------------------
#   Model Info:
#   Prior: Normal-Gamma prior (NG)
# Number of lags for endogenous variables: 2
# Number of lags for weakly exogenous variables: 2
# Number of posterior draws: 5000/1=5000
# Number of stable posterior draws: 2306
# Number of cross-sectional units: 21
# ---------------------------------------------------------------------------
#   Convergence diagnostics
# Geweke statistic:
#   4856 out of 32896 variables' z-values exceed the 1.96 threshold (14.76%).
# ---------------------------------------------------------------------------
# F-test, first order serial autocorrelation of cross-unit residuals
# Summary statistics:
# =========  ==========  ======
# \          # p-values  in %  
# =========  ==========  ======
# >0.1       58          45.31%
# 0.05-0.1   11          8.59% 
# 0.01-0.05  25          19.53%
# <0.01      34          26.56%
# =========  ==========  ======
# ---------------------------------------------------------------------------
# Average pairwise cross-unit correlation of unit-model residuals
# Summary statistics:
# =======  ===========  ===========  ===========  ===========  ===========  ==========
# \        y            i            r            e            x            m         
# =======  ===========  ===========  ===========  ===========  ===========  ==========
# <0.1     4 (19.05%)   19 (90.48%)  16 (76.19%)  20 (95.24%)  11 (52.38%)  13 (61.9%)
# 0.1-0.2  2 (9.52%)    2 (9.52%)    5 (23.81%)   1 (4.76%)    7 (33.33%)   8 (38.1%) 
# 0.2-0.5  14 (66.67%)  0 (0%)       0 (0%)       0 (0%)       3 (14.29%)   0 (0%)    
# >0.5     1 (4.76%)    0 (0%)       0 (0%)       0 (0%)       0 (0%)       0 (0%)    
# =======  ===========  ===========  ===========  ===========  ===========  ==========
# ---------------------------------------------------------------------------

###################################################
### code chunk number 13: gfevd_allgemein
###################################################
gfevd.1=gfevd(model.1,n.ahead=24,running=TRUE)$FEVD

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

#################################################
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
    geom_text(aes(label = round(Proporcion, 2)),
              position = position_stack(vjust = 0.5),
              size = 2, color = "white") +
    labs(title = pais, x = "Trimestre", y = "Proporción") +
    scale_fill_manual(values = c("Interno" = "grey25", "Externo" = "grey80"),
                      name = "Contribución") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
  
  return(p)
}

# Lista de países
paises <- c("NI", "CR", "DR", "GT", "PA", "HN")

# Generar gráficos para cada país
graficos <- lapply(paises, generar_grafico)

# Organizar los gráficos en un panel
grid.arrange(grobs = graficos, ncol = 2)




###############################################################

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
### code chunk number 17: NIC_longrun
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

# Oil - GIRF-----------------------------------------------------------------------------------
shockinfo_girf_oil <- get_shockinfo("girf")
shockinfo_girf_oil$shock <- 'US.poil'
shockinfo_girf_oil$global <- FALSE

irf.girf.oil <- BGVAR::irf(model.1,
                           n.ahead = 12,
                           shockinfo = shockinfo_girf_oil)

plot(irf.girf.oil, resp = 'NI.i', 
     shock = 'US.poil')

plot(irf.girf.oil, resp = c('DR.i', 'CR.i', 'HN.i',
                            'GT.i', 'NI.i', 'PA.i'), 
     shock = shockinfo_girf_oil$shock)

plot(irf.girf.oil, resp = c('NI.i', 'NI.e', 'NI.r',
                            'NI.y', 'NI.x', 'NI.m'), 
     shock = shockinfo_girf_oil$shock)


rm(irf.girf.oil, shockinfo_girf_oil)

# pa - GIRF-----------------------------------------------------------------------------------
shockinfo_girf_pal <- get_shockinfo("girf")
shockinfo_girf_pal$shock <- 'US.pal'
shockinfo_girf_pal$global <- FALSE

irf.girf.pal <- BGVAR::irf(model.1,
                           n.ahead = 12,
                           shockinfo = shockinfo_girf_pal)

plot(irf.girf.pal, resp = 'NI.i', 
     shock = 'US.pal')

plot(irf.girf.pal, resp = c('DR.i', 'CR.i', 'HN.i',
                            'GT.i', 'NI.i', 'PA.i'), 
     shock = shockinfo_girf_pal$shock)

plot(irf.girf.pa, resp = c('NI.i', 'NI.e', 'NI.r',
                            'NI.y', 'NI.x', 'NI.m'), 
     shock = shockinfo_girf_pal$shock)


rm(irf.girf.pal, shockinfo_girf_pal)

# US.i- GIRF-----------------------------------------------------------------------------------
shockinfo_girf_US_i <- get_shockinfo("girf")
shockinfo_girf_US_i$shock <- 'US.i'
shockinfo_girf_US_i$global <- FALSE

irf.girf.us.i <- BGVAR::irf(model.1,
                           n.ahead = 12,
                           shockinfo = shockinfo_girf_US_i)

plot(irf.girf.us.i, resp = 'NI.i', 
     shock = 'US.i')

plot(irf.girf.us.i, resp = c('DR.i', 'CR.i', 'HN.i',
                            'GT.i', 'NI.i', 'PA.i'), 
     shock = shockinfo_girf_US_i$shock)

plot(irf.girf.us.i, resp = c('NI.i', 'NI.e', 'NI.r',
                           'NI.y', 'NI.x', 'NI.m'), 
     shock = shockinfo_girf_US_i$shock)


rm(irf.girf.us.i, shockinfo_girf_US_i)


# CN.i- GIRF-----------------------------------------------------------------------------------
shockinfo_girf_CN_i <- get_shockinfo("girf")
shockinfo_girf_CN_i$shock <- 'CN.i'
shockinfo_girf_CN_i$global <- FALSE

irf.girf.cn.i <- BGVAR::irf(model.1,
                            n.ahead = 12,
                            shockinfo = shockinfo_girf_CN_i)

plot(irf.girf.cn.i, resp = 'NI.i', 
     shock = 'CN.i')

plot(irf.girf.cn.i, resp = c('DR.i', 'CR.i', 'HN.i',
                             'GT.i', 'NI.i', 'PA.i'), 
     shock = shockinfo_girf_CN_i$shock)

plot(irf.girf.cn.i, resp = c('NI.i', 'NI.e', 'NI.r',
                             'NI.y', 'NI.x', 'NI.m'), 
     shock = shockinfo_girf_CN_i$shock)


rm(irf.girf.cn.i, shockinfo_girf_CN_i)



#############################################################
#GLOBAL INFLATION SHOCK
# Gobal inflation across major economies GIRF-----------------------------------------------------------------------------------
shockinfo_girf_global.i <- get_shockinfo("girf", nr_rows = 4)
shockinfo_girf_global.i$shock<-c("UE.i","US.i","CN.i", "UK.i")
shockinfo_girf_global.i$global <- TRUE
shockinfo_girf_global.i$scale<-1 # corresponds to 1 percentage point or 100bp

irf.girf.global.i <- BGVAR::irf(model.1,
                            n.ahead = 12,
                            shockinfo = shockinfo_girf_global.i)

plot(irf.girf.global.i , resp = 'NI.i', 
     shock="Global.i")

plot(irf.girf.global.i , resp = 'US', 
     shock="Global.i")

plot(irf.girf.global.i , resp = c('DR.i', 'CR.i', 'HN.i',
                             'GT.i', 'NI.i', 'PA.i'), 
     shock = "Global.i")

plot(irf.girf.global.i , resp = 'NI', 
     shock = "Global.i")


rm(irf.girf.us.r, shockinfo_girf_US_r)


#############################################################
#US Trade restrictions modeled as reduction in imports
shockinfo_girf_US.m <- get_shockinfo("girf")
shockinfo_girf_US.m$shock<-"US.m"
shockinfo_girf_US.m$global <- FALSE
shockinfo_girf_US.m$scale<--1 # corresponds to 1 percentage point or 100bp

irf.girf.US.m <- BGVAR::irf(model.1,
                                n.ahead = 12,
                                shockinfo = shockinfo_girf_US.m)

plot(irf.girf.US.m , resp = 'NI.i', 
     shock="US.m")

plot(irf.girf.US.m , resp = 'US', 
     shock="US.m")

plot(irf.girf.US.m , resp = c('DR.y', 'CR.y', 'HN.y',
                                  'GT.y', 'NI.y', 'PA.y'), 
     shock = "US.m")

plot(irf.girf.US.m , resp = 'NI', 
     shock = "US.m")


rm(irf.girf.us.r, shockinfo_girf_US_r)

######################################################################


# 1) Extraer residuos del modelo BGVAR
#    'draws=FALSE' devuelve por defecto los residuos promedio (posterior mean).
residuos_list <- resid(model.1, type = "residuals", draws = FALSE)


# 'residuos_list' es una lista, donde cada elemento corresponde
# a la serie de residuos de un país.

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

boxplot(acfs_df,
        xlab="Rezago (lag)",
        ylab="Autocorrelación",
        col = "skyblue",       # Color de relleno de las cajas
        border = "darkblue")   # Color del borde de las cajas
abline(h=0, col="red", lty=2)
abline(h=0.2, col="black", lty=1)
abline(h=-0.2, col="black", lty=1)


#############################################################
####################################################################

# Calcular las correlaciones de pares entre los residuos de países
pair_cor <- avg.pair.cc(model.1)

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


#rm(irf.sign, shockinfo, shockinfo_girf_oil)

#############################################################
# Demand and supply shocks from US sign restrictions

# Create a sign restrictions object
shockinfo <- get_shockinfo("sign")

# --- Aggregate Demand Shock ---
# We assume the demand shock is identified by a shock to output (US.y).
# According to the table, an Aggregate Demand shock should have:
#   - Output: increases (this is the normalization, so no extra add_shockinfo call is needed for US.y)
#   - Price dynamics (US.Dp): increases (≥ 0)
shockinfo <- add_shockinfo(shockinfo, 
                           shock = "US.y", 
                           restriction = "US.i", 
                           sign = ">",        # Means response must be non-negative (≥ 0)
                           horizon = 4,       # Binding for 4 quarters
                           prob = 1)

# --- Aggregate Supply Shock ---
# We assume the supply shock is identified by a shock to price dynamics (US.Dp).
# According to the table, an Aggregate Supply shock should have:
#   - Output (US.y): increases (≥ 0)
shockinfo <- add_shockinfo(shockinfo, 
                           shock = "US.i", 
                           restriction = "US.y", 
                           sign = ">",        # Must be non-negative (≥ 0)
                           horizon = 4,       # Binding for 4 quarters
                           prob = 1)
#   - Price dynamics (US.Dp): decreases (≤ 0)
shockinfo <- add_shockinfo(shockinfo, 
                           shock = "US.i", 
                           restriction = "US.i", 
                           sign = "<",        # Must be non-positive (≤ 0)
                           horizon = 4,       # Binding for 4 quarters
                           prob = 1)

# Compute impulse response functions (IRFs) using the BGVAR model
irf.sign <- irf(model.1, 
                n.ahead = 12, 
                shockinfo = shockinfo, 
                expert = list(MaxTries = 100, 
                              save.store = FALSE, 
                              cores = NULL))

# Plot the IRFs for each shock:
# For Aggregate Demand shock (identified by US.y)
plot(irf.sign, resp = c("US.y", "US.i"), shock = "US.y", cumulative = TRUE)

# For Aggregate Supply shock (identified by US.Dp)
plot(irf.sign, resp = c("US.y", "US.i"), shock = "US.i", cumulative = TRUE)

#Nicaragua
plot(irf.sign, resp = c("NI.y", "NI.i"), shock ="US.y") #Aggregate Demand Shock
plot(irf.sign, resp = c("NI.y", "NI.i"), shock = "US.i") #Aggregate Supply Shock
#CA
plot(irf.sign, resp = c("NI.i", "DR.i", "CR.i", "GT.i", "HN.i", "PA.i"), shock = "US.i", cumulative = TRUE) #Aggregate Supply Shock


##########################################################################
#
# Modelling A global Aggregate demand and Aggregate Supply Shock
#
#########################################################################

# imposes sign restrictions on the cross-section and for a global shock
# Aggregate demand**

shockinfo<-get_shockinfo("sign") #IRF con restricción de signo para las grandes economías
for(cc in c("US","UE","CN", "UK", "JP")){
  shockinfo<-add_shockinfo(shockinfo, shock=paste0(cc,".y"),
                           restriction=paste0(cc,c(".y",".i")),
                           sign=c(">",">"), horizon=c(4,4), 
                           prob=c(0.5,0.5), scale=c(1,1),
                           global=TRUE)
}


shockinfo # corroborar la información del shock

irf.sign <-irf(model.1, n.ahead=12, 
               shockinfo=shockinfo, 
               expert=list(MaxTries=100, 
                           save.store = FALSE))

plot(irf.sign, resp=c("NI.i"), shock="Global.y") #verificar la respuesta de la inflación de Nicaragua ante un shock positivo global de demanda agregada

























# Start estimation of Bayesian Global Vector Autoregression.
# 
# Prior: Normal-Gamma prior.
# Lag order: 1 (endo.), 1 (w. exog.)
# Stochastic volatility: enabled.
# Number of cores used: 1.
# Thinning factor: 1. This means every draw is saved.
# No hyperparameters are chosen, default setting applied.
# 
# Estimation of country models starts...
# Estimation done and took 11 mins 33 seconds.
# Stacking of global model starts... 
# 
# Stacking finished.
# Computation of BGVAR yields 2128 (43%) draws (active trimming).
# Needed time for estimation of bgvar: 12 mins 12 seconds.
# > summary(model.1)
# ---------------------------------------------------------------------------
#   Model Info:
#   Prior: Normal-Gamma prior (NG)
# Number of lags for endogenous variables: 1
# Number of lags for weakly exogenous variables: 1
# Number of posterior draws: 5000/1=5000
# Number of stable posterior draws: 2128
# Number of cross-sectional units: 21
# ---------------------------------------------------------------------------
#   Convergence diagnostics
# Geweke statistic:
#   2235 out of 16512 variables' z-values exceed the 1.96 threshold (13.54%).
# ---------------------------------------------------------------------------
# F-test, first order serial autocorrelation of cross-unit residuals
# Summary statistics:
# =========  ==========  ======
# \          # p-values  in %  
# =========  ==========  ======
# >0.1       64          50%   
# 0.05-0.1   11          8.59% 
# 0.01-0.05  20          15.62%
# <0.01      33          25.78%
# =========  ==========  ======
# ---------------------------------------------------------------------------
# Average pairwise cross-unit correlation of unit-model residuals
# Summary statistics:
# =======  ===========  =========  ===========  ===========  ===========  ===========
# \        y            i          r            e            x            m          
# =======  ===========  =========  ===========  ===========  ===========  ===========
# <0.1     3 (14.29%)   21 (100%)  16 (76.19%)  20 (95.24%)  7 (33.33%)   5 (23.81%) 
# 0.1-0.2  0 (0%)       0 (0%)     5 (23.81%)   1 (4.76%)    3 (14.29%)   6 (28.57%) 
# 0.2-0.5  6 (28.57%)   0 (0%)     0 (0%)       0 (0%)       11 (52.38%)  10 (47.62%)
# >0.5     12 (57.14%)  0 (0%)     0 (0%)       0 (0%)       0 (0%)       0 (0%)     
# =======  ===========  =========  ===========  ===========  ===========  ===========
# ---------------------------------------------------------------------------
# > 

model.ssvs.1<-bgvar(Data=Data,
                    W=W.trade1619,
                    draws=1000,
                    burnin=1000,
                    plag=1,
                    prior="SSVS",
                    hyperpara=NULL, 
                    SV=TRUE,
                    thin=20,
                    Ex=NULL,
                    trend=TRUE,
                    expert=list(save.shrink.store=TRUE),
                    hold.out=0,
                    eigen=1,
                    verbose=TRUE
)

summary(model.ssvs.1)

model.2 <- bgvar(Data = Data, W = W.trade1619, trend = TRUE)
summary(model.2)

# Start estimation of Bayesian Global Vector Autoregression.
# 
# Prior: Normal-Gamma prior.
# Lag order: 1 (endo.), 1 (w. exog.)
# Stochastic volatility: enabled.
# Number of cores used: 1.
# Thinning factor: 1. This means every draw is saved.
# No hyperparameters are chosen, default setting applied.
# 
# Estimation of country models starts...
# Estimation done and took 60 mins 23 seconds.
# Stacking of global model starts... 
# 
# Stacking finished.
# Computation of BGVAR yields 2586 (52%) draws (active trimming).
# Needed time for estimation of bgvar: 71 mins 14 seconds.
# > summary(model.2)
# ---------------------------------------------------------------------------
#   Model Info:
#   Prior: Normal-Gamma prior (NG)
# Number of lags for endogenous variables: 1
# Number of lags for weakly exogenous variables: 1
# Number of posterior draws: 5000/1=5000
# Number of stable posterior draws: 2586
# Number of cross-sectional units: 21
# ---------------------------------------------------------------------------
#   Convergence diagnostics
# Geweke statistic:
#   2149 out of 16640 variables' z-values exceed the 1.96 threshold (12.91%).
# ---------------------------------------------------------------------------
# F-test, first order serial autocorrelation of cross-unit residuals
# Summary statistics:
# =========  ==========  ======
# \          # p-values  in %  
# =========  ==========  ======
# >0.1       68          53.12%
# 0.05-0.1   12          9.38% 
# 0.01-0.05  19          14.84%
# <0.01      29          22.66%
# =========  ==========  ======
# ---------------------------------------------------------------------------
# Average pairwise cross-unit correlation of unit-model residuals
# Summary statistics:
# =======  ===========  =========  ===========  ===========  ===========  ===========
# \        y            i          r            e            x            m          
# =======  ===========  =========  ===========  ===========  ===========  ===========
# <0.1     3 (14.29%)   21 (100%)  16 (76.19%)  20 (95.24%)  4 (19.05%)   5 (23.81%) 
# 0.1-0.2  0 (0%)       0 (0%)     5 (23.81%)   1 (4.76%)    7 (33.33%)   4 (19.05%) 
# 0.2-0.5  6 (28.57%)   0 (0%)     0 (0%)       0 (0%)       10 (47.62%)  12 (57.14%)
# >0.5     12 (57.14%)  0 (0%)     0 (0%)       0 (0%)       0 (0%)       0 (0%)     
# =======  ===========  =========  ===========  ===========  ===========  ===========
# ---------------------------------------------------------------------------
# > 


model.3 <- bgvar(Data = Data, W = W.trade1619, plag =2)
summary(model.3)


# Start estimation of Bayesian Global Vector Autoregression.
# 
# Prior: Normal-Gamma prior.
# Lag order: 2 (endo.), 2 (w. exog.)
# Stochastic volatility: enabled.
# Number of cores used: 1.
# Thinning factor: 1. This means every draw is saved.
# No hyperparameters are chosen, default setting applied.
# 
# Estimation of country models starts...
# Estimation done and took 14 mins 51 seconds.
# Stacking of global model starts... 
# 
# Stacking finished.
# Computation of BGVAR yields 2770 (55%) draws (active trimming).
# Needed time for estimation of bgvar: 22 mins 8 seconds.
# > summary(model.3)
# ---------------------------------------------------------------------------
#   Model Info:
#   Prior: Normal-Gamma prior (NG)
# Number of lags for endogenous variables: 2
# Number of lags for weakly exogenous variables: 2
# Number of posterior draws: 5000/1=5000
# Number of stable posterior draws: 2770
# Number of cross-sectional units: 21
# ---------------------------------------------------------------------------
#   Convergence diagnostics
# Geweke statistic:
#   2120 out of 14964 variables' z-values exceed the 1.96 threshold (14.17%).
# ---------------------------------------------------------------------------
# F-test, first order serial autocorrelation of cross-unit residuals
# Summary statistics:
# =========  ==========  ======
# \          # p-values  in %  
# =========  ==========  ======
# >0.1       44          51.16%
# 0.05-0.1   12          13.95%
# 0.01-0.05  14          16.28%
# <0.01      16          18.6% 
# =========  ==========  ======
# ---------------------------------------------------------------------------
# Average pairwise cross-unit correlation of unit-model residuals
# Summary statistics:
# =======  ==========  =========  ===========  ===========
# \        y           i          r            e          
# =======  ==========  =========  ===========  ===========
# <0.1     4 (19.05%)  21 (100%)  15 (71.43%)  20 (95.24%)
# 0.1-0.2  0 (0%)      0 (0%)     6 (28.57%)   1 (4.76%)  
# 0.2-0.5  9 (42.86%)  0 (0%)     0 (0%)       0 (0%)     
# >0.5     8 (38.1%)   0 (0%)     0 (0%)       0 (0%)     
# =======  ==========  =========  ===========  ===========
# ---------------------------------------------------------------------------
# > 








