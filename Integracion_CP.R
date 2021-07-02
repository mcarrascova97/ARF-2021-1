# Autor: Mariana Carrasco Vargas
# Clase de Administración de Riesgos Financieros 
# Semestre 2021-1

#### Cálculo de VaR por lo métodos: SH con y sin alisado, Delta-Normal y Componentes principales  ####

# Instalamos librerías
{
  install.packages("dplyr")
  install.packages("quantmod")
  install.packages("PerformanceAnalytics")
}

# Cargamos librerías
{
  library("dplyr")
  library("quantmod")
  library("PerformanceAnalytics")
}

#####################################################################################
#####################################################################################
# Funciones requeridas
{
  # Interpolación alambrada
  interpolar_alam <- function(dias,plazos,tasas){
    
    p_c <- plazos[which.min(abs(plazos-dias))]
    p_l <- plazos[which.min(abs(plazos-dias))+1]
    t_c <- tasas[which.min(abs(plazos-dias))]
    t_l <- tasas[which.min(abs(plazos-dias))+1]
    
    if( dias == 0){
      tasa_r <- tasas[1]
    } else{
      tasa_r <- ((1+(t_c*p_c/360))*((1+(t_l*p_l/360))/(1+(t_c*p_c/360)))^((dias-p_c)/(p_l-p_c))-1)*360/dias 
    }
    
    return(tasa_r)
  }
  
  # Cálculo de VaR ordenando el P&L y buscando el cuantil
  VaR <- function(PnL,alpha){
    matriz <- as.data.frame(matrix(0,nrow = length(PnL),ncol=3))
    colnames(matriz) <- c("P&L","Peso","Acumulado")
    matriz$`P&L` <- PnL
    matriz <- matriz[order(matriz$`P&L`),]
    # hist(matriz$`P&L`,main = "Histograma",xlab = "P&L",col = "purple")
    matriz$Peso <- 1/nrow(matriz)
    for (i in 1:nrow(matriz)) {
      matriz$Acumulado[i] <- sum(matriz$Peso[1:i])
    }
    valor_VaR <- matriz$`P&L`[which.min(abs((1-alpha) - matriz$Acumulado))]
    
    return(valor_VaR)
  }
  
  # Cálculo de CVaR ordenando el P&L
  CVaR <- function(VaR,PnL){
    if (VaR == 0) {
      valor_CVaR <- 0
    } else {
      orden <- PnL[order(PnL)]
      valor_CVaR <- sum(orden[1:which.min(abs(VaR-orden))-1])/(which.min(abs(VaR-orden))-1)
    }
    return(valor_CVaR)
  }
  
  # Cálculo de VaR con alisado
  VaR_ca <- function(v,w=rep(1,length(v)),p){ 
    if ( !is.numeric(w) || length(v) != length(w) ) 
      stop("Los valores y los pesos tienen que tener misma longitud") 
    if ( !is.numeric(p) || any( p<0 | p>1) ) 
      stop("Percentil tiene que ser 0<=p<=1") 
    if ( min(w) < 0 ) stop("Los pesos tiene que ser mayores que 0") 
    ranking <- order(v) 
    sumw <- cumsum(w[ranking]) 
    plist <- sumw/sumw[length(sumw)] 
    valor_VaR <- v[ranking[which.max(plist >= p)]]  
    
    return(valor_VaR)
  } 
  
  # CVaR con alisado
  CVaR_ca <- function(v,w,p){ 
    if ( !is.numeric(w) || length(v) != length(w) ) 
      stop("Los valores y los pesos tienen que tener misma longitud") 
    if ( !is.numeric(p) || any( p<0 | p>1) ) 
      stop("Percentil tiene que ser 0<=p<=1") 
    if ( min(w) < 0 ) stop("Los pesos tiene que ser mayores que 0") 
    ranking <- order(v) 
    sumw <- cumsum(w[ranking]) 
    plist <- sumw/sumw[length(sumw)]
    loss <- v[ranking[which(plist < p)]]
    esc <- w[ranking[which(plist < p)]]
    valor_CVaR <- sum(loss*esc)/(sum(esc))
    if(is.na(valor_CVaR)){
      valor_CVaR <- v[ranking[1]]
    }
    
    return(valor_CVaR)
  } 
  
}

#####################################################################################
#####################################################################################
# Definición de parámetros para valorar
# Acciones, Divisas, Bono Cete, Bono M, Bondes, Futuros TDC e IPC, SWAPS ,Opciones tasa de interés
{
  # Cargamos parámetros para valoración
  dir <- "D:/Mariana/Escuela/Facultad/Titulación/Materias/Administración de Riesgos Financieros/Tareas/Tarea 1"
  setwd(dir)
  fecha_val <- as.Date("2020-03-06","%Y-%m-%d") #### Falta cambiar aquí
  interpolacion <- "alambrada"
  tasa_descuento <- "no" # Para cálculo de Cete
  alpha <- 0.98 # Nivel de confianza
  # Para Componentes Principales
  nb_sims <- 2000
  eta <- .85 #Varianza explicada 
  
  # ACCIONES
  acciones <- c("AMXL.MX", "GCARSOA1.MX", "WALMEX.MX" ) # Nombre de las acciones
  pos_ac <- c(-5000,1000,-1200) # Monto inicial invertido en acciones
  
  # DIVISAS
  divisas <- c("EURUSD=X","GBPUSD=X","USDMXN=X" ) # Nombre de las divisas
  pos_div <- c(1500,700,-600) # Monto inicial invertido en divisas
  dias_hist <- 3660 # Días de historia
  
  # Cete: un cupón cero de cetes (con la curva guber) con nocional de 1500 vencimiento de 180 días
  tasas_guber = read.table(paste0(dir,"/tasa_guber.txt"),header = TRUE)
  pos_cete = 1500
  nom_cete = 10
  venc_cete = 180
  
  # Bono M: un bono M con tasa fija de 6.5% con un cupon de 2.5% anual vencimiento de 3600 días
  tasas_fija_bono_m = read.table(paste0(dir,"/tasa_fija_bono_m.txt"),header = TRUE)
  pos_bono_m = 10000
  nom_bono_m = 100
  venc_bono_m = 3600
  tasa_cupon_fija_bono_m = 0.025
  dias_pago_cupon_bono_m = 360

  # Bondes: un bono corto 1000 bondes con plazo 707 con cupón que paga 28 días (calcular cupón basado en curva de fondeo y descontar con la curva correspondiente)
  # tasas_guber
  tasas_guber_st = read.table(paste0(dir,"/tasa_guber_st.txt"),header = TRUE)
  tasas_fondeo = read.table(paste0(dir,"/tfondeo.txt"),header = TRUE)
  pos_bondes = -1000
  nom_bondes = 100
  dias_pago_cupon_bondes = 28
  venc_bondes = 707
  
  # Forward tipo de cambio: 100 contratos de compra de futuros de peso dólar con un strike de 19.83 vencimiento de 5 días
  tasas_libor <- read.table(paste0(dir,"/tasa_libor.txt"),header = TRUE)
  tasas_fwd <- read.table(paste0(dir,"/tasa_fwd.txt"),header = TRUE)
  pos_fwd_tdc <- 100
  venc_fwd_tdc <- 5
  precio_strike_fwd_tdc <- 19.83
  nom_fwd_tdc <- 1
  moneda_fwd_tdc <- "USDMXN=X"
  
  # Futuro IPC: 50 contratos de venta de futuros del IPC con strike de 49525 vencimiento de 53 días
  # tasas_guber
  tasas_dividendos <- read.table(paste0(dir,"/tasa_dividendos.txt"),header = TRUE)
  IPC <- "^MXX"
  pos_fut_IPC<- -50
  venc_fut_IPC <- 53
  precio_strike_fut_IPC <- 49525
  nom_fut_IPC <- 1
  
  # SWAPS: Un swap largo con nocional de 1600 pagando tasa fija de 5.2% anual (cada 28 días) y recibiendo tasa flotante
  #        de la curva TIIE vencimiento de 588 días, un swap corto con nocional de 1200 pagando tasa variable de la TIIE 
  #        y recibiendo fija de 4.9% vencimiento de 270 días
  tasas_TIIE = read.table(paste0(dir,"/tasa_TIIE_SW_OP.txt"),header = TRUE)
  tasas_DIRS = read.table(paste0(dir,"/tasa_DIRS_SW_OP.txt"),header = TRUE)
  pos_swaps <- c(1600,-1200)
  venc_swaps <- c(588,270)
  tasa_fija_swaps <-c(0.052,0.049)
  paga_o_recibe_swaps <- c(1,1) # "0" si paga y "1" si recibe
  dias_pago_cupon_swaps <- c(28,28)
  
  # Opciones tasa de interés: Dos opciones europeas, una de tasa de interés larga call con strike de 4.4 % vencimiento de 
  #                           1700 días 1000 contratos, y otra put larga de tasa de interés con strike de 4.9% de la tiie 
  #                           nocional de 500 contratos vencimiento 700 días. (se valuará basado en las superficies de 
  #                           volatilidad si es in o out the money y en las curvas de tasa de interés TIIE o Pagarés)
  # tasas_TIIE
  # tasas_DIRS
  tasas_volTIIE = read.table(paste0(dir,"/tvoltiie_opc.txt"),header = TRUE)
  pos_opc_ti <- c(1000, 500)
  plazos_opc_ti <- cbind( 1700, 700) #T-t
  pr_opc_ti <- 28 # plazo de referencia  
  d_base_opc_ti <- 360
  call_o_put_opc_ti <- c(0,1) #si es call (cap) o put (floor)
  precio_strike_opc_ti <- c( 0.044, 0.049)
  nom_opc_ti <- 1
}

#####################################################################################
#####################################################################################
# Carga de datos

{
  # Carga de datos históricos de acciones, divisas e IPC
  start_date = fecha_val - dias_hist #fecha inicial
  nombres_EQ_IPC <- c(acciones,divisas,IPC)
  
  # Cargamos datos hitóricos de Yahoo Finance (series de tiempo)
  historicos <- list()
  for(nombre in nombres_EQ_IPC){
    historicos[[nombre]] <- getSymbols(Symbols = nombre,src = "yahoo",auto.assign = FALSE,from = start_date)
  }

  # Carga de datos Cete
  tasas_guber = read.table(paste0(dir,"/tasa_guber.txt"),header = TRUE)
  nodos_guber <- c(1,7,30,90,180,270,360,720,1080,1440,1800,2160,2520,2880,3240,3600,5400,7200,9000,10800)
  names(tasas_guber) <- c("Date",nodos_guber)
  tasas_guber[,1] <- as.Date(format(strptime(as.character(tasas_guber[,1]),"%Y%m%d"),"%Y-%m-%d"))
  tasas_guber <- tasas_guber[!duplicated(tasas_guber$Date), ]
  
  # Carga de datos Bono M
  tasas_fija_bono_m = read.table(paste0(dir,"/tasa_fija_bono_m.txt"),header = TRUE)
  nodos_fija_bono_m  <- c(1,7,30,90,180,270,360,720,1080,1440,1800,2160,2520,2880,3240,3600,5400,7200,9000,10800)
  names(tasas_fija_bono_m) <- c("Date",nodos_fija_bono_m)
  tasas_fija_bono_m[,1] <- as.Date(format(strptime(as.character(tasas_fija_bono_m[,1]),"%Y%m%d"),"%Y-%m-%d"))
  tasas_fija_bono_m <- tasas_fija_bono_m[!duplicated(tasas_fija_bono_m$Date), ]
  
  # Carga de datos Bondes
  tasas_fondeo = read.table(paste0(dir,"/tfondeo.txt"),header = TRUE)
  tasas_fondeo$fecha <- as.Date(format(strptime(as.character(tasas_fondeo$fecha),"%Y%m%d"),"%Y-%m-%d"))
  tasas_fondeo$tfondeo <- tasas_fondeo$tfondeo/100
  names(tasas_fondeo) <- c("Date","tfondeo")
  tasas_fondeo <- tasas_fondeo[!duplicated(tasas_fondeo$Date), ]
  tasas_fondeo_orig <- tasas_fondeo
  
  tasas_guber_st = read.table(paste0(dir,"/tasa_guber_st.txt"),header = TRUE)
  nodos_guber_st <- c(1,7,30,90,180,270,360,720,1080,1440,1800,2160,2520,2880,3240,3600,5400,7200,9000,10800)
  names(tasas_guber_st) <- c("Date",nodos_guber_st)
  tasas_guber_st[,1] <- as.Date(format(strptime(as.character(tasas_guber_st[,1]),"%Y%m%d"),"%Y-%m-%d"))
  tasas_guber_st <- tasas_guber_st[!duplicated(tasas_guber_st$Date), ]
  
  # Carga de datos Forward TDC
  tasas_libor <- read.table(paste0(dir,"/tasa_libor.txt"),header = TRUE)
  nodos_libor <- c(1,7,30,90,180,270,360,720,1080,1440,1800,2160,2520,2880,3240,3600)
  names(tasas_libor) <- c("Date",nodos_libor)
  tasas_libor[,1] <- as.Date(format(strptime(as.character(tasas_libor[,1]),"%Y%m%d"),"%Y-%m-%d"))
  tasas_libor[,2:ncol(tasas_libor)] <- tasas_libor[,2:ncol(tasas_libor)]/100
  tasas_libor <- tasas_libor[!duplicated(tasas_libor$Date), ]

  tasas_fwd <- read.table(paste0(dir,"/tasa_fwd.txt"),header = TRUE)
  nodos_fwd <- c(1,7,30,90,180,270,360,720,1080,1440,1800,2160,2520,2880,3240,3600)
  names(tasas_fwd) <- c("Date",nodos_fwd)
  tasas_fwd[,1] <- as.Date(format(strptime(as.character(tasas_fwd[,1]),"%Y%m%d"),"%Y-%m-%d"))
  tasas_fwd[,2:ncol(tasas_fwd)] <- tasas_fwd[,2:ncol(tasas_fwd)]/100
  tasas_fwd <- tasas_fwd[!duplicated(tasas_fwd$Date), ]
  
  # Carga de datos Futuro IPC
  tasas_dividendos <- read.table(paste0(dir,"/tasa_dividendos.txt"),header = TRUE)
  nodos_dividendos <- c(1,7,30,90,180,270,360,720,1080,1440,1800,2160,2520,2880,3240,3600)
  names(tasas_dividendos) <- c("Date",nodos_dividendos)
  tasas_dividendos[,1] <- as.Date(format(strptime(as.character(tasas_dividendos[,1]),"%Y%m%d"),"%Y-%m-%d"))
  tasas_dividendos[,2:ncol(tasas_dividendos)] <- tasas_dividendos[,2:ncol(tasas_dividendos)]/100
  tasas_dividendos <- tasas_dividendos[!duplicated(tasas_dividendos$Date), ]
  
  # Carga de datos SWAPS
  tasas_TIIE = read.table(paste0(dir,"/tasa_TIIE_SW_OP.txt"),header = TRUE)
  nodos_TIIE <- c(1,7,30,90,180,270,360,720,1080,1440,1800,2160,2520,2880,3240,3600)
  names(tasas_TIIE) <- c("Date",nodos_TIIE)
  tasas_TIIE[,1] <- as.Date(format(strptime(as.character(tasas_TIIE[,1]),"%Y%m%d"),"%Y-%m-%d"))
  tasas_TIIE[,2:ncol(tasas_TIIE)] <- tasas_TIIE[,2:ncol(tasas_TIIE)]/100
  tasas_TIIE <- tasas_TIIE[!duplicated(tasas_TIIE$Date), ]
  
  tasas_DIRS = read.table(paste0(dir,"/tasa_DIRS_SW_OP.txt"),header = TRUE)
  nodos_DIRS <- c(1,7,30,90,180,270,360,720,1080,1440,1800,2160,2520,2880,3240,3600)
  names(tasas_DIRS) <- c("Date",nodos_DIRS)
  tasas_DIRS[,1] <- as.Date(format(strptime(as.character(tasas_DIRS[,1]),"%Y%m%d"),"%Y-%m-%d"))
  tasas_DIRS[,2:ncol(tasas_DIRS)] <- tasas_DIRS[,2:ncol(tasas_DIRS)]/100
  tasas_DIRS <- tasas_DIRS[!duplicated(tasas_DIRS$Date), ]
  
  # Carga de datos Opciones tasa de interés
  tasas_volTIIE = read.table(paste0(dir,"/tvoltiie_opc.txt"),header = TRUE)
  nodos_volTIIE <- c(28,91,182,364,728,1092,1820,3640)
  names(tasas_volTIIE) <- c("Date",nodos_volTIIE)
  tasas_volTIIE[,1] <- as.Date(format(strptime(as.character(tasas_volTIIE[,1]),"%Y%m%d"),"%Y-%m-%d"))
  tasas_volTIIE <- tasas_volTIIE[!duplicated(tasas_volTIIE$Date), ]
}

#####################################################################################
#####################################################################################
# Intersección de insumos
{
# Intersección de históricos de acciones, divisas e IPC
  #Convertimos de serie de tiempo a data frame
  matrices_hist <- list()
  for(nombre in nombres_EQ_IPC){
    matrices_hist[[nombre]] <- data.frame(date = index(historicos[[nombre]]), coredata(historicos[[nombre]]))
    colnames(matrices_hist[[nombre]]) <- c("Date","Open","High","Low","Close","Volume","Adjusted")
  }
  
  # Determinamos las fechas que vamos a intersectar
  fechas_EQ <- list()
  for (nombre in nombres_EQ_IPC) {
    fechas_EQ[[nombre]] = unique(matrices_hist[[nombre]]$Date)
  }
  
  # También traemos las fechas de las tasa
  tasas <- list("guber" = tasas_guber,"fija_bono_m" = tasas_fija_bono_m,"guber_st" = tasas_guber_st, "fondeo" = tasas_fondeo,"libor" = tasas_libor, "fwd" = tasas_fwd, "dividendos" = tasas_dividendos,"TIIE" = tasas_TIIE,"DIRS" = tasas_DIRS,"volTIIE" = tasas_volTIIE)
  fechas_tasas <- list()
  for (tasa in 1:length(tasas)) {
    fechas_tasas[[tasa]] = unique(tasas[[tasa]][,1])
  }
  fechas <- c(fechas_EQ,fechas_tasas)
  
  # Intersectamos
  inter_fechas <- as.Date.numeric(Reduce(intersect,fechas),origin = "1970-01-01")
  
  # Cruzamos con el campo Close de todos los activos
  interseccion <- data.frame(dates = inter_fechas)
  
  for (nombre in nombres_EQ_IPC) {
    matrices_hist[[nombre]][[nombre]] = matrices_hist[[nombre]]$Close
  }
  
  cruce <- interseccion %>% left_join(select(matrices_hist[["AMXL.MX"]],Date,"AMXL.MX"),by = c("dates" = "Date")) %>% 
    left_join(select(matrices_hist[["GCARSOA1.MX"]],Date,"GCARSOA1.MX"),by = c("dates" = "Date")) %>%
    left_join(select(matrices_hist[["WALMEX.MX"]],Date,"WALMEX.MX"),by = c("dates" = "Date")) %>%
    left_join(select(matrices_hist[["EURUSD=X"]],Date,"EURUSD=X"),by = c("dates" = "Date")) %>% 
    left_join(select(matrices_hist[["GBPUSD=X"]],Date,"GBPUSD=X"),by = c("dates" = "Date")) %>% 
    left_join(select(matrices_hist[["USDMXN=X"]],Date,"USDMXN=X"),by = c("dates" = "Date")) %>% 
    left_join(select(matrices_hist[["^MXX"]],Date,"^MXX"),by = c("dates" = "Date"))
  
  # Quitamos NA's
  cruce_completo <- cruce[-which(is.na(cruce$AMXL.MX) | is.na(cruce$`EURUSD=X`) | is.na(cruce$`GBPUSD=X`) | is.na(cruce$GCARSOA1.MX) | is.na(cruce$`USDMXN=X`) | is.na(cruce$WALMEX.MX) | is.na(cruce$`^MXX`)),]
  inter_final <- data.frame(dates = cruce_completo$dates)
  fechas_final <- inter_final %>% arrange(desc(dates))
  
  # Acomodamos de fecha más reciente a más antigua
  cruce_orden <- cruce_completo %>% arrange(desc(dates))
  n <- nrow(cruce_orden)
  
  # Definimos las tablas de precios de acciones, divisas e IPC
  precios_ac <- as.matrix(cruce_orden[2:4])
  precios_div <- as.matrix(cruce_orden[5:7])
  precios_ac_div <- as.matrix(cruce_orden[2:7])
  precios_IPC <- as.matrix(cruce_orden[8])
  
  # Cruce de fechas de tasas
  vec_tasas <- c("tasas_guber","tasas_fija_bono_m","tasas_guber_st","tasas_fondeo","tasas_libor","tasas_fwd","tasas_dividendos","tasas_TIIE","tasas_DIRS","tasas_volTIIE")
  
  for (tasa in vec_tasas){
    eval(parse(text = paste0(tasa,"<- left_join(inter_final,",tasa,",by = c('dates' = 'Date'))")))
    eval(parse(text = paste0(tasa,"<- ",tasa," %>% arrange(desc(dates))")))
  }
  
}

#####################################################################################
#####################################################################################
# Simulación histórica
# Para todos los instrumentos hay que definir y calcular los siguientes elementos:
#   1.Historico de factores de riesgo
#   2.Vector de precios actual
#   3.Valoración al día actual
# Encontraremos el V0 para cada activo
{
  #########################################################
  # Acciones y divisas
  ac_x0 <- t(as.matrix(precios_ac[1,]))
  div_x0 <- t(as.matrix(precios_div[1,]))
  ac_div_x0 <- t(as.matrix(precios_ac_div[1,]))
  
  x_ac_div <- as.matrix(precios_ac_div)
  
  V0_ac <- pos_ac*ac_x0
  V0_div <- pos_div*div_x0
  V0_ac_div <- cbind(V0_ac,V0_div)
  
  #########################################################
  # Cete
  
  x_cete <- matrix(0,n,1)
  colnames(x_cete) <- "Cete"
  for(i in 1:n){
    x_cete[i] <- as.numeric(interpolar_alam(venc_cete,nodos_guber,tasas_guber[i,-1]))
  }
  cete_x_0 <- x_cete[1,]
  V0_cete <- (pos_cete*nom_cete)/(1+ cete_x_0*venc_cete/360)
  
  #########################################################
  # Bono M
  
  x_bono_m <- matrix(0,n,1)
  for(i in 1:n){
    x_bono_m[i] <- as.numeric(interpolar_alam(venc_bono_m,nodos_fija_bono_m,tasas_fija_bono_m[i,-1]))
  }
  bono_m_x_0 <- x_bono_m[1,]
  
  Valuacion_Bono_M <- function(fecha,tasas_fija_bono_m,pos_bono_m, nom_bono_m, venc_bono_m, tasa_cupon_fija_bono_m, dias_pago_cupon_bono_m){
    # Se calculan los cupones por pagar
      cup_por_pagar_bono_m <- trunc(venc_bono_m/dias_pago_cupon_bono_m)
    
    # Se calculan los días devengados del cupón vigente
    dias_deveng_cupon_bono_m <- dias_pago_cupon_bono_m - (venc_bono_m - trunc(venc_bono_m/dias_pago_cupon_bono_m)*dias_pago_cupon_bono_m)
    
    # Se determinan las fechas en la que se paga cupón y se calcula el valor del bono en cada fecha
    {
      cupon_bono_m <- matrix(0,cup_por_pagar_bono_m,7)
      colnames(cupon_bono_m)<-c("Fecha","Tasa cupón","Flujo","Días cupón","Días transcurridos","Tasa VPN","VPN Cupón")
      cupon_bono_m <- as.data.frame(cupon_bono_m)
      cupon_bono_m$Fecha[1] <- fecha_val + (venc_bono_m - trunc(venc_bono_m/dias_pago_cupon_bono_m)*dias_pago_cupon_bono_m)
      cupon_bono_m$`Tasa cupón`[1] <- tasa_cupon_fija_bono_m
      cupon_bono_m$Flujo[1] <- pos_bono_m*nom_bono_m*tasa_cupon_fija_bono_m*dias_pago_cupon_bono_m/360
      cupon_bono_m$`Días cupón`[1] <- dias_pago_cupon_bono_m
      cupon_bono_m$`Días transcurridos`[1] <- cupon_bono_m$`Días cupón`[1]
      cupon_bono_m$`Tasa VPN`[1] <- as.numeric(interpolar_alam(venc_bono_m,nodos_fija_bono_m,tasas_fija_bono_m[match(fecha_val,tasas_fija_bono_m$dates),-1]))
      cupon_bono_m$`VPN Cupón`[1] <- cupon_bono_m$Flujo[1]/(1+cupon_bono_m$`Tasa VPN`[1]*cupon_bono_m$`Días transcurridos`[1]/360)
      
      for(i in 2:cup_por_pagar_bono_m){
        cupon_bono_m$Fecha[i] <- cupon_bono_m$Fecha[i-1] + dias_pago_cupon_bono_m
        cupon_bono_m$`Tasa cupón`[i] <- tasa_cupon_fija_bono_m
        cupon_bono_m$Flujo[i] <- pos_bono_m*nom_bono_m*tasa_cupon_fija_bono_m*dias_pago_cupon_bono_m/360
        cupon_bono_m$`Días cupón`[i] <- dias_pago_cupon_bono_m
        cupon_bono_m$`Días transcurridos`[i] <- cupon_bono_m$`Días cupón`[i] + cupon_bono_m$`Días transcurridos`[i-1] 
        cupon_bono_m$`Tasa VPN`[i] <- as.numeric(interpolar_alam(venc_bono_m,nodos_fija_bono_m,tasas_fija_bono_m[match(fecha_val,tasas_fija_bono_m$dates),-1]))
        cupon_bono_m$`VPN Cupón`[i] <- cupon_bono_m$Flujo[i]/(1+cupon_bono_m$`Tasa VPN`[i]*cupon_bono_m$`Días transcurridos`[i]/360)
      }
      cupon_bono_m$Flujo[cup_por_pagar_bono_m] <- cupon_bono_m$Flujo[cup_por_pagar_bono_m] + pos_bono_m*nom_bono_m
      cupon_bono_m$`VPN Cupón`[cup_por_pagar_bono_m] <- cupon_bono_m$Flujo[cup_por_pagar_bono_m]/(1+cupon_bono_m$`Tasa VPN`[cup_por_pagar_bono_m]*cupon_bono_m$`Días transcurridos`[cup_por_pagar_bono_m]/360)
      cupon_bono_m$Fecha <- as.Date.numeric(cupon_bono_m$Fecha,origin = "1970-01-01")
      
      V0 <- sum(cupon_bono_m$`VPN Cupón`)
      
      return(V0)
    }
  }
  V0_bono_m <- Valuacion_Bono_M(fecha_val,tasas_fija_bono_m,pos_bono_m, nom_bono_m, venc_bono_m, tasa_cupon_fija_bono_m, dias_pago_cupon_bono_m)
  
  #########################################################
  # Bondes
  
  x_bondes <- matrix(0,n,3)
  colnames(x_bondes) <- c("tasa_guber","tasa_guber_st","tasa_fondeo")
  for(i in 1:nrow(x_bondes)){
    x_bondes[i,1] <- as.numeric(interpolar_alam(venc_bondes,nodos_guber,tasas_guber[i,-1]))
    x_bondes[i,2] <- as.numeric(interpolar_alam(venc_bondes,nodos_guber_st,tasas_guber_st[i,-1]))
    x_bondes[i,3] <- tasas_fondeo$tfondeo[i]
  }
  
  bondes_x_0 <- x_bondes[1,]
  
  Valuacion_Bondes <- function(fecha,tasas_guber,tasas_guber_st,tasas_fondeo_orig,pos_bondes,nom_bondes,dias_pago_cupon_bondes,venc_bondes){
    # Se calculan los cupones por pagar
    cup_por_pagar_bondes <- trunc(venc_bondes/dias_pago_cupon_bondes) + 1
    
    # Se calculan los días devengados del cupón vigente
    dias_deveng_cupon_bondes <- dias_pago_cupon_bondes - (venc_bondes - trunc(venc_bondes/dias_pago_cupon_bondes)*dias_pago_cupon_bondes)
    
    # Se busca la tasa de fondeo actual y se calcula la Tasa de interés anual devengada
    
    tasa_fondeo <- tasas_fondeo_orig[match(fecha,tasas_fondeo_orig[,1]),2]
    a <- as.numeric(match(fecha,tasas_fondeo_orig[,1]))
    b <- match(fecha,tasas_fondeo_orig[,1]) - dias_deveng_cupon_bondes + 1
    tasas_tc_dev_bondes <- tasas_fondeo_orig[b:a,2]
    tasa_cupon_dev_bondes <- (prod(1+tasas_tc_dev_bondes/36000)-1)*(36000/dias_deveng_cupon_bondes)
    
    # Se determinan las fechas en la que se paga cupón y se calcula el valor del bono en cada fecha
    
    {
      cupon_bondes <- matrix(0,cup_por_pagar_bondes,8)
      colnames(cupon_bondes)<-c("Fecha","Tasa cupón","Cupón","Días cupón","Días transcurridos","Tasa VPN","Sobretasa VPN","VPN Cupón")
      cupon_bondes <- as.data.frame(cupon_bondes)
      
      cupon_bondes$Fecha[1] <- fecha_val + (venc_bondes - trunc(venc_bondes/dias_pago_cupon_bondes)*dias_pago_cupon_bondes)
      cupon_bondes$`Tasa cupón`[1] <- ((1 + tasa_cupon_dev_bondes*dias_deveng_cupon_bondes/360)*(1+tasa_fondeo/360)^(dias_pago_cupon_bondes - dias_deveng_cupon_bondes) - 1)*360/dias_pago_cupon_bondes
      cupon_bondes$Cupón[1] <- pos_bondes*nom_bondes*cupon_bondes$`Tasa cupón`[1]*dias_pago_cupon_bondes/360
      cupon_bondes$`Días cupón`[1] <- venc_bondes - trunc(venc_bondes/dias_pago_cupon_bondes)*dias_pago_cupon_bondes
      cupon_bondes$`Días transcurridos`[1] <- cupon_bondes$`Días cupón`[1]
      cupon_bondes$`Tasa VPN`[1] <- as.numeric(interpolar_alam(cupon_bondes$`Días transcurridos`[1],nodos_guber,tasas_guber[match(fecha,tasas_guber$dates),-1]))
      cupon_bondes$`Sobretasa VPN`[1] <- as.numeric(interpolar_alam(cupon_bondes$`Días transcurridos`[1],nodos_guber_st,tasas_guber_st[match(fecha,tasas_guber_st$dates),-1]))
      cupon_bondes$`VPN Cupón`[1] <- cupon_bondes$Cupón[1]/(1+(cupon_bondes$`Tasa VPN`[1] + cupon_bondes$`Sobretasa VPN`[1])*cupon_bondes$`Días transcurridos`[1]/360)
      
      for(i in 2:cup_por_pagar_bondes){
        cupon_bondes$Fecha[i] <- cupon_bondes$Fecha[i-1] + dias_pago_cupon_bondes
        cupon_bondes$`Tasa cupón`[i] <- (((1 + tasa_fondeo/360)^dias_pago_cupon_bondes)-1)*(360/dias_pago_cupon_bondes)
        cupon_bondes$Cupón[i] <- pos_bondes*nom_bondes*cupon_bondes$`Tasa cupón`[i]*dias_pago_cupon_bondes/360
        cupon_bondes$`Días cupón`[i] <- dias_pago_cupon_bondes
        cupon_bondes$`Días transcurridos`[i] <- cupon_bondes$`Días cupón`[i] + cupon_bondes$`Días transcurridos`[i-1] 
        cupon_bondes$`Tasa VPN`[i] <- as.numeric(interpolar_alam(cupon_bondes$`Días transcurridos`[i],nodos_guber,tasas_guber[match(fecha,tasas_guber$dates),-1]))
        cupon_bondes$`Sobretasa VPN`[i] <- as.numeric(interpolar_alam(cupon_bondes$`Días transcurridos`[i],nodos_guber_st,tasas_guber_st[match(fecha,tasas_guber_st$dates),-1]))
        cupon_bondes$`VPN Cupón`[i] <- cupon_bondes$Cupón[i]/(1+(cupon_bondes$`Tasa VPN`[i] + cupon_bondes$`Sobretasa VPN`[i])*cupon_bondes$`Días transcurridos`[i]/360)
      }
      cupon_bondes$Cupón[cup_por_pagar_bondes] <- cupon_bondes$Cupón[cup_por_pagar_bondes] + pos_bondes*nom_bondes
      cupon_bondes$`VPN Cupón`[cup_por_pagar_bondes] <- cupon_bondes$Cupón[cup_por_pagar_bondes]/(1+(cupon_bondes$`Tasa VPN`[cup_por_pagar_bondes]+cupon_bondes$`Sobretasa VPN`[cup_por_pagar_bondes])*cupon_bondes$`Días transcurridos`[cup_por_pagar_bondes]/360)
      cupon_bondes$Fecha <- as.Date.numeric(cupon_bondes$Fecha,origin = "1970-01-01")
      print(cupon_bondes)
      # Se calcula el valor del bono
      V0 <- sum(cupon_bondes$`VPN Cupón`)
      
      return(V0)
    }
  }
  V0_bondes <- Valuacion_Bondes(fecha_val,tasas_guber,tasas_guber_st,tasas_fondeo_orig,pos_bondes,nom_bondes,dias_pago_cupon_bondes,venc_bondes)
  
  #########################################################
  # Forward TDC
  
  x_fwd_tdc <- matrix(0,n,2)
  colnames(x_fwd_tdc) <- c("tasa_libor","tasa_fwd")
  for (i in 1:nrow(x_fwd_tdc)) {
    x_fwd_tdc[i,1] <- as.numeric(interpolar_alam(venc_fwd_tdc,nodos_libor,tasas_libor[i,-1]))
    x_fwd_tdc[i,2] <- as.numeric(interpolar_alam(venc_fwd_tdc,nodos_fwd,tasas_fwd[i,-1]))
  }
  
  fwd_tdc_x_0 <- x_fwd_tdc[1,]
  Valuacion_fwd_tdc <- function(fecha,tasas_libor,tasas_fwd,moneda_fwd_tdc,pos_fwd_tdc,venc_fwd_tdc,precio_strike_fwd_tdc,nom_fwd_tdc){
    tc_spot_fwd <- precios_div[1,match(moneda_fwd_tdc,colnames(precios_div))]
    tasa_dom_fwd <- as.numeric(interpolar_alam(venc_fwd_tdc,nodos_fwd,tasas_fwd[match(fecha,tasas_fwd$dates),-1]))
    tasa_ext_fwd <- ((1 + as.numeric(interpolar_alam(venc_fwd_tdc,nodos_libor,tasas_libor[match(fecha,tasas_fwd$dates),-1])))^(venc_fwd_tdc/180)-1)*360/venc_fwd_tdc
    tc_fwd <- tc_spot_fwd*(1 + tasa_dom_fwd*venc_fwd_tdc/360)/(1 + tasa_ext_fwd*venc_fwd_tdc/360)
    V0 <- pos_fwd_tdc*(tc_fwd - precio_strike_fwd_tdc)/(1 + tasa_dom_fwd*venc_fwd_tdc/360)
    
    return(V0)
  }
  V0_fwd_tdc <- Valuacion_fwd_tdc(fecha_val,tasas_libor,tasas_fwd,moneda_fwd_tdc,pos_fwd_tdc,venc_fwd_tdc,precio_strike_fwd_tdc,nom_fwd_tdc)
  
  #########################################################
  # Futuro IPC
  x_fut_ipc <- matrix(0,n,2)
  colnames(x_fut_ipc) <- c("tasa_guber","tasa_dividendos")
  for (i in 1:nrow(x_fut_ipc)) {
    x_fut_ipc[i,1] <- as.numeric(interpolar_alam(venc_fut_IPC,nodos_guber,tasas_guber[i,-1]))
    x_fut_ipc[i,2] <- as.numeric(interpolar_alam(venc_fut_IPC,nodos_dividendos,tasas_dividendos[i,-1]))
  }
  
  fut_ipc_x_0 <- x_fut_ipc[1,]
  Valuacion_fut_IPC <- function(fecha,tasas_guber,tasas_dividendos,pos_fut_IPC,precio_strike_fut_IPC,nom_fut_IPC){
    IPC_spot <- precios_IPC[1,]
    tasa_dom_IPC <- as.numeric(interpolar_alam(venc_fut_IPC,nodos_guber,tasas_guber[match(fecha,tasas_guber$dates),-1]))
    tasa_div_IPC <- ((1 + as.numeric(interpolar_alam(venc_fut_IPC,nodos_dividendos,tasas_dividendos[match(fecha,tasas_dividendos$dates),-1])))^(venc_fut_IPC/180)-1)*(360/venc_fut_IPC)
    IPC_forward <- IPC_spot*(1 + (tasa_dom_IPC - tasa_div_IPC)*(venc_fut_IPC/360))
    V0 <- pos_fut_IPC*nom_fut_IPC*(IPC_forward - precio_strike_fut_IPC)/(1 + tasa_dom_IPC*venc_fut_IPC/360)
    
    return(V0)
  }
  V0_fut_IPC <- Valuacion_fut_IPC(fecha_val,tasas_guber,tasas_dividendos,pos_fut_IPC,precio_strike_fut_IPC,nom_fut_IPC)
  
  #########################################################
  # SWAPS
  x_swaps <- matrix(0,n,4)
  colnames(x_swaps) <- c("tasa_TIIE_1","tasa_DIRS_1","tasa_TIIE_2","tasa_DIRS_2")
  for (i in 1:nrow(x_fut_ipc)) {
    x_swaps[i,1] <- as.numeric(interpolar_alam(venc_swaps[1],nodos_TIIE,tasas_TIIE[i,-1]))
    x_swaps[i,2] <- as.numeric(interpolar_alam(venc_swaps[1],nodos_DIRS,tasas_DIRS[i,-1]))
    x_swaps[i,3] <- as.numeric(interpolar_alam(venc_swaps[2],nodos_TIIE,tasas_TIIE[i,-1]))
    x_swaps[i,4] <- as.numeric(interpolar_alam(venc_swaps[2],nodos_DIRS,tasas_DIRS[i,-1]))
  }
  
  swaps_x_0 <- x_swaps[1,]
  Valuacion_swap <- function(fecha,tasas_TIIE,tasas_DIRS,pos_swaps,venc_swaps,tasa_fija_swaps,paga_o_recibe_swaps,dias_pago_cupon_swaps){
    # Se calculan los cupones por pagar
    cup_por_pagar_swap <- trunc(venc_swaps/dias_pago_cupon_swaps)
    
    # Se determinan las fechas en la que se paga cupón y se calcula el valor del bono en cada fecha
    {
      cupon_swap <- matrix(0,cup_por_pagar_swap,13)
      colnames(cupon_swap) <- c("Fecha","Cupón Fijo","Días p/Cupón","Días transcurridos","Plazo Corto","Tasa corta p/Cupón","Tasa larga p/Cupón","Tasa Forward Variable","Cupón Variable","Tasa VPN","Pata Fija VPN","Pata Var VPN","VPN Flujos")
      cupon_swap <- as.data.frame(cupon_swap)
      
      cupon_swap$Fecha[1] <- fecha_val + (venc_swaps - trunc(venc_swaps/dias_pago_cupon_swaps)*dias_pago_cupon_swaps)
      cupon_swap$`Cupón Fijo`[1] <- pos_swaps*tasa_fija_swaps*dias_pago_cupon_swaps/360
      cupon_swap$`Días p/Cupón`[1] <- dias_pago_cupon_swaps
      cupon_swap$`Días transcurridos`[1] <- cupon_swap$`Días p/Cupón`[1]
      cupon_swap$`Plazo Corto`[1] <- max(cupon_swap$`Días transcurridos`[1]-dias_pago_cupon_swaps,0)
      cupon_swap$`Tasa corta p/Cupón`[1] <- as.numeric(interpolar_alam(cupon_swap$`Plazo Corto`[1],nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
      cupon_swap$`Tasa larga p/Cupón`[1] <- as.numeric(interpolar_alam(cupon_swap$`Días transcurridos`[1],nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
      cupon_swap$`Tasa Forward Variable`[1] <- (((1+cupon_swap$`Tasa larga p/Cupón`[1]*cupon_swap$`Días transcurridos`[1]/360)/(1+cupon_swap$`Tasa corta p/Cupón`[1]*cupon_swap$`Plazo Corto`[1]/360))-1)*(360/(cupon_swap$`Días transcurridos`[1]-cupon_swap$`Plazo Corto`[1]))
      cupon_swap$`Cupón Variable`[1] <- pos_swaps*cupon_swap$`Tasa Forward Variable`[1]*dias_pago_cupon_swaps/360
      cupon_swap$`Tasa VPN`[1] <- as.numeric(interpolar_alam(cupon_swap$`Días transcurridos`[1],nodos_DIRS,tasas_DIRS[match(fecha,tasas_DIRS$dates),-1]))
      cupon_swap$`Pata Fija VPN`[1] <- (-1)^paga_o_recibe_swaps*(-cupon_swap$`Cupón Fijo`[1])/(1+cupon_swap$`Tasa VPN`[1]*cupon_swap$`Días transcurridos`[1]/360)
      cupon_swap$`Pata Var VPN`[1] <- (-1)^paga_o_recibe_swaps*(cupon_swap$`Cupón Variable`[1])/(1+cupon_swap$`Tasa VPN`[1]*cupon_swap$`Días transcurridos`[1]/360)
      cupon_swap$`VPN Flujos`[1] <- (-1)^paga_o_recibe_swaps*(cupon_swap$`Cupón Variable`[1]-cupon_swap$`Cupón Fijo`[1])/(1+cupon_swap$`Tasa VPN`[1]*cupon_swap$`Días transcurridos`[1]/360)
      
      for(i in 2:cup_por_pagar_swap){
        cupon_swap$Fecha[i] <- cupon_swap$Fecha[i-1] + dias_pago_cupon_swaps
        cupon_swap$`Cupón Fijo`[i] <- pos_swaps*tasa_fija_swaps*dias_pago_cupon_swaps/360
        cupon_swap$`Días p/Cupón`[i] <- dias_pago_cupon_swaps
        cupon_swap$`Días transcurridos`[i] <- cupon_swap$`Días transcurridos`[i-1]  + cupon_swap$`Días p/Cupón`[i]
        cupon_swap$`Plazo Corto`[i] <- max(cupon_swap$`Días transcurridos`[i]-dias_pago_cupon_swaps,0)
        cupon_swap$`Tasa corta p/Cupón`[i] <- as.numeric(interpolar_alam(cupon_swap$`Plazo Corto`[i],nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
        cupon_swap$`Tasa larga p/Cupón`[i] <- as.numeric(interpolar_alam(cupon_swap$`Días transcurridos`[i],nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
        cupon_swap$`Tasa Forward Variable`[i] <- (((1+cupon_swap$`Tasa larga p/Cupón`[i]*cupon_swap$`Días transcurridos`[i]/360)/(1+cupon_swap$`Tasa corta p/Cupón`[i]*cupon_swap$`Plazo Corto`[i]/360))-1)*(360/(cupon_swap$`Días transcurridos`[i]-cupon_swap$`Plazo Corto`[i]))
        cupon_swap$`Cupón Variable`[i] <- pos_swaps*cupon_swap$`Tasa Forward Variable`[i]*dias_pago_cupon_swaps/360
        cupon_swap$`Tasa VPN`[i] <- as.numeric(interpolar_alam(cupon_swap$`Días transcurridos`[i],nodos_DIRS,tasas_DIRS[match(fecha,tasas_DIRS$dates),-1]))
        cupon_swap$`Pata Fija VPN`[i] <- (-1)^paga_o_recibe_swaps*(-cupon_swap$`Cupón Fijo`[i])/(1+cupon_swap$`Tasa VPN`[i]*cupon_swap$`Días transcurridos`[i]/360)
        cupon_swap$`Pata Var VPN`[i] <- (-1)^paga_o_recibe_swaps*(cupon_swap$`Cupón Variable`[i])/(1+cupon_swap$`Tasa VPN`[i]*cupon_swap$`Días transcurridos`[i]/360)
        cupon_swap$`VPN Flujos`[i] <- (-1)^paga_o_recibe_swaps*(cupon_swap$`Cupón Variable`[i]-cupon_swap$`Cupón Fijo`[i])/(1+cupon_swap$`Tasa VPN`[i]*cupon_swap$`Días transcurridos`[i]/360)
      }
      cupon_swap$Fecha <- as.Date.numeric(cupon_swap$Fecha,origin = "1970-01-01")
    }
    # Se calcula el valor del Swap
    V0 <- sum(cupon_swap$`VPN Flujos`)
    
    return(V0)
  }
  
  V0_swap <- rep(0,length(pos_swaps))
  for (i in 1:length(V0_swap)) {
    V0_swap[i] <- Valuacion_swap(fecha_val,tasas_TIIE,tasas_DIRS,pos_swaps[i],venc_swaps[i],tasa_fija_swaps[i],paga_o_recibe_swaps[i],dias_pago_cupon_swaps[i])
  }

  #########################################################
  # Opción tasa de interés
  x_opc_ti <- matrix(0,n,6)
  colnames(x_opc_ti) <- c("tasa_TIIE_1","tasa_DIRS_1","tasa_volTIIE_1","tasa_TIIE_2","tasa_DIRS_2","tasa_volTIIE_2")
  for (i in 1:nrow(x_fut_ipc)) {
    x_opc_ti[i,1] <- as.numeric(interpolar_alam(plazos_opc_ti[1],nodos_TIIE,tasas_TIIE[i,-1]))
    x_opc_ti[i,2] <- as.numeric(interpolar_alam(plazos_opc_ti[1],nodos_DIRS,tasas_DIRS[i,-1]))
    x_opc_ti[i,3] <- as.numeric(interpolar_alam(plazos_opc_ti[1],nodos_volTIIE,tasas_volTIIE[i,-1]))
    x_opc_ti[i,4] <- as.numeric(interpolar_alam(plazos_opc_ti[2],nodos_TIIE,tasas_TIIE[i,-1]))
    x_opc_ti[i,5] <- as.numeric(interpolar_alam(plazos_opc_ti[2],nodos_DIRS,tasas_DIRS[i,-1]))
    x_opc_ti[i,6] <- as.numeric(interpolar_alam(plazos_opc_ti[2],nodos_volTIIE,tasas_volTIIE[i,-1]))
  }
  
  opc_ti_x_0 <- x_opc_ti[1,]
  Valuacion_opc_ti <- function(fecha,tasas_TIIE,tasas_DIRS,tasas_volTIIE,pos_opc_ti,plazos_opc_ti,pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti,precio_strike_opc_ti,nom_opc_ti){
    tasa_spot <- as.numeric(interpolar_alam(plazos_opc_ti,nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
    plazo_largo <- pr_opc_ti + plazos_opc_ti
    tasa_larga <- as.numeric(interpolar_alam(plazo_largo,nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
    tasa_forward <- (((1 + tasa_larga*plazo_largo/d_base_opc_ti)/(1 + tasa_spot*plazos_opc_ti[1]/d_base_opc_ti))-1)*(d_base_opc_ti/pr_opc_ti)
    tasa_dom_cont <- log(1 + as.numeric(interpolar_alam(plazos_opc_ti,nodos_DIRS,tasas_DIRS[match(fecha,tasas_DIRS$dates),-1]))*(plazos_opc_ti/360))*(365/plazos_opc_ti)
    vol <- as.numeric(interpolar_alam(plazos_opc_ti,nodos_volTIIE,tasas_volTIIE[match(fecha,tasas_volTIIE$dates),-1]))
    
    d_1 <- ((log(tasa_forward/precio_strike_opc_ti) + ((vol^2)/2)*(plazos_opc_ti[1]/365)))/(vol*(plazos_opc_ti/365)^(1/2))
    d_2 <- ((log(tasa_forward/precio_strike_opc_ti) - ((vol^2)/2)*(plazos_opc_ti[1]/365)))/(vol*(plazos_opc_ti/365)^(1/2))
    N_d_1 <- pnorm((-1)^call_o_put_opc_ti*d_1,0,1)
    N_d_2 <- pnorm((-1)^call_o_put_opc_ti*d_2,0,1)
    prima_opcion <- (tasa_forward*N_d_1 - precio_strike_opc_ti*N_d_2)*(-1)^call_o_put_opc_ti*exp(-tasa_dom_cont*plazos_opc_ti/365)
    prima_caplet_o_flooret <- prima_opcion*((pr_opc_ti/d_base_opc_ti)/(1 + tasa_forward*pr_opc_ti/d_base_opc_ti))
    V0 <- prima_caplet_o_flooret*pos_opc_ti
    
    return(V0)
  }
  
  V0_opc_ti <- rep(0,length(pos_opc_ti))
  for (i in 1:length(V0_opc_ti)) {
    V0_opc_ti[i] <- Valuacion_opc_ti(fecha_val,tasas_TIIE,tasas_DIRS,tasas_volTIIE,pos_opc_ti[i],plazos_opc_ti[i],pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti[i],precio_strike_opc_ti[i],nom_opc_ti[i])
  }
}

#####################################################################################
#####################################################################################
# Integración de factores y cálculo de riesgo en conjunto, y aplicación de simulación
{
  # DIMENSION DE TODOS LOS INSTRUMENTOS
  # Son 8 instrumentos financieros (9 si separamos acciones y divisas)
  n_if=matrix(0,8,1)
  n_if[1]=ncol(x_ac_div) # acciones y divisas
  n_if[2]=ncol(x_cete) # Bonos
  n_if[3]=ncol(x_bono_m)
  n_if[4]=ncol(x_bondes)
  n_if[5]=ncol(x_fwd_tdc) # Forward tipo de cambio
  n_if[6]=ncol(x_fut_ipc) # Futuro IPC
  n_if[7]=ncol(x_swaps) # swaps
  n_if[8]=ncol(x_opc_ti) # opciones tasa de interés
  
  # Valor del portafolio
  V0_port <- c(V0_ac_div[1,],V0_cete,V0_bono_m,V0_bondes,V0_fwd_tdc,V0_fut_IPC,V0_swap,V0_opc_ti)
  V0_total_port <- sum(V0_port)
 
  # Integración de todos los factores de riesgo del portafolio
  x_port <- cbind(x_ac_div,x_cete,x_bono_m,x_bondes,x_swaps,x_opc_ti,x_fwd_tdc,x_fut_ipc) #Factores de riesgo del portafolios de 8(9) instrumentos financieros

  # Cálculo de variaciones diarias del portafolio
  var_diarias_port <- as.matrix(log(x_port[1:(n-1)]/x_port[2:n]))
  
}

#####################################################################################
# Medición de Riesgo
#Medición de riesgo por instrumento, instrumento-factor de riesgo, instrumento - total

# Cálculo de matriz de pérdidas y ganancias
#####################################################################################
# Acciones y divisas
{
  # Calculamos las variaciones diarias en los rendimientos
  var_diarias_ac <- matrix(0,nrow = n-1,ncol = ncol(precios_ac))
  colnames(var_diarias_ac) <- acciones
  var_diarias_div <- matrix(0,nrow = n-1,ncol = ncol(precios_div))
  colnames(var_diarias_div) <- divisas
  var_diarias_ac_div <- matrix(0,nrow = n-1,ncol = ncol(precios_ac_div))
  colnames(var_diarias_ac_div) <- c(acciones,divisas)
  
  for (j in 1:ncol(var_diarias_ac)) {
    for (i in 1:(n-1)) {
      var_diarias_ac[i,j] <- precios_ac[i,j]/precios_ac[i+1,j] - 1
    }
  }
  
  for (j in 1:ncol(var_diarias_div)) {
    for (i in 1:(n-1)) {
      var_diarias_div[i,j] <- precios_div[i,j]/precios_div[i+1,j] - 1
    }
  }
  
  for (j in 1:ncol(var_diarias_ac_div)) {
    for (i in 1:(n-1)) {
      var_diarias_ac_div[i,j] <- precios_ac_div[i,j]/precios_ac_div[i+1,j] - 1
    }
  }
  
  # Estimamos los escenarios
  esc_ac <- sweep(1 + var_diarias_ac, MARGIN = 2, ac_x0, `*`)
  esc_div <- sweep(1 + var_diarias_div, MARGIN = 2, div_x0, `*`)
  esc_ac_div <- sweep(1 + var_diarias_ac_div, MARGIN = 2, ac_div_x0, `*`)
  
  # Calulamos el valor en posición de cada escenario
  y_ac <- sweep(esc_ac, MARGIN = 2, pos_ac, `*`)
  y_div <- sweep(esc_div, MARGIN = 2, pos_div, `*`)
  y_ac_div <- sweep(esc_ac_div, MARGIN = 2, c(pos_ac,pos_div), `*`)
  
  # Calculamos las pédidas y ganancias (P&L)
  PnL_ac <- matrix(0,nrow(y_ac)-1,ncol(y_ac))
  colnames(PnL_ac) <- acciones
  PnL_div <- matrix(0,nrow(y_div)-1,ncol(y_div))
  colnames(PnL_div) <- divisas
  PnL_ac_div <- matrix(0,nrow(y_ac_div)-1,ncol(y_ac_div))
  colnames(PnL_ac_div) <- c(acciones,divisas)
  
  for (j in 1:ncol(PnL_ac)) {
    for (i in 1:nrow(PnL_ac)) {
      PnL_ac[i,j] <- y_ac[1,j] - y_ac[i+1,j]
    }
  }
  
  for (j in 1:ncol(PnL_div)) {
    for (i in 1:nrow(PnL_div)) {
      PnL_div[i,j] <- y_div[1,j] - y_div[i+1,j]
    }
  }
  
  for (j in 1:ncol(PnL_ac_div)) {
    for (i in 1:nrow(PnL_ac_div)) {
      PnL_ac_div[i,j] <- y_ac_div[1,j] - y_ac_div[i+1,j]
    }
  }
}

#####################################################################################
# Cete
{ 
  # Calculamos las variaciones diarias en las tasas
  var_diarias_cete <- matrix(0,nrow = n-1,ncol = ncol(x_cete))
  colnames(var_diarias_cete) <- "Cete"
  for (i in 1:(n-1)) {
    var_diarias_cete[i,] <- x_cete[i,]/x_cete[i+1,] - 1
  }
  # Estimamos los escenarios
  esc_cete <- sweep(1 + var_diarias_cete, MARGIN = 2, cete_x_0, `*`)

  # Calulamos el valor en posición de cada escenario
  y_cete <- (pos_cete*nom_cete)/(1+ esc_cete*venc_cete/360)

  # Calculamos las pédidas y ganancias (P&L)
  PnL_cete <- matrix(0,nrow(y_cete)-1,ncol(y_cete))
  colnames(PnL_cete) <- "Cete"
  for (i in 1:nrow(PnL_cete)) {
    PnL_cete[i,] <- y_cete[1,] - y_cete[i+1,]
  }
}

#####################################################################################
# Bono M
{
  # Calculamos las variaciones diarias en las tasas
  var_diarias_bono_m <- matrix(0,nrow = n-1,ncol = ncol(x_bono_m))
  colnames(var_diarias_bono_m) <- "Bono M"
  for (i in 1:(n-1)) {
    var_diarias_bono_m[i,] <- x_bono_m[i,]/x_bono_m[i+1,] - 1
  }
  # Estimamos los escenarios
  esc_bono_m <- sweep(1 + var_diarias_bono_m, MARGIN = 2, bono_m_x_0, `*`)
  
  # Calulamos el valor en posición de cada escenario
  y_bono_m <- matrix(0,n-1,1)
  for (i in 1:n-1) {
    y_bono_m[i] <- Valuacion_Bono_M(fechas_final[i+1,],tasas_fija_bono_m,pos_bono_m, nom_bono_m, venc_bono_m, tasa_cupon_fija_bono_m, dias_pago_cupon_bono_m)
  }

  # Calculamos las pédidas y ganancias (P&L)
  PnL_bono_m <- matrix(0,nrow(y_bono_m)-1,1)
  colnames(PnL_bono_m) <- "Bono M"
  for (i in 1:nrow(PnL_bono_m)) {
    PnL_bono_m[i,] <- y_bono_m[1,] - y_bono_m[i+1,]
  }
}


#####################################################################################
# Bonde D
{
  # Calculamos las variaciones diarias en las tasas que vamos a utilizar
  var_diarias_tasa_guber <- matrix(0,nrow = nrow(tasas_guber)-1,ncol = ncol(tasas_guber)-1)
  colnames(var_diarias_tasa_guber) <- colnames(tasas_guber[,-1])
  var_diarias_tasa_guber_st <- matrix(0,nrow = nrow(tasas_guber_st)-1,ncol = ncol(tasas_guber_st)-1)
  colnames(var_diarias_tasa_guber_st) <- colnames(tasas_guber_st[,-1])
  var_diarias_tasa_fondeo <- matrix(0,nrow = nrow(tasas_fondeo_orig)-1,ncol = ncol(tasas_fondeo_orig)-1)
  colnames(var_diarias_tasa_fondeo) <- colnames(tasas_fondeo_orig[,-1])
  
  for (j in 1:ncol(var_diarias_tasa_guber)) {
    for (i in 1:nrow(var_diarias_tasa_guber)) {
      var_diarias_tasa_guber[i,j] <- tasas_guber[i,j+1]/tasas_guber[i+1,j+1] - 1
    }
  }
  
  for (j in 1:ncol(var_diarias_tasa_guber_st)) {
    for (i in 1:nrow(var_diarias_tasa_guber_st)) {
      var_diarias_tasa_guber_st[i,j] <- tasas_guber_st[i,j+1]/tasas_guber_st[i+1,j+1] - 1
    }
  }

  for (j in 1:ncol(var_diarias_tasa_fondeo)) {
    for (i in 1:nrow(var_diarias_tasa_fondeo)) {
      var_diarias_tasa_fondeo[i,j] <- tasas_fondeo_orig[i,j+1]/tasas_fondeo_orig[i+1,j+1] - 1
    }
  }
  
  var_diarias_tfondeo <- matrix(0,nrow = nrow(tasas_fondeo)-1,ncol = ncol(tasas_fondeo)-1)
  colnames(var_diarias_tfondeo) <- colnames(tasas_fondeo[,-1])
  for (j in 1:ncol(var_diarias_tfondeo)) {
    for (i in 1:nrow(var_diarias_tfondeo)) {
      var_diarias_tfondeo[i,j] <- tasas_fondeo[i,j+1]/tasas_fondeo[i+1,j+1] - 1
    }
  }
  
  # Estimamos los escenarios
  esc_tasa_guber <- sweep(1 + var_diarias_tasa_guber, MARGIN = 2, as.matrix(tasas_guber[1,-1]), `*`)
  esc_tasa_guber <- as.data.frame(cbind(tasas_guber$dates[-1],esc_tasa_guber))
  colnames(esc_tasa_guber) <- colnames(tasas_guber)
  esc_tasa_guber$dates <- as.Date.numeric(esc_tasa_guber[,1],origin = "1970-01-01")
  esc_tasa_guber_st <- sweep(1 + var_diarias_tasa_guber_st, MARGIN = 2, as.matrix(tasas_guber_st[1,-1]), `*`)
  esc_tasa_guber_st <- as.data.frame(cbind(tasas_guber_st$dates[-1],esc_tasa_guber_st))
  colnames(esc_tasa_guber_st) <- colnames(tasas_guber_st)
  esc_tasa_guber_st$dates <- as.Date.numeric(esc_tasa_guber_st[,1],origin = "1970-01-01")
  esc_tasa_fondeo <- sweep(1 + var_diarias_tasa_fondeo, MARGIN = 2, as.matrix(tasas_fondeo_orig[1,-1]), `*`)
  esc_tasa_fondeo <- as.data.frame(cbind(tasas_fondeo_orig$Date[-1],esc_tasa_fondeo))
  colnames(esc_tasa_fondeo) <- colnames(tasas_fondeo_orig)
  esc_tasa_fondeo$Date <- as.Date.numeric(esc_tasa_fondeo[,1],origin = "1970-01-01")
  
  y_bondes <- matrix(0,n-1,1)
  for (i in 1:nrow(y_bondes)) {
    y_bondes[i,] <- Valuacion_Bondes(fechas_final[i+1,],esc_tasa_guber,esc_tasa_guber_st,esc_tasa_fondeo,pos_bondes,nom_bondes,dias_pago_cupon_bondes,venc_bondes)
  }
  
  # Calculamos las pédidas y ganancias (P&L)
  PnL_bondes <- matrix(0,nrow(y_bondes)-1,1)
  colnames(PnL_bondes) <- "Bondes D"
  for (i in 1:nrow(PnL_bondes)) {
    PnL_bondes[i,] <- y_bondes[1,] - y_bondes[i+1,]
  }
}

#####################################################################################
# Forward TDC
{
  # Calculamos las variaciones diarias en las tasas que vamos a utilizar
  var_diarias_tasa_libor <- matrix(0,nrow = nrow(tasas_libor)-1,ncol = ncol(tasas_libor)-1)
  colnames(var_diarias_tasa_libor) <- colnames(tasas_libor[,-1])
  var_diarias_tasa_fwd <- matrix(0,nrow = nrow(tasas_fwd)-1,ncol = ncol(tasas_fwd)-1)
  colnames(var_diarias_tasa_fwd) <- colnames(tasas_fwd[,-1])
  
  for (j in 1:ncol(var_diarias_tasa_libor)) {
    for (i in 1:nrow(var_diarias_tasa_libor)) {
      var_diarias_tasa_libor[i,j] <- tasas_libor[i,j+1]/tasas_libor[i+1,j+1] - 1
    }
  }
  
  for (j in 1:ncol(var_diarias_tasa_fwd)) {
    for (i in 1:nrow(var_diarias_tasa_fwd)) {
      var_diarias_tasa_fwd[i,j] <- tasas_fwd[i,j+1]/tasas_fwd[i+1,j+1] - 1
    }
  }
  
  # Estimamos los escenarios
  esc_tasa_libor <- sweep(1 + var_diarias_tasa_libor, MARGIN = 2, as.matrix(tasas_libor[1,-1]), `*`)
  esc_tasa_libor <- as.data.frame(cbind(tasas_libor$dates[-1],esc_tasa_libor))
  colnames(esc_tasa_libor) <- colnames(tasas_libor)
  esc_tasa_libor$dates <- as.Date.numeric(esc_tasa_libor[,1],origin = "1970-01-01")
  
  esc_tasa_fwd <- sweep(1 + var_diarias_tasa_fwd, MARGIN = 2, as.matrix(tasas_fwd[1,-1]), `*`)
  esc_tasa_fwd <- as.data.frame(cbind(tasas_fwd$dates[-1],esc_tasa_fwd))
  colnames(esc_tasa_fwd) <- colnames(tasas_fwd)
  esc_tasa_fwd$dates <- as.Date.numeric(esc_tasa_fwd[,1],origin = "1970-01-01")
  
  y_fwd_tdc <- matrix(0,n-1,1)
  for (i in 1:nrow(y_fwd_tdc)) {
    y_fwd_tdc[i,] <- Valuacion_fwd_tdc(fechas_final[i+1,],esc_tasa_libor,esc_tasa_fwd,moneda_fwd_tdc,pos_fwd_tdc,venc_fwd_tdc,precio_strike_fwd_tdc,nom_fwd_tdc)
  }
  
  
  # Calculamos las pédidas y ganancias (P&L)
  PnL_fwd_tdc <- matrix(0,nrow(y_fwd_tdc)-1,1)
  colnames(PnL_fwd_tdc) <- "Forward TDC"
  for (i in 1:nrow(PnL_fwd_tdc)) {
    PnL_fwd_tdc[i,] <- y_fwd_tdc[1,] - y_fwd_tdc[i+1,]
  }
}

#####################################################################################
# Futuro IPC
{
  # Calculamos las variaciones diarias en las tasas que vamos a utilizar
  var_diarias_tasa_dividendos <- matrix(0,nrow = nrow(tasas_dividendos)-1,ncol = ncol(tasas_dividendos)-1)
  colnames(var_diarias_tasa_dividendos) <- colnames(tasas_dividendos[,-1])
  
  for (j in 1:ncol(var_diarias_tasa_dividendos)) {
    for (i in 1:nrow(var_diarias_tasa_dividendos)) {
      var_diarias_tasa_dividendos[i,j] <- tasas_dividendos[i,j+1]/tasas_dividendos[i+1,j+1] - 1
    }
  }
  
  # Estimamos los escenarios
  esc_tasa_dividendos <- sweep(1 + var_diarias_tasa_dividendos, MARGIN = 2, as.matrix(tasas_dividendos[1,-1]), `*`)
  esc_tasa_dividendos <- as.data.frame(cbind(tasas_dividendos$dates[-1],esc_tasa_dividendos))
  colnames(esc_tasa_dividendos) <- colnames(tasas_dividendos)
  esc_tasa_dividendos$dates <- as.Date.numeric(esc_tasa_dividendos[,1],origin = "1970-01-01")
  
  y_fut_ipc <- matrix(0,n-1,1)
  for (i in 1:nrow(y_fut_ipc)) {
    y_fut_ipc[i,] <-  Valuacion_fut_IPC(fechas_final[i+1,],esc_tasa_guber,esc_tasa_dividendos,pos_fut_IPC,precio_strike_fut_IPC,nom_fut_IPC)
  }
  
  
  # Calculamos las pédidas y ganancias (P&L)
  PnL_fut_ipc <- matrix(0,nrow(y_fut_ipc)-1,1)
  colnames(PnL_fut_ipc) <- "Futuro IPC"
  for (i in 1:nrow(PnL_fut_ipc)) {
    PnL_fut_ipc[i,] <- y_fut_ipc[1,] - y_fut_ipc[i+1,]
  }
}

#####################################################################################
# Riesgo de SWAPS
{
  # Calculamos las variaciones diarias en las tasas que vamos a utilizar
  var_diarias_tasa_TIIE <- matrix(0,nrow = nrow(tasas_TIIE)-1,ncol = ncol(tasas_TIIE)-1)
  colnames(var_diarias_tasa_TIIE) <- colnames(tasas_TIIE[,-1])
  var_diarias_tasa_DIRS <- matrix(0,nrow = nrow(tasas_DIRS)-1,ncol = ncol(tasas_DIRS)-1)
  colnames(var_diarias_tasa_DIRS) <- colnames(tasas_DIRS[,-1])
  
  for (j in 1:ncol(var_diarias_tasa_TIIE)) {
    for (i in 1:nrow(var_diarias_tasa_TIIE)) {
      var_diarias_tasa_TIIE[i,j] <- tasas_TIIE[i,j+1]/tasas_TIIE[i+1,j+1] - 1
    }
  }
  
  for (j in 1:ncol(var_diarias_tasa_DIRS)) {
    for (i in 1:nrow(var_diarias_tasa_DIRS)) {
      var_diarias_tasa_DIRS[i,j] <- tasas_DIRS[i,j+1]/tasas_DIRS[i+1,j+1] - 1
    }
  }
  
  # Estimamos los escenarios
  esc_tasa_TIIE <- sweep(1 + var_diarias_tasa_TIIE, MARGIN = 2, as.matrix(tasas_TIIE[1,-1]), `*`)
  esc_tasa_TIIE <- as.data.frame(cbind(tasas_TIIE$dates[-1],esc_tasa_TIIE))
  colnames(esc_tasa_TIIE) <- colnames(tasas_TIIE)
  esc_tasa_TIIE$dates <- as.Date.numeric(esc_tasa_TIIE[,1],origin = "1970-01-01")
  
  esc_tasa_DIRS <- sweep(1 + var_diarias_tasa_DIRS, MARGIN = 2, as.matrix(tasas_DIRS[1,-1]), `*`)
  esc_tasa_DIRS <- as.data.frame(cbind(tasas_DIRS$dates[-1],esc_tasa_DIRS))
  colnames(esc_tasa_DIRS) <- colnames(tasas_DIRS)
  esc_tasa_DIRS$dates <- as.Date.numeric(esc_tasa_DIRS[,1],origin = "1970-01-01")
  
  y_swap_l <- matrix(0,n-1,1)
  for (i in 1:nrow(y_swap_l)) {
    y_swap_l[i,] <- Valuacion_swap(fechas_final[i+1,],tasas_TIIE,tasas_DIRS,pos_swaps[1],venc_swaps[1],tasa_fija_swaps[1],paga_o_recibe_swaps[1],dias_pago_cupon_swaps[1])
  }
  
  y_swap_c <- matrix(0,n-1,1)
  for (i in 1:nrow(y_swap_c)) {
    y_swap_c[i,] <-Valuacion_swap(fechas_final[i+1,],tasas_TIIE,tasas_DIRS,pos_swaps[2],venc_swaps[2],tasa_fija_swaps[2],paga_o_recibe_swaps[2],dias_pago_cupon_swaps[2])
  }
  
  # Calculamos las pédidas y ganancias (P&L)
  PnL_swap_l <- matrix(0,nrow(y_swap_l)-1,1)
  colnames(PnL_swap_l) <- "Swap largo"
  for (i in 1:nrow(PnL_swap_l)) {
    PnL_swap_l[i,] <- y_swap_l[1,] - y_swap_l[i+1,]
  }
  
  PnL_swap_c <- matrix(0,nrow(y_swap_c)-1,1)
  colnames(PnL_swap_c) <- "Swap corto"
  for (i in 1:nrow(PnL_swap_c)) {
    PnL_swap_c[i,] <- y_swap_c[1,] - y_swap_c[i+1,]
  }
}
  
#####################################################################################
# Opción tasa de interés
{
  # Calculamos las variaciones diarias en las tasas que vamos a utilizar
  var_diarias_tasa_volTIIE <- matrix(0,nrow = nrow(tasas_volTIIE)-1,ncol = ncol(tasas_volTIIE)-1)
  colnames(var_diarias_tasa_volTIIE) <- colnames(tasas_volTIIE[,-1])
  
  for (j in 1:ncol(var_diarias_tasa_volTIIE)) {
    for (i in 1:nrow(var_diarias_tasa_volTIIE)) {
      var_diarias_tasa_volTIIE[i,j] <- tasas_volTIIE[i,j+1]/tasas_volTIIE[i+1,j+1] - 1
    }
  }
  
  # Estimamos los escenarios
  esc_tasa_volTIIE <- sweep(1 + var_diarias_tasa_volTIIE, MARGIN = 2, as.matrix(tasas_volTIIE[1,-1]), `*`)
  esc_tasa_volTIIE <- as.data.frame(cbind(tasas_volTIIE$dates[-1],esc_tasa_volTIIE))
  colnames(esc_tasa_volTIIE) <- colnames(tasas_volTIIE)
  esc_tasa_volTIIE$dates <- as.Date.numeric(esc_tasa_volTIIE[,1],origin = "1970-01-01")
  
  y_opc_ti_1 <- matrix(0,n-1,1)
  for (i in 1:nrow(y_opc_ti_1)) {
    y_opc_ti_1[i,] <-  Valuacion_opc_ti(fechas_final[i+1,],esc_tasa_TIIE,esc_tasa_DIRS,esc_tasa_volTIIE,pos_opc_ti[1],plazos_opc_ti[1],pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti[1],precio_strike_opc_ti[1],nom_opc_ti[1])
  }
  
  y_opc_ti_2 <- matrix(0,n-1,1)
  for (i in 1:nrow(y_opc_ti_2)) {
    y_opc_ti_2[i,] <-  Valuacion_opc_ti(fechas_final[i+1,],esc_tasa_TIIE,esc_tasa_DIRS,esc_tasa_volTIIE,pos_opc_ti[2],plazos_opc_ti[2],pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti[2],precio_strike_opc_ti[2],nom_opc_ti[2])
  }
  
  # Calculamos las pédidas y ganancias (P&L)
  PnL_opc_ti_1 <- matrix(0,nrow(y_opc_ti_1)-1,1)
  colnames(PnL_opc_ti_1) <- "Opcion tasa de interés 1"
  for (i in 1:nrow(PnL_opc_ti_1)) {
    PnL_opc_ti_1[i,] <- y_opc_ti_1[1,] - y_opc_ti_1[i+1,]
  }
  
  PnL_opc_ti_2 <- matrix(0,nrow(y_opc_ti_2)-1,1)
  colnames(PnL_opc_ti_2) <- "Opcion tasa de interés 2"
  for (i in 1:nrow(PnL_opc_ti_2)) {
    PnL_opc_ti_2[i,] <- y_opc_ti_2[1,] - y_opc_ti_2[i+1,]
  }
}

#####################################################################################
#####################################################################################
# Cálculo de VaR y CVaR marginal y total por instrumento por SH sin alisado

# Calculamos el VaR y el CVaR ordenando la distribución P&L y hallando el cuantil alpha
# Riesgo Acciones y divisas
{
  # Calculamos el VaR y el CVaR ordenando la distribución P&L y hallando el cuantil alpha
  VaR_SH_ac_div <- list()
  CVaR_SH_ac_div <- list()
  for (i in 1:ncol(PnL_ac_div)) {
    VaR_SH_ac_div[[i]] <- VaR(PnL_ac_div[,i],alpha)
    CVaR_SH_ac_div[[i]] <- CVaR(VaR_SH_ac_div[[i]][1],PnL_ac_div[,i])
  }
  VaR_SH_ac_div[[ncol(PnL_ac_div)+1]] <- VaR(rowSums(PnL_ac),alpha)
  CVaR_SH_ac_div[[ncol(PnL_ac_div)+1]] <- CVaR(VaR_SH_ac_div[[ncol(PnL_ac_div)+1]][1],rowSums(PnL_ac))
  VaR_SH_ac_div[[ncol(PnL_ac_div)+2]] <- VaR(rowSums(PnL_div),alpha)
  CVaR_SH_ac_div[[ncol(PnL_ac_div)+2]] <- CVaR(VaR_SH_ac_div[[ncol(PnL_ac_div)+2]][1],rowSums(PnL_div))
  VaR_SH_ac_div[[ncol(PnL_ac_div)+3]] <- VaR(rowSums(PnL_ac_div),alpha)
  CVaR_SH_ac_div[[ncol(PnL_ac_div)+3]] <- CVaR(VaR_SH_ac_div[[ncol(PnL_ac_div)+3]][1],rowSums(PnL_ac_div))
  names(VaR_SH_ac_div) <- c(acciones,divisas,"Total acciones","Total divisas","Total ac. y div.")
  names(CVaR_SH_ac_div) <-c(acciones,divisas,"Total acciones","Total divisas","Total ac. y div.")
}

#####################################################################################
# Riesgo de Bonos
{
  # Riesgo de Cete
  VaR_SH_cete <- VaR(PnL_cete[,1],alpha)
  CVaR_SH_cete <- CVaR(VaR_SH_cete,PnL_cete[,1])
  
  # Riesgo de Bono M
  VaR_SH_bono_m <- VaR(PnL_bono_m[,1],alpha)
  CVaR_SH_bono_m <- CVaR(VaR_SH_bono_m,PnL_bono_m[,1])
  
  # Riesgo de Bondes
  VaR_SH_bondes<- VaR(PnL_bondes[,1],alpha)
  CVaR_SH_bondes <- CVaR(VaR_SH_bondes,PnL_bondes[,1]) 
  
  # Calculamos las pédidas y ganancias (P&L) de los bonos en total
  PnL_Bonos <- cbind(PnL_cete,PnL_bono_m,PnL_bondes)
  
  VaR_SH_Bonos <-  VaR(rowSums(PnL_Bonos),alpha)
  CVaR_SH_Bonos <- CVaR(VaR_SH_Bonos,PnL_Bonos) 
}

#####################################################################################
# Riesgo de Futuros
{
  # Riesgo de Forward TDC
  VaR_SH_fwd_tdc<- VaR(PnL_fwd_tdc,alpha)
  CVaR_SH_fwd_tdc <- CVaR(VaR_SH_fwd_tdc,PnL_fwd_tdc) 
  
  # Riesgo de Futuro IPC
  VaR_SH_fut_ipc <- VaR(PnL_fut_ipc,alpha)
  CVaR_SH_fut_ipc <- CVaR(VaR_SH_fut_ipc,PnL_fut_ipc) 
  
  # Calculamos las pédidas y ganancias (P&L) de los futuros en total
  PnL_Futuros <- cbind(PnL_fwd_tdc,PnL_fut_ipc)
  
  VaR_SH_Futuros <-  VaR(rowSums(PnL_Futuros),alpha)
  CVaR_SH_Futuros <- CVaR(VaR_SH_Futuros,PnL_Futuros) 
}

#####################################################################################
# Riesgo de SWAPS
{
  VaR_SH_swap_l <- VaR(PnL_swap_l,alpha)
  CVaR_SH_swap_l <- CVaR(VaR_SH_swap_l,PnL_swap_l) 
  
  VaR_SH_swap_c <- VaR(PnL_swap_c,alpha)
  CVaR_SH_swap_c <- CVaR(VaR_SH_swap_c,PnL_swap_c) 
  
  # Calculamos las pédidas y ganancias (P&L) de los swaps en total
  PnL_Swaps <- cbind(PnL_swap_l,PnL_swap_c)
  
  VaR_SH_Swaps <-  VaR(rowSums(PnL_Swaps),alpha)
  CVaR_SH_Swaps <- CVaR(VaR_SH_Swaps,PnL_Swaps) 
}

#####################################################################################
# Riesgo de Opciones
{
  VaR_SH_opc_ti_1 <- VaR(PnL_opc_ti_1,alpha)
  CVaR_SH_opc_ti_1<- CVaR(VaR_SH_opc_ti_1,PnL_opc_ti_1) 
  
  VaR_SH_opc_ti_2 <- VaR(PnL_opc_ti_2,alpha)
  CVaR_SH_opc_ti_2 <- CVaR(VaR_SH_opc_ti_2,PnL_opc_ti_2) 
  
  
  # Calculamos las pédidas y ganancias (P&L) de las opciones en total
  PnL_Opciones <- cbind(PnL_opc_ti_1,PnL_opc_ti_2)
  
  VaR_SH_Opciones <-  VaR(rowSums(PnL_Opciones),alpha)
  CVaR_SH_Opciones <- CVaR(VaR_SH_Opciones,PnL_Opciones)
}

#####################################################################################
# Riesgo integral

# Medición de riesgo por factor de riesgo de todo el portafolio
{
  # Acciones
  # Acciones e IPC
  PnL_port_ac <- cbind(PnL_ac,PnL_fut_ipc)
  VaR_SH_port_ac <- VaR(rowSums(PnL_port_ac),alpha)
  CVaR_SH_port_ac <- CVaR(VaR_SH_port_ac,PnL_port_ac)
  
  # Divisas
  VaR_SH_port_div <- VaR(rowSums(PnL_div),alpha)
  CVaR_SH_port_div <- CVaR(VaR_SH_port_div,PnL_div)
  
  # Tasa de Interés
  # Swaps y bonos
  PnL_port_ti <- cbind(PnL_Swaps,PnL_Bonos)
  VaR_SH_port_ti <- VaR(rowSums(PnL_port_ti),alpha)
  CVaR_SH_port_ti <- CVaR(VaR_SH_port_ti,PnL_port_ti)
  
  # Tipo de cambio
  # Futuro tdc
  VaR_SH_port_tdc <- VaR_fwd_tdc
  CVaR_SH_port_tdc <- CVaR_fwd_tdc
  
  # Volatilidad
  # Opciones de tasa de interés
  VaR_SH_port_vol <- VaR_Opciones
  CVaR_SH_port_vol <- CVaR_Opciones
  
  
  # Medición de riesgo de todo el portafolios
  # Sumar todos los PnL de todos los instrumentos
  
  PnL_port <- cbind(PnL_port_ac,PnL_div,PnL_port_ti,PnL_fwd_tdc,PnL_Opciones)
  VaR_SH_port <- VaR(rowSums(PnL_port),alpha)
  CVaR_SH_port <- CVaR(VaR_SH_port,PnL_port)
}

#####################################################################################
#####################################################################################
# Cálculo de VaR y CVaR marginal y total por instrumento por SH con alisado

# Calculamos el VaR y el CVaR
# Riesgo Acciones y divisas
{
  w0 <- 0.05
  lambda  <- uniroot(function(x) w0*(1-x^(n))/(1-x)-1, c(0,0.99), tol = 1e-28)$root
  
  # Función que genera "n" escenarios con base en w0 y lambda
  genera_esc <- function(lamda,w0,n) {
    p_esc <- matrix(0,n,1)
    for (i in (1:n)){
      p_esc[i] <- w0*lambda^(i-1)
    }
    p_esc
  }
  m <- nrow(PnL_ac)
  p_esc <- genera_esc(lambda,w0,m)
  
  # Calculamos el VaR y el CVaR con alisado
  VaR_SHa_ac_div <- list()
  CVaR_SHa_ac_div <- list()
  for (i in 1:ncol(PnL_ac_div)) {
    VaR_SHa_ac_div[[i]] <- VaR_ca(PnL_ac_div[,i],p_esc,1 - alpha)
    CVaR_SHa_ac_div[[i]] <- CVaR_ca(PnL_ac_div[,i],p_esc,1 - alpha)
  }
  
  VaR_SHa_ac_div[[ncol(PnL_ac_div)+1]] <- VaR_ca(rowSums(PnL_ac),p_esc,1 - alpha)
  CVaR_SHa_ac_div[[ncol(PnL_ac_div)+1]] <- CVaR_ca(rowSums(PnL_ac),p_esc,1 - alpha)
  VaR_SHa_ac_div[[ncol(PnL_ac_div)+2]] <- VaR_ca(rowSums(PnL_div),p_esc,1 - alpha)
  CVaR_SHa_ac_div[[ncol(PnL_ac_div)+2]] <- CVaR_ca(rowSums(PnL_div),p_esc,1 - alpha)
  VaR_SHa_ac_div[[ncol(PnL_ac_div)+3]] <- VaR_ca(rowSums(PnL_ac_div),p_esc,1 - alpha)
  CVaR_SHa_ac_div[[ncol(PnL_ac_div)+3]] <- CVaR_ca(rowSums(PnL_ac_div),p_esc,1 - alpha)
  names(VaR_SHa_ac_div) <- c(acciones,divisas,"Total acciones","Total divisas","Total ac. y div.")
  names(CVaR_SHa_ac_div) <-c(acciones,divisas,"Total acciones","Total divisas","Total ac. y div.")
}

#####################################################################################
# Riesgo de Bonos
{
  # Riesgo de Cete
  VaR_SHa_cete <- VaR_ca(PnL_cete[,1],p_esc,1 - alpha)
  CVaR_SHa_cete <- CVaR_ca(PnL_cete[,1],p_esc,1 - alpha)
  
  # Riesgo de Bono M
  VaR_SHa_bono_m <- VaR_ca(PnL_bono_m[,1],p_esc,1 - alpha)
  CVaR_SHa_bono_m <- CVaR_ca(PnL_bono_m[,1],p_esc,1 - alpha)
  
  # Riesgo de Bondes
  VaR_SHa_bondes<- VaR_ca(PnL_bondes[,1],p_esc,1 - alpha)
  CVaR_SHa_bondes <- CVaR_ca(PnL_bondes[,1],p_esc,1 - alpha) 
  
  # Calculamos las pédidas y ganancias (P&L) de los bonos en total
  PnL_Bonos <- cbind(PnL_cete,PnL_bono_m,PnL_bondes)
  
  VaR_SHa_Bonos <-  VaR_ca(rowSums(PnL_Bonos),p_esc,1 - alpha)
  CVaR_SHa_Bonos <- CVaR_ca(rowSums(PnL_Bonos),p_esc,1 - alpha) 
}

#####################################################################################
# Riesgo de Futuros
{
  # Riesgo de Forward TDC
  VaR_SHa_fwd_tdc<- VaR_ca(PnL_fwd_tdc,p_esc,1 - alpha)
  CVaR_SHa_fwd_tdc <- CVaR_ca(PnL_fwd_tdc,p_esc,1 - alpha) 
  
  # Riesgo de Futuro IPC
  VaR_SHa_fut_ipc <- VaR_ca(PnL_fut_ipc,p_esc,1 - alpha)
  CVaR_SHa_fut_ipc <- CVaR_ca(PnL_fut_ipc,p_esc,1 - alpha) 
  
  # Calculamos las pédidas y ganancias (P&L) de los futuros en total
  PnL_Futuros <- cbind(PnL_fwd_tdc,PnL_fut_ipc)
  
  VaR_SHa_Futuros <-  VaR_ca(rowSums(PnL_Futuros),p_esc,1 - alpha)
  CVaR_SHa_Futuros <- CVaR_ca(rowSums(PnL_Futuros),p_esc,1 - alpha) 
}

#####################################################################################
# Riesgo de SWAPS
{
  VaR_SHa_swap_l <- VaR_ca(PnL_swap_l,p_esc,1 - alpha)
  CVaR_SHa_swap_l <- CVaR_ca(PnL_swap_l,p_esc,1 - alpha) 
  
  VaR_SHa_swap_c <- VaR_ca(PnL_swap_c,p_esc,1 - alpha)
  CVaR_SHa_swap_c <- CVaR_ca(PnL_swap_c,p_esc,1 - alpha) 
  
  # Calculamos las pédidas y ganancias (P&L) de los swaps en total
  PnL_Swaps <- cbind(PnL_swap_l,PnL_swap_c)
  
  VaR_SHa_Swaps <-  VaR_ca(rowSums(PnL_Swaps),p_esc,1 - alpha)
  CVaR_SHa_Swaps <- CVaR_ca(rowSums(PnL_Swaps),p_esc,1 - alpha) 
}

#####################################################################################
# Riesgo de Opciones
{
  VaR_SHa_opc_ti_1 <- VaR_ca(PnL_opc_ti_1,p_esc,1 - alpha)
  CVaR_SHa_opc_ti_1<- CVaR_ca(PnL_opc_ti_1,p_esc,1 - alpha) 
  
  VaR_SHa_opc_ti_2 <- VaR_ca(PnL_opc_ti_2,p_esc,1 - alpha)
  CVaR_SHa_opc_ti_2 <- CVaR_ca(PnL_opc_ti_2,p_esc,1 - alpha) 
  
  # Calculamos las pédidas y ganancias (P&L) de las opciones en total
  PnL_Opciones <- cbind(PnL_opc_ti_1,PnL_opc_ti_2)
  
  VaR_SHa_Opciones <-  VaR_ca(rowSums(PnL_Opciones),p_esc,1 - alpha)
  CVaR_SHa_Opciones <- CVaR_ca(rowSums(PnL_Opciones),p_esc,1 - alpha)
}

#####################################################################################
# Riesgo integral

# Medición de riesgo por factor de riesgo de todo el portafolio
{
  # Acciones
  # Acciones e IPC
  PnL_port_ac <- cbind(PnL_ac,PnL_fut_ipc)
  VaR_SHa_port_ac <- VaR_ca(rowSums(PnL_port_ac),p_esc,1 - alpha)
  CVaR_SHa_port_ac <- CVaR_ca(rowSums(PnL_port_ac),p_esc,1 - alpha)
  
  # Divisas
  VaR_SHa_port_div <- VaR_ca(rowSums(PnL_div),p_esc,1 - alpha)
  CVaR_SHa_port_div <- CVaR_ca(rowSums(PnL_div),p_esc,1 - alpha)
  
  # Tasa de Interés
  # Swaps y bonos
  PnL_port_ti <- cbind(PnL_Swaps,PnL_Bonos)
  VaR_SHa_port_ti <- VaR_ca(rowSums(PnL_port_ti),p_esc,1 - alpha)
  CVaR_SHa_port_ti <- CVaR_ca(rowSums(PnL_port_ti),p_esc,1 - alpha)
  
  # Tipo de cambio
  # Futuro tdc
  VaR_SHa_port_tdc <- VaR_SHa_fwd_tdc
  CVaR_SHa_port_tdc <- CVaR_SHa_fwd_tdc
  
  # Volatilidad
  # Opciones de tasa de interés
  VaR_SHa_port_vol <- VaR_SHa_Opciones
  CVaR_SHa_port_vol <- CVaR_SHa_Opciones
  
  
  # Medición de riesgo de todo el portafolios
  # Sumar todos los PnL de todos los instrumentos
  
  PnL_port <- cbind(PnL_port_ac,PnL_div,PnL_port_ti,PnL_fwd_tdc,PnL_Opciones)
  VaR_SHa_port <- VaR_ca(rowSums(PnL_port),p_esc,1 - alpha)
  CVaR_SHa_port <- CVaR_ca(rowSums(PnL_port),p_esc,1 - alpha)
}

#####################################################################################
#####################################################################################
# Cálculo de VaR y CVaR marginal y total por instrumento por Delta-Normal
# Para todos los instrumentos hay que definir y calcular los siguientes elementos: 
#   1.Historico de factores de riesgo
#   2.Vector de precios actual
#   3.Derivada con respecto al tiempo evaluada en el precio actual
#   4.Primera derivada con respecto a factores de riesgo evaluada y multiplicada con su precio actual
#   5.Segunda derivada con respecto a factores de riesgo evaluada y multiplicada con sus precios actuales (al cuadrado)

#####################################################################################
# Riesgo Acciones y divisas
{  
# Cálculo de varianza y matriz de varianza-covarianza según sea el caso
  VarCov_ac_ind <- matrix(0,1,ncol(var_diarias_ac))
  for (i in 1:ncol(VarCov_ac_ind)) {
    VarCov_ac_ind[,i] <- var(var_diarias_ac[,i])*((n-1)/(n-2))
  }
  VarCov_div_ind <- matrix(0,1,ncol(var_diarias_div))
  for (i in 1:ncol(VarCov_div_ind)) {
    VarCov_div_ind[,i] <- var(var_diarias_div[,i])*((n-1)/(n-2))
  }
  VarCov_ac <- cov(var_diarias_ac)*((n-1)/(n-2))
  VarCov_div <- cov(var_diarias_div)*((n-1)/(n-2))
  
  # Calculo de derivadas
  g_ac <- pos_ac
  g_div <- pos_div
  
  gx_ac <- pos_ac*ac_x0
  gx_div <- pos_div*div_x0
  
  # Cálculo de VaR Delta-Normal
  VaR_DN_ac_ind <- matrix(0,1,ncol(var_diarias_ac))
  colnames(VaR_DN_ac_ind) <- acciones
  for (i in 1:ncol(VaR_DN_ac_ind)) {
    VaR_DN_ac_ind[,i] <- sqrt((gx_ac[,i]^2)*(VarCov_ac_ind[,i]))*qnorm(1-alpha)
  }
  
  VaR_DN_div_ind <- matrix(0,1,ncol(var_diarias_div))
  colnames(VaR_DN_div_ind) <- divisas
  for (i in 1:ncol(VaR_DN_div_ind)) {
    VaR_DN_div_ind[,i] <- sqrt((gx_div[,i]^2)*(VarCov_div_ind[,i]))*qnorm(1-alpha)
  }
  
  # Cálculo de CVaR Delta-Normal total
  VaR_DN_ac <- sqrt(gx_ac %*% VarCov_ac %*% t(gx_ac))*qnorm(1-alpha)
  VaR_DN_div <- sqrt(gx_div %*% VarCov_ac %*% t(gx_div))*qnorm(1-alpha)
  
  # Cálculo de CVaR Delta-Normal 
  CVaR_DN_ac_ind <- matrix(0,1,ncol(var_diarias_ac))
  colnames(CVaR_DN_ac_ind) <- acciones
  for (i in 1:ncol(CVaR_DN_ac_ind)) {
    CVaR_DN_ac_ind[,i] <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt((gx_ac[,i]^2)*(VarCov_ac_ind[,i])))*(-1)
  }
  
  CVaR_DN_div_ind <- matrix(0,1,ncol(var_diarias_div))
  colnames(CVaR_DN_div_ind) <- divisas
  for (i in 1:ncol(CVaR_DN_div_ind)) {
    CVaR_DN_div_ind[,i] <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt((gx_div[,i]^2)*(VarCov_div_ind[,i])))*(-1)
  }
  
  # Cálculo de CVaR Delta-Normal total
  CVaR_DN_ac <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(gx_ac %*% VarCov_ac %*% t(gx_ac)))*(-1)
  CVaR_DN_div <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(gx_div %*% VarCov_div %*% t(gx_div)))*(-1)
}

#####################################################################################
# Riesgo Cete
{  
  # Cálculo de varianza y matriz de varianza-covarianza según sea el caso
  Var_cete <- var(var_diarias_cete)*((n-1)/(n-2))
  
  # Calculo de derivadas
  g_cete <- -(pos_cete*nom_cete*(venc_cete/360))/((1+ cete_x_0*venc_cete/360)^2)
  gx_cete <- g_cete*cete_x_0
  
  # Cálculo de VaR y CVaR Delta-Normal
  VaR_DN_cete <- sqrt((gx_cete^2)*Var_cete)*qnorm(1-alpha)
  CVaR_DN_cete <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt((gx_cete^2)*Var_cete))*(-1)
}

#####################################################################################
# Riesgo Bono M
{
  # Cálculo de varianza y matriz de varianza-covarianza según sea el caso
  Var_bono_m <- var(var_diarias_bono_m)*((n-1)/(n-2))
  
  # Cálculo de la derivada de la valuación del bono respecto a la tasa de descuento
  D_Valuacion_Bono_M <- function(fecha,tasas_fija_bono_m,pos_bono_m, nom_bono_m, venc_bono_m, tasa_cupon_fija_bono_m, dias_pago_cupon_bono_m){
    # Se calculan los cupones por pagar
    cup_por_pagar_bono_m <- trunc(venc_bono_m/dias_pago_cupon_bono_m)
    
    # Se calculan los días devengados del cupón vigente
    dias_deveng_cupon_bono_m <- dias_pago_cupon_bono_m - (venc_bono_m - trunc(venc_bono_m/dias_pago_cupon_bono_m)*dias_pago_cupon_bono_m)
    
    # Se determinan las fechas en la que se paga cupón y se calcula el valor del bono en cada fecha
    {
      cupon_bono_m <- matrix(0,cup_por_pagar_bono_m,7)
      colnames(cupon_bono_m)<-c("Fecha","Tasa cupón","Flujo","Días cupón","Días transcurridos","Tasa VPN","VPN Cupón")
      cupon_bono_m <- as.data.frame(cupon_bono_m)
      cupon_bono_m$Fecha[1] <- fecha_val + (venc_bono_m - trunc(venc_bono_m/dias_pago_cupon_bono_m)*dias_pago_cupon_bono_m)
      cupon_bono_m$`Tasa cupón`[1] <- tasa_cupon_fija_bono_m
      cupon_bono_m$Flujo[1] <- pos_bono_m*nom_bono_m*tasa_cupon_fija_bono_m*dias_pago_cupon_bono_m/360
      cupon_bono_m$`Días cupón`[1] <- dias_pago_cupon_bono_m
      cupon_bono_m$`Días transcurridos`[1] <- cupon_bono_m$`Días cupón`[1]
      cupon_bono_m$`Tasa VPN`[1] <- as.numeric(interpolar_alam(venc_bono_m,nodos_fija_bono_m,tasas_fija_bono_m[match(fecha_val,tasas_fija_bono_m$dates),-1]))
      cupon_bono_m$`VPN Cupón`[1] <- (-cupon_bono_m$Flujo[1]*cupon_bono_m$`Días transcurridos`[1]/360)/(1+cupon_bono_m$`Tasa VPN`[1]*cupon_bono_m$`Días transcurridos`[1]/360)^2
      
      for(i in 2:cup_por_pagar_bono_m){
        cupon_bono_m$Fecha[i] <- cupon_bono_m$Fecha[i-1] + dias_pago_cupon_bono_m
        cupon_bono_m$`Tasa cupón`[i] <- tasa_cupon_fija_bono_m
        cupon_bono_m$Flujo[i] <- pos_bono_m*nom_bono_m*tasa_cupon_fija_bono_m*dias_pago_cupon_bono_m/360
        cupon_bono_m$`Días cupón`[i] <- dias_pago_cupon_bono_m
        cupon_bono_m$`Días transcurridos`[i] <- cupon_bono_m$`Días cupón`[i] + cupon_bono_m$`Días transcurridos`[i-1] 
        cupon_bono_m$`Tasa VPN`[i] <- as.numeric(interpolar_alam(venc_bono_m,nodos_fija_bono_m,tasas_fija_bono_m[match(fecha_val,tasas_fija_bono_m$dates),-1]))
        cupon_bono_m$`VPN Cupón`[i] <- (-cupon_bono_m$Flujo[i]*cupon_bono_m$`Días transcurridos`[i]/360)/(1+cupon_bono_m$`Tasa VPN`[1]*cupon_bono_m$`Días transcurridos`[i]/360)^2
      }
      cupon_bono_m$Flujo[cup_por_pagar_bono_m] <- cupon_bono_m$Flujo[cup_por_pagar_bono_m] + pos_bono_m*nom_bono_m
      cupon_bono_m$`VPN Cupón`[cup_por_pagar_bono_m] <- (-cupon_bono_m$Flujo[cup_por_pagar_bono_m]*cupon_bono_m$`Días transcurridos`[cup_por_pagar_bono_m]/360)/(1+cupon_bono_m$`Tasa VPN`[cup_por_pagar_bono_m]*cupon_bono_m$`Días transcurridos`[cup_por_pagar_bono_m]/360)^2
      cupon_bono_m$Fecha <- as.Date.numeric(cupon_bono_m$Fecha,origin = "1970-01-01")
      
      V0 <- sum(cupon_bono_m$`VPN Cupón`)
      
      return(V0)
    }
  }
  # Calculo de derivadas
  g_bono_m <- D_Valuacion_Bono_M(fecha_val,tasas_fija_bono_m,pos_bono_m, nom_bono_m, venc_bono_m, tasa_cupon_fija_bono_m, dias_pago_cupon_bono_m)
  gx_bono_m <- g_bono_m*bono_m_x_0
  
  # Cálculo de VaR y CVaR Delta-Normal
  VaR_DN_bono_m <- sqrt((gx_bono_m^2)*Var_bono_m)*qnorm(1-alpha)
  CVaR_DN_bono_m <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt((gx_bono_m^2)*Var_bono_m))*(-1)
}

#####################################################################################
# Riesgo Bonde D
{
  # Cálculo de varianza y matriz de varianza-covarianza según sea el caso
  var_diarias_bondes <- cbind(x_bondes[1:(n-1),1]/x_bondes[2:n,1] - 1,x_bondes[1:(n-1),2]/x_bondes[2:n,2] - 1,x_bondes[1:(n-1),3]/x_bondes[2:n,3] - 1)
  colnames(var_diarias_bondes) <- colnames(x_bondes)
  
  VarCov_bondes <- var(var_diarias_bondes)*((n-1)/(n-2))
  
  # Cálculo de derivadas respecto a la tasa cupón variable,  sobretasa y tasas de fondeo
  D_Valuacion_Bondes_tasa_cupon <- function(fecha,tasas_guber,tasas_guber_st,tasas_fondeo_orig,pos_bondes,nom_bondes,dias_pago_cupon_bondes,venc_bondes){
    # Se calculan los cupones por pagar
    cup_por_pagar_bondes <- trunc(venc_bondes/dias_pago_cupon_bondes) + 1
    
    # Se calculan los días devengados del cupón vigente
    dias_deveng_cupon_bondes <- dias_pago_cupon_bondes - (venc_bondes - trunc(venc_bondes/dias_pago_cupon_bondes)*dias_pago_cupon_bondes)
    
    # Se busca la tasa de fondeo actual y se calcula la Tasa de interés anual devengada
    
    tasa_fondeo <- tasas_fondeo_orig[match(fecha,tasas_fondeo_orig[,1]),2]
    a <- as.numeric(match(fecha,tasas_fondeo_orig[,1]))
    b <- match(fecha,tasas_fondeo_orig[,1]) - dias_deveng_cupon_bondes + 1
    tasas_tc_dev_bondes <- tasas_fondeo_orig[b:a,2]
    tasa_cupon_dev_bondes <- (prod(1+tasas_tc_dev_bondes/36000)-1)*(36000/dias_deveng_cupon_bondes)
    
    # Se determinan las fechas en la que se paga cupón y se calcula el valor del bono en cada fecha
    
    {
      cupon_bondes <- matrix(0,cup_por_pagar_bondes,8)
      colnames(cupon_bondes)<-c("Fecha","Tasa cupón","Cupón","Días cupón","Días transcurridos","Tasa VPN","Sobretasa VPN","VPN Cupón")
      cupon_bondes <- as.data.frame(cupon_bondes)
      
      cupon_bondes$Fecha[1] <- fecha_val + (venc_bondes - trunc(venc_bondes/dias_pago_cupon_bondes)*dias_pago_cupon_bondes)
      cupon_bondes$`Tasa cupón`[1] <- ((1 + tasa_cupon_dev_bondes*dias_deveng_cupon_bondes/360)*(1+tasa_fondeo/360)^(dias_pago_cupon_bondes - dias_deveng_cupon_bondes) - 1)*360/dias_pago_cupon_bondes
      cupon_bondes$Cupón[1] <- pos_bondes*nom_bondes*dias_pago_cupon_bondes/360
      cupon_bondes$`Días cupón`[1] <- venc_bondes - trunc(venc_bondes/dias_pago_cupon_bondes)*dias_pago_cupon_bondes
      cupon_bondes$`Días transcurridos`[1] <- cupon_bondes$`Días cupón`[1]
      cupon_bondes$`Tasa VPN`[1] <- as.numeric(interpolar_alam(cupon_bondes$`Días transcurridos`[1],nodos_guber,tasas_guber[match(fecha,tasas_guber$dates),-1]))
      cupon_bondes$`Sobretasa VPN`[1] <- as.numeric(interpolar_alam(cupon_bondes$`Días transcurridos`[1],nodos_guber_st,tasas_guber_st[match(fecha,tasas_guber_st$dates),-1]))
      cupon_bondes$`VPN Cupón`[1] <- cupon_bondes$Cupón[1]/(1+(cupon_bondes$`Tasa VPN`[1] + cupon_bondes$`Sobretasa VPN`[1])*cupon_bondes$`Días transcurridos`[1]/360)
      
      for(i in 2:cup_por_pagar_bondes){
        cupon_bondes$Fecha[i] <- cupon_bondes$Fecha[i-1] + dias_pago_cupon_bondes
        cupon_bondes$`Tasa cupón`[i] <- (((1 + tasa_fondeo/360)^dias_pago_cupon_bondes)-1)*(360/dias_pago_cupon_bondes)
        cupon_bondes$Cupón[i] <- pos_bondes*nom_bondes*dias_pago_cupon_bondes/360
        cupon_bondes$`Días cupón`[i] <- dias_pago_cupon_bondes
        cupon_bondes$`Días transcurridos`[i] <- cupon_bondes$`Días cupón`[i] + cupon_bondes$`Días transcurridos`[i-1] 
        cupon_bondes$`Tasa VPN` <- as.numeric(interpolar_alam(cupon_bondes$`Días transcurridos`[i],nodos_guber,tasas_guber[match(fecha,tasas_guber$dates),-1]))
        cupon_bondes$`Sobretasa VPN`[i] <- as.numeric(interpolar_alam(cupon_bondes$`Días transcurridos`[i],nodos_guber_st,tasas_guber_st[match(fecha,tasas_guber_st$dates),-1]))
        cupon_bondes$`VPN Cupón`[i] <- cupon_bondes$Cupón[i]/(1+(cupon_bondes$`Tasa VPN`[i] + cupon_bondes$`Sobretasa VPN`[i])*cupon_bondes$`Días transcurridos`[i]/360)
      }
      cupon_bondes$Cupón[cup_por_pagar_bondes] <- 0
      cupon_bondes$`VPN Cupón`[cup_por_pagar_bondes] <- cupon_bondes$Cupón[cup_por_pagar_bondes]/(1+(cupon_bondes$`Tasa VPN`[cup_por_pagar_bondes]+cupon_bondes$`Sobretasa VPN`[cup_por_pagar_bondes])*cupon_bondes$`Días transcurridos`[cup_por_pagar_bondes]/360)
      cupon_bondes$Fecha <- as.Date.numeric(cupon_bondes$Fecha,origin = "1970-01-01")
      
      # Se calcula el valor del bono
      V0 <- sum(cupon_bondes$`VPN Cupón`)
      
      return(V0)
    }
  }
  
  g_tasa_cupon <- D_Valuacion_Bondes_tasa_cupon(fecha_val,tasas_guber,tasas_guber_st,tasas_fondeo_orig,pos_bondes,nom_bondes,dias_pago_cupon_bondes,venc_bondes)
  
  D_Valuacion_Bondes_tasa_vp_st <- function(fecha,tasas_guber,tasas_guber_st,tasas_fondeo_orig,pos_bondes,nom_bondes,dias_pago_cupon_bondes,venc_bondes){
    # Se calculan los cupones por pagar
    cup_por_pagar_bondes <- trunc(venc_bondes/dias_pago_cupon_bondes) + 1
    
    # Se calculan los días devengados del cupón vigente
    dias_deveng_cupon_bondes <- dias_pago_cupon_bondes - (venc_bondes - trunc(venc_bondes/dias_pago_cupon_bondes)*dias_pago_cupon_bondes)
    
    # Se busca la tasa de fondeo actual y se calcula la Tasa de interés anual devengada
    
    tasa_fondeo <- tasas_fondeo_orig[match(fecha,tasas_fondeo_orig[,1]),2]
    a <- as.numeric(match(fecha,tasas_fondeo_orig[,1]))
    b <- match(fecha,tasas_fondeo_orig[,1]) - dias_deveng_cupon_bondes + 1
    tasas_tc_dev_bondes <- tasas_fondeo_orig[b:a,2]
    tasa_cupon_dev_bondes <- (prod(1+tasas_tc_dev_bondes/36000)-1)*(36000/dias_deveng_cupon_bondes)
    
    # Se determinan las fechas en la que se paga cupón y se calcula el valor del bono en cada fecha
    
    {
      cupon_bondes <- matrix(0,cup_por_pagar_bondes,8)
      colnames(cupon_bondes)<-c("Fecha","Tasa cupón","Cupón","Días cupón","Días transcurridos","Tasa VPN","Sobretasa VPN","VPN Cupón")
      cupon_bondes <- as.data.frame(cupon_bondes)
      
      cupon_bondes$Fecha[1] <- fecha_val + (venc_bondes - trunc(venc_bondes/dias_pago_cupon_bondes)*dias_pago_cupon_bondes)
      cupon_bondes$`Tasa cupón`[1] <- ((1 + tasa_cupon_dev_bondes*dias_deveng_cupon_bondes/360)*(1+tasa_fondeo/360)^(dias_pago_cupon_bondes - dias_deveng_cupon_bondes) - 1)*360/dias_pago_cupon_bondes
      cupon_bondes$Cupón[1] <- -pos_bondes*nom_bondes*cupon_bondes$`Tasa cupón`[1]*dias_pago_cupon_bondes
      cupon_bondes$`Días cupón`[1] <- venc_bondes - trunc(venc_bondes/dias_pago_cupon_bondes)*dias_pago_cupon_bondes
      cupon_bondes$`Días transcurridos`[1] <- cupon_bondes$`Días cupón`[1]
      cupon_bondes$`Tasa VPN`[1] <- as.numeric(interpolar_alam(cupon_bondes$`Días transcurridos`[1],nodos_guber,tasas_guber[match(fecha,tasas_guber$dates),-1]))
      cupon_bondes$`Sobretasa VPN`[1] <- as.numeric(interpolar_alam(cupon_bondes$`Días transcurridos`[1],nodos_guber_st,tasas_guber_st[match(fecha,tasas_guber_st$dates),-1]))
      cupon_bondes$`VPN Cupón`[1] <- (cupon_bondes$Cupón[1]*cupon_bondes$`Días transcurridos`[1])/(cupon_bondes$`Días transcurridos`[1]*(cupon_bondes$`Tasa VPN`[1] + cupon_bondes$`Sobretasa VPN`[1])+360)^2
      
      for(i in 2:cup_por_pagar_bondes){
        cupon_bondes$Fecha[i] <- cupon_bondes$Fecha[i-1] + dias_pago_cupon_bondes
        cupon_bondes$`Tasa cupón`[i] <- (((1 + tasa_fondeo/360)^dias_pago_cupon_bondes)-1)*(360/dias_pago_cupon_bondes)
        cupon_bondes$Cupón[i] <- -pos_bondes*nom_bondes*cupon_bondes$`Tasa cupón`[i]*dias_pago_cupon_bondes
        cupon_bondes$`Días cupón`[i] <- dias_pago_cupon_bondes
        cupon_bondes$`Días transcurridos`[i] <- cupon_bondes$`Días cupón`[i] + cupon_bondes$`Días transcurridos`[i-1] 
        cupon_bondes$`Tasa VPN` <- as.numeric(interpolar_alam(cupon_bondes$`Días transcurridos`[i],nodos_guber,tasas_guber[match(fecha,tasas_guber$dates),-1]))
        cupon_bondes$`Sobretasa VPN`[i] <- as.numeric(interpolar_alam(cupon_bondes$`Días transcurridos`[i],nodos_guber_st,tasas_guber_st[match(fecha,tasas_guber_st$dates),-1]))
        cupon_bondes$`VPN Cupón`[i] <- (cupon_bondes$Cupón[i]*cupon_bondes$`Días transcurridos`[i])/(cupon_bondes$`Días transcurridos`[i]*(cupon_bondes$`Tasa VPN`[i] + cupon_bondes$`Sobretasa VPN`[i])+360)^2
      }
      cupon_bondes$Cupón[cup_por_pagar_bondes] <- -360*pos_bondes*nom_bondes
      cupon_bondes$`VPN Cupón`[cup_por_pagar_bondes] <- cupon_bondes$Cupón[cup_por_pagar_bondes]/(cupon_bondes$`Días transcurridos`[cup_por_pagar_bondes]*(cupon_bondes$`Tasa VPN`[cup_por_pagar_bondes] + cupon_bondes$`Sobretasa VPN`[cup_por_pagar_bondes])+360)^2
      cupon_bondes$Fecha <- as.Date.numeric(cupon_bondes$Fecha,origin = "1970-01-01")
      
      # Se calcula el valor del bono
      V0 <- sum(cupon_bondes$`VPN Cupón`)
      
      return(V0)
    }
  }
   # Cálculo de g y gx
  g_tasa_vp <- D_Valuacion_Bondes_tasa_vp_st(fecha_val,tasas_guber,tasas_guber_st,tasas_fondeo_orig,pos_bondes,nom_bondes,dias_pago_cupon_bondes,venc_bondes)
  g_tasa_st <- D_Valuacion_Bondes_tasa_vp_st(fecha_val,tasas_guber,tasas_guber_st,tasas_fondeo_orig,pos_bondes,nom_bondes,dias_pago_cupon_bondes,venc_bondes)
  
  g_bondes <- c(g_tasa_cupon,g_tasa_vp,g_tasa_st)
  gx_bondes <- as.matrix(g_bondes*bondes_x_0)
  
  # Cálculo de VaR y CVaR por Delta-Normal
  VaR_DN_bondes <- sqrt(t(gx_bondes) %*% VarCov_bondes %*% gx_bondes)*qnorm(1-alpha)
  CVaR_DN_bondes <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_bondes) %*% VarCov_bondes %*% gx_bondes))*(-1)
  
  # Cálculo de VaR y CVaR por Delta-Normal total de Bonos
  var_diarias_bonos <- cbind(var_diarias_cete,var_diarias_bono_m,var_diarias_bondes)
  VarCov_bonos <- var(var_diarias_bonos)*((n-1)/(n-2))
  
  g_bonos <- c(g_cete,g_bono_m,g_bondes)
  gx_bonos <- as.matrix(g_bonos*c(cete_x_0,bono_m_x_0,bondes_x_0))
  
  # Cálculo de VaR y CVaR por Delta-Normal
  VaR_DN_bonos <- sqrt(t(gx_bonos) %*% VarCov_bonos %*% gx_bonos)*qnorm(1-alpha)
  CVaR_DN_bonos <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_bonos) %*% VarCov_bonos %*% gx_bonos))*(-1)
}

#####################################################################################
# Riesgo Forward TDC
{
  # Cálculo de varianza y matriz de varianza-covarianza según sea el caso
  var_diarias_fwd_tdc <- cbind(x_fwd_tdc[1:(n-1),1]/x_fwd_tdc[2:n,1] - 1,x_fwd_tdc[1:(n-1),2]/x_fwd_tdc[2:n,2] - 1)
  colnames(var_diarias_fwd_tdc) <- colnames(x_fwd_tdc)
  
  VarCov_fwd_tdc <- var(var_diarias_fwd_tdc)*((n-1)/(n-2))
  
  # Cálculo de derivadas respecto a las tasas doméstica y extranjera
  D_Valuacion_fwd_tdc_libor <- function(fecha,tasas_libor,tasas_fwd,moneda_fwd_tdc,pos_fwd_tdc,venc_fwd_tdc,precio_strike_fwd_tdc,nom_fwd_tdc){
    tc_spot_fwd <- precios_div[1,match(moneda_fwd_tdc,colnames(precios_div))]
    tasa_dom_fwd <- as.numeric(interpolar_alam(venc_fwd_tdc,nodos_fwd,tasas_fwd[match(fecha,tasas_fwd$dates),-1]))
    tasa_ext_fwd <- ((1 + as.numeric(interpolar_alam(venc_fwd_tdc,nodos_libor,tasas_libor[match(fecha,tasas_fwd$dates),-1])))^(venc_fwd_tdc/180)-1)*360/venc_fwd_tdc
    tc_fwd <- tc_spot_fwd*(1 + tasa_dom_fwd*venc_fwd_tdc/360)/(1 + tasa_ext_fwd*venc_fwd_tdc/360)
    V0 <- pos_fwd_tdc*(-360*tc_fwd*venc_fwd_tdc/(venc_fwd_tdc*tasa_ext_fwd+360)^2)
    
    return(V0)
  }
  
  D_Valuacion_fwd_tdc_tfwd <- function(fecha,tasas_libor,tasas_fwd,moneda_fwd_tdc,pos_fwd_tdc,venc_fwd_tdc,precio_strike_fwd_tdc,nom_fwd_tdc){
    tc_spot_fwd <- precios_div[1,match(moneda_fwd_tdc,colnames(precios_div))]
    tasa_dom_fwd <- as.numeric(interpolar_alam(venc_fwd_tdc,nodos_fwd,tasas_fwd[match(fecha,tasas_fwd$dates),-1]))
    tasa_ext_fwd <- ((1 + as.numeric(interpolar_alam(venc_fwd_tdc,nodos_libor,tasas_libor[match(fecha,tasas_fwd$dates),-1])))^(venc_fwd_tdc/180)-1)*360/venc_fwd_tdc
    tc_fwd <- tc_spot_fwd*(1 + tasa_dom_fwd*venc_fwd_tdc/360)/(1 + tasa_ext_fwd*venc_fwd_tdc/360)
    V0 <- pos_fwd_tdc*(360*precio_strike_fwd_tdc*venc_fwd_tdc/(venc_fwd_tdc*tasa_dom_fwd+360)^2)
    
    return(V0)
  }
  
  # Cálculo de g y gx
  g_libor <- D_Valuacion_fwd_tdc_libor(fecha_val,tasas_libor,tasas_fwd,moneda_fwd_tdc,pos_fwd_tdc,venc_fwd_tdc,precio_strike_fwd_tdc,nom_fwd_tdc)
  g_tfwd <- D_Valuacion_fwd_tdc_tfwd(fecha_val,tasas_libor,tasas_fwd,moneda_fwd_tdc,pos_fwd_tdc,venc_fwd_tdc,precio_strike_fwd_tdc,nom_fwd_tdc)
  
  g_fwd_tdc <- c(g_libor,g_tfwd)
  gx_fwd_tdc <- as.matrix(g_fwd_tdc*fwd_tdc_x_0)
  
  # Cálculo de VaR y CVaR por Delta-Normal
  VaR_DN_fwd_tdc <- sqrt(t(gx_fwd_tdc) %*% VarCov_fwd_tdc %*% gx_fwd_tdc)*qnorm(1-alpha)
  CVaR_DN_fwd_tdc <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_fwd_tdc) %*% VarCov_fwd_tdc %*% gx_fwd_tdc))*(-1)
}

#####################################################################################
# Riesgo Futuro IPC
{
  # Cálculo de varianza y matriz de varianza-covarianza según sea el caso
  var_diarias_fut_ipc <- cbind(x_fut_ipc[1:(n-1),1]/x_fut_ipc[2:n,1] - 1,x_fut_ipc[1:(n-1),2]/x_fut_ipc[2:n,2] - 1)
  colnames(var_diarias_fut_ipc) <- colnames(x_fut_ipc)
  
  VarCov_fut_ipc <- var(var_diarias_fut_ipc)*((n-1)/(n-2))
  
  # Cálculo de derivadas respecto a las tasas doméstica y de dividendos
  D_Valuacion_fut_IPC_dom <- function(fecha,tasas_guber,tasas_dividendos,pos_fut_IPC,precio_strike_fut_IPC,nom_fut_IPC){
    IPC_spot <- precios_IPC[1,]
    tasa_dom_IPC <- as.numeric(interpolar_alam(venc_fut_IPC,nodos_guber,tasas_guber[match(fecha,tasas_guber$dates),-1]))
    tasa_div_IPC <- ((1 + as.numeric(interpolar_alam(venc_fut_IPC,nodos_dividendos,tasas_dividendos[match(fecha,tasas_dividendos$dates),-1])))^(venc_fut_IPC/180)-1)*(360/venc_fut_IPC)
    V0 <- pos_fut_IPC*nom_fut_IPC*IPC_spot*(tasa_div_IPC*venc_fut_IPC + 360*precio_strike_fut_IPC)/(venc_fut_IPC*tasa_dom_IPC+360)^2
    
    return(V0)
  }
  
  D_Valuacion_fut_IPC_tdiv <- function(fecha,tasas_guber,tasas_dividendos,pos_fut_IPC,precio_strike_fut_IPC,nom_fut_IPC){
    IPC_spot <- precios_IPC[1,]
    tasa_dom_IPC <- as.numeric(interpolar_alam(venc_fut_IPC,nodos_guber,tasas_guber[match(fecha,tasas_guber$dates),-1]))
    tasa_div_IPC <- ((1 + as.numeric(interpolar_alam(venc_fut_IPC,nodos_dividendos,tasas_dividendos[match(fecha,tasas_dividendos$dates),-1])))^(venc_fut_IPC/180)-1)*(360/venc_fut_IPC)
    V0 <- -pos_fut_IPC*nom_fut_IPC*IPC_spot*venc_fut_IPC/(venc_fut_IPC*tasa_dom_IPC+360)
    
    return(V0)
  }
  
  # Cálculo de g y gx
  g_dom <- D_Valuacion_fut_IPC_dom(fecha_val,tasas_guber,tasas_dividendos,pos_fut_IPC,precio_strike_fut_IPC,nom_fut_IPC)
  g_tdiv <- D_Valuacion_fut_IPC_tdiv(fecha_val,tasas_guber,tasas_dividendos,pos_fut_IPC,precio_strike_fut_IPC,nom_fut_IPC)
  
  g_fut_ipc <- c(g_dom,g_tdiv)
  gx_fut_ipc <- as.matrix(g_fut_ipc*fut_ipc_x_0)
  
  # Cálculo de VaR y CVaR por Delta-Normal
  VaR_DN_fut_ipc <- sqrt(t(gx_fut_ipc) %*% VarCov_fut_ipc %*% gx_fut_ipc)*qnorm(1-alpha)
  CVaR_DN_fut_ipc <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_fut_ipc) %*% VarCov_fut_ipc %*% gx_fut_ipc))*(-1)
  
  # Cálculo de VaR y CVaR por Delta-Normal total de Bonos
  var_diarias_futuros <- cbind(var_diarias_fwd_tdc,var_diarias_fut_ipc)
  VarCov_futuros  <- var(var_diarias_futuros)*((n-1)/(n-2))
  
  g_futuros  <- c(g_fwd_tdc,g_fut_ipc)
  gx_futuros  <- as.matrix(g_futuros*c(fwd_tdc_x_0,fut_ipc_x_0))
  
  # Cálculo de VaR y CVaR por Delta-Normal
  VaR_DN_futuros <- sqrt(t(gx_futuros ) %*% VarCov_futuros  %*% gx_futuros )*qnorm(1-alpha)
  CVaR_DN_futuros  <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_futuros) %*% VarCov_futuros  %*% gx_futuros))*(-1)
}

#####################################################################################
# Riesgo Swaps
{
  # Cálculo de varianza y matriz de varianza-covarianza según sea el caso
  var_diarias_swap_1 <- cbind(x_swaps[1:(n-1),1]/x_swaps[2:n,1] - 1,x_swaps[1:(n-1),2]/x_swaps[2:n,2] - 1)
  var_diarias_swap_2 <- cbind(x_swaps[1:(n-1),3]/x_swaps[2:n,3] - 1,x_swaps[1:(n-1),4]/x_swaps[2:n,4] - 1)
  colnames(var_diarias_swap_1) <- colnames(x_swaps)[1:2]
  colnames(var_diarias_swap_2) <- colnames(x_swaps)[3:4]
  var_diarias_swaps <- cbind(var_diarias_swap_1,var_diarias_swap_2)
    
  VarCov_swap_1 <- var(var_diarias_swap_1)*((n-1)/(n-2))
  VarCov_swap_2 <- var(var_diarias_swap_2)*((n-1)/(n-2))
  VarCov_swaps <- var(var_diarias_swaps)*((n-1)/(n-2))
  
  # Cálculo de derivadas respecto a las tasas TIIE y de DIRS
  D_Valuacion_swap_TIIE <- function(fecha,tasas_TIIE,tasas_DIRS,pos_swaps,venc_swaps,tasa_fija_swaps,paga_o_recibe_swaps,dias_pago_cupon_swaps){
    # Se calculan los cupones por pagar
    cup_por_pagar_swap <- trunc(venc_swaps/dias_pago_cupon_swaps)
    
    # Se determinan las fechas en la que se paga cupón y se calcula el valor del bono en cada fecha
    {
      cupon_swap <- matrix(0,cup_por_pagar_swap,13)
      colnames(cupon_swap) <- c("Fecha","Cupón Fijo","Días p/Cupón","Días transcurridos","Plazo Corto","Tasa corta p/Cupón","Tasa larga p/Cupón","Tasa Forward Variable","Cupón Variable","Tasa VPN","Pata Fija VPN","Pata Var VPN","VPN Flujos")
      cupon_swap <- as.data.frame(cupon_swap)
      
      cupon_swap$Fecha[1] <- fecha_val + (venc_swaps - trunc(venc_swaps/dias_pago_cupon_swaps)*dias_pago_cupon_swaps)
      cupon_swap$`Cupón Fijo`[1] <- pos_swaps*tasa_fija_swaps*dias_pago_cupon_swaps/360
      cupon_swap$`Días p/Cupón`[1] <- dias_pago_cupon_swaps
      cupon_swap$`Días transcurridos`[1] <- cupon_swap$`Días p/Cupón`[1]
      cupon_swap$`Plazo Corto`[1] <- max(cupon_swap$`Días transcurridos`[1]-dias_pago_cupon_swaps,0)
      cupon_swap$`Tasa corta p/Cupón`[1] <- as.numeric(interpolar_alam(cupon_swap$`Plazo Corto`[1],nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
      cupon_swap$`Tasa larga p/Cupón`[1] <- as.numeric(interpolar_alam(cupon_swap$`Días transcurridos`[1],nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
      cupon_swap$`Tasa Forward Variable`[1] <- ((360^2/(cupon_swap$`Días transcurridos`[1]-cupon_swap$`Plazo Corto`[1]))*pos_swaps*cupon_swap$`Días transcurridos`[1])/(cupon_swap$`Plazo Corto`[1]*cupon_swap$`Tasa corta p/Cupón`[1]+360)^2
      cupon_swap$`Cupón Variable`[1] <- pos_swaps*cupon_swap$`Tasa Forward Variable`[1]*dias_pago_cupon_swaps/360
      cupon_swap$`Tasa VPN`[1] <- as.numeric(interpolar_alam(cupon_swap$`Días transcurridos`[1],nodos_DIRS,tasas_DIRS[match(fecha,tasas_DIRS$dates),-1]))
      cupon_swap$`Pata Fija VPN`[1] <- (-1)^paga_o_recibe_swaps*(-cupon_swap$`Cupón Fijo`[1])/(1+cupon_swap$`Tasa VPN`[1]*cupon_swap$`Días transcurridos`[1]/360)
      cupon_swap$`Pata Var VPN`[1] <- (-1)^paga_o_recibe_swaps*(cupon_swap$`Cupón Variable`[1])/(1+cupon_swap$`Tasa VPN`[1]*cupon_swap$`Días transcurridos`[1]/360)
      cupon_swap$`VPN Flujos`[1] <- (-1)^paga_o_recibe_swaps*(cupon_swap$`Cupón Variable`[1]-cupon_swap$`Cupón Fijo`[1])/(1+cupon_swap$`Tasa VPN`[1]*cupon_swap$`Días transcurridos`[1]/360)
      
      for(i in 2:cup_por_pagar_swap){
        cupon_swap$Fecha[i] <- cupon_swap$Fecha[i-1] + dias_pago_cupon_swaps
        cupon_swap$`Cupón Fijo`[i] <- pos_swaps*tasa_fija_swaps*dias_pago_cupon_swaps/360
        cupon_swap$`Días p/Cupón`[i] <- dias_pago_cupon_swaps
        cupon_swap$`Días transcurridos`[i] <- cupon_swap$`Días transcurridos`[i-1]  + cupon_swap$`Días p/Cupón`[i]
        cupon_swap$`Plazo Corto`[i] <- max(cupon_swap$`Días transcurridos`[i]-dias_pago_cupon_swaps,0)
        cupon_swap$`Tasa corta p/Cupón`[i] <- as.numeric(interpolar_alam(cupon_swap$`Plazo Corto`[i],nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
        cupon_swap$`Tasa larga p/Cupón`[i] <- as.numeric(interpolar_alam(cupon_swap$`Días transcurridos`[i],nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
        cupon_swap$`Tasa Forward Variable`[i] <- ((360^2/(cupon_swap$`Días transcurridos`[i]-cupon_swap$`Plazo Corto`[i]))*pos_swaps*cupon_swap$`Días transcurridos`[i])/(cupon_swap$`Plazo Corto`[i]*cupon_swap$`Tasa corta p/Cupón`[i]+360)^2
        cupon_swap$`Cupón Variable`[i] <- pos_swaps*cupon_swap$`Tasa Forward Variable`[i]*dias_pago_cupon_swaps/360
        cupon_swap$`Tasa VPN`[i] <- as.numeric(interpolar_alam(cupon_swap$`Días transcurridos`[i],nodos_DIRS,tasas_DIRS[match(fecha,tasas_DIRS$dates),-1]))
        cupon_swap$`Pata Fija VPN`[i] <- (-1)^paga_o_recibe_swaps*(-cupon_swap$`Cupón Fijo`[i])/(1+cupon_swap$`Tasa VPN`[i]*cupon_swap$`Días transcurridos`[i]/360)
        cupon_swap$`Pata Var VPN`[i] <- (-1)^paga_o_recibe_swaps*(cupon_swap$`Cupón Variable`[i])/(1+cupon_swap$`Tasa VPN`[i]*cupon_swap$`Días transcurridos`[i]/360)
        cupon_swap$`VPN Flujos`[i] <- (-1)^paga_o_recibe_swaps*(cupon_swap$`Cupón Variable`[i]-cupon_swap$`Cupón Fijo`[i])/(1+cupon_swap$`Tasa VPN`[i]*cupon_swap$`Días transcurridos`[i]/360)      
      }
      cupon_swap$Fecha <- as.Date.numeric(cupon_swap$Fecha,origin = "1970-01-01")
    }
    # Se calcula el valor del Swap
    V0 <- sum(cupon_swap$`VPN Flujos`)
    
    return(V0)
  }
  
  
  D_Valuacion_swap_DIRS <- function(fecha,tasas_TIIE,tasas_DIRS,pos_swaps,venc_swaps,tasa_fija_swaps,paga_o_recibe_swaps,dias_pago_cupon_swaps){
    # Se calculan los cupones por pagar
    cup_por_pagar_swap <- trunc(venc_swaps/dias_pago_cupon_swaps)
    
    # Se determinan las fechas en la que se paga cupón y se calcula el valor del bono en cada fecha
    {
      cupon_swap <- matrix(0,cup_por_pagar_swap,13)
      colnames(cupon_swap) <- c("Fecha","Cupón Fijo","Días p/Cupón","Días transcurridos","Plazo Corto","Tasa corta p/Cupón","Tasa larga p/Cupón","Tasa Forward Variable","Cupón Variable","Tasa VPN","Pata Fija VPN","Pata Var VPN","VPN Flujos")
      cupon_swap <- as.data.frame(cupon_swap)
      
      cupon_swap$Fecha[1] <- fecha_val + (venc_swaps - trunc(venc_swaps/dias_pago_cupon_swaps)*dias_pago_cupon_swaps)
      cupon_swap$`Cupón Fijo`[1] <- pos_swaps*tasa_fija_swaps*dias_pago_cupon_swaps/360
      cupon_swap$`Días p/Cupón`[1] <- dias_pago_cupon_swaps
      cupon_swap$`Días transcurridos`[1] <- cupon_swap$`Días p/Cupón`[1]
      cupon_swap$`Plazo Corto`[1] <- max(cupon_swap$`Días transcurridos`[1]-dias_pago_cupon_swaps,0)
      cupon_swap$`Tasa corta p/Cupón`[1] <- as.numeric(interpolar_alam(cupon_swap$`Plazo Corto`[1],nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
      cupon_swap$`Tasa larga p/Cupón`[1] <- as.numeric(interpolar_alam(cupon_swap$`Días transcurridos`[1],nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
      cupon_swap$`Tasa Forward Variable`[1] <- (((1+cupon_swap$`Tasa larga p/Cupón`[1]*cupon_swap$`Días transcurridos`[1]/360)/(1+cupon_swap$`Tasa corta p/Cupón`[1]*cupon_swap$`Plazo Corto`[1]/360))-1)*(360/(cupon_swap$`Días transcurridos`[1]-cupon_swap$`Plazo Corto`[1]))
      cupon_swap$`Cupón Variable`[1] <- pos_swaps*cupon_swap$`Tasa Forward Variable`[1]*dias_pago_cupon_swaps/360
      cupon_swap$`Tasa VPN`[1] <- as.numeric(interpolar_alam(cupon_swap$`Días transcurridos`[1],nodos_DIRS,tasas_DIRS[match(fecha,tasas_DIRS$dates),-1]))
      cupon_swap$`Pata Fija VPN`[1] <- 360*(-1)^paga_o_recibe_swaps*cupon_swap$`Cupón Fijo`[1]/(cupon_swap$`Días transcurridos`[1]*cupon_swap$`Tasa VPN`[1]+360)^2
      cupon_swap$`Pata Var VPN`[1] <- -360*(-1)^paga_o_recibe_swaps*cupon_swap$`Cupón Variable`[1]/(cupon_swap$`Días transcurridos`[1]*cupon_swap$`Tasa VPN`[1]+360)^2
      cupon_swap$`VPN Flujos`[1] <- 360*(-1)^paga_o_recibe_swaps*(cupon_swap$`Cupón Fijo`[1]-cupon_swap$`Cupón Variable`[1])/(cupon_swap$`Días transcurridos`[1]*cupon_swap$`Tasa VPN`[1]+360)^2
      
      for(i in 2:cup_por_pagar_swap){
        cupon_swap$Fecha[i] <- cupon_swap$Fecha[i-1] + dias_pago_cupon_swaps
        cupon_swap$`Cupón Fijo`[i] <- pos_swaps*tasa_fija_swaps*dias_pago_cupon_swaps/360
        cupon_swap$`Días p/Cupón`[i] <- dias_pago_cupon_swaps
        cupon_swap$`Días transcurridos`[i] <- cupon_swap$`Días transcurridos`[i-1]  + cupon_swap$`Días p/Cupón`[i]
        cupon_swap$`Plazo Corto`[i] <- max(cupon_swap$`Días transcurridos`[i]-dias_pago_cupon_swaps,0)
        cupon_swap$`Tasa corta p/Cupón`[i] <- as.numeric(interpolar_alam(cupon_swap$`Plazo Corto`[i],nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
        cupon_swap$`Tasa larga p/Cupón`[i] <- as.numeric(interpolar_alam(cupon_swap$`Días transcurridos`[i],nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
        cupon_swap$`Tasa Forward Variable`[i] <- (((1+cupon_swap$`Tasa larga p/Cupón`[i]*cupon_swap$`Días transcurridos`[i]/360)/(1+cupon_swap$`Tasa corta p/Cupón`[i]*cupon_swap$`Plazo Corto`[i]/360))-1)*(360/(cupon_swap$`Días transcurridos`[i]-cupon_swap$`Plazo Corto`[i]))
        cupon_swap$`Cupón Variable`[i] <- pos_swaps*cupon_swap$`Tasa Forward Variable`[i]*dias_pago_cupon_swaps/360
        cupon_swap$`Tasa VPN`[i] <- as.numeric(interpolar_alam(cupon_swap$`Días transcurridos`[i],nodos_DIRS,tasas_DIRS[match(fecha,tasas_DIRS$dates),-1]))
        cupon_swap$`Pata Fija VPN`[i] <- 360*(-1)^paga_o_recibe_swaps*cupon_swap$`Cupón Fijo`[i]/(cupon_swap$`Días transcurridos`[i]*cupon_swap$`Tasa VPN`[i]+360)^2
        cupon_swap$`Pata Var VPN`[i] <- -360*(-1)^paga_o_recibe_swaps*cupon_swap$`Cupón Variable`[i]/(cupon_swap$`Días transcurridos`[i]*cupon_swap$`Tasa VPN`[i]+360)^2
        cupon_swap$`VPN Flujos`[i] <- 360*(-1)^paga_o_recibe_swaps*(cupon_swap$`Cupón Fijo`[i]-cupon_swap$`Cupón Variable`[i])/(cupon_swap$`Días transcurridos`[i]*cupon_swap$`Tasa VPN`[i]+360)^2
      }
      cupon_swap$Fecha <- as.Date.numeric(cupon_swap$Fecha,origin = "1970-01-01")
    }
    # Se calcula el valor del Swap
    V0 <- sum(cupon_swap$`VPN Flujos`)
    
    return(V0)
  }
  
  # Cálculo de g y gx
  g_swap_1 <- c(D_Valuacion_swap_TIIE(fecha_val,tasas_TIIE,tasas_DIRS,pos_swaps[1],venc_swaps[1],tasa_fija_swaps[1],paga_o_recibe_swaps[1],dias_pago_cupon_swaps[1]),D_Valuacion_swap_DIRS(fecha_val,tasas_TIIE,tasas_DIRS,pos_swaps[1],venc_swaps[1],tasa_fija_swaps[1],paga_o_recibe_swaps[1],dias_pago_cupon_swaps[1]))
  g_swap_2 <-c(D_Valuacion_swap_TIIE(fecha_val,tasas_TIIE,tasas_DIRS,pos_swaps[2],venc_swaps[2],tasa_fija_swaps[2],paga_o_recibe_swaps[2],dias_pago_cupon_swaps[2]),D_Valuacion_swap_DIRS(fecha_val,tasas_TIIE,tasas_DIRS,pos_swaps[2],venc_swaps[2],tasa_fija_swaps[2],paga_o_recibe_swaps[2],dias_pago_cupon_swaps[2]))

  gx_swap_1 <- as.matrix(g_swap_1*swaps_x_0[1:2])
  gx_swap_2 <- as.matrix(g_swap_2*swaps_x_0[3:4])
  
  g_swaps <- c(g_swap_1,g_swap_2)
  gx_swaps <- as.matrix(g_swaps*swaps_x_0)
  
  # Cálculo de VaR y CVaR por Delta-Normal
  VaR_DN_swaps_1 <- sqrt(t(gx_swap_1) %*% VarCov_swap_1 %*% gx_swap_1)*qnorm(1-alpha)
  VaR_DN_swaps_2 <- sqrt(t(gx_swap_2) %*% VarCov_swap_2 %*% gx_swap_2)*qnorm(1-alpha)
  CVaR_DN_swap_1  <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_swap_1) %*% VarCov_swap_1  %*% gx_swap_1))*(-1)
  CVaR_DN_swap_2  <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_swap_2) %*% VarCov_swap_2  %*% gx_swap_2))*(-1)
  
  # Cálculo de VaR y CVaR por Delta-Normal
  VaR_DN_swaps <- sqrt(t(gx_swaps) %*% VarCov_swaps %*% gx_swaps )*qnorm(1-alpha)
  CVaR_DN_swaps  <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_swaps) %*% VarCov_swaps  %*% gx_swaps))*(-1)
}

#####################################################################################
# Riesgo Opciones tasa de interés
{
  # Cálculo de varianza y matriz de varianza-covarianza según sea el caso
  var_diarias_opc_1 <- cbind(x_opc_ti[1:(n-1),1]/x_opc_ti[2:n,1] - 1,x_opc_ti[1:(n-1),2]/x_opc_ti[2:n,2] - 1,x_opc_ti[1:(n-1),3]/x_opc_ti[2:n,3] - 1)
  var_diarias_opc_2 <- cbind(x_opc_ti[1:(n-1),4]/x_opc_ti[2:n,4] - 1,x_opc_ti[1:(n-1),5]/x_opc_ti[2:n,5] - 1,x_opc_ti[1:(n-1),6]/x_opc_ti[2:n,6] - 1)
  colnames(var_diarias_opc_1) <- colnames(x_opc_ti)[1:3]
  colnames(var_diarias_opc_2) <- colnames(x_opc_ti)[4:6]
  var_diarias_opc <- cbind(var_diarias_opc_1,var_diarias_opc_2)
  
  VarCov_opc_1 <- var(var_diarias_opc_1)*((n-1)/(n-2))
  VarCov_opc_2 <- var(var_diarias_opc_2)*((n-1)/(n-2))
  VarCov_opc <- var(var_diarias_opc)*((n-1)/(n-2))
  
  D_Valuacion_opc_ti_TIIE <- function(fecha,tasas_TIIE,tasas_DIRS,tasas_volTIIE,pos_opc_ti,plazos_opc_ti,pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti,precio_strike_opc_ti,nom_opc_ti){
    tasa_spot <- as.numeric(interpolar_alam(plazos_opc_ti,nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
    plazo_largo <- pr_opc_ti + plazos_opc_ti
    tasa_larga <- as.numeric(interpolar_alam(plazo_largo,nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
    tasa_forward <- (((1 + tasa_larga*plazo_largo/d_base_opc_ti)/(1 + tasa_spot*plazos_opc_ti/d_base_opc_ti))-1)*(d_base_opc_ti/pr_opc_ti)
    tasa_dom_cont <- log(1 + as.numeric(interpolar_alam(plazos_opc_ti,nodos_DIRS,tasas_DIRS[match(fecha,tasas_DIRS$dates),-1]))*(plazos_opc_ti/360))*(365/plazos_opc_ti)
    vol <- as.numeric(interpolar_alam(plazos_opc_ti,nodos_volTIIE,tasas_volTIIE[match(fecha,tasas_volTIIE$dates),-1]))
    
    d_1 <- (vol*sqrt(plazos_opc_ti))/(sqrt(365)*tasa_forward)
    d_2 <- (vol*sqrt(plazos_opc_ti))/(sqrt(365)*tasa_forward)
    N_d_1 <- pnorm((-1)^call_o_put_opc_ti*d_1,0,1)
    N_d_2 <- pnorm((-1)^call_o_put_opc_ti*d_2,0,1)
    prima_opcion <- N_d_1*(tasa_forward*N_d_1 - precio_strike_opc_ti*N_d_2)*(-1)^call_o_put_opc_ti*(-1/365)*plazos_opc_ti*exp(-tasa_dom_cont*plazos_opc_ti/365)
    prima_caplet_o_flooret <- prima_opcion*((pr_opc_ti/d_base_opc_ti)/(1 + tasa_forward*pr_opc_ti/d_base_opc_ti))
    V0 <- prima_caplet_o_flooret*pos_opc_ti
    
    return(V0)
  }
  
  D_Valuacion_opc_ti_DIRS <- function(fecha,tasas_TIIE,tasas_DIRS,tasas_volTIIE,pos_opc_ti,plazos_opc_ti,pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti,precio_strike_opc_ti,nom_opc_ti){
    tasa_spot <- as.numeric(interpolar_alam(plazos_opc_ti,nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
    plazo_largo <- pr_opc_ti + plazos_opc_ti
    tasa_larga <- as.numeric(interpolar_alam(plazo_largo,nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
    tasa_forward <- (((1 + tasa_larga*plazo_largo/d_base_opc_ti)/(1 + tasa_spot*plazos_opc_ti[1]/d_base_opc_ti))-1)*(d_base_opc_ti/pr_opc_ti)
    tasa_dom_cont <- log(1 + as.numeric(interpolar_alam(plazos_opc_ti,nodos_DIRS,tasas_DIRS[match(fecha,tasas_DIRS$dates),-1]))*(plazos_opc_ti/360))*(365/plazos_opc_ti)
    vol <- as.numeric(interpolar_alam(plazos_opc_ti,nodos_volTIIE,tasas_volTIIE[match(fecha,tasas_volTIIE$dates),-1]))
    
    d_1 <- ((log(tasa_forward/precio_strike_opc_ti) + ((vol^2)/2)*(plazos_opc_ti[1]/365)))/(vol*(plazos_opc_ti/365)^(1/2))
    d_2 <- ((log(tasa_forward/precio_strike_opc_ti) - ((vol^2)/2)*(plazos_opc_ti[1]/365)))/(vol*(plazos_opc_ti/365)^(1/2))
    N_d_1 <- pnorm((-1)^call_o_put_opc_ti*d_1,0,1)
    N_d_2 <- pnorm((-1)^call_o_put_opc_ti*d_2,0,1)
    prima_opcion <- (-1)^call_o_put_opc_ti/365*plazos_opc_ti*exp(-tasa_dom_cont*plazos_opc_ti/365)*(precio_strike_opc_ti*N_d_2-tasa_forward*N_d_1)
    prima_caplet_o_flooret <- prima_opcion*((pr_opc_ti/d_base_opc_ti)/(1 + tasa_forward*pr_opc_ti/d_base_opc_ti))
    V0 <- prima_caplet_o_flooret*pos_opc_ti
    
    return(V0)
  }
  
  D_Valuacion_opc_ti_volTIIE <- function(fecha,tasas_TIIE,tasas_DIRS,tasas_volTIIE,pos_opc_ti,plazos_opc_ti,pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti,precio_strike_opc_ti,nom_opc_ti){
    tasa_spot <- as.numeric(interpolar_alam(plazos_opc_ti,nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
    plazo_largo <- pr_opc_ti + plazos_opc_ti
    tasa_larga <- as.numeric(interpolar_alam(plazo_largo,nodos_TIIE,tasas_TIIE[match(fecha,tasas_TIIE$dates),-1]))
    tasa_forward <- (((1 + tasa_larga*plazo_largo/d_base_opc_ti)/(1 + tasa_spot*plazos_opc_ti[1]/d_base_opc_ti))-1)*(d_base_opc_ti/pr_opc_ti)
    tasa_dom_cont <- log(1 + as.numeric(interpolar_alam(plazos_opc_ti,nodos_DIRS,tasas_DIRS[match(fecha,tasas_DIRS$dates),-1]))*(plazos_opc_ti/360))*(365/plazos_opc_ti)
    vol <- as.numeric(interpolar_alam(plazos_opc_ti,nodos_volTIIE,tasas_volTIIE[match(fecha,tasas_volTIIE$dates),-1]))
    
    d_1<- sqrt(plazos_opc_ti)*(730*log(tasa_forward/precio_strike_opc_ti) + 3*plazos_opc_ti*vol^2)/730*sqrt(365)
    d_2 <- sqrt(plazos_opc_ti)*(log(tasa_forward/precio_strike_opc_ti) + plazos_opc_ti*vol^2/730)/sqrt(365) - plazos_opc_ti^(3/2)*vol^2/365*sqrt(365)
    N_d_1 <- pnorm((-1)^call_o_put_opc_ti*d_1,0,1)
    N_d_2 <- pnorm((-1)^call_o_put_opc_ti*d_2,0,1)
    prima_opcion <- (tasa_forward*N_d_1 - precio_strike_opc_ti*N_d_2)*(-1)^call_o_put_opc_ti*exp(-tasa_dom_cont*plazos_opc_ti/365)
    prima_caplet_o_flooret <- prima_opcion*((pr_opc_ti/d_base_opc_ti)/(1 + tasa_forward*pr_opc_ti/d_base_opc_ti))
    V0 <- prima_caplet_o_flooret*pos_opc_ti
    
    return(V0)
  }
  
  # Cálculo de g y gx
  g_opc_1 <- c(D_Valuacion_opc_ti_TIIE(fecha_val,tasas_TIIE,tasas_DIRS,tasas_volTIIE,pos_opc_ti[1],plazos_opc_ti[1],pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti[1],precio_strike_opc_ti[1],nom_opc_ti[1]),D_Valuacion_opc_ti_DIRS(fecha_val,tasas_TIIE,tasas_DIRS,tasas_volTIIE,pos_opc_ti[1],plazos_opc_ti[1],pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti[1],precio_strike_opc_ti[1],nom_opc_ti[1]), D_Valuacion_opc_ti_volTIIE(fecha_val,tasas_TIIE,tasas_DIRS,tasas_volTIIE,pos_opc_ti[1],plazos_opc_ti[1],pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti[1],precio_strike_opc_ti[1],nom_opc_ti[1]))
  g_opc_2 <- c(D_Valuacion_opc_ti_TIIE(fecha_val,tasas_TIIE,tasas_DIRS,tasas_volTIIE,pos_opc_ti[2],plazos_opc_ti[2],pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti[2],precio_strike_opc_ti[2],nom_opc_ti[2]),D_Valuacion_opc_ti_DIRS(fecha_val,tasas_TIIE,tasas_DIRS,tasas_volTIIE,pos_opc_ti[2],plazos_opc_ti[2],pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti[2],precio_strike_opc_ti[2],nom_opc_ti[2]), D_Valuacion_opc_ti_volTIIE(fecha_val,tasas_TIIE,tasas_DIRS,tasas_volTIIE,pos_opc_ti[2],plazos_opc_ti[2],pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti[2],precio_strike_opc_ti[2],nom_opc_ti[2]))
  
  gx_opc_1 <- as.matrix(g_opc_1*opc_ti_x_0[1:3])
  gx_opc_2 <- as.matrix(g_opc_2*opc_ti_x_0[4:6])
  
  g_opc <- c(g_opc_1,g_opc_2)
  gx_opc <- as.matrix(g_opc*opc_ti_x_0)
  
  # Cálculo de VaR y CVaR por Delta-Normal
  VaR_DN_opc_1 <- sqrt(t(gx_opc_1) %*% VarCov_opc_1 %*% gx_opc_1)*qnorm(1-alpha)
  VaR_DN_opc_2 <- sqrt(t(gx_opc_2) %*% VarCov_opc_2 %*% gx_opc_2)*qnorm(1-alpha)
  CVaR_DN_opc_1  <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_opc_1) %*% VarCov_opc_1  %*% gx_opc_1))*(-1)
  CVaR_DN_opc_2  <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_opc_2) %*% VarCov_opc_2  %*% gx_opc_2))*(-1)
  
  # Cálculo de VaR y CVaR por Delta-Normal
  VaR_DN_opc <- sqrt(t(gx_opc) %*% VarCov_opc %*% gx_opc )*qnorm(1-alpha)
  CVaR_DN_opc  <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_opc) %*% VarCov_opc  %*% gx_opc))*(-1)
}

#####################################################################################
# Riesgo integral
# Medición de riesgo por factor de riesgo de todo el portafolio
{
  # Acciones
  # Acciones e IPC
  var_diarias_port_ac <- cbind(var_diarias_ac,var_diarias_fut_ipc)
  VarCov_port_ac <- var(var_diarias_port_ac)*((n-1)/(n-2))
  
  g_port_ac <- c(g_ac,g_fut_ipc)
  gx_port_ac <-  as.matrix(g_port_ac*c(ac_x0,fut_ipc_x_0))
  
  VaR_DN_port_ac <- sqrt(t(gx_port_ac) %*% VarCov_port_ac %*% gx_port_ac )*qnorm(1-alpha)
  CVaR_DN_port_ac <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_port_ac) %*% VarCov_port_ac  %*% gx_port_ac))*(-1)
  
  # Divisas
  VaR_DN_port_div <- sqrt(gx_div %*% VarCov_div %*% t(gx_div))*qnorm(1-alpha)
  CVaR_DN_port_div <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(gx_div %*% VarCov_div %*% t(gx_div)))*(-1)
  
  # Tasa de Interés
  # Swaps y bonos
  var_diarias_port_ti <- cbind(var_diarias_swaps,var_diarias_bonos)
  VarCov_port_ti <- var(var_diarias_port_ti)*((n-1)/(n-2))
  
  g_port_ti <- c(g_swaps,g_bonos)
  gx_port_ti <-  as.matrix(g_port_ti*c(swaps_x_0,cete_x_0,bono_m_x_0,bondes_x_0))
  
  VaR_DN_port_ti <- sqrt(t(gx_port_ti) %*% VarCov_port_ti %*% gx_port_ti )*qnorm(1-alpha)
  CVaR_DN_port_ti <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_port_ti) %*% VarCov_port_ti  %*% gx_port_ti))*(-1)
  
  # Tipo de cambio
  # Futuro tdc
  VaR_DN_port_tdc <- sqrt(t(gx_fwd_tdc) %*% VarCov_fwd_tdc %*% gx_fwd_tdc)*qnorm(1-alpha)
  CVaR_DN_port_tdc <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_fwd_tdc) %*% VarCov_fwd_tdc  %*% gx_fwd_tdc))*(-1)
  
  # Volatilidad
  # Opciones de tasa de interés
  VaR_DN_port_vol <- sqrt(t(gx_opc) %*% VarCov_opc %*% gx_opc)*qnorm(1-alpha)
  CVaR_DN_port_vol <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_opc) %*% VarCov_opc  %*% gx_opc))*(-1)
  
  # Medición de riesgo de todo el portafolio
  var_diarias_port <- cbind(var_diarias_port_ac,var_diarias_div,var_diarias_port_ti,var_diarias_fwd_tdc,var_diarias_opc)
  VarCov_port <- var(var_diarias_port)*((n-1)/(n-2))
  
  g_port <- c(g_port_ac,g_div,g_port_ti,g_fwd_tdc,g_opc)
  gx_port <-  as.matrix(g_port*c(ac_x0,fut_ipc_x_0,div_x0,swaps_x_0,cete_x_0,bono_m_x_0,bondes_x_0,fwd_tdc_x_0,opc_ti_x_0))
  
  VaR_DN_port <- sqrt(t(gx_port) %*% VarCov_port %*% gx_port )*qnorm(1-alpha)
  CVaR_DN_port <- ((dnorm(qnorm(1-alpha))/(1 - alpha))*sqrt(t(gx_port) %*% VarCov_port  %*% gx_port))*(-1)
}

#####################################################################################
#####################################################################################
# Cálculo de VaR y CVaR marginal y total por instrumento por Componentes Principales
# Para todos los instrumentos hay que definir y calcular los siguientes elementos:
#   1.Historico de factores de riesgo
#   2.Vector de precios actual
#   3.Valoración al día actual
#####################################################################################
# Riesgo Acciones y divisas
{
  # Cálculo de acciones
  prom_ac <- c(mean(var_diarias_ac[,1]),mean(var_diarias_ac[,2]),mean(var_diarias_ac[,3]))
  sd_ac <- c(sd(var_diarias_ac[,1]),sd(var_diarias_ac[,2]),sd(var_diarias_ac[,3]))
  
  VarCov_ac <- var(var_diarias_ac)*((n-1)/(n-2))
  
  G_ac <- eigen(VarCov_ac)      #eigenvectores
  g1_ac <-G_ac$values       #eigenvalores
  g1p_ac <- g1_ac/sum(g1_ac)
  g1pac_ac <- cumsum(g1p_ac) #suma acumulada de varianza
  k_ac <- which.max(g1pac_ac > eta) #valor mínimo k
  y_cp_ac <- as.matrix((var_diarias_ac - prom_ac)) %*% G_ac$vectors[,1:k_ac] #componentes principales
  
  sim_ac<- matrix(0,nrow = nb_sims,ncol = k_ac)
  colnames(sim_ac) <- acciones[1:k_ac]
  for (i in 1:ncol(sim_ac)) {
    sim_ac[,i] <- rnorm(nb_sims)*sd_ac[i] + prom_ac[i]
  }
  
  var_diarias_ac_sim <- sim_ac %*% t(G_ac$vectors[,1:k_ac])
  
  precios_ac_sim <- sweep(1 + var_diarias_ac_sim, MARGIN = 2, ac_x0, `*`)
  
  V_ac_sim<- sweep(precios_ac_sim, MARGIN = 2, pos_ac, `*`)

  PnL_cp_ac <- matrix(0,nb_sims,ncol(V_ac_sim))
  colnames(PnL_cp_ac) <- acciones
  for (i in 1:nb_sims) {
    PnL_cp_ac[i,] <- V_ac_sim[i,] - V0_ac
  }
  
  VaR_CP_ac_ind <- matrix(0,1,ncol(PnL_cp_ac))
  colnames(VaR_CP_ac_ind) <- acciones
  for(i in 1:ncol(PnL_cp_ac)){
    VaR_CP_ac_ind[i] <- quantile(PnL_cp_ac[,i],1-alpha)
  }

  CVaR_CP_ac_ind <- matrix(0,1,ncol(PnL_cp_ac))
  colnames(CVaR_CP_ac_ind) <- acciones
  for(i in 1:ncol(PnL_cp_ac)){
    CVaR_CP_ac_ind[i] <- mean(merge(which(PnL_cp_ac[,i] < VaR_CP_ac_ind[i]),cbind(seq(1,nb_sims),PnL_cp_ac[,i]), by.x=1,by.y=1)[,2])
  }
  
  # Cálculo de VaR Total de acciones
  VaR_CP_ac<- quantile(rowSums(PnL_cp_ac),1-alpha)
  CVaR_CP_ac <- mean(merge(which(rowSums(PnL_cp_ac) < VaR_CP_ac),cbind(seq(1,nb_sims),rowSums(PnL_cp_ac)), by.x=1,by.y=1)[,2])
  
  
# Cálculo para divisas
  prom_div <- c(mean(var_diarias_div[,1]),mean(var_diarias_div[,2]),mean(var_diarias_div[,3]))
  sd_div <- c(sd(var_diarias_div[,1]),sd(var_diarias_div[,2]),sd(var_diarias_div[,3]))
  
  VarCov_div <- var(var_diarias_div)*((n-1)/(n-2))
  
  G_div <- eigen(VarCov_div)      #eigenvectores
  g1_div <-G_div$values       #eigenvalores
  g1p_div <- g1_div/sum(g1_div)
  g1pac_div <- cumsum(g1p_div) #suma acumulada de varianza
  k_div <- which.max(g1pac_div > eta) #valor mínimo k
  y_cp_div <- as.matrix((var_diarias_div - prom_div)) %*% G_div$vectors[,1:k_div] #componentes principales
  
  sim_div <- matrix(0,nrow = nb_sims,ncol = k_div)
  colnames(sim_div) <- divisas[1:k_div]
  for (i in 1:ncol(sim_div)) {
    sim_div[,i] <- rnorm(nb_sims)*sd_div[i] + prom_div[i]
  }
  
  var_diarias_div_sim <- sim_div %*% t(G_div$vectors[,1:k_div])
  
  precios_div_sim <- sweep(1 + var_diarias_div_sim, MARGIN = 2, div_x0, `*`)
  
  V_div_sim<- sweep(precios_div_sim, MARGIN = 2, pos_div, `*`)
  
  PnL_cp_div <- matrix(0,nb_sims,ncol(V_div_sim))
  colnames(PnL_cp_div) <- divisas
  for (i in 1:nb_sims) {
    PnL_cp_div[i,] <- V_div_sim[i,] - V0_div
  }
  
  VaR_CP_div_ind <- matrix(0,1,ncol(PnL_cp_div))
  colnames(VaR_CP_div_ind) <- divisas
  for(i in 1:ncol(PnL_cp_div)){
    VaR_CP_div_ind[i] <- quantile(PnL_cp_div[,i],1-alpha)
  }
  
  CVaR_CP_div_ind <- matrix(0,1,ncol(PnL_cp_div))
  colnames(CVaR_CP_div_ind) <- divisas
  for(i in 1:ncol(PnL_cp_div)){
    CVaR_CP_div_ind[i] <- mean(merge(which(PnL_cp_div[,i] < VaR_CP_div_ind[i]),cbind(seq(1,nb_sims),PnL_cp_div[,i]), by.x=1,by.y=1)[,2])
  }
  
  # Cálculo de VaR Total de divisas
  VaR_CP_div <- quantile(rowSums(PnL_cp_div),1-alpha)
  CVaR_CP_div <- mean(merge(which(rowSums(PnL_cp_div) < VaR_CP_div),cbind(seq(1,nb_sims),rowSums(PnL_cp_div)), by.x=1,by.y=1)[,2])
}

#####################################################################################
# Riesgo Cete
{
  prom_cete <- mean(var_diarias_cete[,1])
  sd_cete <-sd(var_diarias_cete[,1])
  
  VarCov_cete <- var(var_diarias_cete)*((n-1)/(n-2))
  
  G_cete <- eigen(VarCov_cete)      #eigenvectores
  g1_cete <-G_cete$values       #eigenvalores
  g1p_cete <- g1_cete/sum(g1_cete)
  g1pac_cete <- cumsum(g1p_cete) #suma acumulada de varianza
  k_cete <- which.max(g1pac_cete > eta) #valor mínimo k
  y_cp_cete <- as.matrix((var_diarias_cete - prom_cete)) %*% G_cete$vectors[,1:k_cete] #componentes principales
  
  sim_cete <- matrix(0,nrow = nb_sims,ncol = k_cete)
  colnames(sim_cete) <- "Cete"
  for (i in 1:ncol(sim_cete)) {
    sim_cete[,i] <- rnorm(nb_sims)*sd_cete[i] + prom_cete[i]
  }
  
  var_diarias_cete_sim <- sim_cete %*% t(G_cete$vectors[,1:k_cete])
  
  tasas_cete_sim <- sweep(1 + var_diarias_cete_sim, MARGIN = 2, cete_x_0, `*`)
  
  V_cete_sim <- (pos_cete*nom_cete)/(1+ tasas_cete_sim*venc_cete/360)
  
  PnL_cp_cete <- matrix(0,nb_sims,ncol(V_cete_sim))
  colnames(PnL_cp_cete) <- "Cete"
  for (i in 1:nrow(PnL_cp_cete)) {
    PnL_cp_cete[i,] <- V_cete_sim[i,] - V0_cete
  }
  
  # Cálculo de VaR Y CVaR
  VaR_CP_cete <- quantile(PnL_cp_cete,1-alpha)
  CVaR_CP_cete <- mean(merge(which(PnL_cp_cete < VaR_CP_cete),cbind(seq(1,nb_sims),PnL_cp_cete), by.x=1,by.y=1)[,2])
}

#####################################################################################
# Riesgo Bono M
{
  prom_bono_m <- mean(var_diarias_bono_m[,1])
  sd_bono_m <-sd(var_diarias_bono_m[,1])
  
  VarCov_bono_m <- var(var_diarias_bono_m)*((n-1)/(n-2))
  
  G_bono_m <- eigen(VarCov_bono_m)      #eigenvectores
  g1_bono_m <-G_bono_m$values       #eigenvalores
  g1p_bono_m <- g1_bono_m/sum(g1_bono_m)
  g1pac_bono_m <- cumsum(g1p_bono_m) #suma acumulada de varianza
  k_bono_m <- which.max(g1pac_bono_m > eta) #valor mínimo k
  y_cp_bono_m <- as.matrix((var_diarias_bono_m - prom_bono_m)) %*% G_bono_m$vectors[,k_bono_m] #componentes principales
  
  sim_bono_m <- matrix(0,nrow = nb_sims,ncol = 1)
  colnames(sim_bono_m) <- "Cete"
  for (i in 1:ncol(sim_bono_m)) {
    sim_bono_m[,i] <- rnorm(nb_sims)*sd_bono_m[i] + prom_bono_m[i]
  }
  
  var_diarias_bono_m_sim <- sim_bono_m %*% t(G_bono_m$vectors[,k_bono_m])
  
  tasas_bono_m_sim <- sweep(1 + var_diarias_bono_m_sim, MARGIN = 2, bono_m_x_0, `*`)
  
  V_bono_m_sim <- Valuacion_Bono_M(fecha_val,tasas_bono_m_sim ,pos_bono_m, nom_bono_m, venc_bono_m, tasa_cupon_fija_bono_m, dias_pago_cupon_bono_m)
  
  PnL_cp_bono_m <- matrix(0,nb_sims,ncol(V_bono_m_sim))
  colnames(PnL_cp_bono_m) <- "Bono M"
  for (i in 1:nrow(PnL_cp_bono_m)) {
    PnL_cp_bono_m[i,] <- V_bono_m_sim[i,] - V0_bono_m
  }
  
  # Cálculo de VaR Y CVaR
  VaR_CP_bono_m <- quantile(PnL_cp_bono_m,1-alpha)
  CVaR_CP_bono_m <- mean(merge(which(PnL_cp_bono_m < VaR_CP_bono_m),cbind(seq(1,nb_sims),PnL_cp_bono_m), by.x=1,by.y=1)[,2])
}

#####################################################################################
# Riesgo Bonde D
{
  prom_bondes <- c(mean(var_diarias_bondes[,1]),mean(var_diarias_bondes[,2]),mean(var_diarias_bondes[,3]))
  sd_bondes <- c(sd(var_diarias_bondes[,1]),sd(var_diarias_bondes[,2]),sd(var_diarias_bondes[,3]))
  
  VarCov_bondes <- var(var_diarias_bondes)*((n-1)/(n-2))
  
  G_bondes <- eigen(VarCov_bondes)      #eigenvectores
  g1_bondes <-G_bondes$values       #eigenvalores
  g1p_bondes <- g1_bondes/sum(g1_bondes)
  g1pac_bondes <- cumsum(g1p_bondes) #suma acumulada de varianza
  k_bondes <- which.max(g1pac_bondes > eta) #valor mínimo k
  y_cp_bondes <- as.matrix((var_diarias_bondes - prom_bondes)) %*% G_bondes$vectors[,1:k_bondes] #componentes principales
  
  sim_bondes <- matrix(0,nrow = nb_sims,ncol = k_bondes)
  colnames(sim_bondes) <- colnames(var_diarias_bondes)[1:k_bondes]
  for (i in 1:ncol(sim_bondes)) {
    sim_bondes[,i] <- rnorm(nb_sims)*sd_bondes[i] + prom_bondes[i]
  }
  
  var_diarias_bondes_sim <- sim_bondes %*% t(G_bondes$vectors[,1:k_bondes])
  
  tasas_bondes_sim <- sweep(1 + var_diarias_bondes_sim, MARGIN = 2, bondes_x_0, `*`)
  tasas_guber_sim <- tasas_bondes_sim[,1]
  tasas_guber_st_sim <- tasas_bondes_sim[,2]
  tasas_fondeo_sim <- tasas_bondes_sim[,3]
  
  Val_sim_Bondes <- function(tasas_guber_sim,tasas_guber_st_sim,tasas_fondeo_sim,pos_bondes,nom_bondes,dias_pago_cupon_bondes,venc_bondes){
    # Se calculan los cupones por pagar
    cup_por_pagar_bondes <- trunc(venc_bondes/dias_pago_cupon_bondes) + 1
    
    # Se calculan los días devengados del cupón vigente
    dias_deveng_cupon_bondes <- dias_pago_cupon_bondes - (venc_bondes - trunc(venc_bondes/dias_pago_cupon_bondes)*dias_pago_cupon_bondes)

    # Se determinan las fechas en la que se paga cupón y se calcula el valor del bono en cada fecha
    {
      cupon_bondes <- matrix(0,cup_por_pagar_bondes,8)
      colnames(cupon_bondes)<-c("Fecha","Tasa cupón","Cupón","Días cupón","Días transcurridos","Tasa VPN","Sobretasa VPN","VPN Cupón")
      cupon_bondes <- as.data.frame(cupon_bondes)
      
      cupon_bondes$Fecha[1] <- 1
      cupon_bondes$`Tasa cupón`[1] <- ((1 + (prod(1+tasas_fondeo_sim/36000)-1)*(36000/dias_deveng_cupon_bondes)*dias_deveng_cupon_bondes/360)*(1+tasas_fondeo_sim/360)^(dias_pago_cupon_bondes - dias_deveng_cupon_bondes) - 1)*360/dias_pago_cupon_bondes
      cupon_bondes$Cupón[1] <- pos_bondes*nom_bondes*cupon_bondes$`Tasa cupón`[1]*dias_pago_cupon_bondes/360
      cupon_bondes$`Días cupón`[1] <- venc_bondes - trunc(venc_bondes/dias_pago_cupon_bondes)*dias_pago_cupon_bondes
      cupon_bondes$`Días transcurridos`[1] <- cupon_bondes$`Días cupón`[1]
      cupon_bondes$`Tasa VPN`[1] <- tasas_guber_sim
      cupon_bondes$`Sobretasa VPN`[1] <- tasas_guber_st_sim
      cupon_bondes$`VPN Cupón`[1] <- cupon_bondes$Cupón[1]/(1+(cupon_bondes$`Tasa VPN`[1] + cupon_bondes$`Sobretasa VPN`[1])*cupon_bondes$`Días transcurridos`[1]/360)
      
      for(i in 2:cup_por_pagar_bondes){
        cupon_bondes$Fecha[i] <- i
        cupon_bondes$`Tasa cupón`[i] <- (((1 + tasas_fondeo_sim/360)^dias_pago_cupon_bondes)-1)*(360/dias_pago_cupon_bondes)
        cupon_bondes$Cupón[i] <- pos_bondes*nom_bondes*cupon_bondes$`Tasa cupón`[i]*dias_pago_cupon_bondes/360
        cupon_bondes$`Días cupón`[i] <- dias_pago_cupon_bondes
        cupon_bondes$`Días transcurridos`[i] <- cupon_bondes$`Días cupón`[i] + cupon_bondes$`Días transcurridos`[i-1] 
        cupon_bondes$`Tasa VPN`[i] <- tasas_guber_sim
        cupon_bondes$`Sobretasa VPN`[i] <- tasas_guber_st_sim
        cupon_bondes$`VPN Cupón`[i] <- cupon_bondes$Cupón[i]/(1+(cupon_bondes$`Tasa VPN`[i] + cupon_bondes$`Sobretasa VPN`[i])*cupon_bondes$`Días transcurridos`[i]/360)
      }
      cupon_bondes$Cupón[cup_por_pagar_bondes] <- cupon_bondes$Cupón[cup_por_pagar_bondes] + pos_bondes*nom_bondes
      cupon_bondes$`VPN Cupón`[cup_por_pagar_bondes] <- cupon_bondes$Cupón[cup_por_pagar_bondes]/(1+(cupon_bondes$`Tasa VPN`[cup_por_pagar_bondes]+cupon_bondes$`Sobretasa VPN`[cup_por_pagar_bondes])*cupon_bondes$`Días transcurridos`[cup_por_pagar_bondes]/360)
      cupon_bondes$Fecha <- as.Date.numeric(cupon_bondes$Fecha,origin = "1970-01-01")
      
      # Se calcula el valor del bono
      V0 <- sum(cupon_bondes$`VPN Cupón`)
      
      return(V0)
    }
  }
  
  V_bondes_sim <- matrix(0,nb_sims,1)
  for (i in 1:nb_sims) {
    V_bondes_sim[i,] <- Val_sim_Bondes(tasas_guber_sim[i],tasas_guber_st_sim[i],tasas_fondeo_sim[i],pos_bondes,nom_bondes,dias_pago_cupon_bondes,venc_bondes)
  }
  
  PnL_cp_bondes <- matrix(0,nb_sims,ncol(V_bondes_sim))
  colnames(PnL_cp_bondes) <- "Bonde D"
  for (i in 1:nrow(PnL_cp_bondes)) {
    PnL_cp_bondes[i,] <- V_bondes_sim[i,] - V0_bondes
  }
  
  # Cálculo de VaR Y CVaR
  VaR_CP_bondes <- quantile(PnL_cp_bondes,1-alpha)
  CVaR_CP_bondes <- mean(merge(which(PnL_cp_bondes < VaR_CP_bondes),cbind(seq(1,nb_sims),PnL_cp_bondes), by.x=1,by.y=1)[,2])
  
  # Cálculo de VaR Y CVaR para bonos
  PnL_cp_bonos <- cbind(PnL_cp_cete,PnL_cp_bono_m,PnL_cp_bondes)
  VaR_CP_bonos <- quantile(rowSums(PnL_cp_bonos),1-alpha)
  CVaR_CP_bonos <- mean(merge(which(rowSums(PnL_cp_bonos) < VaR_CP_bonos),cbind(seq(1,nb_sims),rowSums(PnL_cp_bonos)), by.x=1,by.y=1)[,2])
}

#####################################################################################
# Riesgo Forward TDC
{
  prom_fwd_tdc <- c(mean(var_diarias_fwd_tdc[,1]),mean(var_diarias_fwd_tdc[,2]))
  sd_fwd_tdc <- c(sd(var_diarias_fwd_tdc[,1]),sd(var_diarias_fwd_tdc[,2]))
  
  VarCov_fwd_tdc <- var(var_diarias_fwd_tdc)*((n-1)/(n-2))
  
  G_fwd_tdc <- eigen(VarCov_fwd_tdc)      #eigenvectores
  g1_fwd_tdc <-G_fwd_tdc$values       #eigenvalores
  g1p_fwd_tdc <- g1_fwd_tdc/sum(g1_fwd_tdc)
  g1pac_fwd_tdc <- cumsum(g1p_fwd_tdc) #suma acumulada de varianza
  k_fwd_tdc <- which.max(g1pac_fwd_tdc > eta) #valor mínimo k
  y_cp_fwd_tdc <- as.matrix((var_diarias_fwd_tdc - prom_fwd_tdc)) %*% G_fwd_tdc$vectors[,1:k_fwd_tdc] #componentes principales
  
  sim_fwd_tdc <- matrix(0,nrow = nb_sims,ncol = k_fwd_tdc)
  colnames(sim_fwd_tdc) <- colnames(var_diarias_fwd_tdc)[1:k_fwd_tdc]
  for (i in 1:ncol(sim_fwd_tdc)) {
    sim_fwd_tdc[,i] <- rnorm(nb_sims)*sd_fwd_tdc[i] + prom_fwd_tdc[i]
  }
  
  var_diarias_fwd_tdc_sim <- sim_fwd_tdc %*% t(G_fwd_tdc$vectors[,1:k_fwd_tdc])
  
  tasas_fwd_tdc_sim <- sweep(1 + var_diarias_fwd_tdc_sim, MARGIN = 2, fwd_tdc_x_0, `*`)
  tasas_libor_sim <- tasas_fwd_tdc_sim[,1]
  tasas_fwd_sim <- tasas_fwd_tdc_sim[,2]

  Val_sim_fwd_tdc<- function(tasas_libor_sim,tasas_fwd_sim,moneda_fwd_tdc,pos_fwd_tdc,venc_fwd_tdc,precio_strike_fwd_tdc,nom_fwd_tdc){
    tc_spot_fwd <- precios_div[1,match(moneda_fwd_tdc,colnames(precios_div))]
    tasa_dom_fwd <- tasas_fwd_sim
    tasa_ext_fwd <- ((1 + tasas_libor_sim)^(venc_fwd_tdc/180)-1)*360/venc_fwd_tdc
    tc_fwd <- tc_spot_fwd*(1 + tasa_dom_fwd*venc_fwd_tdc/360)/(1 + tasa_ext_fwd*venc_fwd_tdc/360)
    V0 <- pos_fwd_tdc*(tc_fwd - precio_strike_fwd_tdc)/(1 + tasa_dom_fwd*venc_fwd_tdc/360)
    
    return(V0)
  }
  
  V_fwd_tdc_sim <- matrix(0,nb_sims,1)
  for (i in 1:nb_sims) {
    V_fwd_tdc_sim[i,] <- Val_sim_fwd_tdc(tasas_libor_sim[i],tasas_fwd_sim[i],moneda_fwd_tdc,pos_fwd_tdc,venc_fwd_tdc,precio_strike_fwd_tdc,nom_fwd_tdc)
  }
  
  PnL_cp_fwd_tdc <- matrix(0,nb_sims,ncol(V_fwd_tdc_sim))
  colnames(PnL_cp_fwd_tdc) <- "Fwd TDC"
  for (i in 1:nrow(PnL_cp_fwd_tdc)) {
    PnL_cp_fwd_tdc[i,] <- V_fwd_tdc_sim[i,] - V0_fwd_tdc
  }
  
  # Cálculo de VaR Y CVaR
  VaR_CP_fwd_tdc <- quantile(PnL_cp_fwd_tdc,1-alpha)
  CVaR_CP_fwd_tdc <- mean(merge(which(PnL_cp_fwd_tdc < VaR_CP_fwd_tdc),cbind(seq(1,nb_sims),PnL_cp_fwd_tdc), by.x=1,by.y=1)[,2])
}

#####################################################################################
# Riesgo Forward TDC
{
  prom_fut_ipc <- c(mean(var_diarias_fut_ipc[,1]),mean(var_diarias_fut_ipc[,2]))
  sd_fut_ipc <- c(sd(var_diarias_fut_ipc[,1]),sd(var_diarias_fut_ipc[,2]))  
  
  VarCov_fut_ipc <- var(var_diarias_fut_ipc)*((n-1)/(n-2))
  
  G_fut_ipc <- eigen(VarCov_fut_ipc)      #eigenvectores
  g1_fut_ipc <-G_fut_ipc$values       #eigenvalores
  g1p_fut_ipc <- g1_fut_ipc/sum(g1_fut_ipc)
  g1pac_fut_ipc <- cumsum(g1p_fut_ipc) #suma acumulada de varianza
  k_fut_ipc <- which.max(g1pac_fut_ipc > eta) #valor mínimo k
  y_cp_fut_ipc <- as.matrix((var_diarias_fut_ipc - prom_fut_ipc)) %*% G_fut_ipc$vectors[,1:k_fut_ipc] #componentes principales
  
  sim_fut_ipc <- matrix(0,nrow = nb_sims,ncol = k_fut_ipc)
  colnames(sim_fut_ipc) <- colnames(var_diarias_fut_ipc)[1:k_fut_ipc]
  for (i in 1:ncol(sim_fut_ipc)) {
    sim_fut_ipc[,i] <- rnorm(nb_sims)*sd_fut_ipc[i] + prom_fut_ipc[i]
  }
  
  var_diarias_fut_ipc_sim <- sim_fut_ipc %*% t(G_fut_ipc$vectors[,1:k_fut_ipc])
  
  tasas_fut_ipc_sim <- sweep(1 + var_diarias_fut_ipc_sim, MARGIN = 2, fut_ipc_x_0, `*`)
  tasas_guber_fut_sim <- tasas_fut_ipc_sim[,1]
  tasas_div_sim <- tasas_fut_ipc_sim[,2]
  
  Val_sim_fut_IPC <- function(tasas_guber_fut_sim,tasas_div_sim,pos_fut_IPC,precio_strike_fut_IPC,nom_fut_IPC){
    IPC_spot <- precios_IPC[1,]
    tasa_dom_IPC <- tasas_guber_fut_sim
    tasa_div_IPC <- ((1 + tasas_div_sim)^(venc_fut_IPC/180)-1)*(360/venc_fut_IPC)
    IPC_forward <- IPC_spot*(1 + (tasa_dom_IPC - tasa_div_IPC)*(venc_fut_IPC/360))
    V0 <- pos_fut_IPC*nom_fut_IPC*(IPC_forward - precio_strike_fut_IPC)/(1 + tasa_dom_IPC*venc_fut_IPC/360)
    
    return(V0)
  }
  
  V_fut_ipc_sim <- matrix(0,nb_sims,1)
  for (i in 1:nb_sims) {
    V_fut_ipc_sim[i,] <- Val_sim_fut_IPC(tasas_guber_fut_sim[i],tasas_div_sim[i],pos_fut_IPC,precio_strike_fut_IPC,nom_fut_IPC)
  }
  
  PnL_cp_fut_ipc <- matrix(0,nb_sims,ncol(V_fut_ipc_sim))
  colnames(PnL_cp_fut_ipc) <- "Futuro IPC"
  for (i in 1:nrow(PnL_cp_fut_ipc)) {
    PnL_cp_fut_ipc[i,] <- V_fut_ipc_sim[i,] - V0_fut_IPC
  }
  
  # Cálculo de VaR Y CVaR
  VaR_CP_fut_ipc <- quantile(PnL_cp_fut_ipc,1-alpha)
  CVaR_CP_fut_ipc <- mean(merge(which(PnL_cp_fut_ipc < VaR_CP_fut_ipc),cbind(seq(1,nb_sims),PnL_cp_fut_ipc), by.x=1,by.y=1)[,2])
  
  # Cálculo de VaR Y CVaR de futuros
  PnL_cp_futuros <- cbind(PnL_cp_fwd_tdc,PnL_cp_fut_ipc)
  VaR_CP_futuros <- quantile(rowSums(PnL_cp_futuros),1-alpha)
  CVaR_CP_futuros <- mean(merge(which(rowSums(PnL_cp_futuros) < VaR_CP_futuros),cbind(seq(1,nb_sims),rowSums(PnL_cp_futuros)), by.x=1,by.y=1)[,2])
}

#####################################################################################
# Riesgo Swaps
{
  prom_swap_1 <- c(mean(var_diarias_swap_1[,1]),mean(var_diarias_swap_1[,2]))
  sd_swap_1 <- c(sd(var_diarias_swap_1[,1]),sd(var_diarias_swap_1[,2]))  
  
  VarCov_swap_1 <- var(var_diarias_swap_1)*((n-1)/(n-2))
  
  G_swap_1 <- eigen(VarCov_swap_1)      #eigenvectores
  g1_swap_1 <-G_swap_1$values       #eigenvalores
  g1p_swap_1 <- g1_swap_1/sum(g1_swap_1)
  g1pac_swap_1 <- cumsum(g1p_swap_1) #suma acumulada de varianza
  k_swap_1 <- which.max(g1pac_swap_1 > eta) #valor mínimo k
  y_cp_swap_1 <- as.matrix((var_diarias_swap_1 - prom_swap_1)) %*% G_swap_1$vectors[,1:k_swap_1] #componentes principales
  
  sim_swap_1 <- matrix(0,nrow = nb_sims,ncol = k_swap_1)
  colnames(sim_swap_1) <- colnames(var_diarias_swap_1)[1:k_swap_1]
  for (i in 1:ncol(sim_swap_1)) {
    sim_swap_1[,i] <- rnorm(nb_sims)*sd_swap_1[i] + prom_swap_1[i]
  }
  
  var_diarias_swap_1_sim <- sim_swap_1 %*% t(G_swap_1$vectors[,1:k_swap_1])
  
  tasas_swap_1_sim <- sweep(1 + var_diarias_swap_1_sim, MARGIN = 2, swaps_x_0[1:2], `*`)
  tasas_TIIE_sim <- tasas_swap_1_sim[,1]
  tasas_DIRS_sim <- tasas_swap_1_sim[,2]
  
  Val_sim_swaps <- function(tasas_TIIE_sim,tasas_DIRS_sim,pos_swaps,venc_swaps,tasa_fija_swaps,paga_o_recibe_swaps,dias_pago_cupon_swaps){
    # Se calculan los cupones por pagar
    cup_por_pagar_swap <- trunc(venc_swaps/dias_pago_cupon_swaps)
    
    # Se determinan las fechas en la que se paga cupón y se calcula el valor del bono en cada fecha
    {
      cupon_swap <- matrix(0,cup_por_pagar_swap,13)
      colnames(cupon_swap) <- c("Fecha","Cupón Fijo","Días p/Cupón","Días transcurridos","Plazo Corto","Tasa corta p/Cupón","Tasa larga p/Cupón","Tasa Forward Variable","Cupón Variable","Tasa VPN","Pata Fija VPN","Pata Var VPN","VPN Flujos")
      cupon_swap <- as.data.frame(cupon_swap)
      
      cupon_swap$Fecha[1] <- 1
      cupon_swap$`Cupón Fijo`[1] <- pos_swaps*tasa_fija_swaps*dias_pago_cupon_swaps/360
      cupon_swap$`Días p/Cupón`[1] <- dias_pago_cupon_swaps
      cupon_swap$`Días transcurridos`[1] <- cupon_swap$`Días p/Cupón`[1]
      cupon_swap$`Plazo Corto`[1] <- max(cupon_swap$`Días transcurridos`[1]-dias_pago_cupon_swaps,0)
      cupon_swap$`Tasa corta p/Cupón`[1] <- tasas_TIIE_sim
      cupon_swap$`Tasa larga p/Cupón`[1] <- tasas_TIIE_sim
      cupon_swap$`Tasa Forward Variable`[1] <- (((1+cupon_swap$`Tasa larga p/Cupón`[1]*cupon_swap$`Días transcurridos`[1]/360)/(1+cupon_swap$`Tasa corta p/Cupón`[1]*cupon_swap$`Plazo Corto`[1]/360))-1)*(360/(cupon_swap$`Días transcurridos`[1]-cupon_swap$`Plazo Corto`[1]))
      cupon_swap$`Cupón Variable`[1] <- pos_swaps*cupon_swap$`Tasa Forward Variable`[1]*dias_pago_cupon_swaps/360
      cupon_swap$`Tasa VPN`[1] <- tasas_DIRS_sim
      cupon_swap$`Pata Fija VPN`[1] <- (-1)^paga_o_recibe_swaps*(-cupon_swap$`Cupón Fijo`[1])/(1+cupon_swap$`Tasa VPN`[1]*cupon_swap$`Días transcurridos`[1]/360)
      cupon_swap$`Pata Var VPN`[1] <- (-1)^paga_o_recibe_swaps*(cupon_swap$`Cupón Variable`[1])/(1+cupon_swap$`Tasa VPN`[1]*cupon_swap$`Días transcurridos`[1]/360)
      cupon_swap$`VPN Flujos`[1] <- (-1)^paga_o_recibe_swaps*(cupon_swap$`Cupón Variable`[1]-cupon_swap$`Cupón Fijo`[1])/(1+cupon_swap$`Tasa VPN`[1]*cupon_swap$`Días transcurridos`[1]/360)
      
      for(i in 2:cup_por_pagar_swap){
        cupon_swap$Fecha[i] <- i
        cupon_swap$`Cupón Fijo`[i] <- pos_swaps*tasa_fija_swaps*dias_pago_cupon_swaps/360
        cupon_swap$`Días p/Cupón`[i] <- dias_pago_cupon_swaps
        cupon_swap$`Días transcurridos`[i] <- cupon_swap$`Días transcurridos`[i-1]  + cupon_swap$`Días p/Cupón`[i]
        cupon_swap$`Plazo Corto`[i] <- max(cupon_swap$`Días transcurridos`[i]-dias_pago_cupon_swaps,0)
        cupon_swap$`Tasa corta p/Cupón`[i] <- tasas_TIIE_sim
        cupon_swap$`Tasa larga p/Cupón`[i] <- tasas_TIIE_sim
        cupon_swap$`Tasa Forward Variable`[i] <- (((1+cupon_swap$`Tasa larga p/Cupón`[i]*cupon_swap$`Días transcurridos`[i]/360)/(1+cupon_swap$`Tasa corta p/Cupón`[i]*cupon_swap$`Plazo Corto`[i]/360))-1)*(360/(cupon_swap$`Días transcurridos`[i]-cupon_swap$`Plazo Corto`[i]))
        cupon_swap$`Cupón Variable`[i] <- pos_swaps*cupon_swap$`Tasa Forward Variable`[i]*dias_pago_cupon_swaps/360
        cupon_swap$`Tasa VPN`[i] <- tasas_DIRS_sim
        cupon_swap$`Pata Fija VPN`[i] <- (-1)^paga_o_recibe_swaps*(-cupon_swap$`Cupón Fijo`[i])/(1+cupon_swap$`Tasa VPN`[i]*cupon_swap$`Días transcurridos`[i]/360)
        cupon_swap$`Pata Var VPN`[i] <- (-1)^paga_o_recibe_swaps*(cupon_swap$`Cupón Variable`[i])/(1+cupon_swap$`Tasa VPN`[i]*cupon_swap$`Días transcurridos`[i]/360)
        cupon_swap$`VPN Flujos`[i] <- (-1)^paga_o_recibe_swaps*(cupon_swap$`Cupón Variable`[i]-cupon_swap$`Cupón Fijo`[i])/(1+cupon_swap$`Tasa VPN`[i]*cupon_swap$`Días transcurridos`[i]/360)
      }
      cupon_swap$Fecha <- as.Date.numeric(cupon_swap$Fecha,origin = "1970-01-01")
    }
    # Se calcula el valor del Swap
    V0 <- sum(cupon_swap$`VPN Flujos`)
    
    return(V0)
  }
  
  V_swap_1_sim <- matrix(0,nb_sims,1)
  for (i in 1:nb_sims) {
    V_swap_1_sim[i,] <- Val_sim_swaps(tasas_TIIE_sim[i],tasas_DIRS_sim[i],pos_swaps,venc_swaps,tasa_fija_swaps,paga_o_recibe_swaps,dias_pago_cupon_swaps)
  }
  
  PnL_cp_swap_1 <- matrix(0,nb_sims,ncol(V_swap_1_sim))
  colnames(PnL_cp_swap_1) <- "Swap 1"
  for (i in 1:nrow(PnL_cp_swap_1)) {
    PnL_cp_swap_1[i,] <- V_swap_1_sim[i,] - V0_swap[1]
  }
  
  # Cálculo de VaR Y CVaR
  VaR_CP_swap_1 <- quantile(PnL_cp_swap_1,1-alpha)
  CVaR_CP_swap_1 <- mean(merge(which(PnL_cp_swap_1 < VaR_CP_swap_1),cbind(seq(1,nb_sims),PnL_cp_swap_1), by.x=1,by.y=1)[,2])
  
  prom_swap_2 <- c(mean(var_diarias_swap_2[,1]),mean(var_diarias_swap_2[,2]))
  sd_swap_2 <- c(sd(var_diarias_swap_2[,1]),sd(var_diarias_swap_2[,2]))  
  
  VarCov_swap_2 <- var(var_diarias_swap_2)*((n-1)/(n-2))
  
  G_swap_2 <- eigen(VarCov_swap_2)      #eigenvectores
  g1_swap_2 <-G_swap_2$values       #eigenvalores
  g1p_swap_2 <- g1_swap_2/sum(g1_swap_2)
  g1pac_swap_2 <- cumsum(g1p_swap_2) #suma acumulada de varianza
  k_swap_2 <- which.max(g1pac_swap_2 > eta) #valor mínimo k
  y_cp_swap_2 <- as.matrix((var_diarias_swap_2 - prom_swap_2)) %*% G_swap_2$vectors[,1:k_swap_2] #componentes principales
  
  sim_swap_2 <- matrix(0,nrow = nb_sims,ncol = k_swap_2)
  colnames(sim_swap_2) <- colnames(var_diarias_swap_2)[1:k_swap_2]
  for (i in 1:ncol(sim_swap_2)) {
    sim_swap_2[,i] <- rnorm(nb_sims)*sd_swap_2[i] + prom_swap_2[i]
  }
  
  var_diarias_swap_2_sim <- sim_swap_2 %*% t(G_swap_2$vectors[,1:k_swap_2])
  
  tasas_swap_2_sim <- sweep(1 + var_diarias_swap_2_sim, MARGIN = 2, swaps_x_0[3:4], `*`)
  tasas_TIIE_sim <- tasas_swap_2_sim[,1]
  tasas_DIRS_sim <- tasas_swap_2_sim[,2]
  
  V_swap_2_sim <- matrix(0,nb_sims,1)
  for (i in 1:nb_sims) {
    V_swap_2_sim[i,] <- Val_sim_swaps(tasas_TIIE_sim[i],tasas_DIRS_sim[i],pos_swaps,venc_swaps,tasa_fija_swaps,paga_o_recibe_swaps,dias_pago_cupon_swaps)
  }
  
  PnL_cp_swap_2 <- matrix(0,nb_sims,ncol(V_swap_2_sim))
  colnames(PnL_cp_swap_2) <- "Swap 2"
  for (i in 1:nrow(PnL_cp_swap_2)) {
    PnL_cp_swap_2[i,] <- V_swap_2_sim[i,] - V0_swap[2]
  }
  
  # Cálculo de VaR Y CVaR
  VaR_CP_swap_2 <- quantile(PnL_cp_swap_2,1-alpha)
  CVaR_CP_swap_2 <- mean(merge(which(PnL_cp_swap_2 < VaR_CP_swap_2),cbind(seq(1,nb_sims),PnL_cp_swap_2), by.x=1,by.y=1)[,2])
  
  # Cálculo de VaR Y CVaR de futuros
  PnL_cp_swaps <- cbind(PnL_cp_swap_1,PnL_cp_swap_2)
  VaR_CP_swaps <- quantile(rowSums(PnL_cp_swaps),1-alpha)
  CVaR_CP_swaps <- mean(merge(which(rowSums(PnL_cp_swaps) < VaR_CP_swaps),cbind(seq(1,nb_sims),rowSums(PnL_cp_swaps)), by.x=1,by.y=1)[,2])
}

#####################################################################################
# Riesgo Opciones tasas de interés
{
  prom_opc_1 <- c(mean(var_diarias_opc_1[,1]),mean(var_diarias_opc_1[,2]),mean(var_diarias_opc_1[,3]))
  sd_opc_1 <- c(sd(var_diarias_opc_1[,1]),sd(var_diarias_opc_1[,2]),sd(var_diarias_opc_1[,3]))  
  
  VarCov_opc_1 <- var(var_diarias_opc_1)*((n-1)/(n-2))
  
  G_opc_1 <- eigen(VarCov_opc_1)      #eigenvectores
  g1_opc_1 <-G_opc_1$values       #eigenvalores
  g1p_opc_1 <- g1_opc_1/sum(g1_opc_1)
  g1pac_opc_1 <- cumsum(g1p_opc_1) #suma acumulada de varianza
  k_opc_1 <- which.max(g1pac_opc_1 > eta) #valor mínimo k
  y_cp_opc_1 <- as.matrix((var_diarias_opc_1 - prom_opc_1)) %*% G_opc_1$vectors[,1:k_opc_1] #componentes principales
  
  sim_opc_1 <- matrix(0,nrow = nb_sims,ncol = k_opc_1)
  colnames(sim_opc_1) <- colnames(var_diarias_opc_1)[1:k_opc_1]
  for (i in 1:ncol(sim_opc_1)) {
    sim_opc_1[,i] <- rnorm(nb_sims)*sd_opc_1[i] + prom_opc_1[i]
  }
  
  var_diarias_opc_1_sim <- sim_opc_1 %*% t(G_opc_1$vectors[,1:k_opc_1])
  
  tasas_opc_1_sim <- sweep(1 + var_diarias_opc_1_sim, MARGIN = 2, opc_ti_x_0[1:3], `*`)
  tasas_TIIE_sim <- tasas_opc_1_sim[,1]
  tasas_DIRS_sim <- tasas_opc_1_sim[,2]
  tasas_volTIIE_sim <- tasas_opc_1_sim[,3]
  
  Val_sim_opc <- function(tasas_TIIE_sim,tasas_DIRS_sim,tasas_volTIIE_sim,pos_opc_ti,plazos_opc_ti,pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti,precio_strike_opc_ti,nom_opc_ti){
    tasa_spot <- tasas_TIIE_sim
    plazo_largo <- pr_opc_ti + plazos_opc_ti
    tasa_larga <- tasas_TIIE_sim
    tasa_forward <- (((1 + tasa_larga*plazo_largo/d_base_opc_ti)/(1 + tasa_spot*plazos_opc_ti/d_base_opc_ti))-1)*(d_base_opc_ti/pr_opc_ti)
    tasa_dom_cont <- log(1 + tasas_DIRS_sim*(plazos_opc_ti/360))*(365/plazos_opc_ti[1])
    vol <- tasas_volTIIE_sim
    
    d_1 <- ((log(tasa_forward/precio_strike_opc_ti) + ((vol^2)/2)*(plazos_opc_ti/365)))/(vol*(plazos_opc_ti/365)^(1/2))
    d_2 <- ((log(tasa_forward/precio_strike_opc_ti) - ((vol^2)/2)*(plazos_opc_ti/365)))/(vol*(plazos_opc_ti/365)^(1/2))
    N_d_1 <- pnorm((-1)^call_o_put_opc_ti*d_1,0,1)
    N_d_2 <- pnorm((-1)^call_o_put_opc_ti*d_2,0,1)
    prima_opcion <- (tasa_forward*N_d_1 - precio_strike_opc_ti*N_d_2)*(-1)^call_o_put_opc_ti*exp(-tasa_dom_cont*plazos_opc_ti/365)
    prima_caplet_o_flooret <- prima_opcion*((pr_opc_ti/d_base_opc_ti)/(1 + tasa_forward*pr_opc_ti/d_base_opc_ti))
    V0 <- prima_caplet_o_flooret*pos_opc_ti
    
    return(V0)
  }
  
  V_opc_1_sim <- matrix(0,nb_sims,1)
  for (i in 1:nb_sims) {
  V_opc_1_sim[i,] <- Val_sim_opc(tasas_TIIE_sim[i],tasas_DIRS_sim[i],tasas_volTIIE_sim[i],pos_opc_ti[1],plazos_opc_ti[1],pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti[1],precio_strike_opc_ti[1],nom_opc_ti[1])
  }
  
  PnL_cp_opc_1 <- matrix(0,nb_sims,ncol(V_opc_1_sim))
  colnames(PnL_cp_opc_1) <- "Opción 1"
  for (i in 1:nrow(PnL_cp_opc_1)) {
    PnL_cp_opc_1[i,] <- V_opc_1_sim[i,] - V0_opc_ti[1]
  }
  
  # Cálculo de VaR Y CVaR
  VaR_CP_opc_1 <- quantile(PnL_cp_opc_1,1-alpha)
  CVaR_CP_opc_1 <- mean(merge(which(PnL_cp_opc_1 < VaR_CP_opc_1),cbind(seq(1,nb_sims),PnL_cp_opc_1), by.x=1,by.y=1)[,2])
  
  prom_opc_2 <- c(mean(var_diarias_opc_2[,1]),mean(var_diarias_opc_2[,2]),mean(var_diarias_opc_2[,3]))
  sd_opc_2 <- c(sd(var_diarias_opc_2[,1]),sd(var_diarias_opc_2[,2]),sd(var_diarias_opc_2[,3]))  
  
  VarCov_opc_2 <- var(var_diarias_opc_2)*((n-1)/(n-2))
  
  G_opc_2 <- eigen(VarCov_opc_2)      #eigenvectores
  g1_opc_2 <-G_opc_2$values       #eigenvalores
  g1p_opc_2 <- g1_opc_2/sum(g1_opc_2)
  g1pac_opc_2 <- cumsum(g1p_opc_2) #suma acumulada de varianza
  k_opc_2 <- which.max(g1pac_opc_2 > eta) #valor mínimo k
  y_cp_opc_2 <- as.matrix((var_diarias_opc_2 - prom_opc_2)) %*% G_opc_2$vectors[,1:k_opc_2] #componentes principales
  
  sim_opc_2 <- matrix(0,nrow = nb_sims,ncol = k_opc_2)
  colnames(sim_opc_2) <- colnames(var_diarias_opc_2)[1:k_opc_2]
  for (i in 1:ncol(sim_opc_2)) {
    sim_opc_2[,i] <- rnorm(nb_sims)*sd_opc_2[i] + prom_opc_2[i]
  }
  
  var_diarias_opc_2_sim <- sim_opc_2 %*% t(G_opc_2$vectors[,1:k_opc_2])
  
  tasas_opc_2_sim <- sweep(1 + var_diarias_opc_2_sim, MARGIN = 2, opc_ti_x_0[4:6], `*`)
  tasas_TIIE_sim <- tasas_opc_2_sim[,1]
  tasas_DIRS_sim <- tasas_opc_2_sim[,2]
  tasas_volTIIE_sim <- tasas_opc_2_sim[,3]
  
  V_opc_2_sim <- matrix(0,nb_sims,1)
  for (i in 1:nb_sims) {
    V_opc_2_sim[i,] <- Val_sim_opc(tasas_TIIE_sim[i],tasas_DIRS_sim[i],tasas_volTIIE_sim[i],pos_opc_ti[2],plazos_opc_ti[2],pr_opc_ti,d_base_opc_ti,call_o_put_opc_ti[2],precio_strike_opc_ti[2],nom_opc_ti[2])
  }
  
  PnL_cp_opc_2 <- matrix(0,nb_sims,ncol(V_opc_2_sim))
  colnames(PnL_cp_opc_2) <- "Opción 2"
  for (i in 1:nrow(PnL_cp_opc_2)) {
    PnL_cp_opc_2[i,] <- V_opc_2_sim[i,] - V0_opc_ti[2]
  }
  
  # Cálculo de VaR Y CVaR
  VaR_CP_opc_2 <- quantile(PnL_cp_opc_2,1-alpha)
  CVaR_CP_opc_2 <- mean(merge(which(PnL_cp_opc_2 < VaR_CP_opc_2),cbind(seq(1,nb_sims),PnL_cp_opc_2), by.x=1,by.y=1)[,2])
  
  # Cálculo de VaR Y CVaR total de opciones
  PnL_cp_opc <- cbind(PnL_cp_opc_1,PnL_cp_opc_2)
  VaR_CP_opc <- quantile(rowSums(PnL_cp_opc),1-alpha)
  CVaR_CP_opc <- mean(merge(which(rowSums(PnL_cp_opc) < VaR_CP_opc),cbind(seq(1,nb_sims),rowSums(PnL_cp_opc)), by.x=1,by.y=1)[,2])
}

#####################################################################################
# Riesgo integral
# Medición de riesgo por factor de riesgo de todo el portafolio
{
  # Acciones
  # Acciones e IPC
  PnL_CP_port_ac <- cbind(PnL_cp_ac,PnL_cp_fut_ipc)
  VaR_DN_port_ac <-  quantile(rowSums(PnL_CP_port_ac),1-alpha)
  CVaR_DN_port_ac <- mean(merge(which(rowSums(PnL_CP_port_ac) < VaR_DN_port_ac),cbind(seq(1,nb_sims),rowSums(PnL_CP_port_ac)), by.x=1,by.y=1)[,2])
  
  # Divisas
  VaR_DN_port_div <- VaR_CP_div
  CVaR_DN_port_div <- CVaR_CP_div
  
  # Tasa de Interés
  # Swaps y bonos
  PnL_CP_port_ti <- cbind(PnL_cp_swaps,PnL_cp_bonos)
  VaR_DN_port_ti <-  quantile(rowSums(PnL_CP_port_ti),1-alpha)
  CVaR_DN_port_ti <- mean(merge(which(rowSums(PnL_CP_port_ti) < VaR_DN_port_ti),cbind(seq(1,nb_sims),rowSums(PnL_CP_port_ti)), by.x=1,by.y=1)[,2])
  
  # Tipo de cambio
  # Futuro tdc
  VaR_DN_port_tdc <- VaR_CP_fwd_tdc
  CVaR_DN_port_tdc <- CVaR_CP_fwd_tdc
  
  # Volatilidad
  # Opciones de tasa de interés
  VaR_DN_port_tdc <- VaR_CP_opc
  CVaR_DN_port_tdc <- CVaR_CP_opc
  
  # Medición de riesgo de todo el portafolio
  PnL_CP_port <- cbind(PnL_cp_port_ac,PnL_cp_div,PnL_CP_port_ti,PnL_cp_fwd_tdc,PnL_cp_opc)
  VaR_DN_port <- quantile(rowSums(PnL_CP_port),1-alpha)
  CVaR_DN_port <- mean(merge(which(rowSums(PnL_CP_port) < VaR_DN_port),cbind(seq(1,nb_sims),rowSums(PnL_CP_port)), by.x=1,by.y=1)[,2])
}