library(rCharts)
#library(shiny)
library(plotly)
library(ggplot2)
library(datasets)
library(magrittr)
library(XML)
library(reshape)
library(gsheet)
library(scales)
library(zoo)
library(pracma)
library(psych)
library(devtools)
library(rmarkdown)
library(astro)

# Use the google spreadsheet
collected_data <- "https://docs.google.com/spreadsheets/d/1oPTPmoJ9phtMOkp-nMB7WHnPESomLzqUj9t0gcE9bYA"
conflicts <- gsheet2text(collected_data, sheetid = 819472314)
conflicts.long <- read.csv(text=conflicts)

before <-gsheet2text(collected_data, sheetid = 457614883)
before.long <- read.csv(text=before)
#arrs.long <- head(arrs.long, -30)

future <-gsheet2text(collected_data, sheetid = 677621454)
future.long <-read.csv(text=future)
#deps.long <- head(deps.long, -30)

current <-gsheet2text(collected_data, sheetid = 772694153)
current.long <-read.csv(text=current)

rain <-gsheet2text(collected_data, sheetid = 1473662223)
rain.long <- read.csv(text=rain,stringsAsFactors = FALSE)
#rain.long <- head(rain.long, -30)

#read the WaterDrumPrices
water <-gsheet2text(collected_data, sheetid =27261871)
water.long <- read.csv(text=water,stringsAsFactors = FALSE)
water.long$Date <-as.Date(water.long$Date, format="%m/%d/%Y")
#water.long <- data.frame(cbind(sapply(water.long[,sapply(water.long,is.character)],trimws,which="both"),water.long[,!sapply(df,is.character)]))
water.long[,1:ncol(water.long)] <- sapply(water.long[,1:ncol(water.long)],as.numeric)

#read the Rivers
rivers <-gsheet2text(collected_data, sheetid =407366559)
rivers.long <- read.csv(text=rivers,stringsAsFactors = FALSE)

#read the Goat Prices
goats <-gsheet2text(collected_data, sheetid =1601716765)
goats.long <- read.csv(text=goats,stringsAsFactors = FALSE)

#read the Fatalities
fatalities <-gsheet2text(collected_data, sheetid =343810263)
fatalities.long <- read.csv(text=fatalities,stringsAsFactors = FALSE)

discharge <- gsheet2text(collected_data, sheetid=407366559)
discharge.long <- read.csv(text=discharge,stringsAsFactors = FALSE)

stations <- gsheet2text(collected_data, sheetid=1052168743)
stations.long <- read.csv(text=stations,stringsAsFactors = FALSE)

cash <- gsheet2text(collected_data, sheetid = 161900539)
cash.long <- read.csv(text=cash,stringsAsFactors = FALSE)

cases <- gsheet2text(collected_data, sheetid = 15526228)
cases.long <- read.csv(text=cases,stringsAsFactors = FALSE)

deaths <- gsheet2text(collected_data, sheetid = 2060381151)
deaths.long <- read.csv(text=deaths,stringsAsFactors = FALSE)

dollos <- gsheet2text(collected_data, sheetid = 1111574539)
dollos.long <- read.csv(text=dollos,stringsAsFactors = FALSE)


Dates <- sapply(conflicts.long[,1],as.character.Date)
conflicts.long$Date <- as.Date(conflicts.long$Date, format="%m/%d/%Y")
before.long$Date <- as.Date(before.long$Date, format="%m/%d/%Y")
future.long$Date <- as.Date(future.long$Date, format="%m/%d/%Y")
current.long$Date <- as.Date(current.long$Date, format="%m/%d/%Y")
rain.long$Date <-as.Date(rain.long$Date, format="%m/%d/%Y")
rivers.long$Date <-as.Date(rivers.long$Date, format="%m/%d/%Y")
goats.long$Date <-as.Date(goats.long$Date, format="%m/%d/%Y")
fatalities.long$Date <-as.Date(fatalities.long$Date, format="%m/%d/%Y")
stations.long$Date <-as.Date(stations.long$Date, format="%m/%d/%Y")
cash.long$Date <-as.Date(cash.long$Date, format="%m/%d/%Y")
cases.long$Date <-as.Date(cases.long$Date, format="%m/%d/%Y")
deaths.long$Date <-as.Date(deaths.long$Date, format="%m/%d/%Y")
dollos.long$Date <-as.Date(dollos.long$Date, format="%m/%d/%Y")


# Force columns to be text
conflicts.long[,2:ncol(conflicts.long)] <- sapply(conflicts.long[,2:ncol(conflicts.long)], as.numeric)
before.long[,2:ncol(before.long)] <- sapply(before.long[,2:ncol(before.long)], as.numeric)
future.long[,2:ncol(future.long)] <- sapply(future.long[,2:ncol(future.long)], as.numeric)
current.long[,2:ncol(current.long)] <- sapply(current.long[,2:ncol(current.long)], as.numeric)
rain.long[,2:ncol(rain.long)] <- sapply(rain.long[,2:ncol(rain.long)], as.numeric)
rivers.long[,2:ncol(rivers.long)] <- sapply(rivers.long[,2:ncol(rivers.long)], as.numeric)
goats.long[,2:ncol(goats.long)] <- sapply(goats.long[,2:ncol(goats.long)], as.numeric)
fatalities.long[,2:ncol(fatalities.long)] <- sapply(fatalities.long[,2:ncol(fatalities.long)], as.numeric)
stations.long[,2:ncol(stations.long)] <- sapply(stations.long[,2:ncol(stations.long)], as.numeric)
cash.long[,2:ncol(cash.long)] <- sapply(cash.long[,2:ncol(cash.long)], as.numeric)
cases.long[,2:ncol(cases.long)] <- sapply(cases.long[,2:ncol(cases.long)], as.numeric)
deaths.long[,2:ncol(deaths.long)] <- sapply(deaths.long[,2:ncol(deaths.long)], as.numeric)
dollos.long[,2:ncol(dollos.long)] <- sapply(dollos.long[,2:ncol(dollos.long)], as.numeric)

#use this spreadsheet to automatically update functions
modelarrivals_AWminus1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- mean(future.long[(t-5):(t- 1),"Bari_FutureRegion"], na.rm=TRUE)
    B<- median(before.long[(t-16):(t- 1),"Sool_BeforeRegion"], na.rm=TRUE)
    C<- tail(movavg(before.long[(t-3):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], 2,type="m"),1)
    D<- rain.long[(t- 1),"Awdal_rain"]
    E<- current.long[(t- 1),"Bari_CurrentRegion"]
    G<- rain.long[(t- 1),"Shabeellaha_Hoose_rain"]
    H<- rain.long[(t- 1),"Awdal_rain"]
    I<- current.long[(t- 1),"Bari_CurrentRegion"]
    J<- fatalities.long[(t- 1),"Bari_Fatalities"]
    K<- current.long[(t- 1),"Mudug_CurrentRegion"]
    L<- fatalities.long[(t- 14),"Banaadir_Fatalities"]
    M<- rain.long[(t- 1),"Shabeellaha_Hoose_rain"]
    N<- fatalities.long[(t- 2),"Banaadir_Fatalities"]
    O<- before.long[(t- 16),"Togdheer_BeforeRegion"]
    P<- before.long[(t- 10),"Nugaal_BeforeRegion"]
    Q<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    R<- fatalities.long[(t- 1),"Bari_Fatalities"]
    S<- rain.long[(t- 1),"Awdal_rain"]
    U<- current.long[(t- 1),"Bari_CurrentRegion"]
    V<- rain.long[(t- 1),"Awdal_rain"]
    if ( is.na(sum(0.0120582852371404*D*E , G,na.rm=TRUE)) ){W<- O}
    else if(sum(0.0120582852371404*D*E , G,na.rm=TRUE)>0){W<-sum( 0.0120582852371404*H*I , 5.60195139754713e-5*J*K*L , M , N,na.rm=TRUE) }
    else{W<- O }
    X<- max(C, W,na.rm=TRUE)
    Y<- max(A,sum( B , X , -P,na.rm=TRUE),na.rm=TRUE)
    Z<- max(Q, R,na.rm=TRUE)
    AA<- atan2(0.0120582852371404*S*U, 0.0120582852371404*V)
    if(is.infinite(Y)){Y <- 0 }
    if(is.infinite(Z)){Z <- 0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(A)){A <- 0 }
    FIN <-sum( Y , -Z*AA,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AWminus2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 2),"Mudug_CurrentRegion"]
    B<- future.long[(t- 2),"Sool_FutureRegion"]
    C<- before.long[(t- 2),"Shabeellaha_Hoose_BeforeRegion"]
    D<- fatalities.long[(t- 15),"Togdheer_Fatalities"]
    E<- current.long[(t- 2),"Gedo_CurrentRegion"]
    G<- tail(movavg(before.long[(t-6):(t- 2),"Sool_BeforeRegion"], 4,type="m"),1)
    H<- mean(stations.long[(t-8):(t- 2),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    I<- median(fatalities.long[(t-17):(t- 2),"Banaadir_Fatalities"], na.rm=TRUE)
    J<- mean(fatalities.long[(t-5):(t- 2),"Banaadir_Fatalities"], na.rm=TRUE)
    K<- median(stations.long[(t-12):(t- 2),"Gedo_LuuqStation_Juba_River"], na.rm=TRUE)
    L<- tail(movavg(before.long[(t-4):(t- 2),"Mudug_BeforeRegion"],2,type="w"),1)
    M<- sqrt(B)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 0.01288615129487*A*M , 0.00226345110029269*C*D , 8.53792904988097e-6*E*G , 1.68003703353206*H*I , -J*K , -0.0312251379007826*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AW1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 15),"Bari_BeforeRegion"]
    B<- current.long[(t- 15),"Jubbada_Dhexe_CurrentRegion"]
    C<- median(before.long[(t-12):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    D<- before.long[(t- 15),"Sool_BeforeRegion"]
    E<- current.long[(t- 15),"Sool_CurrentRegion"]
    G<- conflicts.long[(t- 1),"Mudug_Conflict"]
    H<- fatalities.long[(t- 2),"Togdheer_Fatalities"]
    I<- current.long[(t- 15),"Jubbada_Dhexe_CurrentRegion"]
    J<- before.long[(t- 15),"Bari_BeforeRegion"]
    K<- current.long[(t- 15),"Jubbada_Dhexe_CurrentRegion"]
    L<- median(before.long[(t-12):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    M<- before.long[(t- 15),"Sool_BeforeRegion"]
    N<- current.long[(t- 15),"Sool_CurrentRegion"]
    O<- current.long[(t- 15),"Jubbada_Dhexe_CurrentRegion"]
    P<- tail(movavg(before.long[(t-9):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], 8,type="m"),1)
    Q<- fatalities.long[(t- 12),"Togdheer_Fatalities"]
    R<- future.long[(t- 1),"Togdheer_FutureRegion"]
    S<- before.long[(t- 15),"Bari_BeforeRegion"]
    U<- rain.long[(t- 5),"Bakool_rain"]
    V<- max(G*H, 0.00189690282163337*I,na.rm=TRUE)
    W<- sum(0.566054870160388*J ,0.00189690282163337*K*L , 0.000116228953606384*M*N, na.rm=TRUE) %% 0.00189690282163337*O
    X<- max(0.566054870160388*S, U,na.rm=TRUE)
    if ( is.na(Q) ){Y<- X}
    else if(Q>0){Y<- 0.652241128913957*R }
    else{Y<- X }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(W)){W <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Y)){Y <- 0 }
    FIN <-sum( 0.566054870160388*A , 0.00189690282163337*B*C , 0.000116228953606384*D*E , V*W , P , Y,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AW2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(before.long[(t-3):(t- 1),"Nugaal_BeforeRegion"], 2,type="m"),1)
    B<- rain.long[(t- 1),"Awdal_rain"]
    C<- current.long[(t- 1),"Bari_CurrentRegion"]
    D<- tail(movavg(current.long[(t-7):(t- 1),"Mudug_CurrentRegion"], 6,type="m"),1)
    E<- fatalities.long[(t- 1),"Bari_Fatalities"]
    G<- median(current.long[(t-16):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    H<- rain.long[(t- 6),"Bari_rain"]
    I<- fatalities.long[(t- 10),"Shabeellaha_Hoose_Fatalities"]
    J<- median(future.long[(t-14):(t- 1),"Hiiraan_FutureRegion"], na.rm=TRUE)
    K<- fatalities.long[(t- 1),"Bari_Fatalities"]
    L<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    M<- tail(movavg(current.long[(t-7):(t- 1),"Mudug_CurrentRegion"], 6,type="m"),1)
    N<- atan(0.00438956127037133*E)
    if ( is.na(H) || is.na( I)){O<-0}
    else if(H>= I){O<-1 }
    else{O<-0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 0.490897797231103*A , 0.010425297451883*B*C , D*N , 0.496609087235863*G*O , J , -5.03376316220193e-5*K*L*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AW3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rain.long[(t- 1),"Awdal_rain"]
    B<- current.long[(t- 1),"Bari_CurrentRegion"]
    C<- fatalities.long[(t- 1),"Bari_Fatalities"]
    D<- current.long[(t- 1),"Mudug_CurrentRegion"]
    E<- rain.long[(t- 3),"Awdal_rain"]
    G<- stations.long[(t- 5),"Gedo_LuuqStation_Juba_River"]
    H<- before.long[(t- 8),"Awdal_BeforeRegion"]
    I<- mean(fatalities.long[(t-3):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    J<- mean(rain.long[(t-12):(t- 1),"Mudug_rain"], na.rm=TRUE)
    K<- fatalities.long[(t- 17),"Banaadir_Fatalities"]
    L<- tail(movavg(before.long[(t-4):(t- 1),"Nugaal_BeforeRegion"], 3,type="m"),1)
    M<- rain.long[(t- 1),"Awdal_rain"]
    N<- current.long[(t- 1),"Bari_CurrentRegion"]
    O<- fatalities.long[(t- 17),"Banaadir_Fatalities"]
    P<- before.long[(t- 1),"Bari_BeforeRegion"]
    Q<- tail(movavg(before.long[(t-4):(t- 1),"Nugaal_BeforeRegion"], 3,type="m"),1)
    R<- max(G*H, I*J,na.rm=TRUE)
    S<- atan2(0.0116030921269807*M*N, O)
    U<- max(K, L*S,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( 0.0116030921269807*A*B , 0.000269758964913941*C*D*E , R , U , -0.00276011296006388*P*Q,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AW4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(water.long[(t-14):(t- 1),"Bay_WaterDrumPrice"], na.rm=TRUE)
    B<- rain.long[(t- 1),"Awdal_rain"]
    C<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    D<- rain.long[(t- 1),"Awdal_rain"]
    E<- current.long[(t- 1),"Bari_CurrentRegion"]
    G<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    H<- rain.long[(t- 11),"Bay_rain"]
    I<- fatalities.long[(t- 1),"Bari_Fatalities"]
    J<- current.long[(t- 1),"Mudug_CurrentRegion"]
    K<- fatalities.long[(t- 17),"Shabeellaha_Hoose_Fatalities"]
    L<- fatalities.long[(t- 1),"Bari_Fatalities"]
    M<- current.long[(t- 1),"Mudug_CurrentRegion"]
    N<- before.long[(t- 1),"Bari_BeforeRegion"]
    O<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    P<- ceil(1.96164289553659e-6*C*D*E)
    if ( is.na(H) ){Q<- K}
    else if(H>0){Q<- 0.00575300412890134*I*J }
    else{Q<- K }
    R<- tan(0.00575300412890134*L*M)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 0.0360217919771819*A , B*P , G , Q , -845.562773019685 , -R , -0.00298956802703255*N*O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AW5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Mudug_CurrentRegion"]
    B<- tail(movavg(water.long[(t-16):(t- 1),"Bay_WaterDrumPrice"],15,type="w"),1)
    C<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    D<- tail(movavg(current.long[(t-17):(t- 1),"Shabeellaha_Hoose_CurrentRegion"],16,type="w"),1)
    E<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    G<- tail(movavg(current.long[(t-5):(t- 1),"Shabeellaha_Hoose_CurrentRegion"], 4,type="m"),1)
    H<- fatalities.long[(t- 1),"Bari_Fatalities"]
    I<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    J<- current.long[(t- 1),"Mudug_CurrentRegion"]
    K<- max(sum(0.00440852474324138*A , 0.0346859658464852*B , 8.49641422634106e-5*C*D , 4.10503086918104e-5*E*G , 5.70475815801888e-9*H^2*I*J , -807.431727361165,na.rm=TRUE), 107.423447496697,na.rm=TRUE)
    FIN <-K
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AW6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Mudug_Conflict"]
    B<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    C<- rain.long[(t- 1),"Awdal_rain"]
    D<- current.long[(t- 1),"Bari_CurrentRegion"]
    E<- tail(movavg(fatalities.long[(t-3):(t- 1),"Togdheer_Fatalities"],2,type="w"),1)
    G<- median(fatalities.long[(t-3):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    H<- fatalities.long[(t- 1),"Bari_Fatalities"]
    I<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    J<- current.long[(t- 1),"Mudug_CurrentRegion"]
    K<- median(before.long[(t-14):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    L<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    M<- before.long[(t- 5),"Awdal_BeforeRegion"]
    N<- max(80, 0.719993766024465*L,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( A*B , 0.00822576058278348*C*D , E*G , 5.7405657254647e-9*H^2*I*J , K , N , -M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AW7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 8),"Bari_BeforeRegion"]
    B<- current.long[(t- 1),"Bari_CurrentRegion"]
    C<- median(before.long[(t-14):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    D<- before.long[(t- 8),"Bari_BeforeRegion"]
    E<- current.long[(t- 1),"Bari_CurrentRegion"]
    G<- median(before.long[(t-14):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    H<- rain.long[(t- 1),"Shabeellaha_Hoose_rain"]
    I<- rain.long[(t- 1),"Shabeellaha_Hoose_rain"]
    J<- mean(current.long[(t-9):(t- 1),"Bari_CurrentRegion"], na.rm=TRUE)
    K<- rain.long[(t- 1),"Jubbada_Dhexe_rain"]
    L<- before.long[(t- 15),"Bari_BeforeRegion"]
    M<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    N<- fatalities.long[(t- 11),"Banaadir_Fatalities"]
    O<- future.long[(t- 3),"Nugaal_FutureRegion"]
    P<- tan(sum(0.192696339163953*D , 0.00215438756331097*E*G , H,na.rm=TRUE))
    Q<- tan(J)
    R<- max(sum(0.75356049167792*M , N,na.rm=TRUE), 0.605773978281863*O,na.rm=TRUE)
    S<- max(L, R,na.rm=TRUE)
    U<- max(K, S,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(U)){U <- 0 }
    FIN <-sum( 0.192696339163953*A , 0.00215438756331097*B*C , 6.62728212788617*P , I , Q , U,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AW8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    B<- fatalities.long[(t- 16),"Banaadir_Fatalities"]
    C<- rain.long[(t- 11),"Sool_rain"]
    D<- fatalities.long[(t- 1),"Bari_Fatalities"]
    E<- current.long[(t- 1),"Mudug_CurrentRegion"]
    G<- fatalities.long[(t- 1),"Bari_Fatalities"]
    H<- fatalities.long[(t- 17),"Banaadir_Fatalities"]
    I<- rain.long[(t- 1),"Awdal_rain"]
    J<- current.long[(t- 1),"Bari_CurrentRegion"]
    K<- tail(movavg(rain.long[(t-3):(t- 1),"Gedo_rain"], 2,type="m"),1)
    L<- fatalities.long[(t- 1),"Bari_Fatalities"]
    M<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    N<- fatalities.long[(t- 16),"Banaadir_Fatalities"]
    O<- rain.long[(t- 11),"Sool_rain"]
    P<- fatalities.long[(t- 1),"Bari_Fatalities"]
    Q<- current.long[(t- 1),"Mudug_CurrentRegion"]
    R<- fatalities.long[(t- 1),"Bari_Fatalities"]
    S<- before.long[(t- 1),"Bari_BeforeRegion"]
    U<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    V<- fatalities.long[(t- 16),"Banaadir_Fatalities"]
    W<- max(1.29036479030386*A, B,na.rm=TRUE)
    if ( is.na(C) ){X<- G}
    else if(C>0){X<- 0.00576339647944009*D*E }
    else{X<- G }
    Y<- max(H,sum( 0.0116997788166426*I*J , K,na.rm=TRUE),na.rm=TRUE)
    Z<- max(1.29036479030386*M, N,na.rm=TRUE)
    if ( is.na(O) ){AA<- R}
    else if(O>0){AA<- 0.00576339647944009*P*Q }
    else{AA<- R }
    BB<- tan(sum(Z , AA,na.rm=TRUE))
    CC<- max(U, V,na.rm=TRUE)
    if(is.infinite(W)){W <- 0 }
    if(is.infinite(X)){X <- 0 }
    if(is.infinite(Y)){Y <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(C)){C <- 0 }
    FIN <-sum( W , X , Y , -L , -BB , -0.00236648976866705*S*CC,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AW9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- mean(before.long[(t-8):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    B<- tail(movavg(before.long[(t-5):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], 4,type="m"),1)
    C<- conflicts.long[(t- 14),"Shabeellaha_Dhexe_Conflict"]
    D<- fatalities.long[(t- 1),"Banaadir_Fatalities"]
    E<- fatalities.long[(t- 1),"Bari_Fatalities"]
    G<- current.long[(t- 1),"Mudug_CurrentRegion"]
    H<- mean(before.long[(t-16):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    I<- rain.long[(t- 1),"Awdal_rain"]
    J<- current.long[(t- 1),"Bari_CurrentRegion"]
    K<- fatalities.long[(t- 1),"Bari_Fatalities"]
    L<- current.long[(t- 1),"Mudug_CurrentRegion"]
    M<- median(fatalities.long[(t-3):(t- 1),"Bakool_Fatalities"], na.rm=TRUE)
    N<- current.long[(t- 1),"Mudug_CurrentRegion"]
    O<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    P<- before.long[(t- 1),"Bari_BeforeRegion"]
    if ( is.na(C) ){Q<-sum( 0.00618849989164869*E*G , H,na.rm=TRUE)}
    else if(C>0){Q<- D }
    else{Q<-sum( 0.00618849989164869*E*G , H,na.rm=TRUE) }
    R<- max(sum(0.011072178249205*I*J , 0.00618849989164869*K*L,na.rm=TRUE), M,na.rm=TRUE)
    S<- max(B,sum( Q , R,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( A , S , -0.0171025552409052*N , -0.0511138494893666*O*P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AW10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(before.long[(t-3):(t- 1),"Nugaal_BeforeRegion"], 2,type="m"),1)
    B<- rain.long[(t- 1),"Awdal_rain"]
    C<- current.long[(t- 1),"Bari_CurrentRegion"]
    D<- tail(movavg(current.long[(t-7):(t- 1),"Mudug_CurrentRegion"], 6,type="m"),1)
    E<- fatalities.long[(t- 1),"Bari_Fatalities"]
    G<- median(current.long[(t-16):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    H<- rain.long[(t- 6),"Bari_rain"]
    I<- fatalities.long[(t- 10),"Shabeellaha_Hoose_Fatalities"]
    J<- median(future.long[(t-14):(t- 1),"Hiiraan_FutureRegion"], na.rm=TRUE)
    K<- fatalities.long[(t- 1),"Bari_Fatalities"]
    L<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    M<- tail(movavg(current.long[(t-7):(t- 1),"Mudug_CurrentRegion"], 6,type="m"),1)
    N<- atan(0.00438956127037133*E)
    if ( is.na(H) || is.na( I)){O<-0}
    else if(H>= I){O<-1 }
    else{O<-0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 0.490897797231103*A , 0.010425297451883*B*C , D*N , 0.496609087235863*G*O , J , -5.03376316220193e-5*K*L*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AWJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Galgaduud_Conflict"]
    B<- conflicts.long[(t- 3),"Hiiraan_Conflict"]
    C<- water.long[(t- 1),"Bay_WaterDrumPrice"]
    D<- water.long[(t- 1),"Sool_WaterDrumPrice"]
    E<- conflicts.long[(t- 1),"Galgaduud_Conflict"]
    G<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    H<- median(conflicts.long[(t-4):(t- 1),"Sool_Conflict"], na.rm=TRUE)
    I<- median(conflicts.long[(t-10):(t- 1),"Togdheer_Conflict"], na.rm=TRUE)
    J<- fatalities.long[(t- 1),"Bari_Fatalities"]
    K<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    L<- fatalities.long[(t- 11),"Togdheer_Fatalities"]
    M<- fatalities.long[(t- 1),"Bari_Fatalities"]
    N<- mean(fatalities.long[(t-3):(t- 1),"Banaadir_Fatalities"], na.rm=TRUE)
    O<- rain.long[(t- 1),"Sool_rain"]
    if ( is.na(L) ){P<- N}
    else if(L>0){P<- M }
    else{P<- N }
    Q<- max(sum(A*B , 3.22880207206999e-7*C*D , E*G*H , -173.476669207183*I,na.rm=TRUE),sum( 81.9270040206024 , 9.15968049459402e-5*J*K*P , O,na.rm=TRUE),na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AWJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rain.long[(t- 16),"Jubbada_Hoose_rain"]
    B<- water.long[(t- 1),"Sool_WaterDrumPrice"]
    C<- water.long[(t- 1),"Bay_WaterDrumPrice"]
    D<- water.long[(t- 1),"Jubbada_Hoose_WaterDrumPrice"]
    E<- conflicts.long[(t- 1),"Galgaduud_Conflict"]
    G<- stations.long[(t- 7),"Gedo_DollowStation_Juba_River"]
    H<- conflicts.long[(t- 8),"Mudug_Conflict"]
    I<- stations.long[(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"]
    J<- fatalities.long[(t- 1),"Bari_Fatalities"]
    K<- before.long[(t- 1),"Shabeellaha_Hoose_BeforeRegion"]
    L<- fatalities.long[(t- 1),"Bari_Fatalities"]
    M<- rain.long[(t- 2),"Nugaal_rain"]
    N<- max(6.31138477285862*A, 3.71695193132771e-17*B^2*C*D,na.rm=TRUE)
    O<- exp(I)
    P<- max(L, M,na.rm=TRUE)
    Q<- max(sum(E*G*H , O,na.rm=TRUE), 7.78882495336926e-5*J*K*P,na.rm=TRUE)
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( N , Q , -76.3833791498047,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AWJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- water.long[(t- 1),"Bay_WaterDrumPrice"]
    B<- water.long[(t- 1),"Sool_WaterDrumPrice"]
    C<- median(rain.long[(t-8):(t- 1),"Awdal_rain"], na.rm=TRUE)
    D<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    E<- mean(fatalities.long[(t-16):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    G<- tail(movavg(conflicts.long[(t-3):(t- 1),"Mudug_Conflict"],2,type="w"),1)
    H<- fatalities.long[(t- 1),"Bari_Fatalities"]
    I<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    J<- fatalities.long[(t- 1),"Bari_Fatalities"]
    K<- tail(movavg(conflicts.long[(t-3):(t- 1),"Mudug_Conflict"],2,type="w"),1)
    L<- water.long[(t- 1),"Sool_WaterDrumPrice"]
    M<- median(conflicts.long[(t-10):(t- 1),"Bay_Conflict"], na.rm=TRUE)
    N<- max(393.47975961048, 2.7967018753672e-7*A*B,na.rm=TRUE)
    O<- max(J, K,na.rm=TRUE)
    P<- max(D*E*G, 7.96952932639967e-5*H*I*O,na.rm=TRUE)
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 1.40951804913966*N , C , P , -0.00570608146965771*L , -12.6999663946746*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AWJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(future.long[(t-6):(t- 1),"Mudug_FutureRegion"], na.rm=TRUE)
    B<- tail(movavg(future.long[(t-17):(t- 1),"Nugaal_FutureRegion"], 16,type="m"),1)
    C<- rain.long[(t- 1),"Hiiraan_rain"]
    D<- mean(stations.long[(t-11):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    E<- water.long[(t- 1),"Bay_WaterDrumPrice"]
    G<- water.long[(t- 1),"Sool_WaterDrumPrice"]
    H<- stations.long[(t- 15),"Gedo_LuuqStation_Juba_River"]
    I<- median(before.long[(t-14):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    J<- fatalities.long[(t- 1),"Bari_Fatalities"]
    K<- tail(movavg(before.long[(t-6):(t- 1),"Shabeellaha_Hoose_BeforeRegion"], 5,type="m"),1)
    L<- rivers.long[(t- 1),"Shabelle_River_discharge"]
    M<- median(future.long[(t-13):(t- 1),"Galgaduud_FutureRegion"], na.rm=TRUE)
    N<- max(C*D, 3.24077247614413e-17*E^2*G^2,na.rm=TRUE)
    O<- max(H*I, 7.77818297343888e-5*J^2*K,na.rm=TRUE)
    P<- max(sum(0.120926627087851*B , N , O , -L,na.rm=TRUE), M,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( A , P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AWJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    C<- fatalities.long[(t- 1),"Bari_Fatalities"]
    D<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    E<- fatalities.long[(t- 16),"Banaadir_Fatalities"]
    G<- median(future.long[(t-5):(t- 1),"Mudug_FutureRegion"], na.rm=TRUE)
    H<- rain.long[(t- 16),"Jubbada_Hoose_rain"]
    I<- rain.long[(t- 1),"Sool_rain"]
    J<- rain.long[(t- 1),"Gedo_rain"]
    K<- tail(movavg(before.long[(t-7):(t- 1),"Awdal_BeforeRegion"],6,type="w"),1)
    L<- max(sum(3.67845770676282*H , I,na.rm=TRUE), J,na.rm=TRUE)
    M<- max(A,sum( 0.0237190422945466*B , 8.48763262747337e-5*C^2*D , E , G , L,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 0.992554844515448*M , -46.1986730248398 , -1.06585156714655*K,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AWJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- stations.long[(t- 15),"Gedo_LuuqStation_Juba_River"]
    B<- rain.long[(t- 16),"Jubbada_Hoose_rain"]
    C<- fatalities.long[(t- 1),"Bari_Fatalities"]
    D<- mean(future.long[(t-7):(t- 1),"Mudug_FutureRegion"], na.rm=TRUE)
    E<- rain.long[(t- 5),"Jubbada_Dhexe_rain"]
    G<- median(current.long[(t-17):(t- 1),"Sool_CurrentRegion"], na.rm=TRUE)
    H<- rain.long[(t- 1),"Hiiraan_rain"]
    I<- fatalities.long[(t- 10),"Bari_Fatalities"]
    J<- mean(fatalities.long[(t-6):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    K<- before.long[(t- 8),"Awdal_BeforeRegion"]
    L<- stations.long[(t- 15),"Gedo_LuuqStation_Juba_River"]
    M<- before.long[(t- 1),"Bay_BeforeRegion"]
    N<- mean(fatalities.long[(t-8):(t- 1),"Hiiraan_Fatalities"], na.rm=TRUE)
    O<- max(H, I*J,na.rm=TRUE)
    P<- max(K*L, 0.0187301275782267*M,na.rm=TRUE)
    Q<- max(129.108864706647,sum( A*B , 0.000102684111890545*C^2*D , E , G , O , P , -129.108864706647,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( Q , -N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AWJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Bari_Fatalities"]
    B<- water.long[(t- 1),"Sool_WaterDrumPrice"]
    C<- tail(movavg(before.long[(t-8):(t- 1),"Shabeellaha_Hoose_BeforeRegion"], 7,type="m"),1)
    D<- rain.long[(t- 14),"Shabeellaha_Hoose_rain"]
    E<- future.long[(t- 1),"Togdheer_FutureRegion"]
    G<- fatalities.long[(t- 1),"Bari_Fatalities"]
    H<- conflicts.long[(t- 10),"Jubbada_Hoose_Conflict"]
    I<- current.long[(t- 14),"Togdheer_CurrentRegion"]
    J<- rain.long[(t- 1),"Awdal_rain"]
    K<- fatalities.long[(t- 1),"Bari_Fatalities"]
    L<- water.long[(t- 1),"Sool_WaterDrumPrice"]
    M<- tail(movavg(before.long[(t-8):(t- 1),"Shabeellaha_Hoose_BeforeRegion"], 7,type="m"),1)
    N<- current.long[(t- 14),"Togdheer_CurrentRegion"]
    O<- rain.long[(t- 1),"Awdal_rain"]
    P<- fatalities.long[(t- 1),"Bari_Fatalities"]
    Q<- fatalities.long[(t- 1),"Bari_Fatalities"]
    R<- sinh(0.00693433461640035*E)
    S<- tan(42.4099473288014*G*H)
    U<- tan(sum(0.702827245057452*I , J,na.rm=TRUE))
    V<- tan(3.00767310120673e-8*K*L*M)
    W<- max(sum(0.702827245057452*N , O,na.rm=TRUE), P,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(W)){W <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( 3.00767310120673e-8*A*B*C , D , R , S , U , V , W , -Q,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AWJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 15),"Bari_BeforeRegion"]
    B<- current.long[(t- 15),"Jubbada_Dhexe_CurrentRegion"]
    C<- median(before.long[(t-12):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    D<- before.long[(t- 15),"Sool_BeforeRegion"]
    E<- current.long[(t- 15),"Sool_CurrentRegion"]
    G<- conflicts.long[(t- 1),"Mudug_Conflict"]
    H<- fatalities.long[(t- 2),"Togdheer_Fatalities"]
    I<- current.long[(t- 15),"Jubbada_Dhexe_CurrentRegion"]
    J<- before.long[(t- 15),"Bari_BeforeRegion"]
    K<- current.long[(t- 15),"Jubbada_Dhexe_CurrentRegion"]
    L<- median(before.long[(t-12):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    M<- before.long[(t- 15),"Sool_BeforeRegion"]
    N<- current.long[(t- 15),"Sool_CurrentRegion"]
    O<- current.long[(t- 15),"Jubbada_Dhexe_CurrentRegion"]
    P<- tail(movavg(before.long[(t-9):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], 8,type="m"),1)
    Q<- fatalities.long[(t- 12),"Togdheer_Fatalities"]
    R<- future.long[(t- 1),"Togdheer_FutureRegion"]
    S<- before.long[(t- 15),"Bari_BeforeRegion"]
    U<- rain.long[(t- 5),"Bakool_rain"]
    V<- max(G*H, 0.00189690282163337*I,na.rm=TRUE)
    W<- sum(0.566054870160388*J, 0.00189690282163337*K*L ,0.000116228953606384*M*N,na.rm=TRUE)%%0.00189690282163337*O
    X<- max(0.566054870160388*S, U,na.rm=TRUE)
    if ( is.na(Q) ){Y<- X}
    else if(Q>0){Y<- 0.652241128913957*R }
    else{Y<- X }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(W)){W <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Y)){Y <- 0 }
    FIN <-sum( 0.566054870160388*A , 0.00189690282163337*B*C , 0.000116228953606384*D*E , V*W , P , Y,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AWJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Sool_BeforeRegion"]
    B<- mean(before.long[(t-10):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    C<- median(before.long[(t-12):(t- 1),"Sool_BeforeRegion"], na.rm=TRUE)
    D<- median(before.long[(t-13):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    E<- mean(before.long[(t-7):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    G<- before.long[(t- 15),"Bari_BeforeRegion"]
    H<- future.long[(t- 3),"Nugaal_FutureRegion"]
    I<- conflicts.long[(t- 1),"Mudug_Conflict"]
    J<- mean(fatalities.long[(t-3):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    K<- median(before.long[(t-16):(t- 1),"Bari_BeforeRegion"], na.rm=TRUE)
    L<- tail(movavg(before.long[(t-6):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], 5,type="m"),1)
    M<- fatalities.long[(t- 1),"Galguduud_Fatalities"]
    N<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    O<- max(0.584906959013989*H, 0.915165391007681*I*J,na.rm=TRUE)
    P<- max(sum(G , O,na.rm=TRUE), K,na.rm=TRUE)
    Q<- max(P, L,na.rm=TRUE)
    R<- max(C*D/E, Q,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 0.0115621443353209*A , 0.915165391007681*B , R , -M , -N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_AWJUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Bay_BeforeRegion"]
    B<- tail(movavg(future.long[(t-7):(t- 1),"Jubbada_Dhexe_FutureRegion"], 6,type="m"),1)
    C<- mean(current.long[(t-9):(t- 1),"Gedo_CurrentRegion"], na.rm=TRUE)
    D<- rain.long[(t- 1),"Bari_rain"]
    E<- fatalities.long[(t- 8),"Nugaal_Fatalities"]
    G<- fatalities.long[(t- 1),"Bari_Fatalities"]
    H<- current.long[(t- 1),"Mudug_CurrentRegion"]
    I<- fatalities.long[(t- 11),"Nugaal_Fatalities"]
    J<- mean(fatalities.long[(t-14):(t- 1),"Bari_Fatalities"], na.rm=TRUE)
    K<- rain.long[(t- 16),"Jubbada_Hoose_rain"]
    L<- fatalities.long[(t- 3),"Banaadir_Fatalities"]
    M<- max(I*J, 5.18603498698061*K,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 0.0214444315046024*A , 0.0877600299841897*B , 0.0496708696239565*C , D*E , 9.72016887437201e-5*G^2*H , M , -L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}