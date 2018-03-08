library(rCharts)
library(shiny)
library(datasets)
library(magrittr)
library(XML)
library(reshape)
library(gsheet)
library(plotly)
library(ggplot2)
library(scales)
library(zoo)
library(pracma)
library(psych)
library(devtools)
library(rmarkdown)
library(astro)

# Use the google spreadsheet
jetson <- "https://docs.google.com/spreadsheets/d/1oPTPmoJ9phtMOkp-nMB7WHnPESomLzqUj9t0gcE9bYA"
conflicts <- gsheet2text(jetson, sheetid = 819472314)
conflicts.long <- read.csv(text=conflicts)

before <-gsheet2text(jetson, sheetid = 457614883)
before.long <- read.csv(text=before)
#arrs.long <- head(arrs.long, -30)

future <-gsheet2text(jetson, sheetid = 677621454)
future.long <-read.csv(text=future)
#deps.long <- head(deps.long, -30)


current <-gsheet2text(jetson, sheetid = 772694153)
current.long <-read.csv(text=current)

rain <-gsheet2text(jetson, sheetid = 1473662223)
rain.long <- read.csv(text=rain,stringsAsFactors = FALSE)
#rain.long <- head(rain.long, -30)

#read the WaterDrumPrices
water <-gsheet2text(jetson, sheetid =27261871)
water.long <- read.csv(text=water,stringsAsFactors = FALSE)
water.long$Date <-as.Date(water.long$Date, format="%m/%d/%Y")
#water.long <- data.frame(cbind(sapply(water.long[,sapply(water.long,is.character)],trimws,which="both"),water.long[,!sapply(df,is.character)]))
water.long[,1:ncol(water.long)] <- sapply(water.long[,1:ncol(water.long)],as.numeric)

#read the Rivers
rivers <-gsheet2text(jetson, sheetid =407366559)
rivers.long <- read.csv(text=rivers,stringsAsFactors = FALSE)

#read the Goat Prices
goats <-gsheet2text(jetson, sheetid =1601716765)
goats.long <- read.csv(text=goats,stringsAsFactors = FALSE)

#read the Fatalities
fatalities <-gsheet2text(jetson, sheetid =343810263)
fatalities.long <- read.csv(text=fatalities,stringsAsFactors = FALSE)

discharge <- gsheet2text(jetson, sheetid=407366559)
discharge.long <- read.csv(text=discharge,stringsAsFactors = FALSE)

stations <- gsheet2text(jetson, sheetid=1052168743)
stations.long <- read.csv(text=stations,stringsAsFactors = FALSE)

cash <- gsheet2text(jetson, sheetid = 161900539)
cash.long <- read.csv(text=cash,stringsAsFactors = FALSE)

cases <- gsheet2text(jetson, sheetid = 15526228)
cases.long <- read.csv(text=cases,stringsAsFactors = FALSE)

deaths <- gsheet2text(jetson, sheetid = 2060381151)
deaths.long <- read.csv(text=deaths,stringsAsFactors = FALSE)

dollos <- gsheet2text(jetson, sheetid = 1111574539)
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

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

date_index <- function(x){
  full.date <- as.POSIXct(x, tz="GMT")
  index <- which(conflicts.long$Date== monthStart(full.date))
  return(index)
}
BAY_1arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- before.long[(t- 5),"Bari_BeforeRegion"]
    C<- rain.long[(t- 16),"Bari_rain"]
    D<- before.long[(t- 1),"Mudug_BeforeRegion"]
    E<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    G<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    H<- current.long[(t- 1),"Awdal_CurrentRegion"]
    I<- before.long[(t- 1),"Mudug_BeforeRegion"]
    J<- conflicts.long[(t- 1),"Awdal_Conflict"]
    K<- rain.long[(t- 16),"Bari_rain"]
    L<- before.long[(t- 1),"Mudug_BeforeRegion"]
    M<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    N<- fatalities.long[(t- 1),"Bari_Fatalities"]
    O<- before.long[(t- 1),"Banadir_BeforeRegion"]
    P<- before.long[(t- 1),"Mudug_BeforeRegion"]
    if ( is.na(C) ){Q<- I}
    else if(C>0){Q<-sum( 0.00216084551123691*D*E , 0.000112512408566433*G*H,na.rm=TRUE) }
    else{Q<- I }
    R<- exp(J)
    S<- tan(0.00216084551123691*L*M)
    U<- max(R*K*S, 0.00833736411871744*N*O,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( A , B , Q , U , -0.448385126774561*P,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}
BAY_A4arrivals <- function(start, end){
  start = 23

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- mean(fatalities.long[(t-9):(t- 1),"Shabeellaha_Dhexe_Fatalities"], na.rm=TRUE)
    B<- mean(current.long[(t-23):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    C<- mean(fatalities.long[(t-9):(t- 1),"Shabeellaha_Dhexe_Fatalities"], na.rm=TRUE)
    D<- cos(C)
    if ( is.na(-56.1324545145847) || is.na( D)){E<-0}
    else if(-56.1324545145847> D){E<-1 }
    else{E<-0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(E)){E <- 0 }
    FIN <-sum( A*B , E,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BAY_13arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    C<- current.long[(t- 1),"Awdal_CurrentRegion"]
    D<- rain.long[(t- 1),"Jubbada_Hoose_rain"]
    E<- median(current.long[(t-12):(t- 1),"Bari_CurrentRegion"], na.rm=TRUE)
    G<- before.long[(t- 1),"Mudug_BeforeRegion"]
    H<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    I<- mean(future.long[(t-11):(t- 1),"Nugaal_FutureRegion"], na.rm=TRUE)
    J<- before.long[(t- 15),"Sool_BeforeRegion"]
    K<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    L<- conflicts.long[(t- 1),"Awdal_Conflict"]
    M<- fatalities.long[(t- 1),"Sool_Fatalities"]
    N<- future.long[(t- 1),"Bari_FutureRegion"]
    O<- log(D)
    P<- max(A,sum( 0.000127177740817059*B*C , O*E , 2.11983998917739e-6*G*H*I , J,na.rm=TRUE),na.rm=TRUE)
    Q<- max(sum(P , -K,na.rm=TRUE), 0.805982905429227*L^2*M*N,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BA_2arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- conflicts.long[(t- 1),"Bari_Conflict"]
    B<- before.long[(t- 1),"Banadir_BeforeRegion"]
    C<- conflicts.long[(t- 1),"Bari_Conflict"]
    D<- before.long[(t- 1),"Mudug_BeforeRegion"]
    E<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    G<- before.long[(t- 1),"Bay_BeforeRegion"]
    H<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    I<- water.long[(t- 1),"Hiiraan_WaterDrumPrice"]
    J<- before.long[(t- 6),"Shabeellaha_Dhexe_BeforeRegion"]
    K<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    L<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    M<- median(rain.long[(t-15):(t- 1),"Gedo_rain"], na.rm=TRUE)
    N<- current.long[(t- 17),"Mudug_CurrentRegion"]
    O<- max(0.177793684635044*K, 1.18213777130181*L*M,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 0.0732602367252228*A*B , 0.0732602367252228*C*D , 0.0732602367252228*E*G , 8.54019598974934e-6*H*I , J , O , -N,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  #write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BA_8arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- water.long[(t- 1),"Hiiraan_WaterDrumPrice"]
    B<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    C<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    D<- median(rain.long[(t-15):(t- 1),"Gedo_rain"], na.rm=TRUE)
    E<- conflicts.long[(t- 1),"Bari_Conflict"]
    G<- before.long[(t- 1),"Banadir_BeforeRegion"]
    H<- current.long[(t- 1),"Nugaal_CurrentRegion"]
    I<- median(current.long[(t-5):(t- 1),"Nugaal_CurrentRegion"], na.rm=TRUE)
    J<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    K<- median(current.long[(t-5):(t- 1),"Nugaal_CurrentRegion"], na.rm=TRUE)
    L<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    M<- before.long[(t- 1),"Bay_BeforeRegion"]
    N<- current.long[(t- 8),"Gedo_CurrentRegion"]
    O<- min(J*K, 0.0938139964853952*L*M,na.rm=TRUE)

    if(is.infinite(O)){O <- 0}
    P<- max(sum(0.525731462646251*A , 0.164551229335035*B , C*D , 0.0772156733729328*E*G , H , I , O , -6252.41542789858,na.rm=TRUE), N,na.rm=TRUE)

    FIN <-P
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  #write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BA_9arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- before.long[(t- 1),"Mudug_BeforeRegion"]
    B<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    C<- conflicts.long[(t- 1),"Bari_Conflict"]
    D<- mean(before.long[(t-4):(t- 1),"Banadir_BeforeRegion"], na.rm=TRUE)
    E<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    G<- median(before.long[(t-6):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    H<- before.long[(t- 1),"Bay_BeforeRegion"]
    I<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    J<- median(before.long[(t-6):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    K<- mean(fatalities.long[(t-4):(t- 1),"Mudug_Fatalities"], na.rm=TRUE)
    L<- future.long[(t- 8),"Nugaal_FutureRegion"]
    M<- mean(goats.long[(t-8):(t- 1),"Gedo_goatprice"], na.rm=TRUE)
    N<- min(0.0016221206705837*I*J, K,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 11723.7642528514 , 0.00790448542053325*A*B , 0.0680140693765127*C*D , 0.0016221206705837*E*G , 0.0813222729835992*H*N , L , -0.00920377145047265*M,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  #write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BK_10arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    B<- median(before.long[(t-10):(t- 1),"Mudug_BeforeRegion"], na.rm=TRUE)
    C<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    D<- current.long[(t- 1),"Bay_CurrentRegion"]
    E<- conflicts.long[(t- 1),"Awdal_Conflict"]
    G<- fatalities.long[(t- 1),"Bari_Fatalities"]
    H<- fatalities.long[(t- 4),"Shabeellaha_Dhexe_Fatalities"]
    I<- conflicts.long[(t- 1),"Awdal_Conflict"]
    J<- fatalities.long[(t- 4),"Shabeellaha_Dhexe_Fatalities"]
    K<- mean(conflicts.long[(t-3):(t- 1),"Togdheer_Conflict"], na.rm=TRUE)
    L<- mean(before.long[(t-11):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    M<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    N<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    O<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    P<- rain.long[(t- 8),"Hiiraan_rain"]
    Q<- mean(before.long[(t-11):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    R<- max(sum(A , B,na.rm=TRUE),sum( 0.0122292790045646*C*D , E*G*H , I*J*K , -909.166655615155,na.rm=TRUE),na.rm=TRUE)
    if ( is.na(M) ){S<- O*P}
    else if(M>0){S<- N }
    else{S<- O*P }
    U<- max(sum(1.05613075307977*R , L , -S,na.rm=TRUE), Q,na.rm=TRUE)
    FIN <-U
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BK_4arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- mean(current.long[(t-5):(t- 1),"Togdheer_CurrentRegion"], na.rm=TRUE)
    B<- conflicts.long[(t- 1),"Hiiraan_Conflict"]
    C<- mean(fatalities.long[(t-17):(t- 1),"Mudug_Fatalities"], na.rm=TRUE)
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- current.long[(t- 1),"Gedo_CurrentRegion"]
    G<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    H<- fatalities.long[(t- 6),"Sool_Fatalities"]
    I<- mean(fatalities.long[(t-8):(t- 1),"Jubbada_Dhexe_Fatalities"], na.rm=TRUE)
    J<- fatalities.long[(t- 1),"Hiiraan_Fatalities"]
    K<- rain.long[(t- 8),"Awdal_rain"]
    L<- median(fatalities.long[(t-4):(t- 1),"Awdal_Fatalities"], na.rm=TRUE)
    M<- rain.long[(t- 8),"Awdal_rain"]
    N<- future.long[(t- 7),"Sanaag_FutureRegion"]
    O<- mean(fatalities.long[(t-6):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    P<- median(fatalities.long[(t-3):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    Q<- max(sum(0.311587052568519*A , B*C , 0.000518985717965901*D*E , G*H*I , J*K*L , M,na.rm=TRUE), N*O*P,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BK_JUN6arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- mean(before.long[(t-11):(t- 1),"Jubbada_Hoose_BeforeRegion"], na.rm=TRUE)
    B<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    C<- before.long[(t- 1),"Sool_BeforeRegion"]
    D<- fatalities.long[(t- 10),"Woqooyi_Galbeed_Fatalities"]
    E<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    G<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    H<- current.long[(t- 1),"Bay_CurrentRegion"]
    I<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    J<- fatalities.long[(t- 5),"Woqooyi_Galbeed_Fatalities"]
    K<- before.long[(t- 1),"Awdal_BeforeRegion"]
    L<- floor(0.24775993583984*B)
    M<- exp(J)
    N<- max(135.322051921076, 2.56826319308426*M,na.rm=TRUE)
    O<- max(sum(0.202519965668327*C*D , 0.000500013372597361*E*G*H , I , N,na.rm=TRUE), 8.04196809819944*K,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( A*L , O,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

GE_6arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- before.long[(t- 15),"Sool_BeforeRegion"]
    B<- mean(fatalities.long[(t-7):(t- 1),"Woqooyi_Galbeed_Fatalities"], na.rm=TRUE)
    C<- median(before.long[(t-15):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    D<- median(fatalities.long[(t-10):(t- 1),"Jubbada_Hoose_Fatalities"], na.rm=TRUE)
    E<- future.long[(t- 1),"Sanaag_FutureRegion"]
    G<- conflicts.long[(t- 1),"Shabeellaha_Hoose_Conflict"]
    H<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    I<- current.long[(t- 1),"Awdal_CurrentRegion"]
    J<- fatalities.long[(t- 1),"Galguduud_Fatalities"]
    K<- mean(stations.long[(t-3):(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"], na.rm=TRUE)
    L<- discharge.long[(t- 1),"Shabelle_River_discharge"]
    M<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    N<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    O<- mean(stations.long[(t-3):(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"], na.rm=TRUE)
    P<- before.long[(t- 15),"Sool_BeforeRegion"]
    Q<- mean(fatalities.long[(t-7):(t- 1),"Woqooyi_Galbeed_Fatalities"], na.rm=TRUE)
    if ( is.na(D) ){R<- 1575.10777741652*G}
    else if(D>0){R<- E }
    else{R<- 1575.10777741652*G }
    S<- atan(J)
    U<- max(98.669115644315*H, I*S*K,na.rm=TRUE)
    V<- tan(P*Q)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(V)){V <- 0 }
    FIN <-sum( A*B , C , R , U , -L , -M*N*O , -V,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}


GE_9arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- median(before.long[(t-11):(t- 1),"Bay_BeforeRegion"], na.rm=TRUE)
    B<- rain.long[(t- 1),"Awdal_rain"]
    C<- current.long[(t- 1),"Awdal_CurrentRegion"]
    D<- mean(stations.long[(t-9):(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"], na.rm=TRUE)
    E<- before.long[(t- 7),"Sanaag_BeforeRegion"]
    G<- mean(rain.long[(t-13):(t- 1),"Bari_rain"], na.rm=TRUE)
    H<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    I<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    J<- goats.long[(t- 10),"Togdheer_goatprice"]
    K<- rain.long[(t- 4),"Nugaal_rain"]
    L<- rain.long[(t- 13),"Shabeellaha_Hoose_rain"]
    M<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    N<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    O<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    P<- goats.long[(t- 10),"Togdheer_goatprice"]
    Q<- rain.long[(t- 4),"Nugaal_rain"]
    R<- rain.long[(t- 13),"Shabeellaha_Hoose_rain"]
    S<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    U<- current.long[(t- 1),"Mudug_CurrentRegion"]
    V<- mean(stations.long[(t-13):(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"], na.rm=TRUE)
    W<- median(before.long[(t-11):(t- 1),"Bay_BeforeRegion"], na.rm=TRUE)
    X<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    if ( is.na(B) ){Y<- E*G}
    else if(B>0){Y<- C*D }
    else{Y<- E*G }
    Z<- tan(J)
    if ( is.na(H) ){AA<- M}
    else if(H>0){AA<-sum( I*Z , K*L,na.rm=TRUE) }
    else{AA<- M }
    BB<- tan(P)
    if ( is.na(N) ){CC<- S}
    else if(N>0){CC<-sum( O*BB , Q*R,na.rm=TRUE) }
    else{CC<- S }
    if ( is.na(CC) ){DD<- W}
    else if(CC>0){DD<- 0.115878117014169*U*V }
    else{DD<- W }
    EE<- max(sum(A , Y , AA,na.rm=TRUE), DD,na.rm=TRUE)
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(X)){X <- 0 }
    FIN <-sum( EE , -X,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}



GE_8arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- future.long[(t- 13),"Nugaal_FutureRegion"]
    B<- mean(fatalities.long[(t-5):(t- 1),"Shabeellaha_Dhexe_Fatalities"], na.rm=TRUE)
    C<- future.long[(t- 1),"Sool_FutureRegion"]
    D<- current.long[(t- 1),"Awdal_CurrentRegion"]
    E<- conflicts.long[(t- 1),"Mudug_Conflict"]
    G<- stations.long[(t- 15),"Hiiraan_Bulo_Burti_StationShabelle_River"]
    H<- rain.long[(t- 1),"Awdal_rain"]
    I<- fatalities.long[(t- 1),"Bari_Fatalities"]
    J<- current.long[(t- 1),"Awdal_CurrentRegion"]
    K<- median(before.long[(t-12):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    L<- future.long[(t- 6),"Nugaal_FutureRegion"]
    M<- fatalities.long[(t- 5),"Woqooyi_Galbeed_Fatalities"]
    N<- current.long[(t- 5),"Sanaag_CurrentRegion"]
    O<- factorial(G)
    if ( is.na(H) || is.na( I)){P<-0}
    else if(H>= I){P<-1 }
    else{P<-0 }
    Q<- max(J, 1.88198220182139*K,na.rm=TRUE)
    R<- max(sum(0.00552424283367903*C*D , 0.897023768717364*E*O*P , Q , -L,na.rm=TRUE), M*N,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( A*B , R,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}


MJ_7Xarrivals <- function(start, end){

  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    B<- rain.long[(t- 5),"Shabeellaha_Hoose_rain"]
    C<- fatalities.long[(t- 12),"Mudug_Fatalities"]
    D<- future.long[(t- 1),"Bakool_FutureRegion"]
    E<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    G<- conflicts.long[(t- 1),"Awdal_Conflict"]
    H<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    I<- median(conflicts.long[(t-8):(t- 1),"Galgaduud_Conflict"], na.rm=TRUE)
    J<- current.long[(t- 1),"Banadir_CurrentRegion"]
    K<- mean(future.long[(t-11):(t- 1),"Gedo_FutureRegion"], na.rm=TRUE)
    L<- max(sum(0.000412517433989359*D*E , 0.0277228713192043*G*H*I,na.rm=TRUE), 571.188513377989,na.rm=TRUE)
    M<- max(1.29582415347052*B*C, L,na.rm=TRUE)
    N<- max(0.163082952983062*A, M,na.rm=TRUE)
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( N , -6.17159156717101e-5*J*K,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  return(PA)
}

MJ_2Xarrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- rain.long[(t- 1),"Sanaag_rain"]
    B<- fatalities.long[(t- 17),"Bay_Fatalities"]
    C<- conflicts.long[(t- 1),"Awdal_Conflict"]
    D<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    E<- fatalities.long[(t- 11),"Sool_Fatalities"]
    G<- rain.long[(t- 1),"Sanaag_rain"]
    H<- rain.long[(t- 10),"Mudug_rain"]
    I<- median(fatalities.long[(t-8):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    J<- rain.long[(t- 1),"Gedo_rain"]
    K<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    L<- current.long[(t- 15),"Sanaag_CurrentRegion"]
    M<- future.long[(t- 1),"Bay_FutureRegion"]
    N<- max(J, 1.66768716357206*K,na.rm=TRUE)
    O<- max(sum(A*B , 0.189067008974022*C*D , E^2 , G*H*I , N , -L , -3.55180171284025*M,na.rm=TRUE), 534,na.rm=TRUE)
    FIN <-O
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

MJ_7arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- conflicts.long[(t- 1),"Awdal_Conflict"]
    B<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    C<- future.long[(t- 1),"Bakool_FutureRegion"]
    D<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    E<- future.long[(t- 1),"Bakool_FutureRegion"]
    G<- fatalities.long[(t- 9),"Awdal_Fatalities"]
    H<- median(conflicts.long[(t-6):(t- 1),"Jubbada_Dhexe_Conflict"], na.rm=TRUE)
    I<- current.long[(t- 7),"Nugaal_CurrentRegion"]
    J<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    K<- future.long[(t- 1),"Bakool_FutureRegion"]
    L<- future.long[(t- 1),"Gedo_FutureRegion"]
    M<- future.long[(t- 1),"Mudug_FutureRegion"]
    N<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    O<- max(0.000457589022171376*K, 0.161033534245094,na.rm=TRUE)
    P<- max(1.4575350401709*I, J*O,na.rm=TRUE)
    Q<- acosh(N)
    R<- max(sum(0.161033534245094*A*B , 0.000457589022171376*C*D , E*G*H , P , -L , -M*Q,na.rm=TRUE), 534,na.rm=TRUE)
    FIN <-R
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}




LJ_9arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- future.long[(t- 1),"Togdheer_FutureRegion"]
    B<- median(current.long[(t-10):(t- 1),"Hiiraan_CurrentRegion"], na.rm=TRUE)
    C<- mean(current.long[(t-3):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], na.rm=TRUE)
    D<- fatalities.long[(t- 1),"Sool_Fatalities"]
    E<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    G<- before.long[(t- 1),"Banadir_BeforeRegion"]
    H<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    I<- rain.long[(t- 9),"Bakool_rain"]
    J<- mean(rain.long[(t-9):(t- 1),"Sool_rain"], na.rm=TRUE)
    K<- median(conflicts.long[(t-11):(t- 1),"Gedo_Conflict"], na.rm=TRUE)
    L<- median(before.long[(t-17):(t- 1),"Hiiraan_BeforeRegion"], na.rm=TRUE)
    M<- median(before.long[(t-14):(t- 1),"Shabeellaha_Dhexe_BeforeRegion"], na.rm=TRUE)
    N<- max(C,sum( 0.381252890085792*D*E , 6.07165964780916e-5*G*H,na.rm=TRUE),na.rm=TRUE)
    O<- max(N,sum( I*J , 0.869238135182971*K*L,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( A , B , O , -3.06714050467996*M,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

LJ_6arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- current.long[(t- 7),"Nugaal_CurrentRegion"]
    B<- median(conflicts.long[(t-7):(t- 1),"Togdheer_Conflict"], na.rm=TRUE)
    C<- median(before.long[(t-17):(t- 1),"Hiiraan_BeforeRegion"], na.rm=TRUE)
    D<- mean(current.long[(t-4):(t- 1),"Hiiraan_CurrentRegion"], na.rm=TRUE)
    E<- current.long[(t- 11),"Nugaal_CurrentRegion"]
    G<- fatalities.long[(t- 1),"Sool_Fatalities"]
    H<- mean(future.long[(t-4):(t- 1),"Galgaduud_FutureRegion"], na.rm=TRUE)
    I<- mean(rain.long[(t-12):(t- 1),"Bay_rain"], na.rm=TRUE)
    J<- median(rain.long[(t-10):(t- 1),"Bay_rain"], na.rm=TRUE)
    K<- conflicts.long[(t- 14),"Galgaduud_Conflict"]
    L<- mean(rain.long[(t-3):(t- 1),"Shabeellaha_Dhexe_rain"], na.rm=TRUE)
    M<- before.long[(t- 1),"Banadir_BeforeRegion"]
    N<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    O<- mean(current.long[(t-3):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], na.rm=TRUE)
    P<- log(D)
    Q<- max(0.330162338655957*G*H, I*J,na.rm=TRUE)
    R<- max(E, Q,na.rm=TRUE)
    S<- max(sum(C*P , R , -K*L,na.rm=TRUE), 6.04475718368644e-5*M*N,na.rm=TRUE)
    U<- max(A*B, S,na.rm=TRUE)
    V<- max(U, O,na.rm=TRUE)
    FIN <-V
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}
LJ_6Xarrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- mean(before.long[(t-15):(t- 1),"Nugaal_BeforeRegion"], na.rm=TRUE)
    B<- median(rain.long[(t-12):(t- 1),"Shabeellaha_Dhexe_rain"], na.rm=TRUE)
    C<- mean(before.long[(t-15):(t- 1),"Nugaal_BeforeRegion"], na.rm=TRUE)
    D<- median(rain.long[(t-12):(t- 1),"Shabeellaha_Dhexe_rain"], na.rm=TRUE)
    E<- water.long[(t- 7),"Togdheer_WaterDrumPrice"]
    G<- before.long[(t- 8),"Shabeellaha_Dhexe_BeforeRegion"]
    H<- before.long[(t- 10),"Jubbada_Hoose_BeforeRegion"]
    I<- mean(future.long[(t-13):(t- 1),"Shabeellaha_Hoose_FutureRegion"], na.rm=TRUE)
    J<- median(fatalities.long[(t-16):(t- 1),"Bari_Fatalities"], na.rm=TRUE)
    K<- before.long[(t- 12),"Bay_BeforeRegion"]
    L<- future.long[(t- 11),"Galgaduud_FutureRegion"]
    M<- mean(stations.long[(t-4):(t- 1),"Gedo_LuuqStation_Juba_River"], na.rm=TRUE)
    N<- mean(conflicts.long[(t-3):(t- 1),"Shabeellaha_Dhexe_Conflict"], na.rm=TRUE)
    O<- before.long[(t- 11),"Banadir_BeforeRegion"]
    P<- stations.long[(t- 11),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
    Q<- mean(future.long[(t-10):(t- 1),"Shabeellaha_Hoose_FutureRegion"], na.rm=TRUE)
    R<- stations.long[(t- 1),"Gedo_BardheereStation_Juba_River"]
    S<- max(sum(C*D , E , G , H , -I*J,na.rm=TRUE),sum( 1.93497351844963*K , L*M*N , -O*P,na.rm=TRUE),na.rm=TRUE)
    U<- max(S, Q,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( A*B , U/R,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

MS_6arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    B<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    C<- fatalities.long[(t- 1),"Sool_Fatalities"]
    D<- future.long[(t- 1),"Bakool_FutureRegion"]
    E<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    G<- before.long[(t- 1),"Bay_BeforeRegion"]
    H<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    I<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    J<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    K<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    L<- mean(discharge.long[(t-4):(t- 1),"Juba_River_discharge"], na.rm=TRUE)
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
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 101.965589475677*A*B , 0.0327623487454269*C*D , 0.018367498732068*E*G , 0.00381260278833393*H*I , 5.09551989524317e-5*J*K , L,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}


MS_9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- conflicts.long[(t- 1),"Woqooyi_Galbeed_Conflict"]
    B<- fatalities.long[(t- 16),"Jubbada_Hoose_Fatalities"]
    C<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    D<- median(discharge.long[(t-8):(t- 1),"Shabelle_River_discharge"], na.rm=TRUE)
    E<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    G<- current.long[(t- 1),"Bay_CurrentRegion"]
    H<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    I<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    J<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    K<- rain.long[(t- 1),"Jubbada_Dhexe_rain"]
    L<- current.long[(t- 1),"Bay_CurrentRegion"]
    M<- median(discharge.long[(t-8):(t- 1),"Shabelle_River_discharge"], na.rm=TRUE)
    N<- median(future.long[(t-9):(t- 1),"Gedo_FutureRegion"], na.rm=TRUE)
    O<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    P<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    Q<- max(sum(A*B , C*D , 0.0170904314585955*E*G , 5.23272500008191e-5*H*I , J*K/L , M , N,na.rm=TRUE), 0.00390207857714544*O*P,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  return(PA)
}

MS_minus2arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- fatalities.long[(t- 2),"Shabeellaha_Dhexe_Fatalities"]
    B<- stations.long[(t- 2),"Gedo_DollowStation_Juba_River"]
    C<- rain.long[(t- 2),"Bari_rain"]
    D<- future.long[(t- 6),"Nugaal_FutureRegion"]
    E<- conflicts.long[(t- 2),"Bay_Conflict"]
    G<- fatalities.long[(t- 2),"Nugaal_Fatalities"]
    H<- discharge.long[(t- 12),"Shabelle_River_discharge"]
    I<- before.long[(t- 2),"Woqooyi_Galbeed_BeforeRegion"]
    J<- future.long[(t- 2),"Togdheer_FutureRegion"]
    K<- median(conflicts.long[(t-8):(t- 2),"Bakool_Conflict"], na.rm=TRUE)
    L<- future.long[(t- 8),"Bay_FutureRegion"]
    M<- median(fatalities.long[(t-9):(t- 2),"Nugaal_Fatalities"], na.rm=TRUE)
    N<- fatalities.long[(t- 2),"Shabeellaha_Dhexe_Fatalities"]
    O<- future.long[(t- 2),"Togdheer_FutureRegion"]
    P<- median(conflicts.long[(t-8):(t- 2),"Bakool_Conflict"], na.rm=TRUE)
    Q<- future.long[(t- 8),"Bay_FutureRegion"]
    R<- median(fatalities.long[(t-9):(t- 2),"Nugaal_Fatalities"], na.rm=TRUE)
    S<- max(D,sum( E^2 , G*H,na.rm=TRUE),na.rm=TRUE)
    U<- max(A*B*C, S,na.rm=TRUE)
    V<- max(515.381307465637, U,na.rm=TRUE)
    W<- max(J*K, 1.03689021684028*L*M,na.rm=TRUE)
    X<- max(I, W,na.rm=TRUE)
    Y<- max(O*P, 1.03689021684028*Q*R,na.rm=TRUE)
    if ( is.na(N) ){Z<- Y}
    else if(N>0){Z<- 515.381307465637 }
    else{Z<- Y }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(X)){X <- 0 }
    if(is.infinite(Z)){Z <- 0 }
    FIN <-sum( 1.09228087792705*V , X , -Z,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

LS_JUN10arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    B<- rain.long[(t- 1),"Togdheer_rain"]
    C<- conflicts.long[(t- 10),"Nugaal_Conflict"]
    D<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    E<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    G<- future.long[(t- 12),"Sanaag_FutureRegion"]
    H<- before.long[(t- 1),"Bay_BeforeRegion"]
    I<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    J<- mean(current.long[(t-8):(t- 1),"Nugaal_CurrentRegion"], na.rm=TRUE)
    K<- future.long[(t- 12),"Sanaag_FutureRegion"]
    L<- before.long[(t- 12),"Sool_BeforeRegion"]
    M<- mean(fatalities.long[(t-17):(t- 1),"Shabeellaha_Dhexe_Fatalities"], na.rm=TRUE)
    N<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    O<- max(sum(0.281967386533701*D , E*G , 0.0142740689486181*H*I , J , -K,na.rm=TRUE), 0.789348598567604*L*M,na.rm=TRUE)
    P<- max(A*B*C, O,na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( P , -0.237387364177301*N,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

LS_SEPT10arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- water.long[(t- 1),"Togdheer_WaterDrumPrice"]
    B<- mean(before.long[(t-17):(t- 1),"Banadir_BeforeRegion"], na.rm=TRUE)
    C<- rain.long[(t- 2),"Awdal_rain"]
    D<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    E<- before.long[(t- 3),"Bari_BeforeRegion"]
    G<- mean(conflicts.long[(t-15):(t- 1),"Bari_Conflict"], na.rm=TRUE)
    H<- conflicts.long[(t- 13),"Banadir_Conflict"]
    I<- mean(rain.long[(t-4):(t- 1),"Woqooyi_Galbeed_rain"], na.rm=TRUE)
    J<- before.long[(t- 1),"Bari_BeforeRegion"]
    K<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    L<- mean(before.long[(t-9):(t- 1),"Bakool_BeforeRegion"], na.rm=TRUE)
    M<- stations.long[(t- 5),"Gedo_LuuqStation_Juba_River"]
    N<- mean(fatalities.long[(t-8):(t- 1),"Hiiraan_Fatalities"], na.rm=TRUE)
    O<- cos(A)
    P<- sqrt(K)
    Q<- max(sum(O*B , C*D , E*G , H*I , J*P , L , -4206.10698936085,na.rm=TRUE), 0.00220924875524871*M^8.68404336686729*N,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

LS_1arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- current.long[(t- 1),"Bay_CurrentRegion"]
    B<- stations.long[(t- 17),"Gedo_DollowStation_Juba_River"]
    C<- mean(before.long[(t-7):(t- 1),"Bakool_BeforeRegion"], na.rm=TRUE)
    D<- rain.long[(t- 1),"Awdal_rain"]
    E<- future.long[(t- 1),"Nugaal_FutureRegion"]
    G<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    H<- future.long[(t- 11),"Gedo_FutureRegion"]
    I<- mean(fatalities.long[(t-4):(t- 1),"Jubbada_Hoose_Fatalities"], na.rm=TRUE)
    J<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    K<- fatalities.long[(t- 5),"Galguduud_Fatalities"]
    L<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    M<- mean(stations.long[(t-4):(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"], na.rm=TRUE)
    N<- current.long[(t- 1),"Bay_CurrentRegion"]
    O<- stations.long[(t- 17),"Gedo_DollowStation_Juba_River"]
    P<- mean(before.long[(t-7):(t- 1),"Bakool_BeforeRegion"], na.rm=TRUE)
    Q<- rain.long[(t- 1),"Awdal_rain"]
    R<- future.long[(t- 1),"Nugaal_FutureRegion"]
    S<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    U<- future.long[(t- 11),"Gedo_FutureRegion"]
    V<- mean(fatalities.long[(t-4):(t- 1),"Jubbada_Hoose_Fatalities"], na.rm=TRUE)
    W<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    X<- before.long[(t- 1),"Sool_BeforeRegion"]
    Y<- current.long[(t- 1),"Awdal_CurrentRegion"]
    Z<- median(stations.long[(t-13):(t- 1),"Gedo_DollowStation_Juba_River"], na.rm=TRUE)
    AA<- atan2(D, E)
    if ( is.na(I) || is.na( J)){BB<-0}
    else if(I< J){BB<-1 }
    else{BB<-0 }
    CC<- atan2(Q, R)
    if ( is.na(V) || is.na( W)){DD<-0}
    else if(V< W){DD<-1 }
    else{DD<-0 }
    if ( is.na(K) ){EE<-sum( 0.416259660225839*N , O*P*CC , 0.824421650767792*S*U*DD,na.rm=TRUE)}
    else if(K>0){EE<- L*M }
    else{EE<-sum( 0.416259660225839*N , O*P*CC , 0.824421650767792*S*U*DD,na.rm=TRUE) }
    GF<- max(sum(0.416259660225839*A , B*C*AA , 0.824421650767792*G*H*BB , EE , -X,na.rm=TRUE), Y*Z,na.rm=TRUE)
    FIN <-GF
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

HI_6arrivals <- function(start, end){
      start = 20

      PI <- PA <- PD <- rep(NA, end)
      for (t in start:end){

A<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
B<- conflicts.long[(t- 12),"Mudug_Conflict"]
C<- median(fatalities.long[(t-16):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
D<- conflicts.long[(t- 1),"Awdal_Conflict"]
E<- fatalities.long[(t- 4),"Bakool_Fatalities"]
G<- fatalities.long[(t- 5),"Woqooyi_Galbeed_Fatalities"]
H<- fatalities.long[(t- 5),"Woqooyi_Galbeed_Fatalities"]
I<- future.long[(t- 1),"Nugaal_FutureRegion"]
J<- fatalities.long[(t- 2),"Bakool_Fatalities"]
K<- conflicts.long[(t- 12),"Mudug_Conflict"]
L<- conflicts.long[(t- 1),"Awdal_Conflict"]
M<- conflicts.long[(t- 1),"Awdal_Conflict"]
N<- before.long[(t- 15),"Galgaduud_BeforeRegion"]
O<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
P<- before.long[(t- 15),"Galgaduud_BeforeRegion"]
Q<- log(A)
R<- factorial(H)
if ( is.na(G) ){S<- 1.89162025484638*I}
else if(G>0){S<- 1.3464725543557*R }
else{S<- 1.89162025484638*I }
U<- sqrt(M)
if ( is.na(L) ){V<- O}
else if(L>0){V<- U*N }
else{V<- O }
W<- max(J*K, V,na.rm=TRUE)
if ( is.na(C) ){X<- 595.642857142857}
else if(C>0){X<-sum( 4.3683717395262*D*E , S , W,na.rm=TRUE) }
else{X<- 595.642857142857 }
if ( is.na(B) ){Y<- P}
else if(B>0){Y<- X }
else{Y<- P }
if ( is.na(Q) ){Z<- 8241}
else if(Q>0){Z<- Y }
else{Z<- 8241 }
FIN <-Z
    PA[t] <- FIN
        #Bay_Incidents
        PI[t] <- 0
        #Bay_Departures
        PD[t] <- 0
        }
        write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
        return(PA)
    }
HI_8arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- current.long[(t- 2),"Awdal_CurrentRegion"]
    B<- current.long[(t- 2),"Awdal_CurrentRegion"]
    C<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    D<- current.long[(t- 8),"Sool_CurrentRegion"]
    E<- before.long[(t- 3),"Sool_BeforeRegion"]
    G<- mean(rain.long[(t-6):(t- 1),"Togdheer_rain"], na.rm=TRUE)
    H<- mean(rain.long[(t-6):(t- 1),"Togdheer_rain"], na.rm=TRUE)
    I<- future.long[(t- 1),"Bari_FutureRegion"]
    J<- conflicts.long[(t- 1),"Awdal_Conflict"]
    K<- before.long[(t- 7),"Bay_BeforeRegion"]
    L<- median(future.long[(t-3):(t- 1),"Shabeellaha_Hoose_FutureRegion"], na.rm=TRUE)
    M<- before.long[(t- 1),"Bay_BeforeRegion"]
    N<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    O<- log(G)
    P<- max(2.68821438163323*A,sum( 2.68821438163323*B , C*D , E*O , H*I*J , K , L , -M*N,na.rm=TRUE),na.rm=TRUE)
    Q<- max(5955.19443819961, P,na.rm=TRUE)
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( 1.02200933194717*Q , -5490.62143160097,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

HI_JUN2arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- mean(future.long[(t-5):(t- 1),"Togdheer_FutureRegion"], na.rm=TRUE)
    B<- before.long[(t- 15),"Galgaduud_BeforeRegion"]
    C<- before.long[(t- 7),"Galgaduud_BeforeRegion"]
    D<- mean(before.long[(t-10):(t- 1),"Galgaduud_BeforeRegion"], na.rm=TRUE)
    E<- future.long[(t- 1),"Nugaal_FutureRegion"]
    G<- before.long[(t- 7),"Sanaag_BeforeRegion"]
    H<- rain.long[(t- 11),"Togdheer_rain"]
    I<- conflicts.long[(t- 11),"Hiiraan_Conflict"]
    J<- before.long[(t- 12),"Sanaag_BeforeRegion"]
    K<- mean(stations.long[(t-5):(t- 1),"Gedo_LuuqStation_Juba_River"], na.rm=TRUE)
    L<- before.long[(t- 6),"Sanaag_BeforeRegion"]
    M<- sin(C)
    N<- max(sum(2.4888475403852*E , G*H,na.rm=TRUE), I*J*K,na.rm=TRUE)
    O<- max(sum(1.39992189518051*B*M , -D,na.rm=TRUE),sum( N , -7366.50150925209,na.rm=TRUE),na.rm=TRUE)
    P<- max(A, O,na.rm=TRUE)
    Q<- max(639, P,na.rm=TRUE)
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 1.17782782750182*Q , -L,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

GA_2arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- future.long[(t- 12),"Bay_FutureRegion"]
    B<- before.long[(t- 1),"Sool_BeforeRegion"]
    C<- current.long[(t- 1),"Gedo_CurrentRegion"]
    D<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    E<- conflicts.long[(t- 1),"Awdal_Conflict"]
    G<- mean(future.long[(t-4):(t- 1),"Bakool_FutureRegion"], na.rm=TRUE)
    H<- future.long[(t- 7),"Bakool_FutureRegion"]
    I<- future.long[(t- 16),"Bakool_FutureRegion"]
    J<- mean(fatalities.long[(t-10):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    K<- median(fatalities.long[(t-3):(t- 1),"Awdal_Fatalities"], na.rm=TRUE)
    L<- median(before.long[(t-8):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    M<- exp(E)
    N<- max(1.82892889547117*H, I*J*K,na.rm=TRUE)
    O<- max(sum(2317.07184236127 , 0.00016716681822229*B*C , -D,na.rm=TRUE),sum( M , G , N,na.rm=TRUE),na.rm=TRUE)
    P<- max(A, O,na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 1.08924461338692*P , L , -981.239524000519,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}


GA_4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- before.long[(t- 1),"Sool_BeforeRegion"]
    B<- current.long[(t- 1),"Gedo_CurrentRegion"]
    C<- before.long[(t- 1),"Sool_BeforeRegion"]
    D<- future.long[(t- 12),"Hiiraan_FutureRegion"]
    E<- before.long[(t- 12),"Hiiraan_BeforeRegion"]
    G<- future.long[(t- 12),"Hiiraan_FutureRegion"]
    H<- before.long[(t- 6),"Hiiraan_BeforeRegion"]
    I<- future.long[(t- 16),"Bakool_FutureRegion"]
    J<- future.long[(t- 17),"Jubbada_Dhexe_FutureRegion"]
    K<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    L<- max(1930.3259990639,sum( 0.0592329444425768*H , I , J,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 0.000175797283886105*A*B , 0.000240514376096206*C*D , 1.14963789346095e-5*E*G , 2.81433172993584*L , -3845.93659046548 , -K,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

GA_JUN3arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- future.long[(t- 12),"Togdheer_FutureRegion"]
    B<- before.long[(t- 12),"Galgaduud_BeforeRegion"]
    C<- before.long[(t- 9),"Galgaduud_BeforeRegion"]
    D<- future.long[(t- 12),"Togdheer_FutureRegion"]
    E<- before.long[(t- 1),"Sool_BeforeRegion"]
    G<- before.long[(t- 1),"Sool_BeforeRegion"]
    H<- future.long[(t- 17),"Galgaduud_FutureRegion"]
    I<- median(future.long[(t-3):(t- 1),"Bari_FutureRegion"], na.rm=TRUE)
    J<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    K<- future.long[(t- 12),"Bay_FutureRegion"]
    L<- future.long[(t- 12),"Togdheer_FutureRegion"]
    M<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    N<- future.long[(t- 12),"Togdheer_FutureRegion"]
    O<- before.long[(t- 1),"Sool_BeforeRegion"]
    P<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    Q<- sin(sum(15.8395664737304*D , E,na.rm=TRUE))
    R<- atan2(L, M)
    S<- sin(sum(15.8395664737304*N , O,na.rm=TRUE))
    U<- max(sum(1476.90041915523 , 1.70233005249552*J,na.rm=TRUE), K*R*S,na.rm=TRUE)
    V<- max(sum(15.8395664737304*A , 0.176158485141869*B , C*Q , G , H , I , -15.8395664737304,na.rm=TRUE), U,na.rm=TRUE)
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( V , -1.93797053396671*P,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}


MU_4arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- fatalities.long[(t- 9),"Banaadir_Fatalities"]
    B<- before.long[(t- 1),"Mudug_BeforeRegion"]
    C<- before.long[(t- 1),"Mudug_BeforeRegion"]
    D<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    E<- fatalities.long[(t- 7),"Hiiraan_Fatalities"]
    G<- median(conflicts.long[(t-4):(t- 1),"Togdheer_Conflict"], na.rm=TRUE)
    H<- median(before.long[(t-10):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    I<- future.long[(t- 1),"Mudug_FutureRegion"]
    J<- goats.long[(t- 1),"Jubbada_Dhexe_goatprice"]
    K<- conflicts.long[(t- 8),"Gedo_Conflict"]
    L<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    M<- before.long[(t- 1),"Sool_BeforeRegion"]
    if ( is.na(K) ){N<- 3566}
    else if(K>0){N<- 0.018337678491588*L^2*M }
    else{N<- 3566 }
    O<- max(1.12027672959086e-6*I*J, N,na.rm=TRUE)
    P<- max(sum(0.0903165409865906*B , 0.001762031889232*C*D , E*G , H,na.rm=TRUE), O,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( A , P,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}
MU_JUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA,end)
  for (t in start:end){

    A<- median(current.long[(t-8):(t- 1),"Sool_CurrentRegion"], na.rm=TRUE)
    B<- future.long[(t- 1),"Mudug_FutureRegion"]
    C<- before.long[(t- 1),"Mudug_BeforeRegion"]
    D<- conflicts.long[(t- 1),"Mudug_Conflict"]
    E<- fatalities.long[(t- 4),"Banaadir_Fatalities"]
    G<- mean(conflicts.long[(t-9):(t- 1),"Togdheer_Conflict"], na.rm=TRUE)
    H<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    I<- before.long[(t- 1),"Mudug_BeforeRegion"]
    J<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    K<- future.long[(t- 1),"Togdheer_FutureRegion"]
    L<- future.long[(t- 2),"Shabeallaha_Dhexe_FutureRegion"]
    M<- future.long[(t- 1),"Nugaal_FutureRegion"]
    N<- max(I, J,na.rm=TRUE)
    O<- max(A,sum( 1.06003887447658*B , 0.128078633743735*C , 1.3155059188983^D , E*G , 0.00160755003879662*H*N , -K , -L,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( O , -3.49523822022665*M,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}
MU_JUN10arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- stations.long[(t- 5),"Gedo_DollowStation_Juba_River"]
    B<- median(before.long[(t-17):(t- 1),"Galgaduud_BeforeRegion"], na.rm=TRUE)
    C<- fatalities.long[(t- 9),"Banaadir_Fatalities"]
    D<- before.long[(t- 1),"Mudug_BeforeRegion"]
    E<- future.long[(t- 1),"Mudug_FutureRegion"]
    G<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    H<- before.long[(t- 1),"Mudug_BeforeRegion"]
    I<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    J<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    K<- before.long[(t- 1),"Sool_BeforeRegion"]
    L<- future.long[(t- 1),"Mudug_FutureRegion"]
    M<- current.long[(t- 1),"Sool_CurrentRegion"]
    N<- max(0.0800779194446178*D, 0.00271950669257233*E*G,na.rm=TRUE)
    O<- max(0.00166758144357052*H*I, 0.0171409057854938*J^2*K,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( A*B , C , N , O , -0.000323983700711987*L*M,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

NU_JUN7arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- mean(rain.long[(t-5):(t- 1),"Gedo_rain"], na.rm=TRUE)
    B<- median(stations.long[(t-6):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    C<- median(before.long[(t-5):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    D<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    E<- future.long[(t- 1),"Mudug_FutureRegion"]
    G<- before.long[(t- 8),"Woqooyi_Galbeed_BeforeRegion"]
    H<- before.long[(t- 1),"Bay_BeforeRegion"]
    I<- stations.long[(t- 1),"Gedo_LuuqStation_Juba_River"]
    J<- mean(future.long[(t-10):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    K<- future.long[(t- 1),"Bari_FutureRegion"]
    L<- max(G, 0.0131153680150159*H*I,na.rm=TRUE)
    M<- max(216, L,na.rm=TRUE)
    N<- max(0.000727220998332296*D*E, M,na.rm=TRUE)
    O<- max(sum(A*B , C,na.rm=TRUE), N,na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 85.6414333977509 , O , -2.4689650389585*J , -0.000121000531063962*K^2,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}
NU_8arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- current.long[(t- 1),"Banadir_CurrentRegion"]
    B<- before.long[(t- 1),"Gedo_BeforeRegion"]
    C<- current.long[(t- 1),"Gedo_CurrentRegion"]
    D<- before.long[(t- 1),"Shabeellaha_Hoose_BeforeRegion"]
    E<- water.long[(t- 1),"Nugaal_WaterDrumPrice"]
    G<- median(fatalities.long[(t-13):(t- 1),"Bay_Fatalities"], na.rm=TRUE)
    H<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    I<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    J<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    K<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    L<- mean(future.long[(t-8):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    M<- tan(0.118522172544475*A)
    if ( is.na(D) || is.na( E)){N<-0}
    else if(D>= E){N<-1 }
    else{N<-0 }
    if ( is.na(G) ){O<- J*K}
    else if(G>0){O<- 0.0491956295061396*H*I }
    else{O<- J*K }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 297.085390188805 , 1.95868807008756*M , 1.33427680933494e-5*B*C*N , O , -2.33438018387101*L,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}
NU_4arrivals <- function(start, end){
  start = 20

  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    C<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    D<- before.long[(t- 1),"Bay_BeforeRegion"]
    E<- before.long[(t- 1),"Gedo_BeforeRegion"]
    G<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    H<- current.long[(t- 1),"Bari_CurrentRegion"]
    I<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    J<- before.long[(t- 1),"Bay_BeforeRegion"]
    K<- before.long[(t- 1),"Gedo_BeforeRegion"]
    L<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    M<- current.long[(t- 1),"Bari_CurrentRegion"]
    N<- mean(goats.long[(t-4):(t- 1),"Gedo_goatprice"], na.rm=TRUE)
    if ( is.na(608) || is.na( H)){O<-0}
    else if(608< H){O<-1 }
    else{O<-0 }
    if ( is.na(608) || is.na( M)){P<-0}
    else if(608< M){P<-1 }
    else{P<-0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 529.620675029411 , 9.00613965631703e-8*A*B , 0.00564725125358525*C^2*D , 5.09575779007251e-5*E*G*O , 9.00613965631703e-8*I*J*K*L*P , -0.000400998631124698*N,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BR_JUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    B<- mean(conflicts.long[(t-6):(t- 1),"Sanaag_Conflict"], na.rm=TRUE)
    C<- median(before.long[(t-17):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    D<- conflicts.long[(t- 1),"Sool_Conflict"]
    E<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    G<- rain.long[(t- 8),"Banaadir_rain"]
    H<- conflicts.long[(t- 12),"Hiiraan_Conflict"]
    I<- rain.long[(t- 1),"Banaadir_rain"]
    J<- before.long[(t- 1),"Bari_BeforeRegion"]
    K<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    L<- conflicts.long[(t- 1),"Sool_Conflict"]
    M<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    N<- rain.long[(t- 8),"Banaadir_rain"]
    O<- mean(goats.long[(t-16):(t- 1),"Banadir_goatprice"], na.rm=TRUE)
    P<- cosh(0.0522360723054656*I)
    Q<- max(P, 0.000421726087438536*J*K,na.rm=TRUE)
    R<- max(844.811325514564,sum( D*E*G*H , Q , -L*M*N,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 0.116676975390961*A , B*C , R , -0.000421726087438536*O,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  #write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BR_JUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- future.long[(t- 6),"Awdal_FutureRegion"]
    B<- fatalities.long[(t- 7),"Hiiraan_Fatalities"]
    C<- median(current.long[(t-14):(t- 1),"Sool_CurrentRegion"], na.rm=TRUE)
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    G<- future.long[(t- 6),"Awdal_FutureRegion"]
    H<- future.long[(t- 7),"Awdal_FutureRegion"]
    I<- current.long[(t- 15),"Togdheer_CurrentRegion"]
    J<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    K<- future.long[(t- 16),"Hiiraan_FutureRegion"]
    L<- fatalities.long[(t- 1),"Bakool_Fatalities"]
    M<- future.long[(t- 16),"Woqooyi_Galbeed_FutureRegion"]
    N<- max(0.137868678483108*J, 0.105172231430273*K,na.rm=TRUE)
    O<- max(C,sum( 0.000401810358500709*D*E , 2.05616087011001*G*H , I , N , -735.014286374837,na.rm=TRUE),na.rm=TRUE)
    P<- max(2.05616087011001*A,sum( B , O , -L , -M,na.rm=TRUE),na.rm=TRUE)
    FIN <-P
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  #write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BR_1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){

    A<- future.long[(t- 1),"Mudug_FutureRegion"]
    B<- before.long[(t- 1),"Mudug_BeforeRegion"]
    C<- future.long[(t- 1),"Bari_FutureRegion"]
    D<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    E<- fatalities.long[(t- 5),"Mudug_Fatalities"]
    G<- mean(before.long[(t-17):(t- 1),"Bari_BeforeRegion"], na.rm=TRUE)
    H<- future.long[(t- 3),"Nugaal_FutureRegion"]
    I<- stations.long[(t- 4),"Hiiraan_Bulo_Burti_StationShabelle_River"]
    J<- rain.long[(t- 14),"Jubbada_Hoose_rain"]
    K<- rain.long[(t- 1),"Bakool_rain"]
    L<- rain.long[(t- 1),"Banaadir_rain"]
    if(is.na(A)){M<-0}
    else{M<- gauss(A)}

    N<- max(0.146017438448461*H, I*J,na.rm=TRUE)
    if(is.infinite(N)){N <- 0}
    O<- max(sum(16491*M , 0.000319050428114188*B*C , 0.0254806108383852*D*E , G , N ,na.rm=TRUE), 0.161874505917949*K*L,na.rm=TRUE)
    FIN <-O
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  #write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

#Sanaag
SA_1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Bari_Conflict"]
    B<- future.long[(t- 1),"Gedo_FutureRegion"]
    C<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- before.long[(t- 1),"Sool_BeforeRegion"]
    G<- before.long[(t- 1),"Sool_BeforeRegion"]
    H<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    I<- before.long[(t- 1),"Bari_BeforeRegion"]
    J<- before.long[(t- 1),"Shabeellaha_Hoose_BeforeRegion"]
    K<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    L<- log(B)
    M<- xor(A, L)
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 134.951645053883 , 3472.70286600382*M , 2.21150357444172e-11*C^2*D*E , -6.2652996089654e-5*G*H , -6.578675626779e-9*I*J*K,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}
SA_8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    B<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    C<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    D<- median(future.long[(t-9):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    E<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    G<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    H<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    I<- median(future.long[(t-9):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    J<- median(future.long[(t-4):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    K<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    L<- before.long[(t- 1),"Bari_BeforeRegion"]
    M<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    N<- median(future.long[(t-4):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    O<- fatalities.long[(t- 1),"Bakool_Fatalities"]
    P<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    Q<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    if ( is.na(C) ){R<- 0.171238866197208}
    else if(C>0){R<- 0.81218442210766 }
    else{R<- 0.171238866197208 }
    S<- not(E)
    if ( is.na(H) ){U<- 0.171238866197208}
    else if(H>0){U<- 0.81218442210766 }
    else{U<- 0.171238866197208 }
    if ( is.na(S) ){V<- J}
    else if(S>0){V<-sum( G*U , I,na.rm=TRUE) }
    else{V<- J }
    if ( is.na(A) ){W<- V}
    else if(A>0){W<-sum( B*R , D,na.rm=TRUE) }
    else{W<- V }
    X<- max(126, W,na.rm=TRUE)
    if ( is.na(126) || is.na( M)){Y<-0}
    else if(126< M){Y<-1 }
    else{Y<-0 }
    Z<- floor(9.94678219930287e-8*K^2*L*Y)
    AA<- max(X,sum( Z , -N , -O*P , -39.2499946172489*Q,na.rm=TRUE),na.rm=TRUE)
    FIN <-AA
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

SA_4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    B<- median(before.long[(t-3):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    C<- fatalities.long[(t- 2),"Bari_Fatalities"]
    D<- future.long[(t- 16),"Sool_FutureRegion"]
    E<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    G<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    H<- before.long[(t- 1),"Bari_BeforeRegion"]
    I<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    J<- median(current.long[(t-7):(t- 1),"Bakool_CurrentRegion"], na.rm=TRUE)
    K<- max(138.304084670738,sum( 0.700519661357139*D , -21.7650791173914,na.rm=TRUE),na.rm=TRUE)
    L<- max(C, K,na.rm=TRUE)
    if ( is.na(E) || is.na( 2851.44043032364)){M<-0}
    else if(E>= 2851.44043032364){M<-1 }
    else{M<-0 }
    if ( is.na(I) || is.na( J)){N<-0}
    else if(I> J){N<-1 }
    else{N<-0 }
    O<- max(3648*M, 9.50025012902851e-8*G^2*H*N,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( A/B , 21.7650791173914*L , O , -2884.30155792815,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

SO_JUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- current.long[(t- 1),"Awdal_CurrentRegion"]
    C<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    D<- future.long[(t- 1),"Togdheer_FutureRegion"]
    E<- fatalities.long[(t- 3),"Nugaal_Fatalities"]
    G<- median(before.long[(t-4):(t- 1),"Nugaal_BeforeRegion"], na.rm=TRUE)
    H<- fatalities.long[(t- 8),"Jubbada_Dhexe_Fatalities"]
    I<- fatalities.long[(t- 1),"Sool_Fatalities"]
    J<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    K<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    L<- future.long[(t- 1),"Sool_FutureRegion"]
    M<- current.long[(t- 1),"Awdal_CurrentRegion"]
    N<- fatalities.long[(t- 1),"Banaadir_Fatalities"]
    O<- max(D, E*G,na.rm=TRUE)
    P<- max(0.0641631948417198*K, 0.00367258077969846*L*M,na.rm=TRUE)
    Q<- max(0.221607114629527*I*J, P,na.rm=TRUE)
    if ( is.na(H) ){R<- N}
    else if(H>0){R<- Q }
    else{R<- N }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( 1.6174195213491e-6*A*B*C , O , R,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

SO_2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Bakool_Fatalities"]
    B<- fatalities.long[(t- 3),"Nugaal_Fatalities"]
    C<- median(before.long[(t-17):(t- 1),"Mudug_BeforeRegion"], na.rm=TRUE)
    D<- future.long[(t- 1),"Sool_FutureRegion"]
    E<- stations.long[(t- 1),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
    G<- before.long[(t- 1),"Gedo_BeforeRegion"]
    H<- before.long[(t- 1),"Sool_BeforeRegion"]
    I<- before.long[(t- 1),"Bari_BeforeRegion"]
    J<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    K<- median(current.long[(t-4):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    L<- fatalities.long[(t- 1),"Sool_Fatalities"]
    M<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    N<- before.long[(t- 1),"Gedo_BeforeRegion"]
    O<- future.long[(t- 1),"Sool_FutureRegion"]
    P<- tail(movavg(future.long[(t-8):(t- 1),"Jubbada_Dhexe_FutureRegion"], 7,type="m"),1)
    Q<- sqrt(8.89036289987436e-5*N)
    R<- max(sum(A*B , C,na.rm=TRUE),sum( D*E , 8.89036289987436e-5*G*H , 1.61568227889806e-6*I*J*K , L*M*Q , -O , -P,na.rm=TRUE),na.rm=TRUE)
    FIN <-R
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}


SO_4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 6),"Bari_Conflict"]
    B<- median(current.long[(t-6):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    C<- future.long[(t- 5),"Nugaal_FutureRegion"]
    D<- median(current.long[(t-6):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    E<- current.long[(t- 10),"Sanaag_CurrentRegion"]
    G<- future.long[(t- 9),"Nugaal_FutureRegion"]
    H<- future.long[(t- 4),"Nugaal_FutureRegion"]
    I<- future.long[(t- 1),"Sool_FutureRegion"]
    J<- rain.long[(t- 11),"Jubbada_Dhexe_rain"]
    K<- fatalities.long[(t- 17),"Jubbada_Dhexe_Fatalities"]
    L<- median(current.long[(t-6):(t- 1),"Nugaal_CurrentRegion"], na.rm=TRUE)
    M<- max(sum(0.573289131551979*G , 0.221375027451754*H , I,na.rm=TRUE), J*K,na.rm=TRUE)
    N<- max(sum(0.0569509599165547*A*B , 0.000393439391298001*C*D , E , M , -61.2877570778266 , -L,na.rm=TRUE), 155,na.rm=TRUE)
    FIN <-N
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

TO_JUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 1),"Sanaag_FutureRegion"]
    B<- tail(movavg(current.long[(t-4):(t- 1),"Bay_CurrentRegion"],3,type="w"),1)
    C<- before.long[(t- 1),"Bay_BeforeRegion"]
    D<- tail(movavg(current.long[(t-7):(t- 1),"Mudug_CurrentRegion"],6,type="w"),1)
    E<- before.long[(t- 1),"Mudug_BeforeRegion"]
    G<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    H<- stations.long[(t- 1),"Juba_Dhexe_BualleStation_Juba_River"]
    I<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    J<- fatalities.long[(t- 2),"Mudug_Fatalities"]
    K<- before.long[(t- 1),"Mudug_BeforeRegion"]
    L<- sin(I)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 85.7374887538639 , 0.000100160323946819*A*B , 1.07532610675777e-5*C*D , 1.64860341381927e-8*E^2*G , 0.838663960829489*H*L*J , -0.0606671326019134*K,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}
TO_JUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Mudug_BeforeRegion"]
    B<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    C<- mean(current.long[(t-6):(t- 1),"Sanaag_CurrentRegion"], na.rm=TRUE)
    D<- before.long[(t- 1),"Mudug_BeforeRegion"]
    E<- median(rain.long[(t-6):(t- 1),"Shabeellaha_Hoose_rain"], na.rm=TRUE)
    G<- mean(current.long[(t-6):(t- 1),"Sanaag_CurrentRegion"], na.rm=TRUE)
    H<- before.long[(t- 1),"Mudug_BeforeRegion"]
    I<- before.long[(t- 1),"Bay_BeforeRegion"]
    J<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    K<- median(rain.long[(t-5):(t- 1),"Sool_rain"], na.rm=TRUE)
    L<- cosh(0.000576725186841007*D)
    M<- erf(8.56068843220721e-6*C*L)
    N<- cosh(0.000576725186841007*H)
    if ( is.na(E) ){O<- 14.0206106977167*N}
    else if(E>0){O<- G^2 }
    else{O<- 14.0206106977167*N }
    P<- max(97.8184742195244, 9.93574929200005e-6*I*J,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 0.00101451004891555*A*B*M , 8.56351846891925e-5*O , P , -K,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}
TO_9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- median(before.long[(t-17):(t- 1),"Sool_BeforeRegion"], na.rm=TRUE)
    C<- rain.long[(t- 9),"Awdal_rain"]
    D<- tail(movavg(before.long[(t-6):(t- 1),"Bay_BeforeRegion"],5,type="w"),1)
    E<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    G<- future.long[(t- 1),"Sool_FutureRegion"]
    H<- current.long[(t- 1),"Bay_CurrentRegion"]
    I<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    J<- tail(movavg(before.long[(t-3):(t- 1),"Sool_BeforeRegion"], 2,type="m"),1)
    K<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    L<- before.long[(t- 1),"Bari_BeforeRegion"]
    M<- future.long[(t- 1),"Sool_FutureRegion"]
    N<- tail(movavg(future.long[(t-5):(t- 1),"Shabeellaha_Hoose_FutureRegion"], 4,type="m"),1)
    O<- rain.long[(t- 14),"Banaadir_rain"]
    P<- tail(movavg(conflicts.long[(t-7):(t- 1),"Jubbada_Hoose_Conflict"],6,type="w"),1)
    Q<- before.long[(t- 1),"Bari_BeforeRegion"]
    R<- min(D,sum( 0.005468954332049*E , 0.000323610742427131*G*H , 0.0131843388167927*I*J , 0.005468954332049*K*L , M , -N,na.rm=TRUE),na.rm=TRUE)
    if ( is.na(C) ){S<- O*P}
    else if(C>0){S<- R }
    else{S<- O*P }
    U<- max(sum(A , B,na.rm=TRUE), S,na.rm=TRUE)
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( U , -Q,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}
# Define a server for the Shiny app
# the ids refer to the google sheet refering to the special identifier
shinyServer(function(input, output, session) {


  pred_data <- reactive({

    #testing

    region <- input$region
    fmonths_start <- which(conflicts.long$Date == monthStart(as.Date("2017-02-01")))
    fmonths_end <- which(conflicts.long$Date == monthStart(as.Date("2018-02-01")))
    # prepare columns for the merged graph



    len <- fmonths_end - fmonths_start+1
    PI <- PA <- PD <- rep(NA, len)

    if(region == "Bay"){
      #BAY_A1
      #BAY_12
      #BAY_13
      PA <- BAY_1arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- BAY_A4arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- BAY_13arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]

      reg_arr <- paste("Bay","CurrentRegion",sep="_")

      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else if(region == "Banadir"){
      #BN_JUN2
      #BN_JUN8
      #BN_JUN9
      PA <- BA_2arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- BA_8arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- BA_9arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Banadir","CurrentRegion",sep="_")

      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else if(region == "Gedo"){
      PA <- GE_6arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- GE_9arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- GE_8arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Gedo","CurrentRegion",sep="_")

      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else if(region == "Bari"){
      PA <- BR_JUN3arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- BR_JUN10arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- BR_1arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Gedo","CurrentRegion",sep="_")

      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else if(region == "Middle Juba"){
      PA <- MJ_2Xarrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- MJ_7Xarrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- MJ_7arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Jubbada_Dhexe","CurrentRegion",sep="_")

      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else if(region == "Lower Juba"){
      PA <- LJ_9arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- LJ_6arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- LJ_6Xarrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Jubbada_Hoose","CurrentRegion",sep="_")

      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]

    }
    else if(region == "Middle Shabelle"){
      #MS_5
      #MS_9
      #MS_minus2

      PA <- MS_6arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- MS_9arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- MS_minus2arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Shabeellaha_Dhexe","CurrentRegion",sep="_")

      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]

    }
    else if(region == "Lower Shabelle"){
      PA <- LS_JUN10arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- LS_SEPT10arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- LS_1arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Shabeellaha_Hoose","CurrentRegion",sep="_")

      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else if(region == "Bakool"){
      PA <- BK_10arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- BK_4arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- BK_JUN6arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Bakool","CurrentRegion",sep="_")

      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else if(region == "Hiiraan"){
      PA <- HI_6arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- HI_8arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- HI_JUN2arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Hiiraan","CurrentRegion",sep="_")

      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else if(region == "Galgaduud"){
      PA <- GA_2arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- GA_4arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- GA_JUN3arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Galgaduud","CurrentRegion",sep="_")

      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }

    else if(region == "Mudug"){
      PA <- MU_4arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- MU_JUN2arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- MU_JUN10arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Mudug","CurrentRegion",sep="_")

      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }

    else if(region == "Nugaal"){
      PA <- NU_JUN7arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- NU_8arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- NU_4arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Nugaal","CurrentRegion",sep="_")

      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else if(region == "Sanaag"){
      PA <- NU_JUN7arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- NU_8arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- NU_4arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Nugaal","CurrentRegion",sep="_")
      
      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else if(region == "Sool"){
      PA <- SO_JUN6arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- SO_2arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- SO_4arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Sool","CurrentRegion",sep="_")
      
      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else if(region == "Togdheer"){
      PA <- TO_JUN10arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- TO_JUN5arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- TO_9arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Togdheer","CurrentRegion",sep="_")
      
      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else{
      PA <- BK_10arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- BK_4arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- BK_JUN6arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Whatever","CurrentRegion",sep="_")

      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]

    }

    A<- A[1:len]
    Date <- conflicts.long$Date[fmonths_start:fmonths_end]

    long <- data.frame(
      Date = Date,
      Period=rep((1:len),4),
      Displaced_People = c(A,as.integer(PI), as.integer(PJ),as.integer(PK)),
      Indicator=rep(c("Actual_Arrivals",
                      "Algorithm_1",
                      "Algorithm_2",
                      "Algorithm_3"),
                    each=len))

    Actual_Arrivals <- A

    Model_1_Arrivals <- PI
    Accuracy_Model_1 <- A-PI
    Percentage_1 <- A/Accuracy_Model_1
    Percentage_1[is.na(Percentage_1)] <- " "

    Model_2_Arrivals <- PJ
    Accuracy_Model_2 <- A-PJ
    Percentage_2 <- A/Accuracy_Model_2
    Percentage_2[is.na(Percentage_2)] <- " "


    Model_3_Arrivals <- PK
    Accuracy_Model_3 <- A-PK
    Percentage_3 <- A/Accuracy_Model_3
    Percentage_3[is.na(Percentage_3)] <- " "

    Date <- Date

    a1 <- data.frame(Date = format(Date,"%Y-%m-%d"),
                  Actual_Arrivals = as.integer(Actual_Arrivals),
                  Model_1 = as.integer(Model_1_Arrivals))


    a1$Date <- as.Date(a1$Date, format = "%Y-%m-%d")

    Date <- Date

    a2 <- data.frame(Date = a1$Date,
                Actual_Arrivals = as.integer(Actual_Arrivals),
                Model_2 = as.integer(Model_2_Arrivals))

    a2$Date <- as.Date(a2$Date, format = "%Y-%m-%d")

    a3 <- data.frame(Date = a1$Date,
                Actual_Arrivals = as.integer(Actual_Arrivals),
                Model_3 = as.integer(Model_3_Arrivals))

    a3$Date <- as.Date(a3$Date, format = "%Y-%m-%d")

    wide <- cbind(Date = format (Date,"%Y%b"),
              Actual_Arrivals = as.integer(Actual_Arrivals),
              Model1_Predictions = as.integer(Model_1_Arrivals),
              Model2_Predictions = as.integer(Model_2_Arrivals),
              Model3_Predictions = as.integer(Model_3_Arrivals))

    list(long=long, wide=wide, a1=a1, a2=a2, a3=a3)


  })

  #Create a graph with all the values from the inputs

output$graph1 <- renderPlotly({

  df <- pred_data()[["long"]]

  # don't switch to scientific notation, since we want date to be
  # represented in milliseconds
  p <- plot_ly(df, x = ~Date, y = ~Displaced_People, linetype = ~Indicator, type = 'scatter', mode = 'lines+markers')

 })

 pred_camp <- reactive({

   #testing

   region <- input$camp
   fmonths_start <- which(conflicts.long$Date == monthStart(as.Date("2017-02-01")))
   fmonths_end <- which(conflicts.long$Date == monthStart(as.Date("2018-02-01")))
   # prepare columns for the merged graph



   len <- fmonths_end - fmonths_start+1
   PI <- PA <- PD <- rep(NA, len)

   if(region == "Dollo Ado"){
     #BAY_A1
     #BAY_12
     #BAY_13
     PA <- BAY_1arrivals(fmonths_start, fmonths_end)
     PI <- PA[fmonths_start:fmonths_end]
     PB <- BAY_A4arrivals(fmonths_start, fmonths_end)
     PJ <- PB[fmonths_start:fmonths_end]
     PC <- BAY_13arrivals(fmonths_start, fmonths_end)
     PK <- PC[fmonths_start:fmonths_end]

     reg_arr <- paste("DolloAdo","CurrentRegion",sep="_")

     A <- dollos.long[ fmonths_start:fmonths_end, reg_arr ]
   }

   else{
     PA <- BK_10arrivals(fmonths_start, fmonths_end)
     PI <- PA[fmonths_start:fmonths_end]
     PB <- BK_4arrivals(fmonths_start, fmonths_end)
     PJ <- PB[fmonths_start:fmonths_end]
     PC <- BK_JUN6arrivals(fmonths_start, fmonths_end)
     PK <- PC[fmonths_start:fmonths_end]
     reg_arr <- paste("Whatever","CurrentRegion",sep="_")

     A <- current.long[ fmonths_start:fmonths_end, reg_arr ]

   }

   A<- A[1:len]
   Date <- conflicts.long$Date[fmonths_start:fmonths_end]

   long <- data.frame(
     Date = Date,
     Period=rep((1:len),4),
     Displaced_People = c(A,as.integer(PI), as.integer(PJ),as.integer(PK)),
     Indicator=rep(c("Actual_Arrivals",
                     "Algorithm_1",
                     "Algorithm_2",
                     "Algorithm_3"),
                   each=len))

   Actual_Arrivals <- A

   Model_1_Arrivals <- PI
   Accuracy_Model_1 <- A-PI
   Percentage_1 <- A/Accuracy_Model_1
   Percentage_1[is.na(Percentage_1)] <- " "

   Model_2_Arrivals <- PJ
   Accuracy_Model_2 <- A-PJ
   Percentage_2 <- A/Accuracy_Model_2
   Percentage_2[is.na(Percentage_2)] <- " "

   Model_3_Arrivals <- PK
   Accuracy_Model_3 <- A-PK
   Percentage_3 <- A/Accuracy_Model_3
   Percentage_3[is.na(Percentage_3)] <- " "

   Date <- Date

   a1 <- data.frame(Date = format(Date,"%Y-%m-%d"),
                 Actual_Arrivals = as.integer(Actual_Arrivals),
                 Model_1 = as.integer(Model_1_Arrivals))


   a1$Date <- as.Date(a1$Date, format = "%Y-%m-%d")

   Date <- Date

   a2 <- data.frame(Date = a1$Date,
               Actual_Arrivals = as.integer(Actual_Arrivals),
               Model_2 = as.integer(Model_2_Arrivals))

   a2$Date <- as.Date(a2$Date, format = "%Y-%m-%d")

   a3 <- data.frame(Date = a1$Date,
               Actual_Arrivals = as.integer(Actual_Arrivals),
               Model_3 = as.integer(Model_3_Arrivals))

   a3$Date <- as.Date(a3$Date, format = "%Y-%m-%d")

   wide <- cbind(Date = format (Date,"%Y%b"),
             Actual_Arrivals = as.integer(Actual_Arrivals),
             Model1_Predictions = as.integer(Model_1_Arrivals),
             Model2_Predictions = as.integer(Model_2_Arrivals),
             Model3_Predictions = as.integer(Model_3_Arrivals))

   list(long=long, wide=wide, a1=a1, a2=a2, a3=a3)


 })

 output$graph2 <- renderPlotly({

   df <- pred_camp()[["long"]]

   # don't switch to scientific notation, since we want date to be
   # represented in milliseconds
   p <- plot_ly(df, x = ~Date, y = ~Displaced_People, linetype = ~Indicator, type = 'scatter', mode = 'lines+markers')

  })
#Downloadable csv of selected dataset ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste("report", ".pdf", sep = "")
  },
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)

    # Set up parameters to pass to Rmd document
    params <- list(n = pred_data()[["a1"]],
                   m = pred_data()[["a2"]],
                   o = pred_data()[["a3"]])

    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)


#Downloadable csv of selected dataset ----
output$downloadCsv <- downloadHandler(
  filename = function() {
    paste("data", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(pred_data()[["wide"]], file, row.names = FALSE)
  }
)


}

)
