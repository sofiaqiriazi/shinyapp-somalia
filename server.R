library(rCharts)
library(shiny)
library(datasets)
library(magrittr)
library(XML)
library(reshape)
library(gsheet)
library(ggplot2)
library(scales)
library(zoo)
library(pracma)
library(psych)
library(devtools)
library(rmarkdown)


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
rivers <-gsheet2text(jetson, sheetid =1052168743)
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

# hc<-seq(as.Date(max(conflicts.long[,"Date"])), as.Date("2019-01-6"), by="months")
# ha<-seq(as.Date(max(arrs.long[,"Date"])), as.Date("2019-01-6"), by="months")
# hd<-seq(as.Date(max(deps.long[,"Date"])), as.Date("2019-01-6"), by="months")
# hr<-seq(as.Date(max(rain.long[,"Date"])), as.Date("2019-01-6"), by="months")
#
# fill_start <- nrow(conflicts.long)
# fill_end <- nrow(conflicts.long) + length(hc)
# nrowconflicts <-nrow(conflicts.long)
#
# for (i in 1:length(hc)){
#   conflicts.long[nrowconflicts+i,"Date"] <- hc[i]
#   arrs.long[nrowconflicts+i,"Date"] <- ha[i]
#   deps.long[nrowconflicts+i,"Date"] <- hd[i]
#   rain.long[nrowconflicts+i,"Date"] <- hr[i]
# }

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


BAY_2arrivals <- function(start, end){
  start = 20
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    C<- before.long[(t- 2),"Nugaal_BeforeRegion"]
    D<- future.long[(t- 16),"Mudug_FutureRegion"]
    E<- current.long[(t- 1),"Awdal_CurrentRegion"]
    G<- future.long[(t- 7),"Sanaag_FutureRegion"]
    H<- conflicts.long[(t- 1),"Awdal_Conflict"]
    I<- future.long[(t- 8),"Bari_FutureRegion"]
    J<- goats.long[(t- 6),"Awdal_goatprice"]
    K<- median(future.long[(t-4):(t- 1),"Jubbada_Hoose_FutureRegion"], na.rm=TRUE)
    if ( is.na(4.80168968346346) || is.na( H)){L<-0}
    else if(4.80168968346346<= H){L<-1 }
    else{L<-0 }
    M<- tan(J)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 4.80222888020813*A , 1.59090997853518*B , 3.0114519156067*C , 2.99190205691752*D , 0.951826957171889*E*G*L , I , M , -K,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
 
  return(PA)
}

BAY_14arrivals <- function(start, end){
  start = 20
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){

    A<- fatalities.long[(t- 13),"Gedo_Fatalities"]
    B<- future.long[(t- 3),"Nugaal_FutureRegion"]
    C<- mean(current.long[(t-4):(t- 1),"Hiiraan_CurrentRegion"], na.rm=TRUE)
    D<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    E<- current.long[(t- 1),"Awdal_CurrentRegion"]
    G<- before.long[(t- 1),"Mudug_BeforeRegion"]
    H<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    I<- mean(current.long[(t-15):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], na.rm=TRUE)
    J<- future.long[(t- 2),"Hiiraan_FutureRegion"]
    K<- conflicts.long[(t- 1),"Awdal_Conflict"]
    L<- fatalities.long[(t- 1),"Sool_Fatalities"]
    M<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    N<- future.long[(t- 6),"Togdheer_FutureRegion"]
    if ( is.na(A) ){O<- C}
    else if(A>0){O<- B }
    else{O<- C }
    P<- max(sum(0.000132337180250834*D*E , 6.60533916454304e-7*G*H*I , -J,na.rm=TRUE), 1.07889037646398*K*L*M*N,na.rm=TRUE)
    Q<- max(565, P,na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( O , Q,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  
  return(PA)
}

BAY_16arrivals <- function(start, end){

  len = end
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){

    A<- rain.long[(t- 11),"Hiiraan_rain"]
    B<- before.long[(t- 15),"Sool_BeforeRegion"]
    C<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    D<- current.long[(t- 1),"Awdal_CurrentRegion"]
    E<- before.long[(t- 15),"Sool_BeforeRegion"]
    G<- fatalities.long[(t- 1),"Bay_Fatalities"]
    H<- before.long[(t- 1),"Mudug_BeforeRegion"]
    I<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    J<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    K<- current.long[(t- 1),"Awdal_CurrentRegion"]
    L<- fatalities.long[(t- 1),"Awdal_Fatalities"]
    M<- before.long[(t- 1),"Mudug_BeforeRegion"]
    N<- rain.long[(t- 11),"Hiiraan_rain"]
    O<- before.long[(t- 15),"Sool_BeforeRegion"]
    P<- mean(conflicts.long[(t-9):(t- 1),"Awdal_Conflict"], na.rm=TRUE)
    Q<- before.long[(t- 1),"Mudug_BeforeRegion"]
    R<- fatalities.long[(t- 1),"Bay_Fatalities"]
    S<- before.long[(t- 1),"Mudug_BeforeRegion"]
    U<- fatalities.long[(t- 1),"Bay_Fatalities"]
    if ( is.na(G) ){V<- J}
    else if(G>0){V<- 0.00214195458986615*H*I }
    else{V<- J }
    W<- max(A*B,sum( 0.00015635886853926*C*D , E , V,na.rm=TRUE),na.rm=TRUE)
    X<- sin(P)
    if ( is.na(R) ){Y<- U}
    else if(R>0){Y<- S }
    else{Y<- U }
    if(is.infinite(W)){W <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(X)){X <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(Y)){Y <- 0 }
    FIN <-sum( W , -K , -L*M , -N*O*X , -1.94583981584402e-5*Q*Y,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  return(PA)
}
BA_SEP7arrivals <- function(start, end){
  start = 20
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Mudug_BeforeRegion"]
    B<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    C<- before.long[(t- 1),"Gedo_BeforeRegion"]
    D<- mean(conflicts.long[(t-16):(t- 1),"Jubbada_Dhexe_Conflict"], na.rm=TRUE)
    E<- future.long[(t- 1),"Bari_FutureRegion"]
    G<- water.long[(t- 4),"Togdheer_WaterDrumPrice"]
    H<- before.long[(t- 1),"Mudug_BeforeRegion"]
    I<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    J<- mean(rain.long[(t-12):(t- 1),"Gedo_rain"], na.rm=TRUE)
    K<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    L<- future.long[(t- 1),"Bari_FutureRegion"]
    M<- goats.long[(t- 1),"Jubbada_Hoose_goatprice"]
    N<- future.long[(t- 10),"Gedo_FutureRegion"]
    O<- min(sum(0.634530258083762*K , -9764.89637118794,na.rm=TRUE), L,na.rm=TRUE)
    P<- min(J, O,na.rm=TRUE)
    Q<- min(I, P,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 5.45258316814397*A , 0.634530258083762*B , C*D , E , G , -H*Q , -0.0138771809011655*M , -5.38123507880934*N,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  return(PA)
}

BA_SEP2arrivals <- function(start, end){
  start = 20
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){
    
    A<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    B<- before.long[(t- 1),"Gedo_BeforeRegion"]
    C<- fatalities.long[(t- 3),"Sanaag_Fatalities"]
    D<- before.long[(t- 1),"Mudug_BeforeRegion"]
    E<- current.long[(t- 1),"Gedo_CurrentRegion"]
    G<- before.long[(t- 1),"Gedo_BeforeRegion"]
    H<- water.long[(t- 11),"Banadir_WaterDrumPrice"]
    I<- before.long[(t- 1),"Gedo_BeforeRegion"]
    J<- fatalities.long[(t- 3),"Togdheer_Fatalities"]
    K<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    L<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    M<- current.long[(t- 15),"Jubbada_Dhexe_CurrentRegion"]
    N<- goats.long[(t- 1),"Jubbada_Hoose_goatprice"]
    O<- sin(H)
    P<- cos(0.820777499965034*L)
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
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 0.820777499965034*A , B*C , 0.00038577999608999*D*E , G*O , 0.379038413366182*I*J , K*P , M , -0.0144876933172979*N,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  return(PA)
}

BA_SEP5arrivals <- function(start, end){
  start = 20
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){
    
    A<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    B<- before.long[(t- 1),"Mudug_BeforeRegion"]
    C<- current.long[(t- 1),"Gedo_CurrentRegion"]
    D<- future.long[(t- 15),"Jubbada_Hoose_FutureRegion"]
    E<- mean(fatalities.long[(t-11):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    G<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    H<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    I<- current.long[(t- 15),"Jubbada_Dhexe_CurrentRegion"]
    J<- fatalities.long[(t- 3),"Sanaag_Fatalities"]
    K<- before.long[(t- 1),"Gedo_BeforeRegion"]
    L<- goats.long[(t- 1),"Jubbada_Hoose_goatprice"]
    M<- sin(0.704821320693777*H)
    N<- round(1.9223553076521*K)
    O<- min(26734.1634362641, J*N,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 0.704821320693777*A , 0.000402906694472956*B*C , D*E , G*M , I , O , -0.0122243904473122*L,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  return(PA)
}

BK_10arrivals <- function(start, end){
  start = 20
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
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
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
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
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
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

GE2017_6arrivals <- function(start, end){
  start = 20
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){ 
    
    A<- before.long[(t- 17),"Bakool_BeforeRegion"]
    B<- current.long[(t- 17),"Shabeellaha_Hoose_CurrentRegion"]
    C<- mean(current.long[(t-9):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    D<- future.long[(t- 1),"Sool_FutureRegion"]
    E<- median(rain.long[(t-14):(t- 1),"Shabeellaha_Hoose_rain"], na.rm=TRUE)
    G<- rain.long[(t- 1),"Awdal_rain"]
    H<- mean(before.long[(t-3):(t- 1),"Jubbada_Dhexe_BeforeRegion"], na.rm=TRUE)
    I<- current.long[(t- 1),"Awdal_CurrentRegion"]
    J<- current.long[(t- 1),"Awdal_CurrentRegion"]
    K<- before.long[(t- 17),"Bakool_BeforeRegion"]
    L<- goats.long[(t- 1),"Woqooyi_Galbeed_goatprice"]
    M<- median(before.long[(t-16):(t- 1),"Jubbada_Dhexe_BeforeRegion"], na.rm=TRUE)
    N<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    O<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    P<- atan2(J, K)
    Q<- max(sum(B , C,na.rm=TRUE),sum( 11316.704055172 , D*E , 0.112452583561993*G*H , I*P,na.rm=TRUE),na.rm=TRUE)
    R<- max(A, Q,na.rm=TRUE)
    S<- asinh(N)
    U<- max(sum(R , -0.038408874560855*L,na.rm=TRUE), M*S,na.rm=TRUE)
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( U , -O,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

GE2017_9arrivals <- function(start, end){
  start = 20
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){ 
    
    A<- future.long[(t- 12),"Bakool_FutureRegion"]
    B<- current.long[(t- 1),"Awdal_CurrentRegion"]
    C<- rain.long[(t- 15),"Jubbada_Hoose_rain"]
    D<- rain.long[(t- 16),"Jubbada_Hoose_rain"]
    E<- rain.long[(t- 1),"Awdal_rain"]
    G<- rain.long[(t- 15),"Jubbada_Hoose_rain"]
    H<- rain.long[(t- 15),"Jubbada_Hoose_rain"]
    I<- future.long[(t- 1),"Sool_FutureRegion"]
    J<- current.long[(t- 1),"Awdal_CurrentRegion"]
    K<- median(before.long[(t-17):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    L<- conflicts.long[(t- 1),"Awdal_Conflict"]
    M<- current.long[(t- 1),"Mudug_CurrentRegion"]
    N<- before.long[(t- 1),"Awdal_BeforeRegion"]
    O<- median(future.long[(t-16):(t- 1),"Bakool_FutureRegion"], na.rm=TRUE)
    P<- max(sum(C*D , 0.431327083317988*E*G , H,na.rm=TRUE), 0.00601797424220596*I*J,na.rm=TRUE)
    Q<- max(P, 1.98505049817155*K,na.rm=TRUE)
    R<- max(sum(B , Q,na.rm=TRUE), 0.0789655804265846*L*M,na.rm=TRUE)
    S<- max(A,sum( R , -N,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( S , -O
               ,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

GE2017_8arrivals <- function(start, end){
  start = 20
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){ 
    
    A<- before.long[(t- 15),"Sool_BeforeRegion"]
    B<- fatalities.long[(t- 15),"Banaadir_Fatalities"]
    C<- conflicts.long[(t- 1),"Woqooyi_Galbeed_Conflict"]
    D<- rain.long[(t- 1),"Awdal_rain"]
    E<- current.long[(t- 1),"Awdal_CurrentRegion"]
    G<- future.long[(t- 8),"Nugaal_FutureRegion"]
    H<- median(before.long[(t-13):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    I<- current.long[(t- 1),"Mudug_CurrentRegion"]
    J<- future.long[(t- 2),"Bay_FutureRegion"]
    K<- conflicts.long[(t- 1),"Awdal_Conflict"]
    L<- future.long[(t- 6),"Togdheer_FutureRegion"]
    M<- C%% 1.67227803400689
    N<- max(1.67227803400689*H, 0.776560194394701*I/J,na.rm=TRUE)
    O<- max(sum(B^M , 5.02103697060346e-7*D^2*E^2 , G , N,na.rm=TRUE), 1.93827058789905*K*L,na.rm=TRUE)
    P<- max(A, O,na.rm=TRUE)
    FIN <-P
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

  len = 97
  PI <- PA <- PD <- rep(NA, len)
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
MJ_2arrivals <- function(start, end){

  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){

    A<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    B<- conflicts.long[(t- 1),"Awdal_Conflict"]
    C<- fatalities.long[(t- 9),"Bari_Fatalities"]
    D<- fatalities.long[(t- 9),"Bari_Fatalities"]
    E<- future.long[(t- 1),"Bakool_FutureRegion"]
    G<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    H<- before.long[(t- 1),"Bari_BeforeRegion"]
    I<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    J<- fatalities.long[(t- 9),"Bari_Fatalities"]
    K<- future.long[(t- 1),"Bakool_FutureRegion"]
    L<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    M<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    N<- conflicts.long[(t- 1),"Awdal_Conflict"]
    O<- fatalities.long[(t- 9),"Bari_Fatalities"]
    P<- future.long[(t- 1),"Gedo_FutureRegion"]
    Q<- current.long[(t- 1),"Banadir_CurrentRegion"]
    R<- min(0.152346815708488*B, C,na.rm=TRUE)
    if ( is.na(D) ){S<- H}
    else if(D>0){S<- 0.00038013677579467*E*G }
    else{S<- H }
    U<- max(sum(384.939466657484 , A*R , S,na.rm=TRUE), 0.160505532453264*I,na.rm=TRUE)
    V<- tan(0.00038013677579467*K*L)
    W<- min(0.152346815708488*N, O,na.rm=TRUE)
    X<- tan(M*W)
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(X)){X <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( U , -J , -V , -X , -4.64742734901024e-5*P*Q,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  return(PA)
}


MJ_9arrivals <- function(start, end){

  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){

    A<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    B<- mean(future.long[(t-6):(t- 1),"Bakool_FutureRegion"], na.rm=TRUE)
    C<- conflicts.long[(t- 1),"Awdal_Conflict"]
    D<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    E<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    G<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    H<- fatalities.long[(t- 1),"Gedo_Fatalities"]
    I<- rivers.long[(t- 3),"Juba_Dhexe_BualleStation_Juba_River"]
    J<- conflicts.long[(t- 1),"Awdal_Conflict"]
    K<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    L<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    M<- fatalities.long[(t- 1),"Bari_Fatalities"]
    N<- current.long[(t- 1),"Banadir_CurrentRegion"]
    O<- mean(future.long[(t-10):(t- 1),"Gedo_FutureRegion"], na.rm=TRUE)
    P<- max(0.000492063306548717*A*B, 0.049922717432803*C*D*E,na.rm=TRUE)
    Q<- max(424, 0.1641743664885*G,na.rm=TRUE)
    R<- atan2(0.049922717432803*J*K*L, M)
    S<- max(Q, H*I*R,na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( P , S , -8.31757418929373e-5*N*O,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  return(PA)
}

LJ_8arrivals <- function(start, end){

  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){

    A<- conflicts.long[(t- 1),"Awdal_Conflict"]
    B<- before.long[(t- 11),"Sanaag_BeforeRegion"]
    C<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    D<- rain.long[(t- 11),"Shabeellaha_Dhexe_rain"]
    E<- median(rain.long[(t-7):(t- 1),"Bari_rain"], na.rm=TRUE)
    G<- rain.long[(t- 9),"Bakool_rain"]
    H<- mean(future.long[(t-8):(t- 1),"Galgaduud_FutureRegion"], na.rm=TRUE)
    I<- future.long[(t- 1),"Togdheer_FutureRegion"]
    J<- rain.long[(t- 9),"Bakool_rain"]
    K<- mean(rain.long[(t-10):(t- 1),"Bari_rain"], na.rm=TRUE)
    L<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    M<- conflicts.long[(t- 2),"Jubbada_Dhexe_Conflict"]
    N<- future.long[(t- 11),"Galgaduud_FutureRegion"]
    O<- median(current.long[(t-11):(t- 1),"Gedo_CurrentRegion"], na.rm=TRUE)
    P<- max(I, J*K,na.rm=TRUE)
    Q<- max(sum(0.606757439691736*L , M*N,na.rm=TRUE), O,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( 1.95263185850796*A*B , C*D*E , G , H , P , Q , -328.786062457441,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  return(PA)
}

LJ_1Xarrivals <- function(start, end){

  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){

    A<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    B<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    C<- rain.long[(t- 1),"Awdal_rain"]
    D<- conflicts.long[(t- 1),"Bay_Conflict"]
    E<- fatalities.long[(t- 1),"Bari_Fatalities"]
    G<- median(rivers.long[(t-4):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    H<- current.long[(t- 17),"Nugaal_CurrentRegion"]
    I<- median(before.long[(t-10):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    J<- conflicts.long[(t- 3),"Bay_Conflict"]
    K<- goats.long[(t- 1),"Hiiraan_goatprice"]
    L<- current.long[(t- 1),"Awdal_CurrentRegion"]
    M<- mean(current.long[(t-5):(t- 1),"Hiiraan_CurrentRegion"], na.rm=TRUE)
    N<- fatalities.long[(t- 1),"Bari_Fatalities"]
    O<- mean(rain.long[(t-4):(t- 1),"Awdal_rain"], na.rm=TRUE)
    P<- before.long[(t- 1),"Shabeellaha_Hoose_BeforeRegion"]
    Q<- current.long[(t- 1),"Awdal_CurrentRegion"]
    if ( is.na(J) ){R<- M}
    else if(J>0){R<- 5.03026963301009e-6*K*L }
    else{R<- M }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( A*B*C , D*E*G , H , I , R , -N*O , -0.000115842576271141*P*Q,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  return(PA)
}

LJ_9Xarrivals <- function(start, end){

  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){

    A<- conflicts.long[(t- 3),"Shabeellaha_Hoose_Conflict"]
    B<- median(fatalities.long[(t-10):(t- 1),"Bay_Fatalities"], na.rm=TRUE)
    C<- rain.long[(t- 5),"Bay_rain"]
    D<- fatalities.long[(t- 7),"Togdheer_Fatalities"]
    E<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    G<- mean(conflicts.long[(t-3):(t- 1),"Gedo_Conflict"], na.rm=TRUE)
    H<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    I<- mean(current.long[(t-3):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], na.rm=TRUE)
    J<- median(before.long[(t-17):(t- 1),"Hiiraan_BeforeRegion"], na.rm=TRUE)
    K<- rain.long[(t- 5),"Woqooyi_Galbeed_rain"]
    L<- mean(fatalities.long[(t-5):(t- 1),"Galguduud_Fatalities"], na.rm=TRUE)
    M<- rain.long[(t- 6),"Shabeellaha_Dhexe_rain"]
    N<- mean(fatalities.long[(t-10):(t- 1),"Sool_Fatalities"], na.rm=TRUE)
    O<- median(fatalities.long[(t-17):(t- 1),"Sool_Fatalities"], na.rm=TRUE)
    P<- conflicts.long[(t- 3),"Shabeellaha_Hoose_Conflict"]
    Q<- max(A*B, C*D,na.rm=TRUE)
    R<- max(I, 9.36406661543063*J,na.rm=TRUE)
    S<- max(E*G/H, R,na.rm=TRUE)
    U<- max(sum(K*L , M*N*O,na.rm=TRUE), P,na.rm=TRUE)
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(U)){U <- 0 }
    FIN <-sum( Q , S , U , -1796.93556600875,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  return(PA)
}

MS_3arrivals <- function(start, end){

  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){

    A<- discharge.long[(t- 13),"Juba_River_discharge"]
    B<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    C<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    D<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    E<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    G<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    H<- fatalities.long[(t- 1),"Hiiraan_Fatalities"]
    I<- median(rain.long[(t-11):(t- 1),"Sool_rain"], na.rm=TRUE)
    J<- conflicts.long[(t- 1),"Jubbada_Hoose_Conflict"]
    K<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    L<- mean(discharge.long[(t-4):(t- 1),"Juba_River_discharge"], na.rm=TRUE)
    M<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    N<- discharge.long[(t- 13),"Juba_River_discharge"]
    O<- sin(0.0694275699474916*J*K)
    P<- max(B,sum( 0.004557249344714*C*D , 0.000369204954580446*E*G , 8.37512996580642*H*I/O , L , -M , -N,na.rm=TRUE),na.rm=TRUE)
    Q<- max(A, P,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  return(PA)
}
MS_8arrivals <- function(start, end){
  start = 20
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){

    A<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    B<- rain.long[(t- 11),"Sanaag_rain"]
    C<- rain.long[(t- 2),"Mudug_rain"]
    D<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    E<- future.long[(t- 2),"Togdheer_FutureRegion"]
    G<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    H<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    I<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    J<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    K<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    L<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    M<- mean(conflicts.long[(t-8):(t- 1),"Woqooyi_Galbeed_Conflict"], na.rm=TRUE)
    N<- before.long[(t- 9),"Sool_BeforeRegion"]
    if ( is.na(47.9999363717339) || is.na( C)){O<-0}
    else if(47.9999363717339>= C){O<-1 }
    else{O<-0 }
    if ( is.na(O) ){P<-sum( 70.1892819497891*L*M , N,na.rm=TRUE)}
    else if(O>0){P<-sum( 47.9999363717339 , D*E , 0.394426094748*G*H , 0.00388556381291968*I*J , K,na.rm=TRUE) }
    else{P<-sum( 70.1892819497891*L*M , N,na.rm=TRUE) }
    Q<- max(A*B, P,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  return(PA)
}

MS_9arrivals <- function(start, end){
  start = 20
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
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

LS_SEPT2arrivals <- function(start, end){
  start = 20
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){
    
    A<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    B<- median(current.long[(t-7):(t- 1),"Togdheer_CurrentRegion"], na.rm=TRUE)
    C<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    D<- before.long[(t- 1),"Bakool_BeforeRegion"]
    E<- fatalities.long[(t- 10),"Nugaal_Fatalities"]
    G<- current.long[(t- 14),"Sool_CurrentRegion"]
    H<- rivers.long[(t- 10),"Gedo_LuuqStation_Juba_River"]
    I<- mean(current.long[(t-4):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    J<- before.long[(t- 12),"Sool_BeforeRegion"]
    K<- median(fatalities.long[(t-15):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    L<- current.long[(t- 13),"Nugaal_CurrentRegion"]
    M<- conflicts.long[(t- 1),"Awdal_Conflict"]
    N<- median(current.long[(t-11):(t- 1),"Bari_CurrentRegion"], na.rm=TRUE)
    O<- conflicts.long[(t- 6),"Bay_Conflict"]
    P<- future.long[(t- 12),"Sanaag_FutureRegion"]
    Q<- (12.3801302307998*J)
    R<- max(sum(A*B , 0.390738190643367*C*D , E*G , H*I , Q^K , -L , -M*N,na.rm=TRUE), O*P,na.rm=TRUE)
    FIN <-R
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  return(PA)
}

LS_JUN2arrivals <- function(start, end){
  start = 20
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){
    
    A<- future.long[(t- 1),"Banadir_FutureRegion"]
    B<- current.long[(t- 5),"Shabeellaha_Dhexe_CurrentRegion"]
    C<- mean(before.long[(t-7):(t- 1),"Hiiraan_BeforeRegion"], na.rm=TRUE)
    D<- current.long[(t- 1),"Awdal_CurrentRegion"]
    E<- mean(before.long[(t-7):(t- 1),"Hiiraan_BeforeRegion"], na.rm=TRUE)
    G<- future.long[(t- 1),"Banadir_FutureRegion"]
    H<- current.long[(t- 5),"Shabeellaha_Dhexe_CurrentRegion"]
    I<- mean(before.long[(t-7):(t- 1),"Hiiraan_BeforeRegion"], na.rm=TRUE)
    J<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    K<- current.long[(t- 1),"Bay_CurrentRegion"]
    L<- before.long[(t- 1),"Bari_BeforeRegion"]
    M<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    N<- current.long[(t- 1),"Awdal_CurrentRegion"]
    O<- mean(before.long[(t-7):(t- 1),"Hiiraan_BeforeRegion"], na.rm=TRUE)
    P<- future.long[(t- 1),"Banadir_FutureRegion"]
    Q<- current.long[(t- 5),"Shabeellaha_Dhexe_CurrentRegion"]
    R<- mean(before.long[(t-7):(t- 1),"Hiiraan_BeforeRegion"], na.rm=TRUE)
    S<- rain.long[(t- 1),"Shabeellaha_Hoose_rain"]
    U<- mean(fatalities.long[(t-7):(t- 1),"Bakool_Fatalities"], na.rm=TRUE)
    V<- mean(conflicts.long[(t-5):(t- 1),"Awdal_Conflict"], na.rm=TRUE)
    W<- mean(before.long[(t-4):(t- 1),"Sanaag_BeforeRegion"], na.rm=TRUE)
    X<- atan2(E, 1.21463800391762e-7*G*H*I)
    Y<- atan2(O, 1.21463800391762e-7*P*Q*R)
    Z<- max(sum(0.0180239168241371*J*K , 0.00216114747863908*L*M , N*Y,na.rm=TRUE), S*U*V,na.rm=TRUE)
    AA<- max(D*X,sum( Z , -W,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(A)){A <- 0 }
    FIN <-sum( 1.21463800391762e-7*A*B*C , AA,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  return(PA)
}

LS_SEPT1arrivals <- function(start, end){
  start = 20
  end = 97
  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){
    
    A<- current.long[(t- 1),"Bay_CurrentRegion"]
    B<- rivers.long[(t- 17),"Gedo_DollowStation_Juba_River"]
    C<- mean(before.long[(t-7):(t- 1),"Bakool_BeforeRegion"], na.rm=TRUE)
    D<- rain.long[(t- 1),"Awdal_rain"]
    E<- future.long[(t- 1),"Nugaal_FutureRegion"]
    G<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    H<- future.long[(t- 11),"Gedo_FutureRegion"]
    I<- mean(fatalities.long[(t-4):(t- 1),"Jubbada_Hoose_Fatalities"], na.rm=TRUE)
    J<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    K<- fatalities.long[(t- 5),"Galguduud_Fatalities"]
    L<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    M<- mean(rivers.long[(t-4):(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"], na.rm=TRUE)
    N<- current.long[(t- 1),"Bay_CurrentRegion"]
    O<- rivers.long[(t- 17),"Gedo_DollowStation_Juba_River"]
    P<- mean(before.long[(t-7):(t- 1),"Bakool_BeforeRegion"], na.rm=TRUE)
    Q<- rain.long[(t- 1),"Awdal_rain"]
    R<- future.long[(t- 1),"Nugaal_FutureRegion"]
    S<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    U<- future.long[(t- 11),"Gedo_FutureRegion"]
    V<- mean(fatalities.long[(t-4):(t- 1),"Jubbada_Hoose_Fatalities"], na.rm=TRUE)
    W<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    X<- before.long[(t- 1),"Sool_BeforeRegion"]
    Y<- current.long[(t- 1),"Awdal_CurrentRegion"]
    Z<- median(rivers.long[(t-13):(t- 1),"Gedo_DollowStation_Juba_River"], na.rm=TRUE)
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
  return(PA)
}
# Define a server for the Shiny app
# the ids refer to the google sheet refering to the special identifier
shinyServer(function(input, output, session) {


  pred_data <- reactive({

    #testing

    region <- input$region
    fmonths_start <- which(conflicts.long$Date == monthStart(as.Date("2017-01-01")))
    fmonths_end <- which(conflicts.long$Date == monthStart(as.Date("2018-01-01")))
    # prepare columns for the merged graph



    len <- fmonths_end - fmonths_start+1
    PI <- PA <- PD <- rep(NA, len)

    if(region == "Bay"){
      PA <- BAY_2arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- BAY_14arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- BAY_16arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      
      reg_arr <- paste("Bay","CurrentRegion",sep="_")
      
      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else if(region == "Banadir"){
      PA <- BA_SEP7arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- BA_SEP2arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- BA_SEP5arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Banadir","CurrentRegion",sep="_")
      
      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else if(region == "Gedo"){
      PA <- GE2017_6arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- GE2017_9arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- GE2017_8arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Gedo","CurrentRegion",sep="_")
      
      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else if(region == "Middle Juba"){
      PA <- MJ_7Xarrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- MJ_2arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- MJ_9arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Jubbada_Dhexe","CurrentRegion",sep="_")
      
      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else if(region == "Lower Juba"){
      PA <- LJ_8arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- LJ_1Xarrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- LJ_9Xarrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Jubbada_Hoose","CurrentRegion",sep="_")
      
      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]

    }
    else if(region == "Middle Shabelle"){
      PA <- MS_3arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- MS_8arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- MS_9arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Shabeellaha_Dhexe","CurrentRegion",sep="_")
      
      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]

    }
    else if(region == "Lower Shabelle"){
      PA <- LS_SEPT2arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- LS_JUN2arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- LS_SEPT1arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Shabeellaha_Hoose","CurrentRegion",sep="_")
      
      A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    }
    else{
      PA <- BK_10arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- BK_4arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- BK_JUN6arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
      reg_arr <- paste("Bakool","CurrentRegion",sep="_")
      
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
    
    a1 <- cbind(Date = format(Date,"%Y %b"),
                  Actual_Arrivals = as.integer(Actual_Arrivals),
                  Model_1 = as.integer(Model_1_Arrivals),
                  Accuracy_1 = as.integer(Accuracy_Model_1),
                  Percentage = paste(as.character( as.integer(Percentage_1)),"%",sep=""))

    a1 <- as.data.frame(a1)
    
    
    a2 <- cbind(Date = format(Date,"%Y %b"),
                Actual_Arrivals = as.integer(Actual_Arrivals),
                Model_2 = as.integer(Model_2_Arrivals),
                Accuracy_2 = as.integer(Accuracy_Model_2),
                Percentage = paste(as.character( as.integer(Percentage_2)),"%",sep=""))
    
    a2 <- as.data.frame(a2)
    
    a3 <- cbind(Date = format(Date,"%Y %b"),
                Actual_Arrivals = as.integer(Actual_Arrivals),
                Model_3 = as.integer(Model_3_Arrivals),
                Accuracy_3 = as.integer(Accuracy_Model_3),
                Percentage = paste(as.character( as.integer(Percentage_3)),"%",sep=""))
    
    a3 <- as.data.frame(a3)
    
    
    
    list(long=long, a1=a1, a2=a2, a3=a3)


  })

  #Create a graph with all the values from the inputs

output$graph1 <- renderChart2({
  
  df <- pred_data()[["long"]]
  
  # don't switch to scientific notation, since we want date to be
  # represented in milliseconds
  options(scipen = 13)
  dat <- transform(df, Date2 = as.numeric(as.POSIXct(df$Date))*1000)
  
  h1 <- hPlot(Displaced_People ~ Date2, data = dat, 
              group = 'Indicator', 
              radius=4
  )
  h1$chart(type = "spline")
  h1$colors(c("#59AB00", "#4155AF", "#132A8E", "#0072ff"))
  h1$xAxis(type = 'datetime', labels = list(
    format = '{value:%Y-%b}'  
  ))

  return(h1)
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

}

)
