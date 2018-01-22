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


Bay_1arrivals <- function(start, end){

  len = 97
  PI <- PA <- PD <- rep(NA, len)
  for (t in start:end){

    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- future.long[(t- 1),"Sanaag_FutureRegion"]
    C<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    D<- future.long[(t- 10),"Nugaal_FutureRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    H<- future.long[(t- 3),"Nugaal_FutureRegion"]
    I<- mean(future.long[(t-13):(t- 1),"Shabeellaha_Hoose_FutureRegion"], na.rm=TRUE)
    J<- mean(current.long[(t-3):(t- 1),"Togdheer_CurrentRegion"], na.rm=TRUE)
    K<- median(current.long[(t-12):(t- 1),"Jubbada_Dhexe_CurrentRegion"], na.rm=TRUE)
    L<- max(sum(4.84503108841733*A , 1.70406507425211*B , 1.70406507425211*C , 2.02606412957083*D , 2.34737505506137e-8*E^3*G , H , -I , -0.396277558619911*J,na.rm=TRUE), K,na.rm=TRUE)
    FIN <-L
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  #write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

Bay_2arrivals <- function(start, end){

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
    I<- future.long[(t-8),"Bari_FutureRegion"]
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
  #write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
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
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
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
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
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
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
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
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
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
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
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
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
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



    reg_con <- paste(region,"Conflict",sep="_")
    reg_arr <- paste(region,"CurrentRegion",sep="_")
    reg_dep <- paste(region,"BeforeRegion",sep="_")
    reg_rain <- paste(region,"rain",sep="_")

    I <- conflicts.long[ fmonths_start:fmonths_end,reg_con]
    A <- current.long[ fmonths_start:fmonths_end, reg_arr ]
    D <- before.long[ fmonths_start:fmonths_end, reg_dep ]

    #AA <- A[(total_len-30):total_len]
    R <- rain.long[ fmonths_start:fmonths_end, reg_rain]

    len <- fmonths_end - fmonths_start+1
    PI <- PA <- PD <- rep(NA, len)

    if(region == "Bay"){
      PA <- Bay_1arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- Bay_2arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- MJ_9arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]
    }
    else if(region == "Banadir"){
      PA <- Bay_1arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- Bay_2arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- MJ_9arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]

    }
    else if(region == "Gedo"){
      PA <- Bay_1arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- Bay_2arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- MJ_9arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]

    }
    else if(region == "Jubbada_Dhexe"){
      PA <- MJ_7Xarrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- MJ_2arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- MJ_9arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]

    }
    else if(region == "Jubbada_Hoose"){
      PA <- LJ_8arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- LJ_1Xarrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- LJ_9Xarrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]

    }
    else if(region == "Shabeellaha_Dhexe"){
      PA <- Bay_1arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- Bay_2arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- MJ_9arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]

    }
    else if(region == "Shabeellaha_Hoose"){
      PA <- Bay_1arrivals(fmonths_start, fmonths_end)
      PI <- PA[fmonths_start:fmonths_end]
      PB <- Bay_2arrivals(fmonths_start, fmonths_end)
      PJ <- PB[fmonths_start:fmonths_end]
      PC <- MJ_9arrivals(fmonths_start, fmonths_end)
      PK <- PC[fmonths_start:fmonths_end]

    }
    else{
      PA <- rep(NA, len)
      PI <- rep(NA, len)
    }

    A<- A[1:len]
    Date <- conflicts.long$Date[fmonths_start:fmonths_end]
    long <- data.frame(
      Period=rep((1:len),2),
      Date= Date,
      Actual_Arrivals = A,
      Algorithm_1 = as.integer(PI),
      Algorithm_2 = as.integer(PJ),
      Algorithm_3 = as.integer(PK))

    Actual_Arrivals <- A
    Model_Arrivals <- PI
    Date <- Date
    wide <- cbind(Date = format(Date,"%Y %b"),
                  Actual_Arrivals = as.integer(Actual_Arrivals),
                  Model_Arrivals =as.integer(Model_Arrivals))
    list(long=long, wide=wide)


  })

  #Create a graph with all the values from the inputs
  output$graph1 <- renderChart2({

    long <- pred_data()[["long"]]
    econ <- transform(long, date = as.character(Date))
    #econ <- transform(long, hate = as.character(Algorithm_1))
    m1 <- mPlot(x = "date", y = c("Actual_Arrivals","Algorithm_1", "Algorithm_2", "Algorithm_3"), type = "Line", data = econ)
    #m1$addParams(height = 500, dom = 'graph1')
    #m1$set(xLabelFormat = "Date")
    m1$set(lineColors=c("blue", "red", "purple", "#FF00FF"))
    return(m1)
  })

}

)
