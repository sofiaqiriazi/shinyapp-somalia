

Normal_arrivals <- function(start, end){
  start = 16

  PI <- PA <- PD <- rep(NA, end)

  for (t in start:end){

    A <-3.53137937289135*mean(before.long[(t-16):(t),"Shabeellaha_Dhexe_BeforeRegion"], na.rm=TRUE)
    B <- 0.937064684594128*current.long[(t),"Awdal_CurrentRegion"]*fatalities.long[(t-9),"Togdheer_Fatalities"]
    C <- before.long[(t),"Shabeellaha_Hoose_BeforeRegion"]
    D <- 0.905517931656921*before.long[(t),"Banadir_BeforeRegion"]
    E <- before.long[(t),"Bay_BeforeRegion"]
    MAXC <- max(sum(C , D, na.rm=TRUE), E, na.rm=TRUE)
    G <- future.long[(t-1),"Shabeellaha_Hoose_FutureRegion"]
    K <- current.long[(t),"Shabeellaha_Hoose_CurrentRegion"]
    L <- mean(before.long[(t-13):(t),"Banadir_BeforeRegion"], na.rm=TRUE)
    M <- -1977.20111909046
    N <- -current.long[(t),"Shabeellaha_Hoose_CurrentRegion"]
    MAXK <- max(sum(K, L, na.rm=TRUE), MAXC, na.rm=TRUE)
    MAXG <- max(G, MAXK, na.rm=TRUE)
    P <- 1.01862081296852*max(MAXC, MAXG, na.rm=TRUE)
    FIN <- sum(A , B , P , M , N, na.rm=TRUE)

    PA[t] <- FIN


    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0

  }

  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)

  return(PA)
 }


modelarrivals_BNminus1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    C<- tail(movavg(fatalities.long[(t-4):(t- 1),"Mudug_Fatalities"], 3,type="m"),1)
    D<- before.long[(t- 1),"Mudug_BeforeRegion"]
    E<- tail(movavg(future.long[(t-3):(t- 1),"Galgaduud_FutureRegion"], 2,type="m"),1)
    G<- conflicts.long[(t- 1),"Bari_Conflict"]
    H<- before.long[(t- 1),"Banadir_BeforeRegion"]
    I<- before.long[(t- 1),"Bay_BeforeRegion"]
    J<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    K<- current.long[(t- 16),"Awdal_CurrentRegion"]
    L<- tail(movavg(before.long[(t-15):(t- 1),"Banadir_BeforeRegion"],14,type="w"),1)
    M<- fatalities.long[(t- 1),"Gedo_Fatalities"]
    N<- tail(movavg(before.long[(t-9):(t- 1),"Togdheer_BeforeRegion"],8,type="w"),1)
    O<- max(I, 0.217924425016599*J,na.rm=TRUE)
    P<- max(O, K,na.rm=TRUE)
    Q<- P%% 0.0993623825871936
    R<- tan(M)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 0.217924425016599*A , 0.0790546122710993*B*C , 0.00835587601047044*D*E , G*H*Q , L , -R*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BNminus2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 5),"Bari_BeforeRegion"]
    B<- median(future.long[(t-5):(t- 2),"Nugaal_FutureRegion"], na.rm=TRUE)
    C<- current.long[(t- 9),"Woqooyi_Galbeed_CurrentRegion"]
    D<- tail(movavg(conflicts.long[(t-5):(t- 2),"Togdheer_Conflict"],3,type="w"),1)
    E<- before.long[(t- 12),"Bari_BeforeRegion"]
    G<- tail(movavg(conflicts.long[(t-15):(t- 2),"Mudug_Conflict"],13,type="w"),1)
    H<- current.long[(t- 2),"Awdal_CurrentRegion"]
    I<- before.long[(t- 10),"Bari_BeforeRegion"]
    J<- before.long[(t- 2),"Jubbada_Dhexe_BeforeRegion"]
    K<- future.long[(t- 2),"Togdheer_FutureRegion"]
    L<- mean(fatalities.long[(t-5):(t- 2),"Sanaag_Fatalities"], na.rm=TRUE)
    M<- median(water.long[(t-11):(t- 2),"Galgaduud_WaterDrumPrice"], na.rm=TRUE)
    N<- goats.long[(t- 2),"Bakool_goatprice"]
    O<- sqrt(K)
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
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( A*B , C*D , E*G , 0.124876457423035*H*I , J*O*L , M , -9926.08337829581 , -0.00968438665196099*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BNJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    C<- conflicts.long[(t- 1),"Bari_Conflict"]
    D<- tail(movavg(before.long[(t-4):(t- 1),"Banadir_BeforeRegion"], 3,type="m"),1)
    E<- mean(fatalities.long[(t-4):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    G<- tail(movavg(future.long[(t-6):(t- 1),"Gedo_FutureRegion"],5,type="w"),1)
    H<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    I<- median(before.long[(t-8):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    J<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    K<- before.long[(t- 1),"Bay_BeforeRegion"]
    L<- conflicts.long[(t- 1),"Bari_Conflict"]
    M<- tail(movavg(before.long[(t-4):(t- 1),"Banadir_BeforeRegion"], 3,type="m"),1)
    N<- mean(fatalities.long[(t-4):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    O<- tail(movavg(future.long[(t-6):(t- 1),"Gedo_FutureRegion"],5,type="w"),1)
    P<- water.long[(t- 1),"Hiiraan_WaterDrumPrice"]
    Q<- before.long[(t- 1),"Mudug_BeforeRegion"]
    R<- (sum(0.0868703284495857*J*K , 0.0819722756534686*L*M , 3.20101567484822*N*O,na.rm=TRUE))
    S<- max(0.586149473609031*P, Q,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(S)){S <- 0 }
    FIN <-sum( 0.0868703284495857*A*B , 0.0819722756534686*C*D , 3.20101567484822*E*G , 4.89025816928898*H*I/R , S , -8523.16781354588,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BNJUN2arrivals <- function(start, end){
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}
modelarrivals_BNJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    B<- current.long[(t- 1),"Nugaal_CurrentRegion"]
    C<- before.long[(t- 1),"Mudug_BeforeRegion"]
    D<- water.long[(t- 1),"Bay_WaterDrumPrice"]
    E<- current.long[(t- 4),"Sool_CurrentRegion"]
    G<- conflicts.long[(t- 6),"Awdal_Conflict"]
    H<- before.long[(t- 6),"Shabeellaha_Dhexe_BeforeRegion"]
    I<- fatalities.long[(t- 9),"Sanaag_Fatalities"]
    J<- future.long[(t- 1),"Nugaal_FutureRegion"]
    K<- before.long[(t- 8),"Bay_BeforeRegion"]
    L<- mean(before.long[(t-4):(t- 1),"Banadir_BeforeRegion"], na.rm=TRUE)
    M<- conflicts.long[(t- 6),"Awdal_Conflict"]
    N<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    O<- current.long[(t- 1),"Nugaal_CurrentRegion"]
    P<- before.long[(t- 1),"Mudug_BeforeRegion"]
    Q<- exp(M)
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
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( 0.00644970554752389*A*B , 0.000232134354975325*C*D , E*G , H*I , J , K , L , Q , -N , -O , -4.81297710245971*P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BNJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    B<- current.long[(t- 1),"Nugaal_CurrentRegion"]
    C<- before.long[(t- 1),"Mudug_BeforeRegion"]
    D<- water.long[(t- 1),"Bay_WaterDrumPrice"]
    E<- current.long[(t- 4),"Sool_CurrentRegion"]
    G<- conflicts.long[(t- 6),"Awdal_Conflict"]
    H<- before.long[(t- 6),"Shabeellaha_Dhexe_BeforeRegion"]
    I<- fatalities.long[(t- 9),"Sanaag_Fatalities"]
    J<- future.long[(t- 1),"Nugaal_FutureRegion"]
    K<- before.long[(t- 8),"Bay_BeforeRegion"]
    L<- mean(before.long[(t-4):(t- 1),"Banadir_BeforeRegion"], na.rm=TRUE)
    M<- conflicts.long[(t- 6),"Awdal_Conflict"]
    N<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    O<- current.long[(t- 1),"Nugaal_CurrentRegion"]
    P<- before.long[(t- 1),"Mudug_BeforeRegion"]
    Q<- exp(M)
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
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( 0.00644968999560936*A*B , 0.000232134354975325*C*D , E*G , H*I , J , K , L , Q , -N , -O , -4.81297710245971*P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BNJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Mudug_BeforeRegion"]
    B<- water.long[(t- 1),"Hiiraan_WaterDrumPrice"]
    C<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    D<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    E<- median(rain.long[(t-15):(t- 1),"Gedo_rain"], na.rm=TRUE)
    G<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    H<- before.long[(t- 1),"Bay_BeforeRegion"]
    I<- conflicts.long[(t- 1),"Bari_Conflict"]
    J<- before.long[(t- 1),"Banadir_BeforeRegion"]
    K<- future.long[(t- 1),"Jubbada_Dhexe_FutureRegion"]
    L<- water.long[(t- 7),"Nugaal_WaterDrumPrice"]
    M<- before.long[(t- 1),"Awdal_BeforeRegion"]
    N<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    O<- max(A,sum( 0.464844464717392*B , 0.156038424901233*C , D*E , 0.0936984664666861*G*H , 0.0767994494620685*I*J , -K,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( O , -L , -M*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BNJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- water.long[(t- 1),"Hiiraan_WaterDrumPrice"]
    B<- conflicts.long[(t- 1),"Bari_Conflict"]
    C<- before.long[(t- 1),"Banadir_BeforeRegion"]
    D<- mean(fatalities.long[(t-3):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    E<- tail(movavg(future.long[(t-6):(t- 1),"Gedo_FutureRegion"], 5,type="m"),1)
    G<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    H<- current.long[(t- 3),"Nugaal_CurrentRegion"]
    I<- conflicts.long[(t- 1),"Bari_Conflict"]
    J<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    K<- tail(movavg(before.long[(t-3):(t- 1),"Bay_BeforeRegion"], 2,type="m"),1)
    L<- tail(movavg(future.long[(t-6):(t- 1),"Gedo_FutureRegion"], 5,type="m"),1)
    M<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
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
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 0.500464772441319*A , 0.0773220425411127*B*C , D*E , 0.0134917409605303*G*H , 0.00795798570650005*I*J*K , L , -5721.92313728203 , -2.44915092407495*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BNJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 15),"Shabeellaha_Dhexe_BeforeRegion"]
    B<- tail(movavg(before.long[(t-7):(t- 1),"Mudug_BeforeRegion"], 6,type="m"),1)
    C<- median(before.long[(t-16):(t- 1),"Banadir_BeforeRegion"], na.rm=TRUE)
    D<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    E<- before.long[(t- 1),"Bay_BeforeRegion"]
    G<- conflicts.long[(t- 1),"Bari_Conflict"]
    H<- before.long[(t- 1),"Banadir_BeforeRegion"]
    I<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    J<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    K<- conflicts.long[(t- 1),"Bari_Conflict"]
    L<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    M<- before.long[(t- 1),"Bay_BeforeRegion"]
    N<- before.long[(t- 1),"Bay_BeforeRegion"]
    O<- tail(movavg(current.long[(t-4):(t- 1),"Hiiraan_CurrentRegion"], 3,type="m"),1)
    P<- tail(movavg(before.long[(t-10):(t- 1),"Gedo_BeforeRegion"], 9,type="m"),1)
    Q<- sin(0.0963804584498508*J)
    R<- cos(K)
    if ( is.na(R) ){S<- N}
    else if(R>0){S<- 0.0745988338790564*L*M }
    else{S<- N }
    U<- max(sum(0.0745988338790564*D*E , 0.0679902969578057*G*H , -I*Q*S,na.rm=TRUE), 0.00110933805415979*O*P,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(U)){U <- 0 }
    FIN <-sum( A , B , C , U,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BNJUN8arrivals <- function(start, end){
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
    P<- max(sum(0.525731462646251*A , 0.164551229335035*B , C*D , 0.0772156733729328*E*G , H , I , O , -6252.41542789858,na.rm=TRUE), N)
    FIN <-P
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BNJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Mudug_BeforeRegion"]
    B<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    C<- conflicts.long[(t- 1),"Bari_Conflict"]
    D<- tail(movavg(before.long[(t-4):(t- 1),"Banadir_BeforeRegion"], 3,type="m"),1)
    E<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    G<- median(before.long[(t-6):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    H<- before.long[(t- 1),"Bay_BeforeRegion"]
    I<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    J<- median(before.long[(t-6):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    K<- tail(movavg(fatalities.long[(t-4):(t- 1),"Mudug_Fatalities"], 3,type="m"),1)
    L<- future.long[(t- 8),"Nugaal_FutureRegion"]
    M<- tail(movavg(goats.long[(t-8):(t- 1),"Gedo_goatprice"], 7,type="m"),1)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BNJUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Bakool_FutureRegion"]
    B<- stations.long[(t- 3),"Gedo_DollowStation_Juba_River"]
    C<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    D<- before.long[(t- 1),"Bay_BeforeRegion"]
    E<- before.long[(t- 8),"Bay_BeforeRegion"]
    G<- stations.long[(t- 9),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
    H<- before.long[(t- 6),"Shabeellaha_Dhexe_BeforeRegion"]
    I<- future.long[(t- 1),"Nugaal_FutureRegion"]
    J<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    K<- current.long[(t- 3),"Bari_CurrentRegion"]
    L<- mean(before.long[(t-4):(t- 1),"Banadir_BeforeRegion"], na.rm=TRUE)
    M<- tail(movavg(before.long[(t-3):(t- 1),"Mudug_BeforeRegion"],2,type="w"),1)
    N<- future.long[(t- 1),"Nugaal_FutureRegion"]
    O<- tail(movavg(before.long[(t-4):(t- 1),"Hiiraan_BeforeRegion"], 3,type="m"),1)
    P<- median(current.long[(t-9):(t- 1),"Galgaduud_CurrentRegion"], na.rm=TRUE)
    Q<- atan2(I, J)
    R<- max(N, 0.33261235258023*O,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( A*B , 0.0695347098777455*C*D , E*G , H*Q , K , L , M , R , -P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

BN2016_1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    B<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    C<- before.long[(t- 1),"Bay_BeforeRegion"]
    D<- before.long[(t- 8),"Mudug_BeforeRegion"]
    E<- median(stations.long[(t-12):(t- 1),"Gedo_BardheereStation_Juba_River"], na.rm=TRUE)
    G<- current.long[(t- 1),"Mudug_CurrentRegion"]
    H<- fatalities.long[(t- 8),"Mudug_Fatalities"]
    I<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    J<- before.long[(t- 4),"Woqooyi_Galbeed_BeforeRegion"]
    K<- water.long[(t- 2),"Nugaal_WaterDrumPrice"]
    L<- before.long[(t- 7),"Mudug_BeforeRegion"]
    M<- tail(movavg(current.long[(t-5):(t- 1),"Mudug_CurrentRegion"],4,type="w"),1)
    N<- mean(before.long[(t-7):(t- 1),"Jubbada_Hoose_BeforeRegion"], na.rm=TRUE)
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
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( A*B , 0.000200807705302517*C^2 , D*E , 0.31306112140274*G*H , 0.00820760964741239*I*J , K , L , M , -N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

BN2016_2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    B<- tail(movavg(before.long[(t-12):(t- 1),"Jubbada_Dhexe_BeforeRegion"], 11,type="m"),1)
    C<- before.long[(t- 1),"Bay_BeforeRegion"]
    D<- mean(future.long[(t-3):(t- 1),"Bakool_FutureRegion"], na.rm=TRUE)
    E<- tail(movavg(future.long[(t-4):(t- 1),"Galgaduud_FutureRegion"], 3,type="m"),1)
    G<- conflicts.long[(t- 7),"Shabeellaha_Hoose_Conflict"]
    H<- current.long[(t- 1),"Mudug_CurrentRegion"]
    I<- fatalities.long[(t- 8),"Mudug_Fatalities"]
    J<- rain.long[(t- 1),"Shabeellaha_Hoose_rain"]
    K<- tail(movavg(fatalities.long[(t-4):(t- 1),"Mudug_Fatalities"], 3,type="m"),1)
    L<- tail(movavg(current.long[(t-5):(t- 1),"Mudug_CurrentRegion"], 4,type="m"),1)
    M<- mean(before.long[(t-8):(t- 1),"Bakool_BeforeRegion"], na.rm=TRUE)
    if ( is.na(G) ){N<- 10567.6740556532}
    else if(G>0){N<- 0.709278555492349*H*I }
    else{N<- 10567.6740556532 }
    O<- max(0.0139804864130888*J*K*L, M,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 1622.20116610683 , 0.00316527761246905*A*B , C , D , E , N , O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

BN2016_3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    B<- future.long[(t- 1),"Mudug_FutureRegion"]
    C<- median(future.long[(t-10):(t- 1),"Sanaag_FutureRegion"], na.rm=TRUE)
    D<- mean(current.long[(t-4):(t- 1),"Nugaal_CurrentRegion"], na.rm=TRUE)
    E<- current.long[(t- 11),"Sanaag_CurrentRegion"]
    G<- before.long[(t- 8),"Mudug_BeforeRegion"]
    H<- tail(movavg(before.long[(t-6):(t- 1),"Shabeellaha_Dhexe_BeforeRegion"],5,type="w"),1)
    I<- tail(movavg(future.long[(t-5):(t- 1),"Bakool_FutureRegion"], 4,type="m"),1)
    J<- tail(movavg(before.long[(t-5):(t- 1),"Mudug_BeforeRegion"],4,type="w"),1)
    K<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    L<- tail(movavg(future.long[(t-5):(t- 1),"Bakool_FutureRegion"], 4,type="m"),1)
    M<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    N<- before.long[(t- 1),"Bay_BeforeRegion"]
    O<- mean(stations.long[(t-3):(t- 1),"Hiiraan_Belet_WeyneStation_Shabelle_River"], na.rm=TRUE)
    P<- max(D, E,na.rm=TRUE)
    Q<- max(I,sum( 1.49086051008592*J , K*L , M,na.rm=TRUE),na.rm=TRUE)
    R<- max(sum(0.00600776276208405*A*B , C*P , G , H , Q,na.rm=TRUE), N*O,na.rm=TRUE)
    FIN <-R
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

BN2016_4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    B<- future.long[(t- 1),"Mudug_FutureRegion"]
    C<- current.long[(t- 1),"Bay_CurrentRegion"]
    D<- current.long[(t- 1),"Mudug_CurrentRegion"]
    E<- rain.long[(t- 12),"Awdal_rain"]
    G<- tail(movavg(future.long[(t-7):(t- 1),"Bari_FutureRegion"], 6,type="m"),1)
    H<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    I<- mean(rain.long[(t-3):(t- 1),"Sool_rain"], na.rm=TRUE)
    J<- before.long[(t- 6),"Gedo_BeforeRegion"]
    K<- current.long[(t- 6),"Bakool_CurrentRegion"]
    L<- water.long[(t- 1),"Sanaag_WaterDrumPrice"]
    M<- goats.long[(t- 1),"Bakool_goatprice"]
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
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 11923.4177503595 , 0.00908013257062917*A*B , 0.00126110056829515*C*D , E*G , H^2*I , J , K , -2.79691886960393e-7*L*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

BN2016_5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    B<- future.long[(t- 1),"Mudug_FutureRegion"]
    C<- current.long[(t- 1),"Mudug_CurrentRegion"]
    D<- tail(movavg(current.long[(t-3):(t- 1),"Bay_CurrentRegion"], 2,type="m"),1)
    E<- fatalities.long[(t- 14),"Togdheer_Fatalities"]
    G<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    H<- before.long[(t- 5),"Sool_BeforeRegion"]
    I<- current.long[(t- 1),"Mudug_CurrentRegion"]
    J<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    K<- before.long[(t- 5),"Sool_BeforeRegion"]
    L<- current.long[(t- 1),"Mudug_CurrentRegion"]
    M<- fatalities.long[(t- 3),"Togdheer_Fatalities"]
    N<- mean(current.long[(t-9):(t- 1),"Bakool_CurrentRegion"], na.rm=TRUE)
    O<- before.long[(t- 1),"Bay_BeforeRegion"]
    P<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    Q<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    R<- median(stations.long[(t-9):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    if ( is.na(G) ){S<- I}
    else if(G>0){S<- H }
    else{S<- I }
    if ( is.na(J) ){U<- L}
    else if(J>0){U<- K }
    else{U<- L }
    V<- max(sum(2133.15346920017 , M*N , O,na.rm=TRUE), P*Q*R,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(V)){V <- 0 }
    FIN <-sum( 0.00869886908880646*A*B , 0.00141079579114611*C*D , E*S , U , V,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

BN2016_6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Bay_BeforeRegion"]
    B<- stations.long[(t- 1),"Gedo_BardheereStation_Juba_River"]
    C<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    D<- before.long[(t- 4),"Woqooyi_Galbeed_BeforeRegion"]
    E<- mean(fatalities.long[(t-4):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    G<- mean(future.long[(t-5):(t- 1),"Bakool_FutureRegion"], na.rm=TRUE)
    H<- tail(movavg(fatalities.long[(t-4):(t- 1),"Togdheer_Fatalities"],3,type="w"),1)
    I<- mean(current.long[(t-5):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    J<- before.long[(t- 1),"Banadir_BeforeRegion"]
    K<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    L<- before.long[(t- 8),"Mudug_BeforeRegion"]
    M<- mean(before.long[(t-5):(t- 1),"Shabeellaha_Dhexe_BeforeRegion"], na.rm=TRUE)
    if ( is.na(H) ){N<- J}
    else if(H>0){N<- 1.68930122973877*I }
    else{N<- J }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 0.742462964118337*A*B , 0.00861812092706172*C*D , 3.04665227768342*E*G , 1.38101065646056*N , K , L , M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

BN2016_7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- water.long[(t- 1),"Hiiraan_WaterDrumPrice"]
    B<- future.long[(t- 1),"Bari_FutureRegion"]
    C<- rain.long[(t- 1),"Shabeellaha_Hoose_rain"]
    D<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    E<- future.long[(t- 1),"Mudug_FutureRegion"]
    G<- before.long[(t- 1),"Bay_BeforeRegion"]
    H<- current.long[(t- 2),"Nugaal_CurrentRegion"]
    I<- median(rain.long[(t-13):(t- 1),"Bakool_rain"], na.rm=TRUE)
    J<- mean(future.long[(t-10):(t- 1),"Bay_FutureRegion"], na.rm=TRUE)
    K<- tail(movavg(before.long[(t-4):(t- 1),"Mudug_BeforeRegion"],3,type="w"),1)
    L<- water.long[(t- 1),"Jubbada_Dhexe_WaterDrumPrice"]
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
    FIN <-sum( 0.610768162533982*A , 1.25310811672934*B*C , 0.00735175796380813*D*E , 0.000245901009037608*G^2 , H*I , J , K , -0.300446179785276*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

BN2016_8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(current.long[(t-13):(t- 1),"Hiiraan_CurrentRegion"],12,type="w"),1)
    B<- tail(movavg(current.long[(t-6):(t- 1),"Mudug_CurrentRegion"],5,type="w"),1)
    C<- water.long[(t- 1),"Hiiraan_WaterDrumPrice"]
    D<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    E<- future.long[(t- 1),"Mudug_FutureRegion"]
    G<- conflicts.long[(t- 10),"Bari_Conflict"]
    H<- mean(before.long[(t-9):(t- 1),"Hiiraan_BeforeRegion"], na.rm=TRUE)
    I<- tail(movavg(current.long[(t-6):(t- 1),"Mudug_CurrentRegion"],5,type="w"),1)
    J<- conflicts.long[(t- 10),"Bari_Conflict"]
    K<- tail(movavg(before.long[(t-9):(t- 1),"Mudug_BeforeRegion"], 8,type="m"),1)
    L<- before.long[(t- 1),"Bay_BeforeRegion"]
    M<- B%% 1.49454227443297
    if ( is.na(762) || is.na( I)){N<-0}
    else if(762<= I){N<-1 }
    else{N<-0 }
    O<- max(sum(0.429965160320424*C , 0.00845149074747908*D*E , G*H*N , J , K,na.rm=TRUE), 0.000302331222216253*L^2,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( A*M , O , -4398.3865877254,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

BN2016_9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(current.long[(t-3):(t- 1),"Bay_CurrentRegion"], 2,type="m"),1)
    B<- tail(movavg(current.long[(t-3):(t- 1),"Mudug_CurrentRegion"], 2,type="m"),1)
    C<- before.long[(t- 1),"Bay_BeforeRegion"]
    D<- conflicts.long[(t- 1),"Bay_Conflict"]
    E<- before.long[(t- 8),"Mudug_BeforeRegion"]
    G<- future.long[(t- 1),"Bakool_FutureRegion"]
    H<- tail(movavg(before.long[(t-8):(t- 1),"Banadir_BeforeRegion"],7,type="w"),1)
    I<- conflicts.long[(t- 1),"Bay_Conflict"]
    J<- before.long[(t- 8),"Mudug_BeforeRegion"]
    K<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    L<- future.long[(t- 1),"Mudug_FutureRegion"]
    M<- current.long[(t- 13),"Mudug_CurrentRegion"]
    N<- tail(movavg(future.long[(t-5):(t- 1),"Jubbada_Hoose_FutureRegion"], 4,type="m"),1)
    O<- sin(sum(3.56369094003652 , 83.89958770886*D , 3.56369094003652*E,na.rm=TRUE))
    P<- max(sum(3.56369094003652 , 83.89958770886*I , 3.56369094003652*J,na.rm=TRUE), 0.00915979216913503*K*L,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 0.00151381961416553*A*B , C*O , G , H , P , -M , -N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

BN2016_10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    B<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    C<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    D<- stations.long[(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"]
    E<- mean(current.long[(t-4):(t- 1),"Nugaal_CurrentRegion"], na.rm=TRUE)
    G<- mean(rain.long[(t-14):(t- 1),"Nugaal_rain"], na.rm=TRUE)
    H<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    I<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    J<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    K<- tail(movavg(future.long[(t-8):(t- 1),"Bakool_FutureRegion"], 7,type="m"),1)
    L<- tail(movavg(current.long[(t-5):(t- 1),"Mudug_CurrentRegion"],4,type="w"),1)
    M<- before.long[(t- 1),"Bay_BeforeRegion"]
    N<- mean(stations.long[(t-3):(t- 1),"Hiiraan_Belet_WeyneStation_Shabelle_River"], na.rm=TRUE)
    O<- current.long[(t- 2),"Mudug_CurrentRegion"]
    P<- mean(current.long[(t-11):(t- 1),"Jubbada_Hoose_CurrentRegion"], na.rm=TRUE)
    Q<- (0.00655271490899786*C)
    R<- max(sum(4342.95351929502 , H*I , J*K , L,na.rm=TRUE),sum( M*N , -O,na.rm=TRUE),na.rm=TRUE)
    S<- max(E*G, R,na.rm=TRUE)
    U<- max(sum(4342.95351929502 , A*B , Q^D,na.rm=TRUE), S,na.rm=TRUE)
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( 1.33892029467172*U , -P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Bari_Fatalities"]
    B<- before.long[(t- 1),"Mudug_BeforeRegion"]
    C<- future.long[(t- 1),"Woqooyi_Galbeed_FutureRegion"]
    D<- mean(current.long[(t-4):(t- 1),"Nugaal_CurrentRegion"], na.rm=TRUE)
    E<- current.long[(t- 1),"Gedo_CurrentRegion"]
    G<- current.long[(t- 1),"Bari_CurrentRegion"]
    H<- fatalities.long[(t- 1),"Gedo_Fatalities"]
    I<- tail(movavg(fatalities.long[(t-8):(t- 1),"Mudug_Fatalities"], 7,type="m"),1)
    J<- median(rain.long[(t-9):(t- 1),"Bakool_rain"], na.rm=TRUE)
    K<- tail(movavg(before.long[(t-13):(t- 1),"Hiiraan_BeforeRegion"],12,type="w"),1)
    L<- current.long[(t- 1),"Gedo_CurrentRegion"]
    M<- tail(movavg(water.long[(t-5):(t- 1),"Bakool_WaterDrumPrice"],4,type="w"),1)
    N<- sqrt(C)
    O<- floor(0.00207485985528903*G)
    P<- max(L, 0.442050688064606*M,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( 0.0932582045922656*A*B , N*D , E*O , H*I*J , K , P , -9060.67992987461,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    B<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    C<- future.long[(t- 1),"Mudug_FutureRegion"]
    D<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    E<- tail(movavg(before.long[(t-3):(t- 1),"Mudug_BeforeRegion"], 2,type="m"),1)
    G<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    H<- future.long[(t- 14),"Mudug_FutureRegion"]
    I<- tail(movavg(water.long[(t-12):(t- 1),"Bakool_WaterDrumPrice"],11,type="w"),1)
    J<- current.long[(t- 1),"Gedo_CurrentRegion"]
    K<- current.long[(t- 1),"Bari_CurrentRegion"]
    L<- round(-0.00103838434135449*K)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 0.236728395075358*A , 0.00370133425867487*B*C , 0.0116396096993031*D*E , G , H , I , -19896.2179632592 , -1.9071651571991*J*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 3),"Sanaag_Conflict"]
    B<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    C<- rivers.long[(t- 11),"Juba_River_discharge"]
    D<- current.long[(t- 1),"Gedo_CurrentRegion"]
    E<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    G<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    H<- stations.long[(t- 2),"Gedo_DollowStation_Juba_River"]
    I<- conflicts.long[(t- 9),"Hiiraan_Conflict"]
    J<- current.long[(t- 9),"Togdheer_CurrentRegion"]
    K<- before.long[(t- 1),"Bay_BeforeRegion"]
    L<- mean(before.long[(t-3):(t- 1),"Mudug_BeforeRegion"], na.rm=TRUE)
    M<- water.long[(t- 7),"Galgaduud_WaterDrumPrice"]
    N<- tail(movavg(goats.long[(t-16):(t- 1),"Jubbada_Hoose_goatprice"], 15,type="m"),1)
    O<- tail(movavg(before.long[(t-3):(t- 1),"Togdheer_BeforeRegion"], 2,type="m"),1)
    if ( is.na(B) ){P<- D}
    else if(B>0){P<- C }
    else{P<- D }
    Q<- max(sum(0.694684361700432*E , G*H , I*J , K , L,na.rm=TRUE), M,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( A*P , Q , -0.016201409990671*N , -5.90971441685906*O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    B<- before.long[(t- 1),"Mudug_BeforeRegion"]
    C<- current.long[(t- 1),"Gedo_CurrentRegion"]
    D<- future.long[(t- 15),"Jubbada_Hoose_FutureRegion"]
    E<- tail(movavg(fatalities.long[(t-11):(t- 1),"Togdheer_Fatalities"], 10,type="m"),1)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Bay_BeforeRegion"]
    B<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    C<- before.long[(t- 1),"Mudug_BeforeRegion"]
    D<- current.long[(t- 1),"Gedo_CurrentRegion"]
    E<- fatalities.long[(t- 3),"Sanaag_Fatalities"]
    G<- tail(movavg(stations.long[(t-15):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"],14,type="w"),1)
    H<- before.long[(t- 1),"Bay_BeforeRegion"]
    I<- before.long[(t- 1),"Mudug_BeforeRegion"]
    J<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    K<- median(before.long[(t-5):(t- 1),"Galgaduud_BeforeRegion"], na.rm=TRUE)
    L<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    M<- mean(goats.long[(t-4):(t- 1),"Gedo_goatprice"], na.rm=TRUE)
    N<- min(H, I,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 0.656699238779318*A , 0.656699238779318*B , 0.494683614858048*C , 0.751616373213706*D*E , G*N , J , K , -3.83040093927554*L , -0.011783920876017*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Mudug_BeforeRegion"]
    B<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    C<- before.long[(t- 1),"Gedo_BeforeRegion"]
    D<- mean(conflicts.long[(t-16):(t- 1),"Jubbada_Dhexe_Conflict"], na.rm=TRUE)
    E<- future.long[(t- 1),"Bari_FutureRegion"]
    G<- water.long[(t- 4),"Togdheer_WaterDrumPrice"]
    H<- before.long[(t- 1),"Mudug_BeforeRegion"]
    I<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    J<- tail(movavg(rain.long[(t-12):(t- 1),"Gedo_rain"],11,type="w"),1)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Bari_FutureRegion"]
    B<- rain.long[(t- 1),"Shabeellaha_Hoose_rain"]
    C<- current.long[(t- 1),"Bari_CurrentRegion"]
    D<- current.long[(t- 1),"Gedo_CurrentRegion"]
    E<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    G<- before.long[(t- 1),"Mudug_BeforeRegion"]
    H<- tail(movavg(future.long[(t-5):(t- 1),"Galgaduud_FutureRegion"], 4,type="m"),1)
    I<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    J<- median(before.long[(t-6):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    K<- current.long[(t- 6),"Bakool_CurrentRegion"]
    L<- future.long[(t- 14),"Mudug_FutureRegion"]
    M<- mean(before.long[(t-8):(t- 1),"Jubbada_Hoose_BeforeRegion"], na.rm=TRUE)
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
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( A*B , 0.00197058875505935*C*D , 8.48543352535101e-6*E^2 , 0.0120431601069705*G*H , 0.00111435882011818*I*J , K , L , -M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- water.long[(t- 1),"Bay_WaterDrumPrice"]
    B<- before.long[(t- 1),"Mudug_BeforeRegion"]
    C<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    D<- current.long[(t- 1),"Bari_CurrentRegion"]
    E<- current.long[(t- 1),"Gedo_CurrentRegion"]
    G<- median(fatalities.long[(t-9):(t- 1),"Shabeellaha_Dhexe_Fatalities"], na.rm=TRUE)
    H<- median(current.long[(t-14):(t- 1),"Jubbada_Dhexe_CurrentRegion"], na.rm=TRUE)
    I<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    J<- tail(movavg(conflicts.long[(t-4):(t- 1),"Gedo_Conflict"],3,type="w"),1)
    K<- current.long[(t- 1),"Bari_CurrentRegion"]
    L<- current.long[(t- 1),"Gedo_CurrentRegion"]
    M<- water.long[(t- 1),"Bay_WaterDrumPrice"]
    N<- future.long[(t- 3),"Nugaal_FutureRegion"]
    O<- mean(current.long[(t-10):(t- 1),"Gedo_CurrentRegion"], na.rm=TRUE)
    P<- goats.long[(t- 1),"Jubbada_Hoose_goatprice"]
    Q<- atan2(0.000977607114690172*K*L, M)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( 0.371143649329236*A , 0.0092141903043108*B*C , 0.000977607114690172*D*E , G*H , I*J*Q , N , O , -0.0092141903043108*P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Mudug_BeforeRegion"]
    B<- current.long[(t- 1),"Gedo_CurrentRegion"]
    C<- fatalities.long[(t- 3),"Sanaag_Fatalities"]
    D<- current.long[(t- 3),"Nugaal_CurrentRegion"]
    E<- tail(movavg(rain.long[(t-12):(t- 1),"Bari_rain"],11,type="w"),1)
    G<- future.long[(t- 3),"Nugaal_FutureRegion"]
    H<- future.long[(t- 4),"Nugaal_FutureRegion"]
    I<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    J<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    K<- tail(movavg(fatalities.long[(t-3):(t- 1),"Shabeellaha_Dhexe_Fatalities"],2,type="w"),1)
    L<- before.long[(t- 1),"Mudug_BeforeRegion"]
    M<- mean(goats.long[(t-5):(t- 1),"Gedo_goatprice"], na.rm=TRUE)
    N<- max(6329.56723946874, I,na.rm=TRUE)
    O<- max(sum(0.490846430861034*J , 130.37533697164*K,na.rm=TRUE), L,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 0.000400206821442192*A*B , C*D*E , G , H , N , O , -0.0152068781052589*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}


