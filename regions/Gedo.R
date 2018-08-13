
Normalarrivals <- function(start, end){
  start = 20
  
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- stations.long[(t- 9),"Juba_Dhexe_BualleStation_Juba_River"]
    B<- conflicts.long[(t- 6),"Woqooyi_Galbeed_Conflict"]
    C<- median(before.long[(t-14):(t),"Jubbada_Hoose_BeforeRegion"])
    D<- median(future.long[(t-7):(t),"Jubbada_Hoose_FutureRegion"])
    E<- mean(future.long[(t-9):(t),"Sool_FutureRegion"])
    G<- abs(sum(before.long[(t),"Gedo_BeforeRegion"] , water.long[(t),"Mudug_WaterDrumPrice"] , 1.88605295927384*water.long[(t),"Shabeallaha_Dhexe_WaterDrumPrice"] , 0.0926850555485351*future.long[t,"Hiiraan_FutureRegion"] , -43366.9525919461,na.rm=TRUE))
    H<- max(D,sum( 0.362274886121291*current.long[t,"Jubbada_Hoose_CurrentRegion"] , A*E , G , -883.028267171524,na.rm=TRUE),na.rm=TRUE)
    I<- max(C, H,na.rm=TRUE)
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(B)){B <- 0 }
    FIN <-sum( I , -before.long[t,"Awdal_BeforeRegion"]*B,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_GEminus1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- conflicts.long[(t- 4),"Bay_Conflict"]
    C<- conflicts.long[(t- 1),"Bay_Conflict"]
    D<- conflicts.long[(t- 4),"Bay_Conflict"]
    E<- conflicts.long[(t- 12),"Woqooyi_Galbeed_Conflict"]
    G<- rain.long[(t- 1),"Awdal_rain"]
    H<- future.long[(t- 8),"Nugaal_FutureRegion"]
    I<- median(before.long[(t-17):(t- 1),"Bay_BeforeRegion"], na.rm=TRUE)
    J<- before.long[(t- 15),"Sool_BeforeRegion"]
    K<- rain.long[(t- 5),"Mudug_rain"]
    L<- fatalities.long[(t- 15),"Bari_Fatalities"]
    M<- rain.long[(t- 15),"Jubbada_Hoose_rain"]
    N<- rain.long[(t- 16),"Jubbada_Hoose_rain"]
    O<- rain.long[(t- 9),"Bakool_rain"]
    P<- mean(fatalities.long[(t-16):(t- 1),"Hiiraan_Fatalities"], na.rm=TRUE)
    if ( is.na(B) || is.na( C)){Q<-0}
    else if(B< C){Q<-1 }
    else{Q<-0 }
    R<- log(E)
    S<- atan2(D, R)
    U<- max(0.710742064894768*J, K*L,na.rm=TRUE)
    V<- max(U, M*N,na.rm=TRUE)
    W<- max(V, O*P,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(W)){W <- 0 }
    FIN <-sum( A*Q*S , G , H , I , W,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GEminus2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(before.long[(t-17):(t- 2),"Bay_BeforeRegion"], na.rm=TRUE)
    B<- rain.long[(t- 15),"Jubbada_Hoose_rain"]
    C<- rain.long[(t- 16),"Jubbada_Hoose_rain"]
    D<- tail(movavg(before.long[(t-7):(t- 2),"Shabeellaha_Dhexe_BeforeRegion"], 5,type="m"),1)
    E<- rain.long[(t- 12),"Hiiraan_rain"]
    G<- rain.long[(t- 16),"Jubbada_Dhexe_rain"]
    H<- fatalities.long[(t- 2),"Sool_Fatalities"]
    I<- future.long[(t- 2),"Mudug_FutureRegion"]
    J<- mean(before.long[(t-5):(t- 2),"Bay_BeforeRegion"], na.rm=TRUE)
    K<- before.long[(t- 2),"Nugaal_BeforeRegion"]
    L<- before.long[(t- 16),"Sool_BeforeRegion"]
    M<- before.long[(t- 2),"Nugaal_BeforeRegion"]
    N<- median(rain.long[(t-10):(t- 2),"Nugaal_rain"], na.rm=TRUE)
    O<- mean(before.long[(t-5):(t- 2),"Bay_BeforeRegion"], na.rm=TRUE)
    P<- before.long[(t- 2),"Nugaal_BeforeRegion"]
    Q<- before.long[(t- 16),"Sool_BeforeRegion"]
    R<- and(E, G)
    S<- max(B*C, D*R,na.rm=TRUE)
    U<- sin(2.73489181315727*K)
    V<- max(J*U, L,na.rm=TRUE)
    W<- sin(2.73489181315727*P)
    X<- max(O*W, Q,na.rm=TRUE)
    Y<- max(V,sum( M*N , -X,na.rm=TRUE),na.rm=TRUE)
    Z<- max(0.197684536503192*H*I, 1.31950906308924*Y,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(Z)){Z <- 0 }
    FIN <-sum( A , S , Z,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GEJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- conflicts.long[(t- 1),"Mudug_Conflict"]
    C<- rain.long[(t- 4),"Nugaal_rain"]
    D<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    E<- median(before.long[(t-11):(t- 1),"Bay_BeforeRegion"], na.rm=TRUE)
    G<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    H<- rain.long[(t- 6),"Woqooyi_Galbeed_rain"]
    I<- future.long[(t- 1),"Woqooyi_Galbeed_FutureRegion"]
    J<- median(before.long[(t-8):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    K<- current.long[(t- 1),"Mudug_CurrentRegion"]
    L<- tail(movavg(conflicts.long[(t-4):(t- 1),"Awdal_Conflict"], 3,type="m"),1)
    M<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    N<- median(before.long[(t-6):(t- 1),"Galgaduud_BeforeRegion"], na.rm=TRUE)
    O<- asinh(D)
    if ( is.na(G) || is.na( H)){P<-0}
    else if(G>= H){P<-1 }
    else{P<-0 }
    Q<- max(sum(1.28508944753368*A , B*C*O , E*P , I , J,na.rm=TRUE), 0.0681329145065042*K*L,na.rm=TRUE)
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 1.24051275849806*Q , -M , -N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GEJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rain.long[(t- 4),"Jubbada_Dhexe_rain"]
    B<- median(conflicts.long[(t-10):(t- 1),"Mudug_Conflict"], na.rm=TRUE)
    C<- rain.long[(t- 4),"Jubbada_Dhexe_rain"]
    D<- median(conflicts.long[(t-10):(t- 1),"Mudug_Conflict"], na.rm=TRUE)
    E<- tail(movavg(rain.long[(t-3):(t- 1),"Bari_rain"], 2,type="m"),1)
    G<- current.long[(t- 7),"Nugaal_CurrentRegion"]
    H<- future.long[(t- 4),"Bari_FutureRegion"]
    I<- median(conflicts.long[(t-10):(t- 1),"Mudug_Conflict"], na.rm=TRUE)
    J<- conflicts.long[(t- 6),"Woqooyi_Galbeed_Conflict"]
    K<- future.long[(t- 6),"Togdheer_FutureRegion"]
    L<- tail(movavg(before.long[(t-5):(t- 1),"Jubbada_Dhexe_BeforeRegion"],4,type="w"),1)
    M<- median(rain.long[(t-4):(t- 1),"Woqooyi_Galbeed_rain"], na.rm=TRUE)
    N<- median(before.long[(t-11):(t- 1),"Bay_BeforeRegion"], na.rm=TRUE)
    O<- median(future.long[(t-17):(t- 1),"Shabeallaha_Dhexe_FutureRegion"], na.rm=TRUE)
    P<- conflicts.long[(t- 1),"Awdal_Conflict"]
    Q<- current.long[(t- 1),"Mudug_CurrentRegion"]
    if ( is.na(C) ){R<- E}
    else if(C>0){R<- D }
    else{R<- E }
    S<- atan(R)
    U<- log(I)
    V<- tan(M)
    W<- max(H*U,sum( J*K , L*V,na.rm=TRUE),na.rm=TRUE)
    X<- max(G, W,na.rm=TRUE)
    Y<- max(sum(A*B , S*X , N , -O,na.rm=TRUE), 0.0782256649051841*P*Q,na.rm=TRUE)
    FIN <-Y
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GEJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- median(before.long[(t-12):(t- 1),"Bay_BeforeRegion"], na.rm=TRUE)
    C<- before.long[(t- 15),"Sool_BeforeRegion"]
    D<- future.long[(t- 7),"Sanaag_FutureRegion"]
    E<- conflicts.long[(t- 15),"Galgaduud_Conflict"]
    G<- median(before.long[(t-11):(t- 1),"Bay_BeforeRegion"], na.rm=TRUE)
    H<- stations.long[(t- 6),"Shabelle_Dhexe_JowharStation_Shabelle_River"]
    I<- rain.long[(t- 15),"Jubbada_Hoose_rain"]
    J<- rain.long[(t- 16),"Jubbada_Hoose_rain"]
    K<- rain.long[(t- 6),"Woqooyi_Galbeed_rain"]
    L<- fatalities.long[(t- 1),"Jubbada_Hoose_Fatalities"]
    if ( is.na(H) || is.na( 3.01920831648604)){M<-0}
    else if(H< 3.01920831648604){M<-1 }
    else{M<-0 }
    N<- not(K)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 1.52191098816997*A , 0.736956416409494*B , 0.701859930495416*C , D*E , G*M , 0.895177831486283*I*J*N , L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GEJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(rain.long[(t-4):(t- 1),"Nugaal_rain"], 3,type="m"),1)
    B<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    C<- rain.long[(t- 4),"Nugaal_rain"]
    D<- fatalities.long[(t- 1),"Banaadir_Fatalities"]
    E<- rain.long[(t- 4),"Nugaal_rain"]
    G<- tail(movavg(rain.long[(t-4):(t- 1),"Nugaal_rain"], 3,type="m"),1)
    H<- current.long[(t- 1),"Awdal_CurrentRegion"]
    I<- tail(movavg(rain.long[(t-4):(t- 1),"Nugaal_rain"], 3,type="m"),1)
    J<- tail(movavg(rain.long[(t-4):(t- 1),"Nugaal_rain"], 3,type="m"),1)
    K<- tail(movavg(rain.long[(t-4):(t- 1),"Nugaal_rain"], 3,type="m"),1)
    L<- median(before.long[(t-11):(t- 1),"Bay_BeforeRegion"], na.rm=TRUE)
    M<- conflicts.long[(t- 1),"Awdal_Conflict"]
    N<- current.long[(t- 1),"Mudug_CurrentRegion"]
    O<- tan(sum(18.8473259302183 , 1.18696860208996*G,na.rm=TRUE))
    if ( is.na(I) || is.na( 1.18696860208996)){P<-0}
    else if(I>= 1.18696860208996){P<-1 }
    else{P<-0 }
    Q<- tan(sum(18.8473259302183 , 1.18696860208996*J,na.rm=TRUE))
    R<- atan2(P, Q)
    S<- tan(sum(18.8473259302183 , 1.18696860208996*K,na.rm=TRUE))
    U<- atan2(R, S)
    V<- max(sum(18.8473259302183 , 1.18696860208996*A , B*C , 0.495681519530668*D*E , 7.1497731905703*O , H*U , L,na.rm=TRUE), 0.0782250034346667*M*N,na.rm=TRUE)
    FIN <-V
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GEJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rain.long[(t- 4),"Jubbada_Dhexe_rain"]
    B<- rain.long[(t- 4),"Jubbada_Dhexe_rain"]
    C<- median(conflicts.long[(t-9):(t- 1),"Mudug_Conflict"], na.rm=TRUE)
    D<- fatalities.long[(t- 1),"Jubbada_Hoose_Fatalities"]
    E<- current.long[(t- 7),"Nugaal_CurrentRegion"]
    G<- future.long[(t- 4),"Bari_FutureRegion"]
    H<- conflicts.long[(t- 6),"Woqooyi_Galbeed_Conflict"]
    I<- future.long[(t- 6),"Togdheer_FutureRegion"]
    J<- tail(movavg(before.long[(t-6):(t- 1),"Jubbada_Dhexe_BeforeRegion"],5,type="w"),1)
    K<- median(rain.long[(t-4):(t- 1),"Woqooyi_Galbeed_rain"], na.rm=TRUE)
    L<- median(before.long[(t-11):(t- 1),"Bay_BeforeRegion"], na.rm=TRUE)
    M<- rain.long[(t- 4),"Jubbada_Dhexe_rain"]
    N<- rain.long[(t- 4),"Jubbada_Dhexe_rain"]
    O<- median(conflicts.long[(t-9):(t- 1),"Mudug_Conflict"], na.rm=TRUE)
    P<- fatalities.long[(t- 1),"Jubbada_Hoose_Fatalities"]
    Q<- median(future.long[(t-17):(t- 1),"Shabeallaha_Dhexe_FutureRegion"], na.rm=TRUE)
    R<- conflicts.long[(t- 1),"Awdal_Conflict"]
    S<- current.long[(t- 1),"Mudug_CurrentRegion"]
    if ( is.na(A) ){U<- D}
    else if(A>0){U<- B*C }
    else{U<- D }
    V<- atan(U)
    W<- tan(K)
    X<- max(2.33305483249812*G,sum( H*I , J*W,na.rm=TRUE),na.rm=TRUE)
    Y<- max(E, X,na.rm=TRUE)
    if ( is.na(M) ){Z<- P}
    else if(M>0){Z<- N*O }
    else{Z<- P }
    AA<- max(sum(V*Y , L , Z , -Q,na.rm=TRUE), 0.078225664905184*R*S,na.rm=TRUE)
    FIN <-AA
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GEJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 5),"Sanaag_CurrentRegion"]
    B<- median(conflicts.long[(t-4):(t- 1),"Sool_Conflict"], na.rm=TRUE)
    C<- future.long[(t- 1),"Sool_FutureRegion"]
    D<- conflicts.long[(t- 11),"Galgaduud_Conflict"]
    E<- conflicts.long[(t- 9),"Galgaduud_Conflict"]
    G<- before.long[(t- 13),"Bari_BeforeRegion"]
    H<- current.long[(t- 1),"Awdal_CurrentRegion"]
    I<- fatalities.long[(t- 11),"Banaadir_Fatalities"]
    J<- conflicts.long[(t- 5),"Bakool_Conflict"]
    K<- before.long[(t- 14),"Jubbada_Dhexe_BeforeRegion"]
    L<- water.long[(t- 1),"Woqooyi_Galbeed_WaterDrumPrice"]
    M<- goats.long[(t- 1),"Woqooyi_Galbeed_goatprice"]
    N<- log(I)
    O<- min(J, 2.99885052234207,na.rm=TRUE)
    P<- O%% 2.56910658666049
    Q<- max(A*B,sum( 14361.5472807424 , C*D , E*G , H*N*P , K , -L , -0.0152535691004068*M,na.rm=TRUE),na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GEJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 15),"Sool_BeforeRegion"]
    B<- tail(movavg(fatalities.long[(t-7):(t- 1),"Woqooyi_Galbeed_Fatalities"], 6,type="m"),1)
    C<- median(before.long[(t-15):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    D<- median(fatalities.long[(t-10):(t- 1),"Jubbada_Hoose_Fatalities"], na.rm=TRUE)
    E<- future.long[(t- 1),"Sanaag_FutureRegion"]
    G<- conflicts.long[(t- 1),"Shabeellaha_Hoose_Conflict"]
    H<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    I<- current.long[(t- 1),"Awdal_CurrentRegion"]
    J<- fatalities.long[(t- 1),"Galguduud_Fatalities"]
    K<- tail(movavg(stations.long[(t-3):(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"], 2,type="m"),1)
    L<- rivers.long[(t- 1),"Shabelle_River_discharge"]
    M<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    N<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    O<- tail(movavg(stations.long[(t-3):(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"], 2,type="m"),1)
    P<- before.long[(t- 15),"Sool_BeforeRegion"]
    Q<- tail(movavg(fatalities.long[(t-7):(t- 1),"Woqooyi_Galbeed_Fatalities"], 6,type="m"),1)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GEJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(fatalities.long[(t-3):(t- 1),"Sool_Fatalities"], 2,type="m"),1)
    B<- current.long[(t- 1),"Awdal_CurrentRegion"]
    C<- current.long[(t- 7),"Togdheer_CurrentRegion"]
    D<- rain.long[(t- 15),"Jubbada_Hoose_rain"]
    E<- rain.long[(t- 16),"Jubbada_Hoose_rain"]
    G<- median(before.long[(t-17):(t- 1),"Bay_BeforeRegion"], na.rm=TRUE)
    H<- before.long[(t- 1),"Bay_BeforeRegion"]
    I<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    J<- fatalities.long[(t- 1),"Galguduud_Fatalities"]
    K<- fatalities.long[(t- 5),"Woqooyi_Galbeed_Fatalities"]
    L<- current.long[(t- 5),"Sanaag_CurrentRegion"]
    M<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    N<- before.long[(t- 1),"Bay_BeforeRegion"]
    O<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    P<- fatalities.long[(t- 1),"Galguduud_Fatalities"]
    Q<- fatalities.long[(t- 5),"Woqooyi_Galbeed_Fatalities"]
    R<- current.long[(t- 5),"Sanaag_CurrentRegion"]
    S<- mean(before.long[(t-17):(t- 1),"Sanaag_BeforeRegion"], na.rm=TRUE)
    U<- max(sum(1.52485718956821*B , C,na.rm=TRUE), D*E,na.rm=TRUE)
    if ( is.na(I) || is.na( J)){V<-0}
    else if(I>= J){V<-1 }
    else{V<-0 }
    W<- max(H*V, 1.43543208860659^K*L,na.rm=TRUE)
    if ( is.na(O) || is.na( P)){X<-0}
    else if(O>= P){X<-1 }
    else{X<-0 }
    Y<- max(N*X, 1.43543208860659^Q*R,na.rm=TRUE)
    Z<- Y%% 27.555938730149
    AA<- max(G,sum( W , -M*Z,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(S)){S <- 0 }
    FIN <-sum( 27.555938730149*A , U , AA , -S,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GEJUN9arrivals <- function(start, end){
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GEJUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(before.long[(t-11):(t- 1),"Bay_BeforeRegion"], na.rm=TRUE)
    B<- rain.long[(t- 1),"Awdal_rain"]
    C<- current.long[(t- 1),"Awdal_CurrentRegion"]
    D<- tail(movavg(stations.long[(t-9):(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"], 8,type="m"),1)
    E<- before.long[(t- 7),"Sanaag_BeforeRegion"]
    G<- tail(movavg(rain.long[(t-13):(t- 1),"Bari_rain"],12,type="w"),1)
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
    V<- tail(movavg(stations.long[(t-13):(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"], 12,type="m"),1)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GEJUN11arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(before.long[(t-10):(t- 1),"Bay_BeforeRegion"], na.rm=TRUE)
    B<- rain.long[(t- 15),"Jubbada_Hoose_rain"]
    C<- rain.long[(t- 16),"Jubbada_Hoose_rain"]
    D<- current.long[(t- 1),"Awdal_CurrentRegion"]
    E<- tail(movavg(stations.long[(t-7):(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"], 6,type="m"),1)
    G<- median(fatalities.long[(t-6):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    H<- current.long[(t- 17),"Bay_CurrentRegion"]
    I<- future.long[(t- 7),"Sanaag_FutureRegion"]
    J<- tail(movavg(fatalities.long[(t-3):(t- 1),"Bari_Fatalities"],2,type="w"),1)
    K<- current.long[(t- 7),"Nugaal_CurrentRegion"]
    L<- median(before.long[(t-14):(t- 1),"Bay_BeforeRegion"], na.rm=TRUE)
    M<- conflicts.long[(t- 1),"Awdal_Conflict"]
    N<- current.long[(t- 1),"Mudug_CurrentRegion"]
    O<- max(sum(0.106674930219957*H , 0.836318096733023*I*J , K , L,na.rm=TRUE), 0.0727443916219675*M*N,na.rm=TRUE)
    P<- max(sum(0.836318096733023*A , B*C , D*E*G,na.rm=TRUE), O,na.rm=TRUE)
    FIN <-P
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

GE1_2016arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 7),"Bari_Fatalities"]
    B<- before.long[(t- 7),"Sanaag_BeforeRegion"]
    C<- median(before.long[(t-13):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    D<- current.long[(t- 3),"Sool_CurrentRegion"]
    E<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    G<- current.long[(t- 1),"Bay_CurrentRegion"]
    H<- current.long[(t- 1),"Awdal_CurrentRegion"]
    I<- rain.long[(t- 1),"Awdal_rain"]
    J<- before.long[(t- 1),"Sool_BeforeRegion"]
    K<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    L<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    M<- stations.long[(t- 14),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
    N<- future.long[(t- 10),"Jubbada_Dhexe_FutureRegion"]
    O<- max(sum(1.69190253505245*C , 1.19267489093613*D,na.rm=TRUE), 0.00169428019260398*E*G,na.rm=TRUE)
    P<- max(O, 9.36054034137125e-5*H^2*I,na.rm=TRUE)
    if ( is.na(0.00169428019260398*L) || is.na( M)){Q<-0}
    else if(0.00169428019260398*L> M){Q<-1 }
    else{Q<-0 }
    R<- max(sum(A*B , P , -J , -K^Q,na.rm=TRUE), N,na.rm=TRUE)
    FIN <-R
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

GE2_2016arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(current.long[(t-14):(t- 1),"Awdal_CurrentRegion"],13,type="w"),1)
    B<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    C<- current.long[(t- 1),"Bay_CurrentRegion"]
    D<- tail(movavg(rain.long[(t-4):(t- 1),"Awdal_rain"], 3,type="m"),1)
    E<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    G<- future.long[(t- 8),"Woqooyi_Galbeed_FutureRegion"]
    H<- stations.long[(t- 11),"Hiiraan_Bulo_Burti_StationShabelle_River"]
    I<- tail(movavg(current.long[(t-14):(t- 1),"Awdal_CurrentRegion"],13,type="w"),1)
    J<- before.long[(t- 12),"Bari_BeforeRegion"]
    K<- before.long[(t- 14),"Bari_BeforeRegion"]
    L<- current.long[(t- 1),"Mudug_CurrentRegion"]
    M<- future.long[(t- 10),"Jubbada_Dhexe_FutureRegion"]
    N<- tail(movavg(rain.long[(t-4):(t- 1),"Awdal_rain"], 3,type="m"),1)
    O<- median(rain.long[(t-5):(t- 1),"Bakool_rain"], na.rm=TRUE)
    P<- max(5.33042003459404e-5*B*C*D, E*G,na.rm=TRUE)
    Q<- max(A, P,na.rm=TRUE)
    R<- max(sum(H*I , 0.0326599140113722*J*K,na.rm=TRUE), 1.6798477163082e-5*L^2,na.rm=TRUE)
    S<- max(sum(Q , R,na.rm=TRUE), M,na.rm=TRUE)
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( S , -N*O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

GE3_2016arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    B<- conflicts.long[(t- 1),"Bari_Conflict"]
    C<- before.long[(t- 1),"Banadir_BeforeRegion"]
    D<- before.long[(t- 1),"Mudug_BeforeRegion"]
    E<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    G<- before.long[(t- 1),"Bay_BeforeRegion"]
    H<- tail(movavg(fatalities.long[(t-4):(t- 1),"Mudug_Fatalities"], 3,type="m"),1)
    I<- future.long[(t- 3),"Nugaal_FutureRegion"]
    J<- tail(movavg(before.long[(t-17):(t- 1),"Banadir_BeforeRegion"],16,type="w"),1)
    K<- future.long[(t- 2),"Nugaal_FutureRegion"]
    L<- fatalities.long[(t- 1),"Gedo_Fatalities"]
    M<- tail(movavg(before.long[(t-9):(t- 1),"Togdheer_BeforeRegion"],8,type="w"),1)
    N<- tan(L)
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
    FIN <-sum( 0.219909293907864*A , 0.0505098067168348*B*C , 0.0081339331886208*D*E , 0.0780926830929366*G*H , I , J , -K , -N*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GE1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 13),"Bari_BeforeRegion"]
    B<- median(rain.long[(t-14):(t- 1),"Jubbada_Dhexe_rain"], na.rm=TRUE)
    C<- median(before.long[(t-12):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    D<- future.long[(t- 1),"Sool_FutureRegion"]
    E<- stations.long[(t- 5),"Shabelle_Dhexe_JowharStation_Shabelle_River"]
    G<- future.long[(t- 12),"Bakool_FutureRegion"]
    H<- rain.long[(t- 1),"Awdal_rain"]
    I<- current.long[(t- 1),"Awdal_CurrentRegion"]
    J<- conflicts.long[(t- 1),"Bakool_Conflict"]
    K<- before.long[(t- 1),"Mudug_BeforeRegion"]
    L<- future.long[(t- 3),"Nugaal_FutureRegion"]
    M<- before.long[(t- 13),"Bari_BeforeRegion"]
    N<- median(rain.long[(t-14):(t- 1),"Jubbada_Dhexe_rain"], na.rm=TRUE)
    O<- median(before.long[(t-12):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    P<- median(before.long[(t-9):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    Q<- max(A*B, C,na.rm=TRUE)
    R<- max(sum(10.1297622445581*D , E*G,na.rm=TRUE), 0.122435862795746*H*I,na.rm=TRUE)
    S<- max(M*N, O,na.rm=TRUE)
    U<- max(sum(0.0702239776299492*J*K , L,na.rm=TRUE), S,na.rm=TRUE)
    V<- max(sum(R , U , -5977.25562700561,na.rm=TRUE), P,na.rm=TRUE)
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(V)){V <- 0 }
    FIN <-sum( Q , V,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GE2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Galgaduud_Conflict"]
    B<- before.long[(t- 15),"Sool_BeforeRegion"]
    C<- rain.long[(t- 1),"Awdal_rain"]
    D<- current.long[(t- 1),"Awdal_CurrentRegion"]
    E<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    G<- future.long[(t- 1),"Bari_FutureRegion"]
    H<- future.long[(t- 13),"Jubbada_Hoose_FutureRegion"]
    I<- mean(rain.long[(t-5):(t- 1),"Sanaag_rain"], na.rm=TRUE)
    J<- tail(movavg(conflicts.long[(t-5):(t- 1),"Bakool_Conflict"],4,type="w"),1)
    K<- before.long[(t- 1),"Galgaduud_BeforeRegion"]
    L<- tail(movavg(before.long[(t-17):(t- 1),"Gedo_BeforeRegion"],16,type="w"),1)
    M<- median(before.long[(t-13):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    if ( is.na(I) ){N<- K}
    else if(I>0){N<- 4226.40269713912/J }
    else{N<- K }
    O<- max(sum(A*B , 0.0923522386296179*C*D , 0.0136058685128701*E*G , H , N , -5.94804684081454*L,na.rm=TRUE), 1.95969371736039*M,na.rm=TRUE)
    FIN <-O
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GE3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(current.long[(t-3):(t- 1),"Mudug_CurrentRegion"], 2,type="m"),1)
    B<- conflicts.long[(t- 1),"Awdal_Conflict"]
    C<- median(before.long[(t-11):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    D<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    E<- rain.long[(t- 4),"Togdheer_rain"]
    G<- future.long[(t- 1),"Bari_FutureRegion"]
    H<- future.long[(t- 1),"Woqooyi_Galbeed_FutureRegion"]
    I<- rain.long[(t- 15),"Jubbada_Hoose_rain"]
    J<- rain.long[(t- 16),"Jubbada_Hoose_rain"]
    K<- rain.long[(t- 1),"Awdal_rain"]
    L<- current.long[(t- 1),"Awdal_CurrentRegion"]
    M<- max(sum(1.49585373871517*C , D*E , 0.0345902551982261*G*H , I*J,na.rm=TRUE), 6.77513167319171e-7*K^2*L^2,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 3.34270723391271e-6*A^2*B , M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GE4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(current.long[(t-13):(t- 1),"Jubbada_Hoose_CurrentRegion"], na.rm=TRUE)
    B<- tail(movavg(fatalities.long[(t-16):(t- 1),"Shabeellaha_Dhexe_Fatalities"],15,type="w"),1)
    C<- tail(movavg(fatalities.long[(t-16):(t- 1),"Shabeellaha_Dhexe_Fatalities"],15,type="w"),1)
    D<- future.long[(t- 4),"Nugaal_FutureRegion"]
    E<- conflicts.long[(t- 1),"Awdal_Conflict"]
    G<- current.long[(t- 1),"Mudug_CurrentRegion"]
    H<- future.long[(t- 1),"Sool_FutureRegion"]
    I<- fatalities.long[(t- 5),"Togdheer_Fatalities"]
    J<- future.long[(t- 1),"Sool_FutureRegion"]
    K<- mean(fatalities.long[(t-17):(t- 1),"Shabeellaha_Dhexe_Fatalities"], na.rm=TRUE)
    L<- current.long[(t- 1),"Awdal_CurrentRegion"]
    M<- conflicts.long[(t- 3),"Togdheer_Conflict"]
    N<- fatalities.long[(t- 7),"Bakool_Fatalities"]
    O<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    P<- future.long[(t- 17),"Sanaag_FutureRegion"]
    Q<- max(sum(A/B , C,na.rm=TRUE), D,na.rm=TRUE)
    R<- max(Q, 0.0782256667580134*E*G,na.rm=TRUE)
    S<- max(J, 4780.15625469964/K,na.rm=TRUE)
    U<- max(L, M^2*N,na.rm=TRUE)
    V<- max(R,sum( H*I , S , U , -O*P,na.rm=TRUE),na.rm=TRUE)
    FIN <-V
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GE5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(current.long[(t-3):(t- 1),"Awdal_CurrentRegion"], 2,type="m"),1)
    B<- rain.long[(t- 15),"Jubbada_Hoose_rain"]
    C<- rain.long[(t- 16),"Jubbada_Hoose_rain"]
    D<- future.long[(t- 1),"Sool_FutureRegion"]
    E<- future.long[(t- 8),"Nugaal_FutureRegion"]
    G<- current.long[(t- 11),"Awdal_CurrentRegion"]
    H<- median(rivers.long[(t-5):(t- 1),"Juba_River_discharge"], na.rm=TRUE)
    I<- median(before.long[(t-17):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    J<- current.long[(t- 1),"Bakool_CurrentRegion"]
    K<- median(fatalities.long[(t-8):(t- 1),"Sool_Fatalities"], na.rm=TRUE)
    L<- median(rivers.long[(t-8):(t- 1),"Juba_River_discharge"], na.rm=TRUE)
    M<- conflicts.long[(t- 1),"Awdal_Conflict"]
    N<- current.long[(t- 1),"Mudug_CurrentRegion"]
    O<- max(sum(0.223640214622154*J , K*L,na.rm=TRUE), 0.0889832652401015*M*N,na.rm=TRUE)
    P<- abs(sum(1.56832369242282*A , B*C , D , E , G , H , I , -O,na.rm=TRUE))
    FIN <-P
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GE6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 17),"Bakool_BeforeRegion"]
    B<- current.long[(t- 17),"Shabeellaha_Hoose_CurrentRegion"]
    C<- tail(movavg(current.long[(t-9):(t- 1),"Mudug_CurrentRegion"],8,type="w"),1)
    D<- future.long[(t- 1),"Sool_FutureRegion"]
    E<- median(rain.long[(t-14):(t- 1),"Shabeellaha_Hoose_rain"], na.rm=TRUE)
    G<- rain.long[(t- 1),"Awdal_rain"]
    H<- tail(movavg(before.long[(t-3):(t- 1),"Jubbada_Dhexe_BeforeRegion"],2,type="w"),1)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GE7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(before.long[(t-12):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    B<- median(before.long[(t-9):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    C<- median(future.long[(t-7):(t- 1),"Jubbada_Hoose_FutureRegion"], na.rm=TRUE)
    D<- future.long[(t- 1),"Sool_FutureRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- conflicts.long[(t- 1),"Awdal_Conflict"]
    H<- current.long[(t- 1),"Mudug_CurrentRegion"]
    I<- rain.long[(t- 1),"Awdal_rain"]
    J<- current.long[(t- 1),"Awdal_CurrentRegion"]
    K<- median(before.long[(t-12):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    L<- future.long[(t- 12),"Bakool_FutureRegion"]
    M<- mean(future.long[(t-6):(t- 1),"Banadir_FutureRegion"], na.rm=TRUE)
    N<- max(sum(10.4697224174048*D , 2.91142676637388*E , 0.157051025533882*G*H , 0.145610671314206*I*J , K , -9855.00592900711,na.rm=TRUE),sum( L , -M,na.rm=TRUE),na.rm=TRUE)
    O<- max(C, N,na.rm=TRUE)
    P<- max(B, O,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( A , P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GE8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GE9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GE10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Bari_FutureRegion"]
    B<- future.long[(t- 1),"Woqooyi_Galbeed_FutureRegion"]
    C<- future.long[(t- 4),"Nugaal_FutureRegion"]
    D<- future.long[(t- 1),"Bay_FutureRegion"]
    E<- tail(movavg(conflicts.long[(t-6):(t- 1),"Bakool_Conflict"], 5,type="m"),1)
    G<- rain.long[(t- 4),"Bay_rain"]
    H<- conflicts.long[(t- 15),"Bakool_Conflict"]
    I<- median(before.long[(t-7):(t- 1),"Jubbada_Hoose_BeforeRegion"], na.rm=TRUE)
    J<- conflicts.long[(t- 1),"Awdal_Conflict"]
    K<- current.long[(t- 1),"Mudug_CurrentRegion"]
    L<- rain.long[(t- 4),"Bay_rain"]
    M<- stations.long[(t- 1),"Gedo_DollowStation_Juba_River"]
    N<- current.long[(t- 1),"Awdal_CurrentRegion"]
    O<- median(conflicts.long[(t-3):(t- 1),"Woqooyi_Galbeed_Conflict"], na.rm=TRUE)
    P<- mean(conflicts.long[(t-9):(t- 1),"Bakool_Conflict"], na.rm=TRUE)
    Q<- max(sum(D/E , G*H , I,na.rm=TRUE), 0.0782101589072784*J*K,na.rm=TRUE)
    R<- max(C, Q,na.rm=TRUE)
    S<- max(sum(0.035446716118198*A*B , R , -L,na.rm=TRUE), M*N*O/P,na.rm=TRUE)
    FIN <-S
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GE11arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Bari_FutureRegion"]
    B<- future.long[(t- 1),"Woqooyi_Galbeed_FutureRegion"]
    C<- future.long[(t- 4),"Nugaal_FutureRegion"]
    D<- future.long[(t- 1),"Bay_FutureRegion"]
    E<- tail(movavg(conflicts.long[(t-6):(t- 1),"Bakool_Conflict"], 5,type="m"),1)
    G<- rain.long[(t- 4),"Bay_rain"]
    H<- conflicts.long[(t- 15),"Bakool_Conflict"]
    I<- median(before.long[(t-7):(t- 1),"Jubbada_Hoose_BeforeRegion"], na.rm=TRUE)
    J<- conflicts.long[(t- 1),"Awdal_Conflict"]
    K<- current.long[(t- 1),"Mudug_CurrentRegion"]
    L<- rain.long[(t- 4),"Bay_rain"]
    M<- stations.long[(t- 1),"Gedo_DollowStation_Juba_River"]
    N<- current.long[(t- 1),"Awdal_CurrentRegion"]
    O<- median(conflicts.long[(t-3):(t- 1),"Woqooyi_Galbeed_Conflict"], na.rm=TRUE)
    P<- mean(conflicts.long[(t-9):(t- 1),"Bakool_Conflict"], na.rm=TRUE)
    Q<- max(sum(D/E , G*H , I,na.rm=TRUE), 0.0782101589072784*J*K,na.rm=TRUE)
    R<- max(C, Q,na.rm=TRUE)
    S<- max(sum(0.035446716118198*A*B , R , -L,na.rm=TRUE), M*N*O/P,na.rm=TRUE)
    FIN <-S
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}


