
modelarrivals_TOminus1arrivals<- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    B<- future.long[(t- 1),"Sool_FutureRegion"]
    C<- current.long[(t- 1),"Bay_CurrentRegion"]
    D<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- median(before.long[(t-3):(t- 1),"Sool_BeforeRegion"], na.rm=TRUE)
    H<- before.long[(t- 1),"Sool_BeforeRegion"]
    I<- stations.long[(t- 1),"Gedo_DollowStation_Juba_River"]
    J<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    K<- before.long[(t- 1),"Bari_BeforeRegion"]
    L<- min(E, G,na.rm=TRUE)
    M<- factorial(I)
    N<- min(K, 1300.52883112048,na.rm=TRUE)
    O<- max(106.857142696362,sum( 0.00613941364323252*A , 0.000409028826536182*B*C , 0.00613941364323252*D*L , 0.0105572926947711*H*M*J , -256.845275191631 , -2.65326750909988*N,na.rm=TRUE),na.rm=TRUE)
    FIN <-O
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TOminus2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- tail(movavg(before.long[(t-13):(t- 2),"Togdheer_BeforeRegion"], 11,type="m"),1)
    B<- tail(movavg(before.long[(t-15):(t- 2),"Togdheer_BeforeRegion"], 13,type="m"),1)
    C<- before.long[(t- 2),"Awdal_BeforeRegion"]
    D<- future.long[(t- 5),"Nugaal_FutureRegion"]
    E<- future.long[(t- 4),"Nugaal_FutureRegion"]
    G<- future.long[(t- 6),"Nugaal_FutureRegion"]
    H<- median(fatalities.long[(t-8):(t- 2),"Jubbada_Dhexe_Fatalities"], na.rm=TRUE)
    I<- future.long[(t- 9),"Nugaal_FutureRegion"]
    J<- fatalities.long[(t- 2),"Bari_Fatalities"]
    K<- max(2.48835909170698*C,sum( 1.61079832151075*D , 0.383033139869816*E , G*H , I,na.rm=TRUE),na.rm=TRUE)
    L<- max(1.01673899954768*A,sum( B , K , -J,na.rm=TRUE),na.rm=TRUE)
    M<- max(662, L,na.rm=TRUE)
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 1.02653315697301*M , -572.823006421,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TO1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    B<- fatalities.long[(t- 3),"Awdal_Fatalities"]
    C<- tail(movavg(fatalities.long[(t-13):(t- 1),"Awdal_Fatalities"],12,type="w"),1)
    D<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    E<- fatalities.long[(t- 1),"Galguduud_Fatalities"]
    G<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    H<- before.long[(t- 1),"Bari_BeforeRegion"]
    I<- mean(conflicts.long[(t-12):(t- 1),"Shabeellaha_Dhexe_Conflict"], na.rm=TRUE)
    J<- future.long[(t- 1),"Sanaag_FutureRegion"]
    K<- mean(conflicts.long[(t-17):(t- 1),"Galgaduud_Conflict"], na.rm=TRUE)
    L<- current.long[(t- 1),"Bay_CurrentRegion"]
    M<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    N<- fatalities.long[(t- 3),"Awdal_Fatalities"]
    O<- median(before.long[(t-17):(t- 1),"Sool_BeforeRegion"], na.rm=TRUE)
    P<- min(sum(0.420322402064519*D , E*G , H*I , J^2*K,na.rm=TRUE), L,na.rm=TRUE)
    Q<- max(sum(P , -2330.84291082342,na.rm=TRUE), 0.420322402064519*M*N,na.rm=TRUE)
    R<- max(sum(A*B*C , Q,na.rm=TRUE), O,na.rm=TRUE)
    FIN <-R
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TO2arrivals<- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 1),"Bakool_CurrentRegion"]
    B<- conflicts.long[(t- 1),"Awdal_Conflict"]
    C<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    G<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    H<- current.long[(t- 1),"Bakool_CurrentRegion"]
    I<- future.long[(t- 1),"Sool_FutureRegion"]
    J<- median(current.long[(t-8):(t- 1),"Sool_CurrentRegion"], na.rm=TRUE)
    K<- median(rain.long[(t-6):(t- 1),"Gedo_rain"], na.rm=TRUE)
    L<- median(future.long[(t-10):(t- 1),"Sanaag_FutureRegion"], na.rm=TRUE)
    M<- erf(2.8302880326536e-9*G^2*H)
    N<- max(112, 0.00178102478425264*I*J,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 6.40887890532316e-9*A^2*B*C , 0.00673027503316286*D*E*M , N , -K , -L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TO3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    B<- before.long[(t- 1),"Sool_BeforeRegion"]
    C<- current.long[(t- 1),"Bay_CurrentRegion"]
    D<- future.long[(t- 1),"Sool_FutureRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    H<- mean(before.long[(t-6):(t- 1),"Shabeellaha_Hoose_BeforeRegion"], na.rm=TRUE)
    I<- current.long[(t- 10),"Sanaag_CurrentRegion"]
    J<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    K<- tail(movavg(future.long[(t-17):(t- 1),"Gedo_FutureRegion"],16,type="w"),1)
    L<- before.long[(t- 1),"Bari_BeforeRegion"]
    M<- tanh(0.000184363971271334*H)
    N<- max(sum(0.0156483943562123*A*B , 0.000407298371237719*C*D , 0.00598775351249351*E*G*M,na.rm=TRUE), I,na.rm=TRUE)
    O<- max(sum(N , -J , -K , -2.46909948745724*L,na.rm=TRUE), 106.857141652297,na.rm=TRUE)
    FIN <-O
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TO4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    C<- tail(movavg(fatalities.long[(t-4):(t- 1),"Shabeellaha_Dhexe_Fatalities"], 3,type="m"),1)
    D<- tail(movavg(fatalities.long[(t-10):(t- 1),"Shabeellaha_Dhexe_Fatalities"], 9,type="m"),1)
    E<- tail(movavg(before.long[(t-3):(t- 1),"Awdal_BeforeRegion"],2,type="w"),1)
    G<- future.long[(t- 1),"Gedo_FutureRegion"]
    H<- median(before.long[(t-5):(t- 1),"Awdal_BeforeRegion"], na.rm=TRUE)
    I<- mean(rain.long[(t-3):(t- 1),"Bari_rain"], na.rm=TRUE)
    J<- median(before.long[(t-4):(t- 1),"Awdal_BeforeRegion"], na.rm=TRUE)
    K<- rain.long[(t- 9),"Bakool_rain"]
    L<- future.long[(t- 1),"Shabeallaha_Dhexe_FutureRegion"]
    M<- mean(rain.long[(t-3):(t- 1),"Sanaag_rain"], na.rm=TRUE)
    if ( is.na(E) || is.na( G)){N<-0}
    else if(E>= G){N<-1 }
    else{N<-0 }
    O<- round(0.00310162405400471*I*J)
    P<- max(sum(1.32183776737769e-5*A*B*C , 109.672658801029*D*N , H*O , K , -L*M,na.rm=TRUE), 104,na.rm=TRUE)
    FIN <-P
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TO5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    B<- before.long[(t- 1),"Bari_BeforeRegion"]
    C<- median(before.long[(t-4):(t- 1),"Sool_BeforeRegion"], na.rm=TRUE)
    D<- tail(movavg(future.long[(t-5):(t- 1),"Togdheer_FutureRegion"], 4,type="m"),1)
    E<- tail(movavg(fatalities.long[(t-16):(t- 1),"Togdheer_Fatalities"],15,type="w"),1)
    G<- future.long[(t- 1),"Sanaag_FutureRegion"]
    H<- before.long[(t- 1),"Bari_BeforeRegion"]
    I<- median(before.long[(t-4):(t- 1),"Sool_BeforeRegion"], na.rm=TRUE)
    J<- mean(fatalities.long[(t-4):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    K<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    L<- median(before.long[(t-10):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    M<- mean(future.long[(t-4):(t- 1),"Sanaag_FutureRegion"], na.rm=TRUE)
    N<- atan(0.00188671282144229*H*I)
    O<- max(D, E*G*N,na.rm=TRUE)
    P<- max(93.3659732307153, O,na.rm=TRUE)
    Q<- max(P,sum( 2.83888686769987*J*K , L , -181.493540264348,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 7.9449416885401e-6*A^2 , 0.00188671282144229*B*C , Q , -M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TO6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- median(before.long[(t-6):(t- 1),"Awdal_BeforeRegion"], na.rm=TRUE)
    B<- median(current.long[(t-6):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    C<- median(current.long[(t-6):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    D<- tail(movavg(current.long[(t-6):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], 5,type="m"),1)
    E<- median(current.long[(t-6):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    G<- median(before.long[(t-4):(t- 1),"Awdal_BeforeRegion"], na.rm=TRUE)
    H<- stations.long[(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"]
    I<- stations.long[(t- 1),"Gedo_DollowStation_Juba_River"]
    J<- median(current.long[(t-6):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    K<- median(current.long[(t-6):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    L<- median(before.long[(t-7):(t- 1),"Awdal_BeforeRegion"], na.rm=TRUE)
    M<- max(D, E,na.rm=TRUE)
    N<- 1.03403068099564*H*I%% 0.0322453873686108*J
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 381.067907249217 , 0.00757711369481403*A*B , 0.000118427983803816*C*M , G*N , -0.334920653573672*K , -21.8359351626973*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TO7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- rain.long[(t- 5),"Jubbada_Hoose_rain"]
    B<- current.long[(t- 1),"Sool_CurrentRegion"]
    C<- rain.long[(t- 14),"Banaadir_rain"]
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    G<- future.long[(t- 1),"Sool_FutureRegion"]
    H<- current.long[(t- 1),"Bay_CurrentRegion"]
    I<- fatalities.long[(t- 9),"Jubbada_Hoose_Fatalities"]
    J<- fatalities.long[(t- 16),"Gedo_Fatalities"]
    K<- mean(future.long[(t-13):(t- 1),"Shabeellaha_Hoose_FutureRegion"], na.rm=TRUE)
    L<- before.long[(t- 1),"Bari_BeforeRegion"]
    M<- stations.long[(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"]
    N<- median(before.long[(t-17):(t- 1),"Sool_BeforeRegion"], na.rm=TRUE)
    O<- max(sum(0.0575236259365036*B , 16.100990862711*C , 0.00589629528275626*D*E , 0.000449149476018849*G*H , I , J , -K , -L*M,na.rm=TRUE), N,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( A , O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TO8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    C<- current.long[(t- 1),"Bay_CurrentRegion"]
    D<- future.long[(t- 1),"Sool_FutureRegion"]
    E<- current.long[(t- 1),"Bay_CurrentRegion"]
    G<- current.long[(t- 1),"Bay_CurrentRegion"]
    H<- before.long[(t- 1),"Sool_BeforeRegion"]
    I<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    J<- before.long[(t- 1),"Sool_BeforeRegion"]
    K<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    L<- before.long[(t- 1),"Bari_BeforeRegion"]
    M<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    N<- before.long[(t- 1),"Bari_BeforeRegion"]
    O<- before.long[(t- 1),"Bari_BeforeRegion"]
    P<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    Q<- current.long[(t- 1),"Bay_CurrentRegion"]
    R<- future.long[(t- 1),"Sool_FutureRegion"]
    S<- current.long[(t- 1),"Bay_CurrentRegion"]
    U<- current.long[(t- 1),"Bay_CurrentRegion"]
    V<- (0.00582484547927256*A*B)
    if ( is.na(C) || is.na( 10087.6054175655)){W<-0}
    else if(C>= 10087.6054175655){W<-1 }
    else{W<-0 }
    X<- (0.000416347053227405*D*E)
    if ( is.na(G) || is.na( 10087.6054175655)){Y<-0}
    else if(G>= 10087.6054175655){Y<-1 }
    else{Y<-0 }
    Z<- max(92, 0.0116455014303189*H*I,na.rm=TRUE)
    AA<- tan(0.0116455014303189*J*K)
    BB<- 0.00582484547927256*L*M%% 10087.6054175655
    CC<- tan(BB)
    DD<- (0.00582484547927256*O*P)
    if ( is.na(Q) || is.na( 10087.6054175655)){EE<-0}
    else if(Q>= 10087.6054175655){EE<-1 }
    else{EE<-0 }
    GF<- (0.000416347053227405*R*S)
    if ( is.na(U) || is.na( 10087.6054175655)){HG<-0}
    else if(U>= 10087.6054175655){HG<-1 }
    else{HG<-0 }
    IH<- 2.15535802009812*N%%sum( DD^EE, na.rm = TRUE)
                                  if(is.infinite(V)){V <- 0 }
                                  if(is.infinite(W)){W <- 0 }
                                  if(is.infinite(X)){X <- 0 }
                                  if(is.infinite(Y)){Y <- 0 }
                                  if(is.infinite(Z)){Z <- 0 }
                                  if(is.infinite(A)){A <- 0 }
                                  if(is.infinite(A)){A <- 0 }
                                  if(is.infinite(C)){C <- 0 }
                                  if(is.infinite(C)){C <- 0 }
                                  if(is.infinite(I)){I <- 0 }
                                  if(is.infinite(H)){H <- 0 }
                                  FIN <-sum( V^W , X^Y , Z , -AA , -CC , -IH,na.rm=TRUE)
                                  PA[t] <- FIN
                                  PI[t] <- 0
                                  PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TO9arrivals <- function(start, end){
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TO10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    B<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    C<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    D<- fatalities.long[(t- 1),"Bari_Fatalities"]
    E<- rain.long[(t- 14),"Banaadir_rain"]
    G<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    H<- fatalities.long[(t- 1),"Bari_Fatalities"]
    I<- mean(current.long[(t-5):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    J<- before.long[(t- 1),"Bari_BeforeRegion"]
    K<- median(before.long[(t-4):(t- 1),"Sool_BeforeRegion"], na.rm=TRUE)
    L<- rain.long[(t- 14),"Banaadir_rain"]
    M<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    N<- fatalities.long[(t- 1),"Bari_Fatalities"]
    O<- max(13.8100735936345*E, 0.0364175355500498*G*H,na.rm=TRUE)
    if ( is.na(C) ){P<- I}
    else if(C>0){P<-sum( D , O,na.rm=TRUE) }
    else{P<- I }
    Q<- max(sum(B , P , -438.339372905038,na.rm=TRUE), 0.00153543063909588*J*K,na.rm=TRUE)
    R<- max(78, Q,na.rm=TRUE)
    S<- max(13.8100735936345*L, 0.0364175355500498*M*N,na.rm=TRUE)
    U<- tan(S)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(U)){U <- 0 }
    FIN <-sum( 0.0302199210869935*A , 1.23687439492757*R , -U,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TOJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    B<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    C<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    D<- median(rain.long[(t-16):(t- 1),"Nugaal_rain"], na.rm=TRUE)
    E<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    G<- before.long[(t- 1),"Bari_BeforeRegion"]
    H<- rain.long[(t- 14),"Bay_rain"]
    I<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    J<- tail(movavg(future.long[(t-11):(t- 1),"Sanaag_FutureRegion"],10,type="w"),1)
    K<- before.long[(t- 1),"Bari_BeforeRegion"]
    L<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    M<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    N<- median(rain.long[(t-16):(t- 1),"Nugaal_rain"], na.rm=TRUE)
    O<- median(fatalities.long[(t-10):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    P<- median(current.long[(t-10):(t- 1),"Sanaag_CurrentRegion"], na.rm=TRUE)
    Q<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    R<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    S<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    U<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    V<- max(97.584951575864, 1.04319896512183e-5*C^2,na.rm=TRUE)
    if ( is.na(I) ){W<- 1.43163621593907e-5*K*L}
    else if(I>0){W<- J }
    else{W<- 1.43163621593907e-5*K*L }
    if ( is.na(D) ){X<- H*W}
    else if(D>0){X<- 1.43163621593907e-5*E^2*G }
    else{X<- H*W }
    Y<- M%% N
    if ( is.na(O) ){Z<-sum( 1.72265717505534*Q , 0.410996885323418*R,na.rm=TRUE)}
    else if(O>0){Z<- P }
    else{Z<-sum( 1.72265717505534*Q , 0.410996885323418*R,na.rm=TRUE) }
    if ( is.na(Y) ){AA<-sum( 1.72265717505534*S , 0.410996885323418*U,na.rm=TRUE)}
    else if(Y>0){AA<- Z }
    else{AA<-sum( 1.72265717505534*S , 0.410996885323418*U,na.rm=TRUE) }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(X)){X <- 0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(A)){A <- 0 }
    FIN <-sum( 1.72265717505534*A , 0.410996885323418*B , V , X , -AA,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TOJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- median(fatalities.long[(t-17):(t- 1),"Sool_Fatalities"], na.rm=TRUE)
    B<- median(future.long[(t-3):(t- 1),"Sanaag_FutureRegion"], na.rm=TRUE)
    C<- tail(movavg(before.long[(t-8):(t- 1),"Sanaag_BeforeRegion"],7,type="w"),1)
    D<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    E<- fatalities.long[(t- 1),"Shabeellaha_Hoose_Fatalities"]
    G<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    H<- before.long[(t- 1),"Mudug_BeforeRegion"]
    I<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    J<- before.long[(t- 5),"Togdheer_BeforeRegion"]
    K<- current.long[(t- 12),"Bakool_CurrentRegion"]
    L<- median(fatalities.long[(t-17):(t- 1),"Sool_Fatalities"], na.rm=TRUE)
    M<- median(future.long[(t-3):(t- 1),"Sanaag_FutureRegion"], na.rm=TRUE)
    N<- tail(movavg(before.long[(t-8):(t- 1),"Sanaag_BeforeRegion"],7,type="w"),1)
    O<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    P<- fatalities.long[(t- 1),"Shabeellaha_Hoose_Fatalities"]
    Q<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    R<- before.long[(t- 1),"Mudug_BeforeRegion"]
    S<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    U<- before.long[(t- 5),"Togdheer_BeforeRegion"]
    V<- current.long[(t- 12),"Bakool_CurrentRegion"]
    W<- max(C,sum( 0.219666932980934*D , 0.00675749834867946*E*G , 6.05539174255282e-5*H*I , -948.301191067851,na.rm=TRUE),na.rm=TRUE)
    X<- max(B, W,na.rm=TRUE)
    Y<- max(X,sum( J , -K,na.rm=TRUE),na.rm=TRUE)
    Z<- max(N,sum( 0.219666932980934*O , 0.00675749834867946*P*Q , 6.05539174255282e-5*R*S , -948.301191067851,na.rm=TRUE),na.rm=TRUE)
    AA<- max(M, Z,na.rm=TRUE)
    BB<- max(AA,sum( U , -V,na.rm=TRUE),na.rm=TRUE)
    if ( is.na(sum(A^2*Y , -233.034582350609,na.rm=TRUE)) ){CC<- 97.852941151437}
    else if(sum(A^2*Y , -233.034582350609,na.rm=TRUE)>0){CC<-sum( L^2*BB , -233.034582350609,na.rm=TRUE) }
    else{CC<- 97.852941151437 }
    FIN <-CC
      PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TOJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Sool_BeforeRegion"]
    B<- future.long[(t- 1),"Sool_FutureRegion"]
    C<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    D<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    E<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    G<- rain.long[(t- 6),"Sanaag_rain"]
    H<- before.long[(t- 1),"Bari_BeforeRegion"]
    I<- before.long[(t- 1),"Sool_BeforeRegion"]
    J<- mean(before.long[(t-4):(t- 1),"Nugaal_BeforeRegion"], na.rm=TRUE)
    K<- future.long[(t- 1),"Sool_FutureRegion"]
    L<- rain.long[(t- 6),"Sanaag_rain"]
    M<- before.long[(t- 3),"Nugaal_BeforeRegion"]
    N<- before.long[(t- 10),"Nugaal_BeforeRegion"]
    O<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    P<- rain.long[(t- 6),"Sanaag_rain"]
    Q<- before.long[(t- 1),"Bari_BeforeRegion"]
    R<- before.long[(t- 1),"Sool_BeforeRegion"]
    S<- mean(before.long[(t-4):(t- 1),"Nugaal_BeforeRegion"], na.rm=TRUE)
    U<- future.long[(t- 1),"Sool_FutureRegion"]
    V<- min(0.0119903189213854*H, 0.00350515841225134*I,na.rm=TRUE)
    if ( is.na(G) ){W<-sum( J , -K,na.rm=TRUE)}
    else if(G>0){W<- V }
    else{W<-sum( J , -K,na.rm=TRUE) }
    X<- min(0.0119903189213854*Q, 0.00350515841225134*R,na.rm=TRUE)
    if ( is.na(P) ){Y<-sum( S , -U,na.rm=TRUE)}
    else if(P>0){Y<- X }
    else{Y<-sum( S , -U,na.rm=TRUE) }
    if ( is.na(L) ){Z<- O*Y}
    else if(L>0){Z<- 0.0121295925232922*M*N }
    else{Z<- O*Y }
    AA<- max(sum(0.0013823918775555*A*B , 0.000346794459078053*C*D , E*W , Z,na.rm=TRUE), 106.85625241977,na.rm=TRUE)
    FIN <-AA
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TOJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 4),"Nugaal_BeforeRegion"]
    B<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    C<- median(before.long[(t-4):(t- 1),"Sool_BeforeRegion"], na.rm=TRUE)
    D<- mean(future.long[(t-5):(t- 1),"Gedo_FutureRegion"], na.rm=TRUE)
    E<- rain.long[(t- 1),"Banaadir_rain"]
    G<- fatalities.long[(t- 3),"Nugaal_Fatalities"]
    H<- before.long[(t- 3),"Nugaal_BeforeRegion"]
    I<- current.long[(t- 17),"Bakool_CurrentRegion"]
    J<- D%% 2.26470633506722
    K<- round(0.00192458060797625*H^2)
    L<- max(E*G, K,na.rm=TRUE)
    M<- max(sum(0.0017068793777598*A^2 , 2.84255858753073e-5*B*C*J , L , -92.5222439523044 , -0.17026812199589*I,na.rm=TRUE), 97.8529419447355,na.rm=TRUE)
    FIN <-M
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TOJUN5arrivals <- function(start, end){
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TOJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 10),"Sanaag_CurrentRegion"]
    B<- rain.long[(t- 9),"Banaadir_rain"]
    C<- conflicts.long[(t- 1),"Awdal_Conflict"]
    D<- future.long[(t- 1),"Sool_FutureRegion"]
    E<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    G<- current.long[(t- 1),"Awdal_CurrentRegion"]
    H<- future.long[(t- 1),"Sool_FutureRegion"]
    I<- fatalities.long[(t- 9),"Banaadir_Fatalities"]
    J<- before.long[(t- 1),"Bay_BeforeRegion"]
    K<- max(0.000290444744628479*A^2,sum( B*C , -97.8529397175208,na.rm=TRUE),na.rm=TRUE)
    L<- max(97.8529397175208, K,na.rm=TRUE)
    if ( is.na(J) || is.na( 32491.5410963809)){M<-0}
    else if(J> 32491.5410963809){M<-1 }
    else{M<-0 }
    N<- max(sum(1.69924723508402*D , 0.505791560701742*E , G , -2606.04056658952,na.rm=TRUE), 1.16956041584414*H*I*M,na.rm=TRUE)
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( L , N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TOJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 3),"Nugaal_BeforeRegion"]
    B<- before.long[(t- 3),"Nugaal_BeforeRegion"]
    C<- tail(movavg(before.long[(t-12):(t- 1),"Sanaag_BeforeRegion"],11,type="w"),1)
    D<- before.long[(t- 3),"Nugaal_BeforeRegion"]
    E<- tail(movavg(before.long[(t-12):(t- 1),"Sanaag_BeforeRegion"],11,type="w"),1)
    G<- future.long[(t- 5),"Nugaal_FutureRegion"]
    H<- tail(movavg(before.long[(t-12):(t- 1),"Sanaag_BeforeRegion"],11,type="w"),1)
    I<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    J<- mean(fatalities.long[(t-13):(t- 1),"Sanaag_Fatalities"], na.rm=TRUE)
    K<- mean(rain.long[(t-5):(t- 1),"Hiiraan_rain"], na.rm=TRUE)
    L<- tan(2.01254436885871*A)
    M<- tan(sum(97.4928772343168 , 0.00203536243843467*B^2 , 0.00023366907240474*C^2,na.rm=TRUE))
    N<- max(M,sum( 97.4928772343168 , 0.00203536243843467*D^2 , 0.00023366907240474*E^2 , 0.000179439249063206*G^2 , 8.32565623379112e-5*H*I,na.rm=TRUE),na.rm=TRUE)
    O<- max(L, N,na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( O , -J^2*K,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TOJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- rain.long[(t- 14),"Bakool_rain"]
    B<- future.long[(t- 1),"Sanaag_FutureRegion"]
    C<- tail(movavg(stations.long[(t-5):(t- 1),"Gedo_BardheereStation_Juba_River"], 4,type="m"),1)
    D<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    E<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    G<- before.long[(t- 1),"Bakool_BeforeRegion"]
    H<- before.long[(t- 1),"Bari_BeforeRegion"]
    I<- fatalities.long[(t- 13),"Bay_Fatalities"]
    J<- rain.long[(t- 14),"Banaadir_rain"]
    K<- before.long[(t- 1),"Bay_BeforeRegion"]
    L<- max(sum(2.47017424777942*A , B*C , 0.707365315535024*D*E , 0.00134031235886951*G*H , I*J , -2864.37475182005 , -0.0129559689002382*K,na.rm=TRUE), 97.8529410656966,na.rm=TRUE)
    FIN <-L
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TOJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- rain.long[(t- 1),"Togdheer_rain"]
    B<- mean(stations.long[(t-9):(t- 1),"Gedo_DollowStation_Juba_River"], na.rm=TRUE)
    C<- before.long[(t- 1),"Bay_BeforeRegion"]
    D<- future.long[(t- 1),"Sanaag_FutureRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- before.long[(t- 1),"Bay_BeforeRegion"]
    H<- mean(stations.long[(t-9):(t- 1),"Gedo_DollowStation_Juba_River"], na.rm=TRUE)
    I<- tail(movavg(current.long[(t-6):(t- 1),"Awdal_CurrentRegion"], 5,type="m"),1)
    J<- mean(stations.long[(t-9):(t- 1),"Gedo_DollowStation_Juba_River"], na.rm=TRUE)
    K<- water.long[(t- 1),"Mudug_WaterDrumPrice"]
    L<- tan(93.0400083862072*B)
    if ( is.na(C) || is.na( 74926.1601610972)){M<-0}
    else if(C> 74926.1601610972){M<-1 }
    else{M<-0 }
    N<- tan(93.0400083862072*J)
    if ( is.na(M) ){O<-sum( 4.48223257975114*D , 0.000294968337095004*E*G , H*I , 13.3798360744163*N , -0.292351514602268*K,na.rm=TRUE)}
    else if(M>0){O<- 14811 }
    else{O<-sum( 4.48223257975114*D , 0.000294968337095004*E*G , H*I , 13.3798360744163*N , -0.292351514602268*K,na.rm=TRUE) }
    P<- max(A*L, O,na.rm=TRUE)
    Q<- max(P, 97.8529467215731,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_TOJUN10arrivals <- function(start, end){
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
} 


