modelarrivals_BRminus1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 12),"Togdheer_BeforeRegion"]
    B<- mean(before.long[(t-17):(t- 1),"Bari_BeforeRegion"], na.rm=TRUE)
    C<- tail(movavg(rain.long[(t-4):(t- 1),"Bakool_rain"], 3,type="m"),1)
    D<- fatalities.long[(t- 13),"Bakool_Fatalities"]
    E<- tail(movavg(rain.long[(t-8):(t- 1),"Bakool_rain"], 7,type="m"),1)
    G<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    H<- before.long[(t- 1),"Bari_BeforeRegion"]
    I<- before.long[(t- 13),"Sool_BeforeRegion"]
    J<- tail(movavg(current.long[(t-14):(t- 1),"Awdal_CurrentRegion"], 13,type="m"),1)
    K<- stations.long[(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"]
    L<- conflicts.long[(t- 1),"Shabeellaha_Hoose_Conflict"]
    M<- (sum(H , I , J,na.rm=TRUE))
    N<- max(sum(0.961035184974389 , 5.19281507856289*C , 1.16352682066196*D*E , -1.88952167942493*G,na.rm=TRUE), M/K,na.rm=TRUE)
    O<- max(1.96005380869894*B, 0.961035184974389*N,na.rm=TRUE)
    P<- max(A, O,na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( P , -L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BRminus2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- rain.long[(t- 5),"Banaadir_rain"]
    B<- before.long[(t- 12),"Awdal_BeforeRegion"]
    C<- fatalities.long[(t- 2),"Bari_Fatalities"]
    D<- fatalities.long[(t- 4),"Banaadir_Fatalities"]
    E<- mean(future.long[(t-5):(t- 2),"Nugaal_FutureRegion"], na.rm=TRUE)
    G<- mean(before.long[(t-7):(t- 2),"Mudug_BeforeRegion"], na.rm=TRUE)
    H<- tail(movavg(before.long[(t-8):(t- 2),"Galgaduud_BeforeRegion"],6,type="w"),1)
    I<- median(current.long[(t-6):(t- 2),"Banadir_CurrentRegion"], na.rm=TRUE)
    J<- rain.long[(t- 5),"Banaadir_rain"]
    K<- before.long[(t- 12),"Awdal_BeforeRegion"]
    L<- fatalities.long[(t- 4),"Banaadir_Fatalities"]
    M<- current.long[(t- 2),"Woqooyi_Galbeed_CurrentRegion"]
    N<- fatalities.long[(t- 2),"Galguduud_Fatalities"]
    O<- fatalities.long[(t- 2),"Galguduud_Fatalities"]
    P<- tail(movavg(before.long[(t-8):(t- 2),"Galgaduud_BeforeRegion"],6,type="w"),1)
    Q<- tail(movavg(goats.long[(t-16):(t- 2),"Banadir_goatprice"], 14,type="m"),1)
    R<- before.long[(t- 5),"Woqooyi_Galbeed_BeforeRegion"]
    S<- log(C)
    U<- min(E, G,na.rm=TRUE)
    if(is.infinite(U)){U<-0}
    V<- max(U,sum( H , -I,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(V)){V<-0}
    if ( is.na(N) ){W<- P}
    else if(N>0){W<- O }
    else{W<- P }
    X<- max(sum(J*K , L,na.rm=TRUE), 0.0335671692857542*M*W,na.rm=TRUE)
    if(is.infinite(X)){X <-0}
    Y<- max(1477.71770109804, X,na.rm=TRUE)
    if(is.infinite(Y)){Y<- 0}
    Z<- max(sum(A*B , S , D , V , Y , -0.000776152570495293*Q,na.rm=TRUE), R)
    FIN <-Z
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BR1arrivals <- function(start, end){
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
    N<- max(0.146017438448461*H, I*J, na.rm=TRUE)
    if(is.infinite(N)){N<-0}
    O<- max(sum(16491*M , 0.000319050428114188*B*C , 0.0254806108383852*D*E , G , N,na.rm=TRUE), 0.161874505917949*K*L,na.rm=TRUE)
    FIN <-O
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BR2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- mean(future.long[(t-16):(t- 1),"Sanaag_FutureRegion"], na.rm=TRUE)
    B<- mean(future.long[(t-7):(t- 1),"Nugaal_FutureRegion"], na.rm=TRUE)
    C<- conflicts.long[(t- 1),"Awdal_Conflict"]
    D<- rain.long[(t- 3),"Bakool_rain"]
    E<- median(rain.long[(t-3):(t- 1),"Bakool_rain"], na.rm=TRUE)
    G<- rain.long[(t- 3),"Bakool_rain"]
    H<- rain.long[(t- 1),"Banaadir_rain"]
    I<- tail(movavg(rain.long[(t-3):(t- 1),"Bakool_rain"],2,type="w"),1)
    J<- before.long[(t- 1),"Mudug_BeforeRegion"]
    K<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    L<- tail(movavg(goats.long[(t-3):(t- 1),"Banadir_goatprice"],2,type="w"),1)
    M<- max(D, E,na.rm=TRUE)
    N<- max(sum(1411.63624927552 , 0.228713691541392*H*I,na.rm=TRUE), 2.37000840531761e-5*J*K,na.rm=TRUE)
    O<- max(sum(7.17944192524478*C*M , -G,na.rm=TRUE), N,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 2.72281899664567*A , B , O , -0.000684343134577369*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BR3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    B<- future.long[(t- 1),"Bari_FutureRegion"]
    C<- future.long[(t- 7),"Galgaduud_FutureRegion"]
    D<- median(conflicts.long[(t-13):(t- 1),"Bay_Conflict"], na.rm=TRUE)
    E<- fatalities.long[(t- 1),"Shabeellaha_Hoose_Fatalities"]
    G<- future.long[(t- 8),"Galgaduud_FutureRegion"]
    H<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    I<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    J<- future.long[(t- 1),"Bari_FutureRegion"]
    K<- future.long[(t- 7),"Galgaduud_FutureRegion"]
    L<- median(conflicts.long[(t-13):(t- 1),"Bay_Conflict"], na.rm=TRUE)
    M<- tail(movavg(before.long[(t-10):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], 9,type="m"),1)
    N<- future.long[(t- 6),"Galgaduud_FutureRegion"]
    O<- rain.long[(t- 1),"Bay_rain"]
    P<- median(future.long[(t-7):(t- 1),"Galgaduud_FutureRegion"], na.rm=TRUE)
    Q<- median(before.long[(t-17):(t- 1),"Mudug_BeforeRegion"], na.rm=TRUE)
    if ( is.na(0.0104265994047304*C) || is.na( D)){R<-0}
    else if(0.0104265994047304*C> D){R<-1 }
    else{R<-0 }
    S<- asinh(H)
    if ( is.na(0.0104265994047304*K) || is.na( L)){U<-0}
    else if(0.0104265994047304*K> L){U<-1 }
    else{U<-0 }
    if ( is.na(E) ){V<-sum( 0.0104265994047304*I*J , 689*U,na.rm=TRUE)}
    else if(E>0){V<- G*S }
    else{V<-sum( 0.0104265994047304*I*J , 689*U,na.rm=TRUE) }
    W<- tan(-0.0785949746021708*O*P)
    X<- max(sum(0.0104265994047304*A*B , 689*R , V , -M,na.rm=TRUE), N*W,na.rm=TRUE)
    Y<- max(X, Q,na.rm=TRUE)
    FIN <-Y
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BR4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- mean(stations.long[(t-11):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    B<- mean(before.long[(t-17):(t- 1),"Bari_BeforeRegion"], na.rm=TRUE)
    C<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    D<- rain.long[(t- 1),"Banaadir_rain"]
    E<- before.long[(t- 1),"Mudug_BeforeRegion"]
    G<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    H<- mean(before.long[(t-4):(t- 1),"Banadir_BeforeRegion"], na.rm=TRUE)
    I<- tail(movavg(current.long[(t-9):(t- 1),"Awdal_CurrentRegion"], 8,type="m"),1)
    J<- future.long[(t- 1),"Mudug_FutureRegion"]
    K<- before.long[(t- 1),"Mudug_BeforeRegion"]
    L<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    M<- mean(stations.long[(t-11):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    N<- median(stations.long[(t-11):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    O<- atan2(I, J^2*K)
    P<- max(sum(2.69936844830397e-9*E*G*H , 5286.88682715681*O , L,na.rm=TRUE), M^N,na.rm=TRUE)
    Q<- max(4.93740100090907*C*D, P,na.rm=TRUE)
    R<- max(A*B, Q,na.rm=TRUE)
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( R , -112.248573613173,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BR5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 16),"Bakool_CurrentRegion"]
    B<- mean(before.long[(t-17):(t- 1),"Bari_BeforeRegion"], na.rm=TRUE)
    C<- median(stations.long[(t-8):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    D<- fatalities.long[(t- 4),"Bari_Fatalities"]
    E<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    G<- future.long[(t- 1),"Bari_FutureRegion"]
    H<- fatalities.long[(t- 11),"Bakool_Fatalities"]
    I<- fatalities.long[(t- 4),"Bari_Fatalities"]
    J<- fatalities.long[(t- 4),"Bari_Fatalities"]
    K<- fatalities.long[(t- 11),"Bakool_Fatalities"]
    L<- fatalities.long[(t- 4),"Bari_Fatalities"]
    M<- before.long[(t- 16),"Gedo_BeforeRegion"]
    N<- before.long[(t- 14),"Mudug_BeforeRegion"]
    O<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    P<- max(H, I,na.rm=TRUE)
    Q<- max(K, L,na.rm=TRUE)
    R<- max(P,sum( J*Q , -9.49565604120954*M,na.rm=TRUE),na.rm=TRUE)
    if ( is.na(D) ){S<- N}
    else if(D>0){S<-sum( 0.0082128619514673*E*G , R,na.rm=TRUE) }
    else{S<- N }
    U<- max(A,sum( 2000.41844243911 , B*C , S,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 1.09635569029469*U , -2300.83168555438 , -0.468842601014377*O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BR6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    B<- rain.long[(t- 1),"Bakool_rain"]
    C<- tail(movavg(current.long[(t-9):(t- 1),"Gedo_CurrentRegion"], 8,type="m"),1)
    D<- fatalities.long[(t- 13),"Bakool_Fatalities"]
    E<- tail(movavg(rain.long[(t-9):(t- 1),"Bakool_rain"], 8,type="m"),1)
    G<- before.long[(t- 12),"Sanaag_BeforeRegion"]
    H<- future.long[(t- 5),"Nugaal_FutureRegion"]
    I<- before.long[(t- 1),"Jubbada_Hoose_BeforeRegion"]
    J<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    K<- before.long[(t- 12),"Bari_BeforeRegion"]
    L<- mean(future.long[(t-7):(t- 1),"Nugaal_FutureRegion"], na.rm=TRUE)
    M<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    N<- mean(current.long[(t-4):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    O<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    P<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    Q<- median(before.long[(t-11):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    R<- tan(C)
    S<- max(sum(A*B , 1.40292301897869*R , D*E , G,na.rm=TRUE), H,na.rm=TRUE)
    U<- max(sum(S , -I , -J,na.rm=TRUE),sum( K , L,na.rm=TRUE),na.rm=TRUE)
    V<- max(sum(U , -M,na.rm=TRUE),sum( N , -O*P,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( V , -Q,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BR7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 15),"Hiiraan_Fatalities"]
    B<- fatalities.long[(t- 14),"Jubbada_Dhexe_Fatalities"]
    C<- median(fatalities.long[(t-11):(t- 1),"Gedo_Fatalities"], na.rm=TRUE)
    D<- fatalities.long[(t- 5),"Sanaag_Fatalities"]
    E<- fatalities.long[(t- 7),"Jubbada_Dhexe_Fatalities"]
    G<- mean(current.long[(t-4):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    H<- before.long[(t- 1),"Awdal_BeforeRegion"]
    I<- fatalities.long[(t- 5),"Togdheer_Fatalities"]
    J<- fatalities.long[(t- 14),"Jubbada_Dhexe_Fatalities"]
    K<- median(fatalities.long[(t-11):(t- 1),"Gedo_Fatalities"], na.rm=TRUE)
    L<- fatalities.long[(t- 5),"Sanaag_Fatalities"]
    M<- fatalities.long[(t- 14),"Jubbada_Dhexe_Fatalities"]
    N<- median(fatalities.long[(t-11):(t- 1),"Gedo_Fatalities"], na.rm=TRUE)
    O<- fatalities.long[(t- 5),"Sanaag_Fatalities"]
    P<- fatalities.long[(t- 7),"Jubbada_Dhexe_Fatalities"]
    Q<- mean(current.long[(t-4):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    R<- before.long[(t- 1),"Awdal_BeforeRegion"]
    S<- fatalities.long[(t- 5),"Togdheer_Fatalities"]
    U<- fatalities.long[(t- 14),"Jubbada_Dhexe_Fatalities"]
    V<- median(fatalities.long[(t-11):(t- 1),"Gedo_Fatalities"], na.rm=TRUE)
    W<- fatalities.long[(t- 4),"Sanaag_Fatalities"]
    X<- future.long[(t- 7),"Galgaduud_FutureRegion"]
    Y<- mean(fatalities.long[(t-6):(t- 1),"Woqooyi_Galbeed_Fatalities"], na.rm=TRUE)
    Z<- fatalities.long[(t- 4),"Hiiraan_Fatalities"]
    AA<- max(125.55338060307, I*J*K,na.rm=TRUE)
    BB<- max(L, 1.29368230705319,na.rm=TRUE)
    CC<- max(125.55338060307, S*U*V,na.rm=TRUE)
    if ( is.na(sum(M*N , O^2*P , Q , -R , -CC,na.rm=TRUE)) ){DD<- Z}
    else if(sum(M*N , O^2*P , Q , -R , -CC,na.rm=TRUE)>0){DD<- W*X*Y }
    else{DD<- Z }
    EE<- abs(sum(B*C , D^2*E , G , -H , -AA , -BB*DD,na.rm=TRUE))
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(E)){E <- 0 }
    FIN <-sum( A , EE,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BR8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    B<- rain.long[(t- 2),"Bakool_rain"]
    C<- tail(movavg(conflicts.long[(t-3):(t- 1),"Awdal_Conflict"], 2,type="m"),1)
    D<- current.long[(t- 1),"Nugaal_CurrentRegion"]
    E<- before.long[(t- 16),"Bakool_BeforeRegion"]
    G<- rain.long[(t- 1),"Bakool_rain"]
    H<- before.long[(t- 16),"Bakool_BeforeRegion"]
    I<- rain.long[(t- 1),"Bakool_rain"]
    J<- rain.long[(t- 1),"Banaadir_rain"]
    K<- current.long[(t- 2),"Awdal_CurrentRegion"]
    L<- mean(future.long[(t-7):(t- 1),"Nugaal_FutureRegion"], na.rm=TRUE)
    M<- mean(before.long[(t-3):(t- 1),"Awdal_BeforeRegion"], na.rm=TRUE)
    N<- tail(movavg(future.long[(t-3):(t- 1),"Jubbada_Dhexe_FutureRegion"], 2,type="m"),1)
    O<- max(sum(0.39258503544148*E , -1621.39481108002,na.rm=TRUE), G,na.rm=TRUE)
    P<- tan(0.39258503544148*H)
    Q<- max(sum(0.246466099690165*D , O,na.rm=TRUE), P,na.rm=TRUE)
    R<- max(sum(A*B*C , Q,na.rm=TRUE),sum( 0.163413768479795*I*J , K , L , -M , -N,na.rm=TRUE),na.rm=TRUE)
    FIN <-R
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BR9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- median(before.long[(t-16):(t- 1),"Galgaduud_BeforeRegion"], na.rm=TRUE)
    B<- future.long[(t- 8),"Galgaduud_FutureRegion"]
    C<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    D<- tail(movavg(future.long[(t-7):(t- 1),"Bari_FutureRegion"], 6,type="m"),1)
    E<- median(before.long[(t-13):(t- 1),"Banadir_BeforeRegion"], na.rm=TRUE)
    G<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    H<- rain.long[(t- 1),"Banaadir_rain"]
    I<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    J<- mean(future.long[(t-7):(t- 1),"Mudug_FutureRegion"], na.rm=TRUE)
    K<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    L<- future.long[(t- 8),"Galgaduud_FutureRegion"]
    M<- future.long[(t- 8),"Galgaduud_FutureRegion"]
    N<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    O<- future.long[(t- 1),"Togdheer_FutureRegion"]
    P<- asinh(C)
    Q<- max(0.0772124623430678*E, 5.4743723919641*G*H,na.rm=TRUE)
    R<- tan(0.00097714782428366*K)
    S<- max(3.10827846973248e-5*I*J, R,na.rm=TRUE)
    U<- asinh(N)
    V<- max(sum(B*P , D,na.rm=TRUE),sum( Q , S , -L , -M*U,na.rm=TRUE),na.rm=TRUE)
    W<- max(A,sum( V , -O,na.rm=TRUE),na.rm=TRUE)
    FIN <-W
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BR10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- median(current.long[(t-13):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    B<- conflicts.long[(t- 7),"Woqooyi_Galbeed_Conflict"]
    C<- future.long[(t- 1),"Bari_FutureRegion"]
    D<- current.long[(t- 1),"Mudug_CurrentRegion"]
    E<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    G<- future.long[(t- 7),"Awdal_FutureRegion"]
    H<- goats.long[(t- 1),"Banadir_goatprice"]
    I<- fatalities.long[(t- 7),"Hiiraan_Fatalities"]
    J<- median(current.long[(t-13):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    K<- median(current.long[(t-13):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    L<- conflicts.long[(t- 7),"Woqooyi_Galbeed_Conflict"]
    M<- future.long[(t- 1),"Bari_FutureRegion"]
    N<- current.long[(t- 1),"Mudug_CurrentRegion"]
    O<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    P<- future.long[(t- 7),"Awdal_FutureRegion"]
    Q<- goats.long[(t- 1),"Banadir_goatprice"]
    R<- logistic(B)
    S<- min(11701, 340.170458961684*G,na.rm=TRUE)
    U<- max(0.659032549717203*C,sum( 2.66946563767136e-5*D*E , S , -0.00318829400288195*H,na.rm=TRUE),na.rm=TRUE)
    V<- tan(0.225691955479481*J)
    W<- logistic(L)
    X<- min(11701, 340.170458961684*P,na.rm=TRUE)
    Y<- max(0.659032549717203*M,sum( 2.66946563767136e-5*N*O , X , -0.00318829400288195*Q,na.rm=TRUE),na.rm=TRUE)
    Z<- tan(sum(0.225691955479481*K , W*Y,na.rm=TRUE))
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(Z)){Z <- 0 }
    FIN <-sum( 0.225691955479481*A , R*U , I , V , Z,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BRJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 13),"Bakool_Fatalities"]
    B<- tail(movavg(rain.long[(t-8):(t- 1),"Bakool_rain"], 7,type="m"),1)
    C<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    G<- fatalities.long[(t- 11),"Jubbada_Dhexe_Fatalities"]
    H<- fatalities.long[(t- 13),"Bakool_Fatalities"]
    I<- future.long[(t- 1),"Gedo_FutureRegion"]
    J<- median(rain.long[(t-4):(t- 1),"Shabeellaha_Dhexe_rain"], na.rm=TRUE)
    K<- before.long[(t- 3),"Hiiraan_BeforeRegion"]
    L<- tail(movavg(future.long[(t-7):(t- 1),"Gedo_FutureRegion"], 6,type="m"),1)
    M<- median(current.long[(t-14):(t- 1),"Sool_CurrentRegion"], na.rm=TRUE)
    N<- future.long[(t- 16),"Woqooyi_Galbeed_FutureRegion"]
    O<- max(A*B,sum( 1.65979602121784*C , 0.000283238292393178*D*E , 0.757465891594758*G*H , -I*J,na.rm=TRUE),na.rm=TRUE)
    P<- max(sum(1.17318833164244*O , -K , -L,na.rm=TRUE), M,na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( P , -N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BRJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- before.long[(t- 1),"Bari_BeforeRegion"]
    C<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    D<- rain.long[(t- 1),"Bakool_rain"]
    E<- rain.long[(t- 1),"Banaadir_rain"]
    G<- future.long[(t- 7),"Galgaduud_FutureRegion"]
    H<- rain.long[(t- 1),"Shabeellaha_Hoose_rain"]
    I<- rain.long[(t- 1),"Banaadir_rain"]
    J<- mean(current.long[(t-3):(t- 1),"Hiiraan_CurrentRegion"], na.rm=TRUE)
    K<- future.long[(t- 7),"Galgaduud_FutureRegion"]
    L<- rain.long[(t- 1),"Shabeellaha_Hoose_rain"]
    M<- rain.long[(t- 1),"Banaadir_rain"]
    N<- future.long[(t- 7),"Galgaduud_FutureRegion"]
    O<- rain.long[(t- 1),"Shabeellaha_Hoose_rain"]
    P<- rain.long[(t- 1),"Banaadir_rain"]
    Q<- mean(current.long[(t-3):(t- 1),"Hiiraan_CurrentRegion"], na.rm=TRUE)
    R<- current.long[(t- 15),"Bakool_CurrentRegion"]
    S<- future.long[(t- 16),"Hiiraan_FutureRegion"]
    U<- max(0.000396731069879474*B*C, 0.161874855147734*D*E,na.rm=TRUE)
    if(is.infinite(U)){U<-0}
    V<- max(0.452260998555364*A, U,na.rm=TRUE)
    if(is.infinite(V)){V<-0}
    if ( is.na(H) || is.na( I)){W<-0}
    else if(H<= I){W<-1 }
    else{W<-0 }
    X<- min(G*W, J,na.rm=TRUE)
    if(is.infinite(X)){ X<-0}
    if ( is.na(L) || is.na( M)){Y<-0}
    else if(L<= M){Y<-1 }
    else{Y<-0 }
    if ( is.na(O) || is.na( P)){Z<-0}
    else if(O<= P){Z<-1 }
    else{Z<-0 }
    AA<- min(N*Z, Q,na.rm=TRUE)
    if(is.infinite(AA)){AA<-0}
    BB<- max(sum(K*Y , AA,na.rm=TRUE), 0.000147820220497137*R*S,na.rm=TRUE)
    if(is.infinite(BB)){BB<-0}
    CC<- max(V,sum( X , BB,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(CC)){CC<-0}
    FIN <-CC
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BRJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    B<- tail(movavg(conflicts.long[(t-6):(t- 1),"Sanaag_Conflict"],5,type="w"),1)
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
    O<- tail(movavg(goats.long[(t-16):(t- 1),"Banadir_goatprice"],15,type="w"),1)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BRJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- mean(stations.long[(t-11):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    B<- mean(before.long[(t-17):(t- 1),"Bari_BeforeRegion"], na.rm=TRUE)
    C<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    D<- rain.long[(t- 1),"Banaadir_rain"]
    E<- before.long[(t- 1),"Mudug_BeforeRegion"]
    G<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    H<- mean(before.long[(t-4):(t- 1),"Banadir_BeforeRegion"], na.rm=TRUE)
    I<- tail(movavg(current.long[(t-9):(t- 1),"Awdal_CurrentRegion"], 8,type="m"),1)
    J<- future.long[(t- 1),"Mudug_FutureRegion"]
    K<- before.long[(t- 1),"Mudug_BeforeRegion"]
    L<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    M<- mean(stations.long[(t-11):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    N<- median(stations.long[(t-11):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    O<- atan2(I, J^2*K)
    P<- max(sum(2.69936844830397e-9*E*G*H , 5286.88682715681*O , L,na.rm=TRUE), M^N,na.rm=TRUE)
    Q<- max(4.93740100090907*C*D, P,na.rm=TRUE)
    R<- max(A*B, Q,na.rm=TRUE)
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( R , -112.248573613173,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BRJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 16),"Bakool_CurrentRegion"]
    B<- mean(before.long[(t-17):(t- 1),"Bari_BeforeRegion"], na.rm=TRUE)
    C<- median(stations.long[(t-8):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    D<- fatalities.long[(t- 4),"Bari_Fatalities"]
    E<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    G<- future.long[(t- 1),"Bari_FutureRegion"]
    H<- fatalities.long[(t- 11),"Bakool_Fatalities"]
    I<- fatalities.long[(t- 4),"Bari_Fatalities"]
    J<- fatalities.long[(t- 4),"Bari_Fatalities"]
    K<- fatalities.long[(t- 11),"Bakool_Fatalities"]
    L<- fatalities.long[(t- 4),"Bari_Fatalities"]
    M<- before.long[(t- 16),"Gedo_BeforeRegion"]
    N<- before.long[(t- 14),"Mudug_BeforeRegion"]
    O<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    P<- max(H, I,na.rm=TRUE)
    Q<- max(K, L,na.rm=TRUE)
    R<- max(P,sum( J*Q , -9.49565604120954*M,na.rm=TRUE),na.rm=TRUE)
    if ( is.na(D) ){S<- N}
    else if(D>0){S<-sum( 0.0082128619514673*E*G , R,na.rm=TRUE) }
    else{S<- N }
    U<- max(A,sum( 2000.41844243911 , B*C , S,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 1.09635569029469*U , -2300.83168555438 , -0.468842601014377*O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BRJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    B<- future.long[(t- 1),"Mudug_FutureRegion"]
    C<- mean(fatalities.long[(t-9):(t- 1),"Gedo_Fatalities"], na.rm=TRUE)
    D<- rain.long[(t- 1),"Bakool_rain"]
    E<- fatalities.long[(t- 1),"Shabeellaha_Hoose_Fatalities"]
    G<- fatalities.long[(t- 11),"Galguduud_Fatalities"]
    H<- fatalities.long[(t- 13),"Galguduud_Fatalities"]
    I<- before.long[(t- 1),"Bari_BeforeRegion"]
    J<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    K<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    L<- fatalities.long[(t- 13),"Galguduud_Fatalities"]
    M<- mean(future.long[(t-13):(t- 1),"Shabeellaha_Hoose_FutureRegion"], na.rm=TRUE)
    N<- tail(movavg(current.long[(t-13):(t- 1),"Shabeellaha_Dhexe_CurrentRegion"], 12,type="m"),1)
    O<- erfc(B)
    P<- sinh(0.0449227893781295*D)
    Q<- max(sum(0.124510485802*A , 38757.1746286019*O , C,na.rm=TRUE), P,na.rm=TRUE)
    if ( is.na(K) ){R<- M}
    else if(K>0){R<- L }
    else{R<- M }
    if ( is.na(H) ){S<- R}
    else if(H>0){S<- 0.00038385493782484*I*J }
    else{S<- R }
    if ( is.na(G) ){U<- N}
    else if(G>0){U<- S }
    else{U<- N }
    if ( is.na(E) ){V<- 1256.71505358371}
    else if(E>0){V<- U }
    else{V<- 1256.71505358371 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(V)){V <- 0 }
    FIN <-sum( Q , V,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BRJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- median(current.long[(t-14):(t- 1),"Sool_CurrentRegion"], na.rm=TRUE)
    B<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    C<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    G<- rain.long[(t- 8),"Shabeellaha_Hoose_rain"]
    H<- conflicts.long[(t- 14),"Bakool_Conflict"]
    I<- tail(movavg(rain.long[(t-13):(t- 1),"Bakool_rain"], 12,type="m"),1)
    J<- rain.long[(t- 6),"Hiiraan_rain"]
    K<- tail(movavg(conflicts.long[(t-12):(t- 1),"Sool_Conflict"], 11,type="m"),1)
    L<- mean(future.long[(t-7):(t- 1),"Bay_FutureRegion"], na.rm=TRUE)
    M<- mean(rain.long[(t-5):(t- 1),"Shabeellaha_Hoose_rain"], na.rm=TRUE)
    N<- mean(conflicts.long[(t-7):(t- 1),"Togdheer_Conflict"], na.rm=TRUE)
    O<- sinh(K)
    P<- max(A,sum( 0.280450228557511*B*C , 0.000311870015569877*D*E , 0.103107065265851*G*H*I , J , O , -L,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( P , -M*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BRJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- median(current.long[(t-14):(t- 1),"Sool_CurrentRegion"], na.rm=TRUE)
    B<- tail(movavg(future.long[(t-7):(t- 1),"Bari_FutureRegion"], 6,type="m"),1)
    C<- fatalities.long[(t- 10),"Hiiraan_Fatalities"]
    D<- fatalities.long[(t- 11),"Bakool_Fatalities"]
    E<- median(rain.long[(t-9):(t- 1),"Banaadir_rain"], na.rm=TRUE)
    G<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    H<- tail(movavg(conflicts.long[(t-7):(t- 1),"Sanaag_Conflict"],6,type="w"),1)
    I<- conflicts.long[(t- 9),"Shabeellaha_Hoose_Conflict"]
    J<- fatalities.long[(t- 13),"Bakool_Fatalities"]
    K<- conflicts.long[(t- 9),"Shabeellaha_Hoose_Conflict"]
    L<- conflicts.long[(t- 9),"Shabeellaha_Hoose_Conflict"]
    M<- fatalities.long[(t- 13),"Bakool_Fatalities"]
    N<- conflicts.long[(t- 14),"Sanaag_Conflict"]
    O<- tail(movavg(current.long[(t-5):(t- 1),"Shabeellaha_Dhexe_CurrentRegion"],4,type="w"),1)
    P<- future.long[(t- 16),"Woqooyi_Galbeed_FutureRegion"]
    Q<- sin(K)
    R<- exp(N)
    S<- max(G*H,sum( I*J*Q , L , M , R,na.rm=TRUE),na.rm=TRUE)
    U<- max(B,sum( 5.95606539436068*C , D*E , 0.829524458169284*S , -O,na.rm=TRUE),na.rm=TRUE)
    V<- max(A, U,na.rm=TRUE)
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( V , -P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BRJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- median(current.long[(t-10):(t- 1),"Sool_CurrentRegion"], na.rm=TRUE)
    B<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    C<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    G<- before.long[(t- 13),"Bakool_BeforeRegion"]
    H<- before.long[(t- 6),"Hiiraan_BeforeRegion"]
    I<- mean(rain.long[(t-4):(t- 1),"Bakool_rain"], na.rm=TRUE)
    J<- mean(rain.long[(t-4):(t- 1),"Banaadir_rain"], na.rm=TRUE)
    K<- tail(movavg(before.long[(t-3):(t- 1),"Jubbada_Hoose_BeforeRegion"], 2,type="m"),1)
    L<- future.long[(t- 16),"Woqooyi_Galbeed_FutureRegion"]
    M<- max(sum(0.132297625970236*C , 0.000389959649841556*D*E,na.rm=TRUE), 0.145650392072518*G,na.rm=TRUE)
    N<- max(sum(0.145650392072518*B , M , -H,na.rm=TRUE),sum( 1.38552208289426*I*J , -3.85990809527555*K,na.rm=TRUE),na.rm=TRUE)
    O<- max(A, N,na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( O , -L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BRJUN10arrivals <- function(start, end){
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}



