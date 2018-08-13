
modelarrivals_SOminus1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 5),"Bari_CurrentRegion"]
    B<- median(fatalities.long[(t-10):(t- 1),"Sanaag_Fatalities"], na.rm=TRUE)
    C<- median(current.long[(t-6):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    D<- future.long[(t- 1),"Sanaag_FutureRegion"]
    E<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    G<- conflicts.long[(t- 4),"Woqooyi_Galbeed_Conflict"]
    H<- future.long[(t- 14),"Nugaal_FutureRegion"]
    I<- current.long[(t- 10),"Sanaag_CurrentRegion"]
    J<- median(fatalities.long[(t-15):(t- 1),"Sanaag_Fatalities"], na.rm=TRUE)
    K<- before.long[(t- 1),"Bari_BeforeRegion"]
    L<- median(current.long[(t-5):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    M<- future.long[(t- 1),"Togdheer_FutureRegion"]
    N<- median(fatalities.long[(t-10):(t- 1),"Sanaag_Fatalities"], na.rm=TRUE)
    O<- before.long[(t- 1),"Bari_BeforeRegion"]
    P<- max(A*B,sum( 0.310290584690802*C , 0.00104933044062936*D*E , G*H , I*J , 1.41145509863705e-6*K^2*L , M , N,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( P , -O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SOminus2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 2),"Sanaag_CurrentRegion"]
    B<- before.long[(t- 2),"Bay_BeforeRegion"]
    C<- tail(movavg(current.long[(t-17):(t- 2),"Shabeellaha_Hoose_CurrentRegion"],15,type="w"),1)
    D<- fatalities.long[(t- 2),"Mudug_Fatalities"]
    E<- tail(movavg(conflicts.long[(t-6):(t- 2),"Togdheer_Conflict"], 4,type="m"),1)
    G<- before.long[(t- 2),"Bay_BeforeRegion"]
    H<- current.long[(t- 2),"Woqooyi_Galbeed_CurrentRegion"]
    I<- median(rain.long[(t-16):(t- 2),"Sool_rain"], na.rm=TRUE)
    J<- before.long[(t- 3),"Nugaal_BeforeRegion"]
    K<- median(rain.long[(t-16):(t- 2),"Sool_rain"], na.rm=TRUE)
    L<- before.long[(t- 2),"Bay_BeforeRegion"]
    M<- tail(movavg(current.long[(t-17):(t- 2),"Shabeellaha_Hoose_CurrentRegion"],15,type="w"),1)
    N<- current.long[(t- 10),"Sanaag_CurrentRegion"]
    O<- before.long[(t- 2),"Bay_BeforeRegion"]
    P<- current.long[(t- 2),"Woqooyi_Galbeed_CurrentRegion"]
    Q<- conflicts.long[(t- 2),"Shabeellaha_Dhexe_Conflict"]
    R<- before.long[(t- 2),"Sanaag_BeforeRegion"]
    if ( is.na(I) ){S<- 0.000164607543480734*L*M}
    else if(I>0){S<- J*K }
    else{S<- 0.000164607543480734*L*M }
    U<- atan2(0.000150361101245769*O*P, Q)
    V<- max(sum(D*E , 0.000150361101245769*G*H , S,na.rm=TRUE), N*U,na.rm=TRUE)
    W<- max(sum(2.42956499491305*A , -0.000164607543480734*B*C,na.rm=TRUE), V,na.rm=TRUE)
    if(is.infinite(W)){W <- 0 }
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( W , -1.70566855964028*R,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SO1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 3),"Mudug_Fatalities"]
    B<- conflicts.long[(t- 12),"Togdheer_Conflict"]
    C<- median(current.long[(t-5):(t- 1),"Sanaag_CurrentRegion"], na.rm=TRUE)
    D<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    E<- current.long[(t- 10),"Sanaag_CurrentRegion"]
    G<- before.long[(t- 1),"Bari_BeforeRegion"]
    H<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    I<- before.long[(t- 1),"Bari_BeforeRegion"]
    J<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    K<- fatalities.long[(t- 13),"Togdheer_Fatalities"]
    L<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    M<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    N<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    O<- before.long[(t- 1),"Gedo_BeforeRegion"]
    P<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    Q<- max(sum(0.644180977799172*C , D,na.rm=TRUE), E,na.rm=TRUE)
    R<- max(N, O,na.rm=TRUE)
    S<- min(sum(0.00262130974276893*I*J*K , -0.644180977799172*L,na.rm=TRUE), 0.0198641108398451*M*R,na.rm=TRUE)
    U<- min(0.00262130974276893*G*H, S,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( A*B , Q , U , -0.20324382055947*P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SO2arrivals <- function(start, end){
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SO3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- conflicts.long[(t- 9),"Nugaal_Conflict"]
    B<- tail(movavg(fatalities.long[(t-4):(t- 1),"Hiiraan_Fatalities"], 3,type="m"),1)
    C<- current.long[(t- 1),"Bay_CurrentRegion"]
    D<- future.long[(t- 1),"Sool_FutureRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- current.long[(t- 1),"Bay_CurrentRegion"]
    H<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    I<- before.long[(t- 1),"Bari_BeforeRegion"]
    J<- current.long[(t- 1),"Bay_CurrentRegion"]
    K<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    L<- current.long[(t- 10),"Sanaag_CurrentRegion"]
    M<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    N<- median(stations.long[(t-4):(t- 1),"Gedo_DollowStation_Juba_River"], na.rm=TRUE)
    O<- tanh(0.000433464570266807*D)
    P<- min(H, 5.22774742602611e-11*I^2*J*K,na.rm=TRUE)
    Q<- max(589,sum( 1.30950116720805*L , M*N,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( A*B , C*O , 5.22774742602611e-11*E^2*G*P , Q , -480.568673485203,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SO4arrivals <- function(start, end){
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SO5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- mean(conflicts.long[(t-11):(t- 1),"Togdheer_Conflict"], na.rm=TRUE)
    B<- tail(movavg(future.long[(t-3):(t- 1),"Sool_FutureRegion"], 2,type="m"),1)
    C<- before.long[(t- 1),"Bari_BeforeRegion"]
    D<- tail(movavg(current.long[(t-11):(t- 1),"Sanaag_CurrentRegion"], 10,type="m"),1)
    E<- future.long[(t- 11),"Galgaduud_FutureRegion"]
    G<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    H<- future.long[(t- 11),"Galgaduud_FutureRegion"]
    I<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    J<- mean(conflicts.long[(t-11):(t- 1),"Togdheer_Conflict"], na.rm=TRUE)
    K<- before.long[(t- 1),"Mudug_BeforeRegion"]
    L<- stations.long[(t- 1),"Gedo_DollowStation_Juba_River"]
    M<- before.long[(t- 1),"Bari_BeforeRegion"]
    N<- tail(movavg(current.long[(t-6):(t- 1),"Awdal_CurrentRegion"], 5,type="m"),1)
    O<- tan(708.919823159699*G)
    P<- tan(708.919823159699*I)
    Q<- factorial(J)
    if ( is.na(H*P) || is.na( Q)){R<-0}
    else if(H*P>= Q){R<-1 }
    else{R<-0 }
    S<- sinh(0.00113340372140156*M)
    if ( is.na(R) ){U<- 883.396694154249}
    else if(R>0){U<- K*L*S }
    else{U<- 883.396694154249 }
    V<- max(sum(A*B , 0.00113340372140156*C*D , E*O , -708.919823159699,na.rm=TRUE), U,na.rm=TRUE)
    W<- max(708.919823159699, V,na.rm=TRUE)
    X<- max(W, N,na.rm=TRUE)
    if(is.infinite(X)){X <- 0 }
    FIN <-sum( X , -614.908892497135,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SO6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 1),"Sool_FutureRegion"]
    B<- current.long[(t- 1),"Bay_CurrentRegion"]
    C<- before.long[(t- 1),"Bari_BeforeRegion"]
    D<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    E<- current.long[(t- 1),"Bay_CurrentRegion"]
    G<- conflicts.long[(t- 7),"Togdheer_Conflict"]
    H<- tail(movavg(fatalities.long[(t-4):(t- 1),"Hiiraan_Fatalities"], 3,type="m"),1)
    I<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    J<- rain.long[(t- 14),"Banaadir_rain"]
    K<- current.long[(t- 1),"Awdal_CurrentRegion"]
    L<- tail(movavg(stations.long[(t-6):(t- 1),"Gedo_LuuqStation_Juba_River"], 5,type="m"),1)
    M<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    N<- rain.long[(t- 14),"Banaadir_rain"]
    O<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    P<- max(G*H, 2.30064273252367*I*J,na.rm=TRUE)
    Q<- max(sum(0.00034160316994471*A*B , 5.31610951226317e-11*C^2*D*E , P,na.rm=TRUE), 0.627421584926285*K,na.rm=TRUE)
    R<- tan(sum(2.30064273252367*M*N , O,na.rm=TRUE))
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( Q , -L*R,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SO7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 10),"Hiiraan_Fatalities"]
    B<- fatalities.long[(t- 1),"Sool_Fatalities"]
    C<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    D<- tail(movavg(fatalities.long[(t-7):(t- 1),"Awdal_Fatalities"], 6,type="m"),1)
    E<- current.long[(t- 10),"Sanaag_CurrentRegion"]
    G<- fatalities.long[(t- 10),"Hiiraan_Fatalities"]
    H<- tail(movavg(current.long[(t-8):(t- 1),"Awdal_CurrentRegion"], 7,type="m"),1)
    I<- fatalities.long[(t- 10),"Hiiraan_Fatalities"]
    J<- current.long[(t- 10),"Sanaag_CurrentRegion"]
    K<- before.long[(t- 1),"Bari_BeforeRegion"]
    L<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    M<- future.long[(t- 1),"Sool_FutureRegion"]
    N<- current.long[(t- 1),"Bay_CurrentRegion"]
    O<- rain.long[(t- 1),"Hiiraan_rain"]
    P<- max(sum(I , -J,na.rm=TRUE),sum( 0.0021731724183305*K*L , 0.00061993342459698*M*N , -2541.75136988032,na.rm=TRUE),na.rm=TRUE)
    Q<- max(sum(B*C*D , E,na.rm=TRUE),sum( G , H , P , -66.4510283415142*O,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( A , Q,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SO8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- rain.long[(t- 1),"Jubbada_Dhexe_rain"]
    B<- tail(movavg(fatalities.long[(t-3):(t- 1),"Bari_Fatalities"], 2,type="m"),1)
    C<- median(fatalities.long[(t-8):(t- 1),"Woqooyi_Galbeed_Fatalities"], na.rm=TRUE)
    D<- tail(movavg(fatalities.long[(t-3):(t- 1),"Jubbada_Dhexe_Fatalities"], 2,type="m"),1)
    E<- tail(movavg(current.long[(t-16):(t- 1),"Sanaag_CurrentRegion"],15,type="w"),1)
    G<- before.long[(t- 1),"Bari_BeforeRegion"]
    H<- before.long[(t- 1),"Mudug_BeforeRegion"]
    I<- tail(movavg(current.long[(t-9):(t- 1),"Awdal_CurrentRegion"], 8,type="m"),1)
    J<- median(fatalities.long[(t-14):(t- 1),"Sanaag_Fatalities"], na.rm=TRUE)
    K<- tail(movavg(current.long[(t-16):(t- 1),"Sanaag_CurrentRegion"],15,type="w"),1)
    L<- fatalities.long[(t- 1),"Sool_Fatalities"]
    M<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    N<- rain.long[(t- 1),"Jubbada_Dhexe_rain"]
    O<- tail(movavg(fatalities.long[(t-3):(t- 1),"Bari_Fatalities"], 2,type="m"),1)
    P<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    Q<- (A*B)
    if ( is.na(2.05263260375539) || is.na( D)){R<-0}
    else if(2.05263260375539> D){R<-1 }
    else{R<-0 }
    S<- max(E, 0.00178181617781582*G*H,na.rm=TRUE)
    if(is.infinite(S)){S<-0}

    if ( is.na(R) ){U<- I}
    else if(R>0){U<- S }
    else{U<- I }
    V<- tan(N*O)
    W<- max(0.223422082682777*L*M, 1548.25407759254*V,na.rm=TRUE)
    if(is.infinite(W)){W<-0}
    
    X<- max(K, W,na.rm=TRUE)
    
    if(is.infinite(X)){X<-0}
    
    if ( is.na(J) ){Y<- P}
    else if(J>0){Y<- X }
    else{Y<- P }
    
    Z<- max(sum(Q^C , U,na.rm=TRUE), Y)
    FIN <-Z
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SO9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 1),"Sool_Fatalities"]
    B<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    C<- tail(movavg(fatalities.long[(t-7):(t- 1),"Awdal_Fatalities"], 6,type="m"),1)
    D<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    E<- current.long[(t- 1),"Banadir_CurrentRegion"]
    G<- tail(movavg(future.long[(t-3):(t- 1),"Sool_FutureRegion"], 2,type="m"),1)
    H<- before.long[(t- 1),"Bari_BeforeRegion"]
    I<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    J<- before.long[(t- 1),"Bay_BeforeRegion"]
    K<- fatalities.long[(t- 1),"Hiiraan_Fatalities"]
    L<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    M<- current.long[(t- 10),"Sanaag_CurrentRegion"]
    N<- future.long[(t- 1),"Sool_FutureRegion"]
    O<- current.long[(t- 10),"Sanaag_CurrentRegion"]
    P<- (0.00217046317823848*H*I)
    if ( is.na(J) || is.na( 2144.16146246824)){Q<-0}
    else if(J>= 2144.16146246824){Q<-1 }
    else{Q<-0 }
    R<- max(L, M,na.rm=TRUE)
    if(is.infinite(R)){R<-0}
    S<- max(sum(A*B*C , 1.32203011697754e-5*D*E*G , P^Q , K , R , -N,na.rm=TRUE), O, na.rm=TRUE)
    FIN <-S
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SO10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 9),"Sanaag_FutureRegion"]
    B<- fatalities.long[(t- 10),"Nugaal_Fatalities"]
    C<- fatalities.long[(t- 1),"Sool_Fatalities"]
    D<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    E<- tail(movavg(fatalities.long[(t-7):(t- 1),"Awdal_Fatalities"], 6,type="m"),1)
    G<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    H<- tail(movavg(current.long[(t-8):(t- 1),"Togdheer_CurrentRegion"],7,type="w"),1)
    I<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    J<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    K<- fatalities.long[(t- 3),"Nugaal_Fatalities"]
    L<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    M<- before.long[(t- 1),"Bari_BeforeRegion"]
    N<- median(conflicts.long[(t-4):(t- 1),"Nugaal_Conflict"], na.rm=TRUE)
    O<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    P<- future.long[(t- 8),"Sanaag_FutureRegion"]
    Q<- min(G, H,na.rm=TRUE)
    R<- O%% 1.63066702660064
    S<- max(2.3744203697664e-5*L^2*M*N*R, P,na.rm=TRUE)
    U<- max(J*K, S,na.rm=TRUE)
    V<- max(I, U,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(V)){V <- 0 }
    FIN <-sum( A*B , C*D*E , Q , V,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SOJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- tail(movavg(fatalities.long[(t-7):(t- 1),"Awdal_Fatalities"], 6,type="m"),1)
    B<- current.long[(t- 10),"Sanaag_CurrentRegion"]
    C<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    D<- fatalities.long[(t- 1),"Sool_Fatalities"]
    E<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    G<- tail(movavg(fatalities.long[(t-7):(t- 1),"Awdal_Fatalities"], 6,type="m"),1)
    H<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    I<- future.long[(t- 1),"Sool_FutureRegion"]
    J<- current.long[(t- 1),"Awdal_CurrentRegion"]
    K<- before.long[(t- 1),"Bari_BeforeRegion"]
    L<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    M<- mean(current.long[(t-13):(t- 1),"Galgaduud_CurrentRegion"], na.rm=TRUE)
    N<- before.long[(t- 1),"Bay_BeforeRegion"]
    O<- before.long[(t- 9),"Sanaag_BeforeRegion"]
    P<- atanh(A)
    if ( is.na(P) ){Q<- C}
    else if(P>0){Q<- 1.17942185729443*B }
    else{Q<- C }
    R<- max(D*E*G, 0.000168279745864028*H*I*J,na.rm=TRUE)
    if ( is.na(M) || is.na( N)){S<-0}
    else if(M< N){S<-1 }
    else{S<-0 }
    U<- max(0.00221732185749814*K*L*S, O,na.rm=TRUE)
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(U)){U <- 0 }
    FIN <-sum( Q , R , U,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SOJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- before.long[(t- 1),"Bari_BeforeRegion"]
    C<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    D<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    E<- before.long[(t- 7),"Galgaduud_BeforeRegion"]
    G<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    H<- G%% 0.476138970418332
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(H)){H <- 0 }
    FIN <-sum( 0.534873603715618*A , 2.85210136139383e-6*B*C*D , E*H,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SOJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Sool_BeforeRegion"]
    B<- future.long[(t- 16),"Mudug_FutureRegion"]
    C<- tail(movavg(current.long[(t-16):(t- 1),"Sanaag_CurrentRegion"],15,type="w"),1)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    FIN <-sum( 0.000458733666861666*A*B , C,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SOJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 1),"Banaadir_Fatalities"]
    B<- conflicts.long[(t- 11),"Jubbada_Dhexe_Conflict"]
    C<- fatalities.long[(t- 1),"Banaadir_Fatalities"]
    D<- before.long[(t- 1),"Sool_BeforeRegion"]
    E<- future.long[(t- 1),"Bari_FutureRegion"]
    G<- before.long[(t- 1),"Bari_BeforeRegion"]
    H<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    I<- tail(movavg(current.long[(t-8):(t- 1),"Awdal_CurrentRegion"],7,type="w"),1)
    J<- conflicts.long[(t- 11),"Jubbada_Dhexe_Conflict"]
    K<- fatalities.long[(t- 10),"Awdal_Fatalities"]
    L<- current.long[(t- 10),"Sanaag_CurrentRegion"]
    M<- before.long[(t- 1),"Bari_BeforeRegion"]
    N<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    O<- tail(movavg(current.long[(t-8):(t- 1),"Awdal_CurrentRegion"],7,type="w"),1)
    P<- fatalities.long[(t- 1),"Banaadir_Fatalities"]
    Q<- fatalities.long[(t- 1),"Hiiraan_Fatalities"]
    R<- factorial(B)
    S<- max(1.14751604214374e-6*M*N*O, 2.10255014126494*P,na.rm=TRUE)
    U<- tan(S)
    if ( is.na(K) ){V<- U}
    else if(K>0){V<- L }
    else{V<- U }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( 2.10255014126494*A , 1.41037921699481*R , 1.65635235194669e-5*C*D*E , 1.14751604214374e-6*G*H*I , J*V , Q,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SOJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 1),"Banaadir_Fatalities"]
    B<- stations.long[(t- 3),"Gedo_DollowStation_Juba_River"]
    C<- rain.long[(t- 1),"Shabeellaha_Hoose_rain"]
    D<- before.long[(t- 8),"Sool_BeforeRegion"]
    E<- before.long[(t- 1),"Bay_BeforeRegion"]
    G<- mean(fatalities.long[(t-13):(t- 1),"Awdal_Fatalities"], na.rm=TRUE)
    H<- before.long[(t- 1),"Bari_BeforeRegion"]
    I<- before.long[(t- 1),"Sool_BeforeRegion"]
    J<- future.long[(t- 1),"Bari_FutureRegion"]
    K<- tail(movavg(fatalities.long[(t-4):(t- 1),"Nugaal_Fatalities"],3,type="w"),1)
    L<- future.long[(t- 1),"Gedo_FutureRegion"]
    M<- fatalities.long[(t- 1),"Sool_Fatalities"]
    N<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    O<- tail(movavg(fatalities.long[(t-7):(t- 1),"Awdal_Fatalities"], 6,type="m"),1)
    P<- log(K)
    Q<- max(sum(0.546224496002538*D , E*G , 5.95362730107974e-6*H*I*J , P , -L,na.rm=TRUE), M*N*O,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( A*B , C , Q,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SOJUN6arrivals <- function(start, end){
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SOJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- current.long[(t- 1),"Awdal_CurrentRegion"]
    C<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    D<- conflicts.long[(t- 17),"Sanaag_Conflict"]
    E<- before.long[(t- 3),"Bari_BeforeRegion"]
    G<- tail(movavg(before.long[(t-6):(t- 1),"Sanaag_BeforeRegion"], 5,type="m"),1)
    H<- future.long[(t- 1),"Sool_FutureRegion"]
    I<- current.long[(t- 1),"Awdal_CurrentRegion"]
    J<- conflicts.long[(t- 17),"Sanaag_Conflict"]
    K<- stations.long[(t- 1),"Gedo_BardheereStation_Juba_River"]
    L<- fatalities.long[(t- 1),"Sool_Fatalities"]
    M<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    N<- tail(movavg(fatalities.long[(t-7):(t- 1),"Awdal_Fatalities"], 6,type="m"),1)
    O<- rain.long[(t- 1),"Hiiraan_rain"]
    P<- sinh(K)
    Q<- max(sum(0.0945871302905339*G , 0.00320396289411279*H*I,na.rm=TRUE), J*P,na.rm=TRUE)
    R<- max(Q, L*M*N,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 2.66123846723373e-7*A*B*C*D , E , R , -O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SOJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    C<- before.long[(t- 1),"Bay_BeforeRegion"]
    D<- mean(future.long[(t-14):(t- 1),"Banadir_FutureRegion"], na.rm=TRUE)
    E<- future.long[(t- 1),"Sool_FutureRegion"]
    G<- future.long[(t- 1),"Sool_FutureRegion"]
    H<- current.long[(t- 1),"Awdal_CurrentRegion"]
    I<- fatalities.long[(t- 1),"Sool_Fatalities"]
    J<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    K<- tail(movavg(fatalities.long[(t-7):(t- 1),"Awdal_Fatalities"], 6,type="m"),1)
    L<- before.long[(t- 9),"Sanaag_BeforeRegion"]
    M<- fatalities.long[(t- 8),"Hiiraan_Fatalities"]
    N<- mean(fatalities.long[(t-4):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    O<- rain.long[(t- 14),"Banaadir_rain"]
    P<- tail(movavg(fatalities.long[(t-16):(t- 1),"Shabeellaha_Dhexe_Fatalities"], 15,type="m"),1)
    if ( is.na(C) || is.na( D)){Q<-0}
    else if(C> D){Q<-1 }
    else{Q<-0 }
    R<- max(0.00344343155623769*G*H, I*J*K,na.rm=TRUE)
    S<- max(E, R,na.rm=TRUE)
    U<- max(L, M*N,na.rm=TRUE)
    V<- max(U, O*P,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(V)){V <- 0 }
    FIN <-sum( 0.00221555555247787*A*B*Q , S , V,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SOJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 9),"Sanaag_BeforeRegion"]
    B<- tail(movavg(current.long[(t-17):(t- 1),"Togdheer_CurrentRegion"],16,type="w"),1)
    C<- mean(rain.long[(t-11):(t- 1),"Bari_rain"], na.rm=TRUE)
    D<- before.long[(t- 1),"Bay_BeforeRegion"]
    E<- future.long[(t- 1),"Bari_FutureRegion"]
    G<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    H<- future.long[(t- 7),"Togdheer_FutureRegion"]
    I<- median(rain.long[(t-6):(t- 1),"Woqooyi_Galbeed_rain"], na.rm=TRUE)
    J<- before.long[(t- 1),"Sool_BeforeRegion"]
    K<- mean(fatalities.long[(t-12):(t- 1),"Awdal_Fatalities"], na.rm=TRUE)
    L<- fatalities.long[(t- 1),"Sool_Fatalities"]
    M<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    N<- tail(movavg(fatalities.long[(t-7):(t- 1),"Awdal_Fatalities"], 6,type="m"),1)
    O<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    P<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    Q<- before.long[(t- 1),"Awdal_BeforeRegion"]
    R<- max(A, B,na.rm=TRUE)
    S<- cos(C)
    if ( is.na(S) ){U<- H}
    else if(S>0){U<- 1.07125113308919e-7*D*E*G }
    else{U<- H }
    if ( is.na(I) ){V<- L*M*N}
    else if(I>0){V<- J*K }
    else{V<- L*M*N }
    W<- tan(Q)
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(W)){W <- 0 }
    FIN <-sum( R , U , V , -O , -P*W,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SOJUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 1),"Sool_FutureRegion"]
    B<- current.long[(t- 1),"Awdal_CurrentRegion"]
    C<- fatalities.long[(t- 3),"Nugaal_Fatalities"]
    D<- fatalities.long[(t- 8),"Hiiraan_Fatalities"]
    E<- tail(movavg(conflicts.long[(t-4):(t- 1),"Jubbada_Dhexe_Conflict"], 3,type="m"),1)
    G<- current.long[(t- 2),"Sanaag_CurrentRegion"]
    H<- tail(movavg(fatalities.long[(t-11):(t- 1),"Hiiraan_Fatalities"],10,type="w"),1)
    I<- median(current.long[(t-17):(t- 1),"Jubbada_Hoose_CurrentRegion"], na.rm=TRUE)
    J<- before.long[(t- 1),"Bay_BeforeRegion"]
    K<- before.long[(t- 1),"Bari_BeforeRegion"]
    L<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    M<- before.long[(t- 9),"Sanaag_BeforeRegion"]
    N<- rain.long[(t- 3),"Banaadir_rain"]
    if ( is.na(49.8619609692228) || is.na( H)){O<-0}
    else if(49.8619609692228< H){O<-1 }
    else{O<-0 }
    if ( is.na(I) || is.na( J)){P<-0}
    else if(I<= J){P<-1 }
    else{P<-0 }
    if ( is.na(P) ){Q<- M}
    else if(P>0){Q<- 0.00221766230098416*K*L }
    else{Q<- M }
    R<- max(2.39547562681625*G*O, Q,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 0.0036582576595216*A*B , C*D*E , R , -N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}


