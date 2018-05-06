
modelarrivals_LSminus1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- current.long[(t- 1),"Awdal_CurrentRegion"]
    C<- before.long[(t- 12),"Sool_BeforeRegion"]
    D<- tail(movavg(fatalities.long[(t-11):(t- 1),"Shabeellaha_Dhexe_Fatalities"], 10,type="m"),1)
    E<- mean(current.long[(t-8):(t- 1),"Bay_CurrentRegion"], na.rm=TRUE)
    G<- goats.long[(t- 1),"Mudug_goatprice"]
    H<- current.long[(t- 12),"Mudug_CurrentRegion"]
    I<- median(current.long[(t-11):(t- 1),"Shabeellaha_Dhexe_CurrentRegion"], na.rm=TRUE)
    J<- future.long[(t- 11),"Bari_FutureRegion"]
    K<- future.long[(t- 1),"Shabeellaha_Hoose_FutureRegion"]
    L<- goats.long[(t- 1),"Mudug_goatprice"]
    M<- future.long[(t- 11),"Bari_FutureRegion"]
    N<- current.long[(t- 12),"Mudug_CurrentRegion"]
    O<- future.long[(t- 12),"Sanaag_FutureRegion"]
    P<- conflicts.long[(t- 15),"Gedo_Conflict"]
    Q<- G%% 1.03822610056545
    R<- L%% 1.03822610056545
    if ( is.na(M) || is.na( N)){S<-0}
    else if(M< N){S<-1 }
    else{S<-0 }
    U<- max(K*R*S, O*P,na.rm=TRUE)
    V<- max(B,sum( C*D , E*Q , 0.00228623785044181*H*I , J , U , -1780.2101902798,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(V)){V <- 0 }
    FIN <-sum( A , V,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LSminus2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(before.long[(t-9):(t- 2),"Togdheer_BeforeRegion"], na.rm=TRUE)
    B<- fatalities.long[(t- 2),"Woqooyi_Galbeed_Fatalities"]
    C<- current.long[(t- 6),"Sool_CurrentRegion"]
    D<- current.long[(t- 2),"Awdal_CurrentRegion"]
    E<- future.long[(t- 12),"Sanaag_FutureRegion"]
    G<- current.long[(t- 2),"Sanaag_CurrentRegion"]
    H<- current.long[(t- 2),"Awdal_CurrentRegion"]
    I<- future.long[(t- 8),"Sanaag_FutureRegion"]
    J<- fatalities.long[(t- 11),"Nugaal_Fatalities"]
    K<- rain.long[(t- 14),"Jubbada_Hoose_rain"]
    L<- fatalities.long[(t- 2),"Sanaag_Fatalities"]
    M<- fatalities.long[(t- 2),"Jubbada_Dhexe_Fatalities"]
    N<- future.long[(t- 8),"Sanaag_FutureRegion"]
    O<- sin(0.972630893408568*H)
    P<- ceil(N)
    Q<- max(sum(8.48776459273081*A , B*C , 0.136248039481606*D*E , G*O , I*J*K,na.rm=TRUE), L^2*M*P,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}


modelarrivals_LS1arrivals <- function(start, end){
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LS2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    B<- median(current.long[(t-7):(t- 1),"Togdheer_CurrentRegion"], na.rm=TRUE)
    C<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    D<- before.long[(t- 1),"Bakool_BeforeRegion"]
    E<- fatalities.long[(t- 10),"Nugaal_Fatalities"]
    G<- current.long[(t- 14),"Sool_CurrentRegion"]
    H<- stations.long[(t- 10),"Gedo_LuuqStation_Juba_River"]
    I<- tail(movavg(current.long[(t-4):(t- 1),"Awdal_CurrentRegion"], 3,type="m"),1)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LS3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    B<- before.long[(t- 6),"Togdheer_BeforeRegion"]
    C<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    D<- mean(fatalities.long[(t-5):(t- 1),"Bakool_Fatalities"], na.rm=TRUE)
    E<- current.long[(t- 14),"Sanaag_CurrentRegion"]
    G<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    H<- future.long[(t- 2),"Nugaal_FutureRegion"]
    I<- stations.long[(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"]
    J<- current.long[(t- 1),"Bay_CurrentRegion"]
    K<- current.long[(t- 1),"Sool_CurrentRegion"]
    L<- max(sum(6.13326258706*A , 1.7842436939076*B,na.rm=TRUE), C*D,na.rm=TRUE)
    M<- max(0.903726437855907*G*H, 0.165418226210988*I*J,na.rm=TRUE)
    N<- max(0.00849127521846411*E^2, M,na.rm=TRUE)
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( L , N , -0.326990280655757*K,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LS4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Bakool_Fatalities"]
    B<- future.long[(t- 5),"Sanaag_FutureRegion"]
    C<- mean(conflicts.long[(t-12):(t- 1),"Jubbada_Dhexe_Conflict"], na.rm=TRUE)
    D<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    E<- rain.long[(t- 1),"Jubbada_Dhexe_rain"]
    G<- before.long[(t- 1),"Banadir_BeforeRegion"]
    H<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    I<- median(rain.long[(t-16):(t- 1),"Togdheer_rain"], na.rm=TRUE)
    J<- water.long[(t- 1),"Awdal_WaterDrumPrice"]
    K<- mean(current.long[(t-7):(t- 1),"Bay_CurrentRegion"], na.rm=TRUE)
    L<- before.long[(t- 1),"Awdal_BeforeRegion"]
    M<- mean(current.long[(t-8):(t- 1),"Bay_CurrentRegion"], na.rm=TRUE)
    N<- before.long[(t- 12),"Sool_BeforeRegion"]
    O<- tail(movavg(fatalities.long[(t-10):(t- 1),"Jubbada_Dhexe_Fatalities"], 9,type="m"),1)
    P<- mean(current.long[(t-5):(t- 1),"Bay_CurrentRegion"], na.rm=TRUE)
    if ( is.na(C) ){Q<- G}
    else if(C>0){Q<- D*E }
    else{Q<- G }
    R<- max(A*B, Q,na.rm=TRUE)
    S<- max(M, N*O,na.rm=TRUE)
    U<- max(sum(H*I , 0.00041332262454527*J*K , -24.621588843567*L,na.rm=TRUE), S,na.rm=TRUE)
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( R , U , -P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LS5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 6),"Bay_Conflict"]
    B<- future.long[(t- 12),"Sanaag_FutureRegion"]
    C<- conflicts.long[(t- 11),"Gedo_Conflict"]
    D<- tail(movavg(before.long[(t-3):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], 2,type="m"),1)
    E<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    G<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    H<- before.long[(t- 1),"Awdal_BeforeRegion"]
    I<- mean(conflicts.long[(t-5):(t- 1),"Jubbada_Dhexe_Conflict"], na.rm=TRUE)
    J<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    K<- tail(movavg(conflicts.long[(t-4):(t- 1),"Sool_Conflict"], 3,type="m"),1)
    L<- current.long[(t- 1),"Bay_CurrentRegion"]
    M<- mean(conflicts.long[(t-3):(t- 1),"Galgaduud_Conflict"], na.rm=TRUE)
    N<- tail(movavg(rain.long[(t-5):(t- 1),"Hiiraan_rain"],4,type="w"),1)
    O<- before.long[(t- 7),"Woqooyi_Galbeed_BeforeRegion"]
    P<- tail(movavg(water.long[(t-5):(t- 1),"Bari_WaterDrumPrice"],4,type="w"),1)
    if ( is.na(I) ){Q<- L}
    else if(I>0){Q<- J*K }
    else{Q<- L }
    R<- max(sum(C*D , 0.671567082423603*E*G*H,na.rm=TRUE), 1.7313313939779*Q,na.rm=TRUE)
    S<- max(A*B, R,na.rm=TRUE)
    if ( is.na(N) ){U<- P}
    else if(N>0){U<- O }
    else{U<- P }
    V<- max(S, M*U,na.rm=TRUE)
    FIN <-V
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LS6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    B<- current.long[(t- 11),"Togdheer_CurrentRegion"]
    C<- mean(current.long[(t-13):(t- 1),"Hiiraan_CurrentRegion"], na.rm=TRUE)
    D<- tail(movavg(future.long[(t-14):(t- 1),"Sanaag_FutureRegion"],13,type="w"),1)
    E<- tail(movavg(fatalities.long[(t-7):(t- 1),"Woqooyi_Galbeed_Fatalities"],6,type="w"),1)
    G<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    H<- conflicts.long[(t- 4),"Sool_Conflict"]
    I<- median(current.long[(t-12):(t- 1),"Bay_CurrentRegion"], na.rm=TRUE)
    J<- before.long[(t- 8),"Banadir_BeforeRegion"]
    K<- future.long[(t- 3),"Galgaduud_FutureRegion"]
    L<- future.long[(t- 7),"Galgaduud_FutureRegion"]
    M<- rain.long[(t- 1),"Mudug_rain"]
    N<- tail(movavg(fatalities.long[(t-4):(t- 1),"Woqooyi_Galbeed_Fatalities"], 3,type="m"),1)
    O<- mean(before.long[(t-17):(t- 1),"Shabeellaha_Dhexe_BeforeRegion"], na.rm=TRUE)
    if ( is.na(E) ){P<- J}
    else if(E>0){P<-sum( G*H , I,na.rm=TRUE) }
    else{P<- J }
    if ( is.na(M) || is.na( N)){Q<-0}
    else if(M== N){Q<-1 }
    else{Q<-0 }
    R<- max(P, K*L*Q,na.rm=TRUE)
    S<- max(1377,sum( 0.257081749882479*A , 0.000112204038555112*B*C*D , R,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( S , -O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LS7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 7),"Sool_Fatalities"]
    B<- mean(future.long[(t-8):(t- 1),"Hiiraan_FutureRegion"], na.rm=TRUE)
    C<- before.long[(t- 12),"Sool_BeforeRegion"]
    D<- mean(fatalities.long[(t-12):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    E<- fatalities.long[(t- 3),"Bari_Fatalities"]
    G<- rain.long[(t- 9),"Shabeellaha_Dhexe_rain"]
    H<- current.long[(t- 6),"Togdheer_CurrentRegion"]
    I<- median(stations.long[(t-6):(t- 1),"Gedo_LuuqStation_Juba_River"], na.rm=TRUE)
    J<- before.long[(t- 4),"Sanaag_BeforeRegion"]
    K<- current.long[(t- 11),"Togdheer_CurrentRegion"]
    L<- tail(movavg(current.long[(t-3):(t- 1),"Awdal_CurrentRegion"], 2,type="m"),1)
    M<- rain.long[(t- 5),"Nugaal_rain"]
    N<- rain.long[(t- 5),"Woqooyi_Galbeed_rain"]
    O<- mean(current.long[(t-14):(t- 1),"Jubbada_Dhexe_CurrentRegion"], na.rm=TRUE)
    if ( is.na(A) ){P<- 1.69840528255943*C*D}
    else if(A>0){P<- B }
    else{P<- 1.69840528255943*C*D }
    Q<- max(11.2165325483322*K, 2.0362620678233*L,na.rm=TRUE)
    R<- max(sum(E*G , H*I , J , Q,na.rm=TRUE), M*N,na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( P , R , -O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LS8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    B<- future.long[(t- 15),"Nugaal_FutureRegion"]
    C<- tail(movavg(rain.long[(t-5):(t- 1),"Woqooyi_Galbeed_rain"], 4,type="m"),1)
    D<- before.long[(t- 5),"Sool_BeforeRegion"]
    E<- before.long[(t- 12),"Sool_BeforeRegion"]
    G<- current.long[(t- 1),"Awdal_CurrentRegion"]
    H<- rain.long[(t- 6),"Jubbada_Dhexe_rain"]
    I<- future.long[(t- 7),"Nugaal_FutureRegion"]
    J<- rain.long[(t- 9),"Sool_rain"]
    K<- before.long[(t- 1),"Gedo_BeforeRegion"]
    L<- future.long[(t- 7),"Galgaduud_FutureRegion"]
    M<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    N<- future.long[(t- 12),"Sanaag_FutureRegion"]
    O<- before.long[(t- 5),"Sool_BeforeRegion"]
    P<- current.long[(t- 1),"Awdal_CurrentRegion"]
    if ( is.na(H) ){Q<- K}
    else if(H>0){Q<- I*J }
    else{Q<- K }
    R<- max(sum(B*C , 0.0230923978172593*D*E , G , Q , -L,na.rm=TRUE), M*N,na.rm=TRUE)
    S<- P%% 0.509277669697951
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(S)){S <- 0 }
    FIN <-sum( 0.289706439427311*A , R , -O*S,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LS9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(fatalities.long[(t-4):(t- 1),"Sool_Fatalities"], 3,type="m"),1)
    B<- fatalities.long[(t- 13),"Sool_Fatalities"]
    C<- mean(before.long[(t-4):(t- 1),"Awdal_BeforeRegion"], na.rm=TRUE)
    D<- mean(before.long[(t-4):(t- 1),"Awdal_BeforeRegion"], na.rm=TRUE)
    E<- current.long[(t- 1),"Awdal_CurrentRegion"]
    G<- mean(future.long[(t-8):(t- 1),"Bay_FutureRegion"], na.rm=TRUE)
    H<- fatalities.long[(t- 13),"Sool_Fatalities"]
    I<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    J<- current.long[(t- 1),"Bay_CurrentRegion"]
    K<- current.long[(t- 1),"Awdal_CurrentRegion"]
    L<- tail(movavg(future.long[(t-8):(t- 1),"Jubbada_Dhexe_FutureRegion"], 7,type="m"),1)
    M<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    N<- current.long[(t- 1),"Bay_CurrentRegion"]
    O<- future.long[(t- 11),"Gedo_FutureRegion"]
    P<- rivers.long[(t- 1),"Juba_River_discharge"]
    Q<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    R<- mean(rain.long[(t-10):(t- 1),"Mudug_rain"], na.rm=TRUE)
    S<- max(B*C, D,na.rm=TRUE)
    if ( is.na(H) ){U<- K}
    else if(H>0){U<- 0.122554672843155*I*J }
    else{U<- K }
    V<- max(L, 1.15203986713849e-5*M^2*N*O,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( A*S , E , G , U , V , -P , -Q*R,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LS10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- water.long[(t- 1),"Togdheer_WaterDrumPrice"]
    B<- tail(movavg(before.long[(t-17):(t- 1),"Banadir_BeforeRegion"], 16,type="m"),1)
    C<- rain.long[(t- 2),"Awdal_rain"]
    D<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    E<- before.long[(t- 3),"Bari_BeforeRegion"]
    G<- mean(conflicts.long[(t-15):(t- 1),"Bari_Conflict"], na.rm=TRUE)
    H<- conflicts.long[(t- 13),"Banadir_Conflict"]
    I<- tail(movavg(rain.long[(t-4):(t- 1),"Woqooyi_Galbeed_rain"], 3,type="m"),1)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

LS1_2016arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    B<- rain.long[(t- 1),"Jubbada_Dhexe_rain"]
    C<- current.long[(t- 1),"Bay_CurrentRegion"]
    D<- tail(movavg(fatalities.long[(t-6):(t- 1),"Woqooyi_Galbeed_Fatalities"],5,type="w"),1)
    E<- median(conflicts.long[(t-5):(t- 1),"Mudug_Conflict"], na.rm=TRUE)
    G<- median(before.long[(t-8):(t- 1),"Sool_BeforeRegion"], na.rm=TRUE)
    H<- rain.long[(t- 1),"Awdal_rain"]
    I<- tail(movavg(rain.long[(t-4):(t- 1),"Shabeellaha_Hoose_rain"],3,type="w"),1)
    J<- tail(movavg(before.long[(t-8):(t- 1),"Bakool_BeforeRegion"],7,type="w"),1)
    K<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    L<- before.long[(t- 12),"Sool_BeforeRegion"]
    M<- tail(movavg(rain.long[(t-4):(t- 1),"Shabeellaha_Hoose_rain"],3,type="w"),1)
    N<- rain.long[(t- 1),"Jubbada_Dhexe_rain"]
    O<- current.long[(t- 1),"Awdal_CurrentRegion"]
    P<- current.long[(t- 1),"Awdal_CurrentRegion"]
    Q<- erfc(D)
    R<- min(M, 0.208504266010547,na.rm=TRUE)
    S<- max(A*B,sum( C*Q , E*G , 0.0013327589910882*H*I*J , K*L*R , -11.0016192924878*N,na.rm=TRUE),na.rm=TRUE)
    U<- max(sum(S , -O,na.rm=TRUE), P,na.rm=TRUE)
    FIN <-U
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

LS2_2016arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Bay_BeforeRegion"]
    B<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    C<- mean(before.long[(t-10):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    D<- future.long[(t- 7),"Sanaag_FutureRegion"]
    E<- tail(movavg(rain.long[(t-5):(t- 1),"Shabeellaha_Hoose_rain"],4,type="w"),1)
    G<- future.long[(t- 12),"Sanaag_FutureRegion"]
    H<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    I<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    J<- before.long[(t- 1),"Bay_BeforeRegion"]
    K<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    L<- current.long[(t- 10),"Mudug_CurrentRegion"]
    M<- median(current.long[(t-12):(t- 1),"Hiiraan_CurrentRegion"], na.rm=TRUE)
    N<- tan(0.736609302567234*J)
    O<- min(0.252116342310595*H,sum( I , N,na.rm=TRUE),na.rm=TRUE)
    P<- max(sum(0.736609302567234*A , 0.252116342310595*B , 10.6307741115515*C , 1.29841668026434*D*E , G*O , K,na.rm=TRUE), 0.000146663305234602*L^2,na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( P , -M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

LS3_2016arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 15),"Mudug_Conflict"]
    B<- median(future.long[(t-8):(t- 1),"Sanaag_FutureRegion"], na.rm=TRUE)
    C<- before.long[(t- 1),"Bakool_BeforeRegion"]
    D<- before.long[(t- 1),"Bay_BeforeRegion"]
    E<- before.long[(t- 12),"Sool_BeforeRegion"]
    G<- fatalities.long[(t- 1),"Hiiraan_Fatalities"]
    H<- median(fatalities.long[(t-10):(t- 1),"Shabeellaha_Dhexe_Fatalities"], na.rm=TRUE)
    I<- before.long[(t- 12),"Sool_BeforeRegion"]
    J<- rain.long[(t- 1),"Awdal_rain"]
    K<- current.long[(t- 12),"Shabeellaha_Dhexe_CurrentRegion"]
    L<- median(conflicts.long[(t-6):(t- 1),"Shabeellaha_Dhexe_Conflict"], na.rm=TRUE)
    M<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    N<- future.long[(t- 2),"Nugaal_FutureRegion"]
    O<- max(C,sum( D , -E*G,na.rm=TRUE),na.rm=TRUE)
    P<- sinh(0.00587184945241316*J)
    Q<- tan(P)
    R<- cos(L)
    S<- max(K*R, M*N,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(S)){S <- 0 }
    FIN <-sum( A*B , 0.815982678983483*O , H^2*I*Q , S,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

LS4_2016arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    B<- current.long[(t- 1),"Bay_CurrentRegion"]
    C<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    D<- future.long[(t- 11),"Mudug_FutureRegion"]
    E<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    G<- future.long[(t- 12),"Sanaag_FutureRegion"]
    H<- fatalities.long[(t- 1),"Gedo_Fatalities"]
    I<- future.long[(t- 7),"Sanaag_FutureRegion"]
    J<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    K<- median(before.long[(t-6):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    L<- min(0.0445354016499487*C, 1.61853281845267,na.rm=TRUE)
    M<- max(B*L,sum( 2.710977751945*D , E*G , 0.795437328690138*H*I , J,na.rm=TRUE),na.rm=TRUE)
    N<- max(A, M,na.rm=TRUE)
    O<- max(1786, N,na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 1.34490298946739*O , K , -2182.47411533964,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

LS5_2016arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Bay_CurrentRegion"]
    B<- future.long[(t- 5),"Bay_FutureRegion"]
    C<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    D<- future.long[(t- 1),"Jubbada_Dhexe_FutureRegion"]
    E<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    G<- future.long[(t- 1),"Jubbada_Dhexe_FutureRegion"]
    H<- future.long[(t- 3),"Bay_FutureRegion"]
    I<- median(current.long[(t-15):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    J<- mean(future.long[(t-8):(t- 1),"Bay_FutureRegion"], na.rm=TRUE)
    K<- median(conflicts.long[(t-7):(t- 1),"Jubbada_Dhexe_Conflict"], na.rm=TRUE)
    L<- current.long[(t- 1),"Bay_CurrentRegion"]
    M<- current.long[(t- 10),"Mudug_CurrentRegion"]
    N<- max(I,sum( J*K , 0.00212226087639779*L*M , -4179.34879629865,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 0.00212226087639779*A , 0.174129180645894*B , 0.00526605159381884*C*D , 9.65881231349429e-5*E*G*H , 1.33362346607847*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LSJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    C<- rain.long[(t- 1),"Jubbada_Dhexe_rain"]
    D<- median(stations.long[(t-6):(t- 1),"Gedo_DollowStation_Juba_River"], na.rm=TRUE)
    E<- rain.long[(t- 1),"Jubbada_Dhexe_rain"]
    G<- current.long[(t- 1),"Bay_CurrentRegion"]
    H<- tail(movavg(rain.long[(t-9):(t- 1),"Woqooyi_Galbeed_rain"], 8,type="m"),1)
    I<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    J<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    K<- before.long[(t- 1),"Bari_BeforeRegion"]
    L<- current.long[(t- 1),"Awdal_CurrentRegion"]
    M<- tail(movavg(before.long[(t-7):(t- 1),"Hiiraan_BeforeRegion"],6,type="w"),1)
    N<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    O<- rain.long[(t- 1),"Jubbada_Dhexe_rain"]
    P<- current.long[(t- 1),"Awdal_CurrentRegion"]
    Q<- current.long[(t- 1),"Awdal_CurrentRegion"]
    R<- mean(future.long[(t-8):(t- 1),"Bay_FutureRegion"], na.rm=TRUE)
    S<- rivers.long[(t- 1),"Juba_River_discharge"]
    U<- acosh(L)
    if ( is.na(I) ){V<- M}
    else if(I>0){V<-sum( J*K*U , -9775.44424571598,na.rm=TRUE) }
    else{V<- M }
    if ( is.na(E) ){W<- V}
    else if(E>0){W<- 0.0223771266780618*G*H }
    else{W<- V }
    X<- max(B*C,sum( D*W , -N*O,na.rm=TRUE),na.rm=TRUE)
    Y<- max(A,sum( X , -P,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(Y)){Y <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(S)){S <- 0 }
    FIN <-sum( 0.957759474618228*Y , Q , R , -S,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LSJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
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
    U<- tail(movavg(fatalities.long[(t-7):(t- 1),"Bakool_Fatalities"],6,type="w"),1)
    V<- tail(movavg(conflicts.long[(t-5):(t- 1),"Awdal_Conflict"], 4,type="m"),1)
    W<- tail(movavg(before.long[(t-4):(t- 1),"Sanaag_BeforeRegion"], 3,type="m"),1)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LSJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    B<- current.long[(t- 1),"Bay_CurrentRegion"]
    C<- before.long[(t- 12),"Sool_BeforeRegion"]
    D<- rain.long[(t- 5),"Nugaal_rain"]
    E<- rain.long[(t- 8),"Bakool_rain"]
    G<- tail(movavg(future.long[(t-10):(t- 1),"Bay_FutureRegion"],9,type="w"),1)
    H<- before.long[(t- 12),"Sool_BeforeRegion"]
    I<- before.long[(t- 12),"Awdal_BeforeRegion"]
    J<- current.long[(t- 1),"Awdal_CurrentRegion"]
    K<- rain.long[(t- 5),"Nugaal_rain"]
    L<- rain.long[(t- 8),"Bakool_rain"]
    M<- before.long[(t- 1),"Galgaduud_BeforeRegion"]
    N<- before.long[(t- 1),"Bay_BeforeRegion"]
    O<- current.long[(t- 1),"Nugaal_CurrentRegion"]
    P<- future.long[(t- 1),"Togdheer_FutureRegion"]
    Q<- stations.long[(t- 1),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
    R<- (12.3662580594767*C)
    S<- and(D, E)
    if ( is.na(H) || is.na( I)){U<-0}
    else if(H>= I){U<-1 }
    else{U<-0 }
    V<- max(G*U, J,na.rm=TRUE)
    W<- and(K, L)
    if ( is.na(W) ){X<- N}
    else if(W>0){X<- M }
    else{X<- N }
    Y<- max(sum(0.0114911994031139*A*B , R^S , 1.63507657062849*V,na.rm=TRUE), X,na.rm=TRUE)
    if(is.infinite(Y)){Y <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( Y , -O , -P*Q,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LSJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 6),"Togdheer_BeforeRegion"]
    B<- stations.long[(t- 7),"Hiiraan_Bulo_Burti_StationShabelle_River"]
    C<- current.long[(t- 1),"Bay_CurrentRegion"]
    D<- tail(movavg(rain.long[(t-5):(t- 1),"Woqooyi_Galbeed_rain"], 4,type="m"),1)
    E<- median(before.long[(t-6):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    G<- fatalities.long[(t- 1),"Gedo_Fatalities"]
    H<- future.long[(t- 7),"Sanaag_FutureRegion"]
    I<- before.long[(t- 6),"Banadir_BeforeRegion"]
    J<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    K<- future.long[(t- 12),"Sanaag_FutureRegion"]
    L<- fatalities.long[(t- 4),"Hiiraan_Fatalities"]
    M<- fatalities.long[(t- 7),"Awdal_Fatalities"]
    N<- current.long[(t- 1),"Nugaal_CurrentRegion"]
    O<- tail(movavg(current.long[(t-4):(t- 1),"Togdheer_CurrentRegion"], 3,type="m"),1)
    P<- max(sum(G*H , -I,na.rm=TRUE),sum( J*K , -629.306446522722,na.rm=TRUE),na.rm=TRUE)
    Q<- max(sum(A*B , 0.0243248044201489*C*D , E , P,na.rm=TRUE),sum( L^2*M , N,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( Q , -O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LSJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- mean(current.long[(t-3):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    B<- median(stations.long[(t-5):(t- 1),"Gedo_DollowStation_Juba_River"], na.rm=TRUE)
    C<- current.long[(t- 1),"Bay_CurrentRegion"]
    D<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    E<- median(stations.long[(t-5):(t- 1),"Gedo_DollowStation_Juba_River"], na.rm=TRUE)
    G<- tail(movavg(rain.long[(t-5):(t- 1),"Sool_rain"], 4,type="m"),1)
    H<- fatalities.long[(t- 1),"Gedo_Fatalities"]
    I<- future.long[(t- 8),"Sanaag_FutureRegion"]
    J<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    K<- future.long[(t- 11),"Mudug_FutureRegion"]
    L<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    M<- future.long[(t- 12),"Sanaag_FutureRegion"]
    N<- future.long[(t- 6),"Sanaag_FutureRegion"]
    O<- future.long[(t- 10),"Sanaag_FutureRegion"]
    P<- min(0.000491935539122905*D^2, E,na.rm=TRUE)
    Q<- min(P, G,na.rm=TRUE)
    R<- max(3.56730631035613*K, L*M,na.rm=TRUE)
    if ( is.na(J) ){S<- N*O}
    else if(J>0){S<- R }
    else{S<- N*O }
    U<- max(0.782893180838608*H*I, S,na.rm=TRUE)
    V<- max(C*Q, U,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(V)){V <- 0 }
    FIN <-sum( A*B , V,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LSJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    B<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    C<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    D<- current.long[(t- 1),"Bay_CurrentRegion"]
    E<- mean(future.long[(t-8):(t- 1),"Bay_FutureRegion"], na.rm=TRUE)
    G<- mean(stations.long[(t-9):(t- 1),"Gedo_LuuqStation_Juba_River"], na.rm=TRUE)
    H<- future.long[(t- 1),"Banadir_FutureRegion"]
    I<- future.long[(t- 1),"Mudug_FutureRegion"]
    J<- rivers.long[(t- 1),"Shabelle_River_discharge"]
    K<- conflicts.long[(t- 3),"Bay_Conflict"]
    L<- future.long[(t- 4),"Nugaal_FutureRegion"]
    M<- current.long[(t- 1),"Bay_CurrentRegion"]
    N<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    O<- future.long[(t- 11),"Mudug_FutureRegion"]
    P<- mean(future.long[(t-5):(t- 1),"Bay_FutureRegion"], na.rm=TRUE)
    Q<- atan2(H, I*J)
    if ( is.na(K) ){R<- M}
    else if(K>0){R<- L }
    else{R<- M }
    S<- asin(0.0163368452434756*N)
    if ( is.na(S) ){U<- P}
    else if(S>0){U<- 3.57462142409262*O }
    else{U<- P }
    V<- max(sum(0.0180456994000383*A*B , 0.0163368452434756*C*D , E*G*Q , R,na.rm=TRUE), U,na.rm=TRUE)
    FIN <-V
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LSJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    B<- current.long[(t- 1),"Bay_CurrentRegion"]
    C<- current.long[(t- 1),"Awdal_CurrentRegion"]
    D<- current.long[(t- 1),"Awdal_CurrentRegion"]
    E<- future.long[(t- 7),"Sanaag_FutureRegion"]
    G<- conflicts.long[(t- 17),"Gedo_Conflict"]
    H<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    I<- median(rain.long[(t-8):(t- 1),"Nugaal_rain"], na.rm=TRUE)
    J<- future.long[(t- 11),"Mudug_FutureRegion"]
    K<- current.long[(t- 1),"Bay_CurrentRegion"]
    L<- future.long[(t- 1),"Bakool_FutureRegion"]
    M<- future.long[(t- 9),"Sanaag_FutureRegion"]
    N<- future.long[(t- 13),"Sanaag_FutureRegion"]
    O<- future.long[(t- 13),"Mudug_FutureRegion"]
    P<- tail(movavg(future.long[(t-7):(t- 1),"Sanaag_FutureRegion"], 6,type="m"),1)
    Q<- max(D, 2.28920662176638*E*G,na.rm=TRUE)
    if ( is.na(I) ){R<- K}
    else if(I>0){R<- 4.39117640002846*J }
    else{R<- K }
    if ( is.na(H) ){S<- L}
    else if(H>0){S<- R }
    else{S<- L }
    U<- max(M*N, O,na.rm=TRUE)
    V<- max(sum(0.0182188246753647*A*B , C , Q , S,na.rm=TRUE), U,na.rm=TRUE)
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( 0.808594465624701*V , -P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LSJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    B<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    C<- future.long[(t- 12),"Sanaag_FutureRegion"]
    D<- future.long[(t- 11),"Mudug_FutureRegion"]
    E<- mean(conflicts.long[(t-9):(t- 1),"Mudug_Conflict"], na.rm=TRUE)
    G<- tail(movavg(before.long[(t-4):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], 3,type="m"),1)
    H<- tail(movavg(fatalities.long[(t-9):(t- 1),"Togdheer_Fatalities"], 8,type="m"),1)
    I<- before.long[(t- 1),"Bay_BeforeRegion"]
    J<- tail(movavg(rain.long[(t-3):(t- 1),"Woqooyi_Galbeed_rain"], 2,type="m"),1)
    K<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    L<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    M<- tail(movavg(rain.long[(t-3):(t- 1),"Woqooyi_Galbeed_rain"], 2,type="m"),1)
    N<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    O<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    P<- future.long[(t- 11),"Mudug_FutureRegion"]
    Q<- N%% 0.0142191712970708
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
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( 0.279952770337981*A , B*C , D*E , G*H , 0.0145458777910696*I*J , K*L*M*Q , -0.242087446817961*O*P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LSJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 11),"Mudug_FutureRegion"]
    B<- mean(stations.long[(t-14):(t- 1),"Hiiraan_Belet_WeyneStation_Shabelle_River"], na.rm=TRUE)
    C<- future.long[(t- 1),"Jubbada_Dhexe_FutureRegion"]
    D<- median(fatalities.long[(t-6):(t- 1),"Sanaag_Fatalities"], na.rm=TRUE)
    E<- future.long[(t- 1),"Bakool_FutureRegion"]
    G<- future.long[(t- 1),"Bay_FutureRegion"]
    H<- current.long[(t- 1),"Awdal_CurrentRegion"]
    I<- mean(fatalities.long[(t-8):(t- 1),"Shabeellaha_Dhexe_Fatalities"], na.rm=TRUE)
    J<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    K<- current.long[(t- 1),"Bay_CurrentRegion"]
    L<- mean(future.long[(t-8):(t- 1),"Bay_FutureRegion"], na.rm=TRUE)
    M<- future.long[(t- 1),"Togdheer_FutureRegion"]
    if ( is.na(I) ){N<- 7997.58601212182}
    else if(I>0){N<- 0.014034262708128*J*K }
    else{N<- 7997.58601212182 }
    O<- max(sum(C*D , 0.00142949134319195*E*G , H , N,na.rm=TRUE), L,na.rm=TRUE)
    P<- max(A*B, O,na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 1.08608351962838*P , -4.47137535033928*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LSJUN10arrivals <- function(start, end){
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
    M<- tail(movavg(fatalities.long[(t-17):(t- 1),"Shabeellaha_Dhexe_Fatalities"], 16,type="m"),1)
    N<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    O<- max(sum(0.281967386533701*D , E*G , 0.0142740689486181*H*I , J , -K,na.rm=TRUE), 0.789348598567604*L*M,na.rm=TRUE)
    P<- max(A*B*C, O,na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( P , -0.237387364177301*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}


