
modelarrivals_HIminus2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- conflicts.long[(t- 5),"Togdheer_Conflict"]
    B<- before.long[(t- 11),"Sanaag_BeforeRegion"]
    C<- fatalities.long[(t- 2),"Nugaal_Fatalities"]
    D<- future.long[(t- 2),"Shabeallaha_Dhexe_FutureRegion"]
    E<- rain.long[(t- 2),"Mudug_rain"]
    G<- future.long[(t- 5),"Nugaal_FutureRegion"]
    H<- future.long[(t- 12),"Mudug_FutureRegion"]
    I<- conflicts.long[(t- 2),"Nugaal_Conflict"]
    J<- before.long[(t- 14),"Sanaag_BeforeRegion"]
    K<- before.long[(t- 11),"Sanaag_BeforeRegion"]
    L<- future.long[(t- 2),"Shabeallaha_Dhexe_FutureRegion"]
    M<- before.long[(t- 14),"Sanaag_BeforeRegion"]
    N<- conflicts.long[(t- 5),"Togdheer_Conflict"]
    O<- before.long[(t- 11),"Sanaag_BeforeRegion"]
    P<- fatalities.long[(t- 2),"Nugaal_Fatalities"]
    Q<- future.long[(t- 2),"Shabeallaha_Dhexe_FutureRegion"]
    R<- rain.long[(t- 2),"Mudug_rain"]
    S<- future.long[(t- 5),"Nugaal_FutureRegion"]
    U<- future.long[(t- 12),"Mudug_FutureRegion"]
    V<- conflicts.long[(t- 2),"Nugaal_Conflict"]
    W<- before.long[(t- 14),"Sanaag_BeforeRegion"]
    X<- median(goats.long[(t-16):(t- 2),"Nugaal_goatprice"], na.rm=TRUE)
    Y<- not(E)
    Z<- max(0.808473217189744*H, I*J,na.rm=TRUE)
    AA<- K%% L
    BB<- not(R)
    CC<- max(0.808473217189744*U, V*W,na.rm=TRUE)
    if ( is.na(AA) ){DD<-sum( N*O , 0.914088823693476*P*Q*BB , S , CC,na.rm=TRUE)}
    else if(AA>0){DD<- M }
    else{DD<-sum( N*O , 0.914088823693476*P*Q*BB , S , CC,na.rm=TRUE) }
    EE<- cos(X)
    GF<- EE%% 0.903737698766289
    HG<- max(sum(A*B , 0.914088823693476*C*D*Y , G , Z , DD,na.rm=TRUE), 53.4721951858023/GF,na.rm=TRUE)
    FIN <-HG
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HIminus6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- conflicts.long[(t- 12),"Shabeellaha_Hoose_Conflict"]
    B<- tail(movavg(fatalities.long[(t-17):(t- 6),"Jubbada_Dhexe_Fatalities"], 11,type="m"),1)
    C<- conflicts.long[(t- 14),"Nugaal_Conflict"]
    D<- before.long[(t- 13),"Sool_BeforeRegion"]
    E<- tail(movavg(fatalities.long[(t-10):(t- 6),"Jubbada_Dhexe_Fatalities"], 4,type="m"),1)
    G<- tail(movavg(fatalities.long[(t-10):(t- 6),"Jubbada_Dhexe_Fatalities"], 4,type="m"),1)
    H<- current.long[(t- 6),"Mudug_CurrentRegion"]
    I<- fatalities.long[(t- 12),"Nugaal_Fatalities"]
    J<- rain.long[(t- 10),"Shabeellaha_Dhexe_rain"]
    K<- before.long[(t- 13),"Sool_BeforeRegion"]
    L<- before.long[(t- 6),"Banadir_BeforeRegion"]
    M<- tail(movavg(fatalities.long[(t-12):(t- 6),"Jubbada_Dhexe_Fatalities"], 6,type="m"),1)
    N<- tan(B)
    O<- atanh(N)
    P<- and(O, C)
    if ( is.na(G) ){Q<- 5116}
    else if(G>0){Q<- 1.12201251124016*H*I }
    else{Q<- 5116 }
    R<- max(D*E, Q,na.rm=TRUE)
    if ( is.na(P) ){S<- -106.420875685508}
    else if(P>0){S<-sum( R , -J*K,na.rm=TRUE) }
    else{S<- -106.420875685508 }
    if ( is.na(A) ){U<- L}
    else if(A>0){U<- S }
    else{U<- L }
    V<- tan(M)
    W<- max(595.642857142857,sum( U , -148.513649290726*V,na.rm=TRUE),na.rm=TRUE)
    FIN <-W
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HI1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 5),"Nugaal_FutureRegion"]
    B<- future.long[(t- 1),"Nugaal_FutureRegion"]
    C<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    D<- future.long[(t- 1),"Bakool_FutureRegion"]
    E<- rain.long[(t- 1),"Bari_rain"]
    G<- current.long[(t- 1),"Jubbada_Dhexe_CurrentRegion"]
    H<- tail(movavg(current.long[(t-5):(t- 1),"Jubbada_Dhexe_CurrentRegion"],4,type="w"),1)
    I<- fatalities.long[(t- 17),"Hiiraan_Fatalities"]
    J<- conflicts.long[(t- 14),"Shabeellaha_Dhexe_Conflict"]
    K<- median(rain.long[(t-3):(t- 1),"Bari_rain"], na.rm=TRUE)
    L<- fatalities.long[(t- 2),"Nugaal_Fatalities"]
    M<- median(stations.long[(t-7):(t- 1),"Juba_Dhexe_BualleStation_Juba_River"], na.rm=TRUE)
    N<- max(sum(1.90007806064601 , A,na.rm=TRUE),sum( 1.90007806064601*B , 0.000216542213423974*C*D*E*G , -H,na.rm=TRUE),na.rm=TRUE)
    if ( is.na(I) || is.na( J)){O<-0}
    else if(I> J){O<-1 }
    else{O<-0 }
    if ( is.na(K) ){P<- 5440*L}
    else if(K>0){P<- 595.642855849904 }
    else{P<- 5440*L }
    if ( is.na(O) ){Q<- 6.11422397433814^M}
    else if(O>0){Q<- P }
    else{Q<- 6.11422397433814^M }
    R<- max(N, Q,na.rm=TRUE)
    FIN <-R
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HI2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 1),"Nugaal_FutureRegion"]
    B<- before.long[(t- 3),"Togdheer_BeforeRegion"]
    C<- future.long[(t- 1),"Bakool_FutureRegion"]
    D<- median(rain.long[(t-5):(t- 1),"Jubbada_Dhexe_rain"], na.rm=TRUE)
    E<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    G<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    H<- rain.long[(t- 1),"Bay_rain"]
    I<- median(rain.long[(t-9):(t- 1),"Togdheer_rain"], na.rm=TRUE)
    J<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    K<- tail(movavg(rain.long[(t-6):(t- 1),"Sool_rain"],5,type="w"),1)
    L<- current.long[(t- 1),"Bakool_CurrentRegion"]
    M<- tail(movavg(future.long[(t-14):(t- 1),"Sanaag_FutureRegion"], 13,type="m"),1)
    N<- future.long[(t- 1),"Bari_FutureRegion"]
    if ( is.na(J) ){O<- 0.0438604765186447*L}
    else if(J>0){O<- K }
    else{O<- 0.0438604765186447*L }
    if ( is.na(H) ){P<- O}
    else if(H>0){P<- I }
    else{P<- O }
    if ( is.na(D) ){Q<- P}
    else if(D>0){Q<- 5.7229868468474e-6*E^2*G }
    else{Q<- P }
    R<- max(sum(606 , -B,na.rm=TRUE),sum( C*Q , M , N , -2058.27225468516,na.rm=TRUE),na.rm=TRUE)
    S<- max(1.95797947508616*A, R,na.rm=TRUE)
    FIN <-S
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HI3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 15),"Galgaduud_BeforeRegion"]
    B<- future.long[(t- 12),"Sanaag_FutureRegion"]
    C<- median(goats.long[(t-7):(t- 1),"Woqooyi_Galbeed_goatprice"], na.rm=TRUE)
    D<- fatalities.long[(t- 12),"Galguduud_Fatalities"]
    E<- median(future.long[(t-5):(t- 1),"Shabeellaha_Hoose_FutureRegion"], na.rm=TRUE)
    G<- future.long[(t- 1),"Nugaal_FutureRegion"]
    H<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    I<- future.long[(t- 12),"Sanaag_FutureRegion"]
    J<- mean(conflicts.long[(t-12):(t- 1),"Shabeellaha_Dhexe_Conflict"], na.rm=TRUE)
    K<- mean(conflicts.long[(t-12):(t- 1),"Shabeellaha_Dhexe_Conflict"], na.rm=TRUE)
    L<- before.long[(t- 11),"Shabeellaha_Dhexe_BeforeRegion"]
    M<- mean(fatalities.long[(t-9):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    N<- before.long[(t- 15),"Galgaduud_BeforeRegion"]
    O<- future.long[(t- 12),"Sanaag_FutureRegion"]
    P<- median(goats.long[(t-7):(t- 1),"Woqooyi_Galbeed_goatprice"], na.rm=TRUE)
    Q<- future.long[(t- 1),"Nugaal_FutureRegion"]
    R<- median(goats.long[(t-7):(t- 1),"Woqooyi_Galbeed_goatprice"], na.rm=TRUE)
    S<- before.long[(t- 15),"Galgaduud_BeforeRegion"]
    U<- future.long[(t- 12),"Sanaag_FutureRegion"]
    V<- median(goats.long[(t-7):(t- 1),"Woqooyi_Galbeed_goatprice"], na.rm=TRUE)
    W<- future.long[(t- 1),"Nugaal_FutureRegion"]
    X<- tan(C)
    if ( is.na(I) || is.na( J)){Y<-0}
    else if(I> J){Y<-1 }
    else{Y<-0 }
    if ( is.na(H) ){Z<- K}
    else if(H>0){Z<- Y }
    else{Z<- K }
    AA<- tan(P)
    BB<- tan(V)
    if ( is.na(sum(1.42137239447843*N , O*AA , Q,na.rm=TRUE)) ){CC<-sum( 1.42137239447843*S , U*BB , W,na.rm=TRUE)}
    else if(sum(1.42137239447843*N , O*AA , Q,na.rm=TRUE)>0){CC<- R }
    else{CC<-sum( 1.42137239447843*S , U*BB , W,na.rm=TRUE) }
    DD<- max(595.64285742912*Z,sum( 8.66279132753939*L , -M*CC,na.rm=TRUE),na.rm=TRUE)
    EE<- max(1.95797892934304*G, DD,na.rm=TRUE)
    GF<- max(sum(1.42137239447843*A , B*X , -D*E,na.rm=TRUE), EE,na.rm=TRUE)
    FIN <-GF
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HI4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 1),"Togdheer_FutureRegion"]
    B<- conflicts.long[(t- 7),"Bakool_Conflict"]
    C<- future.long[(t- 12),"Sanaag_FutureRegion"]
    D<- future.long[(t- 1),"Togdheer_FutureRegion"]
    E<- future.long[(t- 9),"Sanaag_FutureRegion"]
    G<- future.long[(t- 12),"Sanaag_FutureRegion"]
    H<- future.long[(t- 1),"Nugaal_FutureRegion"]
    I<- rain.long[(t- 8),"Nugaal_rain"]
    J<- median(stations.long[(t-6):(t- 1),"Gedo_LuuqStation_Juba_River"], na.rm=TRUE)
    K<- median(fatalities.long[(t-10):(t- 1),"Mudug_Fatalities"], na.rm=TRUE)
    L<- conflicts.long[(t- 7),"Awdal_Conflict"]
    M<- before.long[(t- 15),"Galgaduud_BeforeRegion"]
    N<- max(2.00198589444221*H, I*J*K,na.rm=TRUE)
    O<- max(A,sum( B*C , D , E , G , N , -569.533197253193,na.rm=TRUE),na.rm=TRUE)
    P<- max(O, 595.64267907852,na.rm=TRUE)
    Q<- max(P, 0.236340283712891*L*M,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HI5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    B<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    C<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    D<- rain.long[(t- 1),"Hiiraan_rain"]
    E<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    G<- future.long[(t- 1),"Nugaal_FutureRegion"]
    H<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    I<- rain.long[(t- 11),"Hiiraan_rain"]
    J<- rain.long[(t- 12),"Jubbada_Dhexe_rain"]
    K<- rain.long[(t- 1),"Hiiraan_rain"]
    if ( is.na(E) ){L<- 2672.63626376654}
    else if(E>0){L<-sum( 2.31006724962541*G , 1.75858006755041*H*I*J,na.rm=TRUE) }
    else{L<- 2672.63626376654 }
    M<- max(595.642706291656,sum( 3.23526240639516e-9*A^2*B^2*C*D , L , -3046.5807393425 , -4.12486267021721*K,na.rm=TRUE),na.rm=TRUE)
    FIN <-M
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HI6arrivals <- function(start, end){
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
    FIN <- Z
      PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HI7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 3),"Togdheer_BeforeRegion"]
    B<- future.long[(t- 1),"Nugaal_FutureRegion"]
    C<- conflicts.long[(t- 6),"Awdal_Conflict"]
    D<- fatalities.long[(t- 5),"Bakool_Fatalities"]
    E<- fatalities.long[(t- 9),"Shabeellaha_Dhexe_Fatalities"]
    G<- future.long[(t- 5),"Nugaal_FutureRegion"]
    H<- fatalities.long[(t- 9),"Shabeellaha_Dhexe_Fatalities"]
    I<- before.long[(t- 6),"Shabeellaha_Dhexe_BeforeRegion"]
    J<- future.long[(t- 1),"Bakool_FutureRegion"]
    K<- future.long[(t- 1),"Nugaal_FutureRegion"]
    L<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    M<- median(conflicts.long[(t-6):(t- 1),"Gedo_Conflict"], na.rm=TRUE)
    N<- future.long[(t- 1),"Nugaal_FutureRegion"]
    O<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    P<- tail(movavg(future.long[(t-4):(t- 1),"Togdheer_FutureRegion"], 3,type="m"),1)
    Q<- max(sum(603.138365132431 , -A,na.rm=TRUE),sum( B*C , D*E , G , H , -I,na.rm=TRUE),na.rm=TRUE)
    R<- log(L)
    S<- (sum(0.0683422939259692*J*K , R,na.rm=TRUE))
    if ( is.na(M) || is.na( N)){U<-0}
    else if(M> N){U<-1 }
    else{U<-0 }
    if ( is.na(O) ){V<- P}
    else if(O>0){V<- 33.1596623739479 }
    else{V<- P }
    W<- max(Q, S^U*V,na.rm=TRUE)
    FIN <-W
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HI8arrivals <- function(start, end){
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HI9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 1),"Bakool_FutureRegion"]
    B<- future.long[(t- 1),"Nugaal_FutureRegion"]
    C<- fatalities.long[(t- 13),"Hiiraan_Fatalities"]
    D<- future.long[(t- 5),"Banadir_FutureRegion"]
    E<- before.long[(t- 13),"Sool_BeforeRegion"]
    G<- tail(movavg(rain.long[(t-6):(t- 1),"Jubbada_Dhexe_rain"], 5,type="m"),1)
    H<- future.long[(t- 16),"Bay_FutureRegion"]
    I<- future.long[(t- 1),"Nugaal_FutureRegion"]
    J<- conflicts.long[(t- 11),"Sanaag_Conflict"]
    K<- mean(future.long[(t-16):(t- 1),"Shabeellaha_Hoose_FutureRegion"], na.rm=TRUE)
    L<- future.long[(t- 16),"Bay_FutureRegion"]
    M<- future.long[(t- 1),"Bakool_FutureRegion"]
    N<- exp(H)
    O<- M%% 39.5999037799321
    P<- max(sum(9594.3890561065 , -N,na.rm=TRUE),sum( I*J , K , -L*O,na.rm=TRUE),na.rm=TRUE)
    Q<- max(595.642856937338, P,na.rm=TRUE)
    R<- max(sum(39.5999037799321*A , -42.5091638044556*B*C , -0.510360235770738*D*E*G,na.rm=TRUE), Q,na.rm=TRUE)
    FIN <-R
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HI10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 12),"Sanaag_FutureRegion"]
    B<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    C<- future.long[(t- 12),"Sanaag_FutureRegion"]
    D<- tail(movavg(fatalities.long[(t-3):(t- 1),"Shabeellaha_Dhexe_Fatalities"], 2,type="m"),1)
    E<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    G<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    H<- tail(movavg(rain.long[(t-7):(t- 1),"Gedo_rain"], 6,type="m"),1)
    I<- future.long[(t- 5),"Nugaal_FutureRegion"]
    J<- future.long[(t- 1),"Bakool_FutureRegion"]
    K<- tail(movavg(fatalities.long[(t-3):(t- 1),"Shabeellaha_Dhexe_Fatalities"], 2,type="m"),1)
    L<- water.long[(t- 1),"Hiiraan_WaterDrumPrice"]
    M<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    N<- tail(movavg(rain.long[(t-4):(t- 1),"Shabeellaha_Dhexe_rain"], 3,type="m"),1)
    O<- future.long[(t- 1),"Nugaal_FutureRegion"]
    P<- atan2(J*K, L)
    if ( is.na(1.06152196011327) || is.na( P)){Q<-0}
    else if(1.06152196011327< P){Q<-1 }
    else{Q<-0 }
    R<- max(sum(C*D , E*G*H , I,na.rm=TRUE), 54579.701375567*Q,na.rm=TRUE)
    S<- max(595.642855738811, 1.95797899679151*O,na.rm=TRUE)
    U<- max(sum(A*B , R , -M*N,na.rm=TRUE), S,na.rm=TRUE)
    FIN <-U
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HIJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 5),"Woqooyi_Galbeed_Fatalities"]
    B<- stations.long[(t- 1),"Gedo_DollowStation_Juba_River"]
    C<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    D<- future.long[(t- 12),"Sanaag_FutureRegion"]
    E<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    G<- future.long[(t- 12),"Sanaag_FutureRegion"]
    H<- fatalities.long[(t- 5),"Woqooyi_Galbeed_Fatalities"]
    I<- fatalities.long[(t- 5),"Woqooyi_Galbeed_Fatalities"]
    J<- future.long[(t- 5),"Nugaal_FutureRegion"]
    K<- future.long[(t- 1),"Nugaal_FutureRegion"]
    L<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    M<- water.long[(t- 8),"Nugaal_WaterDrumPrice"]
    N<- factorial(A)
    O<- factorial(H)
    P<- E*G%% 1.3290208170006*O^2
    Q<- atan2(P, I)
    R<- max(600.987645299072, 1.94197790995331*K,na.rm=TRUE)
    S<- tan(M)
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(S)){S <- 0 }
    FIN <-sum( 1.3290208170006*N , 6.68248984317236e-12*5567.08446367392^B , C*D*Q , J , R , -12.7310409563461*L*S,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HIJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- tail(movavg(future.long[(t-5):(t- 1),"Togdheer_FutureRegion"], 4,type="m"),1)
    B<- before.long[(t- 15),"Galgaduud_BeforeRegion"]
    C<- before.long[(t- 7),"Galgaduud_BeforeRegion"]
    D<- tail(movavg(before.long[(t-10):(t- 1),"Galgaduud_BeforeRegion"], 9,type="m"),1)
    E<- future.long[(t- 1),"Nugaal_FutureRegion"]
    G<- before.long[(t- 7),"Sanaag_BeforeRegion"]
    H<- rain.long[(t- 11),"Togdheer_rain"]
    I<- conflicts.long[(t- 11),"Hiiraan_Conflict"]
    J<- before.long[(t- 12),"Sanaag_BeforeRegion"]
    K<- tail(movavg(stations.long[(t-5):(t- 1),"Gedo_LuuqStation_Juba_River"],4,type="w"),1)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HIJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- rain.long[(t- 9),"Hiiraan_rain"]
    B<- fatalities.long[(t- 14),"Togdheer_Fatalities"]
    C<- rain.long[(t- 2),"Gedo_rain"]
    D<- current.long[(t- 1),"Jubbada_Dhexe_CurrentRegion"]
    E<- future.long[(t- 2),"Shabeallaha_Dhexe_FutureRegion"]
    G<- before.long[(t- 13),"Sool_BeforeRegion"]
    H<- current.long[(t- 1),"Jubbada_Dhexe_CurrentRegion"]
    I<- current.long[(t- 1),"Jubbada_Dhexe_CurrentRegion"]
    J<- current.long[(t- 8),"Sool_CurrentRegion"]
    K<- future.long[(t- 17),"Jubbada_Hoose_FutureRegion"]
    L<- tail(movavg(current.long[(t-10):(t- 1),"Bakool_CurrentRegion"], 9,type="m"),1)
    M<- max(sum(10.3387188888267*E , 3.30741265006334*G , -12.5214001431031*H,na.rm=TRUE),sum( 1.23033975501628e-6*I^2*J , -K , -L,na.rm=TRUE),na.rm=TRUE)
    if ( is.na(A) ){N<-sum( D , M,na.rm=TRUE)}
    else if(A>0){N<- 0.268435910532953*B^2*C }
    else{N<-sum( D , M,na.rm=TRUE) }
    O<- max(N, 735.692308748692,na.rm=TRUE)
    FIN <-O
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HIJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 1),"Bakool_FutureRegion"]
    B<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    C<- tail(movavg(stations.long[(t-5):(t- 1),"Gedo_DollowStation_Juba_River"], 4,type="m"),1)
    D<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    E<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    G<- before.long[(t- 10),"Bakool_BeforeRegion"]
    H<- conflicts.long[(t- 1),"Bakool_Conflict"]
    I<- mean(before.long[(t-3):(t- 1),"Galgaduud_BeforeRegion"], na.rm=TRUE)
    J<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    K<- tail(movavg(future.long[(t-7):(t- 1),"Jubbada_Dhexe_FutureRegion"], 6,type="m"),1)
    L<- conflicts.long[(t- 1),"Bakool_Conflict"]
    M<- ceil(B)
    N<- ceil(D)
    O<- sin(1.80377407629271*N)
    P<- E%% G
    Q<- cos(P)
    if ( is.na(H) ){R<- -2.85097892966458}
    else if(H>0){R<- 1.78471224918239 }
    else{R<- -2.85097892966458 }
    S<- erf(R)
    if ( is.na(L) ){U<- -2.85097892966458}
    else if(L>0){U<- 1.78471224918239 }
    else{U<- -2.85097892966458 }
    V<- tanh(U)
    W<- max(735.692306686748,sum( 1.40440409544806*A*M*C*O*Q*S , -I , -J*K*V,na.rm=TRUE),na.rm=TRUE)
    FIN <-W
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HIJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 15),"Galgaduud_BeforeRegion"]
    B<- before.long[(t- 15),"Galgaduud_BeforeRegion"]
    C<- fatalities.long[(t- 15),"Woqooyi_Galbeed_Fatalities"]
    D<- future.long[(t- 1),"Nugaal_FutureRegion"]
    E<- future.long[(t- 6),"Togdheer_FutureRegion"]
    G<- fatalities.long[(t- 9),"Shabeellaha_Dhexe_Fatalities"]
    H<- tail(movavg(rain.long[(t-3):(t- 1),"Togdheer_rain"],2,type="w"),1)
    I<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    J<- C%% 1.32999589459216
    K<- atan2(B*J, 7135.25517579709)
    L<- max(735,sum( 2.04512098781508*D , 4.23968312979899*E*G , H , -300.954293264101*I,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 1.01571687384345*A*K , L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HIJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    B<- current.long[(t- 13),"Sool_CurrentRegion"]
    C<- tail(movavg(future.long[(t-5):(t- 1),"Shabeellaha_Hoose_FutureRegion"], 4,type="m"),1)
    D<- before.long[(t- 15),"Galgaduud_BeforeRegion"]
    E<- before.long[(t- 7),"Sanaag_BeforeRegion"]
    G<- tail(movavg(future.long[(t-9):(t- 1),"Sanaag_FutureRegion"],8,type="w"),1)
    H<- current.long[(t- 13),"Sool_CurrentRegion"]
    I<- fatalities.long[(t- 2),"Sanaag_Fatalities"]
    J<- median(before.long[(t-6):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    K<- current.long[(t- 3),"Jubbada_Dhexe_CurrentRegion"]
    L<- mean(current.long[(t-17):(t- 1),"Bay_CurrentRegion"], na.rm=TRUE)
    M<- tail(movavg(future.long[(t-10):(t- 1),"Sanaag_FutureRegion"],9,type="w"),1)
    N<- max(1.84131203796743*B, C,na.rm=TRUE)
    O<- max(1.46583905616537*D, E*G,na.rm=TRUE)
    P<- max(O, H,na.rm=TRUE)
    Q<- max(P, 5.33058114880592*I*J,na.rm=TRUE)
    R<- max(735.692307686805,sum( A , N , Q , -K , -0.14785716417411*L*M,na.rm=TRUE),na.rm=TRUE)
    FIN <-R
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HIJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 1),"Bakool_FutureRegion"]
    B<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    C<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    D<- tail(movavg(before.long[(t-4):(t- 1),"Bari_BeforeRegion"], 3,type="m"),1)
    E<- future.long[(t- 1),"Shabeellaha_Hoose_FutureRegion"]
    G<- rain.long[(t- 1),"Bari_rain"]
    H<- future.long[(t- 1),"Bakool_FutureRegion"]
    I<- future.long[(t- 1),"Bari_FutureRegion"]
    J<- future.long[(t- 1),"Bay_FutureRegion"]
    if ( is.na(A) || is.na( 41.7179505339188*B)){K<-0}
    else if(A> 41.7179505339188*B){K<-1 }
    else{K<-0 }
    L<- min(C, 908.185820489198,na.rm=TRUE)
    M<- max(0.0217523580848461*E*G, 0.0133629130054962*H*I,na.rm=TRUE)
    N<- max(D, M,na.rm=TRUE)
    O<- max(sum(L , N , -J,na.rm=TRUE), 1354.67975506829,na.rm=TRUE)
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 50583.9832091299*K , 1.83625543451209*O , -1751.8457540245,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HIJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- median(rain.long[(t-4):(t- 1),"Mudug_rain"], na.rm=TRUE)
    B<- median(rain.long[(t-7):(t- 1),"Bari_rain"], na.rm=TRUE)
    C<- future.long[(t- 1),"Nugaal_FutureRegion"]
    D<- before.long[(t- 15),"Galgaduud_BeforeRegion"]
    E<- tail(movavg(fatalities.long[(t-8):(t- 1),"Sanaag_Fatalities"],7,type="w"),1)
    G<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    H<- mean(future.long[(t-14):(t- 1),"Sanaag_FutureRegion"], na.rm=TRUE)
    I<- rain.long[(t- 2),"Mudug_rain"]
    J<- max(628.238752330015, A*B,na.rm=TRUE)
    K<- max(194.237731061069,sum( D*E , -6186.82355599983,na.rm=TRUE),na.rm=TRUE)
    L<- max(1.84397113106292*C, K,na.rm=TRUE)
    M<- tan(G)
    if ( is.na(1.91644519943863) || is.na( I)){N<-0}
    else if(1.91644519943863>= I){N<-1 }
    else{N<-0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 194.237731061069*J , 1.02828887194676*L , -121496.131052276 , -1.91644519943863*M*H*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HIJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 1),"Nugaal_FutureRegion"]
    B<- rivers.long[(t- 9),"Shabelle_River_discharge"]
    C<- median(current.long[(t-6):(t- 1),"Bay_CurrentRegion"], na.rm=TRUE)
    D<- rain.long[(t- 1),"Togdheer_rain"]
    E<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    G<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    H<- mean(rain.long[(t-7):(t- 1),"Hiiraan_rain"], na.rm=TRUE)
    I<- future.long[(t- 1),"Bakool_FutureRegion"]
    J<- tail(movavg(current.long[(t-13):(t- 1),"Shabeellaha_Hoose_CurrentRegion"], 12,type="m"),1)
    K<- before.long[(t- 4),"Nugaal_BeforeRegion"]
    L<- rain.long[(t- 1),"Togdheer_rain"]
    if ( is.na(B) || is.na( C)){M<-0}
    else if(B> C){M<-1 }
    else{M<-0 }
    if ( is.na(E) || is.na( 1116)){N<-0}
    else if(E> 1116){N<-1 }
    else{N<-0 }
    if ( is.na(I) || is.na( J)){O<-0}
    else if(I> J){O<-1 }
    else{O<-0 }
    P<- max(sum(735.692302955058 , 3464*M,na.rm=TRUE),sum( 1116*D*N , 88.8218931391454*G*H*O , K , -L^2,na.rm=TRUE),na.rm=TRUE)
    Q<- max(1.95797913216637*A, P,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_HIJUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 1),"Nugaal_FutureRegion"]
    B<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    C<- future.long[(t- 1),"Bakool_FutureRegion"]
    D<- tail(movavg(before.long[(t-11):(t- 1),"Hiiraan_BeforeRegion"],10,type="w"),1)
    E<- conflicts.long[(t- 1),"Hiiraan_Conflict"]
    G<- future.long[(t- 1),"Mudug_FutureRegion"]
    H<- conflicts.long[(t- 1),"Hiiraan_Conflict"]
    I<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    J<- future.long[(t- 1),"Bakool_FutureRegion"]
    K<- future.long[(t- 5),"Nugaal_FutureRegion"]
    L<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    M<- future.long[(t- 1),"Bakool_FutureRegion"]
    N<- rivers.long[(t- 1),"Juba_River_discharge"]
    O<- tail(movavg(before.long[(t-11):(t- 1),"Hiiraan_BeforeRegion"],10,type="w"),1)
    P<- conflicts.long[(t- 1),"Hiiraan_Conflict"]
    Q<- future.long[(t- 1),"Mudug_FutureRegion"]
    R<- conflicts.long[(t- 1),"Hiiraan_Conflict"]
    S<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    U<- future.long[(t- 1),"Bakool_FutureRegion"]
    V<- future.long[(t- 5),"Nugaal_FutureRegion"]
    if ( is.na(E) || is.na( G)){W<-0}
    else if(E== G){W<-1 }
    else{W<-0 }
    X<- tan(sum(0.7620784819751*I*J , -26128.4769803165,na.rm=TRUE))
    Y<- max(sum(0.7620784819751*B*C , -26128.4769803165,na.rm=TRUE),sum( D*W , H*X , K,na.rm=TRUE),na.rm=TRUE)
    Z<- max(58.6894748131062, Y,na.rm=TRUE)
    AA<- max(1.95797824561618*A,sum( 677 , Z,na.rm=TRUE),na.rm=TRUE)
    BB<- tan(N)
    if ( is.na(P) || is.na( Q)){CC<-0}
    else if(P== Q){CC<-1 }
    else{CC<-0 }
    DD<- tan(sum(0.7620784819751*S*U , -26128.4769803165,na.rm=TRUE))
    EE<- exp(sum(O*CC , R*DD , V,na.rm=TRUE))
    GF<- max(AA,sum( 0.7620784819751*L*M*BB , -EE,na.rm=TRUE),na.rm=TRUE)
    FIN <-GF
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}


