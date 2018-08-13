

modelarrivals_BKminus1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(current.long[(t-5):(t- 1),"Togdheer_CurrentRegion"], 4,type="m"),1)
    B<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    C<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    D<- tail(movavg(current.long[(t-5):(t- 1),"Togdheer_CurrentRegion"], 4,type="m"),1)
    E<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    G<- mean(future.long[(t-5):(t- 1),"Bari_FutureRegion"], na.rm=TRUE)
    H<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    I<- rain.long[(t- 11),"Shabeellaha_Hoose_rain"]
    J<- fatalities.long[(t- 2),"Nugaal_Fatalities"]
    K<- future.long[(t- 6),"Sanaag_FutureRegion"]
    L<- mean(future.long[(t-5):(t- 1),"Bari_FutureRegion"], na.rm=TRUE)
    M<- stations.long[(t- 1),"Gedo_BardheereStation_Juba_River"]
    N<- before.long[(t- 1),"Bakool_BeforeRegion"]
    O<- before.long[(t- 1),"Bari_BeforeRegion"]
    P<- before.long[(t- 1),"Sool_BeforeRegion"]
    Q<- future.long[(t- 6),"Sanaag_FutureRegion"]
    R<- tail(movavg(future.long[(t-5):(t- 1),"Togdheer_FutureRegion"], 4,type="m"),1)
    if ( is.na(C) ){S<- G}
    else if(C>0){S<-sum( 0.187458533192864*D , E,na.rm=TRUE) }
    else{S<- G }
    U<- max(sum(H*I , 2.24184551449069*J*K,na.rm=TRUE), L,na.rm=TRUE)
    V<- O%% P
    W<- min(N, V,na.rm=TRUE)
    X<- max(U,sum( 1.37452697035014*M*W , Q,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(X)){X <- 0 }
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( 0.187458533192864*A , B , S , X , -R,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BKminus2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- mean(fatalities.long[(t-5):(t- 2),"Nugaal_Fatalities"], na.rm=TRUE)
    B<- before.long[(t- 2),"Woqooyi_Galbeed_BeforeRegion"]
    C<- tail(movavg(conflicts.long[(t-4):(t- 2),"Nugaal_Conflict"], 2,type="m"),1)
    D<- before.long[(t- 2),"Woqooyi_Galbeed_BeforeRegion"]
    E<- rain.long[(t- 12),"Hiiraan_rain"]
    G<- fatalities.long[(t- 2),"Shabeellaha_Dhexe_Fatalities"]
    H<- before.long[(t- 7),"Nugaal_BeforeRegion"]
    I<- current.long[(t- 11),"Togdheer_CurrentRegion"]
    J<- future.long[(t- 12),"Gedo_FutureRegion"]
    K<- future.long[(t- 2),"Sool_FutureRegion"]
    L<- future.long[(t- 2),"Sool_FutureRegion"]
    M<- goats.long[(t- 2),"Mudug_goatprice"]
    N<- cosh(C)
    O<- cosh(0.0033592382798024*D)
    P<- factorial(G)
    Q<- E%% P
    R<- max(J, 20.7751453551968*K,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( A^2 , 0.0033592382798024*B^2 , N*O*Q , H , I , R , -1.558669469668e-5*L*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BK1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- mean(rain.long[(t-5):(t- 1),"Bay_rain"], na.rm=TRUE)
    B<- mean(future.long[(t-17):(t- 1),"Nugaal_FutureRegion"], na.rm=TRUE)
    C<- median(before.long[(t-13):(t- 1),"Jubbada_Dhexe_BeforeRegion"], na.rm=TRUE)
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    G<- stations.long[(t- 1),"Gedo_DollowStation_Juba_River"]
    H<- median(before.long[(t-9):(t- 1),"Mudug_BeforeRegion"], na.rm=TRUE)
    I<- before.long[(t- 1),"Bari_BeforeRegion"]
    J<- before.long[(t- 6),"Woqooyi_Galbeed_BeforeRegion"]
    K<- mean(fatalities.long[(t-4):(t- 1),"Gedo_Fatalities"], na.rm=TRUE)
    L<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    M<- fatalities.long[(t- 9),"Sool_Fatalities"]
    N<- mean(rain.long[(t-8):(t- 1),"Bakool_rain"], na.rm=TRUE)
    O<- before.long[(t- 7),"Nugaal_BeforeRegion"]
    P<- median(future.long[(t-3):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    Q<- max(I, J,na.rm=TRUE)
    R<- max(H, Q,na.rm=TRUE)
    S<- sin(K)
    if ( is.na(S) ){U<- O}
    else if(S>0){U<- L*M*N }
    else{U<- O }
    V<- max(C,sum( 0.00547268244611714*D*E*G , R , U,na.rm=TRUE),na.rm=TRUE)
    W<- max(B,sum( V , -P,na.rm=TRUE),na.rm=TRUE)
    if ( is.na(A) ){X<- 7723}
    else if(A>0){X<- W }
    else{X<- 7723 }
    FIN <-X
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BK2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- tail(movavg(fatalities.long[(t-3):(t- 1),"Jubbada_Hoose_Fatalities"],2,type="w"),1)
    C<- fatalities.long[(t- 1),"Shabeellaha_Hoose_Fatalities"]
    D<- current.long[(t- 1),"Gedo_CurrentRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- future.long[(t- 13),"Shabeellaha_Hoose_FutureRegion"]
    H<- before.long[(t- 1),"Sool_BeforeRegion"]
    I<- tail(movavg(current.long[(t-6):(t- 1),"Togdheer_CurrentRegion"], 5,type="m"),1)
    J<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    K<- rain.long[(t- 7),"Awdal_rain"]
    L<- rain.long[(t- 10),"Nugaal_rain"]
    M<- tail(movavg(current.long[(t-6):(t- 1),"Togdheer_CurrentRegion"], 5,type="m"),1)
    N<- fatalities.long[(t- 1),"Shabeellaha_Hoose_Fatalities"]
    O<- tail(movavg(fatalities.long[(t-15):(t- 1),"Nugaal_Fatalities"], 14,type="m"),1)
    P<- before.long[(t- 1),"Bari_BeforeRegion"]
    Q<- future.long[(t- 13),"Shabeellaha_Hoose_FutureRegion"]
    R<- future.long[(t- 1),"Togdheer_FutureRegion"]
    S<- min(sum(E , G,na.rm=TRUE), H,na.rm=TRUE)
    U<- atan2(L, M)
    V<- max(0.348051001851398*I, 3.01969783475628*J*K*U,na.rm=TRUE)
    W<- max(sum(0.00042444201330642*D*S , V,na.rm=TRUE), N*O,na.rm=TRUE)
    if ( is.na(C) ){X<-sum( P , Q,na.rm=TRUE)}
    else if(C>0){X<- W }
    else{X<-sum( P , Q,na.rm=TRUE) }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(X)){X <- 0 }
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( A , B , X , -R,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BK3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(before.long[(t-7):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    B<- future.long[(t- 17),"Gedo_FutureRegion"]
    C<- fatalities.long[(t- 4),"Shabeellaha_Dhexe_Fatalities"]
    D<- rain.long[(t- 4),"Togdheer_rain"]
    E<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    G<- water.long[(t- 1),"Woqooyi_Galbeed_WaterDrumPrice"]
    H<- conflicts.long[(t- 1),"Awdal_Conflict"]
    I<- rain.long[(t- 6),"Togdheer_rain"]
    J<- fatalities.long[(t- 4),"Shabeellaha_Dhexe_Fatalities"]
    K<- before.long[(t- 1),"Bari_BeforeRegion"]
    L<- current.long[(t- 1),"Gedo_CurrentRegion"]
    M<- rain.long[(t- 4),"Togdheer_rain"]
    N<- before.long[(t- 1),"Bari_BeforeRegion"]
    O<- future.long[(t- 17),"Gedo_FutureRegion"]
    P<- sinh(H)
    Q<- max(sum(0.271694412738264*E , 0.227492977114644*G,na.rm=TRUE), P,na.rm=TRUE)
    if ( is.na(J) ){R<- M}
    else if(J>0){R<- 0.000499709523595132*K*L }
    else{R<- M }
    if ( is.na(I) ){S<- N}
    else if(I>0){S<- R }
    else{S<- N }
    U<- max(C*D,sum( Q , S , -O,na.rm=TRUE),na.rm=TRUE)
    V<- max(B, U,na.rm=TRUE)
    W<- max(V, 2098.54979385953,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(W)){W <- 0 }
    FIN <-sum( A , W , -1964.5044460368,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BK4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(current.long[(t-5):(t- 1),"Togdheer_CurrentRegion"], 4,type="m"),1)
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
    O<- tail(movavg(fatalities.long[(t-6):(t- 1),"Nugaal_Fatalities"],5,type="w"),1)
    P<- median(fatalities.long[(t-3):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    Q<- max(sum(0.311587052568519*A , B*C , 0.000518985717965901*D*E , G*H*I , J*K*L , M,na.rm=TRUE), N*O*P,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BK5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    B<- fatalities.long[(t- 1),"Sool_Fatalities"]
    C<- future.long[(t- 7),"Sanaag_FutureRegion"]
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- conflicts.long[(t- 14),"Bari_Conflict"]
    G<- future.long[(t- 1),"Bari_FutureRegion"]
    H<- future.long[(t- 1),"Bari_FutureRegion"]
    I<- fatalities.long[(t- 1),"Sool_Fatalities"]
    J<- conflicts.long[(t- 14),"Bari_Conflict"]
    K<- median(fatalities.long[(t-15):(t- 1),"Bari_Fatalities"], na.rm=TRUE)
    L<- future.long[(t- 7),"Sanaag_FutureRegion"]
    M<- H%% 1.39153017152163
    N<- round(L)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 0.23518311903389*A , 2.51628444234526*B*C , 0.514823672616685*D*E , 2.4219167593936*G*M , -I*J*K , -3.54974354299448*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BK6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    B<- before.long[(t- 1),"Bari_BeforeRegion"]
    C<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    D<- median(before.long[(t-10):(t- 1),"Mudug_BeforeRegion"], na.rm=TRUE)
    E<- median(fatalities.long[(t-6):(t- 1),"Jubbada_Dhexe_Fatalities"], na.rm=TRUE)
    G<- future.long[(t- 1),"Bari_FutureRegion"]
    H<- current.long[(t- 1),"Sool_CurrentRegion"]
    I<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    J<- future.long[(t- 1),"Bari_FutureRegion"]
    K<- rain.long[(t- 16),"Gedo_rain"]
    L<- rain.long[(t- 17),"Bari_rain"]
    M<- rain.long[(t- 17),"Bari_rain"]
    N<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    O<- future.long[(t- 1),"Bari_FutureRegion"]
    P<- rain.long[(t- 16),"Gedo_rain"]
    Q<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    R<- rain.long[(t- 17),"Bari_rain"]
    S<- before.long[(t- 1),"Bari_BeforeRegion"]
    U<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    V<- median(before.long[(t-10):(t- 1),"Mudug_BeforeRegion"], na.rm=TRUE)
    W<- median(fatalities.long[(t-6):(t- 1),"Jubbada_Dhexe_Fatalities"], na.rm=TRUE)
    X<- future.long[(t- 1),"Bari_FutureRegion"]
    Y<- current.long[(t- 1),"Sool_CurrentRegion"]
    if ( is.na(E) ){Z<- 236.892932989355}
    else if(E>0){Z<- 0.00121637532390596*G*H }
    else{Z<- 236.892932989355 }
    AA<- max(D, Z,na.rm=TRUE)
    if ( is.na(W) ){BB<- 236.892932989355}
    else if(W>0){BB<- 0.00121637532390596*X*Y }
    else{BB<- 236.892932989355 }
    CC<- max(V, BB,na.rm=TRUE)
    if ( is.na(0.0017791371893848*N*O*P) ){DD<-sum( 5.31974442416125e-7*S^2*U , CC,na.rm=TRUE)}
    else if(0.0017791371893848*N*O*P>0){DD<- Q*R }
    else{DD<-sum( 5.31974442416125e-7*S^2*U , CC,na.rm=TRUE) }
    EE<- max(sum(5.31974442416125e-7*B^2*C , AA,na.rm=TRUE),sum( 0.0017791371893848*I*J*K*L , M , DD,na.rm=TRUE),na.rm=TRUE)
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

modelarrivals_BK7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(fatalities.long[(t-3):(t- 1),"Awdal_Fatalities"], 2,type="m"),1)
    B<- future.long[(t- 1),"Bari_FutureRegion"]
    C<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    D<- future.long[(t- 1),"Bari_FutureRegion"]
    E<- fatalities.long[(t- 14),"Awdal_Fatalities"]
    G<- before.long[(t- 1),"Bari_BeforeRegion"]
    H<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    I<- median(conflicts.long[(t-5):(t- 1),"Hiiraan_Conflict"], na.rm=TRUE)
    J<- future.long[(t- 9),"Woqooyi_Galbeed_FutureRegion"]
    K<- median(before.long[(t-9):(t- 1),"Mudug_BeforeRegion"], na.rm=TRUE)
    L<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    M<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    N<- future.long[(t- 10),"Awdal_FutureRegion"]
    O<- rain.long[(t- 6),"Shabeellaha_Dhexe_rain"]
    P<- conflicts.long[(t- 1),"Shabeellaha_Hoose_Conflict"]
    if ( is.na(L) ){Q<- O}
    else if(L>0){Q<- M*N }
    else{Q<- O }
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
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( A^2*B , 1.9414556014478*C*D*E , 0.000581977895805242*G*H*I , J , K , Q , -P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BK8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    B<- rain.long[(t- 1),"Shabeellaha_Hoose_rain"]
    C<- fatalities.long[(t- 4),"Shabeellaha_Dhexe_Fatalities"]
    D<- rain.long[(t- 16),"Bari_rain"]
    E<- future.long[(t- 16),"Awdal_FutureRegion"]
    G<- rain.long[(t- 17),"Bari_rain"]
    H<- tail(movavg(fatalities.long[(t-16):(t- 1),"Awdal_Fatalities"], 15,type="m"),1)
    I<- median(future.long[(t-3):(t- 1),"Galgaduud_FutureRegion"], na.rm=TRUE)
    J<- before.long[(t- 1),"Bari_BeforeRegion"]
    K<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    L<- future.long[(t- 5),"Bari_FutureRegion"]
    M<- median(before.long[(t-13):(t- 1),"Mudug_BeforeRegion"], na.rm=TRUE)
    N<- sqrt(K)
    O<- max(sum(C*D , E*G , 2.48636601978744*H*I , -562.586319542152,na.rm=TRUE), 0.0500487899820568*J*N,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( A*B , 1.3043749301732*O , L , M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BK9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- mean(before.long[(t-9):(t- 1),"Bari_BeforeRegion"], na.rm=TRUE)
    C<- conflicts.long[(t- 1),"Sanaag_Conflict"]
    D<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    E<- mean(fatalities.long[(t-5):(t- 1),"Jubbada_Dhexe_Fatalities"], na.rm=TRUE)
    G<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    H<- current.long[(t- 1),"Jubbada_Hoose_CurrentRegion"]
    I<- rain.long[(t- 17),"Mudug_rain"]
    J<- mean(rain.long[(t-12):(t- 1),"Togdheer_rain"], na.rm=TRUE)
    K<- before.long[(t- 1),"Bari_BeforeRegion"]
    L<- mean(before.long[(t-9):(t- 1),"Bari_BeforeRegion"], na.rm=TRUE)
    M<- rain.long[(t- 1),"Nugaal_rain"]
    N<- rain.long[(t- 17),"Mudug_rain"]
    O<- tail(movavg(before.long[(t-5):(t- 1),"Sanaag_BeforeRegion"], 4,type="m"),1)
    P<- cos(0.0104132187598937*K*L)
    Q<- max(C*D*E,sum( 0.22589799365995*G , 0.0680639351782458*H , I*J*P , -M*N,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 0.0104132187598937*A*B , Q , -0.123177390356817*O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BK10arrivals <- function(start, end){
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BKJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    B<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    C<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    D<- rain.long[(t- 1),"Hiiraan_rain"]
    E<- current.long[(t- 1),"Sool_CurrentRegion"]
    G<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    H<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    I<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    J<- current.long[(t- 1),"Bay_CurrentRegion"]
    K<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    L<- fatalities.long[(t- 1),"Sool_Fatalities"]
    M<- rain.long[(t- 5),"Nugaal_rain"]
    N<- tail(movavg(rivers.long[(t-5):(t- 1),"Shabelle_River_discharge"],4,type="w"),1)
    O<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    P<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    Q<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    R<- current.long[(t- 1),"Bay_CurrentRegion"]
    S<- max(sum(G*H , 0.0139257547609202*I*J,na.rm=TRUE), K*L*M,na.rm=TRUE)
    U<- max(320.720542348654,sum( S , -750.723955974737,na.rm=TRUE),na.rm=TRUE)
    V<- max(4.87743342833218*B*C,sum( 0.004776862359227*D*E , U,na.rm=TRUE),na.rm=TRUE)
    if ( is.na(A) ){W<-sum( O*P , 0.0139257547609202*Q*R,na.rm=TRUE)}
    else if(A>0){W<-sum( V , -N,na.rm=TRUE) }
    else{W<-sum( O*P , 0.0139257547609202*Q*R,na.rm=TRUE) }
    FIN <-W
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BKJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- stations.long[(t- 1),"Gedo_DollowStation_Juba_River"]
    B<- current.long[(t- 1),"Bay_CurrentRegion"]
    C<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    D<- fatalities.long[(t- 5),"Woqooyi_Galbeed_Fatalities"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    H<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    I<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    J<- rivers.long[(t- 1),"Shabelle_River_discharge"]
    K<- median(before.long[(t-11):(t- 1),"Mudug_BeforeRegion"], na.rm=TRUE)
    L<- fatalities.long[(t- 5),"Woqooyi_Galbeed_Fatalities"]
    M<- before.long[(t- 1),"Bari_BeforeRegion"]
    N<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    O<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    P<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    Q<- future.long[(t- 7),"Togdheer_FutureRegion"]
    if ( is.na(3593.3172160745) || is.na( B)){R<-0}
    else if(3593.3172160745< B){R<-1 }
    else{R<-0 }
    S<- max(sum(3.05505798055072^D , E,na.rm=TRUE), 0.00103968496426174*G^3*H*I,na.rm=TRUE)
    U<- max(sum(C^2 , S,na.rm=TRUE), J,na.rm=TRUE)
    V<- max(sum(3.05505798055072^L , M,na.rm=TRUE), 0.00103968496426174*N^3*O*P,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( 4.07256996965414*A*R*U , K , V , -Q,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BKJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 7),"Bari_BeforeRegion"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    C<- before.long[(t- 1),"Bari_BeforeRegion"]
    D<- future.long[(t- 1),"Bari_FutureRegion"]
    E<- tail(movavg(rain.long[(t-8):(t- 1),"Nugaal_rain"],7,type="w"),1)
    G<- median(fatalities.long[(t-8):(t- 1),"Woqooyi_Galbeed_Fatalities"], na.rm=TRUE)
    H<- before.long[(t- 1),"Bari_BeforeRegion"]
    I<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    J<- before.long[(t- 1),"Awdal_BeforeRegion"]
    K<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    L<- current.long[(t- 10),"Bari_CurrentRegion"]
    M<- before.long[(t- 1),"Bay_BeforeRegion"]
    N<- before.long[(t- 1),"Bay_BeforeRegion"]
    O<- sqrt(C)
    P<- (1.74923126190056*D*E*G)
    if ( is.na(H) || is.na( I)){Q<-0}
    else if(H> I){Q<-1 }
    else{Q<-0 }
    R<- max(sum(0.00768748123481377*B*O , P^Q , J , K,na.rm=TRUE), 0.270358768928302*L,na.rm=TRUE)
    S<- tan(0.00768748123481377*M)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( A , R , -S , -1.77450150367982e-6*N^2,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BKJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Awdal_Fatalities"]
    B<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    C<- before.long[(t- 12),"Awdal_BeforeRegion"]
    D<- before.long[(t- 1),"Bay_BeforeRegion"]
    E<- future.long[(t- 5),"Bari_FutureRegion"]
    G<- median(fatalities.long[(t-6):(t- 1),"Jubbada_Dhexe_Fatalities"], na.rm=TRUE)
    H<- before.long[(t- 1),"Bari_BeforeRegion"]
    I<- median(before.long[(t-12):(t- 1),"Mudug_BeforeRegion"], na.rm=TRUE)
    J<- fatalities.long[(t- 4),"Shabeellaha_Dhexe_Fatalities"]
    K<- rain.long[(t- 4),"Nugaal_rain"]
    L<- before.long[(t- 16),"Togdheer_BeforeRegion"]
    M<- max(sum(0.0405110398764166*D , E*G , H , I,na.rm=TRUE), J*K,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( A^7.03940995994626 , 1.12964136082956*B^2*C , 1.07987750850425*M , L , -149.161942724422,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BKJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(fatalities.long[(t-4):(t- 1),"Jubbada_Dhexe_Fatalities"], 3,type="m"),1)
    B<- median(before.long[(t-15):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    C<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    D<- current.long[(t- 1),"Bay_CurrentRegion"]
    E<- fatalities.long[(t- 1),"Galguduud_Fatalities"]
    G<- mean(fatalities.long[(t-5):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    H<- rain.long[(t- 1),"Hiiraan_rain"]
    I<- current.long[(t- 1),"Sool_CurrentRegion"]
    J<- fatalities.long[(t- 1),"Galguduud_Fatalities"]
    K<- stations.long[(t- 4),"Juba_Dhexe_BualleStation_Juba_River"]
    L<- fatalities.long[(t- 8),"Mudug_Fatalities"]
    M<- stations.long[(t- 16),"Juba_Dhexe_BualleStation_Juba_River"]
    N<- mean(fatalities.long[(t-5):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    O<- fatalities.long[(t- 1),"Galguduud_Fatalities"]
    P<- mean(fatalities.long[(t-5):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    Q<- rain.long[(t- 1),"Hiiraan_rain"]
    R<- current.long[(t- 1),"Sool_CurrentRegion"]
    S<- fatalities.long[(t- 1),"Galguduud_Fatalities"]
    U<- stations.long[(t- 4),"Juba_Dhexe_BualleStation_Juba_River"]
    V<- fatalities.long[(t- 8),"Mudug_Fatalities"]
    W<- stations.long[(t- 16),"Juba_Dhexe_BualleStation_Juba_River"]
    X<- mean(fatalities.long[(t-5):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    Y<- stations.long[(t- 4),"Juba_Dhexe_BualleStation_Juba_River"]
    Z<- fatalities.long[(t- 8),"Mudug_Fatalities"]
    AA<- stations.long[(t- 16),"Juba_Dhexe_BualleStation_Juba_River"]
    BB<- max(A*B, 0.0139360865933054*C*D,na.rm=TRUE)
    CC<- max(982, BB,na.rm=TRUE)
    DD<- max(sum(E*G , 0.00496803418193589*H*I , -J,na.rm=TRUE), K*L*M*N,na.rm=TRUE)
    EE<- max(sum(O*P , 0.00496803418193589*Q*R , -S,na.rm=TRUE), U*V*W*X,na.rm=TRUE)
    if ( is.na(DD) ){GF<- Y*Z*AA}
    else if(DD>0){GF<- EE }
    else{GF<- Y*Z*AA }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(F)){F <- 0 }
    FIN <-sum( CC , GF , -885.863232474793,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BKJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(before.long[(t-11):(t- 1),"Jubbada_Hoose_BeforeRegion"],10,type="w"),1)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BKJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    B<- current.long[(t- 1),"Bay_CurrentRegion"]
    C<- rain.long[(t- 1),"Hiiraan_rain"]
    D<- current.long[(t- 1),"Sool_CurrentRegion"]
    E<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    G<- mean(conflicts.long[(t-13):(t- 1),"Awdal_Conflict"], na.rm=TRUE)
    H<- median(fatalities.long[(t-5):(t- 1),"Mudug_Fatalities"], na.rm=TRUE)
    I<- median(future.long[(t-17):(t- 1),"Banadir_FutureRegion"], na.rm=TRUE)
    J<- rain.long[(t- 1),"Hiiraan_rain"]
    K<- tail(movavg(rivers.long[(t-12):(t- 1),"Shabelle_River_discharge"], 11,type="m"),1)
    L<- max(434, 0.00483795922499033*A*B,na.rm=TRUE)
    M<- tan(I)
    N<- max(0.00483795922499033*C*D,sum( E*G*H , 10.323262606237*M^3 , J,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 2.93280332588889*L , N , -984.188314856659 , -K,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BKJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- before.long[(t- 1),"Mudug_BeforeRegion"]
    C<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    D<- current.long[(t- 1),"Bay_CurrentRegion"]
    E<- median(current.long[(t-15):(t- 1),"Nugaal_CurrentRegion"], na.rm=TRUE)
    G<- current.long[(t- 1),"Shabeellaha_Dhexe_CurrentRegion"]
    H<- rain.long[(t- 1),"Hiiraan_rain"]
    I<- current.long[(t- 1),"Sool_CurrentRegion"]
    J<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    K<- median(fatalities.long[(t-6):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    L<- stations.long[(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"]
    M<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    N<- tail(movavg(future.long[(t-4):(t- 1),"Woqooyi_Galbeed_FutureRegion"],3,type="w"),1)
    O<- acosh(A)
    P<- min(B, 0.00155383499319322*C*D,na.rm=TRUE)
    if ( is.na(L) || is.na( M)){Q<-0}
    else if(L< M){Q<-1 }
    else{Q<-0 }
    R<- max(sum(0.0187584229900101*G , 0.00450851776594516*H*I,na.rm=TRUE), J*K*Q,na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( O*P , E , R , -N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BKJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    B<- current.long[(t- 1),"Bay_CurrentRegion"]
    C<- fatalities.long[(t- 2),"Nugaal_Fatalities"]
    D<- future.long[(t- 6),"Sanaag_FutureRegion"]
    E<- rain.long[(t- 1),"Hiiraan_rain"]
    G<- current.long[(t- 1),"Sool_CurrentRegion"]
    H<- tail(movavg(before.long[(t-3):(t- 1),"Bari_BeforeRegion"],2,type="w"),1)
    I<- stations.long[(t- 10),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
    J<- tail(movavg(conflicts.long[(t-5):(t- 1),"Togdheer_Conflict"], 4,type="m"),1)
    K<- tail(movavg(fatalities.long[(t-5):(t- 1),"Shabeellaha_Dhexe_Fatalities"], 4,type="m"),1)
    L<- fatalities.long[(t- 1),"Jubbada_Hoose_Fatalities"]
    M<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    N<- max(387,sum( 0.00436489107173919*A*B , C*D , 1.02273267191147e-5*E*G*H , I*J*K , L , M,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 2.17941523675548*N , -658.147709120857,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_BKJUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    B<- tail(movavg(fatalities.long[(t-17):(t- 1),"Jubbada_Dhexe_Fatalities"],16,type="w"),1)
    C<- conflicts.long[(t- 1),"Shabeellaha_Dhexe_Conflict"]
    D<- future.long[(t- 6),"Togdheer_FutureRegion"]
    E<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    G<- current.long[(t- 1),"Bay_CurrentRegion"]
    H<- future.long[(t- 10),"Togdheer_FutureRegion"]
    I<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    J<- tail(movavg(conflicts.long[(t-5):(t- 1),"Jubbada_Dhexe_Conflict"], 4,type="m"),1)
    K<- rain.long[(t- 1),"Hiiraan_rain"]
    L<- current.long[(t- 1),"Sool_CurrentRegion"]
    M<- tail(movavg(rivers.long[(t-10):(t- 1),"Shabelle_River_discharge"], 9,type="m"),1)
    N<- max(1068.56394222357, 0.0139192579548518*E*G,na.rm=TRUE)
    O<- exp(J)
    P<- max(I*O, 0.00487813201352683*K*L,na.rm=TRUE)
    Q<- max(H, P,na.rm=TRUE)
    R<- max(C*D,sum( N , Q,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( A*B , R , -802.61592255675 , -M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

