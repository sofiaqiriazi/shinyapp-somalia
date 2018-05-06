
modelarrivals_MJminus1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    B<- rain.long[(t- 1),"Gedo_rain"]
    C<- mean(fatalities.long[(t-10):(t- 1),"Jubbada_Dhexe_Fatalities"], na.rm=TRUE)
    D<- conflicts.long[(t- 1),"Awdal_Conflict"]
    E<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    G<- future.long[(t- 1),"Bakool_FutureRegion"]
    H<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    I<- tail(movavg(rain.long[(t-4):(t- 1),"Nugaal_rain"],3,type="w"),1)
    J<- future.long[(t- 1),"Bakool_FutureRegion"]
    K<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    L<- tail(movavg(rain.long[(t-4):(t- 1),"Nugaal_rain"],3,type="w"),1)
    M<- tail(movavg(future.long[(t-3):(t- 1),"Mudug_FutureRegion"], 2,type="m"),1)
    N<- tail(movavg(conflicts.long[(t-7):(t- 1),"Hiiraan_Conflict"],6,type="w"),1)
    O<- tan(sum(211.839221014594 , 0.000475150065347234*J*K,na.rm=TRUE))
    P<- asinh(N)
    Q<- max(sum(0.161617942449059*A , B*C , 0.161617942449059*D*E , 0.000475150065347234*G*H , I , O , -L*M*P,na.rm=TRUE), 534,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJ1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(conflicts.long[(t-3):(t- 1),"Awdal_Conflict"], 2,type="m"),1)
    B<- current.long[(t- 7),"Nugaal_CurrentRegion"]
    C<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    D<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    E<- tail(movavg(conflicts.long[(t-3):(t- 1),"Awdal_Conflict"], 2,type="m"),1)
    G<- future.long[(t- 1),"Bakool_FutureRegion"]
    H<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    I<- conflicts.long[(t- 2),"Nugaal_Conflict"]
    J<- future.long[(t- 1),"Bay_FutureRegion"]
    K<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    L<- future.long[(t- 1),"Bakool_FutureRegion"]
    M<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    N<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    O<- tan(0.121278507775576*K)
    P<- tan(0.000108381855598572*L*M)
    Q<- max(B,sum( 0.113347291064356*C , 0.121278507775576*D*E , 0.000108381855598572*G*H*I , -J , -O , -P,na.rm=TRUE),na.rm=TRUE)
    R<- max(598.928257672643, Q,na.rm=TRUE)
    if ( is.na(A) ){S<- N}
    else if(A>0){S<-sum( 1.61263913896463*R , -431.855018398068,na.rm=TRUE) }
    else{S<- N }
    FIN <-S
      PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJ2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJ3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    B<- conflicts.long[(t- 1),"Awdal_Conflict"]
    C<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    D<- future.long[(t- 1),"Bakool_FutureRegion"]
    E<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    G<- current.long[(t- 7),"Nugaal_CurrentRegion"]
    H<- stations.long[(t- 8),"Gedo_DollowStation_Juba_River"]
    I<- median(future.long[(t-4):(t- 1),"Shabeallaha_Dhexe_FutureRegion"], na.rm=TRUE)
    J<- median(before.long[(t-5):(t- 1),"Jubbada_Dhexe_BeforeRegion"], na.rm=TRUE)
    K<- max(sum(0.179247176234186*A , 0.183220821121486*B*C , 0.000473987450607226*D*E , G*H , -I , -J,na.rm=TRUE), 1190.96909634257,na.rm=TRUE)
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( K , -656.969072397052,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJ4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    B<- tail(movavg(future.long[(t-4):(t- 1),"Bakool_FutureRegion"], 3,type="m"),1)
    C<- conflicts.long[(t- 1),"Awdal_Conflict"]
    D<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    E<- tail(movavg(before.long[(t-5):(t- 1),"Shabeellaha_Dhexe_BeforeRegion"], 4,type="m"),1)
    G<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    H<- fatalities.long[(t- 1),"Gedo_Fatalities"]
    I<- stations.long[(t- 3),"Juba_Dhexe_BualleStation_Juba_River"]
    J<- current.long[(t- 1),"Awdal_CurrentRegion"]
    K<- future.long[(t- 1),"Gedo_FutureRegion"]
    L<- current.long[(t- 1),"Banadir_CurrentRegion"]
    if ( is.na(G) ){M<- 0.3675584383672*J}
    else if(G>0){M<- H*I }
    else{M<- 0.3675584383672*J }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 294.146064595007 , 0.000396587053713102*A*B , 0.0543017681375425*C*D*E , 1.41225605715946*M , -4.29171251272501e-5*K*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJ5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(future.long[(t-3):(t- 1),"Bakool_FutureRegion"], 2,type="m"),1)
    B<- tail(movavg(current.long[(t-4):(t- 1),"Galgaduud_CurrentRegion"], 3,type="m"),1)
    C<- conflicts.long[(t- 1),"Awdal_Conflict"]
    D<- tail(movavg(before.long[(t-5):(t- 1),"Shabeellaha_Dhexe_BeforeRegion"], 4,type="m"),1)
    E<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    G<- tail(movavg(before.long[(t-3):(t- 1),"Bari_BeforeRegion"],2,type="w"),1)
    H<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    I<- conflicts.long[(t- 1),"Awdal_Conflict"]
    J<- rain.long[(t- 1),"Jubbada_Dhexe_rain"]
    K<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    L<- future.long[(t- 1),"Sool_FutureRegion"]
    M<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    N<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    O<- min(0.177090223898728, E,na.rm=TRUE)
    if ( is.na(I) ){P<- K}
    else if(I>0){P<- J }
    else{P<- K }
    Q<- max(sum(0.000390242154367327*A*B , C*D*O , G , -H*P,na.rm=TRUE), 544.688166344436,na.rm=TRUE)
    R<- max(sum(Q , -L,na.rm=TRUE), 0.158471330367357*M,na.rm=TRUE)
    S<- tan(0.150535832060828*N)
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(S)){S <- 0 }
    FIN <-sum( R , -S,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJ6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Woqooyi_Galbeed_Conflict"]
    B<- fatalities.long[(t- 1),"Gedo_Fatalities"]
    C<- before.long[(t- 1),"Banadir_BeforeRegion"]
    D<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    E<- conflicts.long[(t- 1),"Awdal_Conflict"]
    G<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    H<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    I<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    J<- tail(movavg(future.long[(t-3):(t- 1),"Bakool_FutureRegion"], 2,type="m"),1)
    K<- median(fatalities.long[(t-6):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    L<- future.long[(t- 1),"Togdheer_FutureRegion"]
    M<- fatalities.long[(t- 1),"Banaadir_Fatalities"]
    N<- future.long[(t- 1),"Togdheer_FutureRegion"]
    if ( is.na(L) || is.na( M)){O<-0}
    else if(L<= M){O<-1 }
    else{O<-0 }
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
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 210.750412507048 , A*B , 7.38137299394999e-6*C*D , 0.0496511599239051*E*G*H , 0.00046764176515357*I*J*K*O , N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJ7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Awdal_Conflict"]
    B<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    C<- future.long[(t- 1),"Bakool_FutureRegion"]
    D<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    E<- future.long[(t- 1),"Bakool_FutureRegion"]
    G<- fatalities.long[(t- 9),"Awdal_Fatalities"]
    H<- median(conflicts.long[(t-6):(t- 1),"Jubbada_Dhexe_Conflict"], na.rm=TRUE)
    I<- current.long[(t- 7),"Nugaal_CurrentRegion"]
    J<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    K<- future.long[(t- 1),"Bakool_FutureRegion"]
    L<- future.long[(t- 1),"Gedo_FutureRegion"]
    M<- future.long[(t- 1),"Mudug_FutureRegion"]
    N<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    O<- max(0.000457589022171376*K, 0.161033534245094,na.rm=TRUE)
    P<- max(1.4575350401709*I, J*O,na.rm=TRUE)
    Q<- acosh(N)
    R<- max(sum(0.161033534245094*A*B , 0.000457589022171376*C*D , E*G*H , P , -L , -M*Q,na.rm=TRUE), 534,na.rm=TRUE)
    FIN <-R
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJ8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    B<- tail(movavg(rain.long[(t-17):(t- 1),"Bakool_rain"],16,type="w"),1)
    C<- future.long[(t- 1),"Bakool_FutureRegion"]
    D<- mean(rain.long[(t-4):(t- 1),"Bakool_rain"], na.rm=TRUE)
    E<- conflicts.long[(t- 2),"Nugaal_Conflict"]
    G<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    H<- before.long[(t- 1),"Sool_BeforeRegion"]
    I<- conflicts.long[(t- 1),"Awdal_Conflict"]
    J<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    K<- rain.long[(t- 5),"Bakool_rain"]
    L<- tanh(7.17132479494422e-5*C)
    M<- atan(L)
    if ( is.na(0.603120255476147*D) || is.na( E)){N<-0}
    else if(0.603120255476147*D< E){N<-1 }
    else{N<-0 }
    O<- max(sum(525.750209039171 , 0.0276268895040608*G*H,na.rm=TRUE), 0.0223010443327396*I*J*K,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( A*B*M*N , O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJ9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    B<- tail(movavg(future.long[(t-6):(t- 1),"Bakool_FutureRegion"], 5,type="m"),1)
    C<- conflicts.long[(t- 1),"Awdal_Conflict"]
    D<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    E<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    G<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    H<- fatalities.long[(t- 1),"Gedo_Fatalities"]
    I<- stations.long[(t- 3),"Juba_Dhexe_BualleStation_Juba_River"]
    J<- conflicts.long[(t- 1),"Awdal_Conflict"]
    K<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    L<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    M<- fatalities.long[(t- 1),"Bari_Fatalities"]
    N<- current.long[(t- 1),"Banadir_CurrentRegion"]
    O<- tail(movavg(future.long[(t-10):(t- 1),"Gedo_FutureRegion"], 9,type="m"),1)
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
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJ10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    B<- tail(movavg(future.long[(t-3):(t- 1),"Bakool_FutureRegion"], 2,type="m"),1)
    C<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    D<- conflicts.long[(t- 1),"Awdal_Conflict"]
    E<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    G<- tail(movavg(rain.long[(t-7):(t- 1),"Shabeellaha_Dhexe_rain"], 6,type="m"),1)
    H<- current.long[(t- 1),"Banadir_CurrentRegion"]
    I<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    J<- future.long[(t- 1),"Shabeallaha_Dhexe_FutureRegion"]
    K<- current.long[(t- 1),"Banadir_CurrentRegion"]
    L<- tail(movavg(future.long[(t-12):(t- 1),"Gedo_FutureRegion"], 11,type="m"),1)
    M<- max(540.991514159343,sum( 0.163326728220097*C , 0.164909454087873*D*E,na.rm=TRUE),na.rm=TRUE)
    N<- min(sum(0.00035437239968307*A*B , M , -G,na.rm=TRUE), H,na.rm=TRUE)
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( N , -0.0377778435551326*I*J , -5.99101225652324e-5*K*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Bakool_FutureRegion"]
    B<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    C<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    D<- conflicts.long[(t- 1),"Awdal_Conflict"]
    E<- future.long[(t- 1),"Bakool_FutureRegion"]
    G<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    H<- conflicts.long[(t- 14),"Nugaal_Conflict"]
    I<- conflicts.long[(t- 14),"Nugaal_Conflict"]
    J<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    K<- future.long[(t- 1),"Gedo_FutureRegion"]
    L<- current.long[(t- 1),"Banadir_CurrentRegion"]
    M<- min(G, H,na.rm=TRUE)
    N<- min(0.149919742480831*D,sum( 0.000363362735610532*E , M,na.rm=TRUE),na.rm=TRUE)
    O<- sinh(I)
    P<- max(sum(400.348450739419 , O,na.rm=TRUE), 0.159694268473079*J,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 0.000363362735610532*A*B , C*N , P , -4.62081190847604e-5*K*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rain.long[(t- 1),"Sanaag_rain"]
    B<- fatalities.long[(t- 17),"Bay_Fatalities"]
    C<- conflicts.long[(t- 1),"Awdal_Conflict"]
    D<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    E<- fatalities.long[(t- 11),"Sool_Fatalities"]
    G<- rain.long[(t- 1),"Sanaag_rain"]
    H<- rain.long[(t- 10),"Mudug_rain"]
    I<- median(fatalities.long[(t-8):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    J<- rain.long[(t- 1),"Gedo_rain"]
    K<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    L<- current.long[(t- 15),"Sanaag_CurrentRegion"]
    M<- future.long[(t- 1),"Bay_FutureRegion"]
    N<- max(J, 1.66768716357206*K,na.rm=TRUE)
    O<- max(sum(A*B , 0.189067008974022*C*D , E^2 , G*H*I , N , -L , -3.55180171284025*M,na.rm=TRUE), 534,na.rm=TRUE)
    FIN <-O
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Woqooyi_Galbeed_Conflict"]
    B<- fatalities.long[(t- 1),"Gedo_Fatalities"]
    C<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    D<- conflicts.long[(t- 1),"Awdal_Conflict"]
    E<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    G<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    H<- future.long[(t- 1),"Bakool_FutureRegion"]
    I<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    J<- future.long[(t- 1),"Bakool_FutureRegion"]
    K<- median(current.long[(t-5):(t- 1),"Nugaal_CurrentRegion"], na.rm=TRUE)
    L<- future.long[(t- 1),"Bakool_FutureRegion"]
    M<- mean(future.long[(t-5):(t- 1),"Gedo_FutureRegion"], na.rm=TRUE)
    if ( is.na(J) || is.na( K)){N<-0}
    else if(J>= K){N<-1 }
    else{N<-0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 291.388673462714 , A*B , 6.98696651805853e-6*C^2 , 0.0494177444999008*D*E*G , 0.000547804757886269*H*I*N , -0.00145320142814099*L*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Bakool_FutureRegion"]
    B<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    C<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    D<- conflicts.long[(t- 1),"Awdal_Conflict"]
    E<- conflicts.long[(t- 1),"Woqooyi_Galbeed_Conflict"]
    G<- fatalities.long[(t- 1),"Gedo_Fatalities"]
    H<- median(fatalities.long[(t-6):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    I<- fatalities.long[(t- 17),"Jubbada_Hoose_Fatalities"]
    J<- before.long[(t- 13),"Woqooyi_Galbeed_BeforeRegion"]
    K<- tail(movavg(current.long[(t-4):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], 3,type="m"),1)
    L<- fatalities.long[(t- 2),"Nugaal_Fatalities"]
    M<- fatalities.long[(t- 17),"Jubbada_Hoose_Fatalities"]
    N<- floor(0.242654235851738*D)
    O<- atan2(I, 0.000321358797321021)
    P<- max(0.127445775109852*K, L*M,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( 0.000321358797321021*A*B , 0.684330560164863*C*N , E*G*H*O , J , P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(current.long[(t-6):(t- 1),"Woqooyi_Galbeed_CurrentRegion"],5,type="w"),1)
    B<- tail(movavg(fatalities.long[(t-5):(t- 1),"Togdheer_Fatalities"], 4,type="m"),1)
    C<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    D<- tail(movavg(fatalities.long[(t-5):(t- 1),"Togdheer_Fatalities"], 4,type="m"),1)
    E<- conflicts.long[(t- 1),"Awdal_Conflict"]
    G<- fatalities.long[(t- 9),"Bari_Fatalities"]
    H<- tail(movavg(future.long[(t-3):(t- 1),"Bakool_FutureRegion"], 2,type="m"),1)
    I<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    J<- rain.long[(t- 9),"Bakool_rain"]
    K<- rain.long[(t- 6),"Bakool_rain"]
    L<- future.long[(t- 1),"Gedo_FutureRegion"]
    M<- current.long[(t- 1),"Banadir_CurrentRegion"]
    N<- min(0.13781000358179*E, G,na.rm=TRUE)
    O<- min(D, N,na.rm=TRUE)
    P<- min(0.000362640539347014*I, J,na.rm=TRUE)
    Q<- min(P, K,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 413 , 0.065125649309258*A*B , C*O , H*Q , -4.87538492986151e-5*L*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(current.long[(t-4):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], 3,type="m"),1)
    B<- future.long[(t- 1),"Bakool_FutureRegion"]
    C<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    D<- current.long[(t- 7),"Nugaal_CurrentRegion"]
    E<- median(fatalities.long[(t-11):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    G<- tail(movavg(before.long[(t-5):(t- 1),"Shabeellaha_Dhexe_BeforeRegion"], 4,type="m"),1)
    H<- conflicts.long[(t- 1),"Awdal_Conflict"]
    I<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    J<- rain.long[(t- 1),"Jubbada_Dhexe_rain"]
    K<- min(0.184943321926622*H, I,na.rm=TRUE)
    L<- max(sum(0.166789431425325*A , 0.000385169695843047*B*C , D*E , G*K , -230.048934239207 , -440.652026857125*J,na.rm=TRUE), 534,na.rm=TRUE)
    FIN <-L
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
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
    K<- tail(movavg(future.long[(t-11):(t- 1),"Gedo_FutureRegion"], 10,type="m"),1)
    L<- max(sum(0.000412517433989359*D*E , 0.0277228713192043*G*H*I,na.rm=TRUE), 571.188513377989,na.rm=TRUE)
    M<- max(1.29582415347052*B*C, L,na.rm=TRUE)
    N<- max(0.163082952983062*A, M,na.rm=TRUE)
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( N , -6.17159156717101e-5*J*K,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    B<- future.long[(t- 1),"Bakool_FutureRegion"]
    C<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    D<- current.long[(t- 7),"Nugaal_CurrentRegion"]
    E<- stations.long[(t- 8),"Gedo_DollowStation_Juba_River"]
    G<- median(fatalities.long[(t-6):(t- 1),"Jubbada_Dhexe_Fatalities"], na.rm=TRUE)
    H<- conflicts.long[(t- 1),"Awdal_Conflict"]
    I<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    J<- future.long[(t- 7),"Bay_FutureRegion"]
    if ( is.na(G) ){K<-sum( J , -1072.40891261129,na.rm=TRUE)}
    else if(G>0){K<- 0.176481674029325*H*I }
    else{K<-sum( J , -1072.40891261129,na.rm=TRUE) }
    L<- max(534,sum( 0.172769088503596*A , 0.000440811049120203*B*C , D*E , K , -674.90384801432,na.rm=TRUE),na.rm=TRUE)
    FIN <-L
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Awdal_Conflict"]
    B<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    C<- future.long[(t- 1),"Bakool_FutureRegion"]
    D<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    E<- current.long[(t- 1),"Gedo_CurrentRegion"]
    G<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    H<- conflicts.long[(t- 1),"Awdal_Conflict"]
    I<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    J<- conflicts.long[(t- 2),"Sool_Conflict"]
    K<- stations.long[(t- 1),"Gedo_LuuqStation_Juba_River"]
    L<- median(future.long[(t-4):(t- 1),"Sool_FutureRegion"], na.rm=TRUE)
    M<- future.long[(t- 1),"Gedo_FutureRegion"]
    N<- current.long[(t- 1),"Banadir_CurrentRegion"]
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
    FIN <-sum( 516.441291739128 , 0.0167040624753607*A*B , 0.000371778061622908*C*D , 2.52586674375331e-5*E*G , 0.0167040624753607*H*I*J , -K*L , -4.60676487241776e-5*M*N
               ,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MJJUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    B<- future.long[(t- 1),"Bakool_FutureRegion"]
    C<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    D<- conflicts.long[(t- 1),"Awdal_Conflict"]
    E<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    G<- stations.long[(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"]
    H<- rain.long[(t- 5),"Bakool_rain"]
    I<- future.long[(t- 1),"Gedo_FutureRegion"]
    J<- current.long[(t- 4),"Togdheer_CurrentRegion"]
    K<- conflicts.long[(t- 6),"Sool_Conflict"]
    L<- before.long[(t- 1),"Awdal_BeforeRegion"]
    M<- max(546.600000557223,sum( 0.208797772900475*A , 0.000508447269614676*B*C , 0.00866553251361124*D*E*G*H , -111.885277454987 , -I , -J*K,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( M , -L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}


