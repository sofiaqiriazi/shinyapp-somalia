
modelarrivals_WOminus1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Awdal_BeforeRegion"]
    B<- fatalities.long[(t- 16),"Shabeellaha_Hoose_Fatalities"]
    C<- median(conflicts.long[(t-10):(t- 1),"Nugaal_Conflict"], na.rm=TRUE)
    D<- rain.long[(t- 1),"Bakool_rain"]
    E<- median(conflicts.long[(t-10):(t- 1),"Nugaal_Conflict"], na.rm=TRUE)
    G<- median(stations.long[(t-16):(t- 1),"Juba_Dhexe_BualleStation_Juba_River"], na.rm=TRUE)
    H<- current.long[(t- 1),"Gedo_CurrentRegion"]
    I<- stations.long[(t- 1),"Gedo_DollowStation_Juba_River"]
    J<- future.long[(t- 11),"Nugaal_FutureRegion"]
    K<- before.long[(t- 1),"Awdal_BeforeRegion"]
    L<- median(before.long[(t-17):(t- 1),"Bakool_BeforeRegion"], na.rm=TRUE)
    M<- stations.long[(t- 1),"Gedo_DollowStation_Juba_River"]
    N<- mean(rain.long[(t-5):(t- 1),"Awdal_rain"], na.rm=TRUE)
    O<- future.long[(t- 3),"Nugaal_FutureRegion"]
    if(is.na(I)){P <- 0}
    else {P<- gauss(I^2)}
    Q<- cos(1.60377040222057*K)
    if(is.na(M)){R<-0}
    else{R<- gauss(M^2)}
    S<- logistic(R)
    U<- max(sum(B*C , D*E*G , H*P , J*Q , L*S , N,na.rm=TRUE), 2.19872989210658*O,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(U)){U <- 0 }
    FIN <-sum( 1.60377040222057*A , U,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WOminus2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 2),"Galgaduud_BeforeRegion"]
    B<- before.long[(t- 2),"Gedo_BeforeRegion"]
    C<- conflicts.long[(t- 4),"Sool_Conflict"]
    D<- fatalities.long[(t- 16),"Shabeellaha_Hoose_Fatalities"]
    E<- conflicts.long[(t- 2),"Togdheer_Conflict"]
    G<- before.long[(t- 2),"Nugaal_BeforeRegion"]
    H<- conflicts.long[(t- 2),"Shabeellaha_Dhexe_Conflict"]
    I<- current.long[(t- 2),"Mudug_CurrentRegion"]
    J<- conflicts.long[(t- 4),"Sool_Conflict"]
    K<- conflicts.long[(t- 2),"Shabeellaha_Dhexe_Conflict"]
    L<- before.long[(t- 2),"Galgaduud_BeforeRegion"]
    M<- before.long[(t- 2),"Gedo_BeforeRegion"]
    N<- rain.long[(t- 3),"Awdal_rain"]
    O<- rain.long[(t- 11),"Woqooyi_Galbeed_rain"]
    P<- max(E*G, 802,na.rm=TRUE)
    if ( is.na(J) ){Q<- 5.43898116004239e-5*L*M}
    else if(J>0){Q<- K }
    else{Q<- 5.43898116004239e-5*L*M }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 5.43898116004239e-5*A*B , C*D , 1.2694008657257*P , 0.000789807759957384*H*I*Q , N , O , -923.072085230172,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WO1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 1),"Bay_Fatalities"]
    B<- median(current.long[(t-15):(t- 1),"Togdheer_CurrentRegion"], na.rm=TRUE)
    C<- conflicts.long[(t- 1),"Shabeellaha_Hoose_Conflict"]
    D<- fatalities.long[(t- 12),"Nugaal_Fatalities"]
    E<- before.long[(t- 2),"Nugaal_BeforeRegion"]
    G<- conflicts.long[(t- 11),"Togdheer_Conflict"]
    H<- conflicts.long[(t- 17),"Shabeellaha_Dhexe_Conflict"]
    I<- rain.long[(t- 1),"Bakool_rain"]
    J<- tail(movavg(future.long[(t-3):(t- 1),"Togdheer_FutureRegion"], 2,type="m"),1)
    K<- fatalities.long[(t- 1),"Hiiraan_Fatalities"]
    L<- tail(movavg(future.long[(t-5):(t- 1),"Togdheer_FutureRegion"],4,type="w"),1)
    M<- median(rain.long[(t-3):(t- 1),"Jubbada_Dhexe_rain"], na.rm=TRUE)
    N<- round(0.203620817745419*I)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 1.84842172468375*A , 1.84842172468375*B , C*D , 0.870228113768443*E*G , H^2*N , J , -K , -L , -M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WO2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- tail(movavg(current.long[(t-10):(t- 1),"Galgaduud_CurrentRegion"],9,type="w"),1)
    B<- conflicts.long[(t- 1),"Awdal_Conflict"]
    C<- rain.long[(t- 3),"Banaadir_rain"]
    D<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    E<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    G<- before.long[(t- 4),"Nugaal_BeforeRegion"]
    H<- stations.long[(t- 15),"Gedo_LuuqStation_Juba_River"]
    I<- rain.long[(t- 17),"Togdheer_rain"]
    J<- rain.long[(t- 1),"Banaadir_rain"]
    K<- rain.long[(t- 1),"Nugaal_rain"]
    L<- before.long[(t- 1),"Bay_BeforeRegion"]
    M<- future.long[(t- 3),"Nugaal_FutureRegion"]
    N<- abs(sum(0.129812919409552*J*K , -0.000977854744388617*L*M,na.rm=TRUE))
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 0.0906330126410379*A , B*C , 0.168782940969762*D*E , G*H , I , N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WO3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    B<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    C<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    D<- fatalities.long[(t- 1),"Bari_Fatalities"]
    E<- current.long[(t- 1),"Mudug_CurrentRegion"]
    G<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    H<- current.long[(t- 1),"Mudug_CurrentRegion"]
    I<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    J<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    K<- fatalities.long[(t- 1),"Bari_Fatalities"]
    L<- before.long[(t- 1),"Gedo_BeforeRegion"]
    M<- current.long[(t- 1),"Mudug_CurrentRegion"]
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
    FIN <-sum( 225.699079573551 , 0.0339607044489115*A , 0.178917042233822*B*C , 0.0221782271952569*D*E , 0.00011975842214306*G*H , I , -J , -0.00499689439377272*K*L , -4.55680811026931e-6*M^2,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WO4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- mean(stations.long[(t-16):(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"], na.rm=TRUE)
    B<- median(current.long[(t-14):(t- 1),"Togdheer_CurrentRegion"], na.rm=TRUE)
    C<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    D<- future.long[(t- 3),"Nugaal_FutureRegion"]
    E<- conflicts.long[(t- 4),"Nugaal_Conflict"]
    G<- tail(movavg(before.long[(t-8):(t- 1),"Woqooyi_Galbeed_BeforeRegion"],7,type="w"),1)
    H<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    I<- future.long[(t- 15),"Nugaal_FutureRegion"]
    J<- fatalities.long[(t- 1),"Jubbada_Hoose_Fatalities"]
    K<- conflicts.long[(t- 4),"Nugaal_Conflict"]
    L<- tail(movavg(before.long[(t-8):(t- 1),"Woqooyi_Galbeed_BeforeRegion"],7,type="w"),1)
    M<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    N<- future.long[(t- 15),"Nugaal_FutureRegion"]
    O<- fatalities.long[(t- 1),"Jubbada_Hoose_Fatalities"]
    P<- before.long[(t- 17),"Sanaag_BeforeRegion"]
    Q<- rain.long[(t- 1),"Banaadir_rain"]
    R<- max(A*B, 0.0753287053426782*C,na.rm=TRUE)
    S<- min(sum(K*L , 2.19166338335333*M*N , O,na.rm=TRUE), P,na.rm=TRUE)
    U<- max(sum(E*G , 2.19166338335333*H*I , J , S,na.rm=TRUE), 0.0697774540429266*Q^2,na.rm=TRUE)
    V<- max(2.19166338335333*D, U,na.rm=TRUE)
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(V)){V <- 0 }
    FIN <-sum( R , V , -252.456208439236,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WO5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- tail(movavg(current.long[(t-11):(t- 1),"Galgaduud_CurrentRegion"],10,type="w"),1)
    B<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    C<- median(current.long[(t-8):(t- 1),"Sanaag_CurrentRegion"], na.rm=TRUE)
    D<- stations.long[(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"]
    E<- rain.long[(t- 1),"Bakool_rain"]
    G<- median(conflicts.long[(t-10):(t- 1),"Nugaal_Conflict"], na.rm=TRUE)
    H<- conflicts.long[(t- 1),"Sool_Conflict"]
    I<- fatalities.long[(t- 1),"Bari_Fatalities"]
    J<- current.long[(t- 1),"Mudug_CurrentRegion"]
    K<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    L<- rain.long[(t- 1),"Bakool_rain"]
    M<- rain.long[(t- 1),"Jubbada_Dhexe_rain"]
    N<- mean(fatalities.long[(t-7):(t- 1),"Bakool_Fatalities"], na.rm=TRUE)
    O<- median(rain.long[(t-4):(t- 1),"Awdal_rain"], na.rm=TRUE)
    if ( is.na(sum(K , -L,na.rm=TRUE)) || is.na( M)){P<-0}
    else if(sum(K , -L,na.rm=TRUE)> M){P<-1 }
    else{P<-0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 0.0873525104413986*A , 0.000535531948826976*B*C , D*E*G , 0.00703696986301492*H*I*J*P , N , O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WO6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 5),"Nugaal_BeforeRegion"]
    B<- before.long[(t- 2),"Nugaal_BeforeRegion"]
    C<- conflicts.long[(t- 11),"Togdheer_Conflict"]
    D<- fatalities.long[(t- 12),"Nugaal_Fatalities"]
    E<- median(current.long[(t-15):(t- 1),"Togdheer_CurrentRegion"], na.rm=TRUE)
    G<- rain.long[(t- 15),"Awdal_rain"]
    H<- fatalities.long[(t- 12),"Nugaal_Fatalities"]
    I<- median(current.long[(t-15):(t- 1),"Togdheer_CurrentRegion"], na.rm=TRUE)
    J<- rain.long[(t- 15),"Awdal_rain"]
    K<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    L<- fatalities.long[(t- 1),"Bay_Fatalities"]
    M<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    N<- rain.long[(t- 1),"Banaadir_rain"]
    O<- conflicts.long[(t- 1),"Mudug_Conflict"]
    P<- max(A, 0.875836677197961*B*C,na.rm=TRUE)
    Q<- min(sum(24.8775968645349*H , 2.04026045032698*I , J,na.rm=TRUE), K*L,na.rm=TRUE)
    R<- max(sum(24.8775968645349*D , 2.04026045032698*E , G , Q,na.rm=TRUE), 1.82285933618999*M*N,na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( P , R , -12.2929187691677*O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WO7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- mean(rain.long[(t-4):(t- 1),"Awdal_rain"], na.rm=TRUE)
    B<- mean(fatalities.long[(t-14):(t- 1),"Bay_Fatalities"], na.rm=TRUE)
    C<- median(rain.long[(t-15):(t- 1),"Bakool_rain"], na.rm=TRUE)
    D<- future.long[(t- 15),"Nugaal_FutureRegion"]
    E<- median(fatalities.long[(t-12):(t- 1),"Galguduud_Fatalities"], na.rm=TRUE)
    G<- fatalities.long[(t- 1),"Bay_Fatalities"]
    H<- future.long[(t- 15),"Nugaal_FutureRegion"]
    I<- stations.long[(t- 1),"Gedo_DollowStation_Juba_River"]
    J<- future.long[(t- 3),"Nugaal_FutureRegion"]
    K<- conflicts.long[(t- 3),"Bari_Conflict"]
    L<- conflicts.long[(t- 13),"Sanaag_Conflict"]
    M<- rain.long[(t- 1),"Bakool_rain"]
    N<- mean(conflicts.long[(t-3):(t- 1),"Mudug_Conflict"], na.rm=TRUE)
    O<- median(before.long[(t-10):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    P<- max(H, I*J,na.rm=TRUE)
    Q<- max(M*N, 3.28913418446612*O,na.rm=TRUE)
    R<- max(sum(D*E , G , P,na.rm=TRUE),sum( 21.3494365156776*K*L , Q , -979.705641449499,na.rm=TRUE),na.rm=TRUE)
    S<- max(B*C, R,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(S)){S <- 0 }
    FIN <-sum( A , S,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WO8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- rain.long[(t- 3),"Banaadir_rain"]
    B<- rain.long[(t- 1),"Banaadir_rain"]
    C<- rain.long[(t- 1),"Nugaal_rain"]
    D<- conflicts.long[(t- 15),"Shabeellaha_Dhexe_Conflict"]
    E<- future.long[(t- 3),"Nugaal_FutureRegion"]
    G<- rain.long[(t- 3),"Banaadir_rain"]
    H<- median(conflicts.long[(t-3):(t- 1),"Togdheer_Conflict"], na.rm=TRUE)
    I<- before.long[(t- 3),"Bari_BeforeRegion"]
    J<- before.long[(t- 1),"Awdal_BeforeRegion"]
    K<- fatalities.long[(t- 3),"Bari_Fatalities"]
    L<- tail(movavg(before.long[(t-7):(t- 1),"Bari_BeforeRegion"],6,type="w"),1)
    M<- conflicts.long[(t- 15),"Shabeellaha_Dhexe_Conflict"]
    N<- rain.long[(t- 1),"Nugaal_rain"]
    O<- max(sum(128.500143671391 , 2.29262231566583*B , C*D,na.rm=TRUE), 2.18720020313571*E,na.rm=TRUE)
    P<- max(I,sum( 1.40577857873834*J , 0.0961664708174644*K*L , M,na.rm=TRUE),na.rm=TRUE)
    Q<- max(G*H, P,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( A , O , Q , -N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WO9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    B<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    C<- fatalities.long[(t- 1),"Bari_Fatalities"]
    D<- current.long[(t- 1),"Mudug_CurrentRegion"]
    E<- rain.long[(t- 1),"Bakool_rain"]
    G<- stations.long[(t- 14),"Shabelle_Dhexe_JowharStation_Shabelle_River"]
    H<- median(conflicts.long[(t-11):(t- 1),"Nugaal_Conflict"], na.rm=TRUE)
    I<- fatalities.long[(t- 1),"Bay_Fatalities"]
    J<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    K<- median(future.long[(t-10):(t- 1),"Bakool_FutureRegion"], na.rm=TRUE)
    L<- fatalities.long[(t- 1),"Bari_Fatalities"]
    M<- before.long[(t- 1),"Gedo_BeforeRegion"]
    N<- current.long[(t- 1),"Mudug_CurrentRegion"]
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
    FIN <-sum( 0.182313827205433*A*B , 0.0215032119714942*C*D , E*G*H , I , J , K , -0.00486992546760009*L*M , -2.12985565117815e-6*N^2,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WO10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- mean(fatalities.long[(t-15):(t- 1),"Sanaag_Fatalities"], na.rm=TRUE)
    B<- median(before.long[(t-3):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    C<- rain.long[(t- 1),"Sool_rain"]
    D<- median(before.long[(t-6):(t- 1),"Galgaduud_BeforeRegion"], na.rm=TRUE)
    E<- mean(conflicts.long[(t-17):(t- 1),"Bakool_Conflict"], na.rm=TRUE)
    G<- median(rain.long[(t-7):(t- 1),"Sanaag_rain"], na.rm=TRUE)
    H<- median(current.long[(t-16):(t- 1),"Togdheer_CurrentRegion"], na.rm=TRUE)
    I<- mean(conflicts.long[(t-17):(t- 1),"Bakool_Conflict"], na.rm=TRUE)
    J<- median(rain.long[(t-7):(t- 1),"Sanaag_rain"], na.rm=TRUE)
    K<- future.long[(t- 13),"Nugaal_FutureRegion"]
    L<- rain.long[(t- 2),"Sanaag_rain"]
    M<- tan(sum(160.104125017834 , 43*I , 3.67245873006467*J,na.rm=TRUE))
    N<- max(D,sum( 160.104125017834 , 43*E , 3.67245873006467*G , 3.4639514022604*H , M,na.rm=TRUE),na.rm=TRUE)
    O<- max(sum(A*B , C , N,na.rm=TRUE), 0.0253192155079079*K^2*L,na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( O , -564.393549079287,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WOJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 1),"Bari_CurrentRegion"]
    B<- rain.long[(t- 1),"Togdheer_rain"]
    C<- fatalities.long[(t- 1),"Bari_Fatalities"]
    D<- current.long[(t- 1),"Mudug_CurrentRegion"]
    E<- median(fatalities.long[(t-12):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    G<- rain.long[(t- 1),"Hiiraan_rain"]
    H<- rain.long[(t- 2),"Sanaag_rain"]
    I<- fatalities.long[(t- 1),"Bari_Fatalities"]
    J<- current.long[(t- 1),"Mudug_CurrentRegion"]
    K<- median(fatalities.long[(t-12):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    L<- rain.long[(t- 1),"Hiiraan_rain"]
    M<- current.long[(t- 1),"Mudug_CurrentRegion"]
    N<- goats.long[(t- 1),"Sool_goatprice"]
    O<- tail(movavg(water.long[(t-4):(t- 1),"Gedo_WaterDrumPrice"],3,type="w"),1)
    P<- sin(0.0249921569161226*B)
    Q<- tan(sum(856.424848430606 , 0.0107784524169918*I*J*K , L,na.rm=TRUE))
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 856.424848430606 , A*P , 0.0107784524169918*C*D*E , G , H , Q , -1.87479957317995e-6*M^2 , -2.83169994599374e-8*N*O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WOJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 3),"Nugaal_Fatalities"]
    B<- rain.long[(t- 3),"Bakool_rain"]
    C<- rain.long[(t- 1),"Bakool_rain"]
    D<- rain.long[(t- 1),"Bay_rain"]
    E<- conflicts.long[(t- 16),"Bakool_Conflict"]
    G<- rain.long[(t- 1),"Bakool_rain"]
    H<- before.long[(t- 5),"Bari_BeforeRegion"]
    I<- rain.long[(t- 12),"Shabeellaha_Dhexe_rain"]
    J<- median(conflicts.long[(t-17):(t- 1),"Shabeellaha_Hoose_Conflict"], na.rm=TRUE)
    K<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    L<- fatalities.long[(t- 1),"Bari_Fatalities"]
    M<- current.long[(t- 1),"Mudug_CurrentRegion"]
    N<- before.long[(t- 1),"Mudug_BeforeRegion"]
    O<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    P<- asinh(D*E)
    Q<- acosh(K)
    R<- max(J*Q, 0.0214382077716753*L*M,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( A*B , C*P , G , H , I , R , -0.000147867312771182*N*O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WOJUN3arrivals <- function(start, end){
  
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    B<- fatalities.long[(t- 1),"Bay_Fatalities"]
    C<- median(conflicts.long[(t-16):(t- 1),"Nugaal_Conflict"], na.rm=TRUE)
    D<- current.long[(t- 1),"Bari_CurrentRegion"]
    E<- before.long[(t- 15),"Togdheer_BeforeRegion"]
    G<- rain.long[(t- 1),"Mudug_rain"]
    H<- before.long[(t- 5),"Bari_BeforeRegion"]
    I<- median(rain.long[(t-6):(t- 1),"Awdal_rain"], na.rm=TRUE)
    J<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    K<- conflicts.long[(t- 1),"Gedo_Conflict"]
    L<- fatalities.long[(t- 1),"Bari_Fatalities"]
    M<- current.long[(t- 1),"Mudug_CurrentRegion"]
    N<- water.long[(t- 1),"Gedo_WaterDrumPrice"]
    if ( is.na(E) || is.na( G)){O<-0}
    else if(E< G){O<-1 }
    else{O<-0 }
    P<- min(J^K, 0.020902281089979*L*M)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 530.402161360138 , 0.0252396456705556*A , B*C , D*O , H , I , P , -0.0252396456705556*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WOJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- tail(movavg(conflicts.long[(t-6):(t- 1),"Galgaduud_Conflict"],5,type="w"),1)
    B<- tail(movavg(conflicts.long[(t-6):(t- 1),"Shabeellaha_Hoose_Conflict"], 5,type="m"),1)
    C<- rain.long[(t- 1),"Shabeellaha_Hoose_rain"]
    D<- current.long[(t- 1),"Bari_CurrentRegion"]
    E<- mean(fatalities.long[(t-12):(t- 1),"Jubbada_Hoose_Fatalities"], na.rm=TRUE)
    G<- current.long[(t- 1),"Mudug_CurrentRegion"]
    H<- fatalities.long[(t- 1),"Bari_Fatalities"]
    I<- current.long[(t- 16),"Hiiraan_CurrentRegion"]
    J<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    K<- fatalities.long[(t- 2),"Gedo_Fatalities"]
    L<- current.long[(t- 16),"Hiiraan_CurrentRegion"]
    M<- current.long[(t- 1),"Mudug_CurrentRegion"]
    N<- fatalities.long[(t- 1),"Bari_Fatalities"]
    O<- current.long[(t- 16),"Hiiraan_CurrentRegion"]
    if ( is.na(I) || is.na( 721.425859644404)){P<-0}
    else if(I>= 721.425859644404){P<-1 }
    else{P<-0 }
    Q<- abs(sum(0.0262622900198411*J , -K , -0.0262622900198411*L,na.rm=TRUE))
    if ( is.na(O) || is.na( 721.425859644404)){R<-0}
    else if(O>= 721.425859644404){R<-1 }
    else{R<-0 }
    S<- tan(0.021038018185192*M*N*R)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(S)){S <- 0 }
    FIN <-sum( A*B , 0.000262069645104525*C*D*E , 0.021038018185192*G*H*P , Q , -S,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}
modelarrivals_WOJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- rain.long[(t- 1),"Bakool_rain"]
    B<- mean(conflicts.long[(t-7):(t- 1),"Nugaal_Conflict"], na.rm=TRUE)
    C<- fatalities.long[(t- 1),"Bari_Fatalities"]
    D<- current.long[(t- 1),"Jubbada_Dhexe_CurrentRegion"]
    E<- current.long[(t- 1),"Mudug_CurrentRegion"]
    G<- conflicts.long[(t- 4),"Nugaal_Conflict"]
    H<- median(future.long[(t-10):(t- 1),"Bakool_FutureRegion"], na.rm=TRUE)
    I<- tail(movavg(fatalities.long[(t-4):(t- 1),"Bay_Fatalities"], 3,type="m"),1)
    J<- rain.long[(t- 1),"Bakool_rain"]
    K<- rain.long[(t- 3),"Bakool_rain"]
    L<- rain.long[(t- 1),"Bakool_rain"]
    M<- rain.long[(t- 1),"Bakool_rain"]
    N<- mean(conflicts.long[(t-7):(t- 1),"Nugaal_Conflict"], na.rm=TRUE)
    O<- tail(movavg(before.long[(t-5):(t- 1),"Sool_BeforeRegion"],4,type="w"),1)
    P<- tail(movavg(fatalities.long[(t-4):(t- 1),"Bay_Fatalities"], 3,type="m"),1)
    Q<- tail(movavg(conflicts.long[(t-10):(t- 1),"Sanaag_Conflict"], 9,type="m"),1)
    R<- rain.long[(t- 7),"Awdal_rain"]
    S<- min(G, 4.45859260248485e-7,na.rm=TRUE)
    if ( is.na(L) || is.na( M*N)){U<-0}
    else if(L< M*N){U<-1 }
    else{U<-0 }
    V<- max(J*K*U,sum( 0.0420599532328549*O , P*Q,na.rm=TRUE),na.rm=TRUE)
    W<- max(I, V,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(W)){W <- 0 }
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( A*B , C^2*D*E*S , H , W , -R,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WOJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 1),"Gedo_Fatalities"]
    B<- before.long[(t- 17),"Sanaag_BeforeRegion"]
    C<- fatalities.long[(t- 11),"Togdheer_Fatalities"]
    D<- tail(movavg(rain.long[(t-5):(t- 1),"Mudug_rain"], 4,type="m"),1)
    E<- mean(fatalities.long[(t-13):(t- 1),"Bay_Fatalities"], na.rm=TRUE)
    G<- rain.long[(t- 1),"Shabeellaha_Hoose_rain"]
    H<- current.long[(t- 1),"Bari_CurrentRegion"]
    I<- fatalities.long[(t- 1),"Bari_Fatalities"]
    J<- current.long[(t- 1),"Mudug_CurrentRegion"]
    K<- mean(before.long[(t-9):(t- 1),"Nugaal_BeforeRegion"], na.rm=TRUE)
    L<- rain.long[(t- 3),"Awdal_rain"]
    M<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    N<- current.long[(t- 1),"Mudug_CurrentRegion"]
    O<- min(C, D,na.rm=TRUE)
    P<- max(B*O,sum( 6.30907722561123*E , 0.0175513468416476*G*H , 8.96575100148717e-5*I*J*K , L,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( A , P , -8.45598838844928e-5*M*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WOJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- rain.long[(t- 1),"Sool_rain"]
    B<- median(goats.long[(t-7):(t- 1),"Awdal_goatprice"], na.rm=TRUE)
    C<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    D<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    E<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    G<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    H<- tail(movavg(rain.long[(t-3):(t- 1),"Banaadir_rain"], 2,type="m"),1)
    I<- before.long[(t- 2),"Nugaal_BeforeRegion"]
    J<- tail(movavg(current.long[(t-11):(t- 1),"Shabeellaha_Hoose_CurrentRegion"],10,type="w"),1)
    K<- median(conflicts.long[(t-15):(t- 1),"Jubbada_Dhexe_Conflict"], na.rm=TRUE)
    L<- future.long[(t- 8),"Togdheer_FutureRegion"]
    M<- fatalities.long[(t- 13),"Bakool_Fatalities"]
    N<- median(goats.long[(t-7):(t- 1),"Awdal_goatprice"], na.rm=TRUE)
    O<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    P<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    Q<- fatalities.long[(t- 1),"Nugaal_Fatalities"]
    R<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    S<- tail(movavg(rain.long[(t-3):(t- 1),"Banaadir_rain"], 2,type="m"),1)
    U<- before.long[(t- 2),"Nugaal_BeforeRegion"]
    V<- tail(movavg(current.long[(t-11):(t- 1),"Shabeellaha_Hoose_CurrentRegion"],10,type="w"),1)
    W<- median(conflicts.long[(t-15):(t- 1),"Jubbada_Dhexe_Conflict"], na.rm=TRUE)
    X<- mean(stations.long[(t-9):(t- 1),"Gedo_DollowStation_Juba_River"], na.rm=TRUE)
    Y<- tail(movavg(future.long[(t-3):(t- 1),"Sool_FutureRegion"],2,type="w"),1)
    Z<- tan(sum(0.00113847356598114*N , 1.61596783226739*O*P , 1.61596783226739*Q*R*S , 0.000257597758831663*U*V*W,na.rm=TRUE))
    AA<- max(sum(0.00113847356598114*B , 1.61596783226739*C*D , 1.61596783226739*E*G*H , 0.000257597758831663*I*J*K , L,na.rm=TRUE), M*Z,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(X)){X <- 0 }
    if(is.infinite(Y)){Y <- 0 }
    FIN <-sum( A , AA , -X*Y,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}


modelarrivals_WOJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 17),"Sanaag_CurrentRegion"]
    B<- before.long[(t- 11),"Bari_BeforeRegion"]
    C<- rain.long[(t- 11),"Awdal_rain"]
    D<- future.long[(t- 14),"Sanaag_FutureRegion"]
    E<- stations.long[(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"]
    G<- tail(movavg(fatalities.long[(t-4):(t- 1),"Bay_Fatalities"], 3,type="m"),1)
    H<- fatalities.long[(t- 13),"Bakool_Fatalities"]
    I<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    J<- mean(before.long[(t-3):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    K<- median(conflicts.long[(t-9):(t- 1),"Nugaal_Conflict"], na.rm=TRUE)
    L<- fatalities.long[(t- 16),"Shabeellaha_Hoose_Fatalities"]
    M<- future.long[(t- 1),"Bari_FutureRegion"]
    N<- conflicts.long[(t- 4),"Jubbada_Dhexe_Conflict"]
    O<- rain.long[(t- 1),"Banaadir_rain"]
    P<- conflicts.long[(t- 2),"Sool_Conflict"]
    Q<- max(sum(E*G , H*I , J*K , L,na.rm=TRUE),sum( M*N , O*P,na.rm=TRUE),na.rm=TRUE)
    R<- max(sum(0.00217766976622489*A^2 , B , C , D,na.rm=TRUE), Q,na.rm=TRUE)
    FIN <-R
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WOJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- median(future.long[(t-8):(t- 1),"Gedo_FutureRegion"], na.rm=TRUE)
    B<- mean(future.long[(t-16):(t- 1),"Bakool_FutureRegion"], na.rm=TRUE)
    C<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    D<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    E<- fatalities.long[(t- 1),"Bari_Fatalities"]
    G<- current.long[(t- 1),"Mudug_CurrentRegion"]
    H<- mean(future.long[(t-16):(t- 1),"Bakool_FutureRegion"], na.rm=TRUE)
    I<- tail(movavg(fatalities.long[(t-3):(t- 1),"Jubbada_Hoose_Fatalities"],2,type="w"),1)
    J<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    K<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    L<- median(fatalities.long[(t-13):(t- 1),"Bay_Fatalities"], na.rm=TRUE)
    M<- median(before.long[(t-6):(t- 1),"Hiiraan_BeforeRegion"], na.rm=TRUE)
    N<- fatalities.long[(t- 1),"Bari_Fatalities"]
    O<- conflicts.long[(t- 11),"Galgaduud_Conflict"]
    P<- current.long[(t- 1),"Mudug_CurrentRegion"]
    if ( is.na(J) ){Q<- M}
    else if(J>0){Q<-sum( 0.0865343177175973*K , 7.91929173992739*L,na.rm=TRUE) }
    else{Q<- M }
    R<- max(sum(B , -C*D,na.rm=TRUE),sum( 5.22449629152265e-5*E*G*H , I , Q,na.rm=TRUE),na.rm=TRUE)
    S<- max(A, R,na.rm=TRUE)
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( S , -N*O , -1.66990097617493e-6*P^2,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_WOJUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- rain.long[(t- 1),"Sanaag_rain"]
    B<- mean(conflicts.long[(t-4):(t- 1),"Awdal_Conflict"], na.rm=TRUE)
    C<- rain.long[(t- 1),"Bakool_rain"]
    D<- before.long[(t- 4),"Nugaal_BeforeRegion"]
    E<- rain.long[(t- 1),"Bakool_rain"]
    G<- future.long[(t- 15),"Nugaal_FutureRegion"]
    H<- conflicts.long[(t- 13),"Shabeellaha_Hoose_Conflict"]
    I<- before.long[(t- 2),"Nugaal_BeforeRegion"]
    J<- before.long[(t- 8),"Nugaal_BeforeRegion"]
    K<- before.long[(t- 5),"Bari_BeforeRegion"]
    L<- rain.long[(t- 4),"Bari_rain"]
    M<- cos(C)
    if ( is.na(35.8778494584711) || is.na( H)){N<-0}
    else if(35.8778494584711< H){N<-1 }
    else{N<-0 }
    O<- max(0.0847274240791987*I*J,sum( 172.244705815941 , K,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( A*B , 1.46839549958485*M*D , 1.39836198680329*E*G*N , O , -L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}


