
modelarrivals_MSminus21arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(rivers.long[(t-9):(t- 1),"Juba_River_discharge"], na.rm=TRUE)
    B<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    C<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    D<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    E<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    G<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    H<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    I<- conflicts.long[(t- 1),"Woqooyi_Galbeed_Conflict"]
    J<- rain.long[(t- 1),"Bay_rain"]
    K<- future.long[(t- 1),"Gedo_FutureRegion"]
    L<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    M<- rain.long[(t- 5),"Shabeellaha_Dhexe_rain"]
    N<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    O<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    P<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    Q<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    R<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    S<- conflicts.long[(t- 1),"Woqooyi_Galbeed_Conflict"]
    U<- rain.long[(t- 1),"Bay_rain"]
    V<- future.long[(t- 1),"Gedo_FutureRegion"]
    W<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    X<- before.long[(t- 1),"Bay_BeforeRegion"]
    Y<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    Z<- rain.long[(t- 5),"Shabeellaha_Dhexe_rain"]
    AA<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    if ( is.na(sum(0.00384202419572577*N*O , 4.68606911385675e-5*P*Q , R*S*U , -V,na.rm=TRUE)) ){BB<- Y*Z}
    else if(sum(0.00384202419572577*N*O , 4.68606911385675e-5*P*Q , R*S*U , -V,na.rm=TRUE)>0){BB<- 0.0227156535480595*W*X }
    else{BB<- Y*Z }
    CC<- max(sum(0.00384202419572577*C*D , 4.68606911385675e-5*E*G , H*I*J , -K , -L*M,na.rm=TRUE), BB,na.rm=TRUE)
    if ( is.na(B) ){DD<- AA}
    else if(B>0){DD<- CC }
    else{DD<- AA }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(D)){D <- 0 }
    FIN <-sum( A , DD,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MSminus2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 2),"Shabeellaha_Dhexe_Fatalities"]
    B<- stations.long[(t- 2),"Gedo_DollowStation_Juba_River"]
    C<- rain.long[(t- 2),"Bari_rain"]
    D<- future.long[(t- 6),"Nugaal_FutureRegion"]
    E<- conflicts.long[(t- 2),"Bay_Conflict"]
    G<- fatalities.long[(t- 2),"Nugaal_Fatalities"]
    H<- rivers.long[(t- 12),"Shabelle_River_discharge"]
    I<- before.long[(t- 2),"Woqooyi_Galbeed_BeforeRegion"]
    J<- future.long[(t- 2),"Togdheer_FutureRegion"]
    K<- median(conflicts.long[(t-8):(t- 2),"Bakool_Conflict"], na.rm=TRUE)
    L<- future.long[(t- 8),"Bay_FutureRegion"]
    M<- median(fatalities.long[(t-9):(t- 2),"Nugaal_Fatalities"], na.rm=TRUE)
    N<- fatalities.long[(t- 2),"Shabeellaha_Dhexe_Fatalities"]
    O<- future.long[(t- 2),"Togdheer_FutureRegion"]
    P<- median(conflicts.long[(t-8):(t- 2),"Bakool_Conflict"], na.rm=TRUE)
    Q<- future.long[(t- 8),"Bay_FutureRegion"]
    R<- median(fatalities.long[(t-9):(t- 2),"Nugaal_Fatalities"], na.rm=TRUE)
    S<- max(D,sum( E^2 , G*H,na.rm=TRUE),na.rm=TRUE)
    U<- max(A*B*C, S,na.rm=TRUE)
    V<- max(515.381307465637, U,na.rm=TRUE)
    W<- max(J*K, 1.03689021684028*L*M,na.rm=TRUE)
    X<- max(I, W,na.rm=TRUE)
    Y<- max(O*P, 1.03689021684028*Q*R,na.rm=TRUE)
    if ( is.na(N) ){Z<- Y}
    else if(N>0){Z<- 515.381307465637 }
    else{Z<- Y }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(X)){X <- 0 }
    if(is.infinite(Z)){Z <- 0 }
    FIN <-sum( 1.09228087792705*V , X , -Z,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MS1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    B<- before.long[(t- 13),"Togdheer_BeforeRegion"]
    C<- median(current.long[(t-11):(t- 1),"Hiiraan_CurrentRegion"], na.rm=TRUE)
    D<- before.long[(t- 13),"Togdheer_BeforeRegion"]
    E<- median(future.long[(t-3):(t- 1),"Togdheer_FutureRegion"], na.rm=TRUE)
    G<- tail(movavg(future.long[(t-6):(t- 1),"Bakool_FutureRegion"], 5,type="m"),1)
    H<- current.long[(t- 11),"Sanaag_CurrentRegion"]
    I<- stations.long[(t- 14),"Juba_Dhexe_BualleStation_Juba_River"]
    J<- median(rivers.long[(t-12):(t- 1),"Shabelle_River_discharge"], na.rm=TRUE)
    K<- current.long[(t- 14),"Nugaal_CurrentRegion"]
    L<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    M<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    N<- max(G, 2.54872296968871*H,na.rm=TRUE)
    O<- max(10.1525523599547*E, N,na.rm=TRUE)
    P<- max(C,sum( D , O,na.rm=TRUE),na.rm=TRUE)
    Q<- max(sum(I*J , K,na.rm=TRUE), 0.00393792248010304*L*M,na.rm=TRUE)
    R<- max(sum(0.178354362388297*A , B , P,na.rm=TRUE), Q,na.rm=TRUE)
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( R , -326.109520892347,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MS2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    B<- current.long[(t- 1),"Bay_CurrentRegion"]
    C<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    D<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    E<- rivers.long[(t- 1),"Juba_River_discharge"]
    G<- rain.long[(t- 1),"Togdheer_rain"]
    H<- future.long[(t- 1),"Bakool_FutureRegion"]
    I<- rain.long[(t- 1),"Togdheer_rain"]
    J<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    K<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    L<- future.long[(t- 1),"Bakool_FutureRegion"]
    M<- fatalities.long[(t- 1),"Jubbada_Hoose_Fatalities"]
    N<- future.long[(t- 1),"Bakool_FutureRegion"]
    O<- max(G, 0.0474169723348605*H*I,na.rm=TRUE)
    P<- max(sum(5.17746948315132e-5*C*D , E , O,na.rm=TRUE), 0.0040717698060428*J*K,na.rm=TRUE)
    Q<- max(sum(0.0188209830052748*A*B , P,na.rm=TRUE), L,na.rm=TRUE)
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( Q , -0.011134072490285*M*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MS3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rivers.long[(t- 13),"Juba_River_discharge"]
    B<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    C<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    D<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    E<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    G<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    H<- fatalities.long[(t- 1),"Hiiraan_Fatalities"]
    I<- median(rain.long[(t-11):(t- 1),"Sool_rain"], na.rm=TRUE)
    J<- conflicts.long[(t- 1),"Jubbada_Hoose_Conflict"]
    K<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    L<- mean(rivers.long[(t-4):(t- 1),"Juba_River_discharge"], na.rm=TRUE)
    M<- future.long[(t- 1),"Hiiraan_FutureRegion"]
    N<- rivers.long[(t- 13),"Juba_River_discharge"]
    O<- sin(0.0694275699474916*J*K)
    P<- max(B,sum( 0.004557249344714*C*D , 0.000369204954580446*E*G , 8.37512996580642*H*I/O , L , -M , -N,na.rm=TRUE),na.rm=TRUE)
    Q<- max(A, P,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MS4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    C<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    D<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    E<- stations.long[(t- 14),"Juba_Dhexe_BualleStation_Juba_River"]
    G<- mean(rivers.long[(t-8):(t- 1),"Juba_River_discharge"], na.rm=TRUE)
    H<- rain.long[(t- 1),"Hiiraan_rain"]
    I<- conflicts.long[(t- 2),"Woqooyi_Galbeed_Conflict"]
    J<- rain.long[(t- 11),"Sool_rain"]
    K<- rain.long[(t- 1),"Hiiraan_rain"]
    L<- future.long[(t- 7),"Sanaag_FutureRegion"]
    M<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    N<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    O<- max(sum(0.0194867910457064*A*B , 5.18890718990632e-5*C*D , 0.300636921129227*E*G , H*I*J , K , L,na.rm=TRUE), 0.00390207866431559*M*N,na.rm=TRUE)
    FIN <-O
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MS5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    B<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    C<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    D<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    E<- before.long[(t- 1),"Bay_BeforeRegion"]
    G<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    H<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    I<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    J<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    K<- rivers.long[(t- 1),"Juba_River_discharge"]
    L<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    M<- future.long[(t- 1),"Bakool_FutureRegion"]
    N<- rain.long[(t- 1),"Togdheer_rain"]
    O<- max(L, 0.0372555495219628*M*N,na.rm=TRUE)
    if ( is.na(A) ){P<- O}
    else if(A>0){P<-sum( 100.219637895651*B*C , 0.0188842218738547*D*E , 0.00381626615949535*G*H , 5.10322194764338e-5*I*J , K,na.rm=TRUE) }
    else{P<- O }
    FIN <-P
      PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MS6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rivers.long[(t- 1),"Juba_River_discharge"]
    B<- future.long[(t- 1),"Bakool_FutureRegion"]
    C<- median(rain.long[(t-7):(t- 1),"Jubbada_Dhexe_rain"], na.rm=TRUE)
    D<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    E<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    G<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    H<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    I<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    J<- current.long[(t- 1),"Bay_CurrentRegion"]
    K<- rivers.long[(t- 1),"Juba_River_discharge"]
    L<- tail(movavg(conflicts.long[(t-6):(t- 1),"Woqooyi_Galbeed_Conflict"], 5,type="m"),1)
    M<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    N<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    O<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    P<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    Q<- future.long[(t- 1),"Banadir_FutureRegion"]
    R<- max(A, 0.058112331058838*B*C,na.rm=TRUE)
    S<- K%% 0.480334728960944
    U<- sinh(L)
    V<- max(sum(0.00461214794353355*D*E , 5.31621951275364e-5*G*H , I*J*S,na.rm=TRUE), U,na.rm=TRUE)
    W<- min(sum(0.00461214794353355*M*N , 5.31621951275364e-5*O*P,na.rm=TRUE), Q,na.rm=TRUE)
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(W)){W <- 0 }
    FIN <-sum( R , V , -W,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MS7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Bakool_FutureRegion"]
    B<- median(rain.long[(t-8):(t- 1),"Jubbada_Dhexe_rain"], na.rm=TRUE)
    C<- rivers.long[(t- 3),"Juba_River_discharge"]
    D<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    E<- tail(movavg(future.long[(t-9):(t- 1),"Woqooyi_Galbeed_FutureRegion"], 8,type="m"),1)
    G<- future.long[(t- 1),"Bakool_FutureRegion"]
    H<- median(rain.long[(t-8):(t- 1),"Jubbada_Dhexe_rain"], na.rm=TRUE)
    I<- rivers.long[(t- 3),"Juba_River_discharge"]
    J<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    K<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    L<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    M<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    N<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    O<- tail(movavg(before.long[(t-5):(t- 1),"Bay_BeforeRegion"], 4,type="m"),1)
    P<- tan(sum(0.099606452013967*G*H , -I,na.rm=TRUE))
    Q<- abs(sum(0.099606452013967*A*B , -C , -D*E*P , -5.10342293312645e-5*J*K , -0.00392285674872792*L*M , -0.0182417866734006*N*O,na.rm=TRUE))
    FIN <-Q
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MS8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rain.long[(t- 1),"Awdal_rain"]
    B<- median(rain.long[(t-5):(t- 1),"Bakool_rain"], na.rm=TRUE)
    C<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    D<- before.long[(t- 1),"Bay_BeforeRegion"]
    E<- mean(rivers.long[(t-8):(t- 1),"Juba_River_discharge"], na.rm=TRUE)
    G<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    H<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    I<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    J<- tail(movavg(future.long[(t-3):(t- 1),"Jubbada_Hoose_FutureRegion"], 2,type="m"),1)
    K<- tail(movavg(future.long[(t-7):(t- 1),"Banadir_FutureRegion"],6,type="w"),1)
    L<- before.long[(t- 2),"Sanaag_BeforeRegion"]
    M<- water.long[(t- 1),"Nugaal_WaterDrumPrice"]
    N<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    O<- rain.long[(t- 1),"Awdal_rain"]
    P<- max(0.021103181511146*N, O,na.rm=TRUE)
    Q<- tan(M*P)
    R<- max(sum(5.20190883630903e-5*G*H , 0.00447234964270099*I*J , -K,na.rm=TRUE), L*Q,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( A*B , 0.021103181511146*C*D , E , R,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}


modelarrivals_MS9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Woqooyi_Galbeed_Conflict"]
    B<- fatalities.long[(t- 16),"Jubbada_Hoose_Fatalities"]
    C<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    D<- median(rivers.long[(t-8):(t- 1),"Shabelle_River_discharge"], na.rm=TRUE)
    E<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    G<- current.long[(t- 1),"Bay_CurrentRegion"]
    H<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    I<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    J<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    K<- rain.long[(t- 1),"Jubbada_Dhexe_rain"]
    L<- current.long[(t- 1),"Bay_CurrentRegion"]
    M<- median(rivers.long[(t-8):(t- 1),"Shabelle_River_discharge"], na.rm=TRUE)
    N<- median(future.long[(t-9):(t- 1),"Gedo_FutureRegion"], na.rm=TRUE)
    O<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    P<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    Q<- max(sum(A*B , C*D , 0.0170904314585955*E*G , 5.23272500008191e-5*H*I , J*K/L , M , N,na.rm=TRUE), 0.00390207857714544*O*P,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}



modelarrivals_MS10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    B<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    C<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    D<- before.long[(t- 1),"Bay_BeforeRegion"]
    E<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    G<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    H<- tail(movavg(future.long[(t-11):(t- 1),"Woqooyi_Galbeed_FutureRegion"], 10,type="m"),1)
    I<- mean(rain.long[(t-3):(t- 1),"Togdheer_rain"], na.rm=TRUE)
    J<- tail(movavg(goats.long[(t-7):(t- 1),"Bakool_goatprice"], 6,type="m"),1)
    K<- rivers.long[(t- 1),"Juba_River_discharge"]
    L<- tail(movavg(future.long[(t-3):(t- 1),"Bakool_FutureRegion"],2,type="w"),1)
    M<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    N<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    O<- tan(J)
    P<- max(sum(0.0183831042869513*C*D , E*G*H , I*O , K,na.rm=TRUE), L,na.rm=TRUE)
    Q<- max(sum(4.87564891625497e-5*A*B , P,na.rm=TRUE), 0.00390207862067642*M*N,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}


modelarrivals_MSJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    B<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    C<- tail(movavg(rivers.long[(t-16):(t- 1),"Juba_River_discharge"],15,type="w"),1)
    D<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    E<- rain.long[(t- 1),"Awdal_rain"]
    G<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    H<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    I<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    J<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    K<- before.long[(t- 1),"Sool_BeforeRegion"]
    L<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    M<- rain.long[(t- 1),"Awdal_rain"]
    N<- before.long[(t- 1),"Gedo_BeforeRegion"]
    O<- future.long[(t- 1),"Awdal_FutureRegion"]
    if ( is.na(G) ){P<- J}
    else if(G>0){P<- 0.00380093983798491*H*I }
    else{P<- J }
    Q<- max(0.00650433048618283*D*E, P,na.rm=TRUE)
    R<- sin(0.00650433048618283*L*M)
    S<- sinh(R)
    U<- max(Q, K*S,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 110.722673724422*A*B , C , U , -0.0115238943469961*N*O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}
modelarrivals_MSJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(rivers.long[(t-17):(t- 1),"Juba_River_discharge"],16,type="w"),1)
    B<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    C<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    D<- rain.long[(t- 1),"Awdal_rain"]
    E<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    G<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    H<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    I<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    J<- water.long[(t- 1),"Hiiraan_WaterDrumPrice"]
    K<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    L<- before.long[(t- 1),"Galgaduud_BeforeRegion"]
    M<- rain.long[(t- 1),"Banaadir_rain"]
    N<- max(sum(108.97964367823*B*C , 0.0064440320615573*D*E , 0.00365808078514922*G*H,na.rm=TRUE), 1.75960906416385e-5*I*J,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( A , N , -0.00139328112034529*K*L*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MSJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(rivers.long[(t-16):(t- 1),"Juba_River_discharge"],15,type="w"),1)
    B<- conflicts.long[(t- 6),"Jubbada_Hoose_Conflict"]
    C<- rain.long[(t- 13),"Bakool_rain"]
    D<- conflicts.long[(t- 6),"Nugaal_Conflict"]
    E<- rain.long[(t- 13),"Awdal_rain"]
    G<- conflicts.long[(t- 2),"Nugaal_Conflict"]
    H<- conflicts.long[(t- 6),"Nugaal_Conflict"]
    I<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    J<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    K<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    L<- current.long[(t- 11),"Sanaag_CurrentRegion"]
    M<- future.long[(t- 1),"Bakool_FutureRegion"]
    N<- before.long[(t- 1),"Sool_BeforeRegion"]
    O<- and(G, H)
    if ( is.na(O) ){P<- M}
    else if(O>0){P<-sum( 0.00390190332749639*I*J , 0.0043156594773486*K*L,na.rm=TRUE) }
    else{P<- M }
    Q<- max(6.96486805823005*D*E, P,na.rm=TRUE)
    R<- max(B*C, Q,na.rm=TRUE)
    S<- max(A, R,na.rm=TRUE)
    U<- max(S, 0.161319196205402*N,na.rm=TRUE)
    FIN <-U
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MSJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(rivers.long[(t-6):(t- 1),"Juba_River_discharge"], na.rm=TRUE)
    B<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    C<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    D<- rain.long[(t- 1),"Awdal_rain"]
    E<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    G<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    H<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    I<- future.long[(t- 5),"Sool_FutureRegion"]
    J<- conflicts.long[(t- 9),"Nugaal_Conflict"]
    K<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    L<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    M<- water.long[(t- 1),"Hiiraan_WaterDrumPrice"]
    N<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    O<- max(sum(109.640754332463*B*C , 0.00692772472011925*D*E , 0.00366502932986568*G*H , I*J , -K,na.rm=TRUE), 1.75958771940194e-5*L*M,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( A , O , -109.640754332463*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MSJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- mean(rivers.long[(t-8):(t- 1),"Juba_River_discharge"], na.rm=TRUE)
    B<- rain.long[(t- 1),"Hiiraan_rain"]
    C<- rain.long[(t- 17),"Sanaag_rain"]
    D<- fatalities.long[(t- 3),"Woqooyi_Galbeed_Fatalities"]
    E<- fatalities.long[(t- 11),"Woqooyi_Galbeed_Fatalities"]
    G<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    H<- rain.long[(t- 1),"Awdal_rain"]
    I<- current.long[(t- 1),"Sool_CurrentRegion"]
    J<- stations.long[(t- 1),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
    K<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    L<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    M<- stations.long[(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"]
    N<- mean(future.long[(t-5):(t- 1),"Bakool_FutureRegion"], na.rm=TRUE)
    O<- exp(sum(D , E,na.rm=TRUE))
    P<- max(O, 0.0064636911128383*G*H,na.rm=TRUE)
    Q<- max(B*C, P,na.rm=TRUE)
    R<- tan(J)
    S<- max(I*R, 0.00443227940851751*K*L,na.rm=TRUE)
    U<- max(Q,sum( S , -M*N,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(U)){U <- 0 }
    FIN <-sum( A , U,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MSJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(future.long[(t-6):(t- 1),"Mudug_FutureRegion"], 5,type="m"),1)
    B<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    C<- tail(movavg(current.long[(t-7):(t- 1),"Nugaal_CurrentRegion"],6,type="w"),1)
    D<- current.long[(t- 1),"Sool_CurrentRegion"]
    E<- tail(movavg(current.long[(t-3):(t- 1),"Nugaal_CurrentRegion"], 2,type="m"),1)
    G<- before.long[(t- 1),"Bakool_BeforeRegion"]
    H<- future.long[(t- 4),"Togdheer_FutureRegion"]
    I<- tail(movavg(conflicts.long[(t-4):(t- 1),"Bari_Conflict"], 3,type="m"),1)
    J<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    K<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    L<- mean(fatalities.long[(t-5):(t- 1),"Bari_Fatalities"], na.rm=TRUE)
    M<- median(conflicts.long[(t-4):(t- 1),"Jubbada_Dhexe_Conflict"], na.rm=TRUE)
    N<- median(current.long[(t-4):(t- 1),"Togdheer_CurrentRegion"], na.rm=TRUE)
    O<- mean(rivers.long[(t-14):(t- 1),"Juba_River_discharge"], na.rm=TRUE)
    if ( is.na(A) || is.na( B)){P<-0}
    else if(A< B){P<-1 }
    else{P<-0 }
    if ( is.na(C) || is.na( D)){Q<-0}
    else if(C<= D){Q<-1 }
    else{Q<-0 }
    R<- max(sum(0.407261325661478*G , 2.45691950875206*H*I,na.rm=TRUE), 0.00388058064377794*J*K,na.rm=TRUE)
    S<- max(sum(E , R,na.rm=TRUE), L*M*N,na.rm=TRUE)
    U<- max(P*Q*S, O,na.rm=TRUE)
    FIN <-U
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MSJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    B<- fatalities.long[(t- 11),"Jubbada_Hoose_Fatalities"]
    C<- stations.long[(t- 6),"Gedo_LuuqStation_Juba_River"]
    D<- current.long[(t- 11),"Sanaag_CurrentRegion"]
    E<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    G<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    H<- current.long[(t- 1),"Bay_CurrentRegion"]
    I<- fatalities.long[(t- 5),"Togdheer_Fatalities"]
    J<- fatalities.long[(t- 1),"Jubbada_Hoose_Fatalities"]
    K<- median(before.long[(t-11):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    L<- future.long[(t- 1),"Togdheer_FutureRegion"]
    M<- rain.long[(t- 1),"Gedo_rain"]
    N<- rain.long[(t- 1),"Gedo_rain"]
    O<- current.long[(t- 11),"Sanaag_CurrentRegion"]
    P<- tail(movavg(future.long[(t-4):(t- 1),"Bakool_FutureRegion"], 3,type="m"),1)
    Q<- current.long[(t- 1),"Gedo_CurrentRegion"]
    if ( is.na(I) ){R<- K}
    else if(I>0){R<- J }
    else{R<- K }
    S<- max(sum(L*M , -N*O,na.rm=TRUE),sum( P , -Q,na.rm=TRUE),na.rm=TRUE)
    U<- max(C*D,sum( 0.00368442290178933*E*G , 0.00166209039907475*H*R , S,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(U)){U <- 0 }
    FIN <-sum( A*B , U,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MSJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    B<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    C<- conflicts.long[(t- 1),"Woqooyi_Galbeed_Conflict"]
    D<- rain.long[(t- 1),"Bay_rain"]
    E<- tail(movavg(conflicts.long[(t-5):(t- 1),"Jubbada_Dhexe_Conflict"], 4,type="m"),1)
    G<- tail(movavg(future.long[(t-3):(t- 1),"Bakool_FutureRegion"],2,type="w"),1)
    H<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    I<- rain.long[(t- 1),"Awdal_rain"]
    J<- rivers.long[(t- 1),"Juba_River_discharge"]
    K<- fatalities.long[(t- 11),"Togdheer_Fatalities"]
    L<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    M<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    N<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    O<- stations.long[(t- 1),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
    if ( is.na(K) ){P<- N}
    else if(K>0){P<- 0.00387414576128934*L*M }
    else{P<- N }
    Q<- max(sum(0.00595790939347138*H*I , J,na.rm=TRUE), P,na.rm=TRUE)
    R<- max(G, Q,na.rm=TRUE)
    S<- exp(O)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(S)){S <- 0 }
    FIN <-sum( 0.000351691881729044*A*B , C*D*E , R , -S,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MSJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- mean(rivers.long[(t-9):(t- 1),"Shabelle_River_discharge"], na.rm=TRUE)
    B<- rain.long[(t- 1),"Hiiraan_rain"]
    C<- rain.long[(t- 1),"Banaadir_rain"]
    D<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    E<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    G<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    H<- future.long[(t- 10),"Shabeallaha_Dhexe_FutureRegion"]
    I<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    J<- rain.long[(t- 1),"Awdal_rain"]
    K<- water.long[(t- 1),"Hiiraan_WaterDrumPrice"]
    L<- mean(before.long[(t-3):(t- 1),"Sanaag_BeforeRegion"], na.rm=TRUE)
    M<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    N<- tail(movavg(future.long[(t-9):(t- 1),"Gedo_FutureRegion"],8,type="w"),1)
    O<- tail(movavg(future.long[(t-4):(t- 1),"Gedo_FutureRegion"],3,type="w"),1)
    if ( is.na(B) || is.na( C)){P<-0}
    else if(B>= C){P<-1 }
    else{P<-0 }
    Q<- max(D, 0.0037568215268581*E*G,na.rm=TRUE)
    if ( is.na(P) ){R<- H}
    else if(P>0){R<- Q }
    else{R<- H }
    S<- max(0.00663834123467052*I*J, 1.681437906099e-5*K*L,na.rm=TRUE)
    U<- tan(O)
    V<- max(S, M*N*U,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(V)){V <- 0 }
    FIN <-sum( A , R , V,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MSJUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Sool_Fatalities"]
    B<- rain.long[(t- 6),"Mudug_rain"]
    C<- median(rain.long[(t-9):(t- 1),"Jubbada_Dhexe_rain"], na.rm=TRUE)
    D<- conflicts.long[(t- 1),"Sanaag_Conflict"]
    E<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    G<- rain.long[(t- 2),"Togdheer_rain"]
    H<- conflicts.long[(t- 1),"Sanaag_Conflict"]
    I<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    J<- fatalities.long[(t- 1),"Sool_Fatalities"]
    K<- mean(conflicts.long[(t-3):(t- 1),"Banadir_Conflict"], na.rm=TRUE)
    L<- median(conflicts.long[(t-9):(t- 1),"Nugaal_Conflict"], na.rm=TRUE)
    M<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    N<- conflicts.long[(t- 1),"Sanaag_Conflict"]
    O<- current.long[(t- 1),"Sool_CurrentRegion"]
    P<- current.long[(t- 1),"Sool_CurrentRegion"]
    Q<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    R<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    S<- tail(movavg(before.long[(t-15):(t- 1),"Jubbada_Dhexe_BeforeRegion"], 14,type="m"),1)
    U<- rain.long[(t- 6),"Mudug_rain"]
    V<- median(rain.long[(t-9):(t- 1),"Jubbada_Dhexe_rain"], na.rm=TRUE)
    W<- conflicts.long[(t- 1),"Sanaag_Conflict"]
    X<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    Y<- rain.long[(t- 2),"Togdheer_rain"]
    if ( is.na(M) ){Z<- P}
    else if(M>0){Z<- 0.19796444407516*N*O }
    else{Z<- P }
    AA<- max(sum(B*C , D^2*E*G,na.rm=TRUE),sum( H^2*I , J*K*L , Z,na.rm=TRUE),na.rm=TRUE)
    BB<- max(AA, 0.00390207861372976*Q*R,na.rm=TRUE)
    CC<- max(S,sum( U*V , W^2*X*Y,na.rm=TRUE),na.rm=TRUE)
    if ( is.na(A) ){DD<- CC}
    else if(A>0){DD<- BB }
    else{DD<- CC }
    FIN <- DD
      PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}


