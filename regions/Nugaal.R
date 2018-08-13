
modelarrivals_NUminus1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(stations.long[(t-9):(t- 1),"Juba_Dhexe_BualleStation_Juba_River"], 8,type="m"),1)
    B<- median(rain.long[(t-6):(t- 1),"Mudug_rain"], na.rm=TRUE)
    C<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    D<- median(rain.long[(t-8):(t- 1),"Gedo_rain"], na.rm=TRUE)
    E<- before.long[(t- 1),"Gedo_BeforeRegion"]
    G<- current.long[(t- 1),"Gedo_CurrentRegion"]
    H<- tail(movavg(current.long[(t-4):(t- 1),"Hiiraan_CurrentRegion"], 3,type="m"),1)
    I<- median(rain.long[(t-14):(t- 1),"Sool_rain"], na.rm=TRUE)
    J<- mean(conflicts.long[(t-7):(t- 1),"Sanaag_Conflict"], na.rm=TRUE)
    K<- median(goats.long[(t-15):(t- 1),"Bakool_goatprice"], na.rm=TRUE)
    L<- tanh(sum(551.262507227251*J , -550.565549645753,na.rm=TRUE))
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 492.382288709146 , A*B , 0.0180921733140101*C*D , 9.1082331636846e-9*E*G*H*I*L , -0.000408890970907661*K,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NUminus2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rain.long[(t- 4),"Togdheer_rain"]
    B<- future.long[(t- 2),"Bari_FutureRegion"]
    C<- future.long[(t- 2),"Bari_FutureRegion"]
    D<- water.long[(t- 2),"Shabeallaha_Dhexe_WaterDrumPrice"]
    E<- current.long[(t- 2),"Sanaag_CurrentRegion"]
    G<- goats.long[(t- 2),"Shabeellaha_Hoose_goatprice"]
    H<- rain.long[(t- 2),"Mudug_rain"]
    I<- rain.long[(t- 2),"Mudug_rain"]
    J<- fatalities.long[(t- 6),"Bakool_Fatalities"]
    K<- fatalities.long[(t- 2),"Mudug_Fatalities"]
    L<- rain.long[(t- 2),"Mudug_rain"]
    M<- goats.long[(t- 2),"Jubbada_Hoose_goatprice"]
    N<- tan(0.655106270079457*C)
    if ( is.na(A) ){O<- N}
    else if(A>0){O<- 0.655106270079457*B }
    else{O<- N }
    P<- cos(G)
    Q<- atanh(P)
    if ( is.na(H) ){R<- K}
    else if(H>0){R<-sum( I , -J,na.rm=TRUE) }
    else{R<- K }
    S<- floor(sum(0.0741513252347919*D , 0.0373830497904441*E , Q*R , L,na.rm=TRUE))
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( O , S , -562.775412175752 , -0.000348707572476734*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NU1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Gedo_CurrentRegion"]
    B<- tail(movavg(before.long[(t-13):(t- 1),"Shabeellaha_Hoose_BeforeRegion"], 12,type="m"),1)
    C<- rivers.long[(t- 2),"Shabelle_River_discharge"]
    D<- rain.long[(t- 16),"Sanaag_rain"]
    E<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    G<- current.long[(t- 1),"Gedo_CurrentRegion"]
    H<- mean(rain.long[(t-4):(t- 1),"Gedo_rain"], na.rm=TRUE)
    I<- mean(future.long[(t-5):(t- 1),"Hiiraan_FutureRegion"], na.rm=TRUE)
    J<- mean(future.long[(t-7):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    K<- max(0.118114148396685*E, 0.00303228631515486*G*H,na.rm=TRUE)
    L<- max(sum(C , D,na.rm=TRUE),sum( K , -2.78283702899786*I,na.rm=TRUE),na.rm=TRUE)
    M<- max(355.590952630651, L,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(J)){J <- 0 }
    FIN <-sum( 2.00282968317969e-6*A*B , 1.15604031986002*M , -118.094845944183 , -2.43079385489757*J,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NU2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- water.long[(t- 1),"Shabeallaha_Dhexe_WaterDrumPrice"]
    B<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    C<- tail(movavg(current.long[(t-6):(t- 1),"Awdal_CurrentRegion"], 5,type="m"),1)
    D<- median(future.long[(t-4):(t- 1),"Bari_FutureRegion"], na.rm=TRUE)
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- before.long[(t- 1),"Shabeellaha_Hoose_BeforeRegion"]
    H<- tail(movavg(conflicts.long[(t-17):(t- 1),"Sanaag_Conflict"],16,type="w"),1)
    I<- mean(before.long[(t-15):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    J<- rain.long[(t- 1),"Awdal_rain"]
    K<- before.long[(t- 1),"Banadir_BeforeRegion"]
    L<- tail(movavg(before.long[(t-11):(t- 1),"Awdal_BeforeRegion"], 10,type="m"),1)
    M<- tan(I)
    N<- max(D,sum( 6.03231368184112e-5*E*G , H*M,na.rm=TRUE),na.rm=TRUE)
    O<- 0.0496761433683569*K%% L
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 0.0511521622019474*A , 0.000144022236826227*B*C , N , -531.944935169207 , -J , -O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NU3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Gedo_BeforeRegion"]
    B<- current.long[(t- 1),"Gedo_CurrentRegion"]
    C<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    D<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    E<- tail(movavg(future.long[(t-13):(t- 1),"Hiiraan_FutureRegion"],12,type="w"),1)
    G<- current.long[(t- 1),"Bari_CurrentRegion"]
    H<- future.long[(t- 1),"Togdheer_FutureRegion"]
    I<- before.long[(t- 1),"Gedo_BeforeRegion"]
    J<- mean(before.long[(t-16):(t- 1),"Jubbada_Hoose_BeforeRegion"], na.rm=TRUE)
    if ( is.na(E) || is.na( G)){K<-0}
    else if(E<= G){K<-1 }
    else{K<-0 }
    L<- tan(sum(0.0021004332058694*D , K,na.rm=TRUE))
    M<- max(I, J,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 220.336660928209 , 1.83360583500899e-5*A*B , 8.0166248782205e-6*C^2 , 22.2259237607515*L , -0.319209747394912*H , -0.0566337849195279*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NU4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    C<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    D<- before.long[(t- 1),"Bay_BeforeRegion"]
    E<- before.long[(t- 1),"Gedo_BeforeRegion"]
    G<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    H<- current.long[(t- 1),"Bari_CurrentRegion"]
    I<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    J<- before.long[(t- 1),"Bay_BeforeRegion"]
    K<- before.long[(t- 1),"Gedo_BeforeRegion"]
    L<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    M<- current.long[(t- 1),"Bari_CurrentRegion"]
    N<- mean(goats.long[(t-4):(t- 1),"Gedo_goatprice"], na.rm=TRUE)
    if ( is.na(608) || is.na( H)){O<-0}
    else if(608< H){O<-1 }
    else{O<-0 }
    if ( is.na(608) || is.na( M)){P<-0}
    else if(608< M){P<-1 }
    else{P<-0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 529.620675029411 , 9.00613965631703e-8*A*B , 0.00564725125358525*C^2*D , 5.09575779007251e-5*E*G*O , 9.00613965631703e-8*I*J*K*L*P , -0.000400998631124698*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NU5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 15),"Galgaduud_Conflict"]
    B<- median(rain.long[(t-12):(t- 1),"Bay_rain"], na.rm=TRUE)
    C<- tail(movavg(current.long[(t-17):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], 16,type="m"),1)
    D<- tail(movavg(current.long[(t-4):(t- 1),"Awdal_CurrentRegion"],3,type="w"),1)
    E<- rain.long[(t- 1),"Gedo_rain"]
    G<- tail(movavg(fatalities.long[(t-9):(t- 1),"Sanaag_Fatalities"], 8,type="m"),1)
    H<- tail(movavg(current.long[(t-7):(t- 1),"Bari_CurrentRegion"], 6,type="m"),1)
    I<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    J<- current.long[(t- 2),"Togdheer_CurrentRegion"]
    K<- future.long[(t- 1),"Sanaag_FutureRegion"]
    L<- current.long[(t- 1),"Gedo_CurrentRegion"]
    M<- current.long[(t- 5),"Gedo_CurrentRegion"]
    N<- fatalities.long[(t- 1),"Bakool_Fatalities"]
    O<- and(A, B)
    P<- min(H, 556.16209341364*I,na.rm=TRUE)
    Q<- min(D,sum( E*G , P,na.rm=TRUE),na.rm=TRUE)
    R<- max(sum(0.0757448305598919*C , Q,na.rm=TRUE), 205.884615649741,na.rm=TRUE)
    if ( is.na(O) ){S<- J}
    else if(O>0){S<- R }
    else{S<- J }
    U<- N%% 2.20979887177497e-5
    V<- max(sum(S , -K,na.rm=TRUE), L*M*U,na.rm=TRUE)
    FIN <-V
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NU6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(rain.long[(t-14):(t- 1),"Sool_rain"], na.rm=TRUE)
    B<- median(rain.long[(t-7):(t- 1),"Bay_rain"], na.rm=TRUE)
    C<- median(rain.long[(t-12):(t- 1),"Bay_rain"], na.rm=TRUE)
    D<- conflicts.long[(t- 1),"Sanaag_Conflict"]
    E<- before.long[(t- 1),"Gedo_BeforeRegion"]
    G<- median(rain.long[(t-5):(t- 1),"Shabeellaha_Dhexe_rain"], na.rm=TRUE)
    H<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    I<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    J<- before.long[(t- 1),"Gedo_BeforeRegion"]
    K<- median(conflicts.long[(t-8):(t- 1),"Jubbada_Hoose_Conflict"], na.rm=TRUE)
    L<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    M<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    N<- median(rivers.long[(t-15):(t- 1),"Shabelle_River_discharge"], na.rm=TRUE)
    O<- mean(current.long[(t-6):(t- 1),"Togdheer_CurrentRegion"], na.rm=TRUE)
    P<- future.long[(t- 1),"Togdheer_FutureRegion"]
    Q<- median(rain.long[(t-14):(t- 1),"Sool_rain"], na.rm=TRUE)
    if ( is.na(C) || is.na( D)){R<-0}
    else if(C> D){R<-1 }
    else{R<-0 }
    S<- I%% 0.134083238014528
    U<- round(H*S)
    V<- min(G, U,na.rm=TRUE)
    W<- min(V,sum( 0.068708854076504*J , -K,na.rm=TRUE),na.rm=TRUE)
    X<- M%% 0.134083238014528
    Y<- round(L*X)
    if ( is.na(R) ){Z<- N}
    else if(R>0){Z<-sum( 215.673655443857 , 0.068708854076504*E*W , Y,na.rm=TRUE) }
    else{Z<- N }
    if ( is.na(B) ){AA<- O}
    else if(B>0){AA<- Z }
    else{AA<- O }
    if ( is.na(A) ){BB<- 195.384616007415}
    else if(A>0){BB<- AA }
    else{BB<- 195.384616007415 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( BB , -0.134083238014528*P*Q,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NU7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Gedo_CurrentRegion"]
    B<- tail(movavg(before.long[(t-11):(t- 1),"Gedo_BeforeRegion"], 10,type="m"),1)
    C<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    D<- rain.long[(t- 1),"Bakool_rain"]
    E<- median(rain.long[(t-13):(t- 1),"Nugaal_rain"], na.rm=TRUE)
    G<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    H<- future.long[(t- 1),"Togdheer_FutureRegion"]
    I<- current.long[(t- 4),"Jubbada_Hoose_CurrentRegion"]
    J<- rain.long[(t- 1),"Bakool_rain"]
    K<- max(210,sum( 0.157506325915842*C , -410.565784590184,na.rm=TRUE),na.rm=TRUE)
    L<- factorial(E)
    M<- tan(-1.84141864280886*G)
    N<- tan(I)
    O<- max(sum(2.00875350244057e-5*A*B , K , -D , -L , -M , -0.461156491500489*H , -1.97931263561548*N,na.rm=TRUE), J,na.rm=TRUE)
    FIN <-O
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NU8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Banadir_CurrentRegion"]
    B<- before.long[(t- 1),"Gedo_BeforeRegion"]
    C<- current.long[(t- 1),"Gedo_CurrentRegion"]
    D<- before.long[(t- 1),"Shabeellaha_Hoose_BeforeRegion"]
    E<- water.long[(t- 1),"Nugaal_WaterDrumPrice"]
    G<- median(fatalities.long[(t-13):(t- 1),"Bay_Fatalities"], na.rm=TRUE)
    H<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    I<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    J<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    K<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    L<- mean(future.long[(t-8):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    M<- tan(0.118522172544475*A)
    if ( is.na(D) || is.na( E)){N<-0}
    else if(D>= E){N<-1 }
    else{N<-0 }
    if ( is.na(G) ){O<- J*K}
    else if(G>0){O<- 0.0491956295061396*H*I }
    else{O<- J*K }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 297.085390188805 , 1.95868807008756*M , 1.33427680933494e-5*B*C*N , O , -2.33438018387101*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NU9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    B<- current.long[(t- 1),"Gedo_CurrentRegion"]
    C<- before.long[(t- 1),"Gedo_BeforeRegion"]
    D<- future.long[(t- 1),"Bari_FutureRegion"]
    E<- current.long[(t- 1),"Gedo_CurrentRegion"]
    G<- current.long[(t- 1),"Gedo_CurrentRegion"]
    H<- mean(fatalities.long[(t-14):(t- 1),"Mudug_Fatalities"], na.rm=TRUE)
    I<- median(stations.long[(t-12):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    J<- future.long[(t- 1),"Togdheer_FutureRegion"]
    K<- mean(goats.long[(t-6):(t- 1),"Bakool_goatprice"], na.rm=TRUE)
    L<- max(C, D,na.rm=TRUE)
    M<- L%% E
    N<- (sum(0.00225724636012425*G , H,na.rm=TRUE))
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 373.9425352952 , 7.72734500582504e-6*A^2 , 1.34089484168125e-5*B*M , 0.000251462169043288*N^I , -0.325754679986141*J , -0.000237354841402635*K,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NU10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(rain.long[(t-10):(t- 1),"Bay_rain"], na.rm=TRUE)
    B<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    C<- rain.long[(t- 9),"Shabeellaha_Hoose_rain"]
    D<- tail(movavg(rain.long[(t-4):(t- 1),"Mudug_rain"], 3,type="m"),1)
    E<- current.long[(t- 1),"Gedo_CurrentRegion"]
    G<- tail(movavg(before.long[(t-5):(t- 1),"Gedo_BeforeRegion"], 4,type="m"),1)
    H<- median(stations.long[(t-4):(t- 1),"Juba_Dhexe_BualleStation_Juba_River"], na.rm=TRUE)
    I<- tail(movavg(rain.long[(t-4):(t- 1),"Mudug_rain"], 3,type="m"),1)
    J<- median(future.long[(t-4):(t- 1),"Bari_FutureRegion"], na.rm=TRUE)
    K<- median(stations.long[(t-4):(t- 1),"Juba_Dhexe_BualleStation_Juba_River"], na.rm=TRUE)
    L<- median(rain.long[(t-10):(t- 1),"Bay_rain"], na.rm=TRUE)
    M<- median(rivers.long[(t-14):(t- 1),"Shabelle_River_discharge"], na.rm=TRUE)
    N<- future.long[(t- 1),"Togdheer_FutureRegion"]
    O<- max(9.06884799103493e-6*B^2, 196.300541228833,na.rm=TRUE)
    P<- cos(0.119923111892436*D)
    Q<- cosh(H)
    R<- cos(0.119923111892436*I)
    S<- cosh(K)
    if ( is.na(P) ){U<- S}
    else if(P>0){U<-sum( 9.07935400677723e-7*E*G*Q*R , J,na.rm=TRUE) }
    else{U<- S }
    if ( is.na(C) ){V<- L}
    else if(C>0){V<- U }
    else{V<- L }
    if ( is.na(A) ){W<- M}
    else if(A>0){W<-sum( O , V,na.rm=TRUE) }
    else{W<- M }
    if(is.infinite(W)){W <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( W , -0.454435103957968*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NUJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(stations.long[(t-5):(t- 1),"Gedo_DollowStation_Juba_River"],4,type="w"),1)
    B<- median(before.long[(t-6):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    C<- before.long[(t- 1),"Bay_BeforeRegion"]
    D<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    E<- fatalities.long[(t- 7),"Hiiraan_Fatalities"]
    G<- before.long[(t- 1),"Bay_BeforeRegion"]
    H<- future.long[(t- 1),"Mudug_FutureRegion"]
    I<- current.long[(t- 1),"Gedo_CurrentRegion"]
    J<- future.long[(t- 1),"Mudug_FutureRegion"]
    K<- rain.long[(t- 1),"Awdal_rain"]
    L<- tail(movavg(stations.long[(t-5):(t- 1),"Gedo_DollowStation_Juba_River"],4,type="w"),1)
    M<- future.long[(t- 1),"Sanaag_FutureRegion"]
    N<- tail(movavg(conflicts.long[(t-3):(t- 1),"Woqooyi_Galbeed_Conflict"],2,type="w"),1)
    O<- min(0.0161602501936121, D,na.rm=TRUE)
    P<- max(0.0119060625221367*G,sum( 0.0009017864570462*H*I , -J , -K , -L,na.rm=TRUE),na.rm=TRUE)
    Q<- max(A*B,sum( C*O , E , P,na.rm=TRUE),na.rm=TRUE)
    R<- max(197.773464264659, Q,na.rm=TRUE)
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( R , -0.0859806709840279*M*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NUJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(rain.long[(t-12):(t- 1),"Bay_rain"], na.rm=TRUE)
    B<- conflicts.long[(t- 11),"Woqooyi_Galbeed_Conflict"]
    C<- tail(movavg(before.long[(t-6):(t- 1),"Woqooyi_Galbeed_BeforeRegion"],5,type="w"),1)
    D<- median(before.long[(t-6):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    E<- median(rain.long[(t-12):(t- 1),"Bay_rain"], na.rm=TRUE)
    G<- before.long[(t- 1),"Bay_BeforeRegion"]
    H<- stations.long[(t- 1),"Gedo_LuuqStation_Juba_River"]
    I<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    J<- future.long[(t- 1),"Mudug_FutureRegion"]
    K<- current.long[(t- 8),"Woqooyi_Galbeed_CurrentRegion"]
    L<- median(future.long[(t-5):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    M<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    N<- current.long[(t- 8),"Woqooyi_Galbeed_CurrentRegion"]
    O<- future.long[(t- 1),"Bari_FutureRegion"]
    P<- max(sum(D , E,na.rm=TRUE),sum( 0.0115672519643868*G*H , 0.000622584491123877*I*J,na.rm=TRUE),na.rm=TRUE)
    Q<- max(C, P,na.rm=TRUE)
    R<- max(204, Q,na.rm=TRUE)
    if ( is.na(B) ){S<- K}
    else if(B>0){S<- R }
    else{S<- K }
    U<- min(M, N,na.rm=TRUE)
    if ( is.na(A) ){V<- U}
    else if(A>0){V<-sum( 1.18263714263502*S , -L,na.rm=TRUE) }
    else{V<- U }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( V , -0.274820253306489*O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NUJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    B<- future.long[(t- 1),"Mudug_FutureRegion"]
    C<- before.long[(t- 1),"Bay_BeforeRegion"]
    D<- stations.long[(t- 1),"Gedo_LuuqStation_Juba_River"]
    E<- median(current.long[(t-17):(t- 1),"Bari_CurrentRegion"], na.rm=TRUE)
    G<- rain.long[(t- 1),"Gedo_rain"]
    H<- goats.long[(t- 1),"Mudug_goatprice"]
    I<- future.long[(t- 1),"Bari_FutureRegion"]
    J<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    K<- ceil(1.64968758763484*G)
    L<- max(sum(E , K,na.rm=TRUE), 434.861884914258,na.rm=TRUE)
    M<- max(sum(234.67179355681 , 0.0130864735936891*C*D,na.rm=TRUE), L,na.rm=TRUE)
    N<- max(0.000796057948873277*A*B, M,na.rm=TRUE)
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    FIN <-sum( N , -0.00022879138752455*H , -0.00722391371679523*I*J,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NUJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 8),"Woqooyi_Galbeed_BeforeRegion"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    C<- stations.long[(t- 1),"Gedo_LuuqStation_Juba_River"]
    D<- before.long[(t- 1),"Bay_BeforeRegion"]
    E<- median(rivers.long[(t-11):(t- 1),"Juba_River_discharge"], na.rm=TRUE)
    G<- tail(movavg(current.long[(t-9):(t- 1),"Mudug_CurrentRegion"],8,type="w"),1)
    H<- stations.long[(t- 2),"Hiiraan_Bulo_Burti_StationShabelle_River"]
    I<- mean(rain.long[(t-4):(t- 1),"Gedo_rain"], na.rm=TRUE)
    J<- future.long[(t- 1),"Mudug_FutureRegion"]
    K<- current.long[(t- 1),"Gedo_CurrentRegion"]
    L<- min(271, 0.0126218142414591*G^2,na.rm=TRUE)
    M<- min(E, L,na.rm=TRUE)
    N<- max(0.0126218142414591*D, M,na.rm=TRUE)
    O<- max(0.0126218142414591*B*C, N,na.rm=TRUE)
    P<- max(O, H*I,na.rm=TRUE)
    Q<- max(A, P,na.rm=TRUE)
    R<- floor(0.000727674420108044*K)
    S<- max(Q, J*R,na.rm=TRUE)
    if(is.infinite(S)){S <- 0 }
    FIN <-sum( 1.11901288487613*S , -107.867863633615,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NUJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 8),"Woqooyi_Galbeed_BeforeRegion"]
    B<- median(before.long[(t-13):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    C<- mean(fatalities.long[(t-17):(t- 1),"Shabeellaha_Hoose_Fatalities"], na.rm=TRUE)
    D<- stations.long[(t- 1),"Gedo_LuuqStation_Juba_River"]
    E<- tail(movavg(before.long[(t-3):(t- 1),"Bay_BeforeRegion"], 2,type="m"),1)
    G<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    H<- future.long[(t- 1),"Mudug_FutureRegion"]
    I<- current.long[(t- 2),"Awdal_CurrentRegion"]
    J<- before.long[(t- 13),"Togdheer_BeforeRegion"]
    K<- tail(movavg(future.long[(t-4):(t- 1),"Bari_FutureRegion"], 3,type="m"),1)
    L<- mean(fatalities.long[(t-16):(t- 1),"Jubbada_Hoose_Fatalities"], na.rm=TRUE)
    M<- min(I, J,na.rm=TRUE)
    N<- max(sum(4.41819687868338*C , 0.0110775823974681*D*E,na.rm=TRUE),sum( 0.000672039038012776*G*H , M,na.rm=TRUE),na.rm=TRUE)
    O<- max(A,sum( B , N,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( O , -0.254307119591942*K , -1.68425787870773*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NUJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 8),"Woqooyi_Galbeed_BeforeRegion"]
    B<- tail(movavg(rain.long[(t-8):(t- 1),"Gedo_rain"],7,type="w"),1)
    C<- median(stations.long[(t-5):(t- 1),"Juba_Dhexe_BualleStation_Juba_River"], na.rm=TRUE)
    D<- tail(movavg(before.long[(t-6):(t- 1),"Woqooyi_Galbeed_BeforeRegion"],5,type="w"),1)
    E<- before.long[(t- 1),"Bay_BeforeRegion"]
    G<- stations.long[(t- 1),"Gedo_LuuqStation_Juba_River"]
    H<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    I<- future.long[(t- 1),"Mudug_FutureRegion"]
    J<- median(stations.long[(t-5):(t- 1),"Juba_Dhexe_BualleStation_Juba_River"], na.rm=TRUE)
    K<- future.long[(t- 1),"Bari_FutureRegion"]
    L<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    M<- max(sum(B*C , D,na.rm=TRUE),sum( 0.0121312238778077*E*G , 0.000634040603659448*H*I , J,na.rm=TRUE),na.rm=TRUE)
    N<- max(257.164292871804, M,na.rm=TRUE)
    O<- max(A, N,na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 1.18051756161908*O , -108.202360653745 , -0.00888697711946154*K*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NUJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- mean(rain.long[(t-5):(t- 1),"Gedo_rain"], na.rm=TRUE)
    B<- median(stations.long[(t-6):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    C<- median(before.long[(t-5):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    D<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    E<- future.long[(t- 1),"Mudug_FutureRegion"]
    G<- before.long[(t- 8),"Woqooyi_Galbeed_BeforeRegion"]
    H<- before.long[(t- 1),"Bay_BeforeRegion"]
    I<- stations.long[(t- 1),"Gedo_LuuqStation_Juba_River"]
    J<- tail(movavg(future.long[(t-10):(t- 1),"Woqooyi_Galbeed_FutureRegion"],9,type="w"),1)
    K<- future.long[(t- 1),"Bari_FutureRegion"]
    L<- max(G, 0.0131153680150159*H*I,na.rm=TRUE)
    M<- max(216, L,na.rm=TRUE)
    N<- max(0.000727220998332296*D*E, M,na.rm=TRUE)
    O<- max(sum(A*B , C,na.rm=TRUE), N,na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 85.6414333977509 , O , -2.4689650389585*J , -0.000121000531063962*K^2,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NUJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 8),"Woqooyi_Galbeed_BeforeRegion"]
    B<- mean(rain.long[(t-5):(t- 1),"Gedo_rain"], na.rm=TRUE)
    C<- median(stations.long[(t-4):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    D<- tail(movavg(before.long[(t-7):(t- 1),"Woqooyi_Galbeed_BeforeRegion"],6,type="w"),1)
    E<- before.long[(t- 1),"Bay_BeforeRegion"]
    G<- stations.long[(t- 1),"Gedo_LuuqStation_Juba_River"]
    H<- future.long[(t- 1),"Mudug_FutureRegion"]
    I<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    J<- future.long[(t- 1),"Mudug_FutureRegion"]
    K<- tail(movavg(rain.long[(t-9):(t- 1),"Jubbada_Dhexe_rain"], 8,type="m"),1)
    L<- median(future.long[(t-5):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    M<- future.long[(t- 1),"Bari_FutureRegion"]
    N<- rain.long[(t- 1),"Woqooyi_Galbeed_rain"]
    O<- min(0.000736976815983961*I, J,na.rm=TRUE)
    P<- max(0.0141487941402025*E*G, H*O,na.rm=TRUE)
    Q<- max(240.948905655337, P,na.rm=TRUE)
    R<- max(sum(B*C , D,na.rm=TRUE),sum( Q , -K,na.rm=TRUE),na.rm=TRUE)
    S<- max(A, R,na.rm=TRUE)
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( S , -L , -0.00962183191622608*M*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NUJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- mean(current.long[(t-5):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    B<- mean(water.long[(t-6):(t- 1),"Shabeallaha_Dhexe_WaterDrumPrice"], na.rm=TRUE)
    C<- conflicts.long[(t- 3),"Jubbada_Dhexe_Conflict"]
    D<- tail(movavg(future.long[(t-4):(t- 1),"Sanaag_FutureRegion"], 3,type="m"),1)
    E<- before.long[(t- 1),"Awdal_BeforeRegion"]
    G<- rain.long[(t- 17),"Jubbada_Hoose_rain"]
    H<- mean(goats.long[(t-17):(t- 1),"Awdal_goatprice"], na.rm=TRUE)
    I<- mean(current.long[(t-5):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    J<- mean(water.long[(t-6):(t- 1),"Shabeallaha_Dhexe_WaterDrumPrice"], na.rm=TRUE)
    K<- before.long[(t- 1),"Awdal_BeforeRegion"]
    L<- fatalities.long[(t- 1),"Galguduud_Fatalities"]
    M<- tail(movavg(future.long[(t-4):(t- 1),"Sanaag_FutureRegion"], 3,type="m"),1)
    N<- goats.long[(t- 1),"Bakool_goatprice"]
    O<- median(before.long[(t-9):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    P<- tan(D)
    Q<- tan(H)
    R<- tan(sum(0.247272274024342*I , 0.117179815934248*J , K,na.rm=TRUE))
    S<- max(G, Q*R,na.rm=TRUE)
    U<- max(sum(0.247272274024342*A , 0.117179815934248*B , C*P , E , S , -1206.40836182405 , -L , -M , -0.000363298572984585*N,na.rm=TRUE), O,na.rm=TRUE)
    FIN <-U
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_NUJUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    B<- median(conflicts.long[(t-17):(t- 1),"Woqooyi_Galbeed_Conflict"], na.rm=TRUE)
    C<- median(goats.long[(t-14):(t- 1),"Jubbada_Dhexe_goatprice"], na.rm=TRUE)
    D<- conflicts.long[(t- 14),"Bari_Conflict"]
    E<- tail(movavg(stations.long[(t-11):(t- 1),"Juba_Dhexe_BualleStation_Juba_River"],10,type="w"),1)
    G<- conflicts.long[(t- 16),"Bari_Conflict"]
    H<- tail(movavg(stations.long[(t-13):(t- 1),"Juba_Dhexe_BualleStation_Juba_River"], 12,type="m"),1)
    I<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    J<- conflicts.long[(t- 14),"Bari_Conflict"]
    K<- future.long[(t- 1),"Sanaag_FutureRegion"]
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
    FIN <-sum( 0.0110237547513141*A , 46.9696748104853*B , 668242269/C , D*E , G*H , 0.00633498147741523*I*J , -847.379550420421 , -0.102723521661272*K,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}


