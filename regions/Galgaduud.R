
modelarrivals_GAminus1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 8),"Gedo_CurrentRegion"]
    B<- mean(current.long[(t-10):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    C<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    D<- current.long[(t- 7),"Sanaag_CurrentRegion"]
    E<- fatalities.long[(t- 1),"Bari_Fatalities"]
    G<- future.long[(t- 7),"Bakool_FutureRegion"]
    H<- median(fatalities.long[(t-7):(t- 1),"Shabeellaha_Hoose_Fatalities"], na.rm=TRUE)
    I<- before.long[(t- 1),"Sool_BeforeRegion"]
    J<- tail(movavg(future.long[(t-5):(t- 1),"Bakool_FutureRegion"], 4,type="m"),1)
    K<- before.long[(t- 1),"Awdal_BeforeRegion"]
    L<- before.long[(t- 1),"Sool_BeforeRegion"]
    M<- tail(movavg(future.long[(t-5):(t- 1),"Bakool_FutureRegion"], 4,type="m"),1)
    N<- current.long[(t- 8),"Gedo_CurrentRegion"]
    O<- mean(current.long[(t-10):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    P<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    if ( is.na(A) || is.na( B)){Q<-0}
    else if(A>= B){Q<-1 }
    else{Q<-0 }
    if ( is.na(Q) ){R<- C}
    else if(Q>0){R<- 1409.05787126661 }
    else{R<- C }
    S<- max(I, J,na.rm=TRUE)
    U<- max(D^1.12490060688324,sum( 0.00169471267963051*E*G*H , S , -6.20862393475086*K,na.rm=TRUE),na.rm=TRUE)
    V<- max(R, U,na.rm=TRUE)
    W<- max(L, M,na.rm=TRUE)
    if ( is.na(N) || is.na( O)){X<-0}
    else if(N>= O){X<-1 }
    else{X<-0 }
    if ( is.na(X) ){Y<- P}
    else if(X>0){Y<- 0.000346576880194448 }
    else{Y<- P }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(W)){W <- 0 }
    if(is.infinite(Y)){Y <- 0 }
    FIN <-sum( 1.12490060688324*V , -0.000346576880194448*W*Y,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GAminus2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 2),"Bari_FutureRegion"]
    B<- tail(movavg(current.long[(t-12):(t- 2),"Mudug_CurrentRegion"], 10,type="m"),1)
    C<- future.long[(t- 2),"Bari_FutureRegion"]
    D<- future.long[(t- 2),"Sanaag_FutureRegion"]
    E<- before.long[(t- 6),"Awdal_BeforeRegion"]
    G<- tail(movavg(before.long[(t-7):(t- 2),"Jubbada_Hoose_BeforeRegion"], 5,type="m"),1)
    H<- tail(movavg(future.long[(t-12):(t- 2),"Hiiraan_FutureRegion"], 10,type="m"),1)
    I<- future.long[(t- 2),"Bari_FutureRegion"]
    J<- tail(movavg(current.long[(t-12):(t- 2),"Mudug_CurrentRegion"], 10,type="m"),1)
    K<- tail(movavg(future.long[(t-12):(t- 2),"Hiiraan_FutureRegion"], 10,type="m"),1)
    L<- tail(movavg(future.long[(t-12):(t- 2),"Hiiraan_FutureRegion"], 10,type="m"),1)
    M<- future.long[(t- 2),"Bari_FutureRegion"]
    N<- tail(movavg(current.long[(t-12):(t- 2),"Mudug_CurrentRegion"], 10,type="m"),1)
    O<- future.long[(t- 2),"Bari_FutureRegion"]
    P<- tail(movavg(current.long[(t-12):(t- 2),"Mudug_CurrentRegion"], 10,type="m"),1)
    Q<- tail(movavg(future.long[(t-12):(t- 2),"Hiiraan_FutureRegion"], 10,type="m"),1)
    R<- future.long[(t- 2),"Bari_FutureRegion"]
    S<- tail(movavg(current.long[(t-12):(t- 2),"Mudug_CurrentRegion"], 10,type="m"),1)
    U<- future.long[(t- 4),"Nugaal_FutureRegion"]
    V<- conflicts.long[(t- 2),"Sanaag_Conflict"]
    W<- (sum(128978 , -13507.0056684329*C,na.rm=TRUE))
    if ( is.na(H) || is.na( 0.00125843186222215*I*J)){X<-0}
    else if(H<= 0.00125843186222215*I*J){X<-1 }
    else{X<-0 }
    if ( is.na(L) || is.na( 0.00125843186222215*M*N)){Y<-0}
    else if(L<= 0.00125843186222215*M*N){Y<-1 }
    else{Y<-0 }
    if ( is.na(Q) || is.na( 0.00125843186222215*R*S)){Z<-0}
    else if(Q<= 0.00125843186222215*R*S){Z<-1 }
    else{Z<-0 }
    AA<- exp(V)
    BB<- max(1585.04998369786,sum( 1.64147522090261*U , AA,na.rm=TRUE),na.rm=TRUE)
    CC<- max(0.00125843186222215*A*B,sum( W/D^E , G*X , K*Y , 0.00125843186222215*O*P*Z , BB,na.rm=TRUE),na.rm=TRUE)
    FIN <-CC
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GA1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Bakool_FutureRegion"]
    B<- current.long[(t- 1),"Shabeellaha_Dhexe_CurrentRegion"]
    C<- current.long[(t- 16),"Sanaag_CurrentRegion"]
    D<- before.long[(t- 1),"Sool_BeforeRegion"]
    E<- stations.long[(t- 1),"Gedo_LuuqStation_Juba_River"]
    G<- current.long[(t- 1),"Gedo_CurrentRegion"]
    H<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    I<- future.long[(t- 12),"Bay_FutureRegion"]
    J<- future.long[(t- 16),"Bakool_FutureRegion"]
    K<- mean(future.long[(t-17):(t- 1),"Mudug_FutureRegion"], na.rm=TRUE)
    L<- conflicts.long[(t- 1),"Awdal_Conflict"]
    M<- max(sum(1.9429459375718*C , 0.000115168797543588*D*E*G , H , I , J , -2207.90022760103,na.rm=TRUE),sum( 1498 , -K,na.rm=TRUE),na.rm=TRUE)
    N<- exp(L)
    O<- max(M, N,na.rm=TRUE)
    P<- max(sum(A , -B,na.rm=TRUE), O,na.rm=TRUE)
    FIN <-P
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GA2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 12),"Bay_FutureRegion"]
    B<- before.long[(t- 1),"Sool_BeforeRegion"]
    C<- current.long[(t- 1),"Gedo_CurrentRegion"]
    D<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    E<- conflicts.long[(t- 1),"Awdal_Conflict"]
    G<- tail(movavg(future.long[(t-4):(t- 1),"Bakool_FutureRegion"], 3,type="m"),1)
    H<- future.long[(t- 7),"Bakool_FutureRegion"]
    I<- future.long[(t- 16),"Bakool_FutureRegion"]
    J<- mean(fatalities.long[(t-10):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    K<- median(fatalities.long[(t-3):(t- 1),"Awdal_Fatalities"], na.rm=TRUE)
    L<- median(before.long[(t-8):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    M<- exp(E)
    N<- max(1.82892889547117*H, I*J*K,na.rm=TRUE)
    O<- max(sum(2317.07184236127 , 0.00016716681822229*B*C , -D,na.rm=TRUE),sum( M , G , N,na.rm=TRUE),na.rm=TRUE)
    P<- max(A, O,na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 1.08924461338692*P , L , -981.239524000519,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GA3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(future.long[(t-4):(t- 1),"Jubbada_Dhexe_FutureRegion"], 3,type="m"),1)
    B<- tail(movavg(fatalities.long[(t-17):(t- 1),"Nugaal_Fatalities"], 16,type="m"),1)
    C<- before.long[(t- 1),"Sool_BeforeRegion"]
    D<- stations.long[(t- 1),"Gedo_LuuqStation_Juba_River"]
    E<- tail(movavg(current.long[(t-3):(t- 1),"Gedo_CurrentRegion"], 2,type="m"),1)
    G<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    H<- future.long[(t- 4),"Shabeallaha_Dhexe_FutureRegion"]
    I<- conflicts.long[(t- 1),"Awdal_Conflict"]
    J<- tail(movavg(future.long[(t-11):(t- 1),"Banadir_FutureRegion"], 10,type="m"),1)
    K<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    L<- max(A*B, 0.000121932504526188*C*D*E,na.rm=TRUE)
    M<- cosh(I)
    N<- max(sum(3.65940442118076*G , H,na.rm=TRUE), 3.52839578001776*M)
    
    O<- max(sum(N , -7127.0428295369,na.rm=TRUE), J/K)
    if(is.infinite(O)){O<-sum(N , -7127.0428295369,na.rm=TRUE)}
    P<- max(O, 1262.00301968474)
    Q<- max(L, 1.25597956198949*P)
    FIN <-Q
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GA4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Sool_BeforeRegion"]
    B<- current.long[(t- 1),"Gedo_CurrentRegion"]
    C<- before.long[(t- 1),"Sool_BeforeRegion"]
    D<- future.long[(t- 12),"Hiiraan_FutureRegion"]
    E<- before.long[(t- 12),"Hiiraan_BeforeRegion"]
    G<- future.long[(t- 12),"Hiiraan_FutureRegion"]
    H<- before.long[(t- 6),"Hiiraan_BeforeRegion"]
    I<- future.long[(t- 16),"Bakool_FutureRegion"]
    J<- future.long[(t- 17),"Jubbada_Dhexe_FutureRegion"]
    K<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    L<- max(1930.3259990639,sum( 0.0592329444425768*H , I , J,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 0.000175797283886105*A*B , 0.000240514376096206*C*D , 1.14963789346095e-5*E*G , 2.81433172993584*L , -3845.93659046548 , -K,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GA5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(future.long[(t-6):(t- 1),"Bakool_FutureRegion"], 5,type="m"),1)
    B<- median(fatalities.long[(t-8):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    C<- future.long[(t- 1),"Jubbada_Dhexe_FutureRegion"]
    D<- tail(movavg(conflicts.long[(t-14):(t- 1),"Awdal_Conflict"],13,type="w"),1)
    E<- before.long[(t- 1),"Sool_BeforeRegion"]
    G<- stations.long[(t- 1),"Gedo_LuuqStation_Juba_River"]
    H<- current.long[(t- 1),"Gedo_CurrentRegion"]
    I<- before.long[(t- 1),"Jubbada_Dhexe_BeforeRegion"]
    J<- future.long[(t- 1),"Banadir_FutureRegion"]
    K<- conflicts.long[(t- 1),"Awdal_Conflict"]
    L<- before.long[(t- 5),"Jubbada_Dhexe_BeforeRegion"]
    M<- future.long[(t- 12),"Bay_FutureRegion"]
    N<- exp(K)
    O<- max(L, M,na.rm=TRUE)
    P<- max(sum(C*D , 0.000110146001407846*E*G*H , I , -J,na.rm=TRUE),sum( N , O,na.rm=TRUE),na.rm=TRUE)
    Q<- max(2197.83927090044, P,na.rm=TRUE)
    R<- max(A*B, Q,na.rm=TRUE)
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( 1.08349919054592*R , -796.308032985746,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GA6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Bakool_FutureRegion"]
    B<- before.long[(t- 1),"Sool_BeforeRegion"]
    C<- stations.long[(t- 1),"Gedo_LuuqStation_Juba_River"]
    D<- current.long[(t- 1),"Gedo_CurrentRegion"]
    E<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    G<- current.long[(t- 1),"Shabeellaha_Dhexe_CurrentRegion"]
    H<- future.long[(t- 12),"Bay_FutureRegion"]
    I<- future.long[(t- 1),"Jubbada_Dhexe_FutureRegion"]
    J<- median(conflicts.long[(t-5):(t- 1),"Awdal_Conflict"], na.rm=TRUE)
    K<- current.long[(t- 11),"Woqooyi_Galbeed_CurrentRegion"]
    L<- conflicts.long[(t- 1),"Awdal_Conflict"]
    M<- exp(L)
    N<- max(H,sum( I*J , K , M,na.rm=TRUE),na.rm=TRUE)
    O<- max(sum(1.4976215446362*A , 0.000121451158456123*B*C*D , -E*G,na.rm=TRUE), N,na.rm=TRUE)
    P<- max(2287.4441373579, O,na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( 1.08833300917152*P , -904.450961272514,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GA7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Sool_BeforeRegion"]
    B<- conflicts.long[(t- 1),"Awdal_Conflict"]
    C<- fatalities.long[(t- 1),"Hiiraan_Fatalities"]
    D<- rain.long[(t- 8),"Banaadir_rain"]
    E<- goats.long[(t- 1),"Galgaduud_goatprice"]
    G<- fatalities.long[(t- 1),"Bari_Fatalities"]
    H<- stations.long[(t- 4),"Shabelle_Dhexe_JowharStation_Shabelle_River"]
    I<- tail(movavg(before.long[(t-4):(t- 1),"Bari_BeforeRegion"], 3,type="m"),1)
    J<- current.long[(t- 8),"Sanaag_CurrentRegion"]
    K<- current.long[(t- 1),"Sool_CurrentRegion"]
    L<- rain.long[(t- 7),"Banaadir_rain"]
    M<- future.long[(t- 2),"Banadir_FutureRegion"]
    N<- exp(B)
    if ( is.na(65) || is.na( G)){O<-0}
    else if(65< G){O<-1 }
    else{O<-0 }
    if ( is.na(L) ){P<- M}
    else if(L>0){P<- 1585.04999991878 }
    else{P<- M }
    Q<- max(sum(1.05130396737773*A , 1.05130396737773*N , C*D , E*O , H*I , J , -K,na.rm=TRUE), P,na.rm=TRUE)
    R<- Q%% 102055.908887434
    FIN <-R
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GA8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(rain.long[(t-14):(t- 1),"Togdheer_rain"], na.rm=TRUE)
    B<- fatalities.long[(t- 4),"Jubbada_Dhexe_Fatalities"]
    C<- fatalities.long[(t- 1),"Bari_Fatalities"]
    D<- fatalities.long[(t- 4),"Jubbada_Dhexe_Fatalities"]
    E<- before.long[(t- 1),"Sool_BeforeRegion"]
    G<- current.long[(t- 1),"Gedo_CurrentRegion"]
    H<- tail(movavg(stations.long[(t-4):(t- 1),"Gedo_DollowStation_Juba_River"], 3,type="m"),1)
    I<- before.long[(t- 1),"Awdal_BeforeRegion"]
    J<- future.long[(t- 3),"Bari_FutureRegion"]
    K<- conflicts.long[(t- 1),"Awdal_Conflict"]
    L<- conflicts.long[(t- 1),"Mudug_Conflict"]
    M<- median(conflicts.long[(t-3):(t- 1),"Awdal_Conflict"], na.rm=TRUE)
    N<- exp(K)
    if ( is.na(A) ){O<- N}
    else if(A>0){O<-sum( 1406.87098466257 , 8.85374107793582*B , 8.85374107793582*C*D , 9.51679174631635e-5*E*G*H , -I , -J,na.rm=TRUE) }
    else{O<- N }
    P<- cosh(M)
    Q<- max(1.11243494667124*O, L*P,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GA9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Bakool_FutureRegion"]
    B<- before.long[(t- 1),"Sool_BeforeRegion"]
    C<- tail(movavg(conflicts.long[(t-4):(t- 1),"Awdal_Conflict"],3,type="w"),1)
    D<- before.long[(t- 1),"Sool_BeforeRegion"]
    E<- stations.long[(t- 1),"Gedo_LuuqStation_Juba_River"]
    G<- current.long[(t- 1),"Gedo_CurrentRegion"]
    H<- current.long[(t- 8),"Sanaag_CurrentRegion"]
    I<- future.long[(t- 7),"Bakool_FutureRegion"]
    J<- before.long[(t- 1),"Awdal_BeforeRegion"]
    K<- max(sum(0.035896568152194*B , 3.67471135877285^C , 0.000114843567905122*D*E*G , H,na.rm=TRUE),sum( 13.4420974063689*I , -29093.500816553,na.rm=TRUE),na.rm=TRUE)
    L<- max(sum(2.42975870219419*A , -4734.93297154324,na.rm=TRUE), K,na.rm=TRUE)
    M<- max(L, 1599.8681811859,na.rm=TRUE)
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(J)){J <- 0 }
    FIN <-sum( M , -J,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GA10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 4),"Shabeallaha_Dhexe_FutureRegion"]
    B<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    C<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    D<- fatalities.long[(t- 1),"Mudug_Fatalities"]
    E<- median(rain.long[(t-11):(t- 1),"Hiiraan_rain"], na.rm=TRUE)
    G<- before.long[(t- 1),"Sool_BeforeRegion"]
    H<- current.long[(t- 1),"Gedo_CurrentRegion"]
    I<- before.long[(t- 1),"Sool_BeforeRegion"]
    J<- current.long[(t- 1),"Gedo_CurrentRegion"]
    K<- median(rain.long[(t-11):(t- 1),"Hiiraan_rain"], na.rm=TRUE)
    L<- current.long[(t- 1),"Sool_CurrentRegion"]
    M<- max(A, 6709,na.rm=TRUE)
    N<- max(0.000188665280698418*G*H,sum( 422.388236578383 , 6.57392657327954e-8*I^2*J*K,na.rm=TRUE),na.rm=TRUE)
    O<- max(C*D*E, N,na.rm=TRUE)
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 1.18811344775081*M , B , O , -6786.82897230302 , -0.277512813039466*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GAJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Awdal_Conflict"]
    B<- future.long[(t- 12),"Bay_FutureRegion"]
    C<- median(before.long[(t-5):(t- 1),"Galgaduud_BeforeRegion"], na.rm=TRUE)
    D<- current.long[(t- 4),"Sanaag_CurrentRegion"]
    E<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    G<- conflicts.long[(t- 16),"Mudug_Conflict"]
    H<- before.long[(t- 1),"Sool_BeforeRegion"]
    I<- current.long[(t- 6),"Bakool_CurrentRegion"]
    J<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    K<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    L<- exp(A)
    M<- max(2991.54443458463, 3.26030964193395*D,na.rm=TRUE)
    N<- max(M,sum( E*G , H , I,na.rm=TRUE),na.rm=TRUE)
    O<- max(sum(1.1094204044619*L , B , C,na.rm=TRUE), N,na.rm=TRUE)
    if ( is.na(1426.40865673524) || is.na( K)){P<-0}
    else if(1426.40865673524< K){P<-1 }
    else{P<-0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( 1.1094204044619*O , -1851.15821530141 , -2.1950387495302*J*P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GAJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 4),"Sanaag_CurrentRegion"]
    B<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    C<- mean(stations.long[(t-9):(t- 1),"Juba_Dhexe_BualleStation_Juba_River"], na.rm=TRUE)
    D<- before.long[(t- 1),"Sool_BeforeRegion"]
    E<- conflicts.long[(t- 1),"Awdal_Conflict"]
    G<- before.long[(t- 6),"Sool_BeforeRegion"]
    H<- current.long[(t- 6),"Bakool_CurrentRegion"]
    I<- tail(movavg(future.long[(t-11):(t- 1),"Galgaduud_FutureRegion"], 10,type="m"),1)
    J<- before.long[(t- 6),"Sool_BeforeRegion"]
    K<- before.long[(t- 1),"Sool_BeforeRegion"]
    L<- future.long[(t- 12),"Bay_FutureRegion"]
    M<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    N<- sinh(E)
    O<- max(sum(3.19907832716756*A , B*C , D,na.rm=TRUE), 2.43129928734016*N,na.rm=TRUE)
    P<- max(O, 1692.60777207386,na.rm=TRUE)
    Q<- max(P, G,na.rm=TRUE)
    R<- max(Q, H,na.rm=TRUE)
    S<- atan2(J, K)
    U<- max(sum(R , -2.43129928734016*I,na.rm=TRUE), S^2*L,na.rm=TRUE)
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( U , -1.93962817212154*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GAJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 12),"Togdheer_FutureRegion"]
    B<- before.long[(t- 12),"Galgaduud_BeforeRegion"]
    C<- before.long[(t- 9),"Galgaduud_BeforeRegion"]
    D<- future.long[(t- 12),"Togdheer_FutureRegion"]
    E<- before.long[(t- 1),"Sool_BeforeRegion"]
    G<- before.long[(t- 1),"Sool_BeforeRegion"]
    H<- future.long[(t- 17),"Galgaduud_FutureRegion"]
    I<- median(future.long[(t-3):(t- 1),"Bari_FutureRegion"], na.rm=TRUE)
    J<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    K<- future.long[(t- 12),"Bay_FutureRegion"]
    L<- future.long[(t- 12),"Togdheer_FutureRegion"]
    M<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    N<- future.long[(t- 12),"Togdheer_FutureRegion"]
    O<- before.long[(t- 1),"Sool_BeforeRegion"]
    P<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    Q<- sin(sum(15.8395664737304*D , E,na.rm=TRUE))
    R<- atan2(L, M)
    S<- sin(sum(15.8395664737304*N , O,na.rm=TRUE))
    U<- max(sum(1476.90041915523 , 1.70233005249552*J,na.rm=TRUE), K*R*S,na.rm=TRUE)
    V<- max(sum(15.8395664737304*A , 0.176158485141869*B , C*Q , G , H , I , -15.8395664737304,na.rm=TRUE), U,na.rm=TRUE)
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( V , -1.93797053396671*P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GAJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 7),"Bakool_BeforeRegion"]
    B<- future.long[(t- 7),"Bakool_FutureRegion"]
    C<- current.long[(t- 4),"Sanaag_CurrentRegion"]
    D<- before.long[(t- 7),"Bakool_BeforeRegion"]
    E<- rain.long[(t- 11),"Bay_rain"]
    G<- before.long[(t- 1),"Sool_BeforeRegion"]
    H<- current.long[(t- 8),"Sanaag_CurrentRegion"]
    I<- median(before.long[(t-4):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    J<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    K<- before.long[(t- 7),"Bakool_BeforeRegion"]
    L<- max(1405.24480086772,sum( 2.72190708393784*C , 0.0287142625910291*D*E , G , H , I,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 0.00106134922338474*A*B , 1.0868698839719*L , -2.18565403310112*J , -0.214921660016529*K,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GAJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 7),"Sanaag_CurrentRegion"]
    B<- before.long[(t- 1),"Sool_BeforeRegion"]
    C<- current.long[(t- 6),"Woqooyi_Galbeed_CurrentRegion"]
    D<- mean(current.long[(t-4):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- current.long[(t- 8),"Sanaag_CurrentRegion"]
    H<- stations.long[(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"]
    I<- current.long[(t- 4),"Sanaag_CurrentRegion"]
    J<- conflicts.long[(t- 1),"Shabeellaha_Hoose_Conflict"]
    K<- future.long[(t- 7),"Bakool_FutureRegion"]
    L<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    M<- tan(J)
    N<- max(H*I, 0.959455121198344*M*K,na.rm=TRUE)
    O<- max(1546, N,na.rm=TRUE)
    P<- max(sum(4.1440061668285*E , G,na.rm=TRUE), O,na.rm=TRUE)
    Q<- max(sum(B , C , D,na.rm=TRUE), P,na.rm=TRUE)
    R<- max(3.15227877367352*A, Q,na.rm=TRUE)
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( R , -2.02211335300481*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GAJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 6),"Bakool_CurrentRegion"]
    B<- mean(current.long[(t-8):(t- 1),"Mudug_CurrentRegion"], na.rm=TRUE)
    C<- tail(movavg(future.long[(t-5):(t- 1),"Galgaduud_FutureRegion"],4,type="w"),1)
    D<- median(fatalities.long[(t-10):(t- 1),"Jubbada_Dhexe_Fatalities"], na.rm=TRUE)
    E<- future.long[(t- 16),"Bakool_FutureRegion"]
    G<- median(fatalities.long[(t-12):(t- 1),"Jubbada_Dhexe_Fatalities"], na.rm=TRUE)
    H<- before.long[(t- 1),"Sool_BeforeRegion"]
    I<- conflicts.long[(t- 1),"Awdal_Conflict"]
    J<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    K<- tail(movavg(stations.long[(t-17):(t- 1),"Hiiraan_Belet_WeyneStation_Shabelle_River"], 16,type="m"),1)
    L<- before.long[(t- 12),"Bakool_BeforeRegion"]
    M<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    N<- exp(I)
    O<- max(sum(E*G , H,na.rm=TRUE), N,na.rm=TRUE)
    P<- max(J*K,sum( 2.48776844922408*L , -12892.0291355954,na.rm=TRUE),na.rm=TRUE)
    Q<- max(sum(1543.26107786158 , -C*D,na.rm=TRUE),sum( O , P,na.rm=TRUE),na.rm=TRUE)
    R<- max(sum(A , -B,na.rm=TRUE), Q,na.rm=TRUE)
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( R , -1.93930463563291*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GAJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- stations.long[(t- 4),"Hiiraan_Bulo_Burti_StationShabelle_River"]
    B<- future.long[(t- 13),"Gedo_FutureRegion"]
    C<- future.long[(t- 1),"Bakool_FutureRegion"]
    D<- future.long[(t- 17),"Galgaduud_FutureRegion"]
    E<- future.long[(t- 17),"Jubbada_Dhexe_FutureRegion"]
    G<- future.long[(t- 17),"Jubbada_Hoose_FutureRegion"]
    H<- future.long[(t- 12),"Bay_FutureRegion"]
    I<- future.long[(t- 13),"Jubbada_Dhexe_FutureRegion"]
    J<- tail(movavg(future.long[(t-3):(t- 1),"Jubbada_Dhexe_FutureRegion"], 2,type="m"),1)
    K<- before.long[(t- 1),"Sool_BeforeRegion"]
    L<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    M<- max(A*B,sum( 1.09955975861085*C , D , E , G,na.rm=TRUE),na.rm=TRUE)
    N<- max(2566.91317460999, M,na.rm=TRUE)
    O<- max(H,sum( I , J,na.rm=TRUE),na.rm=TRUE)
    P<- max(N, O,na.rm=TRUE)
    Q<- max(K, 533.80137291894,na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 1.09955975861085*P , Q , -1813.04910985621 , -1.93850377251235*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GAJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 3),"Mudug_BeforeRegion"]
    B<- future.long[(t- 12),"Bay_FutureRegion"]
    C<- before.long[(t- 4),"Mudug_BeforeRegion"]
    D<- current.long[(t- 4),"Sanaag_CurrentRegion"]
    E<- before.long[(t- 1),"Sool_BeforeRegion"]
    G<- future.long[(t- 1),"Jubbada_Dhexe_FutureRegion"]
    H<- current.long[(t- 6),"Togdheer_CurrentRegion"]
    I<- current.long[(t- 6),"Bakool_CurrentRegion"]
    J<- before.long[(t- 4),"Mudug_BeforeRegion"]
    K<- current.long[(t- 4),"Sanaag_CurrentRegion"]
    L<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    M<- max(B, 2247,na.rm=TRUE)
    N<- max(1.08843878764256*M,sum( 1885.39684158995 , 0.000835147573060477*C*D , E,na.rm=TRUE),na.rm=TRUE)
    O<- max(0.450892403462756*A, N,na.rm=TRUE)
    P<- max(O,sum( G , H , I , -0.000835147573060477*J*K,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( P , -902.162463248814 , -1.94704226262197*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GAJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Sool_BeforeRegion"]
    B<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    C<- future.long[(t- 7),"Bakool_FutureRegion"]
    D<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    E<- before.long[(t- 1),"Sool_BeforeRegion"]
    G<- future.long[(t- 7),"Bakool_FutureRegion"]
    H<- median(conflicts.long[(t-7):(t- 1),"Sanaag_Conflict"], na.rm=TRUE)
    I<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    J<- conflicts.long[(t- 1),"Hiiraan_Conflict"]
    K<- before.long[(t- 1),"Sool_BeforeRegion"]
    L<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    M<- future.long[(t- 7),"Bakool_FutureRegion"]
    N<- max(B, 0.0123183391925471*C,na.rm=TRUE)
    O<- sinh(N)
    if ( is.na(2602115.21383863) || is.na( A*O)){P<-0}
    else if(2602115.21383863<= A*O){P<-1 }
    else{P<-0 }
    Q<- exp(0.000385380222366046*G*H)
    R<- tan(4.0308669788211e-5*I^3*J)
    S<- max(L, 0.0123183391925471*M,na.rm=TRUE)
    U<- sinh(S)
    if ( is.na(2602115.21383863) || is.na( K*U)){V<-0}
    else if(2602115.21383863<= K*U){V<-1 }
    else{V<-0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(V)){V <- 0 }
    FIN <-sum( 1467.72222222222 , 1467.72222222222*P , 0.729304544092168*D*E*Q*R*V,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_GAJUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Mudug_FutureRegion"]
    B<- tail(movavg(future.long[(t-5):(t- 1),"Jubbada_Dhexe_FutureRegion"], 4,type="m"),1)
    C<- mean(fatalities.long[(t-4):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    D<- future.long[(t- 1),"Jubbada_Dhexe_FutureRegion"]
    E<- before.long[(t- 1),"Sool_BeforeRegion"]
    G<- before.long[(t- 1),"Sool_BeforeRegion"]
    H<- fatalities.long[(t- 4),"Jubbada_Dhexe_Fatalities"]
    I<- fatalities.long[(t- 3),"Jubbada_Dhexe_Fatalities"]
    J<- mean(fatalities.long[(t-10):(t- 1),"Bari_Fatalities"], na.rm=TRUE)
    K<- fatalities.long[(t- 1),"Gedo_Fatalities"]
    L<- fatalities.long[(t- 3),"Jubbada_Dhexe_Fatalities"]
    M<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    N<- 1489%% E
    O<- asinh(J)
    P<- max(G, H^2*I*O,na.rm=TRUE)
    Q<- max(1489,sum( N , P,na.rm=TRUE),na.rm=TRUE)
    R<- max(C*D,sum( 1.03737110763639*Q , -K*L,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 0.000348470116382759*A*B , R , -2.05631272282681*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}


