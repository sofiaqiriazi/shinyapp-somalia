
modelarrivals_MUminus1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(current.long[(t-13):(t- 2),"Sool_CurrentRegion"], na.rm=TRUE)
    B<- tail(movavg(before.long[(t-5):(t- 2),"Jubbada_Dhexe_BeforeRegion"], 3,type="m"),1)
    C<- tail(movavg(current.long[(t-5):(t- 2),"Awdal_CurrentRegion"],3,type="w"),1)
    D<- current.long[(t- 10),"Woqooyi_Galbeed_CurrentRegion"]
    E<- future.long[(t- 12),"Mudug_FutureRegion"]
    G<- tail(movavg(conflicts.long[(t-15):(t- 2),"Awdal_Conflict"],13,type="w"),1)
    H<- fatalities.long[(t- 2),"Awdal_Fatalities"]
    I<- tail(movavg(current.long[(t-6):(t- 2),"Awdal_CurrentRegion"], 4,type="m"),1)
    J<- before.long[(t- 2),"Nugaal_BeforeRegion"]
    K<- conflicts.long[(t- 2),"Awdal_Conflict"]
    L<- tail(movavg(future.long[(t-14):(t- 2),"Bari_FutureRegion"], 12,type="m"),1)
    M<- median(conflicts.long[(t-7):(t- 2),"Sool_Conflict"], na.rm=TRUE)
    N<- future.long[(t- 2),"Bay_FutureRegion"]
    O<- median(conflicts.long[(t-7):(t- 2),"Sool_Conflict"], na.rm=TRUE)
    P<- tail(movavg(future.long[(t-7):(t- 2),"Jubbada_Hoose_FutureRegion"],5,type="w"),1)
    Q<- max(J, K*L*M,na.rm=TRUE)
    R<- max(E*G,sum( H^8.97290686433423 , 1.61869438136028*I , Q , -N*O,na.rm=TRUE),na.rm=TRUE)
    S<- max(D, R,na.rm=TRUE)
    U<- max(C, S,na.rm=TRUE)
    V<- max(B, U,na.rm=TRUE)
    W<- max(A,sum( V , -P,na.rm=TRUE),na.rm=TRUE)
    FIN <-W
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MUminus2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(current.long[(t-13):(t- 2),"Sool_CurrentRegion"], na.rm=TRUE)
    B<- conflicts.long[(t- 2),"Awdal_Conflict"]
    C<- mean(fatalities.long[(t-4):(t- 2),"Awdal_Fatalities"], na.rm=TRUE)
    D<- median(before.long[(t-13):(t- 2),"Shabeellaha_Dhexe_BeforeRegion"], na.rm=TRUE)
    E<- stations.long[(t- 7),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
    G<- future.long[(t- 12),"Mudug_FutureRegion"]
    H<- future.long[(t- 2),"Nugaal_FutureRegion"]
    I<- mean(current.long[(t-7):(t- 2),"Awdal_CurrentRegion"], na.rm=TRUE)
    J<- tail(movavg(future.long[(t-14):(t- 2),"Mudug_FutureRegion"], 12,type="m"),1)
    K<- future.long[(t- 2),"Woqooyi_Galbeed_FutureRegion"]
    L<- tail(movavg(rain.long[(t-8):(t- 2),"Shabeellaha_Hoose_rain"], 6,type="m"),1)
    M<- tail(movavg(future.long[(t-12):(t- 2),"Mudug_FutureRegion"], 10,type="m"),1)
    N<- tail(movavg(future.long[(t-17):(t- 2),"Mudug_FutureRegion"], 15,type="m"),1)
    O<- tail(movavg(future.long[(t-15):(t- 2),"Mudug_FutureRegion"], 13,type="m"),1)
    P<- exp(B)
    Q<- max(E*G,sum( H , I , J,na.rm=TRUE),na.rm=TRUE)
    R<- max(D, Q,na.rm=TRUE)
    S<- max(A,sum( 1.82931307342884*P*C , R , -K*L,na.rm=TRUE),na.rm=TRUE)
    U<- max(S, M,na.rm=TRUE)
    V<- N%% O
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(V)){V <- 0 }
    FIN <-sum( U , -0.979300689313414*V,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MU1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(current.long[(t-6):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], 5,type="m"),1)
    B<- median(fatalities.long[(t-16):(t- 1),"Banaadir_Fatalities"], na.rm=TRUE)
    C<- tail(movavg(future.long[(t-6):(t- 1),"Mudug_FutureRegion"], 5,type="m"),1)
    D<- tail(movavg(future.long[(t-6):(t- 1),"Mudug_FutureRegion"], 5,type="m"),1)
    E<- future.long[(t- 1),"Mudug_FutureRegion"]
    G<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    H<- before.long[(t- 1),"Mudug_BeforeRegion"]
    I<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    J<- future.long[(t- 1),"Mudug_FutureRegion"]
    K<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    L<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    M<- sin(D)
    N<- logistic(0.00531669665575306*E)
    O<- cosh(I)
    if ( is.na(G) ){P<- O}
    else if(G>0){P<- 0.202222542345441*H }
    else{P<- O }
    Q<- max(0.865862429181062*J, K*L,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( 0.0127397291519001*A*B , 0.965404805729258*C*M*N , P , Q , -95.1296528927314,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MU2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Mudug_FutureRegion"]
    B<- tail(movavg(current.long[(t-4):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], 3,type="m"),1)
    C<- before.long[(t- 1),"Mudug_BeforeRegion"]
    D<- tail(movavg(before.long[(t-3):(t- 1),"Nugaal_BeforeRegion"], 2,type="m"),1)
    E<- future.long[(t- 1),"Mudug_FutureRegion"]
    G<- before.long[(t- 1),"Mudug_BeforeRegion"]
    H<- tail(movavg(before.long[(t-7):(t- 1),"Bay_BeforeRegion"], 6,type="m"),1)
    I<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    J<- before.long[(t- 1),"Sool_BeforeRegion"]
    K<- median(goats.long[(t-5):(t- 1),"Banadir_goatprice"], na.rm=TRUE)
    L<- future.long[(t- 1),"Mudug_FutureRegion"]
    M<- tail(movavg(before.long[(t-3):(t- 1),"Nugaal_BeforeRegion"], 2,type="m"),1)
    N<- E%% 0.00264614042819078
    O<- max(1137.47510432613, 0.144247153537354*G,na.rm=TRUE)
    P<- min(H, 0.120952984080343*I*J,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 0.00264614042819078*A*B , C*D*N , O , P , -0.000458544676257965*K , -0.00151654806996284*L*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MU3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 7),"Hiiraan_Fatalities"]
    B<- median(rain.long[(t-6):(t- 1),"Bakool_rain"], na.rm=TRUE)
    C<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    D<- before.long[(t- 1),"Sool_BeforeRegion"]
    E<- before.long[(t- 1),"Mudug_BeforeRegion"]
    G<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    H<- future.long[(t- 1),"Mudug_FutureRegion"]
    I<- tail(movavg(current.long[(t-5):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], 4,type="m"),1)
    J<- fatalities.long[(t- 1),"Bay_Fatalities"]
    K<- before.long[(t- 1),"Mudug_BeforeRegion"]
    L<- mean(fatalities.long[(t-10):(t- 1),"Hiiraan_Fatalities"], na.rm=TRUE)
    M<- current.long[(t- 11),"Hiiraan_CurrentRegion"]
    N<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    O<- future.long[(t- 1),"Mudug_FutureRegion"]
    if ( is.na(J) ){P<- M}
    else if(J>0){P<- 0.00184666480730525*K*L }
    else{P<- M }
    Q<- max(sum(A*B , 0.0181567149339312*C^2*D,na.rm=TRUE),sum( 0.00184666480730525*E*G , 0.00264420839448385*H*I , P,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( Q , -0.00153030612888659*N*O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MU4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 9),"Banaadir_Fatalities"]
    B<- before.long[(t- 1),"Mudug_BeforeRegion"]
    C<- before.long[(t- 1),"Mudug_BeforeRegion"]
    D<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    E<- fatalities.long[(t- 7),"Hiiraan_Fatalities"]
    G<- median(conflicts.long[(t-4):(t- 1),"Togdheer_Conflict"], na.rm=TRUE)
    H<- median(before.long[(t-10):(t- 1),"Togdheer_BeforeRegion"], na.rm=TRUE)
    I<- future.long[(t- 1),"Mudug_FutureRegion"]
    J<- goats.long[(t- 1),"Jubbada_Dhexe_goatprice"]
    K<- conflicts.long[(t- 8),"Gedo_Conflict"]
    L<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    M<- before.long[(t- 1),"Sool_BeforeRegion"]
    if ( is.na(K) ){N<- 3566}
    else if(K>0){N<- 0.018337678491588*L^2*M }
    else{N<- 3566 }
    O<- max(1.12027672959086e-6*I*J, N,na.rm=TRUE)
    P<- max(sum(0.0903165409865906*B , 0.001762031889232*C*D , E*G , H,na.rm=TRUE), O,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( A , P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MU5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    B<- before.long[(t- 8),"Sanaag_BeforeRegion"]
    C<- mean(fatalities.long[(t-5):(t- 1),"Sanaag_Fatalities"], na.rm=TRUE)
    D<- conflicts.long[(t- 1),"Jubbada_Dhexe_Conflict"]
    E<- tail(movavg(goats.long[(t-4):(t- 1),"Jubbada_Dhexe_goatprice"], 3,type="m"),1)
    G<- tail(movavg(future.long[(t-6):(t- 1),"Mudug_FutureRegion"], 5,type="m"),1)
    H<- current.long[(t- 1),"Shabeellaha_Dhexe_CurrentRegion"]
    I<- rain.long[(t- 1),"Bakool_rain"]
    J<- conflicts.long[(t- 1),"Sool_Conflict"]
    K<- median(fatalities.long[(t-9):(t- 1),"Bay_Fatalities"], na.rm=TRUE)
    L<- tail(movavg(current.long[(t-10):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], 9,type="m"),1)
    M<- median(before.long[(t-16):(t- 1),"Bay_BeforeRegion"], na.rm=TRUE)
    N<- mean(future.long[(t-9):(t- 1),"Jubbada_Hoose_FutureRegion"], na.rm=TRUE)
    O<- before.long[(t- 3),"Awdal_BeforeRegion"]
    P<- stations.long[(t- 15),"Gedo_LuuqStation_Juba_River"]
    if ( is.na(I) || is.na( J)){Q<-0}
    else if(I< J){Q<-1 }
    else{Q<-0 }
    R<- atan2(Q, K)
    S<- atan2(M, N)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( A^2 , B*C , 3.70442024245303e-7*D*E*G , H*R , L*S , -O*P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MU6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 7),"Bari_BeforeRegion"]
    B<- before.long[(t- 9),"Nugaal_BeforeRegion"]
    C<- tail(movavg(future.long[(t-5):(t- 1),"Mudug_FutureRegion"], 4,type="m"),1)
    D<- fatalities.long[(t- 1),"Awdal_Fatalities"]
    E<- tail(movavg(before.long[(t-5):(t- 1),"Bari_BeforeRegion"], 4,type="m"),1)
    G<- fatalities.long[(t- 8),"Shabeellaha_Hoose_Fatalities"]
    H<- conflicts.long[(t- 1),"Togdheer_Conflict"]
    I<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    J<- mean(water.long[(t-4):(t- 1),"Sool_WaterDrumPrice"], na.rm=TRUE)
    K<- tail(movavg(current.long[(t-9):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], 8,type="m"),1)
    L<- current.long[(t- 13),"Shabeellaha_Dhexe_CurrentRegion"]
    M<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    N<- future.long[(t- 1),"Nugaal_FutureRegion"]
    O<- stations.long[(t- 1),"Juba_Dhexe_BualleStation_Juba_River"]
    P<- max(1.25596316748285*C, D*E,na.rm=TRUE)
    Q<- max(H*I, 7.06503365038935e-6*J*K,na.rm=TRUE)
    if ( is.na(G) ){R<- L}
    else if(G>0){R<- Q }
    else{R<- L }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 1.32831722834471*A , B , P , R , -M , -N*O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MU7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- before.long[(t- 1),"Mudug_BeforeRegion"]
    C<- future.long[(t- 1),"Mudug_FutureRegion"]
    D<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    E<- before.long[(t- 1),"Mudug_BeforeRegion"]
    G<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    H<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    I<- before.long[(t- 1),"Sool_BeforeRegion"]
    J<- fatalities.long[(t- 15),"Banaadir_Fatalities"]
    K<- future.long[(t- 1),"Mudug_FutureRegion"]
    L<- current.long[(t- 1),"Sool_CurrentRegion"]
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
    FIN <-sum( 0.664225724736368*A , 0.0882427496455386*B , 0.0027257322333548*C*D , 0.00157930011739063*E*G , 0.01735977952176*H^2*I , J , -0.000324586526038558*K*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MU8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Mudug_FutureRegion"]
    B<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    C<- before.long[(t- 1),"Mudug_BeforeRegion"]
    D<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    E<- before.long[(t- 1),"Mudug_BeforeRegion"]
    G<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    H<- before.long[(t- 1),"Sool_BeforeRegion"]
    I<- median(goats.long[(t-7):(t- 1),"Jubbada_Hoose_goatprice"], na.rm=TRUE)
    J<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    K<- future.long[(t- 1),"Mudug_FutureRegion"]
    L<- max(0.00261480818215121*A*B, 0.00192570076831578*C*D,na.rm=TRUE)
    M<- max(sum(1308.09744269207 , 0.0710002343237065*E,na.rm=TRUE), 0.133365272374889*G*H,na.rm=TRUE)
    N<- max(sum(L , M , -0.00121468663844687*I , -0.00137951749630371*J*K,na.rm=TRUE), 105.263897378436,na.rm=TRUE)
    FIN <-N
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MU9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(current.long[(t-5):(t- 1),"Nugaal_CurrentRegion"], na.rm=TRUE)
    B<- tail(movavg(future.long[(t-5):(t- 1),"Mudug_FutureRegion"], 4,type="m"),1)
    C<- before.long[(t- 14),"Bari_BeforeRegion"]
    D<- conflicts.long[(t- 1),"Mudug_Conflict"]
    E<- mean(water.long[(t-4):(t- 1),"Bakool_WaterDrumPrice"], na.rm=TRUE)
    G<- tail(movavg(current.long[(t-10):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], 9,type="m"),1)
    H<- median(current.long[(t-13):(t- 1),"Jubbada_Dhexe_CurrentRegion"], na.rm=TRUE)
    I<- future.long[(t- 1),"Nugaal_FutureRegion"]
    J<- median(stations.long[(t-7):(t- 1),"Juba_Dhexe_BualleStation_Juba_River"], na.rm=TRUE)
    K<- conflicts.long[(t- 1),"Bari_Conflict"]
    L<- conflicts.long[(t- 1),"Jubbada_Hoose_Conflict"]
    M<- tail(movavg(stations.long[(t-3):(t- 1),"Juba_Dhexe_BualleStation_Juba_River"],2,type="w"),1)
    N<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    O<- acosh(D)
    P<- max(sum(1.28669978973434*B , C*O , 3.29332695174054e-5*E*G,na.rm=TRUE), H,na.rm=TRUE)
    Q<- max(A,sum( P , -I*J , -K*L*M , -1.71454849369428*N,na.rm=TRUE),na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MU10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 7),"Hiiraan_CurrentRegion"]
    B<- conflicts.long[(t- 4),"Galgaduud_Conflict"]
    C<- before.long[(t- 1),"Mudug_BeforeRegion"]
    D<- tail(movavg(before.long[(t-3):(t- 1),"Nugaal_BeforeRegion"], 2,type="m"),1)
    E<- fatalities.long[(t- 7),"Hiiraan_Fatalities"]
    G<- fatalities.long[(t- 7),"Hiiraan_Fatalities"]
    H<- mean(conflicts.long[(t-4):(t- 1),"Sanaag_Conflict"], na.rm=TRUE)
    I<- before.long[(t- 1),"Mudug_BeforeRegion"]
    J<- tail(movavg(fatalities.long[(t-10):(t- 1),"Hiiraan_Fatalities"],9,type="w"),1)
    K<- before.long[(t- 1),"Bari_BeforeRegion"]
    L<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    M<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    N<- future.long[(t- 1),"Mudug_FutureRegion"]
    O<- goats.long[(t- 1),"Jubbada_Dhexe_goatprice"]
    P<- gauss(B)
    Q<- max(sum(G*H , 0.00169426465105935*I*J , K,na.rm=TRUE), 0.086700453838862*L*M,na.rm=TRUE)
    R<- max(sum(0.00169426465105935*C*D , E , Q,na.rm=TRUE), 1.12100361377874e-6*N*O,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( A*P , R,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MUJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Mudug_FutureRegion"]
    B<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    C<- before.long[(t- 1),"Mudug_BeforeRegion"]
    D<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    E<- tail(movavg(before.long[(t-3):(t- 1),"Bari_BeforeRegion"], 2,type="m"),1)
    G<- future.long[(t- 1),"Bari_FutureRegion"]
    H<- tail(movavg(before.long[(t-5):(t- 1),"Awdal_BeforeRegion"], 4,type="m"),1)
    I<- future.long[(t- 1),"Nugaal_FutureRegion"]
    J<- stations.long[(t- 1),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
    K<- fatalities.long[(t- 1),"Sool_Fatalities"]
    L<- future.long[(t- 1),"Bari_FutureRegion"]
    M<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    N<- tan(M)
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
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 1.05039509410171*A , 0.59285553580005*B , 0.155109230953788*C , 0.00993030124351238*D*E , G , -H , -I*J , -0.165971904108459*K*L*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MUJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(current.long[(t-8):(t- 1),"Sool_CurrentRegion"], na.rm=TRUE)
    B<- future.long[(t- 1),"Mudug_FutureRegion"]
    C<- before.long[(t- 1),"Mudug_BeforeRegion"]
    D<- conflicts.long[(t- 1),"Mudug_Conflict"]
    E<- fatalities.long[(t- 4),"Banaadir_Fatalities"]
    G<- tail(movavg(conflicts.long[(t-9):(t- 1),"Togdheer_Conflict"], 8,type="m"),1)
    H<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    I<- before.long[(t- 1),"Mudug_BeforeRegion"]
    J<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    K<- future.long[(t- 1),"Togdheer_FutureRegion"]
    L<- future.long[(t- 2),"Shabeallaha_Dhexe_FutureRegion"]
    M<- future.long[(t- 1),"Nugaal_FutureRegion"]
    N<- max(I, J,na.rm=TRUE)
    O<- max(A,sum( 1.06003887447658*B , 0.128078633743735*C , 1.3155059188983^D , E*G , 0.00160755003879662*H*N , -K , -L,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( O , -3.49523822022665*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MUJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 4),"Banaadir_Fatalities"]
    B<- tail(movavg(before.long[(t-11):(t- 1),"Sool_BeforeRegion"], 10,type="m"),1)
    C<- fatalities.long[(t- 9),"Banaadir_Fatalities"]
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    G<- before.long[(t- 15),"Togdheer_BeforeRegion"]
    H<- current.long[(t- 8),"Woqooyi_Galbeed_CurrentRegion"]
    I<- future.long[(t- 11),"Sanaag_FutureRegion"]
    J<- tail(movavg(future.long[(t-6):(t- 1),"Mudug_FutureRegion"], 5,type="m"),1)
    K<- tail(movavg(current.long[(t-10):(t- 1),"Jubbada_Hoose_CurrentRegion"], 9,type="m"),1)
    L<- before.long[(t- 9),"Togdheer_BeforeRegion"]
    M<- tail(movavg(current.long[(t-9):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], 8,type="m"),1)
    N<- log(B)
    O<- max(sum(A*N , C,na.rm=TRUE),sum( 0.003316052580771*D*E , G,na.rm=TRUE),na.rm=TRUE)
    P<- max(449,sum( 0.000409454413342597*J*K , L , M,na.rm=TRUE),na.rm=TRUE)
    Q<- max(I, P,na.rm=TRUE)
    R<- max(H, Q,na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( O , R , -629.417846511437,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MUJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    B<- before.long[(t- 1),"Mudug_BeforeRegion"]
    C<- before.long[(t- 1),"Bari_BeforeRegion"]
    D<- future.long[(t- 1),"Mudug_FutureRegion"]
    E<- fatalities.long[(t- 7),"Hiiraan_Fatalities"]
    G<- tail(movavg(conflicts.long[(t-6):(t- 1),"Awdal_Conflict"], 5,type="m"),1)
    H<- current.long[(t- 1),"Awdal_CurrentRegion"]
    I<- before.long[(t- 4),"Bari_BeforeRegion"]
    J<- before.long[(t- 1),"Mudug_BeforeRegion"]
    K<- future.long[(t- 1),"Nugaal_FutureRegion"]
    L<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    M<- future.long[(t- 1),"Mudug_FutureRegion"]
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
    FIN <-sum( 0.346042411357239*A , 0.186214243581022*B , 0.0221966716800538*C*D , E*G , H , I , -250.693055662658 , -J^0.186214243581022*K , -0.00681932925869888*L*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MUJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(before.long[(t-15):(t- 1),"Galgaduud_BeforeRegion"], na.rm=TRUE)
    B<- future.long[(t- 14),"Galgaduud_FutureRegion"]
    C<- future.long[(t- 1),"Mudug_FutureRegion"]
    D<- before.long[(t- 1),"Mudug_BeforeRegion"]
    E<- before.long[(t- 1),"Mudug_BeforeRegion"]
    G<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    H<- before.long[(t- 8),"Sanaag_BeforeRegion"]
    I<- before.long[(t- 17),"Bari_BeforeRegion"]
    J<- median(before.long[(t-15):(t- 1),"Galgaduud_BeforeRegion"], na.rm=TRUE)
    K<- future.long[(t- 1),"Mudug_FutureRegion"]
    L<- before.long[(t- 1),"Mudug_BeforeRegion"]
    M<- before.long[(t- 1),"Mudug_BeforeRegion"]
    N<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    O<- before.long[(t- 8),"Sanaag_BeforeRegion"]
    P<- before.long[(t- 17),"Bari_BeforeRegion"]
    Q<- future.long[(t- 1),"Togdheer_FutureRegion"]
    R<- tail(movavg(current.long[(t-4):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], 3,type="m"),1)
    S<- before.long[(t- 8),"Sanaag_BeforeRegion"]
    U<- before.long[(t- 1),"Mudug_BeforeRegion"]
    V<- future.long[(t- 1),"Nugaal_FutureRegion"]
    if ( is.na(sum(1.05302960042865*C , 0.12123145497312*D , 0.00162877197543816*E*G , H , I,na.rm=TRUE)) || is.na( J)){W<-0}
    else if(sum(1.05302960042865*C , 0.12123145497312*D , 0.00162877197543816*E*G , H , I,na.rm=TRUE)>= J){W<-1 }
    else{W<-0 }
    X<- cos(S)
    Y<- max(sum(1.05302960042865*K , 0.12123145497312*L , 0.00162877197543816*M*N , O , P , -Q,na.rm=TRUE), R*X,na.rm=TRUE)
    if ( is.na(W) ){Z<- U}
    else if(W>0){Z<- Y }
    else{Z<- U }
    AA<- max(B, Z,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(V)){V <- 0 }
    FIN <-sum( A , AA , -3.45319925539325*V,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MUJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(current.long[(t-8):(t- 1),"Sool_CurrentRegion"], na.rm=TRUE)
    B<- future.long[(t- 1),"Mudug_FutureRegion"]
    C<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    D<- before.long[(t- 1),"Mudug_BeforeRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    H<- median(current.long[(t-14):(t- 1),"Sool_CurrentRegion"], na.rm=TRUE)
    I<- median(current.long[(t-14):(t- 1),"Sool_CurrentRegion"], na.rm=TRUE)
    J<- conflicts.long[(t- 1),"Nugaal_Conflict"]
    K<- current.long[(t- 1),"Awdal_CurrentRegion"]
    L<- future.long[(t- 1),"Mudug_FutureRegion"]
    M<- before.long[(t- 1),"Bari_BeforeRegion"]
    N<- future.long[(t- 1),"Nugaal_FutureRegion"]
    O<- acosh(M)
    if ( is.na(J) ){P<- 1.05339838945282*L*O}
    else if(J>0){P<- K }
    else{P<- 1.05339838945282*L*O }
    Q<- max(I, P,na.rm=TRUE)
    R<- max(sum(1.05339838945282*B , 0.624305479605904*C , 0.168205574662813*D , 0.00972760096029343*E*G , -H,na.rm=TRUE), Q,na.rm=TRUE)
    S<- max(A, R,na.rm=TRUE)
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( S , -3.39415681899589*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MUJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 9),"Banaadir_Fatalities"]
    B<- before.long[(t- 9),"Togdheer_BeforeRegion"]
    C<- median(before.long[(t-7):(t- 1),"Galgaduud_BeforeRegion"], na.rm=TRUE)
    D<- future.long[(t- 1),"Mudug_FutureRegion"]
    E<- before.long[(t- 1),"Mudug_BeforeRegion"]
    G<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    H<- median(conflicts.long[(t-12):(t- 1),"Bari_Conflict"], na.rm=TRUE)
    I<- current.long[(t- 1),"Awdal_CurrentRegion"]
    J<- rain.long[(t- 10),"Awdal_rain"]
    K<- future.long[(t- 1),"Nugaal_FutureRegion"]
    L<- tail(movavg(before.long[(t-4):(t- 1),"Awdal_BeforeRegion"], 3,type="m"),1)
    M<- atan2(J, 1.09265793207781)
    N<- max(C,sum( 1.09265793207781*D , 0.181537948913199*E , G*H , I*M , -587.178732072463 , -3.77164326832123*K , -8.40304649645697*L,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( A , B , N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MUJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    C<- before.long[(t- 1),"Bari_BeforeRegion"]
    D<- current.long[(t- 1),"Gedo_CurrentRegion"]
    E<- before.long[(t- 1),"Mudug_BeforeRegion"]
    G<- before.long[(t- 1),"Mudug_BeforeRegion"]
    H<- mean(future.long[(t-7):(t- 1),"Nugaal_FutureRegion"], na.rm=TRUE)
    I<- future.long[(t- 1),"Mudug_FutureRegion"]
    J<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    K<- current.long[(t- 1),"Gedo_CurrentRegion"]
    L<- before.long[(t- 1),"Mudug_BeforeRegion"]
    M<- future.long[(t- 1),"Mudug_FutureRegion"]
    N<- future.long[(t- 1),"Nugaal_FutureRegion"]
    O<- E%% 0.13329394483276
    P<- max(D*O, 0.13329394483276*G,na.rm=TRUE)
    Q<- L%% 0.13329394483276
    R<- max(H,sum( 1.06101931052016*I , 0.584337425500737*J , K*Q,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 0.00868664809553655*A*B , C , P , R , -265.587700146552 , -3.98681176172268e-5*M*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MUJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    B<- future.long[(t- 1),"Mudug_FutureRegion"]
    C<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    D<- before.long[(t- 1),"Mudug_BeforeRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    H<- before.long[(t- 8),"Sanaag_BeforeRegion"]
    I<- before.long[(t- 9),"Togdheer_BeforeRegion"]
    J<- tail(movavg(future.long[(t-9):(t- 1),"Sool_FutureRegion"], 8,type="m"),1)
    K<- tail(movavg(rivers.long[(t-7):(t- 1),"Shabelle_River_discharge"],6,type="w"),1)
    L<- mean(fatalities.long[(t-4):(t- 1),"Jubbada_Hoose_Fatalities"], na.rm=TRUE)
    M<- future.long[(t- 1),"Nugaal_FutureRegion"]
    N<- max(0.614932114372036*A,sum( 1.05495439261601*B , 0.614932114372036*C , 0.167578200669237*D , 0.00956361258711489*E*G , H , I , J , -K,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( N , -L , -3.43301192790958*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_MUJUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- stations.long[(t- 5),"Gedo_DollowStation_Juba_River"]
    B<- median(before.long[(t-17):(t- 1),"Galgaduud_BeforeRegion"], na.rm=TRUE)
    C<- fatalities.long[(t- 9),"Banaadir_Fatalities"]
    D<- before.long[(t- 1),"Mudug_BeforeRegion"]
    E<- future.long[(t- 1),"Mudug_FutureRegion"]
    G<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    H<- before.long[(t- 1),"Mudug_BeforeRegion"]
    I<- before.long[(t- 1),"Nugaal_BeforeRegion"]
    J<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    K<- before.long[(t- 1),"Sool_BeforeRegion"]
    L<- future.long[(t- 1),"Mudug_FutureRegion"]
    M<- current.long[(t- 1),"Sool_CurrentRegion"]
    N<- max(0.0800779194446178*D, 0.00271950669257233*E*G,na.rm=TRUE)
    O<- max(0.00166758144357052*H*I, 0.0171409057854938*J^2*K,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( A*B , C , N , O , -0.000323983700711987*L*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}



