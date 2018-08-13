

modelarrivals_LJminus1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rain.long[(t- 4),"Sool_rain"]
    B<- rain.long[(t- 4),"Sool_rain"]
    C<- fatalities.long[(t- 11),"Galguduud_Fatalities"]
    D<- rain.long[(t- 4),"Togdheer_rain"]
    E<- mean(fatalities.long[(t-5):(t- 1),"Galguduud_Fatalities"], na.rm=TRUE)
    G<- median(rain.long[(t-8):(t- 1),"Nugaal_rain"], na.rm=TRUE)
    H<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    I<- median(current.long[(t-3):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    J<- median(rain.long[(t-14):(t- 1),"Bari_rain"], na.rm=TRUE)
    K<- median(current.long[(t-3):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    L<- median(current.long[(t-4):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    M<- median(rain.long[(t-12):(t- 1),"Bari_rain"], na.rm=TRUE)
    N<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    O<- median(current.long[(t-4):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    P<- mean(stations.long[(t-4):(t- 1),"Juba_Dhexe_BualleStation_Juba_River"], na.rm=TRUE)
    Q<- tail(movavg(future.long[(t-5):(t- 1),"Sool_FutureRegion"],4,type="w"),1)
    R<- ceil(G)
    if ( is.na(A) ){S<- H}
    else if(A>0){S<-sum( B*C , D*E*R,na.rm=TRUE) }
    else{S<- H }
    U<- max(S, 796,na.rm=TRUE)
    V<- tan(O)
    W<- max(L*M, 0.681231508291605*N*V,na.rm=TRUE)
    X<- max(K, W,na.rm=TRUE)
    Y<- max(I*J, X,na.rm=TRUE)
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(Y)){Y <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( U , Y , -P*Q,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJ1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- goats.long[(t- 1),"Hiiraan_goatprice"]
    B<- tail(movavg(current.long[(t-17):(t- 1),"Awdal_CurrentRegion"], 16,type="m"),1)
    C<- fatalities.long[(t- 4),"Jubbada_Hoose_Fatalities"]
    D<- conflicts.long[(t- 6),"Mudug_Conflict"]
    E<- current.long[(t- 10),"Sool_CurrentRegion"]
    G<- fatalities.long[(t- 4),"Jubbada_Hoose_Fatalities"]
    H<- median(before.long[(t-5):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    I<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    J<- tail(movavg(rain.long[(t-11):(t- 1),"Hiiraan_rain"],10,type="w"),1)
    K<- tail(movavg(current.long[(t-17):(t- 1),"Awdal_CurrentRegion"], 16,type="m"),1)
    L<- rivers.long[(t- 1),"Shabelle_River_discharge"]
    M<- cos(6.91275795924166*K)
    N<- max(sum(0.00322010393577389*A , 6.91275795924166*B , C*D , E , -G , -17.2546295934327*H,na.rm=TRUE), I*J*M,na.rm=TRUE)
    O<- max(3881.65208700582, N,na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( O , -2119.86717835761 , -4.99659626779389*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJ2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Banadir_BeforeRegion"]
    B<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    C<- before.long[(t- 14),"Togdheer_BeforeRegion"]
    D<- mean(fatalities.long[(t-6):(t- 1),"Sanaag_Fatalities"], na.rm=TRUE)
    E<- mean(stations.long[(t-6):(t- 1),"Juba_Dhexe_BualleStation_Juba_River"], na.rm=TRUE)
    G<- water.long[(t- 3),"Nugaal_WaterDrumPrice"]
    H<- current.long[(t- 15),"Awdal_CurrentRegion"]
    I<- fatalities.long[(t- 1),"Sool_Fatalities"]
    J<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    K<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    L<- tail(movavg(current.long[(t-8):(t- 1),"Gedo_CurrentRegion"], 7,type="m"),1)
    M<- median(stations.long[(t-17):(t- 1),"Hiiraan_Belet_WeyneStation_Shabelle_River"], na.rm=TRUE)
    N<- max(0.304040139329624*I*J, 0.000178462844661094*K*L,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 3.01753274610821e-9*A^2*B , C*D*E , G , H , N , -1139.82730742*M
               ,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJ3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(rain.long[(t-3):(t- 1),"Jubbada_Hoose_rain"], na.rm=TRUE)
    B<- median(current.long[(t-11):(t- 1),"Gedo_CurrentRegion"], na.rm=TRUE)
    C<- conflicts.long[(t- 1),"Awdal_Conflict"]
    D<- current.long[(t- 7),"Nugaal_CurrentRegion"]
    E<- fatalities.long[(t- 1),"Hiiraan_Fatalities"]
    G<- rain.long[(t- 7),"Bari_rain"]
    H<- current.long[(t- 1),"Gedo_CurrentRegion"]
    I<- tail(movavg(current.long[(t-5):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], 4,type="m"),1)
    J<- fatalities.long[(t- 2),"Hiiraan_Fatalities"]
    K<- fatalities.long[(t- 5),"Galguduud_Fatalities"]
    L<- mean(future.long[(t-4):(t- 1),"Bakool_FutureRegion"], na.rm=TRUE)
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
    FIN <-sum( 459.416711798037*A , 1.18729168460161*B , C*D , E*G , 0.000152719826036252*H*I , 0.9075725779165*J*K , L , -7751.54721336723,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJ4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Sool_Fatalities"]
    B<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    C<- mean(conflicts.long[(t-4):(t- 1),"Sool_Conflict"], na.rm=TRUE)
    D<- median(before.long[(t-17):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    E<- current.long[(t- 1),"Gedo_CurrentRegion"]
    G<- tail(movavg(current.long[(t-5):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], 4,type="m"),1)
    H<- current.long[(t- 1),"Shabeellaha_Dhexe_CurrentRegion"]
    I<- rain.long[(t- 1),"Awdal_rain"]
    J<- fatalities.long[(t- 1),"Bari_Fatalities"]
    K<- fatalities.long[(t- 1),"Sool_Fatalities"]
    L<- rain.long[(t- 1),"Jubbada_Dhexe_rain"]
    M<- median(fatalities.long[(t-3):(t- 1),"Sanaag_Fatalities"], na.rm=TRUE)
    N<- mean(future.long[(t-6):(t- 1),"Galgaduud_FutureRegion"], na.rm=TRUE)
    O<- median(before.long[(t-11):(t- 1),"Hiiraan_BeforeRegion"], na.rm=TRUE)
    P<- xor(I, J*K*L*M)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 0.294523151104974*A*B , C*D , 0.000142643977785955*E*G , 0.840187963507452*H*P , N , -O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJ5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(current.long[(t-10):(t- 1),"Hiiraan_CurrentRegion"], na.rm=TRUE)
    B<- rain.long[(t- 1),"Sanaag_rain"]
    C<- median(before.long[(t-6):(t- 1),"Jubbada_Hoose_BeforeRegion"], na.rm=TRUE)
    D<- median(conflicts.long[(t-4):(t- 1),"Sool_Conflict"], na.rm=TRUE)
    E<- median(before.long[(t-10):(t- 1),"Jubbada_Dhexe_BeforeRegion"], na.rm=TRUE)
    G<- fatalities.long[(t- 5),"Gedo_Fatalities"]
    H<- mean(rain.long[(t-4):(t- 1),"Sool_rain"], na.rm=TRUE)
    I<- fatalities.long[(t- 1),"Sool_Fatalities"]
    J<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    K<- before.long[(t- 1),"Banadir_BeforeRegion"]
    L<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    M<- tail(movavg(current.long[(t-3):(t- 1),"Woqooyi_Galbeed_CurrentRegion"],2,type="w"),1)
    N<- tail(movavg(before.long[(t-4):(t- 1),"Bari_BeforeRegion"],3,type="w"),1)
    if ( is.na(B) ){O<- -162.860811781461}
    else if(B>0){O<- C }
    else{O<- -162.860811781461 }
    P<- atan2(G, H)
    Q<- max(1.66373092751397*D*E*P, 0.330331450499903*I*J,na.rm=TRUE)
    R<- max(sum(A , O , Q,na.rm=TRUE), 6.09895696332174e-5*K*L,na.rm=TRUE)
    S<- max(R, M,na.rm=TRUE)
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( S , -N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJ6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 7),"Nugaal_CurrentRegion"]
    B<- median(conflicts.long[(t-7):(t- 1),"Togdheer_Conflict"], na.rm=TRUE)
    C<- median(before.long[(t-17):(t- 1),"Hiiraan_BeforeRegion"], na.rm=TRUE)
    D<- tail(movavg(current.long[(t-4):(t- 1),"Hiiraan_CurrentRegion"],3,type="w"),1)
    E<- current.long[(t- 11),"Nugaal_CurrentRegion"]
    G<- fatalities.long[(t- 1),"Sool_Fatalities"]
    H<- tail(movavg(future.long[(t-4):(t- 1),"Galgaduud_FutureRegion"], 3,type="m"),1)
    I<- mean(rain.long[(t-12):(t- 1),"Bay_rain"], na.rm=TRUE)
    J<- median(rain.long[(t-10):(t- 1),"Bay_rain"], na.rm=TRUE)
    K<- conflicts.long[(t- 14),"Galgaduud_Conflict"]
    L<- tail(movavg(rain.long[(t-3):(t- 1),"Shabeellaha_Dhexe_rain"],2,type="w"),1)
    M<- before.long[(t- 1),"Banadir_BeforeRegion"]
    N<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    O<- tail(movavg(current.long[(t-3):(t- 1),"Woqooyi_Galbeed_CurrentRegion"],2,type="w"),1)
    P<- log(D)
    Q<- max(0.330162338655957*G*H, I*J,na.rm=TRUE)
    R<- max(E, Q,na.rm=TRUE)
    S<- max(sum(C*P , R , -K*L,na.rm=TRUE), 6.04475718368644e-5*M*N,na.rm=TRUE)
    U<- max(A*B, S,na.rm=TRUE)
    V<- max(U, O,na.rm=TRUE)
    FIN <-V
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJ7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Gedo_CurrentRegion"]
    B<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    C<- tail(movavg(before.long[(t-3):(t- 1),"Shabeellaha_Dhexe_BeforeRegion"], 2,type="m"),1)
    D<- median(fatalities.long[(t-14):(t- 1),"Sanaag_Fatalities"], na.rm=TRUE)
    E<- current.long[(t- 11),"Awdal_CurrentRegion"]
    G<- fatalities.long[(t- 14),"Sanaag_Fatalities"]
    H<- rain.long[(t- 11),"Hiiraan_rain"]
    I<- fatalities.long[(t- 14),"Sanaag_Fatalities"]
    J<- water.long[(t- 11),"Togdheer_WaterDrumPrice"]
    K<- stations.long[(t- 1),"Gedo_BardheereStation_Juba_River"]
    L<- fatalities.long[(t- 1),"Sool_Fatalities"]
    M<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    N<- median(fatalities.long[(t-14):(t- 1),"Sanaag_Fatalities"], na.rm=TRUE)
    O<- fatalities.long[(t- 1),"Sool_Fatalities"]
    P<- future.long[(t- 2),"Sool_FutureRegion"]
    Q<- rivers.long[(t- 10),"Shabelle_River_discharge"]
    R<- atan2(H, I)
    S<- max(J/K, L*M*N,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( 0.000119670854441919*A*B , C*D , 1.74072019056672*E*G*R , S , -O , -P , -Q,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJ8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Awdal_Conflict"]
    B<- before.long[(t- 11),"Sanaag_BeforeRegion"]
    C<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    D<- rain.long[(t- 11),"Shabeellaha_Dhexe_rain"]
    E<- median(rain.long[(t-7):(t- 1),"Bari_rain"], na.rm=TRUE)
    G<- rain.long[(t- 9),"Bakool_rain"]
    H<- tail(movavg(future.long[(t-8):(t- 1),"Galgaduud_FutureRegion"],7,type="w"),1)
    I<- future.long[(t- 1),"Togdheer_FutureRegion"]
    J<- rain.long[(t- 9),"Bakool_rain"]
    K<- mean(rain.long[(t-10):(t- 1),"Bari_rain"], na.rm=TRUE)
    L<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    M<- conflicts.long[(t- 2),"Jubbada_Dhexe_Conflict"]
    N<- future.long[(t- 11),"Galgaduud_FutureRegion"]
    O<- median(current.long[(t-11):(t- 1),"Gedo_CurrentRegion"], na.rm=TRUE)
    P<- max(I, J*K,na.rm=TRUE)
    Q<- max(sum(0.606757439691736*L , M*N,na.rm=TRUE), O,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( 1.95263185850796*A*B , C*D*E , G , H , P , Q , -328.786062457441,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJ9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Togdheer_FutureRegion"]
    B<- median(current.long[(t-10):(t- 1),"Hiiraan_CurrentRegion"], na.rm=TRUE)
    C<- tail(movavg(current.long[(t-3):(t- 1),"Woqooyi_Galbeed_CurrentRegion"],2,type="w"),1)
    D<- fatalities.long[(t- 1),"Sool_Fatalities"]
    E<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    G<- before.long[(t- 1),"Banadir_BeforeRegion"]
    H<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    I<- rain.long[(t- 9),"Bakool_rain"]
    J<- tail(movavg(rain.long[(t-9):(t- 1),"Sool_rain"],8,type="w"),1)
    K<- median(conflicts.long[(t-11):(t- 1),"Gedo_Conflict"], na.rm=TRUE)
    L<- median(before.long[(t-17):(t- 1),"Hiiraan_BeforeRegion"], na.rm=TRUE)
    M<- median(before.long[(t-14):(t- 1),"Shabeellaha_Dhexe_BeforeRegion"], na.rm=TRUE)
    N<- max(C,sum( 0.381252890085792*D*E , 6.07165964780916e-5*G*H,na.rm=TRUE),na.rm=TRUE)
    O<- max(N,sum( I*J , 0.869238135182971*K*L,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( A , B , O , -3.06714050467996*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJ10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(current.long[(t-3):(t- 1),"Woqooyi_Galbeed_CurrentRegion"],2,type="w"),1)
    B<- conflicts.long[(t- 1),"Bari_Conflict"]
    C<- median(conflicts.long[(t-4):(t- 1),"Nugaal_Conflict"], na.rm=TRUE)
    D<- stations.long[(t- 2),"Juba_Dhexe_BualleStation_Juba_River"]
    E<- rain.long[(t- 3),"Shabeellaha_Dhexe_rain"]
    G<- mean(fatalities.long[(t-4):(t- 1),"Sanaag_Fatalities"], na.rm=TRUE)
    H<- current.long[(t- 10),"Sool_CurrentRegion"]
    I<- current.long[(t- 7),"Nugaal_CurrentRegion"]
    J<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    K<- median(fatalities.long[(t-3):(t- 1),"Sool_Fatalities"], na.rm=TRUE)
    L<- water.long[(t- 11),"Togdheer_WaterDrumPrice"]
    M<- stations.long[(t- 1),"Gedo_BardheereStation_Juba_River"]
    N<- rivers.long[(t- 10),"Shabelle_River_discharge"]
    O<- median(before.long[(t-5):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    P<- max(2.82338650763823*I, 0.494731580020056*J*K,na.rm=TRUE)
    Q<- max(sum(H , P,na.rm=TRUE), L/M,na.rm=TRUE)
    R<- max(A,sum( B^2*C , D*E*G , Q,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( R , -N , -O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    B<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    C<- rain.long[(t- 1),"Awdal_rain"]
    D<- conflicts.long[(t- 1),"Bay_Conflict"]
    E<- fatalities.long[(t- 1),"Bari_Fatalities"]
    G<- median(stations.long[(t-4):(t- 1),"Hiiraan_Bulo_Burti_StationShabelle_River"], na.rm=TRUE)
    H<- current.long[(t- 17),"Nugaal_CurrentRegion"]
    I<- median(before.long[(t-10):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    J<- conflicts.long[(t- 3),"Bay_Conflict"]
    K<- goats.long[(t- 1),"Hiiraan_goatprice"]
    L<- current.long[(t- 1),"Awdal_CurrentRegion"]
    M<- tail(movavg(current.long[(t-5):(t- 1),"Hiiraan_CurrentRegion"],4,type="w"),1)
    N<- fatalities.long[(t- 1),"Bari_Fatalities"]
    O<- mean(rain.long[(t-4):(t- 1),"Awdal_rain"], na.rm=TRUE)
    P<- before.long[(t- 1),"Shabeellaha_Hoose_BeforeRegion"]
    Q<- current.long[(t- 1),"Awdal_CurrentRegion"]
    if ( is.na(J) ){R<- M}
    else if(J>0){R<- 5.03026963301009e-6*K*L }
    else{R<- M }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( A*B*C , D*E*G , H , I , R , -N*O , -0.000115842576271141*P*Q,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rain.long[(t- 1),"Jubbada_Hoose_rain"]
    B<- current.long[(t- 7),"Nugaal_CurrentRegion"]
    C<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    D<- mean(conflicts.long[(t-10):(t- 1),"Nugaal_Conflict"], na.rm=TRUE)
    E<- median(fatalities.long[(t-6):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    G<- stations.long[(t- 5),"Hiiraan_Bulo_Burti_StationShabelle_River"]
    H<- median(current.long[(t-8):(t- 1),"Nugaal_CurrentRegion"], na.rm=TRUE)
    I<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    J<- before.long[(t- 1),"Banadir_BeforeRegion"]
    K<- fatalities.long[(t- 1),"Sool_Fatalities"]
    L<- future.long[(t- 1),"Galgaduud_FutureRegion"]
    M<- mean(conflicts.long[(t-17):(t- 1),"Shabeellaha_Hoose_Conflict"], na.rm=TRUE)
    N<- median(future.long[(t-11):(t- 1),"Sool_FutureRegion"], na.rm=TRUE)
    O<- sin(1.00393015951607*J)
    P<- max(sum(G*H , I*O,na.rm=TRUE), 0.315214572498863*K*L,na.rm=TRUE)
    Q<- max(C*D*E, P,na.rm=TRUE)
    R<- max(B, Q,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 175.618213833921*A , R , -M*N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- goats.long[(t- 1),"Hiiraan_goatprice"]
    B<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    C<- rain.long[(t- 1),"Awdal_rain"]
    D<- median(conflicts.long[(t-4):(t- 1),"Togdheer_Conflict"], na.rm=TRUE)
    E<- before.long[(t- 10),"Sool_BeforeRegion"]
    G<- median(conflicts.long[(t-4):(t- 1),"Togdheer_Conflict"], na.rm=TRUE)
    H<- tail(movavg(current.long[(t-5):(t- 1),"Awdal_CurrentRegion"], 4,type="m"),1)
    I<- rain.long[(t- 1),"Awdal_rain"]
    J<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    K<- current.long[(t- 3),"Bari_CurrentRegion"]
    L<- median(conflicts.long[(t-4):(t- 1),"Togdheer_Conflict"], na.rm=TRUE)
    M<- mean(rain.long[(t-5):(t- 1),"Jubbada_Dhexe_rain"], na.rm=TRUE)
    N<- median(rain.long[(t-10):(t- 1),"Jubbada_Dhexe_rain"], na.rm=TRUE)
    O<- before.long[(t- 1),"Awdal_BeforeRegion"]
    P<- tail(movavg(fatalities.long[(t-4):(t- 1),"Sool_Fatalities"],3,type="w"),1)
    Q<- tail(movavg(rivers.long[(t-4):(t- 1),"Shabelle_River_discharge"],3,type="w"),1)
    if ( is.na(G) ){R<- I}
    else if(G>0){R<- 3.04913528221951*H }
    else{R<- I }
    S<- atan2(L, 0.00195618800744056)
    if ( is.na(J) ){U<- M*N}
    else if(J>0){U<- K*S }
    else{U<- M*N }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( 0.00195618800744056*A , B*C*D , E , R , U , -O*P , -7.61801106894457*Q,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- stations.long[(t- 10),"Gedo_LuuqStation_Juba_River"]
    C<- before.long[(t- 15),"Woqooyi_Galbeed_BeforeRegion"]
    D<- conflicts.long[(t- 16),"Hiiraan_Conflict"]
    E<- mean(current.long[(t-10):(t- 1),"Jubbada_Dhexe_CurrentRegion"], na.rm=TRUE)
    G<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    H<- future.long[(t- 9),"Jubbada_Dhexe_FutureRegion"]
    I<- current.long[(t- 1),"Awdal_CurrentRegion"]
    J<- conflicts.long[(t- 1),"Woqooyi_Galbeed_Conflict"]
    K<- goats.long[(t- 1),"Bay_goatprice"]
    L<- stations.long[(t- 10),"Gedo_LuuqStation_Juba_River"]
    M<- rivers.long[(t- 1),"Shabelle_River_discharge"]
    N<- conflicts.long[(t- 2),"Jubbada_Hoose_Conflict"]
    O<- goats.long[(t- 1),"Togdheer_goatprice"]
    P<- mean(fatalities.long[(t-8):(t- 1),"Bakool_Fatalities"], na.rm=TRUE)
    Q<- tail(movavg(before.long[(t-4):(t- 1),"Awdal_BeforeRegion"], 3,type="m"),1)
    if ( is.na(J) ){R<- L}
    else if(J>0){R<- 0.00955933522723327*K }
    else{R<- L }
    S<- max(sum(A*B , C*D , E,na.rm=TRUE),sum( G*H , I , R , -M*N , -0.0251374709254543*O,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( S , -P*Q,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 17),"Sool_BeforeRegion"]
    B<- tail(movavg(fatalities.long[(t-5):(t- 1),"Sool_Fatalities"],4,type="w"),1)
    C<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    D<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    E<- median(before.long[(t-10):(t- 1),"Jubbada_Dhexe_BeforeRegion"], na.rm=TRUE)
    G<- median(conflicts.long[(t-14):(t- 1),"Gedo_Conflict"], na.rm=TRUE)
    H<- before.long[(t- 8),"Shabeellaha_Dhexe_BeforeRegion"]
    I<- conflicts.long[(t- 1),"Shabeellaha_Hoose_Conflict"]
    J<- current.long[(t- 10),"Sool_CurrentRegion"]
    K<- rain.long[(t- 1),"Sanaag_rain"]
    L<- median(rain.long[(t-4):(t- 1),"Shabeellaha_Hoose_rain"], na.rm=TRUE)
    M<- before.long[(t- 17),"Sool_BeforeRegion"]
    N<- tail(movavg(stations.long[(t-3):(t- 1),"Juba_Dhexe_BualleStation_Juba_River"],2,type="w"),1)
    O<- median(before.long[(t-4):(t- 1),"Woqooyi_Galbeed_BeforeRegion"], na.rm=TRUE)
    P<- max(0.802859558451015*J, K*L,na.rm=TRUE)
    Q<- max(A*B,sum( 1.24863056389723*C*D , E*G , 2.28567472064033*H/I , P , -M*N,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( Q , -O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- mean(before.long[(t-15):(t- 1),"Nugaal_BeforeRegion"], na.rm=TRUE)
    B<- median(rain.long[(t-12):(t- 1),"Shabeellaha_Dhexe_rain"], na.rm=TRUE)
    C<- mean(before.long[(t-15):(t- 1),"Nugaal_BeforeRegion"], na.rm=TRUE)
    D<- median(rain.long[(t-12):(t- 1),"Shabeellaha_Dhexe_rain"], na.rm=TRUE)
    E<- water.long[(t- 7),"Togdheer_WaterDrumPrice"]
    G<- before.long[(t- 8),"Shabeellaha_Dhexe_BeforeRegion"]
    H<- before.long[(t- 10),"Jubbada_Hoose_BeforeRegion"]
    I<- mean(future.long[(t-13):(t- 1),"Shabeellaha_Hoose_FutureRegion"], na.rm=TRUE)
    J<- median(fatalities.long[(t-16):(t- 1),"Bari_Fatalities"], na.rm=TRUE)
    K<- before.long[(t- 12),"Bay_BeforeRegion"]
    L<- future.long[(t- 11),"Galgaduud_FutureRegion"]
    M<- tail(movavg(stations.long[(t-4):(t- 1),"Gedo_LuuqStation_Juba_River"],3,type="w"),1)
    N<- tail(movavg(conflicts.long[(t-3):(t- 1),"Shabeellaha_Dhexe_Conflict"], 2,type="m"),1)
    O<- before.long[(t- 11),"Banadir_BeforeRegion"]
    P<- stations.long[(t- 11),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
    Q<- mean(future.long[(t-10):(t- 1),"Shabeellaha_Hoose_FutureRegion"], na.rm=TRUE)
    R<- stations.long[(t- 1),"Gedo_BardheereStation_Juba_River"]
    S<- max(sum(C*D , E , G , H , -I*J,na.rm=TRUE),sum( 1.93497351844963*K , L*M*N , -O*P,na.rm=TRUE),na.rm=TRUE)
    U<- max(S, Q,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( A*B , U/R,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rain.long[(t- 6),"Banaadir_rain"]
    B<- median(fatalities.long[(t-8):(t- 1),"Bay_Fatalities"], na.rm=TRUE)
    C<- tail(movavg(current.long[(t-4):(t- 1),"Awdal_CurrentRegion"], 3,type="m"),1)
    D<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    E<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    G<- rain.long[(t- 1),"Awdal_rain"]
    H<- mean(stations.long[(t-6):(t- 1),"Gedo_LuuqStation_Juba_River"], na.rm=TRUE)
    I<- tail(movavg(current.long[(t-4):(t- 1),"Nugaal_CurrentRegion"],3,type="w"),1)
    J<- median(before.long[(t-6):(t- 1),"Jubbada_Hoose_BeforeRegion"], na.rm=TRUE)
    K<- median(future.long[(t-3):(t- 1),"Nugaal_FutureRegion"], na.rm=TRUE)
    L<- fatalities.long[(t- 1),"Awdal_Fatalities"]
    M<- future.long[(t- 1),"Bay_FutureRegion"]
    N<- before.long[(t- 1),"Shabeellaha_Hoose_BeforeRegion"]
    O<- tail(movavg(current.long[(t-4):(t- 1),"Awdal_CurrentRegion"], 3,type="m"),1)
    P<- max(sum(3.71541515153775*C , D*E*G , H*I , J , -K,na.rm=TRUE), 2.40226987142885*L*M,na.rm=TRUE)
    Q<- max(A*B, P,na.rm=TRUE)
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( Q , -0.000123173431833836*N*O,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- goats.long[(t- 1),"Bay_goatprice"]
    B<- conflicts.long[(t- 1),"Gedo_Conflict"]
    C<- rain.long[(t- 5),"Woqooyi_Galbeed_rain"]
    D<- rain.long[(t- 9),"Bakool_rain"]
    E<- median(rain.long[(t-17):(t- 1),"Woqooyi_Galbeed_rain"], na.rm=TRUE)
    G<- current.long[(t- 10),"Sool_CurrentRegion"]
    H<- conflicts.long[(t- 1),"Awdal_Conflict"]
    I<- before.long[(t- 17),"Sool_BeforeRegion"]
    J<- tail(movavg(conflicts.long[(t-5):(t- 1),"Sool_Conflict"], 4,type="m"),1)
    K<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    L<- goats.long[(t- 1),"Bari_goatprice"]
    M<- tail(movavg(conflicts.long[(t-5):(t- 1),"Shabeellaha_Hoose_Conflict"], 4,type="m"),1)
    N<- asinh(J)
    O<- max(6682, K,na.rm=TRUE)
    P<- max(H*I*N, O,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 0.00322049508009936*A , B*C , D*E , G , P , -0.00504644513709665*L , -36.7863731063603*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 3),"Shabeellaha_Hoose_Conflict"]
    B<- median(fatalities.long[(t-10):(t- 1),"Bay_Fatalities"], na.rm=TRUE)
    C<- rain.long[(t- 5),"Bay_rain"]
    D<- fatalities.long[(t- 7),"Togdheer_Fatalities"]
    E<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    G<- tail(movavg(conflicts.long[(t-3):(t- 1),"Gedo_Conflict"],2,type="w"),1)
    H<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    I<- tail(movavg(current.long[(t-3):(t- 1),"Woqooyi_Galbeed_CurrentRegion"],2,type="w"),1)
    J<- median(before.long[(t-17):(t- 1),"Hiiraan_BeforeRegion"], na.rm=TRUE)
    K<- rain.long[(t- 5),"Woqooyi_Galbeed_rain"]
    L<- mean(fatalities.long[(t-5):(t- 1),"Galguduud_Fatalities"], na.rm=TRUE)
    M<- rain.long[(t- 6),"Shabeellaha_Dhexe_rain"]
    N<- mean(fatalities.long[(t-10):(t- 1),"Sool_Fatalities"], na.rm=TRUE)
    O<- median(fatalities.long[(t-17):(t- 1),"Sool_Fatalities"], na.rm=TRUE)
    P<- conflicts.long[(t- 3),"Shabeellaha_Hoose_Conflict"]
    Q<- max(A*B, C*D,na.rm=TRUE)
    R<- max(I, 9.36406661543063*J,na.rm=TRUE)
    S<- max(E*G/H, R,na.rm=TRUE)
    U<- max(sum(K*L , M*N*O,na.rm=TRUE), P,na.rm=TRUE)
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(S)){S <- 0 }
    if(is.infinite(U)){U <- 0 }
    FIN <-sum( Q , S , U , -1796.93556600875,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_LJJUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- goats.long[(t- 1),"Hiiraan_goatprice"]
    B<- current.long[(t- 1),"Awdal_CurrentRegion"]
    C<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    D<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    E<- rain.long[(t- 1),"Awdal_rain"]
    G<- stations.long[(t- 4),"Gedo_LuuqStation_Juba_River"]
    H<- mean(current.long[(t-6):(t- 1),"Nugaal_CurrentRegion"], na.rm=TRUE)
    I<- fatalities.long[(t- 1),"Bari_Fatalities"]
    J<- rain.long[(t- 4),"Bakool_rain"]
    K<- fatalities.long[(t- 1),"Bari_Fatalities"]
    L<- rain.long[(t- 6),"Bakool_rain"]
    M<- conflicts.long[(t- 13),"Nugaal_Conflict"]
    N<- mean(current.long[(t-6):(t- 1),"Nugaal_CurrentRegion"], na.rm=TRUE)
    O<- before.long[(t- 1),"Shabeellaha_Hoose_BeforeRegion"]
    P<- current.long[(t- 1),"Awdal_CurrentRegion"]
    Q<- max(sum(5.24210156458818e-6*A*B , C*D*E , G*H , 1.30522313265882*I*J,na.rm=TRUE),sum( K*L*M , N,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( Q , -0.000125394244083141*O*P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

