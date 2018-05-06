

modelarrivals_SAminus1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 17),"Hiiraan_BeforeRegion"]
    B<- conflicts.long[(t- 7),"Shabeellaha_Hoose_Conflict"]
    C<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    D<- conflicts.long[(t- 7),"Shabeellaha_Hoose_Conflict"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    H<- rain.long[(t- 1),"Mudug_rain"]
    I<- median(future.long[(t-4):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    J<- before.long[(t- 1),"Gedo_BeforeRegion"]
    K<- cos(5.99821960756545*D)
    L<- max(C*K,sum( 0.00131211529395988*E*G , -2434.81207995507,na.rm=TRUE),na.rm=TRUE)
    M<- max(sum(0.0829279817375132*A , 0.0364016149047691*B*L , -157.2763587529*H,na.rm=TRUE), I,na.rm=TRUE)
    N<- max(0.0829279817375132,sum( M , -J,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(N)){N <- 0 }
    FIN <-sum( 125.91707188375 , N,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SAminus2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 2),"Gedo_BeforeRegion"]
    B<- current.long[(t- 2),"Woqooyi_Galbeed_CurrentRegion"]
    C<- stations.long[(t- 2),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
    D<- before.long[(t- 2),"Gedo_BeforeRegion"]
    E<- stations.long[(t- 2),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
    G<- current.long[(t- 2),"Woqooyi_Galbeed_CurrentRegion"]
    H<- before.long[(t- 2),"Sool_BeforeRegion"]
    I<- future.long[(t- 2),"Shabeellaha_Hoose_FutureRegion"]
    J<- mean(future.long[(t-5):(t- 2),"Shabeellaha_Hoose_FutureRegion"], na.rm=TRUE)
    K<- median(before.long[(t-7):(t- 2),"Jubbada_Hoose_BeforeRegion"], na.rm=TRUE)
    L<- median(before.long[(t-6):(t- 2),"Bakool_BeforeRegion"], na.rm=TRUE)
    M<- rain.long[(t- 2),"Sool_rain"]
    N<- erf(2.384613927151e-12*D^2*E*G)
    O<- asinh(sum(J*K , -5954.18494543878 , -L,na.rm=TRUE))
    P<- max(126,sum( 0.344332355046033*H , -I*O , -446.06751417352*M,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( 2.384613927151e-12*A^2*B^2*C*N , P,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SA1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- conflicts.long[(t- 1),"Bari_Conflict"]
    B<- future.long[(t- 1),"Gedo_FutureRegion"]
    C<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- before.long[(t- 1),"Sool_BeforeRegion"]
    G<- before.long[(t- 1),"Sool_BeforeRegion"]
    H<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    I<- before.long[(t- 1),"Bari_BeforeRegion"]
    J<- before.long[(t- 1),"Shabeellaha_Hoose_BeforeRegion"]
    K<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    L<- log(B)
    M<- xor(A, L)
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 134.951645053883 , 3472.70286600382*M , 2.21150357444172e-11*C^2*D*E , -6.2652996089654e-5*G*H , -6.578675626779e-9*I*J*K,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SA2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Galgaduud_BeforeRegion"]
    B<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    C<- tail(movavg(current.long[(t-3):(t- 1),"Bay_CurrentRegion"], 2,type="m"),1)
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- future.long[(t- 1),"Bari_FutureRegion"]
    G<- future.long[(t- 1),"Togdheer_FutureRegion"]
    H<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    I<- rain.long[(t- 10),"Mudug_rain"]
    J<- median(current.long[(t-12):(t- 1),"Togdheer_CurrentRegion"], na.rm=TRUE)
    K<- tail(movavg(current.long[(t-3):(t- 1),"Bakool_CurrentRegion"], 2,type="m"),1)
    L<- fatalities.long[(t- 1),"Shabeellaha_Dhexe_Fatalities"]
    M<- rain.long[(t- 10),"Mudug_rain"]
    N<- max(0.145452965941785*K, L*M,na.rm=TRUE)
    O<- max(126,sum( 4.0071261040803e-9*A*B*C , 4.5473706438896e-8*D*E*G*H , I , J , N , -1057.66177332745,na.rm=TRUE),na.rm=TRUE)
    FIN <-O
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SA3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    B<- before.long[(t- 1),"Bari_BeforeRegion"]
    C<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    D<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    E<- median(current.long[(t-12):(t- 1),"Togdheer_CurrentRegion"], na.rm=TRUE)
    if ( is.na(162.903641074268) || is.na( D)){G<-0}
    else if(162.903641074268<= D){G<-1 }
    else{G<-0 }
    H<- max(sum(126 , 5.71342966187121e-11*A^2*B*C*G,na.rm=TRUE), E,na.rm=TRUE)
    FIN <-H
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SA4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    B<- median(before.long[(t-3):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    C<- fatalities.long[(t- 2),"Bari_Fatalities"]
    D<- future.long[(t- 16),"Sool_FutureRegion"]
    E<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    G<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    H<- before.long[(t- 1),"Bari_BeforeRegion"]
    I<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    J<- median(current.long[(t-7):(t- 1),"Bakool_CurrentRegion"], na.rm=TRUE)
    K<- max(138.304084670738,sum( 0.700519661357139*D , -21.7650791173914,na.rm=TRUE),na.rm=TRUE)
    L<- max(C, K,na.rm=TRUE)
    if ( is.na(E) || is.na( 2851.44043032364)){M<-0}
    else if(E>= 2851.44043032364){M<-1 }
    else{M<-0 }
    if ( is.na(I) || is.na( J)){N<-0}
    else if(I> J){N<-1 }
    else{N<-0 }
    O<- max(3648*M, 9.50025012902851e-8*G^2*H*N,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( A/B , 21.7650791173914*L , O , -2884.30155792815,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SA5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- rain.long[(t- 5),"Bay_rain"]
    B<- before.long[(t- 1),"Bari_BeforeRegion"]
    C<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    D<- future.long[(t- 2),"Mudug_FutureRegion"]
    E<- tail(movavg(before.long[(t-7):(t- 1),"Sanaag_BeforeRegion"], 6,type="m"),1)
    G<- before.long[(t- 1),"Awdal_BeforeRegion"]
    H<- future.long[(t- 1),"Woqooyi_Galbeed_FutureRegion"]
    I<- mean(future.long[(t-4):(t- 1),"Sanaag_FutureRegion"], na.rm=TRUE)
    J<- median(future.long[(t-4):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    K<- rain.long[(t- 1),"Togdheer_rain"]
    L<- before.long[(t- 1),"Awdal_BeforeRegion"]
    if(is.na(D)){M<-0}
    else {M<- gauss(D)}
    N<- max(sum(0.00158667385521141*B*C , -4204.54829528962,na.rm=TRUE), 7812.34197630949*M,na.rm=TRUE)
    O<- max(I, J,na.rm=TRUE)
    P<- log(L)
    Q<- max(126,sum( 0.00115359065275708*E*G , H , O , -K*P,na.rm=TRUE),na.rm=TRUE)
    if ( is.na(A) ){R<- 126}
    else if(A>0){R<-sum( N , Q,na.rm=TRUE) }
    else{R<- 126 }
    FIN <-R
      PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SA6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- tail(movavg(future.long[(t-9):(t- 1),"Galgaduud_FutureRegion"],8,type="w"),1)
    B<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    C<- tail(movavg(fatalities.long[(t-3):(t- 1),"Sool_Fatalities"], 2,type="m"),1)
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    G<- mean(fatalities.long[(t-6):(t- 1),"Mudug_Fatalities"], na.rm=TRUE)
    H<- tail(movavg(fatalities.long[(t-3):(t- 1),"Sool_Fatalities"], 2,type="m"),1)
    I<- fatalities.long[(t- 1),"Sanaag_Fatalities"]
    J<- before.long[(t- 1),"Bari_BeforeRegion"]
    K<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    L<- mean(future.long[(t-12):(t- 1),"Shabeallaha_Dhexe_FutureRegion"], na.rm=TRUE)
    M<- tail(movavg(future.long[(t-4):(t- 1),"Woqooyi_Galbeed_FutureRegion"], 3,type="m"),1)
    N<- future.long[(t- 9),"Nugaal_FutureRegion"]
    if ( is.na(A) || is.na( B)){O<-0}
    else if(A<= B){O<-1 }
    else{O<-0 }
    P<- (1.79389144404674e-5*J)
    if ( is.na(C) ){Q<- 1.79389144404674e-5}
    else if(C>0){Q<-sum( 0.00144190326623957*D*E , G*H , I^P*K , L , M , -772.056141206425,na.rm=TRUE) }
    else{Q<- 1.79389144404674e-5 }
    R<- max(126,sum( O*Q , -16.0003099122913*N,na.rm=TRUE),na.rm=TRUE)
    FIN <-R
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SA7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- mean(current.long[(t-12):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    B<- median(future.long[(t-4):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    C<- mean(current.long[(t-12):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    D<- median(future.long[(t-4):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    E<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    G<- stations.long[(t- 16),"Gedo_DollowStation_Juba_River"]
    H<- rain.long[(t- 3),"Shabeellaha_Dhexe_rain"]
    I<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    J<- before.long[(t- 1),"Bari_BeforeRegion"]
    K<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    L<- A%% B
    M<- C%% D
    N<- max(M,sum( 0.600244694306745*E , 2258.26368002775*G , 2.31191439442237*H , 8.86012789206053e-12*I^3*J , -8133.23172426654 , -4.41297958025712e-5*K^2,na.rm=TRUE),na.rm=TRUE)
    O<- max(126,sum( L , N,na.rm=TRUE),na.rm=TRUE)
    FIN <-O
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SA8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    B<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    C<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    D<- median(future.long[(t-9):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    E<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    G<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    H<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    I<- median(future.long[(t-9):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    J<- median(future.long[(t-4):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    K<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    L<- before.long[(t- 1),"Bari_BeforeRegion"]
    M<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    N<- median(future.long[(t-4):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    O<- fatalities.long[(t- 1),"Bakool_Fatalities"]
    P<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    Q<- fatalities.long[(t- 1),"Jubbada_Dhexe_Fatalities"]
    if ( is.na(C) ){R<- 0.171238866197208}
    else if(C>0){R<- 0.81218442210766 }
    else{R<- 0.171238866197208 }
    S<- not(E)
    if ( is.na(H) ){U<- 0.171238866197208}
    else if(H>0){U<- 0.81218442210766 }
    else{U<- 0.171238866197208 }
    if ( is.na(S) ){V<- J}
    else if(S>0){V<-sum( G*U , I,na.rm=TRUE) }
    else{V<- J }
    if ( is.na(A) ){W<- V}
    else if(A>0){W<-sum( B*R , D,na.rm=TRUE) }
    else{W<- V }
    X<- max(126, W,na.rm=TRUE)
    if ( is.na(126) || is.na( M)){Y<-0}
    else if(126< M){Y<-1 }
    else{Y<-0 }
    Z<- floor(9.94678219930287e-8*K^2*L*Y)
    AA<- max(X,sum( Z , -N , -O*P , -39.2499946172489*Q,na.rm=TRUE),na.rm=TRUE)
    FIN <-AA
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SA9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- mean(future.long[(t-3):(t- 1),"Togdheer_FutureRegion"], na.rm=TRUE)
    B<- rain.long[(t- 15),"Mudug_rain"]
    C<- rain.long[(t- 1),"Bari_rain"]
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    G<- before.long[(t- 1),"Gedo_BeforeRegion"]
    H<- before.long[(t- 1),"Gedo_BeforeRegion"]
    I<- before.long[(t- 1),"Bari_BeforeRegion"]
    J<- median(current.long[(t-11):(t- 1),"Togdheer_CurrentRegion"], na.rm=TRUE)
    K<- fatalities.long[(t- 11),"Jubbada_Hoose_Fatalities"]
    L<- fatalities.long[(t- 13),"Gedo_Fatalities"]
    M<- median(current.long[(t-12):(t- 1),"Togdheer_CurrentRegion"], na.rm=TRUE)
    N<- before.long[(t- 1),"Gedo_BeforeRegion"]
    O<- fatalities.long[(t- 12),"Shabeellaha_Hoose_Fatalities"]
    P<- min(sum(21.3924807575878 , H^2,na.rm=TRUE), I,na.rm=TRUE)
    if ( is.na(C) ){Q<- J}
    else if(C>0){Q<-sum( 0.00196463836708175*D*E , -G^2 , -3.31138770028502*P,na.rm=TRUE) }
    else{Q<- J }
    R<- max(Q,sum( K , L , M , -N , -O,na.rm=TRUE),na.rm=TRUE)
    S<- max(B, R,na.rm=TRUE)
    U<- max(126, S,na.rm=TRUE)
    V<- max(A, U,na.rm=TRUE)
    FIN <-V
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SA10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    B<- rain.long[(t- 12),"Banaadir_rain"]
    C<- conflicts.long[(t- 1),"Sanaag_Conflict"]
    D<- rain.long[(t- 5),"Gedo_rain"]
    E<- rain.long[(t- 14),"Shabeellaha_Hoose_rain"]
    G<- rain.long[(t- 8),"Banaadir_rain"]
    H<- fatalities.long[(t- 10),"Jubbada_Hoose_Fatalities"]
    I<- future.long[(t- 16),"Mudug_FutureRegion"]
    J<- rain.long[(t- 4),"Mudug_rain"]
    K<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    L<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    if(is.na(B)){M<-0}
    else if(B>0){M<-1}
    else{M<-0}
    
    N<- and(D, E)
    if ( is.na(G) ){O<- I}
    else if(G>0){O<- 6.24184313632607*H }
    else{O<- I }
    if ( is.na(N) ){P<- -151.297231808526}
    else if(N>0){P<- O }
    else{P<- -151.297231808526 }
    Q<- max(259.942100348958,sum( A*M , C , P , -J,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( 1.50628941054869*Q , -265.411102818204 , -3.9673156948415e-5*K*L,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SAJUN1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 2),"Bari_BeforeRegion"]
    B<- before.long[(t- 2),"Bari_BeforeRegion"]
    C<- before.long[(t- 2),"Bari_BeforeRegion"]
    D<- current.long[(t- 2),"Woqooyi_Galbeed_CurrentRegion"]
    E<- before.long[(t- 2),"Sool_BeforeRegion"]
    G<- median(current.long[(t-6):(t- 2),"Galgaduud_CurrentRegion"], na.rm=TRUE)
    H<- before.long[(t- 2),"Bari_BeforeRegion"]
    I<- before.long[(t- 2),"Bari_BeforeRegion"]
    J<- before.long[(t- 2),"Bari_BeforeRegion"]
    K<- current.long[(t- 2),"Woqooyi_Galbeed_CurrentRegion"]
    L<- before.long[(t- 2),"Sool_BeforeRegion"]
    M<- median(current.long[(t-6):(t- 2),"Galgaduud_CurrentRegion"], na.rm=TRUE)
    N<- before.long[(t- 2),"Sool_BeforeRegion"]
    O<- before.long[(t- 2),"Sool_BeforeRegion"]
    P<- mean(future.long[(t-4):(t- 2),"Sanaag_FutureRegion"], na.rm=TRUE)
    Q<- current.long[(t- 2),"Sool_CurrentRegion"]
    R<- before.long[(t- 2),"Bari_BeforeRegion"]
    S<- fatalities.long[(t- 5),"Shabeellaha_Dhexe_Fatalities"]
    U<- before.long[(t- 8),"Sool_BeforeRegion"]
    V<- before.long[(t- 2),"Bari_BeforeRegion"]
    W<- before.long[(t- 2),"Bari_BeforeRegion"]
    X<- before.long[(t- 2),"Bari_BeforeRegion"]
    Y<- current.long[(t- 2),"Woqooyi_Galbeed_CurrentRegion"]
    Z<- before.long[(t- 2),"Sool_BeforeRegion"]
    AA<- median(current.long[(t-6):(t- 2),"Galgaduud_CurrentRegion"], na.rm=TRUE)
    BB<- before.long[(t- 2),"Bari_BeforeRegion"]
    CC<- before.long[(t- 2),"Bari_BeforeRegion"]
    DD<- before.long[(t- 2),"Bari_BeforeRegion"]
    EE<- current.long[(t- 2),"Woqooyi_Galbeed_CurrentRegion"]
    GF<- before.long[(t- 2),"Sool_BeforeRegion"]
    HG<- median(current.long[(t-6):(t- 2),"Galgaduud_CurrentRegion"], na.rm=TRUE)
    IH<- before.long[(t- 2),"Sool_BeforeRegion"]
    JI<- before.long[(t- 2),"Sool_BeforeRegion"]
    if ( is.na(115.083334508743) || is.na( C)){KJ<-0}
    else if(115.083334508743> C){KJ<-1 }
    else{KJ<-0 }
    if ( is.na(E) || is.na( G)){LK<-0}
    else if(E> G){LK<-1 }
    else{LK<-0 }
    if ( is.na(115.083334508743) || is.na( J)){ML<-0}
    else if(115.083334508743> J){ML<-1 }
    else{ML<-0 }
    if ( is.na(L) || is.na( M)){NM<-0}
    else if(L> M){NM<-1 }
    else{NM<-0 }
    ON<- tan(sum(0.587761095544872*H , I*ML , K^NM , N,na.rm=TRUE))
    PO<- tan(R)
    if ( is.na(115.083334508743) || is.na( X)){QP<-0}
    else if(115.083334508743> X){QP<-1 }
    else{QP<-0 }
    RQ<- median(AA, 4)
    if ( is.na(Z) || is.na( RQ)){SR<-0}
    else if(Z> RQ){SR<-1 }
    else{SR<-0 }
    if ( is.na(115.083334508743) || is.na( DD)){US<-0}
    else if(115.083334508743> DD){US<-1 }
    else{US<-0 }
    VT<- median(HG, 4)
    if ( is.na(GF) || is.na( VT)){WU<-0}
    else if(GF> VT){WU<-1 }
    else{WU<-0 }
    XV<- tan(sum(0.587761095544872*BB , CC*US , EE^WU , IH,na.rm=TRUE))
    YW<- tan(sum(0.587761095544872*V , W*QP , Y^SR , 3.65797631449709*XV , JI,na.rm=TRUE))
    ZX<- max(115.083334508743,sum( 0.587761095544872*A , B*KJ , D^LK , 3.65797631449709*ON , O , P , -115.083334508743 , -Q , -PO , -S , -U , -YW,na.rm=TRUE),na.rm=TRUE)
    FIN <-ZX
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SAJUN2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 1),"Woqooyi_Galbeed_FutureRegion"]
    B<- future.long[(t- 1),"Woqooyi_Galbeed_FutureRegion"]
    C<- fatalities.long[(t- 1),"Awdal_Fatalities"]
    D<- future.long[(t- 1),"Woqooyi_Galbeed_FutureRegion"]
    E<- future.long[(t- 1),"Woqooyi_Galbeed_FutureRegion"]
    G<- before.long[(t- 1),"Bari_BeforeRegion"]
    H<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    I<- before.long[(t- 1),"Bari_BeforeRegion"]
    J<- fatalities.long[(t- 1),"Woqooyi_Galbeed_Fatalities"]
    K<- goats.long[(t- 1),"Jubbada_Hoose_goatprice"]
    L<- tail(movavg(future.long[(t-6):(t- 1),"Sanaag_FutureRegion"],5,type="w"),1)
    M<- future.long[(t- 1),"Woqooyi_Galbeed_FutureRegion"]
    N<- before.long[(t- 1),"Gedo_BeforeRegion"]
    O<- tan(0.646483320328856*B)
    P<- tan(0.646483320328856*E)
    if ( is.na(C) ){Q<- P}
    else if(C>0){Q<- D }
    else{Q<- P }
    R<- sin(sum(0.104240933245969 , I,na.rm=TRUE))
    S<- sin(K)
    U<- max(J, 1.04417082842184*S,na.rm=TRUE)
    V<- tan(0.646483320328856*M)
    W<- max(sum(0.646483320328856*A , O*Q , 0.0262152873099058*G*H*R*U , L , V , -N,na.rm=TRUE), 115.083337655478,na.rm=TRUE)
    FIN <-W
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SAJUN3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- median(rain.long[(t-13):(t- 1),"Gedo_rain"], na.rm=TRUE)
    B<- before.long[(t- 1),"Galgaduud_BeforeRegion"]
    C<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    D<- median(rain.long[(t-13):(t- 1),"Gedo_rain"], na.rm=TRUE)
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    H<- mean(before.long[(t-10):(t- 1),"Nugaal_BeforeRegion"], na.rm=TRUE)
    I<- before.long[(t- 1),"Galgaduud_BeforeRegion"]
    J<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    K<- mean(future.long[(t-5):(t- 1),"Woqooyi_Galbeed_FutureRegion"], na.rm=TRUE)
    L<- future.long[(t- 1),"Shabeallaha_Dhexe_FutureRegion"]
    M<- before.long[(t- 1),"Bari_BeforeRegion"]
    N<- median(rain.long[(t-13):(t- 1),"Gedo_rain"], na.rm=TRUE)
    O<- sinh(A)
    P<- sqrt(O)
    Q<- tanh(5.88567218945823e-7*B*C)
    R<- round(P*Q)
    S<- floor(D)
    U<- tanh(5.88567218945823e-7*I*J)
    V<- round(5.75953611758131e-6*E*G*H*U)
    W<- max(S*V,sum( K , -L,na.rm=TRUE),na.rm=TRUE)
    X<- sinh(N)
    Y<- min(98.0833254979859, 5.75953611758131e-6*M*X,na.rm=TRUE)
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(W)){W <- 0 }
    if(is.infinite(Y)){Y <- 0 }
    FIN <-sum( 115.083333706852 , R*W , -Y,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SAJUN4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- goats.long[(t- 1),"Gedo_goatprice"]
    B<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    C<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    D<- tail(movavg(future.long[(t-7):(t- 1),"Galgaduud_FutureRegion"],6,type="w"),1)
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    H<- mean(current.long[(t-9):(t- 1),"Awdal_CurrentRegion"], na.rm=TRUE)
    I<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    J<- mean(future.long[(t-6):(t- 1),"Galgaduud_FutureRegion"], na.rm=TRUE)
    K<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    L<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    M<- tail(movavg(goats.long[(t-8):(t- 1),"Bay_goatprice"], 7,type="m"),1)
    if ( is.na(B) || is.na( 1403.25104015997)){N<-0}
    else if(B> 1403.25104015997){N<-1 }
    else{N<-0 }
    if ( is.na(C) || is.na( D)){O<-0}
    else if(C>= D){O<-1 }
    else{O<-0 }
    if ( is.na(I) || is.na( J)){P<-0}
    else if(I> J){P<-1 }
    else{P<-0 }
    Q<- (sum(327.763112956052*A*N*O , 0.995055458185469*E*G*H*P , K , -43708.0456114156*L,na.rm=TRUE))
    R<- max(115.083333644486, Q/M,na.rm=TRUE)
    FIN <-R
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SAJUN5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- tail(movavg(future.long[(t-3):(t- 1),"Togdheer_FutureRegion"], 2,type="m"),1)
    B<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    C<- fatalities.long[(t- 2),"Awdal_Fatalities"]
    D<- fatalities.long[(t- 8),"Awdal_Fatalities"]
    E<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    G<- fatalities.long[(t- 12),"Nugaal_Fatalities"]
    H<- future.long[(t- 12),"Awdal_FutureRegion"]
    I<- median(fatalities.long[(t-7):(t- 1),"Woqooyi_Galbeed_Fatalities"], na.rm=TRUE)
    J<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    K<- rain.long[(t- 1),"Sanaag_rain"]
    L<- logistic(106.391374426238*I)
    if ( is.na(1.11964654691815e-5*J^2) || is.na( K)){M<-0}
    else if(1.11964654691815e-5*J^2> K){M<-1 }
    else{M<-0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( 106.391374426238 , 0.268353901357915*A , 1.11964654691815e-5*B^2 , 215.526901690584*C*D , 6.16247453454133e-6*E^2*G*H*L*M,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SAJUN6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    C<- before.long[(t- 1),"Bari_BeforeRegion"]
    D<- tail(movavg(fatalities.long[(t-9):(t- 1),"Galguduud_Fatalities"], 8,type="m"),1)
    E<- future.long[(t- 2),"Togdheer_FutureRegion"]
    G<- future.long[(t- 1),"Gedo_FutureRegion"]
    H<- future.long[(t- 2),"Togdheer_FutureRegion"]
    I<- fatalities.long[(t- 11),"Jubbada_Hoose_Fatalities"]
    J<- fatalities.long[(t- 12),"Jubbada_Hoose_Fatalities"]
    K<- fatalities.long[(t- 13),"Jubbada_Hoose_Fatalities"]
    L<- rain.long[(t- 5),"Nugaal_rain"]
    M<- future.long[(t- 2),"Togdheer_FutureRegion"]
    N<- fatalities.long[(t- 11),"Jubbada_Hoose_Fatalities"]
    O<- fatalities.long[(t- 12),"Jubbada_Hoose_Fatalities"]
    P<- fatalities.long[(t- 13),"Jubbada_Hoose_Fatalities"]
    Q<- before.long[(t- 1),"Bari_BeforeRegion"]
    R<- tan(C)
    S<- cosh(D)
    U<- max(113.616722133538,sum( 0.0014008403556467*A*B , -R , -1.03865874740774*S,na.rm=TRUE),na.rm=TRUE)
    V<- max(E, G,na.rm=TRUE)
    W<- max(H, I,na.rm=TRUE)
    X<- max(W, J,na.rm=TRUE)
    Y<- max(X, K,na.rm=TRUE)
    Z<- max(M, N,na.rm=TRUE)
    AA<- max(Z, O,na.rm=TRUE)
    BB<- max(AA, P,na.rm=TRUE)
    CC<- tan(Q)
    if ( is.na(L) ){DD<- CC}
    else if(L>0){DD<- BB }
    else{DD<- CC }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(Y)){Y <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(D)){D <- 0 }
    FIN <-sum( 1.01172565376926*U , 2.36161528844302e-6*V*Y*DD,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SAJUN7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    C<- before.long[(t- 1),"Bari_BeforeRegion"]
    D<- tail(movavg(fatalities.long[(t-9):(t- 1),"Galguduud_Fatalities"], 8,type="m"),1)
    E<- future.long[(t- 2),"Togdheer_FutureRegion"]
    G<- future.long[(t- 1),"Gedo_FutureRegion"]
    H<- future.long[(t- 2),"Togdheer_FutureRegion"]
    I<- fatalities.long[(t- 11),"Jubbada_Hoose_Fatalities"]
    J<- fatalities.long[(t- 12),"Jubbada_Hoose_Fatalities"]
    K<- fatalities.long[(t- 13),"Jubbada_Hoose_Fatalities"]
    L<- rain.long[(t- 5),"Nugaal_rain"]
    M<- future.long[(t- 2),"Togdheer_FutureRegion"]
    N<- fatalities.long[(t- 11),"Jubbada_Hoose_Fatalities"]
    O<- fatalities.long[(t- 12),"Jubbada_Hoose_Fatalities"]
    P<- fatalities.long[(t- 13),"Jubbada_Hoose_Fatalities"]
    Q<- before.long[(t- 1),"Bari_BeforeRegion"]
    R<- tan(C)
    S<- cosh(D)
    U<- max(113.616722133538,sum( 0.0014008403556467*A*B , -R , -1.03865874740774*S,na.rm=TRUE),na.rm=TRUE)
    V<- max(E, G,na.rm=TRUE)
    W<- max(H, I,na.rm=TRUE)
    X<- max(W, J,na.rm=TRUE)
    Y<- max(X, K,na.rm=TRUE)
    Z<- max(M, N,na.rm=TRUE)
    AA<- max(Z, O,na.rm=TRUE)
    BB<- max(AA, P,na.rm=TRUE)
    CC<- tan(Q)
    if ( is.na(L) ){DD<- CC}
    else if(L>0){DD<- BB }
    else{DD<- CC }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(V)){V <- 0 }
    if(is.infinite(Y)){Y <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(D)){D <- 0 }
    FIN <-sum( 1.01172565376926*U , 2.36161528844302e-6*V*Y*DD,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SAJUN8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    B<- conflicts.long[(t- 3),"Awdal_Conflict"]
    C<- tail(movavg(water.long[(t-11):(t- 1),"Togdheer_WaterDrumPrice"], 10,type="m"),1)
    D<- median(rain.long[(t-4):(t- 1),"Banaadir_rain"], na.rm=TRUE)
    E<- tail(movavg(current.long[(t-16):(t- 1),"Jubbada_Dhexe_CurrentRegion"], 15,type="m"),1)
    G<- conflicts.long[(t- 3),"Awdal_Conflict"]
    H<- tail(movavg(water.long[(t-11):(t- 1),"Togdheer_WaterDrumPrice"], 10,type="m"),1)
    I<- tail(movavg(current.long[(t-16):(t- 1),"Jubbada_Dhexe_CurrentRegion"], 15,type="m"),1)
    J<- conflicts.long[(t- 3),"Awdal_Conflict"]
    K<- tail(movavg(water.long[(t-11):(t- 1),"Togdheer_WaterDrumPrice"], 10,type="m"),1)
    L<- tail(movavg(current.long[(t-16):(t- 1),"Jubbada_Dhexe_CurrentRegion"], 15,type="m"),1)
    M<- conflicts.long[(t- 3),"Awdal_Conflict"]
    N<- tail(movavg(water.long[(t-11):(t- 1),"Togdheer_WaterDrumPrice"], 10,type="m"),1)
    O<- conflicts.long[(t- 3),"Awdal_Conflict"]
    P<- tail(movavg(water.long[(t-11):(t- 1),"Togdheer_WaterDrumPrice"], 10,type="m"),1)
    Q<- median(rain.long[(t-4):(t- 1),"Banaadir_rain"], na.rm=TRUE)
    R<- tail(movavg(current.long[(t-16):(t- 1),"Jubbada_Dhexe_CurrentRegion"], 15,type="m"),1)
    S<- conflicts.long[(t- 3),"Awdal_Conflict"]
    U<- tail(movavg(water.long[(t-11):(t- 1),"Togdheer_WaterDrumPrice"], 10,type="m"),1)
    V<- tail(movavg(current.long[(t-16):(t- 1),"Jubbada_Dhexe_CurrentRegion"], 15,type="m"),1)
    W<- conflicts.long[(t- 3),"Awdal_Conflict"]
    X<- tail(movavg(water.long[(t-11):(t- 1),"Togdheer_WaterDrumPrice"], 10,type="m"),1)
    Y<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    Z<- tail(movavg(current.long[(t-16):(t- 1),"Jubbada_Dhexe_CurrentRegion"], 15,type="m"),1)
    AA<- conflicts.long[(t- 3),"Awdal_Conflict"]
    BB<- tail(movavg(water.long[(t-11):(t- 1),"Togdheer_WaterDrumPrice"], 10,type="m"),1)
    CC<- sin(C)
    DD<- min(B,sum( CC , -D,na.rm=TRUE),na.rm=TRUE)
    EE<- logistic(114.26437490915*DD)
    GF<- sin(H)
    HG<- asinh(GF)
    IH<- min(G, HG,na.rm=TRUE)
    JI<- sin(K)
    KJ<- asinh(JI)
    LK<- min(J, KJ,na.rm=TRUE)
    if ( is.na(I*LK) || is.na( 115.083329872667)){ML<-0}
    else if(I*LK> 115.083329872667){ML<-1 }
    else{ML<-0 }
    NM<- sin(N)
    ON<- asinh(NM)
    PO<- min(M, ON,na.rm=TRUE)
    if ( is.na(L*PO) || is.na( 115.083329872667)){QP<-0}
    else if(L*PO> 115.083329872667){QP<-1 }
    else{QP<-0 }
    RQ<- sin(P)
    SR<- min(O,sum( RQ , -Q,na.rm=TRUE),na.rm=TRUE)
    US<- logistic(114.26437490915*SR)
    VT<- sin(U)
    WU<- asinh(VT)
    XV<- min(S, WU,na.rm=TRUE)
    if ( is.na(R*XV) || is.na( 115.083329872667)){YW<-0}
    else if(R*XV> 115.083329872667){YW<-1 }
    else{YW<-0 }
    ZX<- sin(X)
    AAY<- asinh(ZX)
    BBZ<- min(W, AAY,na.rm=TRUE)
    if ( is.na(V*BBZ) || is.na( 115.083329872667)){AAA<-0}
    else if(V*BBZ> 115.083329872667){AAA<-1 }
    else{AAA<-0 }
    BBB<- max(36894.9926156712*US*YW, AAA,na.rm=TRUE)
    CCC<- BB
    DDD<- sin(CCC)
    EEE<- asinh(DDD)
    GFF<- min(AA, EEE,na.rm=TRUE)
    if ( is.na(Z*GFF) || is.na( 115.083329872667)){HGG<-0}
    else if(Z*GFF> 115.083329872667){HGG<-1 }
    else{HGG<-0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(Y)){Y <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(G)){G <- 0 }
    FIN <-sum( 115.083329872667 , 0.667557237640508*A*EE , E*IH*ML , QP , BBB , -2.28029795399033*Y*HGG,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SAJUN9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    C<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    G<- before.long[(t- 1),"Bari_BeforeRegion"]
    H<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    I<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    J<- before.long[(t- 1),"Galgaduud_BeforeRegion"]
    K<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    L<- before.long[(t- 1),"Bari_BeforeRegion"]
    M<- current.long[(t- 1),"Galgaduud_CurrentRegion"]
    N<- ceil(1.31595098254662e-7*A*B)
    O<- round(1.94939758823441e-7*D*E)
    P<- round(1.94939758823441e-7*G*H)
    Q<- atan2(1.70629392206533*P, I)
    R<- round(1.94939758823441e-7*L*M)
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( 15714.7520023661*N , 0.166592833077527*C*O , Q , -15599.6686692213 , -2.09250868732095e-7*J*K*R,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

modelarrivals_SAJUN10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){
    
    A<- future.long[(t- 1),"Sanaag_FutureRegion"]
    B<- mean(future.long[(t-3):(t- 1),"Shabeallaha_Dhexe_FutureRegion"], na.rm=TRUE)
    C<- future.long[(t- 1),"Sanaag_FutureRegion"]
    D<- stations.long[(t- 1),"Shabelle_Dhexe_JowharStation_Shabelle_River"]
    E<- before.long[(t- 1),"Togdheer_BeforeRegion"]
    G<- future.long[(t- 1),"Sanaag_FutureRegion"]
    H<- mean(future.long[(t-3):(t- 1),"Shabeallaha_Dhexe_FutureRegion"], na.rm=TRUE)
    I<- future.long[(t- 1),"Sanaag_FutureRegion"]
    J<- before.long[(t- 1),"Sanaag_BeforeRegion"]
    K<- tail(movavg(current.long[(t-3):(t- 1),"Hiiraan_CurrentRegion"], 2,type="m"),1)
    L<- tan(sum(113.655704398737 , 0.0178749402081017*A,na.rm=TRUE))
    if ( is.na(B) || is.na( C)){M<-0}
    else if(B>= C){M<-1 }
    else{M<-0 }
    if ( is.na(D) || is.na( 0.800227695328565)){N<-0}
    else if(D<= 0.800227695328565){N<-1 }
    else{N<-0 }
    O<- tan(sum(113.655704398737 , 0.0178749402081017*G,na.rm=TRUE))
    if ( is.na(M) ){P<- H}
    else if(M>0){P<-sum( 31632.9980974275*N , 7.96648621109844e-9*E^3 , 5.01679812205639*O,na.rm=TRUE) }
    else{P<- H }
    if ( is.na(L) ){Q<- I}
    else if(L>0){Q<- P }
    else{Q<- I }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 111.269298720953 , Q , -3.95196868258332e-5*J*K,na.rm=TRUE)
    PA[t] <- FIN
    PI[t] <- 0
    PD[t] <- 0
  }
  return(PA)
}

    

