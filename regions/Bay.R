
modelarrivalsminus1_1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- future.long[(t- 1),"Sanaag_FutureRegion"]
    C<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    D<- future.long[(t- 10),"Nugaal_FutureRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    H<- future.long[(t- 3),"Nugaal_FutureRegion"]
    I<- mean(future.long[(t-13):(t- 1),"Shabeellaha_Hoose_FutureRegion"], na.rm=TRUE)
    J<- tail(movavg(current.long[(t-3):(t- 1),"Togdheer_CurrentRegion"], 2,type="m"),1)
    K<- median(current.long[(t-12):(t- 1),"Jubbada_Dhexe_CurrentRegion"], na.rm=TRUE)
    L<- max(sum(4.84503108841733*A , 1.70406507425211*B , 1.70406507425211*C , 2.02606412957083*D , 2.34737505506137e-8*E^3*G , H , -I , -0.396277558619911*J,na.rm=TRUE), K,na.rm=TRUE)
    FIN <-L
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivalsminus1_2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    C<- future.long[(t- 1),"Sanaag_FutureRegion"]
    D<- future.long[(t- 10),"Nugaal_FutureRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    H<- future.long[(t- 3),"Nugaal_FutureRegion"]
    I<- median(future.long[(t-4):(t- 1),"Jubbada_Hoose_FutureRegion"], na.rm=TRUE)
    J<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    K<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    L<- max(sum(4.88281349617007*A , 1.70985734411931*B , 1.6698879385747*C , 2.02695796095132*D , 2.35314983669832e-8*E^3*G , H , -I , -0.392410307797819*J,na.rm=TRUE), 1.70985734411931*K,na.rm=TRUE)
    FIN <-L
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivalsminus1_3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    C<- future.long[(t- 1),"Sanaag_FutureRegion"]
    D<- future.long[(t- 10),"Nugaal_FutureRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    H<- future.long[(t- 3),"Nugaal_FutureRegion"]
    I<- median(future.long[(t-4):(t- 1),"Jubbada_Hoose_FutureRegion"], na.rm=TRUE)
    J<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    K<- abs(sum(4.88281349617007*A , 1.70985734411931*B , 1.6698879385747*C , 2.02695796095132*D , 2.35314983669832e-8*E^3*G , H , -I , -0.392410307797819*J,na.rm=TRUE))
    FIN <-K
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivalsminus1_4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- future.long[(t- 1),"Sanaag_FutureRegion"]
    C<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    D<- future.long[(t- 10),"Nugaal_FutureRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    H<- future.long[(t- 3),"Nugaal_FutureRegion"]
    I<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    J<- abs(sum(5.06141875517708*A , 1.86252695291916*B , 1.71521807682413*C , 2.02871687302873*D , 2.3744514027128e-8*E^3*G , H , -1338.60534187287 , -0.351794191125006*I,na.rm=TRUE))
    FIN <-J
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivalsminus1_5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    C<- future.long[(t- 1),"Sanaag_FutureRegion"]
    D<- future.long[(t- 10),"Nugaal_FutureRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    H<- future.long[(t- 3),"Nugaal_FutureRegion"]
    I<- mean(future.long[(t-4):(t- 1),"Gedo_FutureRegion"], na.rm=TRUE)
    J<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    FIN <-sum( 4.82887531388463*A , 1.70289480464375*B , 1.65496054485129*C , 2.00798843542565*D , 2.34518356772877e-8*E^3*G , H , -I , -0.393189522390147*J,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

Minus2_1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 2),"Bari_BeforeRegion"]
    B<- current.long[(t- 2),"Awdal_CurrentRegion"]
    C<- current.long[(t- 2),"Awdal_CurrentRegion"]
    D<- before.long[(t- 2),"Bari_BeforeRegion"]
    E<- current.long[(t- 2),"Awdal_CurrentRegion"]
    G<- current.long[(t- 2),"Woqooyi_Galbeed_CurrentRegion"]
    H<- before.long[(t- 2),"Bari_BeforeRegion"]
    I<- current.long[(t- 2),"Awdal_CurrentRegion"]
    J<- future.long[(t- 2),"Sanaag_FutureRegion"]
    K<- future.long[(t- 15),"Mudug_FutureRegion"]
    L<- before.long[(t- 2),"Gedo_BeforeRegion"]
    M<- before.long[(t- 2),"Bari_BeforeRegion"]
    N<- current.long[(t- 2),"Awdal_CurrentRegion"]
    O<- current.long[(t- 2),"Awdal_CurrentRegion"]
    P<- before.long[(t- 2),"Bari_BeforeRegion"]
    Q<- current.long[(t- 2),"Awdal_CurrentRegion"]
    R<- future.long[(t- 2),"Sanaag_FutureRegion"]
    S<- rain.long[(t- 2),"Gedo_rain"]
    U<- max(G, 7.15546755231496*H,na.rm=TRUE)
    V<- max(E, U,na.rm=TRUE)
    W<- max(1.17983082236031, 8.48269773360738e-6*I^2*J,na.rm=TRUE)
    X<- max(1.17983082236031, 8.48269773360738e-6*Q^2*R,na.rm=TRUE)
    Y<- sum(1.11587760900997e-8*M*N %% 1.11587760900997e-8*O^3*P,na.rm=TRUE) 
    Z<- max(1.17983082236031*K, L*Y,na.rm=TRUE)
            if(is.infinite(A)){A <- 0 }
            if(is.infinite(B)){B <- 0 }
            if(is.infinite(C)){C <- 0 }
            if(is.infinite(D)){D <- 0 }
            if(is.infinite(V)){V <- 0 }
            if(is.infinite(W)){W <- 0 }
            if(is.infinite(Z)){Z <- 0 }
            if(is.infinite(S)){S <- 0 }
            FIN <-sum( 1.11587760900997e-8*A*B , 1.11587760900997e-8*C^3*D , V , W , Z , -S,na.rm=TRUE)
            PA[t] <- FIN
            #Bay_Incidents
            PI[t] <- 0
            #Bay_Departures
            PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

Minus2_2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 2),"Awdal_CurrentRegion"]
    B<- before.long[(t- 2),"Bari_BeforeRegion"]
    C<- current.long[(t- 2),"Awdal_CurrentRegion"]
    D<- current.long[(t- 2),"Woqooyi_Galbeed_CurrentRegion"]
    E<- before.long[(t- 2),"Bari_BeforeRegion"]
    G<- current.long[(t- 2),"Awdal_CurrentRegion"]
    H<- future.long[(t- 2),"Sanaag_FutureRegion"]
    I<- future.long[(t- 15),"Mudug_FutureRegion"]
    J<- before.long[(t- 2),"Gedo_BeforeRegion"]
    K<- current.long[(t- 2),"Awdal_CurrentRegion"]
    L<- before.long[(t- 2),"Bari_BeforeRegion"]
    M<- current.long[(t- 2),"Awdal_CurrentRegion"]
    N<- future.long[(t- 2),"Sanaag_FutureRegion"]
    O<- rain.long[(t- 2),"Gedo_rain"]
    P<- max(D, 7.15455838064045*E,na.rm=TRUE)
    Q<- max(C, P,na.rm=TRUE)
    R<- max(1.17983082236031, 8.48269773360738e-6*G^2*H,na.rm=TRUE)
    S<- max(1.17983082236031, 8.48269773360738e-6*M^2*N,na.rm=TRUE)
    U<- sum(1.17983082236031 %% 1.11587760900997e-8*K^3*L, na.rm=TRUE)
            V<- max(1.17983082236031*I, J*U,na.rm=TRUE)
            if(is.infinite(A)){A <- 0 }
            if(is.infinite(B)){B <- 0 }
            if(is.infinite(Q)){Q <- 0 }
            if(is.infinite(R)){R <- 0 }
            if(is.infinite(V)){V <- 0 }
            if(is.infinite(O)){O <- 0 }
            FIN <-sum( 1.17983082236031 , 1.11587760900997e-8*A^3*B , Q , R , V , -O,na.rm=TRUE)
            PA[t] <- FIN
            #Bay_Incidents
            PI[t] <- 0
            #Bay_Departures
            PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

Minus2_3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 2),"Awdal_CurrentRegion"]
    B<- before.long[(t- 2),"Bari_BeforeRegion"]
    C<- current.long[(t- 2),"Awdal_CurrentRegion"]
    D<- current.long[(t- 2),"Woqooyi_Galbeed_CurrentRegion"]
    E<- before.long[(t- 2),"Bari_BeforeRegion"]
    G<- current.long[(t- 2),"Awdal_CurrentRegion"]
    H<- future.long[(t- 2),"Sanaag_FutureRegion"]
    I<- future.long[(t- 15),"Mudug_FutureRegion"]
    J<- before.long[(t- 2),"Gedo_BeforeRegion"]
    K<- current.long[(t- 2),"Awdal_CurrentRegion"]
    L<- before.long[(t- 2),"Bari_BeforeRegion"]
    M<- current.long[(t- 2),"Awdal_CurrentRegion"]
    N<- future.long[(t- 2),"Sanaag_FutureRegion"]
    O<- rain.long[(t- 2),"Gedo_rain"]
    P<- max(D, 7.15599635586592*E,na.rm=TRUE)
    Q<- max(C, P,na.rm=TRUE)
    R<- max(1.17983082236031, 8.48269773360738e-6*G^2*H,na.rm=TRUE)
    S<- max(1.17983082236031, 8.48269773360738e-6*M^2*N,na.rm=TRUE)
    U<- sum(1.11587760900997e-8*K^3*L %% S,na.rm=TRUE)
            V<- max(1.17983082236031*I, J*U,na.rm=TRUE)
            if(is.infinite(A)){A <- 0 }
            if(is.infinite(B)){B <- 0 }
            if(is.infinite(Q)){Q <- 0 }
            if(is.infinite(R)){R <- 0 }
            if(is.infinite(V)){V <- 0 }
            if(is.infinite(O)){O <- 0 }
            FIN <-sum( 1.11587760900997e-8*A^3*B , Q , R , V , -O,na.rm=TRUE)
            PA[t] <- FIN
            #Bay_Incidents
            PI[t] <- 0
            #Bay_Departures
            PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

Minus2_4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 2),"Awdal_CurrentRegion"]
    B<- future.long[(t- 2),"Sanaag_FutureRegion"]
    C<- current.long[(t- 2),"Awdal_CurrentRegion"]
    D<- before.long[(t- 2),"Bari_BeforeRegion"]
    E<- current.long[(t- 2),"Awdal_CurrentRegion"]
    G<- current.long[(t- 2),"Woqooyi_Galbeed_CurrentRegion"]
    H<- before.long[(t- 2),"Bari_BeforeRegion"]
    I<- future.long[(t- 15),"Mudug_FutureRegion"]
    J<- before.long[(t- 2),"Gedo_BeforeRegion"]
    K<- current.long[(t- 2),"Awdal_CurrentRegion"]
    L<- future.long[(t- 2),"Sanaag_FutureRegion"]
    M<- current.long[(t- 2),"Awdal_CurrentRegion"]
    N<- before.long[(t- 2),"Bari_BeforeRegion"]
    O<- rain.long[(t- 2),"Gedo_rain"]
    P<- max(G, 7.15586927332544*H,na.rm=TRUE)
    Q<- max(E, P,na.rm=TRUE)
    R<- sum(8.48269773360738e-6*K^2*L %% 1.11587760900997e-8*M^3*N,na.rm = TRUE)
            S<- max(1.17983082236031*I, J*R,na.rm=TRUE)
            if(is.infinite(A)){A <- 0 }
            if(is.infinite(B)){B <- 0 }
            if(is.infinite(C)){C <- 0 }
            if(is.infinite(D)){D <- 0 }
            if(is.infinite(Q)){Q <- 0 }
            if(is.infinite(S)){S <- 0 }
            if(is.infinite(O)){O <- 0 }
            FIN <-sum( 8.48269773360738e-6*A^2*B , 1.11587760900997e-8*C^3*D , Q , S , -O,na.rm=TRUE)
            PA[t] <- FIN
            #Bay_Incidents
            PI[t] <- 0
            #Bay_Departures
            PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAYA1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- future.long[(t- 1),"Sanaag_FutureRegion"]
    C<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    D<- future.long[(t- 10),"Nugaal_FutureRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    H<- future.long[(t- 3),"Nugaal_FutureRegion"]
    I<- mean(future.long[(t-13):(t- 1),"Shabeellaha_Hoose_FutureRegion"], na.rm=TRUE)
    J<- tail(movavg(current.long[(t-3):(t- 1),"Togdheer_CurrentRegion"], 2,type="m"),1)
    K<- median(current.long[(t-12):(t- 1),"Jubbada_Dhexe_CurrentRegion"], na.rm=TRUE)
    L<- max(sum(4.84503108841733*A , 1.70406507425211*B , 1.70406507425211*C , 2.02606412957083*D , 2.34737505506137e-8*E^3*G , H , -I , -0.396277558619911*J,na.rm=TRUE), K,na.rm=TRUE)
    FIN <-L
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAYA2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- before.long[(t- 1),"Bari_BeforeRegion"]
    C<- before.long[(t- 1),"Bari_BeforeRegion"]
    D<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    E<- future.long[(t- 10),"Nugaal_FutureRegion"]
    G<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    H<- future.long[(t- 3),"Nugaal_FutureRegion"]
    I<- future.long[(t- 11),"Hiiraan_FutureRegion"]
    J<- tail(movavg(future.long[(t-6):(t- 1),"Jubbada_Hoose_FutureRegion"], 5,type="m"),1)
    K<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    L<- median(current.long[(t-12):(t- 1),"Jubbada_Dhexe_CurrentRegion"], na.rm=TRUE)
    M<- max(2.14340267697426*E, 1.93781524831724*G,na.rm=TRUE)
    N<- max(sum(2.35286323903499e-8*B^3 , 2.35286323903499e-8*C^3*D , M,na.rm=TRUE), H,na.rm=TRUE)
    O<- max(sum(4.90421487561024*A , N , -I , -J , -0.449570838691363*K,na.rm=TRUE), L,na.rm=TRUE)
    FIN <-O
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAYA3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    C<- future.long[(t- 1),"Sanaag_FutureRegion"]
    D<- future.long[(t- 10),"Nugaal_FutureRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    H<- future.long[(t- 3),"Nugaal_FutureRegion"]
    I<- median(future.long[(t-4):(t- 1),"Jubbada_Hoose_FutureRegion"], na.rm=TRUE)
    J<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    K<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    L<- max(sum(4.88281349617007*A , 1.70985734411931*B , 1.6698879385747*C , 2.02695796095132*D , 2.35314983669832e-8*E^3*G , H , -I , -0.392410307797819*J,na.rm=TRUE), 1.70985734411931*K,na.rm=TRUE)
    FIN <-L
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAYA4arrivals <- function(start, end){
  start = 24
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- mean(fatalities.long[(t-9):(t- 1),"Shabeellaha_Dhexe_Fatalities"], na.rm=TRUE)
    B<- tail(movavg(current.long[(t-23):(t- 1),"Awdal_CurrentRegion"], 22,type="m"),1)
    C<- mean(fatalities.long[(t-9):(t- 1),"Shabeellaha_Dhexe_Fatalities"], na.rm=TRUE)
    D<- cos(C)
    if ( is.na(-56.1324545145847) || is.na( D)){E<-0}
    else if(-56.1324545145847> D){E<-1 }
    else{E<-0 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(E)){E <- 0 }
    FIN <-sum( A*B , E,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAYA5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    C<- conflicts.long[(t- 1),"Bari_Conflict"]
    D<- conflicts.long[(t- 1),"Jubbada_Hoose_Conflict"]
    E<- future.long[(t- 1),"Bari_FutureRegion"]
    G<- before.long[(t- 1),"Banadir_BeforeRegion"]
    H<- current.long[(t- 1),"Woqooyi_Galbeed_CurrentRegion"]
    if ( is.na(G) || is.na( H)){I<-0}
    else if(G<= H){I<-1 }
    else{I<-0 }
    J<- max(7.38950606002206*D*E*I, 651.051010484291,na.rm=TRUE)
    K<- max(sum(1.81502783203591*B , -C,na.rm=TRUE), J,na.rm=TRUE)
    L<- max(606.458646879911,sum( A , K,na.rm=TRUE),na.rm=TRUE)
    FIN <-L
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAYA6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- conflicts.long[(t- 1),"Gedo_Conflict"]
    C<- current.long[(t- 1),"Awdal_CurrentRegion"]
    D<- current.long[(t- 1),"Awdal_CurrentRegion"]
    E<- current.long[(t- 1),"Awdal_CurrentRegion"]
    G<- current.long[(t- 1),"Awdal_CurrentRegion"]
    H<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    I<- before.long[(t- 1),"Bay_BeforeRegion"]
    J<- (3.33209975974583*A)
    K<- sinh(B)
    if ( is.na(K) || is.na( C)){L<-0}
    else if(K> C){L<-1 }
    else{L<-0 }
    M<- max(sum(G , H,na.rm=TRUE), 0.0387394629882406*I,na.rm=TRUE)
    N<- max(E, M,na.rm=TRUE)
    O<- max(D, N,na.rm=TRUE)
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 349.303675370176 , 0.838184696170641*J^L , 1.14087751383413*O,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAYA7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(before.long[(t-8):(t- 1),"Bay_BeforeRegion"],7,type="w"),1)
    FIN <-A
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}



modelarrivals_BAY1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- before.long[(t- 5),"Bari_BeforeRegion"]
    C<- rain.long[(t- 16),"Bari_rain"]
    D<- before.long[(t- 1),"Mudug_BeforeRegion"]
    E<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    G<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    H<- current.long[(t- 1),"Awdal_CurrentRegion"]
    I<- before.long[(t- 1),"Mudug_BeforeRegion"]
    J<- conflicts.long[(t- 1),"Awdal_Conflict"]
    K<- rain.long[(t- 16),"Bari_rain"]
    L<- before.long[(t- 1),"Mudug_BeforeRegion"]
    M<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    N<- fatalities.long[(t- 1),"Bari_Fatalities"]
    O<- before.long[(t- 1),"Banadir_BeforeRegion"]
    P<- before.long[(t- 1),"Mudug_BeforeRegion"]
    if ( is.na(C) ){Q<- I}
    else if(C>0){Q<-sum( 0.00216084551123691*D*E , 0.000112512408566433*G*H,na.rm=TRUE) }
    else{Q<- I }
    R<- exp(J)
    S<- tan(0.00216084551123691*L*M)
    U<- max(R*K*S, 0.00833736411871744*N*O,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( A , B , Q , U , -0.448385126774561*P,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAY2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    C<- before.long[(t- 2),"Nugaal_BeforeRegion"]
    D<- future.long[(t- 16),"Mudug_FutureRegion"]
    E<- current.long[(t- 1),"Awdal_CurrentRegion"]
    G<- future.long[(t- 7),"Sanaag_FutureRegion"]
    H<- conflicts.long[(t- 1),"Awdal_Conflict"]
    I<- future.long[(t- 8),"Bari_FutureRegion"]
    J<- goats.long[(t- 6),"Awdal_goatprice"]
    K<- median(future.long[(t-4):(t- 1),"Jubbada_Hoose_FutureRegion"], na.rm=TRUE)
    if ( is.na(4.80168968346346) || is.na( H)){L<-0}
    else if(4.80168968346346<= H){L<-1 }
    else{L<-0 }
    M<- tan(J)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(K)){K <- 0 }
    FIN <-sum( 4.80222888020813*A , 1.59090997853518*B , 3.0114519156067*C , 2.99190205691752*D , 0.951826957171889*E*G*L , I , M , -K,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAY3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- mean(before.long[(t-13):(t- 1),"Shabeellaha_Dhexe_BeforeRegion"], na.rm=TRUE)
    B<- mean(current.long[(t-17):(t- 1),"Jubbada_Dhexe_CurrentRegion"], na.rm=TRUE)
    C<- current.long[(t- 1),"Awdal_CurrentRegion"]
    D<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    E<- future.long[(t- 1),"Sanaag_FutureRegion"]
    G<- before.long[(t- 2),"Gedo_BeforeRegion"]
    H<- future.long[(t- 3),"Nugaal_FutureRegion"]
    I<- fatalities.long[(t- 13),"Gedo_Fatalities"]
    J<- future.long[(t- 5),"Nugaal_FutureRegion"]
    K<- before.long[(t- 4),"Gedo_BeforeRegion"]
    L<- before.long[(t- 10),"Gedo_BeforeRegion"]
    M<- tail(movavg(conflicts.long[(t-5):(t- 1),"Woqooyi_Galbeed_Conflict"], 4,type="m"),1)
    if ( is.na(I) ){N<- K}
    else if(I>0){N<- 7.28028841064564*J }
    else{N<- K }
    O<- max(B,sum( 5.11973052701946*C , 1.59567097909628*D , 1.59271718057403*E , G , H , N , -L*M,na.rm=TRUE),na.rm=TRUE)
    P<- max(A, O,na.rm=TRUE)
    if(is.infinite(P)){P <- 0 }
    FIN <-sum( P , -281.797989582422,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAY4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Sool_BeforeRegion"]
    B<- tail(movavg(rain.long[(t-4):(t- 1),"Awdal_rain"], 3,type="m"),1)
    C<- current.long[(t- 1),"Awdal_CurrentRegion"]
    D<- mean(future.long[(t-3):(t- 1),"Sool_FutureRegion"], na.rm=TRUE)
    E<- fatalities.long[(t- 16),"Sool_Fatalities"]
    G<- median(fatalities.long[(t-8):(t- 1),"Sool_Fatalities"], na.rm=TRUE)
    H<- median(future.long[(t-8):(t- 1),"Sanaag_FutureRegion"], na.rm=TRUE)
    I<- future.long[(t- 3),"Nugaal_FutureRegion"]
    J<- median(current.long[(t-7):(t- 1),"Sool_CurrentRegion"], na.rm=TRUE)
    K<- future.long[(t- 11),"Shabeallaha_Dhexe_FutureRegion"]
    L<- current.long[(t- 1),"Gedo_CurrentRegion"]
    M<- median(fatalities.long[(t-8):(t- 1),"Sool_Fatalities"], na.rm=TRUE)
    N<- future.long[(t- 5),"Nugaal_FutureRegion"]
    O<- conflicts.long[(t- 8),"Galgaduud_Conflict"]
    P<- sin(B)
    Q<- erfc(M)
    R<- max(sum(0.157488441036449*K , L*Q,na.rm=TRUE), N*O,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(G)){G <- 0 }
    if(is.infinite(H)){H <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(R)){R <- 0 }
    FIN <-sum( A*P , 0.0292491764810561*C*D , E*G*H , I , J , R,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAY5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Mudug_BeforeRegion"]
    B<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    C<- median(conflicts.long[(t-13):(t- 1),"Shabeellaha_Dhexe_Conflict"], na.rm=TRUE)
    D<- before.long[(t- 2),"Jubbada_Dhexe_BeforeRegion"]
    E<- fatalities.long[(t- 10),"Nugaal_Fatalities"]
    G<- mean(rain.long[(t-3):(t- 1),"Nugaal_rain"], na.rm=TRUE)
    H<- conflicts.long[(t- 1),"Awdal_Conflict"]
    I<- rain.long[(t- 6),"Sanaag_rain"]
    J<- median(fatalities.long[(t-8):(t- 1),"Woqooyi_Galbeed_Fatalities"], na.rm=TRUE)
    K<- rain.long[(t- 1),"Awdal_rain"]
    L<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    M<- current.long[(t- 1),"Awdal_CurrentRegion"]
    N<- current.long[(t- 1),"Nugaal_CurrentRegion"]
    O<- tanh(G)
    P<- factorial(H)
    if ( is.na(K) ){Q<- N}
    else if(K>0){Q<- 0.000131923062226079*L*M }
    else{Q<- N }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(E)){E <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(I)){I <- 0 }
    if(is.infinite(J)){J <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( 0.000200329999932846*A*B*C , D*E*O , 1.11354350678477*P*I*J , Q,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAY6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Mudug_BeforeRegion"]
    B<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    C<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    D<- future.long[(t- 1),"Sanaag_FutureRegion"]
    E<- fatalities.long[(t- 17),"Hiiraan_Fatalities"]
    G<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    H<- current.long[(t- 1),"Awdal_CurrentRegion"]
    I<- conflicts.long[(t- 1),"Awdal_Conflict"]
    J<- fatalities.long[(t- 7),"Bari_Fatalities"]
    K<- future.long[(t- 7),"Sanaag_FutureRegion"]
    L<- tail(movavg(fatalities.long[(t-7):(t- 1),"Woqooyi_Galbeed_Fatalities"],6,type="w"),1)
    M<- before.long[(t- 1),"Mudug_BeforeRegion"]
    N<- max(sum(31*E , 0.000129185744445161*G*H,na.rm=TRUE), I*J*K*L,na.rm=TRUE)
    O<- max(447.7559641061,sum( 5.54978156662037e-8*A*B*C , D , N , -447.7559641061,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( O , -0.288114007837762*M,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}
modelarrivals_BAY7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 7),"Togdheer_BeforeRegion"]
    B<- future.long[(t- 3),"Nugaal_FutureRegion"]
    C<- median(current.long[(t-12):(t- 1),"Jubbada_Dhexe_CurrentRegion"], na.rm=TRUE)
    D<- rain.long[(t- 1),"Awdal_rain"]
    E<- current.long[(t- 1),"Awdal_CurrentRegion"]
    G<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    H<- before.long[(t- 15),"Mudug_BeforeRegion"]
    I<- mean(fatalities.long[(t-4):(t- 1),"Togdheer_Fatalities"], na.rm=TRUE)
    J<- median(conflicts.long[(t-14):(t- 1),"Shabeellaha_Hoose_Conflict"], na.rm=TRUE)
    K<- before.long[(t- 8),"Togdheer_BeforeRegion"]
    L<- future.long[(t- 14),"Bay_FutureRegion"]
    M<- mean(rain.long[(t-13):(t- 1),"Sanaag_rain"], na.rm=TRUE)
    N<- median(rain.long[(t-17):(t- 1),"Shabeellaha_Hoose_rain"], na.rm=TRUE)
    O<- current.long[(t- 1),"Nugaal_CurrentRegion"]
    P<- future.long[(t- 3),"Nugaal_FutureRegion"]
    Q<- median(current.long[(t-12):(t- 1),"Jubbada_Dhexe_CurrentRegion"], na.rm=TRUE)
    R<- sin(J)
    if ( is.na(O) || is.na(sum( P , Q,na.rm=TRUE))){S<-0}
    else if(O<=sum( P , Q,na.rm=TRUE)){S<-1 }
    else{S<-0 }
    U<- xor(N, S)
    if ( is.na(D) ){V<- L*M*U}
    else if(D>0){V<-sum( 3.82647468159365*E , 1.8465774349568*G , H*I*R , K,na.rm=TRUE) }
    else{V<- L*M*U }
    W<- max(sum(B , C,na.rm=TRUE), V,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(W)){W <- 0 }
    FIN <-sum( A , W,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAY8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- mean(rain.long[(t-7):(t- 1),"Woqooyi_Galbeed_rain"], na.rm=TRUE)
    B<- mean(stations.long[(t-17):(t- 1),"Hiiraan_Belet_WeyneStation_Shabelle_River"], na.rm=TRUE)
    C<- median(before.long[(t-6):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    D<- future.long[(t- 3),"Nugaal_FutureRegion"]
    E<- mean(stations.long[(t-17):(t- 1),"Hiiraan_Belet_WeyneStation_Shabelle_River"], na.rm=TRUE)
    G<- tail(movavg(future.long[(t-10):(t- 1),"Jubbada_Dhexe_FutureRegion"],9,type="w"),1)
    H<- median(before.long[(t-6):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    I<- tail(movavg(current.long[(t-3):(t- 1),"Sanaag_CurrentRegion"], 2,type="m"),1)
    J<- current.long[(t- 1),"Awdal_CurrentRegion"]
    K<- mean(stations.long[(t-12):(t- 1),"Gedo_BardheereStation_Juba_River"], na.rm=TRUE)
    L<- before.long[(t- 1),"Mudug_BeforeRegion"]
    M<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    N<- current.long[(t- 1),"Awdal_CurrentRegion"]
    O<- tail(movavg(future.long[(t-14):(t- 1),"Jubbada_Hoose_FutureRegion"],13,type="w"),1)
    P<- tan(G)
    Q<- max(sum(1359.11265214998 , D , E,na.rm=TRUE), -312.023710839421*P,na.rm=TRUE)
    R<- max(B*C, Q,na.rm=TRUE)
    if ( is.na(A) ){S<- H}
    else if(A>0){S<- R }
    else{S<- H }
    U<- max(S,sum( 0.450775817558832*I , J*K , 1.41010805503524e-6*L*M*N,na.rm=TRUE),na.rm=TRUE)
    if(is.infinite(U)){U <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( U , -O,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAY9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rivers.long[(t- 4),"Juba_River_discharge"]
    B<- median(before.long[(t-6):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    C<- median(stations.long[(t-17):(t- 1),"Hiiraan_Belet_WeyneStation_Shabelle_River"], na.rm=TRUE)
    D<- current.long[(t- 1),"Awdal_CurrentRegion"]
    E<- before.long[(t- 1),"Mudug_BeforeRegion"]
    G<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    H<- conflicts.long[(t- 1),"Awdal_Conflict"]
    I<- mean(before.long[(t-6):(t- 1),"Nugaal_BeforeRegion"], na.rm=TRUE)
    J<- before.long[(t- 1),"Mudug_BeforeRegion"]
    K<- median(before.long[(t-6):(t- 1),"Gedo_BeforeRegion"], na.rm=TRUE)
    L<- max(B*C,sum( 5.21302263891898*D , 0.00211945212925433*E*G , 5.59841432052634*H*I , -0.401248993268112*J , -4.09167099427801*K,na.rm=TRUE),na.rm=TRUE)
    M<- max(A, L,na.rm=TRUE)
    if(is.infinite(M)){M <- 0 }
    FIN <-sum( M , -177.745210060968,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAY10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 4),"Sanaag_FutureRegion"]
    B<- before.long[(t- 7),"Sanaag_BeforeRegion"]
    C<- fatalities.long[(t- 15),"Galguduud_Fatalities"]
    D<- before.long[(t- 15),"Sool_BeforeRegion"]
    E<- rain.long[(t- 1),"Awdal_rain"]
    G<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    H<- current.long[(t- 1),"Awdal_CurrentRegion"]
    I<- current.long[(t- 2),"Sanaag_CurrentRegion"]
    J<- fatalities.long[(t- 1),"Shabeellaha_Hoose_Fatalities"]
    K<- median(rain.long[(t-17):(t- 1),"Nugaal_rain"], na.rm=TRUE)
    L<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    M<- tail(movavg(before.long[(t-3):(t- 1),"Mudug_BeforeRegion"], 2,type="m"),1)
    N<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    O<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    if ( is.na(E) ){P<- I}
    else if(E>0){P<- 0.000129598731155451*G*H }
    else{P<- I }
    if ( is.na(K) ){Q<- N}
    else if(K>0){Q<- 0.00203104451240726*L*M }
    else{Q<- N }
    if ( is.na(J) ){R<- 3822.7098773043}
    else if(J>0){R<- Q }
    else{R<- 3822.7098773043 }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(C)){C <- 0 }
    if(is.infinite(D)){D <- 0 }
    if(is.infinite(P)){P <- 0 }
    if(is.infinite(R)){R <- 0 }
    if(is.infinite(O)){O <- 0 }
    FIN <-sum( 0.00807681643709717*A*B*C , D , P , R , -O,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAY11arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Awdal_Conflict"]
    B<- median(future.long[(t-4):(t- 1),"Mudug_FutureRegion"], na.rm=TRUE)
    C<- rain.long[(t- 1),"Awdal_rain"]
    D<- before.long[(t- 1),"Bari_BeforeRegion"]
    E<- current.long[(t- 1),"Awdal_CurrentRegion"]
    G<- current.long[(t- 1),"Shabeellaha_Hoose_CurrentRegion"]
    H<- current.long[(t- 1),"Awdal_CurrentRegion"]
    I<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    J<- before.long[(t- 1),"Hiiraan_BeforeRegion"]
    K<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    L<- conflicts.long[(t- 1),"Awdal_Conflict"]
    M<- fatalities.long[(t- 1),"Sool_Fatalities"]
    N<- fatalities.long[(t- 1),"Sool_Fatalities"]
    O<- future.long[(t- 1),"Bari_FutureRegion"]
    P<- max(4.76749509718008*H, 1.31254556814522*I,na.rm=TRUE)
    Q<- cos(1.31254556814522*K)
    R<- min(L, 0.836842876996738*M,na.rm=TRUE)
    S<- max(J*Q, 0.836842876996738*R^2*N*O,na.rm=TRUE)
    if ( is.na(C) ){U<- S}
    else if(C>0){U<-sum( 2.71361373849448e-6*D*E*G , P,na.rm=TRUE) }
    else{U<- S }
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    if(is.infinite(U)){U <- 0 }
    FIN <-sum( A*B , U,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAY12arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- future.long[(t- 1),"Sanaag_FutureRegion"]
    B<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    C<- before.long[(t- 1),"Bay_BeforeRegion"]
    D<- median(conflicts.long[(t-4):(t- 1),"Jubbada_Dhexe_Conflict"], na.rm=TRUE)
    E<- before.long[(t- 1),"Gedo_BeforeRegion"]
    G<- conflicts.long[(t- 5),"Hiiraan_Conflict"]
    H<- future.long[(t- 3),"Nugaal_FutureRegion"]
    I<- tail(movavg(current.long[(t-11):(t- 1),"Woqooyi_Galbeed_CurrentRegion"], 10,type="m"),1)
    J<- median(before.long[(t-9):(t- 1),"Bay_BeforeRegion"], na.rm=TRUE)
    K<- fatalities.long[(t- 5),"Woqooyi_Galbeed_Fatalities"]
    L<- rain.long[(t- 1),"Shabeellaha_Dhexe_rain"]
    M<- factorial(G)
    N<- factorial(K)
    O<- max(sum(1.76598174585614*A , 4.94630290273755e-5*B^2 , 2.97984436675825e-5*C^2*D , 1.76598174585614*E/M , H , I , J,na.rm=TRUE), N,na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(L)){L <- 0 }
    FIN <-sum( O , -L,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAY13arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    C<- current.long[(t- 1),"Awdal_CurrentRegion"]
    D<- rain.long[(t- 1),"Jubbada_Hoose_rain"]
    E<- median(current.long[(t-12):(t- 1),"Bari_CurrentRegion"], na.rm=TRUE)
    G<- before.long[(t- 1),"Mudug_BeforeRegion"]
    H<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    I<- mean(future.long[(t-11):(t- 1),"Nugaal_FutureRegion"], na.rm=TRUE)
    J<- before.long[(t- 15),"Sool_BeforeRegion"]
    K<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    L<- conflicts.long[(t- 1),"Awdal_Conflict"]
    M<- fatalities.long[(t- 1),"Sool_Fatalities"]
    N<- future.long[(t- 1),"Bari_FutureRegion"]
    O<- log(D)
    P<- max(A,sum( 0.000127177740817059*B*C , O*E , 2.11983998917739e-6*G*H*I , J,na.rm=TRUE),na.rm=TRUE)
    Q<- max(sum(P , -K,na.rm=TRUE), 0.805982905429227*L^2*M*N,na.rm=TRUE)
    FIN <-Q
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAY14arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- fatalities.long[(t- 13),"Gedo_Fatalities"]
    B<- future.long[(t- 3),"Nugaal_FutureRegion"]
    C<- tail(movavg(current.long[(t-4):(t- 1),"Hiiraan_CurrentRegion"], 3,type="m"),1)
    D<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    E<- current.long[(t- 1),"Awdal_CurrentRegion"]
    G<- before.long[(t- 1),"Mudug_BeforeRegion"]
    H<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    I<- tail(movavg(current.long[(t-15):(t- 1),"Woqooyi_Galbeed_CurrentRegion"],14,type="w"),1)
    J<- future.long[(t- 2),"Hiiraan_FutureRegion"]
    K<- conflicts.long[(t- 1),"Awdal_Conflict"]
    L<- fatalities.long[(t- 1),"Sool_Fatalities"]
    M<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    N<- future.long[(t- 6),"Togdheer_FutureRegion"]
    if ( is.na(A) ){O<- C}
    else if(A>0){O<- B }
    else{O<- C }
    P<- max(sum(0.000132337180250834*D*E , 6.60533916454304e-7*G*H*I , -J,na.rm=TRUE), 1.07889037646398*K*L*M*N,na.rm=TRUE)
    Q<- max(565, P,na.rm=TRUE)
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    FIN <-sum( O , Q,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAY15arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- median(current.long[(t-13):(t- 1),"Bari_CurrentRegion"], na.rm=TRUE)
    B<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    C<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    D<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    E<- current.long[(t- 1),"Awdal_CurrentRegion"]
    G<- median(fatalities.long[(t-14):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    H<- current.long[(t- 4),"Mudug_CurrentRegion"]
    I<- median(fatalities.long[(t-14):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    J<- fatalities.long[(t- 1),"Togdheer_Fatalities"]
    K<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    L<- conflicts.long[(t- 1),"Awdal_Conflict"]
    M<- fatalities.long[(t- 1),"Bari_Fatalities"]
    N<- future.long[(t- 7),"Sanaag_FutureRegion"]
    O<- median(fatalities.long[(t-5):(t- 1),"Nugaal_Fatalities"], na.rm=TRUE)
    P<- (2.94699444169742*E)
    Q<- (1.09149994990758*H)
    R<- not(J*K)
    S<- max(sum(1.13231747567167*B , C*D , P^G , Q^I*R,na.rm=TRUE), 1.88261672354918*L*M*N*O,na.rm=TRUE)
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(S)){S <- 0 }
    FIN <-sum( A , S,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAY16arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- rain.long[(t- 11),"Hiiraan_rain"]
    B<- before.long[(t- 15),"Sool_BeforeRegion"]
    C<- water.long[(t- 1),"Bakool_WaterDrumPrice"]
    D<- current.long[(t- 1),"Awdal_CurrentRegion"]
    E<- before.long[(t- 15),"Sool_BeforeRegion"]
    G<- fatalities.long[(t- 1),"Bay_Fatalities"]
    H<- before.long[(t- 1),"Mudug_BeforeRegion"]
    I<- before.long[(t- 1),"Shabeellaha_Dhexe_BeforeRegion"]
    J<- current.long[(t- 1),"Hiiraan_CurrentRegion"]
    K<- current.long[(t- 1),"Awdal_CurrentRegion"]
    L<- fatalities.long[(t- 1),"Awdal_Fatalities"]
    M<- before.long[(t- 1),"Mudug_BeforeRegion"]
    N<- rain.long[(t- 11),"Hiiraan_rain"]
    O<- before.long[(t- 15),"Sool_BeforeRegion"]
    P<- tail(movavg(conflicts.long[(t-9):(t- 1),"Awdal_Conflict"],8,type="w"),1)
    Q<- before.long[(t- 1),"Mudug_BeforeRegion"]
    R<- fatalities.long[(t- 1),"Bay_Fatalities"]
    S<- before.long[(t- 1),"Mudug_BeforeRegion"]
    U<- fatalities.long[(t- 1),"Bay_Fatalities"]
    if ( is.na(G) ){V<- J}
    else if(G>0){V<- 0.00214195458986615*H*I }
    else{V<- J }
    W<- max(A*B,sum( 0.00015635886853926*C*D , E , V,na.rm=TRUE),na.rm=TRUE)
    X<- sin(P)
    if ( is.na(R) ){Y<- U}
    else if(R>0){Y<- S }
    else{Y<- U }
    if(is.infinite(W)){W <- 0 }
    if(is.infinite(K)){K <- 0 }
    if(is.infinite(L)){L <- 0 }
    if(is.infinite(M)){M <- 0 }
    if(is.infinite(N)){N <- 0 }
    if(is.infinite(O)){O <- 0 }
    if(is.infinite(X)){X <- 0 }
    if(is.infinite(Q)){Q <- 0 }
    if(is.infinite(Y)){Y <- 0 }
    FIN <-sum( W , -K , -L*M , -N*O*X , -1.94583981584402e-5*Q*Y,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

modelarrivals_BAY17arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- current.long[(t- 1),"Awdal_CurrentRegion"]
    B<- current.long[(t- 15),"Sool_CurrentRegion"]
    C<- median(current.long[(t-13):(t- 1),"Jubbada_Dhexe_CurrentRegion"], na.rm=TRUE)
    D<- current.long[(t- 1),"Sanaag_CurrentRegion"]
    E<- before.long[(t- 1),"Bari_BeforeRegion"]
    G<- before.long[(t- 1),"Woqooyi_Galbeed_BeforeRegion"]
    H<- future.long[(t- 10),"Nugaal_FutureRegion"]
    I<- future.long[(t- 1),"Jubbada_Hoose_FutureRegion"]
    J<- future.long[(t- 11),"Hiiraan_FutureRegion"]
    K<- current.long[(t- 1),"Togdheer_CurrentRegion"]
    L<- median(current.long[(t-13):(t- 1),"Jubbada_Dhexe_CurrentRegion"], na.rm=TRUE)
    M<- max(sum(1.93731763498863*D , 2.35635991468814e-8*E^3*G,na.rm=TRUE), 2.14796113697192*H,na.rm=TRUE)
    N<- max(C, M,na.rm=TRUE)
    O<- max(B, N,na.rm=TRUE)
    P<- max(sum(4.83180158409821*A , O , -I , -J , -0.440155682027959*K,na.rm=TRUE), L,na.rm=TRUE)
    FIN <-P
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BAY2016_1arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Bari_Conflict"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    FIN <-sum( 992.981037629729 , 0.0402404975169252*A*B,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BAY2016_2arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Bari_BeforeRegion"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    FIN <-sum( 702.083573146598 , 0.00810841192540213*A*B,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BAY2016_3arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- tail(movavg(goats.long[(t-8):(t- 1),"Bay_goatprice"],7,type="w"),1)
    B<- asinh(A)
    if(is.infinite(B)){B <- 0 }
    FIN <-sum( 45422.4554412277 , -3047.86038402401*B,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BAY2016_4arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Bari_Conflict"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    FIN <-sum( 992.981037629729 , 0.0402404975169252*A*B,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BAY2016_5arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- before.long[(t- 1),"Bay_BeforeRegion"]
    B<- goats.long[(t- 1),"Sanaag_goatprice"]
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    FIN <-sum( 743.593401837418 , 2.26553905555862e-7*A*B,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BAY2016_6arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Bari_Conflict"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    FIN <-sum( 992.981037629729 , 0.0402404975169252*A*B,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BAY2016_7arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Bari_Conflict"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    FIN <-sum( 992.981037629729 , 0.0402404975169252*A*B,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BAY2016_8arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Bari_Conflict"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    FIN <-sum( 815.423726638684 , 0.0429828622146568*A*B,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BAY2016_9arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Bari_Conflict"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    FIN <-sum( 992.981037629729 , 0.0402404975169252*A*B,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}

BAY2016_10arrivals <- function(start, end){
  start = 20
  PI <- PA <- PD <- rep(NA, end)
  for (t in start:end){ 
    
    A<- conflicts.long[(t- 1),"Bari_Conflict"]
    B<- before.long[(t- 1),"Bay_BeforeRegion"]
    if(is.infinite(A)){A <- 0 }
    if(is.infinite(B)){B <- 0 }
    FIN <-sum( 992.981037629729 , 0.0402404975169252*A*B,na.rm=TRUE)
    PA[t] <- FIN
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
  }
  write.csv(as.data.frame(PA[1:length(PA)]), file ="results.csv",row.names = FALSE)
  return(PA)
}



