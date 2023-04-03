

######## packages ##########
install.packages('magrittr')
install.packages('maxLik')
install.packages('readxl')
install.packages('ggplot2')
install.packages('GGally')
#install.packages('apollo')
install.packages('randtoolbox')
install.packages('RCurl')
install.packages('readxl')
install.packages('openxlsx')
library(openxlsx)
library(RCurl)
library(randtoolbox)
#library(apollo)
library(GGally)
library(maxLik)
library(magrittr)
library(readxl)
library(ggplot2)
require(ggiraphExtra)
require(ggiraph)

############ files ################
filenames="https://raw.githubusercontent.com/bastianhj/2steptask-t/main/filenames.dat" 
filenames<-read.table(url(filenames),header=FALSE)
lista<-vector()
for(i in 1:nrow(filenames)){
  file<-filenames[i,1]
  path<-paste("https://raw.githubusercontent.com/bastianhj/2steptask-t/main/",file,sep="")
  lista[i]<-path
}
reporturl<-"https://raw.githubusercontent.com/bastianhj/2steptask-t/main/SelfReportTest.xlsx"
report<-read.xlsx((reporturl))
routineurl<-"https://raw.githubusercontent.com/bastianhj/2steptask-t/main/Routine.dat"
routine<-read.table(url(routineurl),header=TRUE)
autourl<-"https://raw.githubusercontent.com/bastianhj/2steptask-t/main/Auto.dat"
auto<-read.table(url(autourl),header=TRUE)
report$Q5[3]<-'6-8'

N<-length(lista)
data<-vector()
atq<-vector()

for (i in 1:N){
bd<-read.csv(url(lista[i]))
s<-paste(bd$participant[1],bd$session[1],sep="-")
bd<-bd[-(is.na(bd$Stay))==FALSE,]
data<-rbind(data,cbind(Subj=s,Rep=bd$trials.thisRepN,thisN=bd$trials_2.thisN,
                       Trans=bd$Transitions,Rew=bd$Rewards,Mode=bd$Mode,Stay=bd$Stay,Neigh=bd$Neighbour))
bd<-read.csv(url(lista[i]))
bd<-bd[(bd$Resp_final_1!=""),]
atq<-rbind(atq,c(Subj=s,P1=bd$Resp_final_1,P2=bd$Resp_final_2,P3=bd$Resp_final_3))%>%data.frame
}

data<-as.data.frame(data)
data<-cbind(Subj=data$Subj,exp=as.numeric(data$thisN)+1,trial=as.numeric(data$Rep)+1,
            Transitions=data$Trans,Rewards=data$Rew,Mode=data$Mode,Stay=data$Stay,Neighbour=data$Neigh)%>%data.frame
I<-ncol(data)
for (i in 1:N){
  bd<-read.csv(url(lista[i]))
  s<-paste(bd$participant[i],bd$session[i],sep="-")
data[data$Subj==s,(I+1):(I+ncol(atq)-1)]=atq[atq$Subj==s,2:ncol(atq)]
  
}
#chequear cuantas obs hay de cada uno
subj_list<-vector()
k=1
for (i in 1:N){
  bd<-read.csv(url(lista[i]))
  s<-paste(bd$participant[i],bd$session[i],sep="-")
  if(sum(data$exp==1 & data$Subj==s)>0){
    subj_list[k]<-s 
    k=k+1
  }
  
  
  print(c(sum(data$exp==2 & data$Subj==s),s))
  
}

 subj_list<-subj_list[-c(17,23)]
 data<-data[-(which( (data$Subj)=="10-2" | (data$Subj)=="8-2")),]
 attach(data)
 
################################### MODEL BPLD (base+Probability Learning + Deacayment of Non-chosen alternatives)

Tot<-34
# Nstage=2
# Nstates=c(1,2)        
# a_set=c(1,2) #1=Vecino A, 2=Vecino B
e<-1
N<-200
S<-length(subj_list)

#stay<-as.numeric(Stay)
Choice<-(Neighbour=="['N_A']")*1+(Neighbour=="['N_B']")*2+(Neighbour=="['N_C_3']")*3+ 
        (Neighbour=="['N_A_3']")*1+(Neighbour=="['N_B_3']")*2

RL1<-function(beta){
  w<-beta[3]
  lambda<-beta[4]
  b<-(beta[5])
  pp<-beta[6]
  theta<-beta[7]
  #for (k in 1:S){
    s<-subj_list[k]
    pstay<-vector()
    pstay[1]<-0.5
    delta1<-matrix(0,nrow=(N+1),ncol=1)
    delta2<-matrix(0,nrow=(N+1),ncol=1)
    Q_net<-matrix(0,nrow=(N+1),ncol=3)
    subsample<-which((as.numeric(exp)==e & (Subj)==s))[1:N]
    subsample<-c(subsample,which((as.numeric(exp)==2 & (Subj)==s))[1])
    
    P_transA<-matrix(0,nrow=(N+1),ncol=3)
    P_transB<-matrix(0,nrow=(N+1),ncol=3)
    P_transA[1,]<-c(0.5,0.5,0) #inicialmente creo que ambos vecinos me llevan equiprobablemente a cada bus
    P_transB[1,]<-c(0.5,0.5,0)
    stay<-as.numeric(Stay[subsample])[2:(N+1)]
    Transitions_obs<-as.numeric(Transitions[subsample])[2:(N+1)]
    Rewards_obs<-as.numeric(Rewards[subsample])[2:(N+1)]
    modo<-data$Mode[subsample][2:(N+1)]
    Choice_obs<-Choice[subsample][1:(N)]
    QMF_s1_ai=matrix(0,nrow=(N+1),ncol=3)
    QMF_s1_ai[1,]=c(0,0,0) #inicialmente la persona cree, independiente de
    #que ambas decisiones lo harán llegar temprano
    QMF_s2=matrix(0,nrow=(N+1),ncol=3)
    QMF_s2[1,]=c(0,0,0)#inicialmente la persona cree, independiente de
    #que ambos buses decisiones lo harán llegar temprano
    QMB=matrix(0,nrow=(N+1),ncol=3)
    QMB[1,]=c(0,0,0)
    
    Q_net[1,]<-c(0,0,0)
    #tau<-as.numeric(Time[subsample])
    rep_ai<-vector()
    stress<-as.numeric(atq$I5[s])
    
    #modo<-datos_MC$modo[datos_MC$Subj==s]
    
    #Neighbour_obs<-Neighbour[subsample]
    
    
    for (i in 1:(N)){

      R<-Rewards_obs[i]
      #Q neto
      #beta2<-c(alpha,lambda,b,pp,i,s)
      m<-(modo[i]=="rojo")*1+(modo[i]=="azul")*2+(modo[i]=="verde")*3
      
      #MF
      j=(Choice_obs[i]==1)*1+(Choice_obs[i]==2)*2+(Choice_obs[i]==3)*3
      
      Q1<-QMF_s1_ai[i,j] #valor vecino
      Q2<-QMF_s2[i,m]#valor modo
      
      delta1[i,1]=Q2+R*lambda-Q1
      
      if(delta1[i,1]>=0){
        alpha<-beta[1]
      }else{
        alpha<-beta[2]
      }
      QMF_s1_ai[i+1,j]=Q1+alpha*delta1[i,1]
      QMF_s1_ai[i+1,-j]=QMF_s1_ai[i,-j]*(1-alpha)
      
      delta2[i,1]=R-Q2#
      
      if(delta2[i,1]>=0){
        alpha<-beta[1]
      }else{
        alpha<-beta[2]
      }
      QMF_s2[i+1,m]=Q2+alpha*delta2[i,1]
      QMF_s2[i+1,-m]=QMF_s2[i,-m]*(1-alpha)
      
      ##### MB
      
      # Probability learning
      rep_ai<-c(1,0,0)*(j==1)+c(0,1,0)*(j==2)+c(0,0,1)*(j==3)

      if(j==1){
            P_transA[i+1,m]=P_transA[i,m]*(1-theta)+theta
            P_transA[i+1,-m]=P_transA[i,-m]*(1-theta)
            P_transB[i+1,]=P_transB[i,]
      }else{
            P_transB[i+1,m]=P_transB[i,m]*(1-theta)+theta
            P_transB[i+1,-m]=P_transB[i,-m]*(1-theta)
            P_transA[i+1,]=P_transA[i,]
      }


      rep_ai<-c(1,0,0)*(j==1)+c(0,1,0)*(j==2)+c(0,0,1)*(j==3)
    #  QMB[i+1,j]<-t(P_trans)%*%(QMF_s2[i+1,])  
      QMB[i+1,1]<-t(P_transA[i+1,])%*%(QMF_s2[i+1,])  
      QMB[i+1,2]<-t(P_transB[i+1,])%*%(QMF_s2[i+1,])  

        
      Q_net[i+1,]<-w*QMB[i+1,]+(1-w)*QMF_s1_ai[i+1,]
      # #con esto se calculan las probabilidades 
      # #should I stay or should I go now
       p<-exp(Q_net[i+1,1:2]*rep_ai[1:2]*b*pp)/sum(exp(Q_net[i+1,1:2]*rep_ai[1:2]*b*pp)) #probabilidad de elegir cada alternativa 
       pstay[i+1]<-t(p)%*%rep_ai[1:2]
      
      #p<-exp(Q_net[i+1,1:2]*b*pp)/sum(exp(Q_net[i+1,1:2]*b*pp))
      #pstay[i+1]<-p[j] #probabilidad de que en la siguiente elección elija j

      # pMB<-exp(QMB[i+1,1:2]*rep_ai[1:2]*b*pp)/sum(exp(QMB[i+1,1:2]*rep_ai[1:2]*b*pp)) #probabilidad de elegir cada alternativa
      # pMF<-exp(QMF_s1_ai[i+1,1:2]*rep_ai[1:2]*b*pp)/sum(exp(QMF_s1_ai[i+1,1:2]*rep_ai[1:2]*b*pp)) 
      #  pstay[i+1]<-t(pMB*w+pMF*(1-w))%*%rep_ai[1:2]
       #probabilidad de que en la siguiente elección elija j
    }

  
  pstay<-pstay[-1]
  pstay[stay==0]<-(1-pstay[stay==0])

  P<-log(pstay)
  #}

 
  return(cbind(P,Q_net[-1,],QMF_s1_ai[-1,],
             QMF_s2[-1,],QMB[-1,],P_transA[-1,],P_transB[-1,] ))
}

RL2<-function(beta){
  LL<-RL1(beta)[,1]
  return(LL)
}

ws<-vector()
betas<-vector()
alphas1<-vector()
alphas2<-vector()
Q<-matrix(0,nrow=N,ncol=3*Tot)
QMF_s1_mat<-matrix(0,nrow=(N),ncol=3*Tot)
QMF_s2_mat<-matrix(0,nrow=(N),ncol=3*Tot)
QMB_mat<-matrix(0,nrow=(N),ncol=3*Tot)
PA<-matrix(0,nrow=(N),ncol=3*Tot)
PB<-matrix(0,nrow=(N),ncol=3*Tot)

P_mat<-vector()
sig<-vector()
test<-vector()
rho<-vector()
valid<-vector()
pars<-vector()
prob<-vector()
j<-1
A<-matrix(c(-1,0,0,0,0,0,0,
            0,-1,0,0,0,0,0,
            0,0,-1,0,0,0,0,
            0,0,0,0,0,0,-1,
            1,0,0,0,0,0,0,
            0,1,0,0,0,0,0,
            0,0,1,0,0,0,0,
            0,0,0,0,0,0,1
            ),8,7,1)
B<-c(1,1,1,1,0,0,0,0)


i<-0.1
beta<-c(alpha=i,alpha2=i,w=i,lambda=1,b=i,pp=1,theta=i)

for (k in 1:Tot){

print(summary(RL <- maxLik(RL2, start=beta, method="BFGS", iterlim=100, finalHessian='bhhh',fix=c(4,6),constraints = list(ineqA=A,ineqB=B))))

ws[k]<-RL$estimate[3]
betas[k]<-RL$estimate[5]
alphas1[k]<-RL$estimate[1]
alphas2[k]<-RL$estimate[2]
L0<-log(0.5)*(N-1)
LB<-RL$maximum
rho[k]<-1-(LB/L0)
LT<--2*(L0-LB)

if(LT>=qchisq(0.95,5)){
  test[k]<-1
}else{test[k]<-0}
  
if(summary(RL)$estimate[3,4]<=0.1){
  sig[k]<-TRUE
}else{sig[k]<-FALSE}
if(summary(RL)$estimate[1,2]!="Inf"){
  valid[k]<-1
}else{valid[k]<-0}

Q[,(3*k-2)]<-c(RL1(RL$estimate)[,2])
Q[,(3*k-1)]<-c(RL1(RL$estimate)[,3])
Q[,(3*k)]<-c(RL1(RL$estimate)[,4])

QMF_s1_mat[,(3*k-2)]<-c(RL1(RL$estimate)[,5])
QMF_s1_mat[,(3*k-1)]<-c(RL1(RL$estimate)[,6])
QMF_s1_mat[,(3*k)]<-c(RL1(RL$estimate)[,7])

QMF_s2_mat[,(3*k-2)]<-c(RL1(RL$estimate)[,8])
QMF_s2_mat[,(3*k-1)]<-c(RL1(RL$estimate)[,9])
QMF_s2_mat[,(3*k)]<-c(RL1(RL$estimate)[,10])

QMB_mat[,(3*k-2)]<-c(RL1(RL$estimate)[,11])
QMB_mat[,(3*k-1)]<-c(RL1(RL$estimate)[,12])
QMB_mat[,(3*k)]<-c(RL1(RL$estimate)[,13])

PA[,(3*k-2)]<-c(RL1(RL$estimate)[,14])
PA[,(3*k-1)]<-c(RL1(RL$estimate)[,15])
PA[,(3*k)]<-c(RL1(RL$estimate)[,16])

PB[,(3*k-2)]<-c(RL1(RL$estimate)[,17])
PB[,(3*k-1)]<-c(RL1(RL$estimate)[,18])
PB[,(3*k)]<-c(RL1(RL$estimate)[,19])

s<-subj_list[k]
subsample<-which((as.numeric(exp)==e & (Subj)==s))[1:N]
stay<-as.numeric(Stay[subsample])[-seq(1,N*S,N)]
Transitions_obs<-as.numeric(Transitions[subsample])[-seq(1,N*S,N)]
Rewards_obs<-as.numeric(Rewards[subsample])[-seq(1,N*S,N)]

pstay<-exp(c(RL1(RL$estimate)[,1]))
pstay[stay==0]<-(1-pstay[stay==0])


P_mat<-rbind(P_mat,cbind(k,pstay,Transitions_obs,Rewards_obs))
}

p_medians<-vector()
P_mat<-as.data.frame(P_mat)
par(mfrow=c(4,5))

for (k in 1:Tot){
  
  p1<-(P_mat[P_mat$k==k & P_mat$Transitions_obs==1 & P_mat$Rewards_obs==1,2])
  p2<-(P_mat[P_mat$k==k & P_mat$Transitions_obs==-1 & P_mat$Rewards_obs==1,2])
  p3<-(P_mat[P_mat$k==k & P_mat$Transitions_obs==1 & P_mat$Rewards_obs==-1,2])
  p4<-(P_mat[P_mat$k==k & P_mat$Transitions_obs==-1 & P_mat$Rewards_obs==-1,2])
  
  p_medians<-(cbind(RC=p1,RR=p2,UC=p3,UR=p4))
  valid[k]
  if(test[k]==1 & valid[k]==1){
    if(sig[k]==TRUE){
      boxplot(p_medians,main=bquote("w= "~.(round(ws[k],3))~"*;"~rho~"= "~.(round(rho[k],2))),ylim=c(0,1),ylab="Pstay",col=3)
      box(col="green")
    }else{
      boxplot(p_medians,main=bquote("w= "~.(round(ws[k],3))~";"~rho~"= "~.(round(rho[k],2))),ylim=c(0,1),ylab="Pstay",col=3)
      box(col="green")
    }

  }else{
    if(sig[k]==TRUE){
    boxplot(p_medians,main=bquote("w= "~.(round(ws[k],3))~"*;"~rho~"= "~.(round(rho[k],2))),ylim=c(0,1),ylab="Pstay")
    }else{
    boxplot(p_medians,main=bquote("w= "~.(round(ws[k],3))~";"~rho~"= "~.(round(rho[k],2))),ylim=c(0,1),ylab="Pstay")
      
    }
    
    }
  
}

ws_BPLD<-ws
test_BPLD<-test
valid_BPLD<-valid
betas_BPLD<-betas
alphas1_BPLD<-alphas1
alphas2_BPLD<-alphas2

condBPLD<-(valid_BPLD==1 & test_BPLD==1)
La<-sum((1-rho[condBPLD])*L0)
L0t<-L0*sum(condBPLD)
Rho_BPLD<-1-(La-4)/L0t
Rho_BPLD
mean(ws_BPLD[condBPLD])
sum(ws_BPLD[condBPLD]<0.5)/length(ws_BPLD[condBPLD])
NBPLD<-sum(condBPLD)

############################# Exploration model BPLD

e<-2 #experimento parte 2
N2<-200 
S<-length(subj_list)
modo<-vector()

RL3<-function(beta){
  w<-beta[3]
  lambda<-beta[4]
  b<-(beta[5])
  pp<-beta[6]
  theta<-beta[7]
  #for (k in 1:S){
  P_transA<-matrix(0,nrow=(N2),ncol=3)
  P_transB<-matrix(0,nrow=(N2),ncol=3)
  P_transC<-matrix(0,nrow=(N2),ncol=3)
  P_transA[1,]<-PA[(N2-1),(3*k-2):(3*k)]     #probabilidades aprendidas en la parte anterior
  P_transB[1,]<-PB[(N2-1),(3*k-2):(3*k)]  
  P_transC[1,]<-c(1/3,1/3,1/3)
  delta1<-matrix(0,nrow=N2,ncol=1)
  delta2<-matrix(0,nrow=N2,ncol=1)
  Q_net<-matrix(0,nrow=(N2),ncol=3)
  pstay_glob<-vector()
  pstay<-vector()
  pstay[1]<-1/3
 # pstay[1]<-P_mat[P_mat$k==k,2][sum(P_mat$k==k)]
  alt_choice<-vector()
  s<-subj_list[k]
  
  subsample<-which((as.numeric(exp)==e & (Subj)==s))
  #subsample<-c(which((as.numeric(exp)==1 & (Subj)==s))[N],subsample)
  stay<-as.numeric(Stay[subsample])[2:(N2)]
  Transitions_obs<-as.numeric(Transitions[subsample])[2:(N2)]
  Rewards_obs<-as.numeric(Rewards[subsample])[2:(N2)]
  Choice_obs<-Choice[subsample][1:(N2)]
  modo<-data$Mode[subsample][2:(N2)]


  
  QMF_s1_ai=matrix(0,nrow=(N2),ncol=3)
  QMF_s1_ai[1,]=QMF_s1_mat[(N),(3*k-2):(3*k)] 
#  QMF_s1_ai[1,3]=max(QMF_s1_ai[1,])+abs(max(QMF_s1_ai[1,]))*0.01
  
  QMF_s2=matrix(0,nrow=(N2),ncol=3)
  QMF_s2[1,]=QMF_s2_mat[(N),(3*k-2):(3*k)] 
 # QMF_s2[1,3]=max(QMF_s2[1,])+abs(max(QMF_s2[1,]))*0.01
  
  QMB=matrix(0,nrow=(N2),ncol=3)
  QMB[1,]=QMB_mat[(N),(3*k-2):(3*k)]
 # QMB[1,3]=max(QMB[1,])+abs(max(QMB[1,]))*0.01

  rep_ai<-vector()
  stress<-as.numeric(atq$I5[s])

  for (i in 1:(N2-1)){
    R<-Rewards_obs[i]
    
    m<-(modo[i]=="rojo")*1+(modo[i]=="azul")*2+(modo[i]=="verde")*3
    
    #MF
    j=(Choice_obs[i]==1)*1+(Choice_obs[i]==2)*2+(Choice_obs[i]==3)*3
    
    Q1<-QMF_s1_ai[i,j]
    Q2<-QMF_s2[i,m]
    
    delta1[i,1]=Q2+R*lambda-Q1
    
    if(delta1[i,1]>0){
      alpha<-beta[1]
    }else{
      alpha<-beta[2]
    }
    QMF_s1_ai[i+1,j]=Q1+alpha*delta1[i,1]
    
    QMF_s1_ai[i+1,-j]=QMF_s1_ai[i,-j]*(1-alpha)
    
    delta2[i,1]=R-Q2#cambiar el index i por i+1#beta_r puede ser diferente p/c sujeto
    
    if(delta2[i,1]>0){
      alpha<-beta[1]
    }else{
      alpha<-beta[2]
    }
    QMF_s2[i+1,m]=Q2+alpha*delta2[i,1]
    QMF_s2[i+1,-m]= QMF_s2[i,-m]*(1-alpha)
    
    #MB
     if(j==1){
            P_transA[i+1,m]=P_transA[i,m]*(1-theta)+theta
            P_transA[i+1,-m]=P_transA[i,-m]*(1-theta)
            P_transB[i+1,]=P_transB[i,]
            P_transC[i+1,]=P_transC[i,]

            P_trans<-P_transA[i+1,]
      }
      if(j==2){
            P_transB[i+1,m]=P_transB[i,m]*(1-theta)+theta
            P_transB[i+1,-m]=P_transB[i,-m]*(1-theta)
            P_transA[i+1,]=P_transA[i,]
            P_transC[i+1,]=P_transC[i,]

            P_trans<-P_transB[i+1,]
      }
      if(j==3){
            P_transC[i+1,m]=P_transC[i,m]*(1-theta)+theta
            P_transC[i+1,-m]=P_transC[i,-m]*(1-theta)
            P_transB[i+1,]=P_transB[i,]
            P_transA[i+1,]=P_transA[i,]

            P_trans<-P_transC[i+1,]
      }

    
    QMB[i+1,1]<-t(P_transA[i+1,])%*%(QMF_s2[i+1,]) #esto es Bellman Equation (Bellman, 1957)
    QMB[i+1,2]<-t(P_transB[i+1,])%*%(QMF_s2[i+1,])
    QMB[i+1,3]<-t(P_transC[i+1,])%*%(QMF_s2[i+1,])
    
    ### Editar para CLASES LATENTES
    #L<-gamma0+gamma1*stress
    #w<-1/(1+exp(L))
    
    Q_net[i+1,]<-w*QMB[i+1,]+(1-w)*QMF_s1_ai[i+1,]
    
    
    #con esto se calculan las probabilidades
    
     rep_ai<-c(1,0,0)*(j==1)+c(0,1,0)*(j==2)+c(0,0,1)*(j==3)
    
     p<-exp(Q_net[i+1,]*rep_ai*b*pp)/sum(exp(Q_net[i+1,]*rep_ai*b*pp)) #probabilidad de elegir cada alternativa
     pstay[i+1]<-t(p)%*%rep_ai #probabilidad de que en la siguiente elección elija j

    #p<-exp(Q_net[i+1,]*b*pp)/sum(exp(Q_net[i+1,]*b*pp))
    #pstay[i+1]<-p[j] #probabilidad de que en la siguiente elección elija j

    # pMB<-exp(QMB[i+1,1:3]*rep_ai[1:3]*b*pp)/sum(exp(QMB[i+1,1:3]*rep_ai[1:3]*b*pp)) #probabilidad de elegir cada alternativa
    # pMF<-exp(QMF_s1_ai[i+1,1:3]*rep_ai[1:3]*b*pp)/sum(exp(QMF_s1_ai[i+1,1:3]*rep_ai[1:3]*b*pp)) 

    #  pstay[i+1]<-t(pMB*w+pMF*(1-w))%*%rep_ai[1:3]
    
  }
  # if ((Choice_obs)[N2+1]==j){
  #   stay[N2+1]<-1
  # }else{stay[N2+1]<-0}
  
  #stay<-stay[-c(1)]
  pstay<-pstay[-c(1)]
  pstay[stay==0]<-(1-pstay[stay==0])
  
  P<-log(pstay)

  return(cbind(P,Q_net[-1,],QMF_s1_ai[-1,], QMF_s2[-1,],QMB[-1,],P_transA[-1,],P_transB[-1,],P_transC[-1,]))

}

RL4<-function(beta){
  LL<-RL3(beta)[,1]
  return(LL)
}


j=1
pars<-vector()
prob<-vector()
A<-matrix(c(-1,0,0,0,0,0,0,
            0,-1,0,0,0,0,0,
            0,0,-1,0,0,0,0,
            0,0,0,0,0,0,-1,
            1,0,0,0,0,0,0,
            0,1,0,0,0,0,0,
            0,0,1,0,0,0,0,
            0,0,0,0,0,0,1
            ),8,7,1)
B<-c(1,1,1,1,0,0,0,0)


i<-0.1



# for(i in seq(0.1,1,0.1)){

Q_ex<-matrix(0,nrow=(N2-1),ncol=Tot*3)
Q_max<-matrix(0,nrow=(N2-1),ncol=Tot)
Q_min<-matrix(0,nrow=(N2-1),ncol=Tot)
Exp<-vector()
ws2<-vector()

beta<-c(alpha=i,alpha2=i,w=i,lambda=1,b=i,pp=1,theta=i)
betas<-vector()
test2<-vector()
valid2<-vector()
L<-round((N2-1))
for(k in 1:Tot){
  s<-subj_list[k]
((RL <- maxLik(RL4, start=beta, method="BFGS", iterlim=100, finalHessian='bhhh',fix=c(4,6),constraints = list(ineqA=A,ineqB=B))))
  Q_ex[,(3*k-2)]<-c(RL3(RL$estimate)[,2])
  Q_ex[,(3*k-1)]<-c(RL3(RL$estimate)[,3])
  Q_ex[,(3*k)]<-c(RL3(RL$estimate)[,4])
  ws2[k]<-RL$estimate[3]
  betas[k]<-RL$estimate[5]
  
  L0<-log(1/2)*(N2-1)
  LB<-RL$maximum
  rho[k]<-1-(LB/L0)
  LT<--2*(L0-LB)
  
  if(LT>=qchisq(0.95,5)){
    test2[k]<-1
  }else{test2[k]<-0}
  
 if(summary(RL)$estimate[1,2]!="Inf"){
  valid2[k]<-1
}else{valid2[k]<-0}

 for (t in 1:(L)){ 
 Q_max[t,k]<- which.max(Q_ex[t,(3*k-2):(3*k)])
 Q_min[t,k]<- which.min(Q_ex[t,(3*k-2):(3*k)])
 }
}

ws2_BPLD<-ws2
test2_BPLD<-test2
valid2_BPLD<-valid2
betas2_BPLD<-betas

 Exp<-vector()
  E<-199
  Exp1<-vector()
  Exp2<-vector()
  for(k in 1:Tot){
    s<-subj_list[k]
    subsample<-which(as.numeric(exp)==2 & (Subj)==s)[2:N2]
    subsample2<-which(as.numeric(exp)==2 & (Subj)==s)[1:(N2-1)]
    subsample3<-seq((1+(N2-1)*(k-1)),((N2-1)*k),1)
    Choice_obs<-Choice[subsample]
    Choice_obs2<-Choice[subsample2]
    L<-sum(Q_max[1:E,k]!=3)
   Exp1[k]<-100*sum(Choice_obs==3 & Q_max[1:E,k]!=3)/E
   Exp2[k]<-100*sum(Choice_obs==3 & Choice_obs2 !=3)/E
}

Exp_BPLD1<-Exp1
Exp_BPLD2<-Exp2
