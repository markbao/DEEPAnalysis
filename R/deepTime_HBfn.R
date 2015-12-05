
#################################
## Hierarchical Bayes Time Fns ##
#################################

#' @title
#' Estimation of Time Preference Parameters Using Hierarchical Markov Chain Mote Carlo Bayesian Framework
#'
#' @description
#' \code{deepTimeHB} Estimates Risk Preferences and outputs results in current working directory.
#'
#' @import MSBVAR
#' @import MCMCpack
#'
#' @return Outputs multiple files with Time Preference Parameters.
#' @export

#******* Originally called Time_Unbounded_100712.R *********#

deepTimeHB <- function()
{

  #################################
  #################################
  #####      1. Vtimeqh function
  #################################
  #################################

  Vtimeqh <- function (x, t, beta, r, delta){
    #Value of receiving x in t days.

    if (t==0){
      value <- x
    }	else {
      value <- x*beta*exp(-r*t)
    }
    return(delta*value)
  }



  #################################
  #################################
  #####      2. hbfielddata_time function
  #################################
  #################################

  hbfielddata_time <- function(spec, study_ID){
    # estimates time discounting parameters from field data

    nargin <- length(as.list(match.call())) -1

    if (nargin==1){
      serials=read.csv('serials_time.csv', header=FALSE)
      adaptive_questions=read.csv('adaptive_questions_time.csv', header=FALSE)
      choices=read.csv('choices_time.csv', header=FALSE)
    }	else{
      filename = sprintf('serials_time_%d.csv',study_ID)
      serials=read.csv(filename, header=FALSE)
      filename = sprintf('adaptive_questions_time_%d.csv',study_ID)
      adaptive_questions=read.csv(filename, header=FALSE)
      filename = sprintf('choices_time_%d.csv',study_ID)
      choices=read.csv(filename, header=FALSE)
    }

    all_questions_paths <- DEEP::all_questions_paths_time
#     all_questions_paths=read.csv('all_questions_paths_time.csv', header=FALSE)
    ngambles=length(adaptive_questions)
    nresp=length(t(serials))

    gambles1=matrix(0,nresp*ngambles,2)
    gambles2=matrix(0,nresp*ngambles,2)
    k=0


    for (i in 1:nresp){
      for (j in 1:ngambles){
        k=k+1
        questionid=adaptive_questions[i,j]
        question=all_questions_paths[questionid+1,]
        if (question[1] != questionid){
          print('error')
        }
        choice=choices[i,j]
        if (choice==1){
          gambles1[k,1]=question[,4]
          gambles1[k,2]=question[,5]
          gambles2[k,1]=question[,6]
          gambles2[k,2]=question[,7]
        }else{
          gambles1[k,1]=question[,6]
          gambles1[k,2]=question[,7]
          gambles2[k,1]=question[,4]
          gambles2[k,2]=question[,5]
        }
      }
    }

    if (nargin==1){
      write.csv(gambles1, 'gambles1_time_adaptive_r.csv')
      write.csv(gambles2, 'gambles2_time_adaptive_r.csv')
      hbtime092010qh(ngambles,ngambles,spec)

    }	else{
      filename = sprintf('gambles1_time_adaptive_%d_r.csv',study_ID)
      write.csv(gambles1, filename, row.names = FALSE)
      filename = sprintf('gambles2_time_adaptive_%d_r.csv',study_ID)
      write.csv(gambles2, filename, row.names = FALSE)
      hbtime092010qh(ngambles,ngambles,spec,study_ID)
    }
  }



  #################################
  #################################
  #####      3. hbtime092010qh function
  #################################
  #################################

  hbtime092010qh <- function (qmax,quest,spec,name){

    nargin <- length(as.list(match.call())) -1

    #time discounting - includes covariates.
    #nargin = 2 for field data, 4 for simulations
    #beta=wt(:,1);r=wt(:,2);theta=wt(:,3);
    #limited to quasi-hyperbolic discounting.
    #qmax: number of questions used for estimation
    #quest: number of questions asked

    if (spec==0){covin=matrix()}
    else if (spec==1){covin=t(as.matrix(c(1,2,13)))}
    else if (spec==2){covin=t(as.matrix(c(2,13)))}
    else if (spec==3){covin=t(as.matrix(c(1,2)))}
    else if (spec==4){covin=t(as.matrix(c(1,2,7,8,9,13)))}

    #adaptive jump size for delta:
    accep_delta=c()
    jump_delta=1e-5

    #time = c()
    #time[6] = as.numeric(format(Sys.time(), "%OS3"))
    #wini=rbeta(5, round(time[6],digits=0), round((time[6]-round(time[6], digits=0))*10, digits=0), ncp = 0)
    #if(is.na(wini[1])==TRUE){stop('error: rbeta generation failed')}
    #change seed of random generator

    #test09/09/09: new measures
    if (spec==0){
      covariates=c()
    }	else{
      covariates=read.csv('covariates_all_time.csv', header=FALSE)
      covariates=covariates[1,]
      covariates=covariates[,covin]
    }

    #data:
    if (nargin==3){
      #data:
      gambles1_full=read.csv('gambles1_time_adaptive.csv', header=FALSE)
      gambles2_full=read.csv('gambles2_time_adaptive.csv', header=FALSE)
      #full set of respondents
      ids_full=read.csv('serials_time.csv', header=FALSE)
    }	else{
      filename = sprintf('gambles1_time_adaptive_%d_r.csv',name)
      gambles1_full=read.csv(filename, header=TRUE)
      filename = sprintf('gambles2_time_adaptive_%d_r.csv',name)
      gambles2_full=read.csv(filename, header=TRUE)
      filename = sprintf('serials_time_%d.csv',name)
      ids_full=read.csv(filename, header=FALSE)
    }

    #respondents that we estimate
    ids=ids_full
    gambles1=c()
    gambles2=c()

    for (i in 1:length(t(ids_full))){
      if (sum(ids==ids_full[i,])>0){
        gambles1=rbind(gambles1,gambles1_full[as.numeric(quest*(i-1)+1):as.numeric(quest*i),])
        gambles2=rbind(gambles2,gambles2_full[as.numeric(quest*(i-1)+1):as.numeric(quest*i),])
      }
    }
    # Z=t(covariates)
    n=length(t(ids))
    Z=matrix(1,nrow=1,ncol=n)
    people=n


    starttime = proc.time()

    #p=number of parameters estimated for each respondent
    p=2
    qcov=nrow(Z)
    theta=matrix(0, nrow = p, ncol = qcov)
    ncoef=p
    N=p+people
    jump=0.01

    nit1=10000
    nit2=40000

    alphaconv=c()
    deltaconv=c()
    likeconv=c()

    ######OLIVIER 02/12/12
    jump=1;
    #####



    ####################################
    #initialize wts, meanw and D:
    ###############################

    #random starting point:
    w0=as.matrix(cbind(0.3+0.5*runif(1), 0.007+0.002*runif(1)))

    ############OLIVIER 02/10/12
    w0=matrix(c(0.8, 0.008), nrow=1, byrow=TRUE)
    ######################################


    #w0=[0.7+0.2*runif(1) 0.007+0.002*runif(1)]
    wts=cbind(matrix(w0[,1],people,1),matrix(w0[,2],people,1)) +0.001*(1-2*matrix(runif(people*p),people,p))


    #test 12/15/09:
    delta=0.045+0.01*runif(1)
    ######################################

    #meanw=mean(wts)
    D=diag(p)

    ##test 12/03/09:
    D=matrix(c(0.1, 0, 0, 0.0001), nrow=2, byrow=TRUE)


    sumwts=matrix(0,people,ncoef)
    #summeanw=zeros(1,ncoef)
    sumD=matrix(0,ncoef,ncoef)
    sumdelta=0
    sumit=0
    sumtheta=matrix(0,p,qcov)

    betas=wts[,1]
    rs=wts[,2]

    x1s=gambles1[,1]
    t1s=gambles1[,2]
    x2s=gambles2[,1]
    t2s=gambles2[,2]

    po=matrix(1,people,1)
    index=1
    for (k in 1:people){
      for (q in 1:quest){
        if (q <= qmax){
          x1=x1s[index]
          t1=t1s[index]
          x2=x2s[index]
          t2=t2s[index]
          V1=Vtimeqh(x1,t1,betas[k],rs[k],delta)
          V2=Vtimeqh(x2,t2,betas[k],rs[k],delta)
          pokj=1/(1+exp(V2-V1))
          po[k]=po[k]*pokj
        }
        index=index+1
      }
    }

    do=c()
    for (k in 1:people){
      #covariates:
      #    do=[do;exp(-0.5*(wts(k,:)-meanw)%*%solve(D)%*%(wts(k,:)-meanw)')];
      do=rbind(do, exp(-0.5*(wts[k,]-t(Z[,k])%*%t(theta))%*%(solve(D,t(wts[k,]-t(Z[,k])%*%t(theta))))))
    }

    ######################################
    # iterations
    ######################################

    for (i in 1:(nit1+nit2)){

      #acceptance rate;
      accep=0

      ############################################update meanw:####################
      ###########test 05/06/09: prior on meanw~N([0.6;0.8;2.2];100I);
      #  if people==1
      #      meanw=mvnrnd(wts,D/people,1);
      #  else
      #      meanw=mvnrnd(mean(wts),D/people,1);
      #  end
      #######################################################################

      ############################################update theta:####################
      B = t(wts)
      Vn = solve(kronecker(Z%*%t(Z),solve(D)))
      vecUn = Vn %*% kronecker(Z,solve(D)) %*% c(B)

      triu_Vn = Vn
      triu_Vn[lower.tri(triu_Vn)]=0

      Vn=triu_Vn+t(triu_Vn)-diag(diag(Vn))

      theta=rmultnorm(1,vecUn,Vn)
      theta2=matrix(theta[1:p])

      for (l in 2:qcov){
        if(qcov > 1){
          if(is.na(theta[(l-1)*p+1]) == FALSE){
            if(is.na(theta[l*p]) == FALSE){
              theta2=cbind(theta2, matrix(theta[(l-1)*p+1:l*p]))
            }
          }
        }
      }

      theta=theta2


      test=t(wts-t(Z)%*%t(theta))%*%(wts-t(Z)%*%t(theta))

      HBDinv=matrix(c(0.1, 0, 0, 0.0001), nrow=2, byrow=TRUE)
      iwpb=3
      invtest = solve(test + (p+iwpb)  * HBDinv)

      triu_invtest = invtest
      triu_invtest[lower.tri(triu_invtest)]=0
      invtest=triu_invtest+t(triu_invtest)-diag(diag(invtest))

      Dinv=rwish(people+p+iwpb, invtest)
      D = solve(Dinv)

      ######################################################################
      oks=matrix(0,people,1)
      nwts=matrix(0,people,p)
      it=0

      while (sum(oks)<people){
        d=rmultnorm(people,matrix(0,p,1),jump*D)
        nwtstemp=wts+d
        for (k in 1:people){
          if (oks[k]==0){
            wtemp=nwtstemp[k,]
            #Tanaka
            #   if wtemp(1)>=0.1 & wtemp(1)<=1 & wtemp(2)>=0.1 & wtemp(3)>=0.95 & wtemp(3)<20
            #test 12/02/09: r<=0.05

            #####OLIVIER 02/12/12
            #                if (wtemp[1]>=0 & wtemp[1]<=1 & wtemp[2]>0 & wtemp[2]<=0.05){
            if (wtemp[1]>=0 & wtemp[1]<=2 & wtemp[2]>0 & wtemp[2]<=0.05){
              ##################

              nwts[k,]=wtemp
              oks[k]=1
            }
          }
        }
        it=it+1
      }

      if (it>1000) {stop("it > 1000")}

      nbetas=nwts[,1]
      nrs=nwts[,2]

      pn=matrix(1,people,1)

      index=1
      for (k in 1:people){
        for (q in 1:quest){
          if ( q <= qmax ){
            x1=x1s[index]
            t1=t1s[index]
            x2=x2s[index]
            t2=t2s[index]
            nV1=Vtimeqh(x1,t1,nbetas[k],nrs[k],delta)
            nV2=Vtimeqh(x2,t2,nbetas[k],nrs[k],delta)
            pnkj=1/(1+exp(nV2-nV1))
            pn[k]=pn[k]*pnkj
          }
          index=index+1
        }
      }

      do=c()
      dn=c()

      for (k in 1:people) {
        #covariates:
        do=rbind(do, exp(-0.5*(wts[k,]-t(Z[,k])%*%t(theta))%*%(solve(D,t(wts[k,]-t(Z[,k])%*%t(theta))))))
        dn=rbind(dn, exp(-0.5*(nwts[k,]-t(Z[,k])%*%t(theta))%*%(solve(D,t(nwts[k,]-t(Z[,k])%*%t(theta))))))
      }

      # keyboard

      #compute r for each respondent and accept or reject:
      for (k in 1:people){
        r = pn[k]*dn[k] / (po[k]*do[k])
        if ( r>1 ){
          wts[k,]=nwts[k,]
          po[k]=pn[k]
          do[k]=dn[k]
          accep = accep + 1
        }	else{
          draw=runif(1,0,1)
          if ( r > draw ){
            wts[k,]=nwts[k,]
            po[k]=pn[k]
            do[k]=dn[k]
            accep=accep+1
          }
        }
      }

      accep=accep/people

      if (accep<0.3){
        jump=0.9*jump
      }	else{
        jump=1.1*jump
      }

      ##############update delta ###########################################
      ok_delta=0
      while (ok_delta == 0){
        d = rmultnorm(1,as.matrix(0),as.matrix(jump_delta))
        ndelta_temp = delta + d
        if (ndelta_temp>0){
          ndelta=ndelta_temp
          ok_delta=1
        }
      }

      betas=wts[,1]
      rs=wts[,2]

      r=0
      pn=matrix(1,people,1)

      index=1
      for (k in 1:people){
        for (q in 1:quest){
          if (q <= qmax){
            x1=x1s[index]
            t1=t1s[index]
            x2=x2s[index]
            t2=t2s[index]
            nV1=Vtimeqh(x1,t1,betas[k],rs[k],ndelta)
            nV2=Vtimeqh(x2,t2,betas[k],rs[k],ndelta)
            pnkj=1/(1+exp(nV2-nV1))
            pn[k]=pn[k]*pnkj
          }
          index=index+1
        }
        r=r+log(pn[k]/po[k])
      }

      r=exp(r)

      accep_d=0
      if (r>1){
        delta=ndelta
        po=pn
        accep_d=1
      }	else{
        draw=runif(1,0,1)
        if (r>draw){
          delta=ndelta
          po=pn
          accep_d=1
        }
      }

      accep_delta=rbind(accep_delta,accep_d)
      if (length(t(accep_delta))>100){
        accep_delta = accep_delta[-1,]
      }

      #####################################################################

      if (i>=nit1 & (i %% 10)==0){
        sumwts=sumwts+wts
        sumD=sumD+D
        #covariates:
        #        summeanw=summeanw+meanw
        sumtheta=sumtheta+theta
        sumdelta=sumdelta+delta
        sumit=sumit+1
      }
      if ((i %% 10) == 0){
        #covariates:
        alphaconv=rbind(alphaconv, t(c(theta)))
        deltaconv=rbind(deltaconv, delta)
        likeconv=rbind(likeconv, sum(log(po)))
        print(i)
      }
      if ((i %% 500)==0){
        filename = sprintf('deltaconvqh_q%d_R%d_r.csv',qmax,spec)
        write.csv(deltaconv, filename)
        filename = sprintf('alphaconvqh_q%d_R%d_r.csv',qmax,spec)
        write.csv(alphaconv, filename)
        filename = sprintf('likeconvqh_q%d_R%d_r.csv',qmax,spec)
        write.csv(likeconv, filename)
        delta
        theta
        exp(sum(log(po))/qmax/people)
      }
    }

    wts=sumwts/sumit
    D=sumD/sumit
    #covariates:
    #meanw=summeanw/sumit
    theta=sumtheta/sumit
    delta=sumdelta/sumit

    endtime = proc.time()
    comptime = endtime - starttime

    filename = sprintf('deltaconv_time_q%d_R%d_%d_r.csv',qmax,spec,name)
    write.csv(deltaconv, filename)
    filename = sprintf('alphaconv_time_q%d_R%d_%d_r.csv',qmax,spec,name)
    write.csv(alphaconv, filename)
    filename = sprintf('likeconv_time_q%d_R%d_%d_r.csv',qmax,spec,name)
    write.csv(likeconv, filename)
    filename = sprintf('wts_time_q%d_R%d_%d_r.csv',qmax,spec,name)
    write.csv(wts, filename)
    filename = sprintf('D_time_q%d_R%d_%d_r.csv',qmax,spec,name)
    write.csv(D, filename)
    #covariates:
    filename = sprintf('theta_time_q%d_R%d_%d_r.csv',qmax,spec,name)
    write.csv(theta, filename)
    filename = sprintf('delta_time_q%d_R%d_%d_r.csv',qmax,spec,name)
    write.csv(delta, filename)

  }


  hbfielddata_time(0,1)

}
