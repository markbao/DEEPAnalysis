
#################################
## Hierarchical Bayes Risk Fns ##
#################################

#' @title
#' Estimation of Risk Preference Parameters Using Hierarchical Markov Chain Mote Carlo Bayesian Framework
#'
#' @description
#' \code{deepRiskHB} Estimates Risk Preferences and outputs results in current working directory.
#'
#' @import MSBVAR
#' @import MCMCpack
#'
#' @return Outputs multiple files with Risk Preference Parameters.
#' @export

#******* Originally called Risk_Unbounded_070113.R *********#

deepRiskHB <- function()
{

  #change the value of the first argurment of hbvalue042710 to be the question that you want to estimate after
  # e.g. hbvalue042710(2,ngambles,spec,study_ID) to stop estimating after 2 questions

  #################################
  #################################
  #####      1. g function
  #################################
  #################################

  g <- function(q,alpha){
    #probability weighing function

    if (q>=1){
      q=1-.Machine$double.eps;
    }

    return(exp(-exp(alpha*log(-log(q)))))

  }



  #################################
  #################################
  #####      2. V050509 function
  #################################
  #################################

  V050509 <- function(x,p,y,q,alpha,sigma_gain,lambda,delta,sigma_loss){
    #Value function. x<y

    #if (y<x & x>0) | (x<y & y<0)
    if ((x*y>0 & abs(x)>abs(y)) | (x*y<0 & x>0)){
      #make sure that we fall in one of the 3 cases on page 8 of Tanaka et al. 2007)
      xt=x
      pt=p
      x=y
      p=q
      y=xt
      q=pt
    }

    #if x>0 & y>0
    #    value=g(p+q,alpha)*x^sigma+g(q,alpha)*(y^sigma-x^sigma)
    #end
    #if y<0 & x<0
    #    value=-lambda*(g(p+q,alpha)*(-x)^sigma+g(q,alpha)*((-y)^sigma-(-x)^sigma))
    #end

    #if x<0 & y>0
    #    value=-lambda*g(p,alpha)*(-x)^sigma+g(q,alpha)*y^sigma
    #end

    if (x>0 & y>0){
      value=x^sigma_gain+g(q,alpha)*(y^sigma_gain-x^sigma_gain)
    }

    if (y<0 & x<0){
      value=-lambda*((-x)^sigma_loss+g(q,alpha)*((-y)^sigma_loss-(-x)^sigma_loss))
    }

    if (x<0 & y>0){
      value=-lambda*g(p,alpha)*(-x)^sigma_loss+g(q,alpha)*y^sigma_gain
    }

    return(delta*value)

  }


  #################################
  #################################
  #####      3. hbfielddata_risk function
  #################################
  #################################


  hbfielddata_risk <- function(spec,study_ID){
    #estimates time discounting parameters from field data

    nargin <- length(as.list(match.call())) -1

    #load data:
    if (nargin==1){
      serials=read.csv('serials_risk.csv', header=FALSE)
      adaptive_questions=read.csv('adaptive_questions_risk.csv', header=FALSE)
      choices=read.csv('choices_risk.csv', header=FALSE)
    }	else{
      filename = sprintf('serials_risk_%d.csv',study_ID)
      serials=read.csv(filename, header=FALSE)
      filename = sprintf('adaptive_questions_risk_%d.csv',study_ID)
      adaptive_questions=read.csv(filename, header=FALSE)
      filename = sprintf('choices_risk_%d.csv',study_ID)
      choices=read.csv(filename, header=FALSE)
    }

    all_questions_paths <- DEEP::all_questions_paths_risk
#     all_questions_paths=read.csv('all_questions_paths_risk.csv', header=FALSE)


    ngambles=length(adaptive_questions)
    nresp=length(t(serials))


    #gambles1 contains the chosen gambles, gambles2 the other. All gambles
    #stacked together.

    gambles1=matrix(0,nresp*ngambles,4)
    gambles2=matrix(0,nresp*ngambles,4)
    k = 0

    for (i in 1:nresp){
      for (j in 1:ngambles){
        k = k + 1
        questionid=adaptive_questions[i,j]
        question=all_questions_paths[questionid+1,]
        if (question[1] != questionid){
          print('error')
        }
        choice=choices[i,j]
        if (choice==1){
          gambles1[k,1]=question[,2]
          gambles1[k,2]=question[,3]
          gambles1[k,3]=question[,4]
          gambles1[k,4]=question[,5]
          gambles2[k,1]=question[,6]
          gambles2[k,2]=question[,7]
          gambles2[k,3]=question[,8]
          gambles2[k,4]=question[,9]
        }   else{
          gambles1[k,1]=question[,6]
          gambles1[k,2]=question[,7]
          gambles1[k,3]=question[,8]
          gambles1[k,4]=question[,9]
          gambles2[k,1]=question[,2]
          gambles2[k,2]=question[,3]
          gambles2[k,3]=question[,4]
          gambles2[k,4]=question[,5]
        }
      }
    }

    if (nargin==1){
      write.csv(gambles1, 'gambles1_risk_adaptive_r.csv', row.names = FALSE)
      write.csv(gambles2, 'gambles2_risk_adaptive_r.csv', row.names = FALSE)
      hbvalue042710(ngambles,ngambles,spec)
    }	else{
      filename = sprintf('gambles1_risk_adaptive_%d_r.csv',study_ID)
      write.csv(gambles1, filename, row.names = FALSE)
      filename = sprintf('gambles2_risk_adaptive_%d_r.csv',study_ID)
      write.csv(gambles2, filename, row.names = FALSE)

      #change the value of the first argurment of hbvalue042710 to be the question that you want to estimate after
      hbvalue042710(ngambles,ngambles,spec,study_ID)
    }

  }



  #################################
  #####      4. hbvalue042710 function
  #################################

  hbvalue042710 <- function(qmax,quest,spec,name){
    #covariates include Gray waves.
    #for online experiment - includes covariates.
    #test: different sigmas for gains and losses.
    #alpha=wt(:,1);sigma_gain=wt(:,2);lambda=wt(:,3);sigma_loss=wt(:,4)
    #outs: which respondents should be out.
    #qmax: number of questions used for estimation
    #quest: number of questions asked

    nargin <- length(as.list(match.call())) -1

    # if (spec==0){covin=matrix()}
    if (spec==0){covin=c()}
    else if (spec==1){covin=t(as.matrix(c(1,2,13)))}
    else if (spec==2){covin=t(as.matrix(c(2,13)))}
    else if (spec==3){covin=t(as.matrix(c(1,2)))}
    else if (spec==4){covin=t(as.matrix(c(1,2,7,8,9,13)))}

    #time = c()
    #time[6] = as.numeric(format(Sys.time(), "%OS3"))
    #wini=rbeta(5, round(time[6],digits=0), round((time[6]-round(time[6], digits=0))*10, digits=0), ncp = 0)
    #if(is.na(wini[1])==TRUE){stop('error: rbeta generation failed')}
    #change seed of random generator

    if (spec==0){
      covariates=c()
    }	else{
      covariates=read.csv('covariates_all_risk.csv', row.names=1, header=FALSE)
      covariates=covariates[,covin]
    }

    if (nargin==3){
      #data:
      gambles1_full=read.csv('gambles1_risk_adaptive_r.csv', header=FALSE)
      gambles2_full=read.csv('gambles2_risk_adaptive_r.csv', header=FALSE)
      #full set of respondents
      ids_full=read.csv('serials_risk.csv', header=FALSE)
    }	else{
      filename = sprintf('gambles1_risk_adaptive_%d_r.csv',name)
      gambles1_full=read.csv(filename, header=TRUE)
      filename = sprintf('gambles2_risk_adaptive_%d_r.csv',name)
      gambles2_full=read.csv(filename, header=TRUE)
      filename = sprintf('serials_risk_%d.csv',name)
      ids_full=read.csv(filename, header=FALSE)
    }

    #%respondents that we estimate
    ids=ids_full
    gambles1=c()
    gambles2=c()
    for (i in 1:length(t(ids_full))){
      if (sum(ids==ids_full[i,])>0){
        gambles1=rbind(gambles1,gambles1_full[as.numeric(quest*(i-1)+1):as.numeric(quest*i),])
        gambles2=rbind(gambles2,gambles2_full[as.numeric(quest*(i-1)+1):as.numeric(quest*i),])
      }
    }

    #Z=t(covariates)
    n=length(t(ids))
    #Z=c(matrix(1,1,n),Z)  ############
    Z=matrix(1,1,n)
    people=n

    p=3
    qcov=nrow(Z)
    theta=matrix(0,p,qcov)
    ncoef=p
    N=p+people
    jump=0.1
    jump_delta=0.01

    #test 12/15/09
    jump_delta=1e-5
    ###

    nit1=50000
    nit2=50000

    alphaconv=c()
    deltaconv=c()
    likeconv=c()

    ###################################
    #initialize wts, meanw and D:

    ###################################
    #random starting point:
    #w0=as.matrix(cbind(0.1+0.9*runif(1), 0.1+0.9*runif(1), 0.95+3*runif(1)))
    #wts=(matrix(1, people, 1)%*%w0)+(0.1*matrix(runif(people*p),people,p))
    #delta=0.045+0.01*runif(1)

    w0 = as.matrix(cbind(0.6, 0.8, 2.2))
    wts=(matrix(1, people, 1)%*%w0)+(0.01*matrix(runif(people*p),people,p))
    delta=0.05
    ###################################

    #meanw=mean(wts)
    D=diag(p)

    sumwts=matrix(0,people,ncoef)
    #summeanw=matrix(0,1,ncoef)
    sumD=matrix(0,ncoef,ncoef)
    sumdelta=0
    sumit=0
    sumtheta=matrix(0,p,qcov)

    alphas=wts[,1]
    sigmas_gain=wts[,2]
    lambdas=wts[,3]
    sigmas_loss=wts[,2]

    x1s=gambles1[,1]
    p1s=gambles1[,2]
    y1s=gambles1[,3]
    q1s=gambles1[,4]
    x2s=gambles2[,1]
    p2s=gambles2[,2]
    y2s=gambles2[,3]
    q2s=gambles2[,4]

    po=matrix(0,people,1)
    index=1
    for (k in 1:people){
      for (q in 1:quest){
        if (q <= qmax){
          x1=x1s[index]
          p1=p1s[index]
          y1=y1s[index]
          q1=q1s[index]
          x2=x2s[index]
          p2=p2s[index]
          y2=y2s[index]
          q2=q2s[index]
          V1=V050509(x1,p1,y1,q1,alphas[k],sigmas_gain[k],lambdas[k],delta,sigmas_loss[k])
          V2=V050509(x2,p2,y2,q2,alphas[k],sigmas_gain[k],lambdas[k],delta,sigmas_loss[k])
          pokj=1/(1+exp(V2-V1))
          po[k]=po[k]*pokj
        }
        index=index+1
      }
    }

    do=c()
    for (k in 1:people){
      #covariates
      #do=[do;exp(-0.5*(wts(k,:)-meanw)*inv(D)*(wts(k,:)-meanw)')];
      do=rbind(do, exp(-0.5*(wts[k,]-t(Z[,k])%*%t(theta))%*%(solve(D,t(wts[k,]-t(Z[,k])%*%t(theta))))))
    }

    #######################################
    #iterations

    for (i in 1:(nit1+nit2)){

      #acceptance rate
      accep=0

      #added 01/07/11:
      like = matrix(0,1,people)
      #

      ############################################update meanw:####################
      ###########test 05/06/09: prior on meanw~N([0.6;0.8;2.2];100I);
      #  if people==1
      #      meanw=mvnrnd(wts,D/people,1)
      #  else
      #      meanw=mvnrnd(mean(wts),D/people,1)
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
      #######################################################################

      ############################## DRAW D #################################
      #updtate D=draw from the inverse Wishart distribution
      #covariates:
      test=t(wts-t(Z)%*%t(theta))%*%(wts-t(Z)%*%t(theta))
      #test=(wts-ones(people,1)*meanw)'*(wts-ones(people,1)*meanw)

      ###########test 01/03/10
      #HBDinv=diag(p)
      iwpb=3
      HBDinv=0.1*diag(p)
      #iwpb=15
      #######################


      # ***NOT*** SAME AS IN LENK PAPER:
      #D=iwishrnd(test+(p+iwpb)*eye(p),nresp+p+iwpb)
      #D=iwishrnd(test+eye(p),nresp+p+iwpb)
      invtest = solve(test+(p+iwpb)*HBDinv)

      triu_invtest = invtest
      triu_invtest[lower.tri(triu_invtest)]=0
      invtest=triu_invtest+t(triu_invtest)-diag(diag(invtest))

      #06/06/07
      if (sum(sum(is.na(invtest)))>0){
        stop("sum(sum(is.na(invtest)))>0")
      }
      #

      Dinv=rwish(people+p+iwpb, invtest)
      D = solve(Dinv)

      ######################################################################



      ###################################################update wts ########
      oks=matrix(0,people,1)
      nwts=matrix(0,people,p)
      it=0

      while (sum(oks)<people){
        d=rmultnorm(people,matrix(0,p,1),jump*D)
        nwtstemp=wts+d
        for (k in 1:people){
          if (oks[k]==0){
            wtemp=nwtstemp[k,]

            if (wtemp[1]>=0.05 & wtemp[1]<=2 & wtemp[2]>0.05 & wtemp[2]<=2 & wtemp[3]>0 & wtemp[3]<10){
              nwts[k,]=wtemp
              oks[k]=1
            }
          }
        }
        it=it+1
      }

      if (it>1000) {stop("it > 1000")}

      nalphas=nwts[,1]
      nsigmas_gain=nwts[,2]
      nlambdas=nwts[,3]
      nsigmas_loss=nwts[,2]

      pn=matrix(1,people,1)

      index=1
      for (k in 1:people){
        for (q in 1:quest){
          if (q<=qmax){
            x1=x1s[index]
            p1=p1s[index]
            y1=y1s[index]
            q1=q1s[index]
            x2=x2s[index]
            p2=p2s[index]
            y2=y2s[index]
            q2=q2s[index]
            nV1=V050509(x1,p1,y1,q1,nalphas[k],nsigmas_gain[k],nlambdas[k],delta,nsigmas_loss[k])
            nV2=V050509(x2,p2,y2,q2,nalphas[k],nsigmas_gain[k],nlambdas[k],delta,nsigmas_loss[k])
            pnkj=1/(1+exp(nV2-nV1))
            pn[k]=pn[k]*pnkj
          }
          index=index+1
        }
      }

      #compute the posterior do and dn:
      do=c()
      dn=c()

      for (k in 1:people){
        #covariates:
        do=rbind(do, exp(-0.5*(wts[k,]-t(Z[,k])%*%t(theta))%*%(solve(D,t(wts[k,]-t(Z[,k])%*%t(theta))))))
        dn=rbind(dn, exp(-0.5*(nwts[k,]-t(Z[,k])%*%t(theta))%*%(solve(D,t(nwts[k,]-t(Z[,k])%*%t(theta))))))
      }

      #compute r for each respondent and accept or reject:
      for (k in 1:people){
        r=pn[k]*dn[k]/(po[k]*do[k])
        if (r>1){
          wts[k,]=nwts[k,]
          po[k]=pn[k]
          do[k]=dn[k]
          accep=accep+1

          #added 01/07/11
          like[k]=pn[k]^(1/qmax)
          #

        }	else{
          draw=runif(1,0,1)
          if (r>draw){
            wts[k,]=nwts[k,]
            po[k]=pn[k]
            do[k]=dn[k]
            accep=accep+1

            #added 01/07/11
            like[k]=pn[k]^(1/qmax)
          }	else{
            like[k]=po[k]^(1/qmax)
          }
        }
      }

      accep=accep/people

      if (accep<0.3){
        jump=0.9*jump
      }	else{
        jump=1.1*jump
      }
      ######################################################################


      ##############update delta ###########################################

      ###############################

      ok_delta=0
      while (ok_delta == 0){
        d = rmultnorm(1,as.matrix(0),as.matrix(jump_delta))
        ndelta_temp = delta + d
        if (ndelta_temp>0){
          ndelta=ndelta_temp
          ok_delta=1
        }
      }

      alphas=wts[,1]
      sigmas_gain=wts[,2]
      lambdas=wts[,3]
      sigmas_loss=wts[,2]

      r=1

      pn=matrix(1,people,1)

      index=1
      for (k in 1:people){
        for (q in 1:quest){
          if (q <= qmax){
            x1=x1s[index]
            p1=p1s[index]
            y1=y1s[index]
            q1=q1s[index]
            x2=x2s[index]
            p2=p2s[index]
            y2=y2s[index]
            q2=q2s[index]
            nV1=V050509(x1,p1,y1,q1,alphas[k],sigmas_gain[k],lambdas[k],ndelta,sigmas_loss[k])
            nV2=V050509(x2,p2,y2,q2,alphas[k],sigmas_gain[k],lambdas[k],ndelta,sigmas_loss[k])
            pnkj=1/(1+exp(nV2-nV1))
            pn[k]=pn[k]*pnkj
          }
          index=index+1
        }
        r=r*pn[k]/po[k]
      }

      ###test 12/06/09:
      r=exp(r/people/qmax)

      if (r>1){
        delta=ndelta
        po=pn
      }	else{
        draw=runif(1,0,1)
        if (r>draw){
          delta=ndelta
          po=pn
        }
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

        #change 01/07/11: delete followin line and replace with next line
        #likeconv=rbind(likeconv, sum(log(po)))
        likeconv=rbind(likeconv, t(c(like)))
        #
        print(i)
      }
      if ((i %% 500)==0){
        filename = sprintf('deltaconvv_risk_q%d_R%d_r.csv',qmax,spec)
        write.csv(deltaconv, filename)
        filename = sprintf('alphaconvv_risk_q%d_R%d_r.csv',qmax,spec)
        write.csv(alphaconv, filename)
        filename = sprintf('likeconvv_risk_q%d_R%d_r.csv',qmax,spec)
        write.csv(likeconv, filename)
        print(delta)
        print(theta)
        exp(sum(log(po))/qmax/people)
      }
    }

    wts=sumwts/sumit
    D=sumD/sumit
    #covariates:
    #meanw=summeanw/sumit
    theta=sumtheta/sumit
    delta=sumdelta/sumit

    #added 01/07/11
    geofit=colMeans(likeconv[(round(nit1/10)+1):((nit1+nit2)/10),])
    #

    if (nargin==3){
      filename = sprintf('deltaconv_risk_q%d_R%d_r.csv',qmax,spec)
      write.csv(deltaconv, filename)
      filename = sprintf('alphaconv_risk_q%d_R%d_r.csv',qmax,spec)
      write.csv(alphaconv, filename)
      filename = sprintf('likeconv_risk_q%d_R%d_r.csv',qmax,spec)
      write.csv(likeconv, filename)
      filename = sprintf('wts_risk_q%d_R%d_r.csv',qmax,spec)
      write.csv(wts, filename)
      filename = sprintf('D_risk_q%d_R%d_r.csv',qmax,spec)
      write.csv(D, filename)
      #covariates:
      filename = sprintf('theta_risk_q%d_R%d_r.csv',qmax,spec)
      write.csv(theta, filename)
      filename = sprintf('delta_risk_q%d_R%d_r.csv',qmax,spec)
      write.csv(delta, filename)

      #added 01/07/11:
      filename = sprintf('geofit_risk_q%d_R%d_r.csv',qmax,spec)
      write.csv(c(geofit), filename)
      #

    }	else{
      filename = sprintf('deltaconv_risk_q%d_R%d_%d_r.csv',qmax,spec,name)
      write.csv(deltaconv, filename)
      filename = sprintf('alphaconv_risk_q%d_R%d_%d_r.csv',qmax,spec,name)
      write.csv(alphaconv, filename)
      filename = sprintf('likeconv_risk_q%d_R%d_%d_r.csv',qmax,spec,name)
      write.csv(likeconv, filename)
      filename = sprintf('wts_risk_q%d_R%d_%d_r.csv',qmax,spec,name)
      write.csv(wts, filename)
      filename = sprintf('D_risk_q%d_R%d_%d_r.csv',qmax,spec,name)
      write.csv(D, filename)
      #covariates:
      filename = sprintf('theta_risk_q%d_R%d_%d_r.csv',qmax,spec,name)
      write.csv(theta, filename)
      filename = sprintf('delta_risk_q%d_R%d_%d_r.csv',qmax,spec,name)
      write.csv(delta, filename)

      #added 01/07/11:
      filename = sprintf('geofit_risk_q%d_R%d_%d_r.csv',qmax,spec,name)
      write.csv(c(geofit), filename)
      #
    }

  }

  hbfielddata_risk(0,1)



}
