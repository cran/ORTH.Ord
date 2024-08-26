
##########################################################################
#Module: Fit
#
#Main esimtation machinery for orthogonalized
#residuals.
##########################################################################

Fit<-function(y,X,Z,id,n,L,orth_z,independence,MMORTH,init_beta,init_alpha,miter,crit_level){
  p<-ncol(X)

  converge<-0
  flag<-0
  if (independence==0){
    q<-ncol(Z)-1
    if (length(init_alpha)!=0){
      alpha<-init_alpha
    } else {
      alpha<-rep(0.01,q)
    }
  } else {
    q<-NULL
  }

  beta<-firstbeta(init_beta, y, X)


  omega<-matrix(data = 0,nrow=p,ncol=p)
  fit_iter<-1
  while (fit_iter<=miter & ((converge==0)*(flag==0))==1) {
    Score_result<-Score(beta,alpha,y,X,Z,L,n,id,p,q,orth_z,omega,MMORTH,independence)
    if (independence==0){
      theta<-c(beta,alpha)
    } else {
      theta<-c(beta)
    }
    U<-Score_result[[1]]
    DVD<-Score_result[[3]]
    omega<-DVD[1:p,1:p]
    delta<-solve(DVD, U)
    theta<-theta + delta
    beta<-theta[1:p]
    if (independence==0){
      alpha<-ifelse(theta[(p+1):(p+q)]<50,theta[(p+1):(p+q)],50)
    } else {
      alpha<-NULL
    }
    crit<-max(abs(delta))
    converge<-(crit <= crit_level)*1
    fit_iter<-fit_iter+1

    message("Number of iteration is:",fit_iter,"\n")
    message("The difference between parameter estimations:",crit,"\n")
    if (converge==1){message("Model is converged! \n")}
  }

  if (flag==1){
    converge<-0
    message("Miter is:", miter,"\n")
    message("Alpha estimate is:", alpha,"\n")
    message("Variance Flag, Algorithm violating limit on joint mean of Y_ij Y_ik.\n")
  }

  fit_iter<-fit_iter-1

  ###Covariance ;
  if (converge==1){
    DVD<-Score_result[[3]]
    UUt<-Score_result[[2]]
    cov<-(solve(DVD)%*%UUt%*%solve(t(DVD)))

    #bias-corrected covariance:
    UU_BC0<-Score_result[[4]]
    cov_BC0<-(solve(DVD)%*%UU_BC0%*%solve(t(DVD)))
    UU_BC1<-Score_result[[5]]
    cov_BC1<-(solve(DVD)%*%UU_BC1%*%solve(t(DVD)))
    UU_BC2<-Score_result[[6]]
    cov_BC2<-(solve(DVD)%*%UU_BC2%*%solve(t(DVD)))
    UU_BC3<-Score_result[[7]]
    cov_BC3<-(solve(DVD)%*%UU_BC3%*%solve(t(DVD)))
  }else{
    cov<-NULL
    cov_BC0<-NULL
    cov_BC1<-NULL
    cov_BC2<-NULL
    cov_BC3<-NULL
    message("The model is not converged within",miter,"iterations. \n")
    stop("Not converged! The program is stopped.")
  }

  Result_fit<-list(beta,alpha,cov,fit_iter,crit,L,Score_result[[1]],Score_result[[2]],Score_result[[3]],converge,cov_BC0,cov_BC1,cov_BC2,cov_BC3)



  return(Result_fit)
}


