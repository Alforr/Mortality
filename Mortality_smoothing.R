Mort1Dsmooth <-
  function(x, y, offset, w,
           overdispersion=FALSE,
           ndx=floor(length(x)/5),
           deg=3, pord=2, 
           lambda=NULL, df=NULL, method=1,
           coefstart=NULL,
           control=list()){
    ## Input:
    ## x: abcissae of data
    ## y: count response
    ## offset: an a priori known component (optional)
    ## w: a vector of weights to be used in the
    ##    fitting process (optional)
    ## overdispersion: logical on the presence of an
    ##                 overdispersion parameter.
    ##                 Default: FALSE
    ## ndx: number of internal knots -1.
    ##      Default: floor(length(x)/5)
    ## deg: degree of the B-splines. Default: 3
    ## pord: order of differences. Default: 2
    ## lambda: smoothing parameter (optional)
    ## df: a number which specifies the degrees
    ##     of freedom (optional)
    ## method: the method for controlling
    ##         the amount of smoothing. Default: 1
    ## coefstart: eventual starting coefficients
    ## control: a list of control parameters
    ## MON: logical on monitoring
    ## TOL1: convergence of the IWLS algorithm.
    ##       Default: 1e-06.
    ## TOL2: difference between two adjacent
    ##       smoothing parameters in the grid search,
    ##       log-scale. Default: 0.1.
    ## RANGE: range in between lambda is searched, log-scale.
    ##        Default: [10^-8 ; 10^8]
    ## MAX.IT: the maximum number of iterations. Default: 50
    
    
    
    ## Output: a Mort1Dsmooth object containing
    ## aic: Akaike Information Criterion
    ## bic: Bayesian Information Criterion
    ## dev: Poisson-deviance
    ## lev: diag of the hat-matrix
    ## df: effective dimension
    ## lambda: the selected (given) lambda
    ## psi2: (estimated) overdispersion parameter
    ## ndx: number of internal knots -1
    ## deg: degree of the B-splines
    ## pord: order of differences
    ## B: B-splines basis
    ## x: abcissae of data
    ## y: count responses
    ## offset: an a priori known component
    ## w: (only for weighted fits) the specified weights
    ## coef: fitted (penalized) B-splines coefficients  
    ## residuals: the deviance residuals
    ## fitted.values: fitted counts
    ## linear.predictor: fitted linear predictor
    ## logmortality: fitted mortality rates in log-scale
    ## call: the matched call
    ## n: number of observations
    ## tolerance: convergence tolerance
    
    ## checkings:
    if(missing(w)){
      w <- rep(1, length(y))
    }
    ## about x and y:
    if(missing(y)){
      if(is.list(x)){
        if(any(is.na(match(c("x", "y"), names(x)))))
          stop("cannot find x and y in list")
        x <- x$x
        y <- x$y
      }
      else if(is.complex(x)) {
        y <- Im(x)
        x <- Re(x)
      }
      else if(is.matrix(x) && ncol(x) == 2) {
        y <- x[, 2]
        x <- x[, 1]
      }
      else {
        y <- x
        x <- time(x)
      }
    }
    ## about the offset
    m <- length(y)
    if(missing(offset)) {
      offset <- rep(0, m)
    }else{
      if(length(offset) == 1) {
        offset <- rep(offset, m)
      }
      else{
        offset=offset
      }
    }  
    check <- Mort1Dsmooth_checker(x=x, y=y,
                                  offset=offset, w=w,
                                  overdispersion=overdispersion,
                                  ndx=ndx, deg=deg,
                                  pord=pord, 
                                  lambda=lambda, df=df,
                                  method=method,
                                  coefstart=coefstart,
                                  control=control)
    x <- check$x
    y <- check$y
    m <- check$m
    offset <- check$offset
    offsetINIT <- check$offsetINIT
    wei <- check$w
    over <- check$overdispersion
    ndx <- check$ndx
    deg <- check$deg
    pord <- check$pord
    lambda <- check$lambda
    df <- check$df
    MET <- check$method
    a.init <- check$coefstart
    MON <- check$control$MON
    TOL1 <- check$control$TOL1
    TOL2 <- check$control$TOL2
    RANGE <- check$control$RANGE
    MAX.IT <- check$control$MAX.IT
    call <- match.call()
    ## B-splines basis
    xl <- min(x)
    xr <- max(x)
    xmax <- xr + 0.01 * (xr - xl)
    xmin <- xl - 0.01 * (xr - xl)
    B <- MortSmooth_bbase(x, xmin, xmax, ndx, deg)
    ## penalty stuff
    nb <- ncol(B)
    D. <- diff(diag(nb), diff=pord)
    DtD <- t(D.)%*%D.
    ## General initialize:
    if(is.null(a.init)){
      y[is.na(y)] <- 0
      ## 0) simple poisson-GLM with ages(years)
      ##    only for the interpolation cases
      fit0 <- glm(round(y) ~ x + offset(offset),
                  family=poisson, weights=wei)
      ## 1) simple penalized-poisson-GLM with B
      etaGLM <- log(fit0$fitted) - offset
      eta0 <- log((y + 1)) - offset
      eta0[wei==0] <- etaGLM[wei==0]
      mu0 <- exp(offset + eta0)
      w0 <- wei*mu0
      z0 <- wei*((y - mu0)/mu0 + eta0)
      BtWB <- t(B) %*% (w0 * B)
      BtWz <- t(B) %*% (w0 * z0)
      a.init <- solve(BtWB + 1e08 * DtD, BtWz)
    }else{
      a.init=a.init
    }
    psi2 <- 1
    
    ## ## plotting starting coef
    ## ran <- range(log(y/e), B%*%a.init,
    ##              na.rm=TRUE, finite = TRUE)
    ## plot(x, log(y/e), ylim=ran)
    ## lines(x, B%*%a.init, col=2, lwd=2)
    
    ## optimize AIC or BIC
    if(MET==1|MET==2){
      ## if overdisperion is true
      if(over){
        tol.over <- 10
        i.over <- 0
        while(tol.over > 1e-03 && i.over < 5){
          i.over <- i.over+1
          lambda.hat <- Mort1Dsmooth_optimize(x=x,
                                              y=y,
                                              offset=offset,
                                              wei=wei,
                                              psi2=psi2,
                                              B=B, DtD=DtD,
                                              a.init=a.init,
                                              MON=MON,
                                              TOL1=TOL1,
                                              TOL2=TOL2,
                                              RANGE=RANGE,
                                              MAX.IT=MAX.IT,
                                              MET=MET)
          FIT <- Mort1Dsmooth_estimate(x=x, y=y,
                                       offset=offset,
                                       wei=wei,
                                       psi2=psi2,
                                       B=B, 
                                       lambda=lambda.hat,
                                       DtD=DtD,
                                       a.init=a.init, 
                                       MON=MON, TOL1=TOL1,
                                       MAX.IT=MAX.IT)
          ## recalculating overdispersion parameter
          psi2.old <- psi2
          psi2 <- FIT$dev / (m - FIT$df)
          tol.over <- abs(psi2 - psi2.old)/abs(psi2)
        }
      }else{## if psi2==1
        lambda.hat <- Mort1Dsmooth_optimize(x=x,
                                            y=y,
                                            offset=offset,
                                            wei=wei,
                                            psi2=psi2,
                                            B=B, DtD=DtD,
                                            a.init=a.init,
                                            MON=MON,
                                            TOL1=TOL1,
                                            TOL2=TOL2,
                                            RANGE=RANGE,
                                            MAX.IT=MAX.IT,
                                            MET=MET)
        FIT <- Mort1Dsmooth_estimate(x=x, y=y, offset=offset,
                                     wei=wei,
                                     psi2=psi2,
                                     B=B, 
                                     lambda=lambda.hat,
                                     DtD=DtD,
                                     a.init=a.init, 
                                     MON=MON, TOL1=TOL1,
                                     MAX.IT=MAX.IT)
        psi2 <- FIT$dev / (m - FIT$df)
      }
      if(log10(lambda.hat)>=log10(RANGE[2]) |
         log10(lambda.hat)<=log10(RANGE[1])) {
        warning(paste("optimal lambda at the edge of the grid."))
      }
    }
    ## given lambda
    if(MET==3){
      lambda.hat <- lambda
      FIT <- Mort1Dsmooth_estimate(x=x, y=y, offset=offset,
                                   wei=wei,
                                   psi2=psi2,
                                   B=B, 
                                   lambda=lambda.hat, DtD=DtD,
                                   a.init=a.init, 
                                   MON=MON, TOL1=TOL1,
                                   MAX.IT=MAX.IT)
      psi2 <- FIT$dev / (m - FIT$df)
    }
    ## optimize given df
    if(MET==4){
      Mort1Dsmooth_opt_df <- function(X){
        FIT <- Mort1Dsmooth_estimate(x=x, y=y, offset=offset,
                                     wei=wei,
                                     psi2=psi2,
                                     B=B, 
                                     lambda=X, DtD=DtD,
                                     a.init=a.init, 
                                     MON=MON, TOL1=TOL1,
                                     MAX.IT=MAX.IT)
        return(abs(FIT$df - df))
      }
      by.lambda <- length(seq(log10(RANGE[1]),
                              log10(RANGE[2]),by=TOL2))
      lambda.hat <- cleversearch(fn=Mort1Dsmooth_opt_df,
                                 lower=log10(RANGE[1]),
                                 upper=log10(RANGE[2]),
                                 ngrid=by.lambda,
                                 startvalue=1,
                                 logscale=TRUE,
                                 verbose=FALSE)[[1]]
      if(log10(lambda.hat)>=log10(RANGE[2]) |
         log10(lambda.hat)<=log10(RANGE[1])){
        warning(paste("optimal lambda at the edge of the grid."))
      }
      FIT <- Mort1Dsmooth_estimate(x=x, y=y, offset=offset,
                                   wei=wei,
                                   psi2=psi2,
                                   B=B, 
                                   lambda=lambda.hat,
                                   DtD=DtD,
                                   a.init=a.init, 
                                   MON=MON, TOL1=TOL1,
                                   MAX.IT=MAX.IT)
      psi2 <- FIT$dev / (m - FIT$df)
    }
    aic <- FIT$aic
    bic <- FIT$bic
    df <- FIT$df
    dev <- FIT$dev
    coef <- FIT$a
    psi2 <- psi2
    h <- FIT$h
    tolerance <- FIT$tol
    eta.hat <- B%*%coef
    logmortality <- eta.hat
    fitted.values <- exp(eta.hat + offset)
    res <- sign(y - fitted.values) *
      sqrt(2 * (y * log(ifelse(y == 0, 1,
                               y/fitted.values)) -
                  (y - fitted.values)))
    lin <- as.vector(eta.hat) + as.vector(offset)
    ## output
    object <- list(## fitted values
      coefficients=as.vector(coef),
      residuals=as.vector(res),
      fitted.values=as.vector(fitted.values),
      linear.predictors=lin,
      logmortality=logmortality,
      ## diagnostics
      lev=h, df=df, deviance=dev,
      aic=aic, bic=bic,
      psi2=psi2, lambda=lambda.hat,
      ## general
      call=call, n=m, tolerance=tolerance,
      ## smoothing specifications
      ndx=ndx, deg=deg, pord=pord, B=B,
      ## overdispersion=over,
      ## observed values
      x=x, y=as.vector(y),
      offset=as.vector(offsetINIT),
      w=as.vector(wei)
    )
    class(object) <- "Mort1Dsmooth"
    object
  }

Mort1Dsmooth_optimize <-
  function(x, y, offset, wei, psi2,
           B, DtD,
           a.init,
           MON, TOL1, TOL2,
           RANGE, MAX.IT, MET){
    ## the function first applies a rough cleversearch
    ## then around the optimal values,
    ## it search with a fine cleversearch
    ## a more precise value for the smoothing parameter
    
    ## Input:
    ## x: abcissae of data
    ## y: count response
    ## offset: an a priori known component
    ## wei: weigths
    ## psi2: overdispersion parameter
    ## B: B-splines basis
    ## DtD: inner product of the matrix of differences
    ## a.init: starting coefficients
    ## MON: logical on monitoring
    ## TOL1: relative convergence tolerance
    ## TOL2: difference between two adjacent
    ##       smoothing parameters in the second grid search,
    ##       log-scale.
    ## RANGE: range in between grid-search
    ## MAX.IT: the maximum number of iterations
    ## MET: criterion for optimizing lambda (bic or aic)
    
    ## Output: a list containing
    ## lambda: the optimal smoothing parameter
    
    ## object function 
    Mort1Dsmooth_opt_ic <- function(X){
      FIT <- Mort1Dsmooth_estimate(x = x, y = y,
                                   offset = offset, 
                                   wei = wei,
                                   psi2 = psi2, B = B,
                                   lambda = X, DtD = DtD, 
                                   a.init = a.init,
                                   MON=MON, TOL1=TOL1,
                                   MAX.IT=MAX.IT)
      return(ifelse(MET==2, FIT$aic, FIT$bic))
    }
    
    
    ## ROUGH SEARCHING
    ## possible lambdas
    lambdas.0 <- seq(log10(RANGE[1]),log10(RANGE[2]),
                     TOL2*4)
    ## length of possible lambdas
    by.lambda.0 <- length(lambdas.0)
    ## starting values for lambdas in cleversearch
    l.st.0 <- median(lambdas.0)
    ## optimizing lambda
    lambda.hat0 <- cleversearch(fn=Mort1Dsmooth_opt_ic,
                                lower=log10(RANGE[1]),
                                upper=log10(RANGE[2]),
                                startvalue=l.st.0,
                                ngrid=by.lambda.0,
                                logscale=TRUE,
                                verbose=FALSE)[[1]]
    ## FINE SEARCHING
    ## sub-optimal lambda=starting value in fine cleversearch
    l.st <- log10(lambda.hat0)
    ## range in which search
    min.l <- max(l.st-TOL2*4, log10(RANGE[1]))
    max.l <- min(l.st+TOL2*4, log10(RANGE[2]))
    ## possible lambdas
    lambdas <- seq(min.l, max.l, TOL2)
    ## length of possible lambdas
    by.lambda <- length(lambdas)
    ## optimizing lambda
    lambda.hat <- cleversearch(fn=Mort1Dsmooth_opt_ic,
                               lower=min.l,
                               upper=max.l,
                               startvalue=l.st,
                               ngrid=by.lambda,
                               logscale=TRUE,
                               verbose=FALSE)[[1]]
    
    return(lambda.hat)
  }
Mort1Dsmooth_checker <-
  function(x, y, offset, w,
           overdispersion,
           ndx, deg, pord, 
           lambda, df, method,
           coefstart,
           control){
    ## Input:
    ## x: abcissae of data
    ## y: count response
    ## offset: an a priori known component (optional)
    ## w: weights
    ## overdispersion: logical on the presence of
    ##                  overdispersion
    ## ndx: number of internal knots -1.
    ##      Default: floor(length(x)/5)
    ## deg: degree of the B-splines. Default: 3
    ## pord: order of differences. Default: 2
    ## lambda: smoothing parameter. Default: NULL (optional)
    ## df: a number which specifies the degrees of freedom.
    ##     Default: NULL (optional)
    ## method: the method for controlling the amount of
    ##         smoothing. Default: 4
    ## coefstart: eventual initial coefficients
    ## control: a list of control parameters
    
    ## Output: a list containing CHECKED arguments
    ##         for the Mort1Dsmooth function
    m <- length(y)
    
    offsetINIT <- offset
    ## about infinitive or NA offset
    whioff <- which(is.infinite(offset) | is.na(offset))
    whiwei <- which(w==0)
    if(any(!whioff%in%whiwei)){
      stop("weights different from zero associated with infinitive or NA offset values")
    }
    offset[c(whioff, whiwei)] <- 100
    ## about lengths and wrong values
    if (length(x)!=length(y)) 
      stop("Arguments must have same length")
    if (length(y) != m | length(offset) != m) 
      stop("Argument arrays of wrong length")
    if (deg < 1 | deg >= 10) 
      stop("Wrong value for deg")
    if (pord <= 0 | pord >= 5) 
      stop("Wrong value for pord")
    if (ndx < 2 | ndx >= floor(m*.9)) 
      stop("Wrong value for ndx")
    coefstart.check <- is.null(coefstart)
    if(!coefstart.check){
      if(length(coefstart)!=(ndx+deg)){
        stop("coefstart must have length equal to ndx+deg")
      }
    }
    ## about method
    if (method != 1 & method != 2 &
        method != 3 & method != 4) 
      stop("Wrong value for method")
    ## method = 1 adjusts lambda so that the BIC is minimized
    ## method = 2 adjusts lambda so that the AIC is minimized
    ## method = 3 uses the value supplied for lambda
    ## method = 4 adjusts lambda so that the degrees of
    ##          freedom is equal to df
    ## check-point methods
    lambda.check <- is.null(lambda)
    df.check <- is.null(df)
    MET <- NULL
    ## both lambda and df NULL
    if(lambda.check & df.check & method==1){MET=1}
    if(lambda.check & df.check & method==2){MET=2}
    if(lambda.check & df.check & method==3){
      stop("with method 3, provide lambda")
    }
    if(lambda.check & df.check & method==4){
      stop("with method 4, provide df")
    }
    ## lambda NULL and df GIVEN
    if(lambda.check & !df.check & method==1){
      stop("df and method 1 cannot be chosen together")
    }
    if(lambda.check & !df.check & method==2){
      stop("df and method 2 cannot be chosen together")
    }
    if(lambda.check & !df.check & method==3){
      stop("df and method 3 cannot be chosen together")
    }
    if(lambda.check & !df.check & method==4){MET=4}
    ## lambda GIVEN and df NULL
    if(!lambda.check & df.check & method==1){
      stop("lambda and method 1 cannot be chosen together")
    }
    if(!lambda.check & df.check & method==2){
      stop("lambda and method 2 cannot be chosen together")
    }
    if(!lambda.check & df.check & method==3){MET=3}
    if(!lambda.check & df.check & method==4){
      stop("lambda and method 4 cannot be chosen together")
    }
    ## both lambda and df GIVEN, regardless method
    if(!lambda.check & !df.check){
      stop("lambda and df cannot be chosen together")
    }  
    ## impossible values for lambda and df
    if(!lambda.check && lambda<0)
      stop("lambda must be positive")
    if(!df.check && df<pord)
      stop("df must be larger than pord")
    if(!df.check && df>c(ndx+deg))
      stop("df must be smaller than ndx+deg")
    if (!df.check & length(df)!=1)
      stop("df must be length 1")
    if (!lambda.check & length(lambda)!=1)
      stop("lambda must be length 1")
    ## setting control-parameters
    con <- list(MON=FALSE, TOL1=1e-06, TOL2=0.5,
                RANGE=c(10^-4, 10^6), MAX.IT=50)
    nmsC <- names(con)
    con[(namc <- names(control))] <- control
    if (length(noNms <- namc[!namc %in% nmsC]) > 0) 
      warning("unknown names in control: ",
              paste(noNms, collapse = ", "))
    ## stop about weights
    if(length(w)!=m){ 
      stop("length of w and y must be equal")
    }
    ## warning about interpolation/extrapolation
    if(any(w==0)){
      warning("Interpolation and/or extrapolation is taking place", call. = FALSE)
    }
    ## about the overdispersion parameter
    if(overdispersion & method==3)
      warning("given method 3, overdispersion is computed a posteriori")
    if(overdispersion & method==4)
      warning("given method 4, overdispersion is computed a posteriori")
    ## warning about weights
    if(min(w) < 0) {
      warning(paste("At least one weight entry is negative"))
    }
    ## returning
    llist <- list(x=x, y=y, offset=offset, w=w,
                  offsetINIT=offsetINIT,
                  overdispersion=overdispersion, m=m,
                  ndx=ndx, deg=deg, pord=pord, 
                  lambda=lambda, df=df, method=method,
                  coefstart=coefstart,
                  control=con)
    llist
  }
Mort1Dsmooth_estimate <-
  function(x, y, offset, wei,
           psi2, B, lambda,
           DtD, a.init,
           MON, TOL1, MAX.IT){
    ## Input:
    ## x: abcissae of data
    ## y: count response
    ## offset: an a priori known component
    ## wei: weigths
    ## psi2: overdispersion parameter
    ## B: B-splines basis
    ## lambda: smoothing parameter
    ## DtD: matrix of differences
    ## a.init: starting coefficients
    ## MON: logical on monitoring
    ## TOL1: relative convergence tolerance
    ## MAX.IT: the maximum number of iterations
    
    ## Output: a list containing
    ## a: fitted coefficients
    ## h: diag of the hat-matrix
    ## df: effective dimension
    ## aic: Akaike Information Criterion
    ## bic: Bayesian Information Criterion
    ## dev: Poisson-deviance
    ## tol: tolerance level
    ## BtWB: inner product of B-splines and weights
    ## P: penalty term
    
    ## penalty stuff
    lambdaP <- lambda * DtD
    ## initialize
    tol <- 1
    i <- 0
    a <- a.init
    a.old <- 10
    ## monitoring?
    if(MON){
      cat("lambda =", lambda, "\n")
      cat("Iter         tol", "\n")}
    while(tol > TOL1 && i < MAX.IT){
      i <- i+1
      ## update the coefficients
      a <- Mort1Dsmooth_update(x=x, y=y,
                               offset=offset, wei=wei,
                               psi2=psi2, B=B,
                               lambdaP=lambdaP, a=a)
      ## conputing the current tolerance level
      tol <- max(abs(a - a.old)/abs(a))
      ## replace the old coeff
      a.old <- a
      ## monitoring?
      if(MON){
        cat(i, "      ", tol, "\n")}
    }
    if(i > (MAX.IT-1)) {
      warning(paste("parameter estimates did NOT converge in",
                    MAX.IT,
                    "iterations. Increase MAX.IT in control."))
    }
    ## final step after convergence
    eta <- B%*%a
    mu  <- exp(eta + offset)
    w   <- c(wei*mu)
    z   <- c(wei*((y - mu)/mu + eta))
    ## regression
    BtWB   <- t(B) %*% (w * B)
    P      <- psi2*lambdaP
    BtWB.P <- BtWB + P
    BtWz   <- t(B) %*% (w * z)
    a      <- solve(BtWB.P, BtWz)
    ## coefficients
    a <- matrix(a, ncol = 1)
    ## diag of the hat-matrix
    H <- solve(BtWB.P, BtWB)
    h <- diag(H)
    ## diagnostics
    ## replace zeros in response
    y1       <- y
    y1[y==0] <- 10^(-4)
    ## deviance
    dev <- 2*(sum(wei*(y1 * log(y1/mu) ), na.rm = TRUE))
    ## effective dimension
    df <- sum(h)
    ## Akaike Information Criterion
    aic <- dev/psi2 + 2 * df
    ## Bayesian Information Criterion
    bic <- dev/psi2 + log(sum(wei)) * df
    ## output
    llist <- list(a=a, h=h, 
                  df=df, aic=aic, bic=bic,
                  dev=dev, tol=tol,
                  BtWB=BtWB, P=P)
    llist
  }
Mort1Dsmooth_update <-
  function(x, y, offset, wei, psi2, B,
           lambdaP, a){
    ## Input:
    ## x: abcissae of data
    ## y: count response
    ## offset: an a priori known component
    ## wei: weights
    ## psi2: overdispersion parameter
    ## B: B-splines basis
    ## P: penalty term/matrix
    ## a: coefficients
    
    ## Output:
    ## a: updated coefficients
    
    ## linear predictor
    eta <- B%*%a
    ## expected values
    mu <- exp(eta + offset)
    ## weights
    w <- c(wei*mu)
    ## working response
    z <- c(wei*((y - mu)/mu + eta))
    ## regression
    BtWB <- t(B) %*% (w * B)
    BtWz <- t(B) %*% (w * z)
    a <- solve(BtWB + psi2*lambdaP, BtWz)
    ## coefficients
    a <- matrix(a, ncol = 1)
    a
  }
print.summary.Mort1Dsmooth <-
  function(x, digits=max(3, getOption("digits")-3), ...){
    if(!is.null(cl <- x$call)){
      cat("Call:\n")
      dput(cl, control=NULL)
    }
    cat("\nNumber of Observations                  :", x$n, "\n")
    cat("Effective dimension                     :", format(signif(x$df, 4)), "\n")
    cat("(Selected) smoothing parameter          :", format(signif(x$lambda, 5)), "\n")
    cat("Bayesian Information Criterion (BIC)    :", signif(x$bic,5), "\n")
    cat("Akaike's Information Criterion (AIC)    :", signif(x$aic,5), "\n")
    cat("(Estimated) dispersion parameter (psi^2):", signif(x$psi2,3), "\n")
    cat("\nResiduals:\n", sep="")
    nam <- c("Min", "1Q", "Median", "3Q", "Max")
    rq <- structure(quantile(x$residuals), names = nam)
    print(rq, digits = digits, ...)
    cat("\nSettings and control:\n")
    cat("  number of B-splines    :", x$ndx + x$deg, "\n")
    cat("  degree of the B-splines:", x$deg, "\n")
    cat("  order of differences   :", x$pord, "\n")
    cat("  convergence tolerance  :", format(signif(x$tolerance, 5)))
    cat("\n")
    invisible(x)
  }
residuals.Mort1Dsmooth <-
  function(object,
           type = c("deviance",
                    "pearson",
                    "anscombe",
                    "working"), ...){
    type <- match.arg(type)
    r <- object$residuals
    y <- object$y
    fitted.values <- object$fitted.values
    w <- object$w
    res <- switch(type,
                  deviance = r, 
                  pearson  = (y - fitted.values) / sqrt(fitted.values),
                  anscombe = (3/2) * (y^(2/3) - fitted.values^(2/3)) / fitted.values^(1/6),
                  working  = (y - fitted.values)
    )
    res
  }
MortSmooth_BWB <-
  function(RTBx, RTBy, nbx, nby, W){
    ## Input:
    ## RTBx: row tensor of the B-spline basis for x
    ## RTBy: row tensor of the B-spline basis for y
    ## nbx: number of B-spline basis for x
    ## nby: number of B-spline basis for y
    ## W: matrix of weights
    
    ## Output:
    ## BWB: a matrix of inner product of a matrix and a sparse weight matrix
    
    BWB <- t(RTBx)%*%W%*%RTBy
    BWB <- array(c(BWB),c(nbx,nbx,nby,nby))
    BWB <- matrix(c(aperm(BWB,c(1,4,2,3))),ncol=nbx*nby)
    return(BWB)
  }
MortSmooth_bbase <-
  function(x, xl, xr, ndx, deg){
    ## Input:
    ## x   = abcissae of data
    ## xl  = left boundary
    ## xr  = right boundary
    ## ndx = number of internal knots -1
    ##       or number of internal intervals
    ## deg = degree of the splines
    
    ## Output:
    ## B = matrix with the B-spline basis
    
    ## distance between knots
    dx <- (xr - xl) / ndx
    ## One needs (ndx+1) internal knots and 
    ## deg knots on both right and left side
    ## in order to joint all the B-splines
    knots <- seq(xl - deg * dx, xr + deg * dx, by = dx)
    ## Truncated deg-th power functions
    ## equally-spaced at given knots along x
    P <- outer(x, knots, MortSmooth_tpower, deg)
    ## number of B-splines which equal to the number of knots
    n <- dim(P)[2]
    ## in the numerator we have the matrix
    ## of deg+1 differences for each knots
    ## this matrix is rescaled by 
    ## (deg! * dx^deg) == (gamma(deg + 1) * dx ^ deg)
    D <- diff(diag(n), diff = deg + 1) /
      (gamma(deg + 1) * dx ^ deg)
    ## the matrix of differences is used to compute B-splines
    ## as differences of truncated power functions
    ## in P %*% t(D)
    ## the last factor is (-1) ^ (deg + 1)
    B <- (-1) ^ (deg + 1) * P %*% t(D)
    B
  }
MortSmooth_tpower <-
  function(x, t, p){
    ## Input:
    ## x = abcissae of data
    (x - t) ^ p * (x > t)
    ## (x-t)^p gives the curve
    ## (x>t) is an indicator function; it is 1 when x>t
    ## and 0 when x<=t, i.e. before each knot
  }
cleversearch <- function(fn, lower, upper, ngrid, startvalue, logscale=TRUE, clever=TRUE, verbose=FALSE) {
  
  ##construct grid
  ndims <- length(lower)
  grid <- NULL
  for (i in 1:ndims) {
    if (logscale) {
      grid <- cbind(grid, 10^seq(lower[i], upper[i], length=ngrid))
    } else {
      grid <- cbind(grid, seq(lower[i], upper[i], length=ngrid))
    }
  }
  
  fmin <- Inf
  fn1 <- function(pnew) fn(pnew)
  
  if (clever) {
    
    ##initialize
    if (missing(startvalue)) {
      if (ndims == 1) { ##start at the lowest possible parameter
        index <- 1 
      } else {          ##start in the middle of grid
        index <- floor(ngrid/2) * rep(1, ndims) 
      }
    } else {
      index <- NULL
      for (i in 1:ndims) {
        tmp <- max(which(order(c(startvalue[i], grid[,i])) == 1) - 1, 1)
        index <- c(index, tmp)
      }
    }    
    par <- rep(0, ndims)
    for (i in 1:ndims) {
      par[i] <- grid[index[i], i]
    }
    lookup <- array(NA, rep(ngrid, ndims))
    
    ##search
    move <- 1
    nstep <- 0
    while (move) {
      
      move <- 0
      for (i in 1:ndims) {
        
        lookupi <- index
        
        for (j in (index[i] - 1):(index[i] + 1)) {
          
          j <- max(min(j, ngrid), 1)
          lookupi[i] <- j
          if (is.na(lookup[t(lookupi)])) {
            pnew <- par
            pnew[i] <- grid[j, i]
            fnew <- fn1(pnew)
            lookup[t(lookupi)] <- fnew
            nstep <- nstep + 1
          } else {
            fnew <- lookup[t(lookupi)]
          }
          if (fnew < fmin) {            
            fmin <- fnew
            index[i] <- j
            par <- pnew
            move <- move + 1
            if (verbose == TRUE) {
              cat(paste("\nIndex: ", paste(index, collapse=","),
                        ", Moved in step: ", nstep, ", Objective: ", fmin, "\n",
                        sep=""))
            }            
          }
        }##j
      }##i
    }##while
    
    
  } else { ##full grid evaluation
    
    if (ndims==1) {
      nstep <- 0
      pnew <- rep(0, ndims)
      for (i in 1:ngrid) {
        pnew <- grid[i, 1]
        fnew <- fn1(pnew)
        nstep <- nstep + 1
        if (fnew < fmin) {
          fmin <- fnew
          par <- pnew
          if (verbose == TRUE) {
            cat(paste("\nIndex: ", i, ", Moved in step: ", nstep, ", Objective: ",
                      fmin, "\n", sep=""))
          }
        }
      }
    } else if (ndims==2){
      nstep <- 0
      pnew <- rep(0, ndims)
      for (i in 1:ngrid) {
        pnew[1] <- grid[i, 1]
        for (j in 1:ngrid) {
          pnew[2] <- grid[j, 2]    
          fnew <- fn1(pnew)
          nstep <- nstep + 1
          if (fnew < fmin) {
            fmin <- fnew
            par <- pnew
            if (verbose == TRUE) {
              cat(paste("\nIndex: ", paste(c(i,j), collapse=","),
                        ", Moved in step: ", nstep, ", Objective: ", fmin, "\n",
                        sep=""))
            }
          }
        }
      }      
    } else if (ndims==3){
      nstep <- 0
      pnew <- rep(0, ndims)
      for (i in 1:ngrid) {
        pnew[1] <- grid[i, 1]
        for (j in 1:ngrid) {
          pnew[2] <- grid[j, 2]
          for (k in 1:ngrid) {
            pnew[3] <- grid[k, 3]          
            fnew <- fn1(pnew)
            nstep <- nstep + 1
            if (fnew < fmin) {
              fmin <- fnew
              par <- pnew
              if (verbose == TRUE) {
                cat(paste("\nIndex: ", paste(c(i,j,k), collapse=","),
                          ", Moved in step: ", nstep, ", Objective: ", fmin, "\n",
                          sep=""))
              }
            }
          }
        }
      }
    } else {
      stop("full grid evaluation only available on 1d, 2d and 3d parameters!")
    }
  }
  return(list(par=par, value=fmin, counts=nstep))  
}

