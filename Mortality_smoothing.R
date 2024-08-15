######################## Librerias (instalar)
{
  install.packages("readxl")
  install.packages("tidyverse")
  install.packages("haven")
  install.packages("readr")
  install.packages("lubridate")
  install.packages("dplyr")
  install.packages("tidyr")
  install.packages("ggplot2")
  install.packages("stringr")
  install.packages("devtools")
  install.packages("mgcv")
  #install.packages("svcm")
  install.packages("MortCast")
}
######################## Librerias (usar)
{
  library(gridExtra)
  library(haven)
  library(readr)
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(readxl)
  library(tidyverse)
  library(devtools)
  library(mgcv)
  #library(svcm)
  library(MortCast)
  library(StMoMo)
  #library(demography)
  #library(vital)
}
######################## Funciones 
# Tabla de vida (Chiang)
{
  #create_life_table <- function(data, rate_col, start_age = 25, end_age = 99, fn = 0.5) {
    life_table <- data.frame(matrix(nrow = nrow(data), ncol = 0))
    life_table$Edad <- data$Edad
    life_table$Mx <- data[[rate_col]]
    life_table <- life_table %>% filter(Edad >= start_age & Edad <= end_age)
    
    life_table <- life_table %>%
      mutate(qx = ifelse(Mx != 0, Mx / (1 + fn * Mx), 0)) # Setup probabilidad de muerte en un año
    life_table$qx[nrow(life_table)] <- 1
    life_table$lx <- numeric(nrow(life_table))
    life_table$dx <- numeric(nrow(life_table))
    life_table$lx[1] <- 100000
    life_table$dx[1] <- life_table$lx[1] * life_table$qx[1]
    
    for (i in 2:nrow(life_table)) { # Armar las columnas de lx y dx
      life_table$lx[i] <- life_table$lx[i - 1] - life_table$dx[i - 1]
      life_table$dx[i] <- life_table$lx[i] * life_table$qx[i]
    } 
    
    life_table$Lx <- numeric(nrow(life_table)) # Armemos Lx
    for (i in 1:(nrow(life_table) - 1)) {
      life_table$Lx[i] <- (life_table$lx[i] + life_table$lx[i + 1]) / 2
    }
    last_row <- nrow(life_table) # Truncar los valores w-esimos segun Chiang (1972)
    life_table$Lx[last_row] <- life_table$lx[last_row] / life_table$Mx[last_row]
    
    life_table$Tx <- numeric(nrow(life_table)) # Armemos Tx
    life_table$Tx[last_row] <- life_table$Lx[last_row] # se parte del final con el valor terminal de Lx = lx/mx
    for (i in (last_row - 1):1) { # se itera con lx + Tx+1(edad siguiente)
      life_table$Tx[i] <- life_table$Lx[i] + life_table$Tx[i + 1]
    }
    
    life_table$P_x1 <- numeric(nrow(life_table)) # Armemos P(x,1)
    life_table$P_x1[1] <- life_table$Lx[1] / life_table$lx[1] # Primer valor como Lx/lx
    for (i in 2:nrow(life_table)) { # Valores consecutivos como Tx/T(x-1)
      life_table$P_x1[i] <- life_table$Tx[i] / life_table$Tx[i - 1]
    }
    
    life_table <- life_table %>%
      mutate(e_0 = ifelse(Tx != 0, Tx / lx))
    
    return(life_table)
  }
  #generate_life_tables <- function(data, gender, year) {
    categories <- c("alta", "media", "baja", "totales")
    methods <- c("observed", "WH", "Splines")
    
    for (category in categories) {
      for (method in methods) {
        rate_col <- paste0("Mx_", method, "_", category)
        life_table_name <- paste0("lf", gender, "_", category, "_", year, "_", tolower(method))
        
        # Generate the life table
        life_table <- create_life_table(data, rate_col)
        
        # Assign the life table to a variable with the appropriate name
        assign(life_table_name, life_table, envir = .GlobalEnv)
      }
    }
  }
  create_life_table <- function(data, rate_col, age_col = "age", start_age = 25, end_age = 110, fn = 0.5) {
    life_table <- data.frame(matrix(nrow = nrow(data), ncol = 0))
    life_table$Edad <- data[[age_col]]
    life_table$Mx <- data[[rate_col]]
    life_table <- life_table %>% filter(Edad >= start_age & Edad <= end_age)
    
    life_table <- life_table %>%
      mutate(qx = ifelse(Mx != 0, Mx / (1 + fn * Mx), 0)) # Setup probabilidad de muerte en un año
    life_table$qx[nrow(life_table)] <- 1
    life_table$lx <- numeric(nrow(life_table))
    life_table$dx <- numeric(nrow(life_table))
    life_table$lx[1] <- 100000
    life_table$dx[1] <- life_table$lx[1] * life_table$qx[1]
    
    for (i in 2:nrow(life_table)) { # Armar las columnas de lx y dx
      life_table$lx[i] <- life_table$lx[i - 1] - life_table$dx[i - 1]
      life_table$dx[i] <- life_table$lx[i] * life_table$qx[i]
    } 
    
    life_table$Lx <- numeric(nrow(life_table)) # Armemos Lx
    for (i in 1:(nrow(life_table) - 1)) {
      life_table$Lx[i] <- (life_table$lx[i] + life_table$lx[i + 1]) / 2
    }
    last_row <- nrow(life_table) # Truncar los valores w-esimos segun Chiang (1972)
    life_table$Lx[last_row] <- life_table$lx[last_row] / life_table$Mx[last_row]
    
    life_table$Tx <- numeric(nrow(life_table)) # Armemos Tx
    life_table$Tx[last_row] <- life_table$Lx[last_row] # se parte del final con el valor terminal de Lx = lx/mx
    for (i in (last_row - 1):1) { # se itera con lx + Tx+1(edad siguiente)
      life_table$Tx[i] <- life_table$Lx[i] + life_table$Tx[i + 1]
    }
    
    life_table$P_x1 <- numeric(nrow(life_table)) # Armemos P(x,1)
    life_table$P_x1[1] <- life_table$Lx[1] / life_table$lx[1] # Primer valor como Lx/lx
    for (i in 2:nrow(life_table)) { # Valores consecutivos como Tx/T(x-1)
      life_table$P_x1[i] <- life_table$Tx[i] / life_table$Tx[i - 1]
    }
    
    life_table <- life_table %>%
      mutate(e_0 = ifelse(Tx != 0, Tx / lx))
    
    return(life_table)
  }
  generate_life_tables <- function(data, gender, year, age_col = "age") {
    categories <- c("alta", "media", "baja", "totales")
    methods <- c("observed", "WH", "Splines")
    
    for (category in categories) {
      for (method in methods) {
        rate_col <- paste0("Mx_", method, "_", category)
        life_table_name <- paste0("lf", gender, "_", category, "_", year, "_", tolower(method))
        
        # Generate the life table
        life_table <- create_life_table(data, rate_col, age_col)
        
        # Assign the life table to a variable with the appropriate name
        assign(life_table_name, life_table, envir = .GlobalEnv)
      }
    }
  }

  

# WH y Splines
{
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
}
# Funcionales de tablas y df
{
  create_joined_dataframe <- function(df1, df2, df3, df4_pob, edad_range, output_name) {
    # Create a data frame with one column "Edad" that goes from the start to the end of the specified range
    edad_df <- data.frame(Edad = edad_range)
    
    # Left join the three data frames on the "Edad" variable
    joined_df <- edad_df %>%
      left_join(df1, by = "Edad") %>%
      left_join(df2, by = "Edad") %>%
      left_join(df3, by = "Edad") %>%
      left_join(df4_pob, by = "Edad")
    
    # Remove the 'year', 'total', and 'NA' columns if they exist
    columns_to_remove <- c("year", "total", "NA")
    joined_df <- joined_df %>%
      select(-any_of(columns_to_remove))
    
    # Filter to keep only ages 25 and up
    joined_df <- joined_df %>%
      filter(Edad >= 25)
    
    # Determine the maximum age to group from
    max_age <- max(edad_range)
    
    # Group ages from max_age and above and summarize the data
    tabla_sub_max <- joined_df %>%
      filter(Edad < max_age)
    
    tabla_max_y_mas <- joined_df %>%
      filter(Edad >= max_age) %>%
      summarise(
        Edad = max_age,
        alta = sum(alta, na.rm = TRUE),
        media = sum(media, na.rm = TRUE),
        baja = sum(baja, na.rm = TRUE),
        esc_bajo = sum(esc_bajo, na.rm = TRUE),
        esc_medio = sum(esc_medio, na.rm = TRUE),
        esc_alto = sum(esc_alto, na.rm = TRUE)
      )
    
    # Combine the summarized data with the other age groups
    joined_df <- tabla_sub_max %>%
      bind_rows(tabla_max_y_mas) %>%
      arrange(Edad)
    
    # Compute the totals for mortality and population
    joined_df <- joined_df %>%
      mutate(
        totales = baja + media + alta,
        esc_totales = esc_bajo + esc_medio + esc_alto
      )
    
    # Assign the resulting data frame to the name specified in the function input
    assign(output_name, joined_df, envir = .GlobalEnv)
    
    # Return the joined data frame
    return(joined_df)
  }
  smooth_mortality <- function(df, age_col = "Edad") {
    categories <- c("alta", "media", "baja", "totales")
    exposures <- c("esc_alto", "esc_medio", "esc_bajo", "esc_totales")
    
    # Initialize an empty list to store the results
    results_list <- list()
    
    for (i in seq_along(categories)) {
      deaths_col <- categories[i]
      exposures_col <- exposures[i]
      
      # Ensure exposure is numeric and non-zero to avoid division by zero or NA
      exposure_vals <- as.numeric(df[[exposures_col]])
      exposure_vals[is.na(exposure_vals) | exposure_vals == 0] <- NA
      
      # Compute observed mortality rate
      Mx_observed <- df[[deaths_col]] / exposure_vals
      
      # Whittaker-Henderson smoothing
      fit_wh <- Mort1Dsmooth(x = df[[age_col]], y = df[[deaths_col]], offset = log(exposure_vals))
      smoothed_wh <- fitted(fit_wh)
      
      # Compute WH smoothed mortality rate
      Mx_WH <- smoothed_wh / exposure_vals
      
      # Penalized Splines smoothing
      data <- data.frame(age = df[[age_col]], deaths = df[[deaths_col]], exposures = exposure_vals)
      fit_ps <- gam(deaths ~ s(age, bs = "ps") + offset(log(exposures)), family = poisson, data = data)
      smoothed_ps <- fitted(fit_ps)
      
      # Compute Splines smoothed mortality rate
      Mx_Splines <- smoothed_ps / exposure_vals
      
      # Store the results in the list
      results_list[[deaths_col]] <- data.frame(
        age = df[[age_col]], 
        Mx_observed = Mx_observed, 
        WH = smoothed_wh,
        Mx_WH = Mx_WH,
        Splines = smoothed_ps,
        Mx_Splines = Mx_Splines
      )
    }
    
    # Combine the results into a single data frame
    combined_results <- data.frame(age = df[[age_col]])
    for (i in seq_along(categories)) {
      combined_results <- combined_results %>%
        left_join(results_list[[categories[i]]], by = "age", suffix = c("", paste0("_", categories[i])))
    }
    
    # Hard-code the renaming for alta category
    colnames(combined_results)[2] <- "Mx_observed_alta"
    colnames(combined_results)[3] <- "WH_alta"
    colnames(combined_results)[4] <- "Mx_WH_alta"
    colnames(combined_results)[5] <- "Splines_alta"
    colnames(combined_results)[6] <- "Mx_Splines_alta"
    
    # Compute total mortality rates for observed and smoothed data
    combined_results <- combined_results %>%
      mutate(
        Mx_totales_observed = Mx_observed_totales,
        Mx_totales_WH = WH_totales / df$esc_totales,
        Mx_totales_Splines = Splines_totales / df$esc_totales
      )
    
    return(combined_results)
  }
  save_life_tables_to_csv <- function(life_table_data, gender, year) {
    categories <- c("alta", "media", "baja", "totales")
    methods <- c("observed", "WH", "splines")
    
    for (category in categories) {
      for (method in methods) {
        file_name <- paste0("life_table_", gender, "_", category, "_", year, "_", method, ".csv")
        write.csv(life_table_data[[paste0("lf", gender, "_", category, "_", year, "_", method)]], file_name, row.names = FALSE)
      }
    }
  }
  
  
} 
# Extrapolacion Kannisto:
  {
    extend_mortality_rates <- function(smot_df) {
      # Identify the columns in the smot_df that are mortality rates (Mx)
      rate_columns <- grep("^Mx_", names(smot_df), value = TRUE)
      
      # Create a new age vector from the minimum age in smot_df to 110
      extended_ages <- 25:110
      
      # Initialize a new data frame with extended ages
      extended_smot_df <- data.frame(age = extended_ages)
      
      # Loop through each rate column and apply the Kannisto method
      for (rate_col in rate_columns) {
        # Extract the mortality rates for the ages in the current data frame
        mx <- smot_df[[rate_col]]
        names(mx) <- smot_df$age
        
        # Apply the Kannisto method to extend the rates from 90 to 110
        mx_extended <- kannisto(mx, est.ages = 80:90, proj.ages = 91:110)
        
        # Create a new data frame with the extended ages and the extended mortality rates
        mx_extended_df <- data.frame(age = as.numeric(names(mx_extended)), mx_extended = mx_extended)
        
        # Merge the extended mortality rates into the extended_smot_df
        extended_smot_df <- merge(extended_smot_df, mx_extended_df, by = "age", all.x = TRUE)
        
        # Rename the column to match the original naming convention
        names(extended_smot_df)[ncol(extended_smot_df)] <- rate_col
      }
      
      return(extended_smot_df)
    }
  }
# Funcionales de ploteo
{
  plot_mortality_comparison <- function(df, year, gender) {
    categories <- c("alta", "media", "baja", "totales")
    
    for (category in categories) {
      plot <- ggplot(df, aes(x = age)) +
        geom_line(aes(y = get(paste0("Mx_observed_", category)), color = "Observed"), size = 1) +
        geom_point(aes(y = get(paste0("Mx_observed_", category)), color = "Observed"), size = 1.5) +
        geom_line(aes(y = get(paste0("Mx_WH_", category)), color = "WH"), size = 1, linetype = "dashed") +
        geom_point(aes(y = get(paste0("Mx_WH_", category)), color = "WH"), size = 1.5) +
        geom_line(aes(y = get(paste0("Mx_Splines_", category)), color = "Splines"), size = 1, linetype = "dotted") +
        geom_point(aes(y = get(paste0("Mx_Splines_", category)), color = "Splines"), size = 1.5) +
        geom_vline(xintercept = 90, linetype = "dashed", color = "black") +
        labs(
          title = paste("Comparison of Mortality Rates for", category, "-", gender, year),
          x = "Age",
          y = "Mortality Rate (Mx)",
          color = "Method"
        ) +
        scale_color_manual(values = c("Observed" = "blue", "WH" = "red", "Splines" = "green")) +
        xlim(60, max(df$age)) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "top"
        )
      
      # Save the plot to a file
      ggsave(paste0("mortality_rate_comparison_", category, "_", gender, "_", year, ".png"), plot = plot)
    }
  } # Para comparasion de Tasas de mortalidad por metodo/categoria/año/genero
  
  plot_life_expectancy_comparison <- function(life_table_data, gender, year) {
    categories <- c("alta", "media", "baja", "totales")
    methods <- c("observed", "WH", "Splines")
    
    for (category in categories) {
      # Check if the necessary columns exist in the life tables
      life_tables_exist <- all(sapply(methods, function(method) {
        paste0("lf", gender, "_", category, "_", year, "_", method) %in% names(life_table_data)
      }))
      
      if (!life_tables_exist) {
        message(paste("Life tables for", category, "in", year, "are incomplete. Skipping..."))
        next
      }
      
      # Extract the life tables
      e_0_observed <- life_table_data[[paste0("lf", gender, "_", category, "_", year, "_observed")]]$e_0
      e_0_WH <- life_table_data[[paste0("lf", gender, "_", category, "_", year, "_WH")]]$e_0
      e_0_Splines <- life_table_data[[paste0("lf", gender, "_", category, "_", year, "_Splines")]]$e_0
      age <- life_table_data[[paste0("lf", gender, "_", category, "_", year, "_observed")]]$Edad
      
      # Plotting
      p <- ggplot() +
        geom_line(aes(x = age, y = e_0_observed, color = "Observed"), size = 1) +
        geom_point(aes(x = age, y = e_0_observed, color = "Observed"), size = 1.5) +
        geom_line(aes(x = age, y = e_0_WH, color = "WH"), size = 1, linetype = "dashed") +
        geom_point(aes(x = age, y = e_0_WH, color = "WH"), size = 1.5) +
        geom_line(aes(x = age, y = e_0_Splines, color = "Splines"), size = 1, linetype = "dotted") +
        geom_point(aes(x = age, y = e_0_Splines, color = "Splines"), size = 1.5) +
        geom_vline(xintercept = c(65, 90), linetype = "dashed", color = "black") +
        labs(
          title = paste("Life Expectancy Comparison for", category, gender, year),
          x = "Age",
          y = "Life Expectancy (e_0)",
          color = "Method"
        ) +
        scale_color_manual(values = c("Observed" = "blue", "WH" = "red", "Splines" = "green")) +
        scale_x_continuous(breaks = seq(60, 110, by = 10), limits = c(60, 110)) +
        scale_y_continuous(breaks = seq(0, max(c(e_0_observed, e_0_WH, e_0_Splines), na.rm = TRUE), by = 5)) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "top"
        )
      
      # Save the plot
      ggsave(paste0("life_expectancy_comparison_", category, "_", gender, "_", year, ".png"), plot = p, width = 7, height = 7)
    }
  }  
  
  plot_method_comparison <- function(life_table_data, gender, year) {
    categories <- c("alta", "media", "baja", "totales")
    methods <- c("observed", "WH", "Splines")
    
    for (method in methods) {
      # Check if the necessary columns exist in the life tables
      life_tables_exist <- all(sapply(categories, function(category) {
        paste0("lf", gender, "_", category, "_", year, "_", method) %in% names(life_table_data)
      }))
      
      if (!life_tables_exist) {
        message(paste("Life tables for", method, "in", year, "are incomplete. Skipping..."))
        next
      }
      
      # Extract the life tables for each category
      data_list <- lapply(categories, function(category) {
        life_table_data[[paste0("lf", gender, "_", category, "_", year, "_", method)]]
      })
      
      # Combine the data for all categories
      combined_data <- do.call(rbind, lapply(seq_along(categories), function(i) {
        data <- data_list[[i]]
        data$category <- categories[i]
        return(data)
      }))
      
      # Plotting
      p <- ggplot(combined_data, aes(x = Edad, y = e_0, color = category)) +
        geom_line(size = 1) +
        geom_point(size = 1.5) +
        geom_vline(xintercept = c(65, 90), linetype = "dashed", color = "black") +
        labs(
          title = paste("Life Expectancy Comparison for", method, gender, year),
          x = "Age",
          y = "Life Expectancy (e_0)",
          color = "Category"
        ) +
        scale_color_manual(values = c("alta" = "blue", "media" = "red", "baja" = "green", "totales" = "purple")) +
        xlim(60, max(combined_data$Edad)) +
        scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5)) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "top"
        )
      
      # Save the plot
      ggsave(paste0("life_expectancy_comparison_", method, "_", gender, "_", year, ".png"), plot = p, width = 7, height = 7)
    }
  }
  
  plot_qx_comparison <- function(life_table_data, gender, year) {
    categories <- c("alta", "media", "baja", "totales")
    methods <- c("observed", "WH", "splines")
    
    for (category in categories) {
      plot <- ggplot(life_table_data[[paste0("lf", gender, "_", category, "_", year, "_observed")]], aes(x = Edad)) +
        geom_line(aes(y = qx, color = "Observed"), size = 1) +
        geom_point(aes(y = qx, color = "Observed"), size = 1.5) +
        geom_line(data = life_table_data[[paste0("lf", gender, "_", category, "_", year, "_WH")]], aes(y = qx, color = "WH"), size = 1, linetype = "dashed") +
        geom_point(data = life_table_data[[paste0("lf", gender, "_", category, "_", year, "_WH")]], aes(y = qx, color = "WH"), size = 1.5) +
        geom_line(data = life_table_data[[paste0("lf", gender, "_", category, "_", year, "_splines")]], aes(y = qx, color = "Splines"), size = 1, linetype = "dotted") +
        geom_point(data = life_table_data[[paste0("lf", gender, "_", category, "_", year, "_splines")]], aes(y = qx, color = "Splines"), size = 1.5) +
        # Add vertical lines at ages 65, 75, and 85
        geom_vline(xintercept = 65, linetype = "dashed", color = "black") +
        geom_vline(xintercept = 90, linetype = "dashed", color = "black") +
        labs(
          title = paste("Comparison of Probability of Death (qx) for", category, gender, year),
          x = "Age",
          y = "Probability of Death (qx)",
          color = "Method"
        ) +
        scale_color_manual(values = c("Observed" = "blue", "WH" = "red", "Splines" = "green")) +
        xlim(60, max(life_table_data[[paste0("lf", gender, "_", category, "_", year, "_observed")]]$Edad)) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "top"
        )
      
      # Save the plot
      ggsave(paste0("qx_comparison_", category, "_", gender, "_", year, ".png"), plot = plot)
    }
  }
  
  
}


#smooth_mortality <- function(df, age_col = "Edad") {
  categories <- c("alta", "media", "baja")
  exposures <- c("esc_alto", "esc_medio", "esc_bajo")
  
  # Initialize an empty list to store the results
  results_list <- list()
  
  for (i in seq_along(categories)) {
    deaths_col <- categories[i]
    exposures_col <- exposures[i]
    
    # Ensure exposure is numeric and non-zero to avoid division by zero or NA
    exposure_vals <- as.numeric(df[[exposures_col]])
    exposure_vals[is.na(exposure_vals) | exposure_vals == 0] <- NA
    
    # Compute observed mortality rate
    Mx_observed <- df[[deaths_col]] / exposure_vals
    
    # Whittaker-Henderson smoothing
    fit_wh <- Mort1Dsmooth(x = df[[age_col]], y = df[[deaths_col]], offset = log(exposure_vals))
    smoothed_wh <- fitted(fit_wh)
    
    # Compute WH smoothed mortality rate
    Mx_WH <- smoothed_wh / exposure_vals
    
    # Penalized Splines smoothing
    data <- data.frame(age = df[[age_col]], deaths = df[[deaths_col]], exposures = exposure_vals)
    fit_ps <- gam(deaths ~ s(age, bs = "ps") + offset(log(exposures)), family = poisson, data = data)
    smoothed_ps <- fitted(fit_ps)
    
    # Compute Splines smoothed mortality rate
    Mx_Splines <- smoothed_ps / exposure_vals
    
    # Store the results in the list
    results_list[[deaths_col]] <- data.frame(
      age = df[[age_col]], 
      Mx_observed = Mx_observed, 
      WH = smoothed_wh,
      Mx_WH = Mx_WH,
      Splines = smoothed_ps,
      Mx_Splines = Mx_Splines
    )
  }
  
  # Combine the results into a single data frame
  combined_results <- data.frame(age = df[[age_col]])
  for (i in seq_along(categories)) {
    combined_results <- combined_results %>%
      left_join(results_list[[categories[i]]], by = "age", suffix = c("", paste0("_", categories[i])))
  }
  
  # Hard-code the renaming for alta category
  colnames(combined_results)[2] <- "Mx_observed_alta"
  colnames(combined_results)[3] <- "WH_alta"
  colnames(combined_results)[4] <- "Mx_WH_alta"
  colnames(combined_results)[5] <- "Splines_alta"
  colnames(combined_results)[6] <- "Mx_Splines_alta"
  
  # Compute total mortality rates for observed and smoothed data, only if the rates are not NA
  combined_results <- combined_results %>%
    mutate(
      Mx_totales_observed = rowSums(select(combined_results, starts_with("Mx_observed_")), na.rm = TRUE),
      Mx_totales_WH = rowSums(select(combined_results, starts_with("WH_")), na.rm = TRUE) / df$esc_totales,
      Mx_totales_Splines = rowSums(select(combined_results, starts_with("Splines_")), na.rm = TRUE) / df$esc_totales
    )
  
  return(combined_results)
} #funciona mas, tiene errores en totales
  #save_life_tables_as_images <- function(life_table_data, gender, year) #{
  #{categories <- c("alta", "media", "baja", "totales")
  methods <- c("observed", "WH", "splines")
  
  for (category in categories) {
    for (method in methods) {
      # Construct the life table name
      life_table_name <- paste0("lf", gender, "_", category, "_", year, "_", method)
      
      # Check if the life table exists in the list
      if (exists(life_table_name, where = life_table_data)) {
        life_table_subset <- life_table_data[[life_table_name]] %>%
          filter(Edad >= 50 & Edad <= 85)
        
        # Create a table grob
        table_grob <- tableGrob(life_table_subset)
        
        # Save the table as an image
        file_name <- paste0("life_table_", gender, "_", category, "_", year, "_", method, "_50to85.png")
        ggsave(file_name, plot = table_grob, width = 10, height = 8, units = "in")
      } else {
        message(paste("Life table", life_table_name, "not found. Skipping."))
      }
    }
  }
  } # salio pesimo hay que reconfigurar
  #  
  
    