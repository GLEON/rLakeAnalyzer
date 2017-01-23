##########################################################################
## Original function directly from ForTran
r2b_orig = function(k1,k2,x,y) {
  
  if(k2-k1<=2) {
    r2b=0
    #    a = 0  # original code does not produce a or b?
    #    b = 0
  } else {
    is = k1:(k2-1)
    sxx = sum(x[is]^2)
    sxy = sum(x[is]*y[is])
    sy = sum(y[is])
    sx = sum(x[is])
    
    n = k2-k1
    
    a = 0.0
    if(k1 > 1) {
      a = (n*sxy-sy*sx)/(n*sxx-sx*sx)
    }
    b = (sy-a*sx)/n
    r2b = max( abs( y[is] - a*x[is] - b )/sqrt(a^2 + 1) )
  }
  
  #return(data.frame(r2b=r2b,a=a,b=b))
  return(r2b)
}

########################################################################
## Slow function using lm()
r2b_lm = function(k1,k2,x,y) {
   if(k2-k1<=2) {
     r2b=0
     a = 0  # original code does not produce a or b?
     b = 0
   } else {
     is = k1:(k2-1)
  
     lmResult <- lm(y[is] ~ x[is])
     n = k2-k1
  
  
     if(k1 > 1) {
       a = as.numeric(coef(lmResult)["x[is]"])
       b = as.numeric(coef(lmResult)["(Intercept)"])
     } else {
       a = 0.0
       b = (sum(y[is]) - a * sum(x[is]))/n
     }
  
  
     r2b = max( abs( y[is] - a*x[is] - b )/sqrt(a^2 + 1) )
   }
  
  #may eventually need this as a dataframe to utilize all of lm()
  #only relevant when k1 > 1
  #return(data.frame(r2b=r2b,a=a,b=b))
  return(r2b)
}
##########################################################################
## Faster function realizing speed improvements from simplelm()
r2b_fast = function(k1,k2,x,y) {
  if(k2-k1<=2) {
    r2b=0
    a = 0  # original code does not produce a or b?
    b = 0
  } else {
    is = k1:(k2-1)
    
    ## Taken from http://stackoverflow.com/a/40142643/5596534
    ## Keeping in other parameters for future considerations
    simplelm <- function (x, y) {
      ## number of data
      #n <- length(x)
      ## centring
      y0 <- .Internal(mean(y)); yc <- y - y0
      x0 <- .Internal(mean(x)); xc <- x - x0
      ## fitting an intercept-free model: yc ~ xc + 0
      xty <- drop(crossprod(xc, yc))
      xtx <- drop(crossprod(xc))
      slope <- xty / xtx
      #rc <- yc - xc * slope
      ## Pearson estimate of residual standard error
      #sigma2 <- drop(crossprod(rc)) / (n - 2)
      ## standard error for slope
      #slope_se <- sqrt(sigma2 / xtx)
      ## t-score and p-value for slope
      #tscore <- slope / slope_se
      #pvalue <- 2 * pt(abs(tscore), n - 2, lower.tail = FALSE)
      ## return estimation summary for slope
      intercept <- y0 - x0 * slope
      return(c(slope, intercept))
      #c("Estimate" = slope, "Std. Error" = slope_se, "t value" = tscore, "Pr(>|t|)" = pvalue)
    }
    
    
    lmResult <- simplelm(y=y[is],x=x[is])
    n = k2-k1
    
    
    if(k1 > 1) {
      a = lmResult[1]
      b = lmResult[2]
    } else {
      a = 0.0
      b = (sum(y[is]) - a * sum(x[is]))/n
    }
    
    
    r2b = max( abs( y[is] - a*x[is] - b )/sqrt(a^2 + 1) )
  }
  
  #return(data.frame(r2b=r2b,a=a,b=b))
  return(r2b)
}


#############################################################################


## Now to test with some data
ni <- c( 1, 201, 402 )
i <- 1
k1 <- ni[i]
k2 <- ni[i+1]

## Do these functions produce equivalent results?
identical(r2b_lm(k1, k2, y=t11$temper, x=t11$depth), r2b_orig(k1, k2, y=t11$temper, x=t11$depth)  )
identical(r2b_fast(k1, k2, y=t11$temper, x=t11$depth), r2b_orig(k1, k2, y=t11$temper, x=t11$depth) )


## And what is the speed comparison?
library(microbenchmark)
microbenchmark(r2b_lm(k1, k2, y=t11$temper, x=t11$depth),
               r2b_orig(k1, k2, y=t11$temper, x=t11$depth),
               r2b_fast(k1, k2, y=t11$temper, x=t11$depth),
               times=10000)







