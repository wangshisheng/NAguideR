##################################################################################
#### MLE for the Truncated Normal
#### Creating a Function that Returns the Log Likelihood, Gradient and
#### Hessian Functions
##################################################################################

## data = numeric vector
## t    = truncation limits
mklhood <- function(data, t, ...) {

    data <- na.omit(data)
    n <- length(data)
    t <- sort(t)

    psi<-function(y, mu, sigma){
        exp(-(y-mu)^2/(2*sigma^2))/(sigma*sqrt(2*pi))
    }

    psi.mu<-function(y,mu,sigma){
        exp(-(y-mu)^2/(2*sigma^2)) * ((y-mu)/(sigma^3*sqrt(2*pi)))
    }

    psi.sigma<-function(y,mu,sigma){
        exp(-(y-mu)^2/(2*sigma^2)) *
            (((y-mu)^2)/(sigma^4*sqrt(2*pi)) - 1/(sigma^2*sqrt(2*pi)))
    }

    psi2.mu<-function(y,mu,sigma){
        exp(-(y - mu)^2/(2*sigma^2)) *
            (((y - mu)^2)/(sigma^5*sqrt(2*pi))-1/(sigma^3*sqrt(2*pi)))
    }

    psi2.sigma<-function(y,mu,sigma){
        exp(-(y-mu)^2/(2*sigma^2)) *
            ((2)/(sigma^3*sqrt(2*pi)) - (5*(y-mu))/(sigma^5*sqrt(2*pi)) +
             ((y-mu)^4)/(sigma^7*sqrt(2*pi)))
    }

    psi12.musig<-function(y,mu,sigma){
        exp(-(y-mu)^2/(2*sigma^2)) *
            (((y-mu)^3)/(sigma^6*sqrt(2*pi)) - (3*(y-mu))/(sigma^4*sqrt(2*pi)))
    }

    ll.tnorm2<-function(p){
        out <- (-n*log(pnorm(t[2],p[1],p[2])-pnorm(t[1],p[1],p[2]))) -
            (n*log(sqrt(2*pi*p[2]^2))) - (sum((data-p[1])^2)/(2*p[2]^2))
        -1*out
    }

    grad.tnorm<-function(p){
        g1 <- (-n*(integrate(psi.mu,t[1],t[2],mu=p[1],sigma=p[2], stop.on.error = FALSE)$value) /
               (pnorm(max(t),p[1],p[2])-pnorm(min(t),p[1],p[2]))) - ((n*p[1]-sum(data))/p[2]^2)
        g2 <- (-n*(integrate(psi.sigma,t[1],t[2],mu=p[1],sigma=p[2], stop.on.error = FALSE)$value) /
               (pnorm(max(t),p[1],p[2])-pnorm(min(t),p[1],p[2]))) - ((n)/(p[2])) + ((sum((data-p[1])^2))/(p[2]^3))
        out <- c(g1,g2)
        return(out)
    }

    hessian.tnorm<-function(p){

        h1<- -n*(integrate(psi,t[1],t[2],mu=p[1],sigma=p[2], stop.on.error = FALSE)$value *
                                                                                   integrate(psi2.mu,t[1],t[2],mu=p[1],sigma=p[2], stop.on.error = FALSE)$value -
                                                                                                                                                         integrate(psi.mu,t[1],t[2],mu=p[1],sigma=p[2], stop.on.error = FALSE)$value^2) /
            (integrate(psi,t[1],t[2],mu=p[1],sigma=p[2], stop.on.error = FALSE)$value^2) -
            n/(p[2]^2)

        h3<- -n*(integrate(psi,t[1],t[2],mu=p[1],sigma=p[2], stop.on.error = FALSE)$value *
                                                                                   integrate(psi12.musig,t[1],t[2],mu=p[1],sigma=p[2], stop.on.error = FALSE)$value -
                                                                                                                                                             integrate(psi.mu,t[1],t[2],mu=p[1],sigma=p[2], stop.on.error = FALSE)$value *
                                                                                                                                                                                                                                  integrate(psi.sigma,t[1],t[2],mu=p[1],sigma=p[2], stop.on.error = FALSE)$value) /
            (integrate(psi,t[1],t[2],mu=p[1],sigma=p[2], stop.on.error = FALSE)$value^2) +
            (2*(n*p[1]-sum(data)))/(p[2]^3)

        h2<- -n*(integrate(psi,t[1],t[2],mu=p[1],sigma=p[2], stop.on.error = FALSE)$value *
                                                                                   integrate(psi2.sigma,t[1],t[2],mu=p[1],sigma=p[2], stop.on.error = FALSE)$value -
                                                                                                                                                            integrate(psi.sigma,t[1],t[2],mu=p[1],sigma=p[2], stop.on.error = FALSE)$value^2) /
            (integrate(psi,t[1],t[2],mu=p[1],sigma=p[2], stop.on.error = FALSE)$value^2) +
            (n)/(p[2]^2)-(3*sum((data-p[1])^2))/(p[2]^4)

        H<-matrix(0,nrow=2,ncol=2)
        H[1,1]<-h1
        H[2,2]<-h2
        H[1,2]<-H[2,1]<-h3
        return(H)
    }


    return(list(ll.tnorm2 = ll.tnorm2, grad.tnorm = grad.tnorm, hessian.tnorm = hessian.tnorm))
}
##################################################################################
###### Newton Raphson Function
###### This takes in the Objects Returned from mklhood Function above
##################################################################################

NewtonRaphsonLike <- function(lhood, p, tol = 1e-07, maxit = 100) {

    cscore <- lhood$grad.tnorm(p)
    if(sum(abs(cscore)) < tol)
        return(list(estimate = p, value = lhood$ll.tnorm2(p), iter = 0))
    cur <- p
    for(i in 1:maxit) {
        inverseHess <- solve(lhood$hessian.tnorm(cur))
        cscore <- lhood$grad.tnorm(cur)
        new <- cur - cscore %*% inverseHess
        if (new[2] <= 0) stop("Sigma < 0")
        cscore <- lhood$grad.tnorm(new)

        if(((abs(lhood$ll.tnorm2(cur)- lhood$ll.tnorm2(new))/(lhood$ll.tnorm2(cur))) < tol))
            return(list(estimate = new, value= lhood$ll.tnorm2(new), iter = i))
        cur <- new
    }

    return(list(estimate = new, value= lhood$ll.tnorm2(new), iter = i))
}

##################################################################################
###### Based on the MLE Functions (mklhood) and NewtonRaphson Function
###### (NewtonRaphsonLike), This function estimates the MEAN and SD from the
###### Truncated using Newton Raphson.
##################################################################################

## missingdata = matrix where rows = features, columns = samples
## perc = if %MVs > perc then just sample mean / SD
## iter = # iterations in NR algorithm

EstimatesComputation <- function(missingdata, perc, iter=50) {

    ## 2 column matrix where column 1 = means, column 2 = SD
    ParamEstim <- matrix(NA, nrow = nrow(missingdata), ncol = 2)
    nsamp <- ncol(missingdata)

    ## sample means / SDs
    ParamEstim[,1] <- rowMeans(missingdata, na.rm = TRUE)
    ParamEstim[,2] <- apply(missingdata, 1, function(x) sd(x, na.rm = TRUE))

    ## Case 1: missing % > perc => use sample mean / SD
    na.sum <- apply(missingdata, 1, function(x) sum(is.na(x)))
    idx1 <- which(na.sum/nsamp >= perc)

    ## Case 2: sample mean > 3 SD away from LOD => use sample mean / SD
    lod <- min(missingdata, na.rm=TRUE) ## why use the min of whole data set??????
    idx2 <- which(ParamEstim[,1] > 3*ParamEstim[,2] + lod)

    ## Case 3: for all others, use NR method to obtain truncated mean / SD estimate
    idx.nr <- setdiff(1:nrow(missingdata), c(idx1, idx2))
    ## t = limits of integration (LOD and upper)
    upplim <- max(missingdata, na.rm=TRUE) + 2*max(ParamEstim[,2])
    for (i in idx.nr) {
        Likelihood <- mklhood(missingdata[i,], t=c(lod, upplim))
        res <- tryCatch(NewtonRaphsonLike(Likelihood, p = ParamEstim[i,]),
                        error = function(e) 1000)

        if (length(res) == 1) {
            next
        } else if (res$iter >= iter) {
            next
        } else {
            ParamEstim[i,] <- as.numeric(res$estimate)
        }
    }
    return(ParamEstim)
}



####################################################################
#### This Function imputes the data BASED on KNN-EUCLIDEAN
####################################################################

## data = data set to be imputed, where rows = features, columns = samples
## k    = number of neighbors for imputing values
## rm.na, rm.nan, rm.inf = whether NA, NaN, and Inf values should be imputed


KNNEuc <- function (data, k, rm.na = TRUE, rm.nan = TRUE, rm.inf = TRUE) {

    nr <- dim(data)[1]

    imp.knn <- data
    imp.knn[is.finite(data) == FALSE] <- NA
    t.data<-t(data)

    mv.ind <- which(is.na(imp.knn), arr.ind = TRUE)
    arrays <- unique(mv.ind[, 2])
    array.ind <- match(arrays, mv.ind[, 2])
    nfeatures <- 1:nr

    for (i in 1:length(arrays)) {
        set <- array.ind[i]:min((array.ind[(i + 1)] - 1), dim(mv.ind)[1], na.rm = TRUE)
        cand.features <- nfeatures[-unique(mv.ind[set, 1])]
        cand.vectors <- t.data[,cand.features]
        exp.num <- arrays[i]

        for (j in set) {
            feature.num <- mv.ind[j, 1]
            tar.vector <- data[feature.num,]

            dist <- sqrt(colMeans((tar.vector-cand.vectors)^2, na.rm = TRUE))
            dist[is.nan(dist) | is.na(dist)] <- Inf
            dist[dist==0] <- ifelse(is.finite(min(dist[dist>0])), min(dist[dist>0])/2, 1)

            if (sum(is.finite(dist)) < k) {
                stop(message = "Fewer than K finite distances found")
            }
            k.features.ind <- order(dist)[1:k]
            k.features <- cand.features[k.features.ind]
            wghts <- 1/dist[k.features.ind]/sum(1/dist[k.features.ind])
            imp.knn[feature.num, exp.num] <- wghts %*% data[k.features, exp.num]
        }
    }

    if (!rm.na) {
        imp.knn[is.na(data) == TRUE & is.nan(data) == FALSE] <- NA
    }
    if (!rm.inf) {
        index <- is.finite(data) == FALSE & is.na(data) == FALSE &
            is.nan(data) == FALSE
        imp.knn[index] <- data[index]
    }
    if (!rm.nan) {
        imp.knn[is.nan(data) == TRUE] <- NaN
    }
    return(imp.knn)
}

####################################################################
#### This Function imputes the data based on KNN-CORRELATION or
#### KNN-TRUNCATION. The Parameter Estimates based on the Truncated
#### Normal from EstimateComputation function is run on this function
####################################################################

imputeKNN <- function (data, k , distance = "correlation",
                       rm.na = TRUE, rm.nan = TRUE, rm.inf = TRUE, perc=1,...) {

    if (!(is.matrix(data))) {
        stop(message = paste(deparse(substitute(data)),
                             " is not a matrix.", sep = ""))
    }

    distance <- match.arg(distance, c("correlation","truncation"))

    nr <- dim(data)[1]
    if (k < 1 | k > nr) {
        stop(message = "k should be between 1 and the number of rows")
    }

    if (distance=="correlation"){
        genemeans<-rowMeans(data,na.rm=TRUE)
        genesd<-apply(data, 1, function(x) sd(x, na.rm = TRUE))
        data<-(data-genemeans)/genesd
    }

    if (distance=="truncation"){

	ParamMat <- EstimatesComputation(data, perc = perc)

    	genemeans<-ParamMat[,1]
        genesd<-ParamMat[,2]
        data<-(data-genemeans)/genesd
    }

    imp.knn <- data
    imp.knn[is.finite(data) == FALSE] <- NA
    t.data<-t(data)

    mv.ind <- which(is.na(imp.knn), arr.ind = TRUE)
    arrays <- unique(mv.ind[, 2])
    array.ind <- match(arrays, mv.ind[, 2])
    ngenes <- 1:nr

    for (i in 1:length(arrays)) {
        set <- array.ind[i]:min((array.ind[(i + 1)] - 1), dim(mv.ind)[1],
                                na.rm = TRUE)
        cand.genes <- ngenes[-unique(mv.ind[set, 1])]
        cand.vectors <- t.data[,cand.genes]
        exp.num<- arrays[i]
        for (j in set) {

            gene.num <- mv.ind[j, 1]
            tar.vector <- data[gene.num,]

            r <- (cor(cand.vectors,tar.vector, use = "pairwise.complete.obs"))
            dist <- switch(distance,
                           correlation = (1 - abs(r)),
                           truncation = (1 - abs(r)))
            dist[is.nan(dist) | is.na(dist)] <- Inf
            dist[dist==0]<-ifelse(is.finite(min(dist[dist>0])), min(dist[dist>0])/2, 1)
            dist[abs(r) == 1] <- Inf

            if (sum(is.finite(dist)) < k) {
                stop(message = "Fewer than K finite distances found")
            }
            k.genes.ind <- order(dist)[1:k]
            k.genes <- cand.genes[k.genes.ind]

            wghts <- (1/dist[k.genes.ind]/sum(1/dist[k.genes.ind])) * sign(r[k.genes.ind])
            imp.knn[gene.num, exp.num] <- wghts %*% data[k.genes, exp.num]
        }
    }

    if (distance=="correlation") {
        imp.knn <- (imp.knn * genesd) + genemeans
    }

    if(distance=="truncation") {
        imp.knn <- (imp.knn * genesd) + genemeans
    }

    if (!rm.na) {
        imp.knn[is.na(data) == TRUE & is.nan(data) == FALSE] <- NA
    }
    if (!rm.inf) {
        index <- is.finite(data) == FALSE & is.na(data) == FALSE &
            is.nan(data) == FALSE
        imp.knn[index] <- data[index]
    }
    if (!rm.nan) {
        imp.knn[is.nan(data) == TRUE] <- NaN
    }
    return(imp.knn)
}

##################################################################################
#### Root Mean Squared Error Function
##################################################################################
Rmse <- function(imp, mis, true, norm = FALSE) {
    imp <- as.matrix(imp)
    mis <- as.matrix(mis)
    true <- as.matrix(true)
    missIndex <- which(is.na(mis))
    errvec <- imp[missIndex] - true[missIndex]
    rmse <- sqrt(mean(errvec^2))
    if (norm) {
        rmse <- rmse/sd(true[missIndex])
    }
    return(rmse)
}
##################################################################################
#### Compute the Errors for the Different Imputation Methods
##################################################################################
ErrorsComputation <- function(trunc, corr, euc, miss, complete) {

    ImputeErrors <- NULL
    missing <- t(miss)
    original <- t(complete)
    KnnTrunc <- Rmse(trunc, missing, original)
    KnnCorr <- Rmse(corr, missing, original)
    KnnEuc <- Rmse(euc, missing, original)
    ImputeErrors <- c(KnnTrunc, KnnCorr, KnnEuc)
    return(ImputeErrors)

}



