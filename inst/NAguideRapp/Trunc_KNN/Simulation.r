#### SimulationFunctions
CorrMatrixNegFixed <- function(blocks, a, corr, off) {
    Corr <- NULL
    for (i in 1:blocks) {
        Corr[[i]] <- matrix(NA, ncol = a, nrow = a)
        diag= rep(1,a)
        offdiag = corr
        Corr[[i]][lower.tri(Corr[[i]])] <- offdiag
        Corr[[i]][upper.tri(Corr[[i]])] <- t(Corr[[i]])[upper.tri(t(Corr[[i]]))]
        diag(Corr[[i]]) <- diag
        for ( k in ((ncol(Corr[[i]])/2)+1):(ncol(Corr[[i]])) ) {
            for( j in 1:(nrow(Corr[[i]])/2) ) {
                Corr[[i]][j,k] <- -1*Corr[[i]][j,k]
            }
        }

        for ( k in ((nrow(Corr[[i]])/2)+1):(nrow(Corr[[i]])) ) {
            for( j in 1:(ncol(Corr[[i]])/2) ) {
                Corr[[i]][k,j] <- -1*Corr[[i]][k,j]
            }
        }

    }
    res <- as.matrix(bdiag(Corr))
    res[which(res == 0, arr.ind = TRUE)] <- off
    return(res)
}


##################################################################################
## This function Simulates complete data sets.
## We are creating a dataset of size N (samples) by p (Metabolites)
##################################################################################

SimulatedData <- function(n, p, covar, low, high)  {
	## No of Samples == N
	## No of Metabolites = p
        Means <- runif(p, min = low, max = high)
        data <- rmvnorm(n, mean = Means, sigma = covar)
	return(data)
}


##################################################################################
###### Create MissingValues Datasets
##################################################################################

MissingData <- function(data, totalmiss, mar, perc) {
	missdata <- data

      ## First create Missing Not At Random (MNAR)
        belowthresh <- which(data < quantile(data, probs = c(totalmiss-mar)/100), arr.ind = TRUE)
        missdata[belowthresh] <- NA


	## Next create Missing At Random (MAR)
        dataVector <- c(1:length(data))
        randommiss <- sample(dataVector[-which(is.na(missdata))], (mar/100)*length(data) )
        missdata[randommiss] <- NA

	## Next exclude metabolites with more that 'perc' missing
        NumberMissing <- apply(missdata, 2, function(x) length(which(is.na(x))))

        ind  <- which(NumberMissing >= (nrow(data)*perc))
        if(length(ind) > 0 ){
            missdata <- missdata[,-ind]
            data <- data[,-ind]
        }

	return(list(missdata, data, NumberMissing))
}

