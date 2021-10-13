# AUXILIARY FUNCTIONS

# ----------------------------------------------
# Rsq(), aux function
Rsq <- function(model) 
{
    pred <- predict(model)
    n <- length(pred)
    res <- resid(model)
    w <- weights(model)
    if (is.null(w)) 
        w <- rep(1, n)
    rss <- sum(w * res^2)
    resp <- pred + res
    center <- weighted.mean(resp, w)
    r.df <- summary(model)$df[2]
    int.df <- 1
    tss <- sum(w * (resp - center)^2)
    r.sq <- 1 - rss/tss
    adj.r.sq <- 1 - (1 - r.sq) * (n - int.df)/r.df
    out <- list(pseudo.R.squared = r.sq, adj.R.squared = adj.r.sq)
    return(out)
}


# --------------------------------------
# on loading seedwater
.onAttach <- function(lib, pkg)
{
   vers <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
   packageStartupMessage(paste("---\nseedwater version", vers))
}