dbenford <- function(x){
    log10(1 + 1/x)
}
 
pbenford <- function(q){
    cumprobs <- cumsum(dbenford(1:9))
    return(cumprobs[q])
}
 
qbenford <- function(p){
    cumprobs <- cumsum(dbenford(1:9))
    cumprobs[9] <- 1 # To fix a rounding error
    quantiles <- sapply(p, function(x) {10 - sum(cumprobs >= x)})
    return(quantiles)
}
 
rbenford <- function(n){
    sample(1:9, size = n, replace = TRUE, prob = dbenford(1:9))
}