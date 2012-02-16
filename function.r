BenfordObsExp <- function(x){
    data <- substitute(x)
    n <- length(x)
    # Peel off the first digit
    x <- as.numeric(substring(formatC(x, format = 'e'), 1, 1))
    obsFreq <- tabulate(x, nbins = 9)
    benFreq <- n * dbenford(1:9)
    plot(1:9, benFreq, xlim = c(1, 9), ylim = c(0, max(obsFreq, benFreq)), type = 'l',
        main = paste(data, ": observed (red) and expected (Benford's Law)"),
        xlab = "Digit", ylab = "Frequency")
    axis(1, at = 1:9)
    points(1:9, obsFreq, col = "red", pch = 16)
}