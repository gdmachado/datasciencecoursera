pollutantmean <- function(directory, pollutant, id = 1:332) {
    filenames <- paste(directory, sprintf("%03d.csv", id), sep="/")
    df <- do.call(rbind, lapply(filenames, read.csv))
    mean(df[[pollutant]], na.rm=TRUE)
}