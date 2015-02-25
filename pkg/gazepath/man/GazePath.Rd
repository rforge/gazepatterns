\name{GazePath}
\alias{GazePath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GazePath
}
\description{
Classify fixations and saccades}
\usage{
GazePath(data, x1, y1, x2 = NULL, y2 = NULL, distance, trial, height_px, height_mm, width_px, width_mm, outputmethod = "sample", res_x = 1280, res_y = 1024, samplerate = 500, method = "Mould", thres_vel = 35, thres_dur = 100, min_dist = 250)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{x1}{
%%     ~~Describe \code{x1} here~~
}
  \item{y1}{
%%     ~~Describe \code{y1} here~~
}
  \item{x2}{
%%     ~~Describe \code{x2} here~~
}
  \item{y2}{
%%     ~~Describe \code{y2} here~~
}
  \item{distance}{
%%     ~~Describe \code{distance} here~~
}
  \item{trial}{
%%     ~~Describe \code{trial} here~~
}
  \item{height_px}{
%%     ~~Describe \code{height_px} here~~
}
  \item{height_mm}{
%%     ~~Describe \code{height_mm} here~~
}
  \item{width_px}{
%%     ~~Describe \code{width_px} here~~
}
  \item{width_mm}{
%%     ~~Describe \code{width_mm} here~~
}
  \item{outputmethod}{
%%     ~~Describe \code{outputmethod} here~~
}
  \item{res_x}{
%%     ~~Describe \code{res_x} here~~
}
  \item{res_y}{
%%     ~~Describe \code{res_y} here~~
}
  \item{samplerate}{
%%     ~~Describe \code{samplerate} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{thres_vel}{
%%     ~~Describe \code{thres_vel} here~~
}
  \item{thres_dur}{
%%     ~~Describe \code{thres_dur} here~~
}
  \item{min_dist}{
%%     ~~Describe \code{min_dist} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, x1, y1, x2 = NULL, y2 = NULL, distance, trial, 
    height_px, height_mm, width_px, width_mm, outputmethod = "sample", 
    res_x = 1280, res_y = 1024, samplerate = 500, method = "Mould", 
    thres_vel = 35, thres_dur = 100, min_dist = 250) 
{
    if (!is.data.frame(data)) {
        stop("please insert a data frame and define the column numbers of the variables")
    }
    data[, distance] <- ifelse(data[, distance] < min_dist, NA, 
        data[, distance])
    D <- by(data[, distance], data[, trial], data.frame)
    if (!is.null(x2) & !is.null(y2)) {
        X <- by((data[, x1] + data[, x2])/2, data[, trial], data.frame)
        Y <- by((data[, y1] + data[, y2])/2, data[, trial], data.frame)
    }
    else {
        X <- by(data[, x1], data[, trial], data.frame)
        Y <- by(data[, y1], data[, trial], data.frame)
    }
    final <- "Please insert a correct method"
    if (method == "Eyelink") {
        final <- list()
        for (i in 1:length(unique(data[, trial]))) {
            X[[i]] <- Boundary(X[[i]], (res_x - width_px[i])/2, 
                res_x - (res_x - width_px[i])/2)
            Y[[i]] <- Boundary(Y[[i]], (res_y - height_px[i])/2, 
                res_y - (res_y - height_px[i])/2)
            final[[i]] <- Eyelink(X[[i]], Y[[i]], D[[i]], height_mm[i], 
                width_mm[i], height_px[i], width_px[i], Hz = samplerate)
        }
    }
    if (method == "Tobii") {
        final <- list()
        for (i in 1:length(unique(data[, trial]))) {
            X[[i]] <- Boundary(X[[i]], (res_x - width_px[i])/2, 
                res_x - (res_x - width_px[i])/2)
            Y[[i]] <- Boundary(Y[[i]], (res_y - height_px[i])/2, 
                res_y - (res_y - height_px[i])/2)
            final[[i]] <- Tobii(cbind(X[[i]], Y[[i]]), D[[i]], 
                thres_dur = 100, Hz = samplerate)
        }
    }
    if (method == "Fixed") {
        s <- list()
        for (i in 1:length(unique(data[, trial]))) {
            X[[i]] <- Boundary(X[[i]], (res_x - width_px[i])/2, 
                res_x - (res_x - width_px[i])/2)
            Y[[i]] <- Boundary(Y[[i]], (res_y - height_px[i])/2, 
                res_y - (res_y - height_px[i])/2)
            s[[i]] <- Speed_Deg(X[[i]], Y[[i]], D[[i]], height_mm[i], 
                width_mm[i], height_px[i], width_px[i], samplerate)
            s[[i]] <- ifelse(s[[i]] > 1000, NA, s[[i]])
        }
        fix <- possiblefix(unlist(s), thres_vel)
        final <- list()
        for (i in 1:length(unique(data[, trial]))) {
            final[[i]] <- fixationANDsaccade(s[[i]], thres_vel, 
                thres_dur, Hz = samplerate)
        }
    }
    if (method == "Mould") {
        fix <- thres_vel <- numeric()
        s <- list()
        for (i in 1:length(unique(data[, trial]))) {
            X[[i]] <- Boundary(X[[i]], (res_x - width_px[i])/2, 
                res_x - (res_x - width_px[i])/2)
            Y[[i]] <- Boundary(Y[[i]], (res_y - height_px[i])/2, 
                res_y - (res_y - height_px[i])/2)
            if (length(which(!is.na(X[[i]]))) > samplerate & 
                length(which(!is.na(Y[[i]]))) > samplerate & 
                length(which(!is.na(D[[i]]))) > samplerate) {
                s[[i]] <- Speed_Deg(X[[i]], Y[[i]], D[[i]], height_mm[i], 
                  width_mm[i], height_px[i], width_px[i], samplerate)
                s[[i]] <- ifelse(s[[i]] > 1000, NA, s[[i]])
                thres_vel[i] <- Mould_vel(s[[i]], Hz = samplerate)
                fix <- c(fix, possiblefix(s[[i]], thres_vel[i]))
            }
            else {
                s[[i]] <- NA
                thres_vel[i] <- NA
            }
        }
        fix <- fix[!is.na(fix)]
        if (length(fix) > 100) {
            thres_dur <- Mould_dur(fix, Hz = samplerate)
        }
        else {
            warning("There were not enough data points to derive a duration threshold, the threshold specified is used (default = 100ms)")
        }
        final <- list()
        for (i in 1:length(unique(data[, trial]))) {
            if (!is.na(thres_vel[i])) {
                final[[i]] <- fixationANDsaccade(s[[i]], thres_vel[i], 
                  thres_dur, Hz = samplerate)
            }
            else {
                final[[i]] <- NA
            }
        }
    }
    if (method == "MouldDur") {
        fix <- thres_vel <- numeric()
        s <- list()
        for (i in 1:length(unique(data[, trial]))) {
            X[[i]] <- Boundary(X[[i]], (res_x - width_px[i])/2, 
                res_x - (res_x - width_px[i])/2)
            Y[[i]] <- Boundary(Y[[i]], (res_y - height_px[i])/2, 
                res_y - (res_y - height_px[i])/2)
            if (length(which(!is.na(X[[i]]))) > samplerate & 
                length(which(!is.na(Y[[i]]))) > samplerate & 
                length(which(!is.na(D[[i]]))) > samplerate) {
                s[[i]] <- Speed_Deg(X[[i]], Y[[i]], D[[i]], height_mm[i], 
                  width_mm[i], height_px[i], width_px[i], samplerate)
                s[[i]] <- ifelse(s[[i]] > 1000, NA, s[[i]])
                thres_vel[i] <- Mould_vel(s[[i]], Hz = samplerate)
                fix <- c(fix, possiblefix(s[[i]], thres_vel[i]))
            }
            else {
                s[[i]] <- NA
                thres_vel[i] <- NA
            }
        }
        thres_dur <- thres_dur
        final <- list()
        for (i in 1:length(unique(data[, trial]))) {
            if (!is.na(thres_vel[i])) {
                final[[i]] <- fixationANDsaccade(s[[i]], thres_vel[i], 
                  thres_dur, Hz = samplerate)
            }
            else {
                final[[i]] <- NA
            }
        }
    }
    if (method == "Mould.all") {
        s <- list()
        for (i in 1:length(unique(data[, trial]))) {
            X[[i]] <- Boundary(X[[i]], (res_x - width_px[i])/2, 
                res_x - (res_x - width_px[i])/2)
            Y[[i]] <- Boundary(Y[[i]], (res_y - height_px[i])/2, 
                res_y - (res_y - height_px[i])/2)
            s[[i]] <- Speed_Deg(X[[i]], Y[[i]], D[[i]], height_mm[i], 
                width_mm[i], height_px[i], width_px[i], samplerate)
            s[[i]] <- ifelse(s[[i]] > 1000, NA, s[[i]])
        }
        thres_vel <- Mould_vel(unlist(s), plot = F, Hz = samplerate)
        fix <- possiblefix(unlist(s), thres_vel)
        fix <- fix[!is.na(fix)]
        thres_dur <- Mould_dur(fix, plot = F, Hz = samplerate)
        final <- list()
        for (i in 1:length(unique(data[, trial]))) {
            final[[i]] <- fixationANDsaccade(s[[i]], thres_vel, 
                thres_dur, Hz = samplerate)
        }
    }
    if (method == "Mould.fix") {
        s <- list()
        for (i in 1:length(unique(data[, trial]))) {
            X[[i]] <- Boundary(X[[i]], (res_x - width_px[i])/2, 
                res_x - (res_x - width_px[i])/2)
            Y[[i]] <- Boundary(Y[[i]], (res_y - height_px[i])/2, 
                res_y - (res_y - height_px[i])/2)
            s[[i]] <- Speed_Deg(X[[i]], Y[[i]], D[[i]], height_mm[i], 
                width_mm[i], height_px[i], width_px[i], samplerate)
            s[[i]] <- ifelse(s[[i]] > 1000, NA, s[[i]])
        }
        thres_vel <- Mould_vel(unlist(s), plot = F, Hz = samplerate)
        final <- list()
        for (i in 1:length(unique(data[, trial]))) {
            final[[i]] <- fixationANDsaccade(s[[i]], thres_vel, 
                thres_dur, Hz = samplerate)
        }
    }
    if (outputmethod == "simple") {
        for (i in 1:length(unique(data[, trial]))) {
            final[[i]] <- simplify(final[[i]], X[[i]], Y[[i]], 
                Hz)
        }
    }
    return(list(final, X, Y, thres_vel, thres_dur, fix))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
