\name{GazePath}
\alias{GazePath}
\title{
Classify fixations and saccades}
\description{
This R-package transforms raw eye-tracking data into fixations and saccades
}
\usage{
GazePath(data, x1, y1, x2 = NULL, y2 = NULL, distance, trial,
height_px, height_mm, width_px, width_mm, 
res_x = 1280, res_y = 1024, samplerate = 500, 
method = "Mould", posthoc = FALSE, thres_vel = 35, 
thres_dur = 100, min_dist = 250)
}

\arguments{
  \item{data}{
The dataframe with at least the raw x- and y-coordinates, the distance to the screen in mm and a trial index.
}
  \item{x1}{
the column of the dataframe containing the x-coordinates
}
  \item{y1}{
the column of the dataframe containing the y-coordinates
}
  \item{x2}{
When tracking was binocular, the column of the dataframe containing the x-coordinates of the second eye
}
  \item{y2}{
When tracking was binocular, the column of the dataframe containing the y-coordinates of the second eye
}
  \item{distance}{
The column of the dataframe containing the distance in mm
}
  \item{trial}{
The column of the dataframe containing the trial or stimuli index
}
  \item{height_px}{
The height of the stimuli in pixels, must a be vector of length number of trials
}
  \item{height_mm}{
The height of the stimuli in mm, must a be vector of length number of trials
}
  \item{width_px}{
The width of the stimuli in pixels, must a be vector of length number of trials
}
  \item{width_mm}{
The height of the stimuli in pixels, must a be vector of length number of trials
}
  \item{res_x}{
The horizontal resolution of the monitor in pixels
}
  \item{res_y}{
The vertical resolution of the monitor in pixels
}
  \item{samplerate}{
The samplerate, e.g., 60 for a 60 Hz scanner, 500 for a 500 Hz scanner
}
  \item{method}{
Must be one of the following methods: Mould, Mould.all, MouldDur, Mould.fix, Tobii, Eyelink.
Mould estimates velocity threshold per trial and duration thresholds per person
Mould.all estimates velocity and duration thresholds per person
MouldDur estimates velocity threshold per trial and uses the duration thresholds specified under 'thres_dur'
Mould.fix estimates velocity thresholds per person and uses the duration thresholds specified under 'thres_dur'
}
  \item{posthoc}{
TRUE or FALSE (default). When TRUE successive fixations close in space are merged.
}
  \item{thres_vel}{
The desired velocity threshold, will only be used for method 'Eyelink'
}
  \item{thres_dur}{
The desired duration threshold, will only be used for method 'Eyelink', 'MouldDur' and 'Mould.fix' 
}
  \item{min_dist}{
The minimal distance from the screen in mm, samples below this distance are considered unreliable and are omitted from further analysis.
}
}
\details{
no details
}
\value{
GazePath returns a list of 16 items, plot() and summary() can be used to verify the procedure and obtain the fixations
}
\references{
Mould et al. (2012)
}
\author{
Daan van Renswoude
}

\examples{
#### Example
data(screen)
data(eye_data)
example <- GazePath(eye_data, x1 = 10, y1 = 11, distance = 14, trial = 3, 
height_px = screen[,1], height_mm = screen[,2], width_px = screen[,3], width_mm = screen[,4], 
method = 'Mould', samplerate = 500)

## Use different i's to check the estimated fixations per trial
i <- 1
plot(example, i = i)

## The estimated fixations, orderded per trial
summary(example)
}
\keyword{ fixations }
\keyword{ saccades }
