% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evalTransc.R
\name{evalTransc}
\alias{evalTransc}
\title{Check whether a Transcription Matches the Correct Value}
\usage{
evalTransc(filename = NULL, text, correct, alphaOnly = TRUE, matchCase = FALSE)
}
\arguments{
\item{filename}{An RDS file output from \code{transcribeServer()} OR an RDS file containing a single character string representing the entered transcription.}

\item{text}{A character string to test against the correct transcription. Not necessary if \code{filename} is supplied.}

\item{correct}{A character string of the correct transcription to which \code{text} or \code{filename} will be compared.}

\item{alphaOnly}{Boolean. Should the transcription be compared only in alphabetic characters entered, ignoring any spaces, punctuation, or digits? Defaults to TRUE.}

\item{matchCase}{Boolean. Should the comparison be case-sensitive? Defaults to FALSE.}
}
\value{
A Boolean value indicating whether the transcription in \code{text} or \code{filename} matches the value given in \code{correct} (=1) or not (=0).
}
\description{
Check whether a Transcription Matches the Correct Value
}
\details{
This function is likely not very useful on its own, except for post-processing of transcriptions entered with \code{transcribeUI()} and \code{transcribeServer()}. This function serves as a base for its interactive version \code{\link{evalTranscServer}}, for giving feedback to a participant based on a transcription they've entered.
}
\examples{
# Using the filename argument
transc <- "this is a sample transcription"
saveRDS(transc, "foo.rds")

is.correct <- evalTransc(filename = "foo.rds",
                         correct = "This is a sample transcription.")
print(is.correct)

# Using the text argument instead
# Play with the values matchCase and alphaOnly to observe changes in the returned value.
is.correct <- evalTransc(text = "this is a sample transcription",
                         correct = "This is a sample transcription.")
print(is.correct)

}
