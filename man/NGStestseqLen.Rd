\name{NGStestseqLen}
\alias{NGStestseqLen}
\docType{data}
\title{
Capture Sequence Length for Test Set Samples
}
\description{
This is a data frame containing the capture sequence lengths for each sample in \code{\link[MSIseq:NGStestdata]{NGStestdata}}.
}
\usage{data(NGStestseqLen)}
\format{
  A data frame with 186 observations on the following 2 variables:
  \describe{
    \item{\code{Tumor_Sample_Barcode}}{the sample ID as a character vector}
    \item{\code{Sequence_Length}}{a numeric vector indicating capture sequence length for each sample; the length unit is Mb.}
  }
}
\details{
This is the sample data for the \code{\link[MSIseq:Compute.input.variables]{Compute.input.variables}} function input.
}
\source{
\url{https://tcga-data.nci.nih.gov/tcga/}
}

\keyword{datasets}
