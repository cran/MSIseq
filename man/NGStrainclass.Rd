\name{NGStrainclass}
\alias{NGStrainclass}
\docType{data}
\title{
MSI Status Classification for the TCGA Training Set Samples
}
\description{
This is a data frame containing the MSI status for each sample in \code{\link[MSIseq:train.mutationNum]{train.mutationNum}}.
}
\usage{data(NGStrainclass)}
\format{
  A data frame with 426 observations on the following 2 variables:
  \describe{
    \item{\code{Tumor_Sample_Barcode}}{the sample ID as a character vector}
    \item{\code{MSI_status}}{a factor indicating MSI status for each sample, containing two levels: "MSI-H" and "Non-MSI-H". Other values will cause \code{\link[MSIseq:MSIseq.train]{MSIseq.train}} to generate an error.}
  }
}
\details{
This is the sample data for \code{\link[MSIseq:MSIseq.train]{MSIseq.train}} function input.
}
\source{
\url{https://tcga-data.nci.nih.gov/tcga/}
}
\keyword{datasets}
