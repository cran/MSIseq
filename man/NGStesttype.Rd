\name{NGStesttype}
\alias{NGStesttype}
\docType{data}
\title{
Cancer Types for the TCGA Test Set Samples
}
\description{
This is a data frame containing the cancer types for each sample in \code{\link[MSIseq:test.mutationNum]{test.mutationNum}}.
}
\usage{data(NGStesttype)}
\format{
  A data frame with 426 observations on the following 2 variables:
  \describe{
    \item{\code{Tumor_Sample_Barcode}}{the sample ID as a character vector}
    \item{\code{cancer_type}}{a factor indicating cancer types for each sample}
  }
}
\details{
This is the sample data for \code{\link[MSIseq:MSIseq.classify]{MSIseq.classify}} function input.
}
\source{
\url{https://tcga-data.nci.nih.gov/tcga/}
}
\keyword{datasets}
