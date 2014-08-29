\name{NGStestdata}
\alias{NGStestdata}
\docType{data}
\title{
Mutation Annotation Data with Test Set Samples
}
\description{
This is a data frame based on the TCGA "mutation annotation file" format.
}
\usage{data(NGStestdata)}
\format{
  A data frame with 72,111 observations on the following 5 variables: 
  \describe{
    \item{\code{Chrom}}{a character vector that indicates the chromosome identifier
      without the "chr" prefix: "1", "2", ..., "22", "X", "Y"}
    \item{\code{Start_Position}}{a numeric vector}
    \item{\code{End_Position}}{a numeric vector}
    \item{\code{Variant_Type}}{a character vector that indicates the
      type of variant; legal values are "SNP", "INS" and "DEL". Other
      values will cause the \code{\link[MSIseq:Compute.input.variables]{Compute.input.variables}} to generate an error}
    \item{\code{Tumor_Sample_Barcode}}{the sample ID as a character vector}
  }
  Any other columns are ignored.
}
\details{
This is sample input data for the \code{\link[MSIseq:Compute.input.variables]{Compute.input.variables}} function.
}
\source{
\url{https://tcga-data.nci.nih.gov/tcga/}
}
\references{
Mutation Annotation File Format \url{https://wiki.nci.nih.gov/display/TCGA/File+Format+Specifications}
}
\keyword{datasets}
