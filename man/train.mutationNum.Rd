\name{train.mutationNum}
\alias{train.mutationNum}
\docType{data}
\title{
Table of Mutation Numbers in Train Set Samples
}
\description{
This is a data frame containing the counts of 9 mutation types for each sample in \code{\link[MSIseq:NGStraindata]{NGStraindata}}. It was generated from \code{\link[MSIseq:Compute.input.variables]{Compute.input.variables}}
}
\usage{data(train.mutationNum)}
\format{
  A data frame with 361 observations on the following 9 variables:
  \describe{
    \item{\code{T.sns}}{total count of SNSs/Mb} 
    \item{\code{S.sns}}{count of SNSs in simple sequence repeats/Mb}
    \item{\code{T.ind}}{total count of indels/Mb}
    \item{\code{S.ind}}{count of indels in simple sequence repeats/Mb}
    \item{\code{T}}{total mutation count/Mb}
    \item{\code{S}}{mutation count in simple sequence repeats/Mb}
    \item{\code{ratio.sns}}{S.sns/T.sns}
    \item{\code{ratio.ind}}{S.ind/T.ind}
    \item{\code{ratio}}{S/T}
  }
}
\details{
This is the sample data for \code{\link[MSIseq:MSIseq.train]{MSIseq.train}} function input.
}
\source{
\url{https://tcga-data.nci.nih.gov/tcga/}
}
\keyword{datasets}
