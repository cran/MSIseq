\name{MSIseq.train}
\alias{MSIseq.train}
\title{
Build Microsatellite Instability Classification Model with Training Dataset
}
\description{
This function generate a detector for MSI status based on the mutation information in the \code{mutationNum} parameter.
}
\usage{
MSIseq.train(mutationNum, classification, cancerType = NULL)
}
\arguments{
  \item{mutationNum}{
    A data frame output from \code{\link[MSIseq:Compute.input.variables]{Compute.input.variables}}, which containing 9 variables: T.sns, S.sns, T.ind, S.ind, T, S, Ratio.sns, Ratio.ind, Ratio. 
}
  \item{classification}{
    A data frame with two columns: Tumor_Sample_Barcode (tumor ID) and the 
    corresponding MSI_status. Check \code{\link[MSIseq:NGStrainclass]{NGStrainclass}} for detail.
}
  \item{cancerType}{
    A data frame with two columns: Tumor_Sample_Barcode (tumor ID) and the 
    corresponding cancer_type. Check \code{\link[MSIseq:NGStraintype]{NGStraintype}} for detail.
}
}
\details{
This function builds and evaluates a decision tree model from \code{mutationNum}. 
}
\value{
A \code{Weka_classifier} object: a decision tree model built with the 'RWeka' function \code{J48()}
}
\references{
Kurt Hornik, Christian Buchta, Achim Zeileis (2009) Open-Source Machine Learning: R Meets Weka. Computational Statistics, 24(2), 225-232.
}
\author{
Mini Huang
}

\seealso{ 
\code{\link[MSIseq:MSIseq.classify]{MSIseq.classify}}, 
\code{\link[MSIseq:Compute.input.variables]{Compute.input.variables}}
}
\examples{
## load sample data (train.mutationNum, NGStraintype, 
## NGStrainclass)

data(train.mutationNum)
data(NGStrainclass)
data(NGStraintype)

## create NGSclassifier with traindata
## note that this is a built-in classifier, which can be directly used 
## if you do not have your own training data to create a classifier

NGSclassifier<-MSIseq.train(mutationNum = train.mutationNum, 
  classification=NGStrainclass, cancerType=NGStraintype)
}
