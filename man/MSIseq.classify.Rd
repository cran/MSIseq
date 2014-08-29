\name{MSIseq.classify}
\alias{MSIseq.classify}
\title{
Classify Microsatellite Instability with MSIseq Classifier
}
\description{
This function gives a classification result for MSI status based on \code{classifier} with the mutation information in the \code{mutationNum} argument.
}
\usage{
MSIseq.classify(mutationNum, classifier = NGSclassifier, cancerType = NULL)
}
\arguments{
  \item{mutationNum}{
    A data frame output from \code{\link[MSIseq:Compute.input.variables]{Compute.input.variables}}, which containing 9 variables: T.sns, S.sns, T.ind, S.ind, T, S, Ratio.sns, Ratio.ind, Ratio. 
}
  \item{classifier}{
    A RWeka J48 model returned from the function \code{\link[MSIseq:MSIseq.train]{MSIseq.train}}. Notice that if this argument is missing, the function will use a default build-in classifier, NGSclassifier.
}
  \item{cancerType}{
    A data frame with two columns: Tumor_Sample_Barcode (tumor ID) and the 
corresponding cancer_type. Check \code{\link[MSIseq:NGStraintype]{NGStraintype}} for detail. If the classifier used in this function is trained with cancerType argument, cancerType should be provided here.
}

}
\details{
This function gives a classification of MSI status for the samples with their \code{mutationNum} information. The classification is made with the decision tree model in \code{classifier}.
}
\value{
A data frame with two columns, Tumor_Sample_Barcode and the corresponding classified MSI_status. 
}
\references{
Kurt Hornik, Christian Buchta, Achim Zeileis (2009) Open-Source Machine Learning: R Meets Weka. Computational Statistics, 24(2), 225-232.
}
\author{
Mini Huang
}

\seealso{
\code{\link[MSIseq:MSIseq.train]{MSIseq.train}},
\code{\link[MSIseq:Compute.input.variables]{Compute.input.variables}}
}
\examples{
## load sample data: test.mutationNum

data(test.mutationNum)
data(NGStestseqLen)


## classify on test data with NGSclassifier (the default classifier)

result <- MSIseq.classify(mutationNum = test.mutationNum)
}