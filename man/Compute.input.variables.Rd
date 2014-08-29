\name{Compute.input.variables}
\alias{Compute.input.variables}
\title{
Compute Mutation Counts from Tumor Exome Somatic Mutations
}
\description{
This function returns a data frame of mutation counts from the mutation information in the \code{data} argument.
}
\usage{
Compute.input.variables(data, repeats, uniform.seq.len=38, seq.len = NULL)
}
\arguments{
  \item{data}{
    A data frame in the NGS "mutation annotation file" format. 
    See \code{\link[MSIseq:NGStestdata]{NGStestdata}} for details.
}
  \item{repeats}{
    A data frame indicating the genome coordinates of simple sequence repeats. 
}
  \item{uniform.seq.len}{
    The length of the capture sequence. This argument should be used when the capture sequences for all the samples have the same length. This argument will be ignored when \code{seq} 
    is supplied.
}
  \item{seq.len}{
    A data frame with two columns: Tumor_Sample_Barcode and the
    corresponding Sequence_Length (Mb). This should be provided 
    if the capture sequences for the tumors have different lengths.
}
}
\details{
This function computes 9 variables (listed below) from \code{data}. 
Mutations are one of two types: single nucleotide substitutions (SNSs) and short insertions/deletions (indels). 
\itemize{
  \item {T.sns: total count of SNSs/Mb}
  \item {S.sns: count of SNSs in simple sequence repeats/Mb}
  \item {T.ind: total count of indels/Mb}
  \item {S.ind: count of indels in simple sequence repeats/Mb}
  \item {T: total mutation count/Mb}
  \item {S: mutation count in simple sequence repeats/Mb}
  \item {ratio.sns: S.sns/T.sns}
  \item {ratio.ind: S.ind/T.ind}
  \item {ratio: S/T}
}
}
\value{
A data frame with 9 columns that are the 9 variables listed above.
}
\author{
Mini Huang
}

\seealso{
\code{\link[MSIseq:MSIseq.train]{MSIseq.train}},
\code{\link[MSIseq:MSIseq.classify]{MSIseq.classify}},
}
\examples{
\dontrun{
## load sample data: NGStestdata, NGStestseqLen

data(NGStestdata)
data(NGStestseqLen)

## download the Hg19repeats annotation file and load it
url <- 
"http://steverozen.net/data/Hg19repeats.rda"
file <- basename(url)
download.file(url, file)
load("Hg19repeats.rda")

## get mutation counts for test data 

test.mutationNum<-Compute.input.variables(NGStestdata, 
	repeats=Hg19repeats, seq.len = NGStestseqLen)
}
}