\name{find.mono.repeats}
\alias{find.mono.repeats}
\title{
Find Mono-nucleotide Repeats
}
\description{
This function reads in a file with multiple fasta records and find the mononucleotide repeats in the text.
}
\usage{
find.mono.repeats(text, min.len = 5)
}
\arguments{
  \item{text}{
    The return object from function \code{\link[MSIseq:read.fasta]{read.fasta}}.
}
  \item{min.len}{
    The minimum length of the mononucleotide repeats searched by the function.
}
}
\details{
This function find the mononucleotide repeats in the text and return a data frame with the following columns.
\itemize{
  \item {chr: a character vector that indicates the chromosome identifier
      with the "chr" prefix: "chr1", "chr2", ..., "chr22", "chrX", "chrY"}
  \item {start: a numeric vector that indicates the start position in the
  	chromosome}
  \item {stop: a numeric vector that indicates the end position in the chromosome}
}
}
\value{
A data frame with 3 columns that are the 3 variables listed above.
}
\author{
Mini Huang
}

\seealso{
\code{\link[MSIseq:read.fasta]{read.fasta}}
}