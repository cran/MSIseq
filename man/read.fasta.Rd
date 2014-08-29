\name{read.fasta}
\alias{read.fasta}
\title{
Read Fasta File
}
\description{
This function reads in a file with multiple fasta records and return a list.
}
\usage{
read.fasta(file.name)
}
\arguments{
  \item{file.name}{
    The path to a fasta file with multiple fasta records.
}
}
\details{
This function reads in a file with multiple fasta records and return a list of each chromosome and their sequence context.
}
\value{
A list of each chromosome and their sequence context.
}
\author{
Mini Huang
}
\seealso{
\code{\link[MSIseq:find.mono.repeats]{find.mono.repeats}}
}