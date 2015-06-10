\name{MSIseq-package}
\alias{MSIseq-package}
\alias{MSIseq}
\docType{package}
\title{
 Assess Tumor Microsatellite Instability with a Decision Tree Classifier from Tumor Exome Somatic Mutations
}
\description{
A decision tree classifier for detecting microsatellite instability (MSI) in somatic mutation data from whole exome sequencing. MSI is detected based on different mutation rates in all sites as well as in simple sequence repeats. This mechanism can also be applied to sequence data of targeted gene panels with shorter sequence length.
}
\details{
\tabular{ll}{
Package: \tab MSIseq\cr
Type: \tab Package\cr
Depends: \tab RWeka, rJava, R.utils, R (>= 3.2.0)\cr
Imports: \tab RWeka, rJava, R.utils\cr
Version: \tab 1.0.0\cr
Date: \tab 2014-06-10\cr
}
This package contains two main functions:

\code{MSIseq.train()} is used to generate a detector for MSI status from mutation information.
\code{MSIseq.classify()} uses a detector to classify the MSI status of new tumors from mutation catalogs.
}

\author{
Mini Huang

Maintainer: Mini Huang <mini.huang@nus.edu.sg>
}
\keyword{ package }
\seealso{
\code{\link[MSIseq:MSIseq.train]{MSIseq.train}}, 
\code{\link[MSIseq:MSIseq.classify]{MSIseq.classify}}
}

