% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_GIFT.R
\name{compute_GIFT}
\alias{compute_GIFT}
\title{Calculates the gene presence-based MCI of a metabolic pathway}
\usage{
compute_GIFT(definition, present)
}
\arguments{
\item{definition}{Definition string of a given metabolic pathway}

\item{present}{Vector of functional units present in the genome}
}
\value{
A fullness value
}
\description{
Calculates the gene presence-based MCI of a metabolic pathway
}
\examples{
compute_GIFT(definition,present)
compute_GIFT("K01580 (K13524,K07250,K00823,K16871) (K00135,(K00139,K17761))",c("K01580","K00823","K16871"))
}