% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attribution.R
\name{get_version_citation}
\alias{get_version_citation}
\title{Get Version Citation}
\usage{
get_version_citation(
  zenodo_id = the$working_version,
  style = c("havard-cite-them-right", "apa", "modern-language-association", "vancouver",
    "chicago-fullnote-bibliography", "ieee"),
  verbose = TRUE
)
}
\arguments{
\item{zenodo_id}{String. ID for a Zenodo deposit. Should correspond to the version of a deposit.}

\item{style}{Character. One of "havard-cite-them-right",
"apa",
"modern-language-association",
"vancouver",
"chicago-fullnote-bibliography", or
"ieee"}

\item{verbose}{Logical. Print the citation?}
}
\value{
Character. Text for a citation
}
\description{
Get Version Citation
}
\examples{

\dontrun{
get_version_citation(zenodo_id = "15692263",
                    style = "apa")
}

}
