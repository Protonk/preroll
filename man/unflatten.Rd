\name{unflatten}
\alias{unflatten}
\title{Build square matrix of legislator votes and names
  for import to pscl}
\usage{
  unflatten(data, json = NULL)
}
\arguments{
  \item{data}{List of legislator names and votes}

  \item{json}{Character vector showing file path}
}
\value{
  Matrix of vote results
}
\description{
  Takes a ragged array or a json object representing a
  ragged array
}

