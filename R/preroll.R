#' Build square matrix of legislator votes and names 
#'   for import to pscl
#'
#' Takes a ragged array or a json object representing a ragged array
#'
#' @param data List of legislator names and votes
#' @param json Character vector showing file path 
#' @return Matrix of vote results
#'   
#' @export
unflatten <- function(data, json = NULL) {
  require(RJSONIO)
  if (!is.null(data)) {
    legis.list <- data
  } else {
    legis.list <- fromJSON(json)
  }
  # build vectors of possible names
  # bit magical, but we strip list names, then
  # convert to a flat character vector and extract those
  legis.names <- unique(names(unlist(unname(legis.list))))
  # prefilling the vector outside the loop is a nice 
  # performance gain
  prefill <- vector("numeric", length = length(legis.names))
  sortStretch <- function(orig) {
    # Assign all to NA, so that...
    prefill[] <- NA
    # we replace ONLY those elements whose names occur in
    # this list element's vector with the content of
    # this element's vector
    prefill[match(names(orig), legis.names)] <- orig
    return(prefill)
  }
  square <- lapply(json, sortStretch)
  # pack into a matrix, now that they are the same length
  # then transpose because that's what rollcall() expects
  out.mat <- t(do.call(rbind, square))
  # name (we've already assured that the sorting order will be correct)
  rownames(out.mat) <- legis.names
  return(out.mat)
}
