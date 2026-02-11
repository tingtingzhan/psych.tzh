
#' @title bibs of package psych.tzh
#' 
#' @param key,... \link[utils]{bibentry}
#' 
#' @keywords internal
#' @importFrom utils bibentry person
#' @name psych_bib
#' @export
.gorsuch83 <- \(key = 'Gorsuch83', ...) {
  bibentry(
    bibtype = 'book', key = key, ...,
    title = 'Factor Analysis',
    author = person(given = c('Richard', 'L.'), family = 'Gorsuch'),
    year = '1983',
    edition = '2nd',
    publisher = 'Psychology Press',
    doi = '10.4324/9780203781098'
  )
}