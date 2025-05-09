
#' @title Extensions for \link[psych]{fa} object
#' 
#' @param x an \link[psych]{fa} object
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @examples
#' library(psych)
#' # ?psych::fa
#' 
#' m1 = Harman74.cor$cov |> 
#'  fa(nfactors = 2L, fm = 'pa', rotate = 'varimax')
#' m1a = Harman74.cor$cov |> 
#'  cov2cor() |>
#'  fa(nfactors = 2L, fm = 'pa', rotate = 'varimax')
#' # names(which(!mapply(adv.tzh::relaxed_identical, m1, m1a)))
#' 
#' m1 |> fa.plot() # ?psych::plot.psych; ?psych::fa.plot
#' 
#' m2 = swiss |>
#'  cov() |> 
#'  fa(nfactors = 2L, fm = 'pa', rotate = 'varimax')
#' m2 |> fa.plot() 
#' m2 |> plot_fa_()
#'  
#' library(rmd.tzh); list(
#'  'fa1' = m1,
#'  'fa2' = m2
#' ) |> render_(file = 'fa')
#' 
#' @keywords internal
#' @name fa_ext
#' @importFrom psych fa.plot
#' @export  
plot_fa_ <- function(x, ...) {
  
  # don't occupy S3 method dispatch `plot.fa`
  # even that now they do not have `psych::plot.fa`
  
  lab <- x[['r']] |> # correlation or covariance matrix; see ?psych::fa
    colnames()
  
  # the last line of ?psych::fa.plot
  # `text(load, labels, pos = pos, ...)`
  # does not add color to the labels
  # write to author, after tzh think this a little bit..
  
  if (!length(lab)) {
    fa.plot(x, ...)
    return(invisible())
  }
  
  if (!all(nzchar(lab))) stop('zchar in `lab`')
  fa.plot(x, labels = lab, ...)
  return(invisible())
  
}

#' @rdname fa_ext
#' @importFrom rmd.tzh md_
#' @export md_.fa
#' @export
md_.fa <- function(x, xnm, ...) {
  
  return(c(
    sprintf(fmt = '[Factor analysis](https://en.wikipedia.org/wiki/Factor_analysis) on `%s` is performed by <u>**`R`**</u> package <u>**`psych`**</u>.', (as.list(x$Call)$r) |> deparse1()),
    '\n',
    '```{r}', # try automatic height and weight?
    sprintf(fmt = 'psych.tzh::plot_fa_(%s)', xnm), # not sure how to put in `...`
    '```',
    '<any-text>'
  ))
  
}



