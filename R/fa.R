
#' @title S3 Methods for \link[psych]{fa}
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
#' # names(which(!mapply(ThomasJeffersonUniv::relaxed_identical, m1, m1a)))
#' 
#' m1 |> fa.plot() # ?psych::plot.psych; ?psych::fa.plot
#' 
#' m2 = swiss |>
#'  cov() |> 
#'  fa(nfactors = 2L, fm = 'pa', rotate = 'varimax')
#' m2 |> fa.plot() 
#' m2 |> plot_fa_()
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
#' @export
rmd_.fa <- function(x, xnm, ...) {
  
  #h_ <- attr(x, which = 'fig.height', exact = TRUE) %||% 4
  #w_ <- attr(x, which = 'fig.width', exact = TRUE) %||% 7
  
  #w <- ncol * w_
  #h <- ceiling(length(x) / ncol) * h_
  
  return(c(
    sprintf(fmt = 'Factor analysis on `%s` is performed by <u>**`R`**</u> package <u>**`psych`**</u>.', (as.list(x$Call)$r) |> deparse1()),
    #sprintf(fmt = '```{r fig.height = %.1f, fig.width = %.1f}', h, w), 
    '```{r}', # try automatic height and weight?
    sprintf(fmt = 'plot_fa_(%s)', xnm), # not sure how to put in `...`
    '```',
    '<any-text>'
  ))
  
}



