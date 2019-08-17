#' Population Total with replicate weights
#'
#' @param x Name indicating the variable.
#' @param num \code{TRUE} or \code{FALSE} indicating whether x is numeric or categorical.
#' @param design \code{svydb.repdesign} object.
#' @param return.replicates \code{TRUE} to return the totals computed with replicate weights.
#' @description
#' Computes population total from survey data sets with replicate weights.
#' @examples
#' data(ss16hde)
#' hde.dbrepsurv = svydbrepdesign(wt = WGTP, repwt="wgtp[0-9]+", scale = 4/80, data = ss16hde)
#' svydbreptotal(x = BATH, design = hde.dbrepsurv , num = T)
#' svydbreptotal(x = FS, design = hde.dbrepsurv , num = F)
#' svydbreptotal(x = HHT, design = hde.dbrepsurv , num = T, return.replicates = T)$replicates
#' coef(svydbreptotal(x = BATH, design = hde.dbrepsurv , num = T))
#' SE(svydbreptotal(x = BATH, design = hde.dbrepsurv , num = T))
#' # OR with a database connection
#' # library(MonetDBLite)
#' # library(DBI)
#' # library(dbplyr)
#' # con = dbConnect(MonetDBLite())
#' # dbWriteTable(con, "ss16hde", ss16hde)
#' # ss16hde.db = tbl(con, "ss16hde")
#' # hde.dbrepsurv = svydbrepdesign(wt = WGTP, repwt="wgtp[0-9]+", scale = 4/80, data = ss16hde.db)
#' @author Charco Hui
#' @seealso
#' \code{\link{svydbrepdesign}}, \code{\link{svydbreptotal}}
#' @export

svydbreptotal = function(x, design, num, return.replicates = F){
  x = enquo(x)

  if(!("svydb.repdesign" %in% class(design))){
    stop("Please provide a svydb.repdesign")
  }

  if(missing(num)){
    stop("Is x a numeric or categorical variable?, num = T OR num = F?")
  }

  dsn = design$clone()
  dsn$setx(!!enquo(x))

  d = dsn$data

  dsn$storename("x", colnames(d))

  if(num == F){
    d = dummy_mut(d, !!sym(dsn$names$x), withBase = T)
  }

  dsn$storename("x", colnames(d))

  fullTotTbl = d %>% summarise_at(vars(dsn$names$x),
                                  funs(sum(. * (!!sym(dsn$wt))))) %>%
    collect()

  cnt = 1
  getRepTots = function(names, fullTot){
    replicates = d %>% summarise_at(vars(dsn$repwt), funs(sum((. * !!sym(names))))) %>%
      collect()
    repTot = replicates %>%
      summarise_all(funs((. - !!quo(fullTot[cnt]))^2))
    cnt <<- cnt + 1
    if(return.replicates == T){
      list(replicates = replicates,
           repVar = db_rowSums(repTot) %>%
             transmute_all(funs(. * !!quo(dsn$scale))) %>% collect())
    }else{
      list(repVar = db_rowSums(repTot) %>%
             transmute_all(funs(. * !!quo(dsn$scale))) %>% collect())
    }
  }

  ans = lapply(colnames(fullTotTbl), getRepTots, fullTot = as.vector(t(fullTotTbl)))
  repVar = lapply(ans, function(x) x$repVar) %>% Reduce(rbind, .) %>% pull()

  tot = fullTotTbl %>% t() %>% as.vector()
  attr(tot, "var") = repVar
  attr(tot, "statistic") <- "Total"
  attr(tot, "name") = dsn$names$x

  if(return.replicates == T){
    replicates = lapply(ans, function(x) x$replicates %>% collect()) %>%
      Reduce(rbind, .)
    tot = list(svydbrepstat = tot, replicates = replicates)
  }
  class(tot) = c("svydbrepstat")
  return(tot)
}
