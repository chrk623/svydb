#' Population Mean with replicate weights
#'
#' @param x Name indicating the variable.
#' @param num \code{TRUE} or \code{FALSE} indicating whether x is numeric or categorical.
#' @param design \code{svydb.repdesign} object.
#' @param return.replicates \code{TRUE} to return the means computed with replicate weights.
#' @description
#' Computes population mean from survey data sets with replicate weights.
#' @examples
#' data(ss16hde)
#' hde.dbrepsurv = svydbrepdesign(wt = WGTP, repwt="wgtp[0-9]+", scale = 4/80, data = ss16hde)
#' svydbrepmean(x = BATH, design = hde.dbrepsurv , num = T)
#' svydbrepmean(x = FS, design = hde.dbrepsurv , num = F)
#' svydbrepmean(x = HHT, design = hde.dbrepsurv , num = T, return.replicates = T)$replicates
#' coef(svydbrepmean(x = BATH, design = hde.dbrepsurv , num = T))
#' SE(svydbrepmean(x = BATH, design = hde.dbrepsurv , num = T))
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

svydbrepmean = function(x, design, num, return.replicates = F){
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

  N = dsn$getwt()
  fullMeanTbl = d %>% summarise_at(vars(dsn$names$x),
                                   funs(sum(. * (!!sym(dsn$wt)))/!!quo(N))) %>%
    collect()

  repN = d %>% select(dsn$repwt) %>% summarise_all(sum) %>% collect()
  repN = paste(colnames(repN), "/", repN, collapse = " ; ")
  cnt = 1
  getRepTots = function(names, fullMean){
    replicates = d %>%
      summarise_at(vars(dsn$repwt), funs(sum(. * !!sym(names)))) %>%
      transmute(!!!parse_exprs(repN)) %>% compute()
    # repMean = replicates %>%
    #   summarise_all(funs((. - !!quo(fullMean[cnt]))^2))
    repMean = replicates %>%
      summarise_all(funs((. - local(fullMean[cnt]))^2))
    cnt <<- cnt + 1
    if(return.replicates == T){
      # list(replicates = replicates,
      #      repVar = db_rowSums(repMean) %>%
      #        transmute_all(funs(. * !!quo(dsn$scale))) %>% collect())
      list(replicates = replicates,
           repVar = db_rowSums(repMean) %>%
             transmute_all(funs(. * local(dsn$scale))) %>% collect())
    }else{
      # list(repVar = db_rowSums(repMean) %>%
      #        transmute_all(funs(. * !!quo(dsn$scale))) %>% collect())
      list(repVar = db_rowSums(repMean) %>%
             transmute_all(funs(. * local(dsn$scale))) %>% collect())
    }
  }
  ans = lapply(colnames(fullMeanTbl), getRepTots, fullMean = as.vector(t(fullMeanTbl)))
  repVar = lapply(ans, function(x) x$repVar) %>% Reduce(rbind, .) %>% pull()

  means = fullMeanTbl %>% t() %>% as.vector()
  attr(means, "var") = repVar
  attr(means, "statistic") <- "Mean"
  attr(means, "name") = dsn$names$x

  if(return.replicates == T){
    replicates = lapply(ans, function(x) x$replicates %>% collect()) %>%
      Reduce(rbind, .)
    means = list(svydbrepstat = means, replicates = replicates)
  }
  class(means) = c("svydbrepstat")
  return(means)
}
