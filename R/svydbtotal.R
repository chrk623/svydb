#' Population Total
#'
#' @param x Name indicating the variable.
#' @param num \code{TRUE} or \code{FALSE} indicating whether x is numeric or categorical.
#' @param design \code{svydb.design} object.
#' @param return.total \code{TRUE} to return only totals, no standard errors.
#' @description
#' Computes population total from survey data sets.
#' @examples
#' data(nhane)
#' nh.dbsurv = svydbdesign(st = SDMVSTRA , wt = WTMEC2YR,id = SDMVPSU , data = nhane)
#' svydbtotal(x = DirectChol, design = nh.dbsurv , num = T)
#' svydbtotal(x = Race3, design = nh.dbsurv , num = F)
#' svydbtotal(x = DirectChol, design = nh.dbsurv , num = T, return.total = T)
#' coef(svydbtotal(x = DirectChol, design = nh.dbsurv , num = T))
#' SE(svydbtotal(x = DirectChol, design = nh.dbsurv , num = T))
#' # OR with a database connection
#' # require(MonetDBLite)
#' # require(DBI)
#' # require(dbplyr)
#' # con = dbConnect(MonetDBLite())
#' # dbWriteTable(con, "nhane", nhane)
#' # nhane.db = tbl(con, "nhane")
#' # nh.dbsurv = svydbdesign(st = SDMVSTRA , wt = WTMEC2YR,id = SDMVPSU , data = nhane.db)
#' @author Charco Hui
#' @seealso
#' \code{\link{svydbdesign}}, \code{\link{svydbmean}}
#' @export

svydbtotal = function(x, num, design, return.total = F, ...) {

    if (!("svydb.design" %in% class(design))) {
        stop("Please provide a svydb.design")
    }

    if (missing(num)) {
        stop("Is x a numeric or categorical variable?, num = T OR num = F?")
    }

    dsn = design$clone()
    dsn$setx(!!enquo(x))
    d = dsn$data
    dsn$storename("x", colnames(d))

    if (num == F) {
        d = dummy_mut(d, !!sym(dsn$names$x), withBase = T)
    }
    dsn$storename("x", colnames(d))
    d = d %>% mutate_at(vars(dsn$names$x), funs((. * !!sym(dsn$wt)))) %>% compute(temporary = T)

    totTbl = d %>% select(dsn$names$x) %>% summarise_all(sum) %>% collect() %>% t()

    if (return.total == TRUE) {
        colnames(totTbl) = "Total"
        return(totTbl)
    }

    varTbl = d %>% select(dsn$st, dsn$id, dsn$names$x) %>% group_by(!!!syms(c(dsn$st, dsn$id))) %>% summarise_at(vars(dsn$names$x),
        funs(sum(.))) %>% compute(temporary = T)
    varTbl = inner_join(varTbl, dsn$getmh(), by = dsn$st)

    barTbl = varTbl %>% select(-one_of(dsn$id)) %>% group_by(!!sym(dsn$st)) %>% summarise_at(vars(dsn$names$x), funs(bar = sum(./m_h)))
    dsn$storename("bar", colnames(barTbl))

    varTbl = inner_join(varTbl, barTbl, by = dsn$st)
    `zhi-zbar` = paste(dsn$names$x, "-", dsn$names$bar, collapse = " ; ")
    varTbl = varTbl %>% mutate(!!!parse_exprs(`zhi-zbar`)) %>% ungroup() %>% compute(temporary = T)
    dsn$storename("diff", colnames(varTbl))

    varTbl = sapply(dsn$names$diff, svydbVar, st = dsn$st, m_h = "m_h", data = varTbl)

    class(totTbl) = "svydbstat"
    attr(totTbl, "var") = varTbl
    attr(totTbl, "statistic") <- "Total"
    attr(totTbl, "name") = dsn$names$x

    return(totTbl)
}
