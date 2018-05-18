#'  Survey Tables
#'
#' @param formula A formula specifying margins for the table, only + can be used.
#' @param design \code{svydb.design} object.
#' @param as.local A logical value indicating the returning object type.
#' Default is database tables, \code{tbl_sql} if the input is \code{tbl_sql}.
#' @description
#' Create contingency tables for survey data sets.
#' @examples
#' data(nhane)
#' nh.dbsurv = svydbdesign(st = SDMVSTRA , wt = WTMEC2YR,id = SDMVPSU , data = nhane)
#' svydbtable (~Race3 + Smoke100 + Gender, nh.dbsur)
#' svydbtable (~MaritalStatus , design = nh.dbsurv , as.local = T)
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
#' \code{\link{svydbdesign}}

svydbtable = function(formula, design, as.local = F) {

    dsn = design$clone()
    dsn$setx(formula)
    d = dsn$data
    d = d %>% filter_all(all_vars(!is.na(.)))
    dsn$storename("all", colnames(d))

    ff = all.vars(formula)
    dsn$storename("base", ff[1], force = T)

    if (length(ff) == 1) {
        out = d %>% group_by(!!sym(dsn$names$base)) %>% summarise(wt = sum(!!sym(dsn$wt))) %>% arrange(!!sym(dsn$names$base))
        if (as.local == T) {
            out = collect(out)
        }
        return(out)
    }

    dsn$storename("by", ff[2], force = T)
    d = d %>% select(dsn$names$all, dsn$wt) %>% dummy_mut(data = ., by = !!sym(dsn$names$by), withBase = T)
    dsn$storename("dummy", colnames(d))
    d = d %>% select(-one_of(dsn$names$by))
    d = compute(d)

    if (length(ff) == 2) {
        out = d %>% group_by(!!sym(dsn$names$base)) %>% summarise_at(vars(dsn$names$dummy), funs(sum(. * (!!sym(dsn$wt))))) %>%
            arrange(!!sym(dsn$names$base))
        if (as.local == T) {
            out = collect(out)
        }
        return(out)
    }

    dsn$storename("others", ff[-c(1, 2)], force = T)
    combnTbl = d %>% select(dsn$names$others) %>% distinct() %>% arrange(!!!syms(dsn$names$others)) %>% collect()
    combnLst = split(combnTbl, seq(1, nrow(combnTbl)))

    sTbls = function(by) {
        nname = paste(colnames(by), " = ", by, collapse = " & ")
        con = gsub(pattern = "=", replacement = "==", nname)
        d = d %>% filter(!!parse_expr(con)) %>% select(-one_of(colnames(by))) %>% group_by(!!sym(dsn$names$base)) %>%
            summarise_at(vars(dsn$names$dummy), funs(sum(. * (!!sym(dsn$wt))))) %>% arrange(!!sym(dsn$names$base)) %>%
            list(.)
        names(d) = nname
        d
    }
    out = lapply(combnLst, sTbls) %>% flatten()

    if (as.local == T) {
        out = lapply(out, collect)
    }

    return(out)
}
