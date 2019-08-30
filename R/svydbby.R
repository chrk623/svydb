#'Survey statistics on subsets
#'
#' @param x A variable specifying the variable to pass to FUN.
#' @param by A variable specifying factors that define the subsets.
#' @param FUN A function indicating the desired survey statistics.
#' @param design \code{svydb.design} object.
#' @param ... Other arguments to pass to FUN.
#' @description
#' Survey statistics on subsets, currently only works on \code{\link{svydbtotal}},
#' \code{\link{svydbmean}}.
#' @examples
#' data(nhane)
#' nh.dbsurv = svydbdesign(st = SDMVSTRA, wt = WTMEC2YR, id = SDMVPSU, data = nhane)
#' svydbby(x = Age, by = Gender, FUN = svydbmean, design = nh.dbsurv, num = T)
#' svydbby(x = BMI, by = Race3, FUN = svydbtotal, design = nh.dbsurv, num = T)
#' # OR with a database connection
#' # library(MonetDBLite)
#' # library(DBI)
#' # library(dbplyr)
#' # con = dbConnect(MonetDBLite())
#' # dbWriteTable(con, "nhane", nhane)
#' # nhane.db = tbl(con, "nhane")
#' # nh.dbsurv = svydbdesign(st = SDMVSTRA, wt = WTMEC2YR, id = SDMVPSU, data = nhane.db)
#' @author Charco Hui
#' @seealso
#' \code{\link{svydbdesign}}, \code{\link{svydbtotal}}, \code{\link{svydbmean}}
#' @export

svydbby = function(x, by, FUN, design, ...) {

    if (!("svydb.design" %in% class(design))) {
        stop("Please provide a svydb.design")
    }

    x = enquo(x)
    by = enquo(by)

    dsn = design$clone()
    dsn$setx(!!x)
    dsn$storename("x", colnames(dsn$data))
    dsn$addx(x = !!by)
    d = dsn$data
    dsn$storename("by", colnames(d))

    byGroups = d %>% distinct(!!sym(dsn$names$by)) %>% pull() %>% na.omit()
    byGroups = paste(dsn$names$by, "==", byGroups)

    out = lapply(byGroups, function(g) {
        FUN(x = !!x, design = dsn$subset(g, logical = F), ...) %>% as.data.frame()
    }) %>% Reduce(rbind, .)

    rownames(out) = byGroups
    out = list(out)
    names(out) = dsn$names$x

    return(out)
}
