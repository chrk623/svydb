#'  Conditional plots
#'
#' @param formula  Formula indicating x and y. i.e. y~x.
#' @param by Formula indicating the conditions of each plot. i.e by~by2
#' @param design \code{svydb.design} object.
#' @description
#' Hexagon binning will be used for conditional plots.
#' @examples
#' data(nhane)
#' nh.dbsurv = svydbdesign(st = SDMVSTRA, wt = WTMEC2YR, id = SDMVPSU, data = nhane)
#' svydbcoplot(Age ~ Height, by = SmokeNow~Gender, design = nh.dbsurv)
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
#' \code{\link{svydbdesign}}, \code{\link{svydbhexbin}}, \code{\link{svydbhexplot}}
#' @export
svydbcoplot = function(formula, by, design) {
    if (!is_formula(by)) {
        stop("by must be a formula")
    }

    y = all.vars(formula)[1]
    x = all.vars(formula)[-1]
    dsn = design$clone()

    by_var = all.vars(by)
    by = dsn$data %>% select(!!!syms(by_var)) %>% distinct() %>% arrange(!!sym(by_var[1])) %>% collect()
    by = split(by, seq(nrow(by)))

    filterData = function(by, dsn, x, y) {
        dsn = dsn$subset(paste(colnames(by), " == ", by, collapse = " & "), logical = F)
        hb = svydbhexbin(formula, design = dsn)
        if (length(hb$x) | length(hb$y) | length(hb$count) != 0) {
            cbind(tibble(x = hb$x, y = hb$y, count = hb$count), by)
        }
    }

    p = lapply(by, filterData, dsn = dsn) %>% Reduce(rbind, .)

    p = ggplot(p) + geom_hex(aes(x = x, y = y, fill = count), color = "black", stat = "identity") + labs(x = x, y = y) +
        scale_fill_continuous(trans = "reverse") + facet_wrap(by_var, labeller = "label_both")

    print(p)

    invisible(p)
}
