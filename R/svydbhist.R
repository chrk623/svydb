#' Histogram
#'
#' @param x Name indicating the variable.
#' @param design \code{svydb.design} object.
#' @param binwidth The width of each  bin. Binswidths are calculated with Sturges’ formula.
#' @param xlab,ylab labels for xlab and ylab.
#' @description
#' Histograms weighted by sampling weights.
#' @examples
#' data(nhane)
#' nh.dbsurv = svydbdesign(st = SDMVSTRA , wt = WTMEC2YR,id = SDMVPSU , data = nhane)
#' svydbhist(x = DirectChol , design = nh.dbsurv)
#' svydbhist(x = DirectChol , design = nh.dbsurv ,binwidth = 0.25)
#' svydbhist(x = Height , design = nh.dbsurv, xlab = "xx", ylab = "yy")
#' svydbhist(x = Weight , design = nh.dbsurv) + ggtitle("Weight")
#' # OR with a database connection
#' # library(MonetDBLite)
#' # library(DBI)
#' # library(dbplyr)
#' # con = dbConnect(MonetDBLite())
#' # dbWriteTable(con, "nhane", nhane)
#' # nhane.db = tbl(con, "nhane")
#' # nh.dbsurv = svydbdesign(st = SDMVSTRA , wt = WTMEC2YR,id = SDMVPSU , data = nhane.db)
#' @author Charco Hui
#' @seealso
#' \code{\link{svydbdesign}}, \code{\link{svydbmean}}
#' @export

svydbhist = function(x, design, binwidth = NULL, xlab = "x", ylab = "Density", ...) {

    if (!("svydb.design" %in% class(design))) {
        stop("Please provide a svydb.design")
    }

    dsn = design$clone()
    dsn$setx(!!enquo(x))
    d = dsn$data
    dsn$storename("x", colnames(d))
    d_n = d %>% db_nrow()

    x_max = d %>% dbmax(!!sym(dsn$names$x), asNum = T)
    x_min = d %>% dbmin(!!sym(dsn$names$x), asNum = T)
    x_range = x_max - x_min

    if (is.null(binwidth)) {
        binwidth = ceiling(log2(d_n) + 1)
        pbreaks = pretty(c(x_min, x_max), n = binwidth, min.n = 1)
    } else {
        pbreaks = seq(from = floor(x_min), to = ceiling(x_max), by = binwidth)
    }

    d = db_cut2(var = !!sym(dsn$names$x), breaks = pbreaks, data = d) %>% arrange(cut)
    dsn$data = d

    props = svydbmean(x = cut, design = dsn, num = F, return.mean = T) %>% collect() %>% t()
    colnames(props) = "Mean"

    mids = pbreaks[-length(pbreaks)] + diff(pbreaks)/2

    if (length(mids) != nrow(props)) {
        props = tbl_df(props) %>% mutate(uqid = row_number())
        d = left_join(mids %>% tbl_df() %>% mutate(uqid = row_number()), props, by = "uqid")
        d = d %>% mutate(Mean = case_when(is.na(Mean) ~ 0, TRUE ~ Mean)) %>% select(-uqid)
    } else {
        d = cbind(mids, props) %>% tbl_df()
    }

    colnames(d) = c("x", "y")
    d$y = d$y/diff(pbreaks)
    p = ggplot(d) + geom_col(aes(x, y)) + labs(x = dsn$names$x, y = ylab)

    print(p)

    invisible(p)
}
