#' Quantiles
#'
#' @param x Name indicating the variable.
#' @param quantiles Quantiles to estimate, a number, or a vector of numbers for multiple quantiles. Default to 0.5.
#' @param design \code{svydb.design} object.
#' @description
#' Compute quantiles from survey data sets.
#' @examples
#' data(nhane)
#' nh.dbsurv = svydbdesign(st = SDMVSTRA, wt = WTMEC2YR, id = SDMVPSU, data = nhane)
#' svydbquantile(x = Age, quantile = 0.5, design = nh.dbsurv)
#' svydbquantile(x = Age, quantile = c(0.25, 0.75), design = nh.dbsurv)
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
#' \code{\link{svydbdesign}}
#' @export

svydbquantile = function(x, quantiles = 0.5, design) {
    oldoptions = options("survey.lonely.psu")
    options(survey.lonely.psu = "adjust")

    dsn = design$clone()
    dsn$setx(!!enquo(x))
    d = dsn$data
    dsn$storename("x", colnames(d))

    q_name = quantiles
    qvec = c()
    b = T

    for (i in 1:length(quantiles)) {
        if (quantiles[i] >= 1) {
            qvec[i] = dbmax(data = d, var = !!sym(dsn$names$x))
        } else if (quantiles[i] <= 0) {
            qvec[i] = dbmin(data = d, var = !!sym(dsn$names$x))
        } else {
            if (b) {
                N = dsn$getwt()
                nrowdata = db_nrow(d)
                sampN = ceiling(nrowdata^(2/3))
                d = compute(d)

                if (!is.null(d$src$con)) {
                  sdata = d %>% svydb_monet_sampleN(sampN) %>% tbl_df()
                } else {
                  sdata = d %>% sample_n(sampN) %>% tbl_df()
                }

                sdata = sdata %>% rename(x = !!sym(dsn$names$x))
                s.surv = svydesign(id = c2f(dsn$id), st = c2f(dsn$st), weights = c2f(dsn$wt), data = sdata, nest = T)
                b = F
            }
            notfound = TRUE

            while (notfound) {
                q = svyquantile(~x, s.surv, quantiles[i], alpha = 0.1, ci = TRUE, na.rm = T)

                temp_lq = q$CIs[1]
                temp_uq = q$CIs[2]

                readIn = d %>% select(x = dsn$names$x, wt = dsn$wt) %>% filter(x >= temp_lq & x <= temp_uq)
                readIn_wts = readIn %>% select(wt) %>% summarise(sum(wt)) %>% pull()
                notRead = d %>% select(x = dsn$names$x, wt = dsn$wt) %>% filter(x < temp_lq)
                notRead_wts = notRead %>% select(wt) %>% summarise(sum(wt)) %>% pull()

                thold = (N * quantiles[i])
                if ((notRead_wts <= thold) & (thold < (readIn_wts + notRead_wts))) {
                  qs = readIn %>% arrange(x)
                  qs = qs %>% collect() %>% mutate(wt2 = cumsum(wt))
                  c_wts = N * quantiles[i] - notRead_wts
                  qs = qs %>% filter(wt2 >= !!quo(c_wts)) %>% select(x) %>% slice(1) %>% pull()
                  qvec[i] = qs
                  notfound = F
                } else {
                  if (!is.null(d$src$con)) {
                    sdata = d %>% svydb_monet_sampleN(sampN) %>% tbl_df() %>% rename(x = !!sym(dsn$names$x))
                  } else {
                    sdata = d %>% sample_n(sampN) %>% tbl_df() %>% rename(x = !!sym(dsn$names$x))
                  }
                  s.surv = svydesign(id = c2f(dsn$id), st = c2f(dsn$st), weights = c2f(dsn$wt), data = sdata, nest = T)
                }
            }
        }
    }
    names(qvec) = as.character(q_name)
    options(oldoptions)
    return(qvec)
}
