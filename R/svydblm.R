#' Regression
#'
#' @param formula Model formula.
#' @param design \code{svydb.design} object.
#' @description
#' Linear models for survey data sets.
#' @examples
#' data(nhane)
#' nh.dbsurv = svydbdesign(st = SDMVSTRA, wt = WTMEC2YR, id = SDMVPSU, data = nhane)
#' svydblm(DirectChol ~ Age + BMI + factor(Gender), design = nh.dbsurv)
#' svydblm(DirectChol ~ Age + BMI, design = nh.dbsurv$subset(Race3 == 3))
#' coef(svydblm(DirectChol ~ Age + BMI, design = nh.dbsurv))
#' SE(svydblm(DirectChol ~ Age + BMI, design = nh.dbsurv))
#' vcov(svydblm(DirectChol ~ Age + BMI, design = nh.dbsurv))
#' head(residuals(svydblm(DirectChol ~ Age + BMI, design = nh.dbsurv)))
#' summary(svydblm(DirectChol ~ Age + BMI, design = nh.dbsurv))
#' predict(svydblm(DirectChol ~ Age + BMI, design = nh.dbsurv),
#'     newdata = data.frame(Age = 1:3, BMI = 4:6))
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

svydblm = function(formula, design) {
    if (!("svydb.design" %in% class(design))) {
        stop("Please provide a svydb.design")
    }

    if (missing(formula)) {
        stop("Please provide a formula")
    }

    fit = list()
    fit$call = match.call()
    dsn = design$clone()
    dsn$setx(formula)
    d = dsn$data
    dsn$storename("y", all.vars(formula)[1])
    dsn$storename("variables", all.vars(formula)[-1])

    d = d %>%
      mutate(!!sym(dsn$wt) := case_when(is.na(!!sym(dsn$names$y))
                                        ~ 0, TRUE ~ !!sym(dsn$wt)))
    # d = d %>% mutate(`:=`(!!sym(dsn$names$y), case_when(is.na(!!sym(dsn$names$y)) ~ 0, TRUE ~ !!sym(dsn$names$y))))
    d = d %>% filter_all(all_vars(!is.na(.)))
    d = d %>% mutate(intercept = 1)
    dsn$storename("intercept", colnames(d))
    xy = d

    facVar = attr(terms(formula, specials = "factor"), "specials")$factor
    if (!is.null(facVar)) {
        facVar = facVar - 1
        dsn$storename("factor", dsn$names$variables[facVar], force = T)
        for (i in 1:length(facVar)) {
            dd = dummy_mut(xy, by = !!sym(dsn$names$factor[i]), withBase = F, return.level = T)
            xy = dd$dum
            dsn$storelevel(x = dd$levels, name = dsn$names$factor[i])
            dsn$names$variables = c(dsn$names$variables, dsn$names$factor[i])
            dsn$names$variables = dsn$names$variables[-(grep(dsn$names$factor[i], dsn$names$variables)[1])]
        }
    }

    fit$terms = terms(paste("~", paste(dsn$names$variables, collapse = " + ")) %>% as.formula)
    dsn$storename("dummy", colnames(xy))
    xy = compute(xy)
    dsn$storename("variables", c(dsn$names$variables, dsn$names$dummy), force = T)

    if (!is.null(facVar)) {
        dsn$removename("variables", dsn$names$factor)
    }
    db_xtwx_i = function(x, col, wt, data) {
        data = data %>% summarise_at(vars(x), funs(xtx = sum(. * (!!sym(col)) * (!!sym(wt)))))
        return(data)
    }
    xtx = lapply(c(dsn$names$intercept, dsn$names$variables), db_xtwx_i, x = c(dsn$names$intercept, dsn$names$variables),
        wt = dsn$wt, data = xy)
    xtx = Reduce(full_join, xtx) %>% collect() %>% as.matrix()
    colnames(xtx) = c(dsn$names$intercept, dsn$names$variables)

    xty = xy %>% transmute_at(vars(dsn$names$intercept, dsn$names$variables), funs(. * (!!sym(dsn$wt)) * (!!sym(dsn$names$y)))) %>%
        summarise_all(sum) %>% collect() %>% t()

    xy = xy %>% filter(!(!!sym(dsn$wt)) == 0) %>% compute()
    beta = solve(xtx) %*% xty
    fit$coefname = c(dsn$names$intercept, dsn$names$variables)
    fit$coefficients = beta %>% t()


    dsn$storename("xb", paste(dsn$names$variables, "_xb", sep = ""))
    e = paste(dsn$names$y, " - ", beta["intercept", ], " - ", paste(rownames(beta)[-1], " * ", beta[-1, ], sep = "",
        collapse = " - "), sep = "")

    xy = xy %>% mutate(residuals = !!parse_expr(e)) %>% select(-one_of(dsn$names$y))
    dsn$storename("residuals", colnames(xy))
    res = xy %>% select(residuals)
    fit$residuals = res

    varTbl = xy %>% mutate_at(vars(dsn$names$intercept, dsn$names$variables), funs(. * (!!sym(dsn$names$residuals)) *
        (!!sym(dsn$wt)))) %>% group_by(!!sym(dsn$st), !!sym(dsn$id)) %>% summarise_all(sum) %>% compute()
    dsn$storename("zhi", c(dsn$names$intercept, dsn$names$variables), force = T)
    barTbl = varTbl %>% select(dsn$st, dsn$names$zhi) %>% group_by(!!sym(dsn$st)) %>% summarise_all(sum)

    mh = dsn$getmh()
    barTbl = inner_join(barTbl, mh, by = dsn$st) %>% mutate_at(vars(dsn$names$intercept, dsn$names$variables), funs(bar = ./m_h)) %>%
        compute()
    dsn$storename("bar", colnames(barTbl))

    varTbl = inner_join(varTbl %>% select(dsn$st, dsn$names$zhi), barTbl %>% select(dsn$st, dsn$names$bar, m_h),
        by = dsn$st)
    `zhi-zbar` = paste("`", dsn$names$zhi, "`", "-", "`", dsn$names$bar, "`", collapse = " ; ", sep = "")
    varTbl = varTbl %>% mutate(!!!parse_exprs(`zhi-zbar`)) %>% ungroup() %>% compute()
    dsn$storename("diff", colnames(varTbl))

    covDiag = sapply(dsn$names$diff, svydbVar, st = dsn$st, m_h = "m_h", data = varTbl)
    e = outer(dsn$names$diff, dsn$names$diff, paste, sep = ",")
    e = e[lower.tri(e)] %>% strsplit(., ",")
    covLt = sapply(e, svydbVar2, st = dsn$st, m_h = "m_h", data = varTbl)
    cov.mat = diag(covDiag)
    cov.mat[lower.tri(cov.mat)] = covLt
    cov.mat[upper.tri(cov.mat)] = t(cov.mat)[upper.tri(cov.mat)]

    fit$cov.unscaled = solve(xtx) %*% cov.mat %*% solve(xtx)
    colnames(fit$cov.unscaled) = rownames(fit$cov.unscaled)

    fit$formula = formula
    fit$df.residual = sum(mh %>% select(m_h) %>% pull) - db_nrow(mh) - nrow(beta) + 1
    fit$call = match.call()
    fit$design = dsn

    class(fit) = c("svydblm", "lm")
    return(fit)
}
