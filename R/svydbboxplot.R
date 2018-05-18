#' Boxplot
#'
#' @param x Name indicating the variable.
#' @param groups  Ifgroupsis defined, boxes of x will be split bygroups.
#' @param design \code{svydb.design} object.
#' @param varwidth  if \code{varwidth = T},  the  width of the boxes are proportional
#' to the number of observations in that box.
#' @param outlier if \code{outlier = T}, observations outside of the 1.5 * IQR will be
#' plotted as points.
#' @param all.outlier if \code{all.outlier = F}, Not all the outliers will be plotted.
#' #' @description
#' #' Boxplots for survey data sets.
#' @examples
#' data(nhane)
#' nh.dbsurv = svydbdesign(st = SDMVSTRA , wt = WTMEC2YR,id = SDMVPSU , data = nhane)
#' svydbboxplot(x = Age , groups = Gender ,design = nh.dbsurv)
#' svydbboxplot(x = Age , groups = Race3 ,design = nh.dbsurv, varwidth = T)
#' svydbboxplot(x = Height , groups = Race3 ,design = nh.dbsurv, outlier = T)
#' svydbboxplot(x = Weight , groups = Gender ,design = nh.dbsurv,
#'      outlier = T, all.outlier = T, varwidth = T)
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
#' \code{\link{svydbdesign}}, \code{\link{svydbquantile}}

svydbboxplot = function(x, groups = NULL, design, varwidth = F, outlier = F, all.outlier = F) {

    groups = enquo(groups)
    dsn = design$clone()
    dsn$setx(!!enquo(x))
    d = dsn$data
    dsn$storename("x", colnames(d))

    if (quo_is_null(groups)) {
        boxes = svydbquantile(x = !!sym(dsn$names$x), quantile = c(0, 0.25, 0.5, 0.75, 1), design = dsn) %>% t() %>%
            tbl_df() %>% mutate("")
        ax = c(x = "", y = dsn$names$x)
        colnames(boxes) = c(as.character(letters[1:5]), "x")
    } else {
        group_name = as.character(groups)[2]
        dsn$addx(group_name)
        d = dsn$data
        dsn$storename("groups", colnames(d))

        group_levels = distinct(d, !!sym(dsn$names$groups)) %>% collect()
        group_names = paste(colnames(group_levels), pull(group_levels), sep = " ")
        group_names2 = paste(colnames(group_levels), paste("'", pull(group_levels), "'", sep = ""), sep = " ")

        f = function(x) {
            svydbquantile(x = !!sym(dsn$names$x), quantile = c(0, 0.25, 0.5, 0.75, 1), design = dsn$subset(x, logical = F))
        }

        boxes = sapply(gsub(pattern = " ", replacement = "==", x = , group_names2), f)
        boxes = t(boxes) %>% tbl_df() %>% bind_cols(group_levels)
        ax = c(x = dsn$names$groups, y = dsn$names$x)
        colnames(boxes) = c(as.character(letters[1:5]), "x")
        boxes$x = as.character(boxes$x)
    }

    haveOut = F
    if (outlier == T) {
        boxes = boxes %>% mutate(outUP = d + 1.5 * (d - b), checkUP = ifelse(outUP < e, T, F), outLow = b - 1.5 *
            (d - b), checkLow = ifelse(outLow > a, T, F))
        if (any(boxes$checkLow) == T) {
            haveOut = T
            boxes = boxes %>% mutate(a = ifelse(checkLow == T, outLow, a))
            outls = paste(gsub(pattern = " ", replacement = "==", x = group_names2), "&", dsn$names$x, "<", boxes$a,
                collapse = " | ")
            outlsLow = d %>% filter(!!!parse_exprs(outls)) %>% select(x = dsn$names$groups, y = dsn$names$x) %>%
                mutate(x = as.character(x))
            if (all.outlier == F) {
                outlsLow = outlsLow %>% group_by(x) %>% summarise(y = min(y))
            }
            outlsLow = outlsLow %>% tbl_df()
        }

        if (any(boxes$checkUP) == T) {
            haveOut = T
            boxes = boxes %>% mutate(e = ifelse(checkUP == T, outUP, e))
            outls = paste(gsub(pattern = " ", replacement = "==", x = group_names2), "&", dsn$names$x, ">", boxes$e,
                collapse = " | ")
            outlsUP = d %>% filter(!!!parse_exprs(outls)) %>% select(x = dsn$names$groups, y = dsn$names$x) %>% mutate(x = as.character(x))
            if (all.outlier == F) {
                outlsUP = outlsUP %>% group_by(x) %>% summarise(y = max(y))
            }
            outlsUP = outlsUP %>% tbl_df()
        }
        outls = bind_rows(outlsUP, outlsLow)
    }

    p = ggplot(boxes) + labs(x = ax["x"], y = ax["y"])

    if (varwidth == T) {
        boxwid = d %>% group_by(!!sym(dsn$names$groups)) %>% summarise(wid = n()) %>% collect()
        p$data = p$data %>% mutate(width = (boxwid$wid/sum(boxwid$wid)))
        p = p + geom_boxplot(aes(x = as.factor(x), ymin = a, lower = b, middle = c, upper = d, ymax = e, width = width),
            stat = "identity")

    } else {
        p = p + geom_boxplot(aes(x = as.factor(x), ymin = a, lower = b, middle = c, upper = d, ymax = e), stat = "identity")
    }

    if (haveOut == T) {
        p = p + geom_point(data = outls, aes(x = x, y = y))
    }

    print(p)

    return(p)
}
