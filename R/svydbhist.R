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
