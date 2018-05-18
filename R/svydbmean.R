svydbmean = function(x, num, design, return.mean = F, ...) {
    
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
    N = dsn$getwt()
    meanTbl = d %>% transmute_at(vars(dsn$names$x), funs((. * !!sym(dsn$wt))/!!quo(N))) %>% summarise_all(sum) %>% 
        compute(temporary = T) %>% collect()
    
    if (return.mean == TRUE) {
        colnames(meanTbl) = dsn$names$x
        return(meanTbl)
    }
    
    dhi_exprs = paste(dsn$names$x, " - ", "meanTbl$", dsn$names$x, sep = "", collapse = " ; ")
    varTbl = d %>% mutate(dhi = !!!parse_exprs(dhi_exprs))
    dsn$storename("dhi", colnames(varTbl))
    
    varTbl = varTbl %>% mutate_at(vars(dsn$names$dhi), funs((. * !!sym(dsn$wt))/!!quo(N))) %>% select(dsn$st, dsn$id, 
        dsn$names$dhi)
    
    barTbl = varTbl %>% select(dsn$st, dsn$names$dhi) %>% group_by(!!sym(dsn$st)) %>% summarise_all(sum)
    barTbl = inner_join(barTbl, dsn$getmh(), by = dsn$st) %>% mutate_at(vars(dsn$names$dhi), funs(bar = ./m_h)) %>% 
        select(-one_of(dsn$names$dhi))
    dsn$storename("bar", colnames(barTbl))
    
    varTbl = varTbl %>% group_by(!!!syms(c(dsn$st, dsn$id))) %>% summarise_all(sum) %>% compute(temporary = T)
    varTbl = inner_join(varTbl, barTbl, by = dsn$st)
    
    `dhi-dbar` = paste("`", dsn$names$dhi, "`", "-", "`", dsn$names$bar, "`", collapse = " ; ", sep = "")
    varTbl = varTbl %>% mutate(!!!parse_exprs(`dhi-dbar`)) %>% ungroup() %>% compute(temporary = T)
    dsn$storename("diff", colnames(varTbl))
    
    varTbl = sapply(dsn$names$diff, svydbVar, st = dsn$st, m_h = "m_h", data = varTbl)
    
    meanTbl = t(meanTbl)
    class(meanTbl) = "svydbstat"
    attr(meanTbl, "var") = varTbl
    attr(meanTbl, "statistic") <- "Mean"
    attr(meanTbl, "name") = dsn$names$x
    
    return(meanTbl)
}
