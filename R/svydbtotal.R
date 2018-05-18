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
