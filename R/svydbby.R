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
