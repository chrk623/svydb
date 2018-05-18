svydbhcell2xy = function(d) {
    xbins = d$xbins
    xbnds = d$xbnds
    c3 = diff(xbnds)/xbins
    ybnds = d$ybnds
    c4 = (diff(ybnds) * sqrt(3))/(2 * d$shape * xbins)
    jmax = d$dimen[2]
    cell = d$cell - 1
    i = cell%/%jmax
    j = cell%%jmax
    y = c4 * i + ybnds[1]
    x = c3 * ifelse(i%%2 == 0, j, j + 0.5) + xbnds[1]
    
    return(list(x = x, y = y))
}


svydbhbin = function(xy, x, y, xName, yName, cell, cnt, xcm, ycm, size, shape, rx, ry, bnd, n) {
    xmin = rx[1]
    ymin = ry[1]
    xr = rx[2] - xmin
    yr = ry[2] - ymin
    c1 = size/xr
    c2 = size * shape/(yr * sqrt(3))
    
    jinc = floor(bnd[2])
    lat = floor(jinc + 1)
    iinc = floor(2 * jinc)
    lmax = floor(bnd[1] * as.integer(bnd[2]))
    con1 = 0.25
    con2 = 1/3
    
    xy = xy %>% mutate(sx = !!quo(c1), sy = !!quo(c2), xmin = !!quo(xmin), ymin = !!quo(ymin))
    xy = xy %>% mutate(sx = sx * (x - xmin))
    xy = xy %>% mutate(sy = sy * (y - ymin))
    xy = xy %>% mutate(j1 = floor(sx + 0.5), i1 = floor(sy + 0.5))
    xy = xy %>% mutate(dist1 = (sx - j1)^2 + 3 * (sy - i1)^2, iinc = !!quo(iinc), lat = !!quo(lat))
    xy = xy %>% mutate(con1 = !!quo(con1), con2 = !!quo(con2), j2 = floor(sx), i2 = floor(sy))
    xy = xy %>% mutate(con3 = (sx - j2 - 0.5)^2 + 3 * (sy - i2 - 0.5)^2)
    xy = xy %>% mutate(L = case_when(dist1 < con1 ~ floor(i1 * iinc + j1 + 1), dist1 > con2 ~ floor(floor(sy) * iinc + 
        floor(as.double(sx)) + lat), TRUE ~ case_when(dist1 <= con3 ~ floor(i1 * iinc + j1 + 1), TRUE ~ floor(i2 * 
        iinc + j2 + lat))))
    
    Lfulltbl = xy %>% select(x, y, L)
    cmsTbl = Lfulltbl %>% group_by(L) %>% summarise(xcm = mean(x), ycm = mean(y)) %>% arrange(L) %>% select(-L) %>% 
        collect()
    Ltbl = xy %>% select(L2 = L, wt) %>% group_by(L2) %>% summarise(cnt = sum(wt))
    xy = xy %>% select(x, y) %>% mutate(L2 = row_number())
    xy = left_join(xy, Ltbl, by = "L2") %>% mutate(cnt = case_when(is.na(cnt) ~ 0, TRUE ~ cnt)) %>% arrange(L2)
    cntsTbl = xy %>% rename(cell = L2) %>% mutate(lt1 = case_when(cnt > 0 ~ 1, TRUE ~ 0)) %>% filter(lt1 == 1) %>% 
        select(-lt1) %>% collect()
    
    out = list(cell = cntsTbl$cell, count = cntsTbl$cnt, xcm = cmsTbl$xcm, ycm = cmsTbl$ycm, xbins = size, shape = shape, 
        xbnds = rx, ybnds = ry, dimen = bnd, n = n, ncells = nrow(cntsTbl), xlab = xName, ylab = yName)
    xy = svydbhcell2xy(out)
    out = c(xy, out)
    
    return(out)
}


svydbhexbin = function(formula, design, xbins = 30, shape = 1) {
    
    dsn = design$clone()
    dsn$setx(formula)
    dsn$storename("y", all.vars(formula)[1])
    dsn$storename("x", all.vars(formula)[-1])
    d = dsn$data
    d = d %>% rename(x = !!sym(dsn$names$x)) %>% rename(y = !!sym(dsn$names$y)) %>% rename(wt = !!sym(dsn$wt)) %>% 
        filter(!is.na(x)) %>% filter(!is.na(y))
    d = compute(d)
    
    n = d %>% db_nrow()
    x = d %>% select(x)
    y = d %>% select(y)
    
    xbnds = c(dbmin(d, x), dbmax(d, x))
    ybnds = c(dbmin(d, y), dbmax(d, y))
    
    jmax = floor(xbins + 1.5001)
    c1 = 2 * floor((xbins * shape)/sqrt(3) + 1.5001)
    imax = trunc((jmax * c1 - 1)/jmax + 1)
    lmax = jmax * imax
    
    ans = svydbhbin(xy = d, x = x, y = y, xName = dsn$names$x, yName = dsn$names$y, cell = as.integer(lmax), cnt = as.integer(lmax), 
        xcm = as.integer(lmax), ycm = as.integer(lmax), size = xbins, shape = shape, rx = as.double(xbnds), ry = as.double(ybnds), 
        bnd = as.integer(c(imax, jmax)), n = n)
    
    return(ans)
}
svydbhexplot = function(d, xlab = d$xlab, ylab = d$ylab) {
    pdata = tibble(x = d$x, y = d$y, count = d$count, xcm = d$xcm, ycm = d$ycm)
    
    p = ggplot(pdata) + geom_hex(aes(x = x, y = y, fill = count), color = "black", stat = "identity") + labs(x = xlab, 
        y = ylab) + scale_fill_continuous(trans = "reverse")
    
    print(p)
    
    invisible(p)
}
