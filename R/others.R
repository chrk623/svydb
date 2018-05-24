dbmin = function(data, var, asNum = T) {
    var = enquo(var)
    data = data %>% ungroup() %>% select(!!var) %>% summarise(min = min(!!var))
    if (asNum == T) {
        data %>% pull
    } else {
        data
    }
}

dbmax = function(data, var, asNum = T) {
    var = enquo(var)
    data = data %>% ungroup() %>% select(!!var) %>% summarise(max = max(!!var))
    if (asNum == T) {
        data %>% pull
    } else {
        data
    }
}

db_nrow = function(data) {
    data %>% ungroup() %>% count() %>% pull()
}

db_dim = function(data) {
    n_rows = data %>% ungroup() %>% count() %>% pull()
    n_cols = ncol(data)

    out = c(n_rows, n_cols)
    names(out) = c("rows", "cols")
    out
}

db_view = function(data, num = 1) {
    if (class(data) == "list") {
        View(data[[num]] %>% tbl_df())
    } else {
        View(data %>% tbl_df)
    }
}

db_rowSums_mut = function(data, vars = NULL, newRowName = "rSum") {
    if (is.null(vars)) {
        s = paste(colnames(data), collapse = " + ")
    } else {
        s = paste(vars, collapse = " + ")
    }
    q = quote(mutate(data, rSum = s))

    eval(parse(text = sub("rSum", newRowName, sub("s", s, deparse(q)))))
}

db_rowSums = function(data){
  cn = colnames(data)
  rs = paste("`", cn, "`", sep = "", collapse = " + ")
  data %>% transmute(rowsum = !!parse_expr(rs))
}

db_cbind = function(x, y) {
    x = x %>% mutate(`___i` = "key")
    y = y %>% mutate(`___i` = "key")
    out = inner_join(x, y, by = "___i", copy = T) %>% select(-`___i`)
    return(out)
}

db_slice = function(data, n) {
    n = enquo(n)
    data %>% mutate(`___i` = row_number()) %>% filter(`___i` <= !!n) %>% select(-`___i`)
}

dummy_mut = function(data, by, withBase = T, return.level = F) {
    by = enquo(by)
    dum = data %>% distinct(!!by) %>% arrange(!!by) %>% data.frame() %>% na.omit()
    level = as.character(dum[, 1])
    cs = contrasts(as.factor(dum[, 1]))
    by_name = colnames(dum)

    if (withBase == T) {
        c1 = c(1, rep(0, nrow(dum) - 1))
        dum = cbind(dum, c1, contrasts(as.factor(dum[, 1])))
        name = paste(colnames(dum)[1], as.character(dum[, 1]), sep = "_")
        colnames(dum) = c(colnames(dum)[1], name)
    } else {
        dum = cbind(dum, contrasts(as.factor(dum[, 1])))
        name = paste(colnames(dum)[1], as.character(dum[, 1])[-1], sep = "_")
        colnames(dum) = c(colnames(dum)[1], name)
    }

    dum = inner_join(data, dum, by = by_name, copy = T)

    if (withBase == F) {
        dum = dum %>% select(-!!by)
    }

    if (return.level == T) {
        dum = list(dum = dum, levels = level)
        return(dum)
    } else {
        return(dum)
    }
}

db_cut = function(var, breaks, data) {
    var = enquo(var)
    var_name = as.character(var)[2]
    data = data %>% rename(vars = !!var) %>% filter(!is.na(vars))
    breaks = breaks[-1]
    trues = seq(length(breaks))
    temp_exprs = paste("ifelse(vars", "<= ", breaks, ",", trues, ", _f_)")
    temp_exprs[length(breaks)] = gsub(pattern = "[_]f[_]", replacement = "NA", x = temp_exprs[length(breaks)])
    mut_exprs = temp_exprs[1]

    for (i in 2:length(breaks)) {
        mut_exprs = gsub(pattern = "[_]f[_]", replacement = temp_exprs[i], x = mut_exprs)
    }

    mut_exprs = parse_expr(mut_exprs)
    data = data %>% mutate(`_cuts_` = !!mut_exprs) %>% rename(`:=`(!!sym(var_name), vars)) %>% rename(cuts = `_cuts_`)
    data
}

db_cut2 = function(var, breaks, right = TRUE, data) {
    var = enquo(var)
    mult = diff(breaks)[1]

    if (right == TRUE) {
        data = data %>% mutate(cut = ((!!quo(mult)) * ceiling((!!var)/(!!quo(mult)))) - (!!quo(mult)))
    } else {
        data = data %>% mutate(cut = ((!!quo(mult)) * floor((!!var)/(!!quo(mult)))))
    }

    return(data)
}

svydbVar2 = function(x, xleft = 1, xright = 2, st, m_h, data) {
    xleft = x[xleft]
    xright = x[xright]
    m = data
    m = m %>% select(xleft = !!sym(xleft), xright = !!sym(xright), st = st, m_h = m_h)
    m_h = m %>% select(st, m_h) %>% mutate(m_h = 1) %>% group_by(st) %>% summarise(m_h = sum(m_h))
    m = m %>% select(-m_h)
    m = m %>% mutate(ztz = xleft * xright) %>% group_by(st) %>% summarise(ztz = sum(ztz))
    m = left_join(m, m_h, by = "st")
    m = m %>% mutate(scaled = ztz * (m_h/(m_h - 1))) %>% select(scaled) %>% summarise(sum(scaled)) %>% compute(temporary = T) %>%
        pull()
    return(m)
}
svydbVar = function(x, st, m_h, data) {
    m = data
    m = m %>% select(x = x, st = st, m_h = m_h)
    m_h = m %>% select(st, m_h) %>% mutate(m_h = 1) %>% group_by(st) %>% summarise(m_h = sum(m_h))
    m = m %>% select(-m_h)
    m = m %>% mutate(ztz = x * x) %>% group_by(st) %>% summarise(ztz = sum(ztz))
    m = left_join(m, m_h, by = "st")
    m = m %>% mutate(scaled = ztz * (m_h/(m_h - 1))) %>% select(scaled) %>% summarise(sum(scaled)) %>% compute(temporary = T) %>%
        pull()
    return(m)
}

c2f = function(x) {
    as.formula(paste("~", x, sep = ""))
}

db_columnAsCharacter = function(x, cols){

  checktype =  x %>% select(!!!syms(cols)) %>%
    head(1) %>% collect %>% lapply(type_sum) %>% unlist()

  numCols = checktype[checktype %in% c("int", "dbl")] %>% names()

  for(i in 1:length(numCols)){
    x = x %>% mutate(!!quo_name(numCols[1]) := as.character(!!sym(numCols[1])))
  }

  x
}

svydb_monet_sampleN = function(data, n) {
    q = paste("SELECT * FROM", data$ops$x, "SAMPLE", n)
    dbGetQuery(data$src$con, q)
}

as.data.frame.svydbstat = function(x) {
    ans = cbind(coef(x), SE(x))
    colnames(ans) = c(attr(x, "statistic"), "SE")
    ans
}

print.svydbstat = function(xx, ...) {
    v <- attr(xx, "var")
    m = cbind(xx, sqrt(v))
    colnames(m) = c(attr(xx, "statistic"), "SE")
    printCoefmat(m)
}

coef.svydbstat = function(object, ...) {
    attr(object, "statistic") = NULL
    attr(object, "name") = NULL
    attr(object, "var") = NULL
    unclass(object) %>% t() %>% as.vector()
}

SE.svydbstat = function(x, ...) {
    s = attr(x, "var") %>% sqrt()
    names(s) = attr(x, "name")
    return(s)
}

print.svydbrepstat =  function (xx, ...){
  if(is.list(xx)){
    xx = xx$svydbrepstat
  }
  v <- attr(xx, "var")
  m = cbind(xx, sqrt(v))
  colnames(m) = c(attr(xx, "statistic"), "SE")
  rownames(m) = attr(xx, "name")
  printCoefmat(m)
}

coef.svydbrepstat = function(object, ...) {
    if (is.list(object)) {
        object = object$svydbrepstat
    }
    attr(object, "statistic") = NULL
    attr(object, "name") = NULL
    attr(object, "var") = NULL
    unclass(object) %>% t() %>% as.vector()
}

SE.svydbrepstat = function(x, ...) {
    if (is.list(x)) {
        x = x$svydbrepstat
    }
    s = attr(x, "var") %>% sqrt()
    names(s) = attr(x, "name")
    return(s)
}

print.svydblm = function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    print(x$design)
    cat("\nSurvey design:\n")
    print(x$design$call)
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
    if (length(coef(x))) {
        cat("Coefficients:\n")
        print.default(format(coef(x), digits = digits), print.gap = 2L, quote = FALSE)
    } else cat("No coefficients\n")
    cat("\n")
    invisible(x)
}
summary.svydblm = function(object) {
    df.r = object$df.residual
    coef.p = coef(object)
    covmat = vcov(object)
    dimnames(covmat) = list(colnames(coef.p), colnames(coef.p))
    var.cf = diag(covmat)
    s.err = sqrt(var.cf)
    tvalue = coef.p/s.err
    dn = c("Estimate", "Std. Error")
    pvalue <- 2 * pt(-abs(tvalue), df.r)
    coef.table <- rbind(coef.p, t(as.matrix(s.err)), tvalue, pvalue) %>% t()
    dimnames(coef.table) <- list(colnames(coef.p), c(dn, "t value", "Pr(>|t|)"))
    ans = list(df.residual = df.r, coefficients = coef.table, cov.unscaled = covmat, cov.scaled = covmat, call = object$call,
        design = object$design)
    class(ans) <- c("summary.svydblm", "summary.glm")
    return(ans)
}
print.summary.svydblm = function(x, digits = max(3, getOption("digits") - 3), signif.stars = getOption("show.signif.stars"),
    ...) {
    cat("\nCall:\n")
    cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
    cat("Survey design:\n")
    print(x$design$call)
    cat("\nCoefficients:\n")
    coefs <- x$coefficients
    if (!is.null(aliased <- is.na(x$coefficients[, 1])) && any(aliased)) {
        cn <- names(aliased)
        coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn, colnames(coefs)))
        coefs[!aliased, ] <- x$coefficients
    }
    printCoefmat(coefs, digits = digits, signif.stars = signif.stars, na.print = "NA", ...)
    invisible(x)
}

predict.svydblm = function(object, newdata = NULL, ...) {
    tt = delete.response(object$terms)
    mf = model.frame(tt, data = newdata, xlev = object$design$levels)
    mm = model.matrix(tt, mf)
    eta = drop(mm %*% as.vector(coef(object)))
    attr(eta, "var") = drop(rowSums((mm %*% vcov(object)) * mm))
    attr(eta, "statistic") = "link"
    class(eta) <- "svydbstat"
    eta
}

vcov.svydblm = function(x, ...) {
    x$cov.unscaled
}

