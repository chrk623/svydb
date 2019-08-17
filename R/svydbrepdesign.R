makesvydbrepdesign <- R6Class("svydb.repdesign",
                              public = list(
                                dataOg = NULL,
                                data = NULL,
                                vars = NULL,
                                st = NULL,
                                id = NULL,
                                wt = NULL,
                                repwt = NULL,
                                scale = NULL,
                                names = list(
                                ),
                                initialize = function(vars = NA, st = NA, id = NA,
                                                      wt = NA, repwt = NULL, scale, data){

                                  if(quo_is_null(wt)){
                                    stop("Please provide sampling weights")
                                  }else{
                                    self$wt = as_label(wt)
                                  }

                                  if(is.null(repwt)){
                                    stop("Please provide replicate weights")
                                  }else{
                                    self$repwt = grep(pattern = repwt, colnames(data), value = T)
                                  }

                                  if(quo_is_null(st)){
                                    data = data %>% mutate(st = 1)
                                    self$st = "st"
                                  }else{
                                    self$st = as_label(st)
                                  }

                                  if(quo_is_null(id)){
                                    data = data %>% mutate(id = row_number())
                                    self$id = "id"
                                  }else{
                                    self$id = as_label(id)
                                  }
                                  self$scale = scale
                                  self$data = data %>% select(everything())
                                  self$dataOg <<- self$data
                                },
                                setx = function(x){
                                  tc = tryCatch(class(x), error = function(e) e)

                                  if("formula" %in% tc){
                                    x = all.vars(x)
                                    self$data <<- self$data %>%
                                      select(!!x, st = self$st, id = self$id, self$wt, self$repwt) %>%
                                      filter_all(any_vars(!is.na(.)))
                                    self$vars <<- x
                                  }else{
                                    x = enquo(x)
                                    self$data <<- self$data %>%
                                     select(!!x, st = self$st, id = self$id, self$wt, self$repwt) %>%
                                      filter(!is.na(!!x))
                                    self$vars <<- as.character(x)
                                  }
                                  self$names[["logged"]] = c(self$st, self$id, self$wt, self$repwt, "m_h")
                                },
                                addx = function(x){
                                  l = enquo(x)
                                  r = syms(colnames(self$data))
                                  self$data =  self$dataOg %>%
                                    select(!!l, !!!r)
                                },
                                getwt = function(){
                                  self$data %>% select(self$wt) %>% summarise_all(sum) %>%
                                    pull()
                                },
                                getmh = function(){
                                  self$data %>% group_by(!!sym(self$st)) %>%
                                    summarise(m_h = n_distinct(!!sym(self$id)))
                                },
                                subset = function(..., logical = T){
                                  d = self$clone()
                                  if(logical == T){
                                    d$data = d$data  %>% filter(...)
                                  }else{
                                    d$data = d$data  %>% filter(!!parse_expr(...))
                                  }
                                  return(d)
                                },
                                subset_rows = function(from, to){
                                  self$dataSub = self$data %>% db_selectRows(., from = from, to = to)
                                },
                                storename = function(name, obj, force = FALSE){
                                  if(force == TRUE){
                                    self$names$logged =
                                      self$names$logged[-which(self$names$logged %in% obj)]
                                  }
                                  if(!all(obj %in% self$names$logged)){
                                    new = setdiff(obj, self$names$logged)
                                    self$names[[name]] = c(new)
                                    self$names$logged = c(self$names$logged, new)
                                  }
                                },
                                removename = function(name, obj){
                                  self$names$logged =
                                    self$names$logged[-which(self$names$logged %in% obj)]
                                  self$names[[name]] =
                                    (self$names[[name]])[-which(self$names[[name]] %in% obj)]
                                },
                                print = function(){
                                  rows = self$data %>% db_nrow()
                                  txt = sprintf("svydb.repdesign, %s observation(s), %s sets of replicate weights, scale = %s", rows, length(self$repwt), self$scale)
                                  cat(txt)
                                }
                              )
)
#' Survey replicate design
#'
#' @param st Column name specifying the strata column. \code{NULL} for no strata.
#' @param id Column name specifying the cluster column. \code{NULL} for no cluster.
#' @param wt Column name specifying the sampling weights column.
#' @param repwt Regular expressions that matches the columns of the replicate weights.
#' @param scale Scaling constant for variance.
#' @param data A data frame or sql table of the survey data set.
#' @description
#' Gathers all information that are needed to compute survey statistics
#' into a design. Currently, only \code{\link{svydbreptotal}}, \code{\link{svydbrepmean}},
#' are available for replicate statistics.
#' @examples
#' data(ss16hde)
#' hde.dbrepsurv = svydbrepdesign(wt = WGTP, repwt="wgtp[0-9]+", scale = 4/80, data = ss16hde)
#' hde.dbrepsurv$subset(BATH == 1)
#' hde.dbrepsurv$clone()
#' # OR with a database connection
#' # library(MonetDBLite)
#' # library(DBI)
#' # library(dbplyr)
#' # con = dbConnect(MonetDBLite())
#' # dbWriteTable(con, "ss16hde", ss16hde)
#' # ss16hde.db = tbl(con, "ss16hde")
#' # hde.dbrepsurv = svydbrepdesign(wt = WGTP, repwt="wgtp[0-9]+", scale = 4/80, data = ss16hde.db)
#' @author Charco Hui
#' @export
svydbrepdesign = function(st = NULL, id = NULL, wt = NULL, repwt = NULL, scale, data){
  st = enquo(st)
  id = enquo(id)
  wt = enquo(wt)

  d = makesvydbrepdesign$new(st = st, id = id, wt = wt,
                             repwt = repwt, scale = scale, data = data)
  d
}
