makesvydbdesign <- R6Class("svydb.design",
                           public = list(
                             dataOg = NULL,
                             data = NULL,
                             dataSub = NULL,
                             vars = NULL,
                             st = NULL,
                             id = NULL,
                             wt = NULL,
                             call = NULL,
                             names = list(),
                             levels = list(),
                             initialize = function(vars = NA, st = NA,
                                                   id = NA, wt = NA, data){
                               if(quo_is_null(wt)){
                                 stop("Please provide sampling weights")
                               }else{
                                 self$wt = as.character(wt)[2]
                               }

                               if(quo_is_null(st)){
                                 data = data %>% mutate(st = 1)
                                 self$st = "st"
                               }else{
                                 self$st = as.character(st)[2]
                               }

                               if(quo_is_null(id)){
                                 data = data %>% mutate(id = row_number())
                                 self$id = "id"
                               }else{
                                 self$id = as.character(id)[2]
                               }
                               self$data = data %>% select(everything())
                               self$dataOg <<- self$data
                             },
                             setx = function(x){
                               tc = tryCatch(class(x), error = function(e) e)

                               if("formula" %in% tc){
                                 x = all.vars(x)
                                 self$data <<- self$data %>%
                                   select(!!x, self$st, self$id, self$wt) %>%
                                   filter_all(any_vars(!is.na(.)))
                                 self$vars <<- x
                               }else{
                                 x = enquo(x)
                                 self$data <<- self$data %>%
                                   select(!!x, self$st, self$id, self$wt) %>%
                                   filter(!is.na(!!x))
                                 self$vars <<- as.character(x)[2]
                               }
                               self$names[["logged"]] = c(self$st, self$id, self$wt, "m_h")
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
                             storelevel = function(x, name){
                               ll = list(x)
                               names(ll) = name
                               self$levels = c(self$levels, ll)
                             },
                             storecall = function(x){
                               self$call = x
                             },
                             print = function(){
                               nid = self$getmh() %>% pull(m_h) %>% sum()
                               rows = self$data %>% db_nrow()
                               txt = sprintf("svydb.design, %s observation(s), %s Clusters\n", rows, nid)
                               cat(txt)
                             }
                           )
)

#' Survey design
#'
#' @param st Column name specifying the strata column. \code{NULL} for no strata.
#' @param id Column name specifying the cluster column. \code{NULL} for no cluster.
#' @param wt Column name specifying the sampling weights column.
#' @param data A data frame or sql table of the survey data set..
#' @description
#' Gathers all information that are needed to compute survey statistics
#' into a design.
#' @examples
#' data(nhane)
#' nh.dbsurv = svydbdesign(st = SDMVSTRA , wt = WTMEC2YR,id = SDMVPSU , data = nhane)
#' nh.dbsurv$subset(Race3 == 3)
#' nh.dbsurv$getmh()
#' nh.dbsurv$getwt()
#' nh.dbsurv$clone()
#' # OR with a database connection
#' # require(MonetDBLite)
#' # require(DBI)
#' # require(dbplyr)
#' # con = dbConnect(MonetDBLite())
#' # dbWriteTable(con, "nhane", nhane)
#' # nhane.db = tbl(con, "nhane")
#' @author Charco Hui
#' @seealso
#' \code{\link{svydbtotal}}, \code{\link{svydbmean}}
#'
svydbdesign = function(st = NULL, id = NULL, wt = NULL, data){
  st = enquo(st)
  id = enquo(id)
  wt = enquo(wt)

  d = makesvydbdesign$new(st = st, id = id, wt = wt, data = data)
  d$storecall(match.call())
  d
}
