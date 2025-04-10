## Function for computing useful descriptive statistics from a vector
## Handles count / continuous data, including factors with multiple levels

rowStat <- function(x, case = NULL, varname = NULL,as.df = FALSE, ...) {
  dd <- tibble(
    varname = character(),
    n       = integer(),
    perc    = numeric(),
    M       = numeric(),
    Med     = numeric(),
    SD      = numeric(),
    min     = numeric(),
    max     = numeric()
  )
  N <- length(x)
  unique.x <- unique(x)
  type <- ""
  d <- NULL
  
  if (is.null(varname) & !is.null(case)){
    varname <- as.character(case)
  }
  
  if (is.null(varname) & is.null(case)) {
    varname <- "var"
  }
  
  if (length(unique.x) == 2) {
    type <- "binary"
    if (is.null(case)) {
      case <- unique(x)[1]
      warning(sprintf("data has two levels but case is not defined; setting case to %s", case))
    }
    if (!case %in% unique(x)) {
      stop("Case not represented in data; check case argument?")
    }
    d <- tibble(
      varname= varname,
      n    = sum(x == case),
      perc = n/N * 100
    )
  } else if (is.character(x) | is.factor(x)) {
    type <- "factor"
    levs <- unique(x) |> as.character() |> na.omit()
    d <- map(levs, \(u) {
      tibble(
        varname = u,
        n = sum(x == u, na.rm = TRUE),
        perc = n / N * 100
      ) 
    }) %>% bind_rows()
  } else if (is.numeric(x)){
    type <- "numeric"
    ## Treat as continuous --> stat 'em
    d <- tibble(
      varname = varname,
      n   = sum(!is.na(x)),
      perc= n / N,
      M   = mean(x, na.rm=TRUE),
      Med = median(x, na.rm=TRUE),
      SD  = sd(x, na.rm=TRUE),
      min = min(x, na.rm=TRUE),
      max = max(x,na.rm=TRUE)
    )
  } else {
    stop('data type not supported')
  }
  
  if(is.null(d)) {
    stop(sprintf("input type (%s) with %d levels not supported",class(x),unique(x)))
  }
  
  ret <- bind_rows(dd, d)
  
  if(as.df == TRUE) {
    return(ret)
  } else {
    return(list("N" = N, "Type" = type, "Stats" = ret))
  }
}
