## ------------------------------------------------------------------------- ##
splitdir <- function(x, spx, parter = NULL)
{
  mycut <- split(x, wdpart(spx, parter = parter))
  mycut[[1]] <- rbind(mycut[[1]], mycut[[length(mycut)]])
  mycut <- mycut[1:(length(mycut)-1)]
  names(mycut) <- getdirections(parter)
  mycut
}

## ------------------------------------------------------------------------- ##
wdpart <- function(x, parter = NULL, north = NULL)
{
  wdir <- getdirections(parter = parter)
  nparter <- length(wdir)
  if (any(x == 0)) x[x == 0] <- 360
  br <- c(0, (1:nparter) * 360/nparter - 360/nparter/2, 360)
  cut(x, breaks = br)#, labels = c(wdir, wdir[1]))
}

## ------------------------------------------------------------------------- ##
getdirections <- function(parter = NULL, newdir = NULL)
{ #, north = NULL
  require(addfuns)
  basedir <- c("N","E","S","W")
  if (is.null(parter)) parter <- 4
  if (is.null(newdir)) newdir <- basedir
  stopifnot(parter %% 4 == 0)
  
  if (length(newdir) >= parter) {
    return(newdir)
  } else {
    newdir0 <- newdir
    n0 <- odd(newdir0)
    (addir <- paste0(c(n0[1], rep(n0[-1], each = 2), n0[1]), rep(even(newdir0), each = 2)))
    newdir <- vector("character", length = length(newdir0) + length(addir))
    newdir[1:length(newdir) %% 2 == 1] <- newdir0
    newdir[1:length(newdir) %% 2 == 0] <- addir
    getdirections(parter = parter, newdir = newdir)
  }
}

## ------------------------------------------------------------------------- ##
assign_wd <- function(x, type = NULL) {
  if (is.na(x)) {
    return(NA)
  } else {
    stopifnot(is.numeric(x) & x >= 0 & x <= 360)
    if (is.null(type)) type = "char"
    if (type == "char") {
      if (x < 45 | x >= 315) return("N")
      else if (x >= 45 & x < 135) return("E")
      else if (x >= 135 & x < 225) return("S")
      else if (x >= 225 & x < 315) return("W")
      else stop("No solution!")
    } else if (type == "num") {
      if (x < 45 | x >= 315) return(1)
      else if (x >= 45 & x < 135) return(2)
      else if (x >= 135 & x < 225) return(3)
      else if (x >= 225 & x < 315) return(4)
      else stop("No solution!")
    } else stop("Input \"x\" has to be either of type \"char\" or \"num\"!")
  }
}

assign_ws <- function(x, type = NULL) {
  if (is.na(x)) {
    return(NA)
  } else {
    stopifnot(is.numeric(x) & x >= 0)
    if(is.null(type)) type = "char"
    if(type == "char") {
      if (x < 10) return("L")
      else if(x >= 10 & x < 20) return("M")
      else if(x >= 20) return("H")
      else stop("No solution!")
    } else if (type == "num") {
      if (x < 10) return(1)
      else if(x >= 10 & x < 20) return(2)
      else if(x >= 20) return(3)
      else stop("No solution!")
    } else stop("Input \"x\" has to be either of type \"char\" or \"num\"!")
  }
}

assign_var <- function(x, var_type, ...) {
  var_type = tolower(var_type)
  if(var_type == "hastighed") return(assign_ws(x, ...))
  else if(var_type == "retning") return(assign_wd(x, ...))
  else return(0)
}

## ------------------------------------------------------------------------- ##
wddiff <- function(x) {
  stopifnot(is.numeric(x) & length(x) > 1)
  a <- x[1] - x[-1]
  b <- x[1] - (x[-1] + 360)
  r <- matrix(c(a, b), ncol = 2, byrow = F)
  apply(r, 1, function(s) s[which.min(abs(s))])
}

wdmean <- function(x) {
  z <- round(mean(c(x[1], x[1] - wddiff(x)/2)), 1)
  return(if(z <= 0) z + 360 else if(z > 360) z - 360 else z)
}

## ------------------------------------------------------------------------- ##
diff_wd <- function(x, y) {
  if(is.na(x) | is.na(y)) {
    return(NA)
  } else {
    a <- x - y
    b <- x - (y + 360)
    r <- c(a, b)
    return(r[which.min(abs(r))])  
  }
}

## ------------------------------------------------------------------------- ##
mycor <- function(wp) {
  cor0 <- wp %>% summarize(ws_num = 0, wd_num = 0, p = 1, cor = cor(Husahagi, Suduroy, use = "pairwise.complete.obs"))
  cor1d <- wp %>% group_by(wd_num) %>% summarize(n = n(), cor = cor(Husahagi, Suduroy, use = "pairwise.complete.obs")) %>% 
    mutate(N = sum(n), p = n/N, ws_num = 0) %>% select(ws_num, wd_num, p, cor)
  cor1s <- wp %>% group_by(ws_num) %>% summarize(n = n(), cor = cor(Husahagi, Suduroy, use = "pairwise.complete.obs")) %>%
    mutate(N = sum(n), p = n/N, wd_num = 0) %>% select(ws_num, wd_num, p, cor)
  cor2 <- wp %>% group_by(ws_num, wd_num) %>% summarize(n = n(), cor = cor(Husahagi, Suduroy, use = "pairwise.complete.obs")) %>%
    mutate(N = sum(n), p = n/N) %>% select(ws_num, wd_num, p, cor)
  corall <- bind_rows(cor0, cor1d, cor1s, cor2)
  corall$ws_niv <- corall$wd_niv <- NA
  for(i in 1:nrow(corall)) {
    corall$ws_niv[i] <- hlabel(corall$ws_num[i])
    corall$wd_niv[i] <- rlabel(corall$wd_num[i])
  }
  return(corall)
}

myfejl <- function(wp) {
  e0 <- wp %>% summarize(ws_num = 0, wd_num = 0, mae = mae(e), mae_p = mae(e_per)) %>% select(ws_num, wd_num, mae, mae_p)
  e1s <- wp %>% group_by(ws_num) %>% summarize(wd_num = 0, mae = mae(e), mae_p = mae(e_per)) %>% select(ws_num, wd_num, mae, mae_p)
  e1d <- wp %>% group_by(wd_num) %>% summarize(ws_num = 0, mae = mae(e), mae_p = mae(e_per)) %>% select(ws_num, wd_num, mae, mae_p)
  e2 <- wp %>% group_by(ws_num, wd_num) %>% summarize(mae = mae(e), mae_p = mae(e_per)) %>% select(ws_num, wd_num, mae, mae_p)
  eall <- bind_rows(e0, e1d, e1s, e2)
}

## ------------------------------------------------------------------------- ##
rlabel <- function(x) {
  if (x == 1) return("N")
  else if (x == 2) return("Ø")
  else if (x == 3) return("S")
  else if (x == 4) return("V")
  else return("Alle")
}

hlabel <- function(x) {
  if (x == 1) return("L")
  else if (x == 2) return("M")
  else if (x == 3) return("H")
  else return("Alle")
}

## ------------------------------------------------------------------------- ##
# Beregner mean absolute error mellem x og y (hvis kun x er defineret som input anses x for at være fejlen)
mae <- function(x, y = NULL) {
  stopifnot(is.vector(x))
  if (!is.null(y)) {
    stopifnot(is.vector(y) & length(y) == length(x))
    x <- x - y
  }
  sum(abs(x))/length(x)
}

## ------------------------------------------------------------------------- ##
# Funktioner til effektkurveberegning
ssquares <- function(x, y = NULL, na.action = na.rm) {
  stopifnot(is.numeric(x))
  if(!is.null(y)) {
    stopifnot(is.numeric(y) & length(y) == length(x))
    x <- x - y
  }
  x <- x[!is.na(x)]
  x %*% x
}

optfun <- function(p, y, fun, ...) {
  yhat <- myfun(p, ...)
  ssquares(x = y, y = yhat)
}

myfun <- function(p, x, p0 = T) {
  y <- if (p0) p[1] + p[2] * x + p[3] * x^2 + p[4] * x^3 else
    p[1] * x + p[2] * x^2 + p[3] * x^3
  return(apply(cbind(y, 0), 1, max, na.rm = T))
}

## --
myout_ret <- function(x, r, mypar, p0 = T) {
  myfun(p = mypar[r, ], x = x, p0 = p0)
}


## -- 

print_pdf <- function(maxrow = maxrow,Dataset = Dataset,pdf.name = pdf.name){
  npages = ceiling(nrow(Dataset)/maxrow)
  pdf(pdf.name, height=15, width=10); for (i in 1:npages) {idx = seq(1+((i-1)*maxrow), i*maxrow)
  grid.newpage()
  grid.table(Dataset[idx, ])}
  dev.off()
  
}

## --

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

## ------------------------------------------------------------------------- ##
## ------------------------------------------------------------------------- ##
