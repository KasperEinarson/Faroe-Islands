# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #
# Import data for effektkurven (2014 data for Husahagi):
y <- as_data_frame(read.csv("data/pcfit.csv", dec = ",", sep = ";", header = T))
y <- y %>% group_by(tid) %>% mutate(wd_niv = assign_wd(x = wd), ws_num = assign_wd(x = wd, type = "num"))

cap <- 1#3 * .9 # installeret kapacitet på Husahagi

# --------------------------------------------------------------------------- #
# Optimal effektkurve for all data
p <- rep(.01, 3)
ohat <- optim(par = p, fn = optfun, y = y$np, fun = myfun, x = y$ws, p0 = F)
opar <- ohat$par

wp <- w2 %>% select(Tid, Husahagi, Suduroy, ws_num, wd_num) %>% 
  mutate(Husahagi = cap * myfun(p = opar, x = Husahagi, p0 = F), Suduroy = cap * myfun(p = opar, x = Suduroy, p0 = F),
         e = Husahagi - Suduroy, e_per = e/Husahagi)
wp <- wp %>% filter(Husahagi != 0)

wp_e <- myfejl(ungroup(wp))

# Korrelationsanalyse af effekten
wp_corall <- mycor(ungroup(wp))
wp_corall$Data <- 2
wp_corall <- left_join(wp_corall, wp_e %>% select(ws_num, wd_num, fejl = mae, fejl_p = mae_p), by = c("ws_num", "wd_num"))

# --------------------------------------------------------------------------- #
# Optimal effektkuver ift. vindretning
spy <- split(y, y$ws_num)
spypar <- t(sapply(spy, function(r) {
  p <- rep(0.01, 3)
  ohat <- optim(par = p, fn = optfun, y = r$np, fun = myfun, x = r$ws, p0 = F)
  ohat$par
}))

wpr <- w2 %>% select(Tid, Husahagi, Suduroy, ws_num, wd_num) %>% group_by(Tid) %>%
  mutate(Husahagi = cap * myout_ret(Husahagi, r = ws_num, mypar = spypar, p0 = F),
            Suduroy = cap * myout_ret(Suduroy, r = ws_num, mypar = spypar, p0 = F),
            e = Husahagi - Suduroy, e_per = e/Husahagi)
wpr <- wpr %>% filter(Husahagi != 0)

wpr_e <- myfejl(ungroup(wpr))
# Korrelationsanalyse af effekten
wpr_corall <- mycor(ungroup(wpr))
wpr_corall$Data <- 3
wpr_corall <- left_join(wpr_corall, wpr_e %>% select(ws_num, wd_num, fejl = mae, fejl_p = mae), by = c("ws_num", "wd_num"))

# --------------------------------------------------------------------------- #
# Samlet korrelation datafil
corall <- bind_rows(w_corall, wp_corall, wpr_corall)

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #