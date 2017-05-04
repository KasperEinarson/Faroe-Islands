# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# Convert data to applicable form ####
w1 <- wmean[!(wmean$Var == "Retning" & wmean$Site == "Suduroy"), ]
w1$Var[w1$Var == "Hastighed"] <- w1$Site[w1$Var == "Hastighed"]
w2 <- w1 %>% select(-Site) %>% spread(Var, Value) %>% group_by(Tid) %>%
  mutate(as_ws = assign_ws(c(Husahagi)), as_wd = assign_wd(Retning), 
         ws_num = assign_ws(Husahagi, type = "num"), wd_num = assign_wd(Retning, type = "num")) %>%
  mutate(e = Husahagi - Suduroy, e_per = e/Husahagi)


w2 <- w1 %>% select(-Site) %>% spread(Var, Value) %>% group_by(Tid) %>%
  mutate(as_ws = assign_ws(c(Husahagi)), as_wd = assign_wd(Retning), 
         ws_num = assign_ws(Husahagi, type = "num"), wd_num = assign_wd(Retning, type = "num")) %>%
  mutate(e = Husahagi - Suduroy, e_per = e/Husahagi)

w2 <- ungroup(w2[apply(w2, 1, function(r) all(!is.na(r))), ])
## --
rm(w1)

# --------------------------------------------------------------------------- #
# Analysis of the deviation between the wind speeds at the two Sites ####
w_e <- myfejl(w2)

# --------------------------------------------------------------------------- #
# Calculate correlation between wind directions and wind speed levels ####
w_corall <- mycor(w2)
w_corall$Data <- 1
w_corall <- left_join(w_corall, w_e %>% select(ws_num, wd_num, fejl = mae, fejl_p = mae_p), by = c("ws_num", "wd_num"))

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #
