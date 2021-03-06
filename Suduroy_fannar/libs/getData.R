  # --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #
# vinddata for Husahagi
vh <- read_excel("data/Vinddata_Husahagi.xlsx")


ws.vh <- vh[, c(1:7)]
hojde <- c(104.3, 104, 70.6, 70.2, 46.1, 45.8)
names(ws.vh) <- c("Tid", hojde)
ws.vh$Site <- "Husahagi"
ws.vh$Var <- "Hastighed"
ws.vh_long <- ws.vh %>% gather(Hojde, Value, -Tid, -Site, -Var)

wd.vh <- vh[, c(1, 8:9)]
hojde <- c(71.1, 38)
names(wd.vh) <- c("Tid", hojde)
wd.vh$Site <- "Husahagi"
wd.vh$Var <- "Retning"
wd.vh_long <- wd.vh %>% gather(Hojde, Value, -Tid, -Site, -Var)

vh_long <- bind_rows(ws.vh_long, wd.vh_long)


# --------------------------------------------------------------------------- #
# vinddata for Suduroy
vs <- read_excel("data/Vinddata_Suduroy.xlsx")
#vs <- read_excel("data/Kopi af Vindm�ling Porkeri aug - nov 2016.xlsx")

View(vs)

vs <- vs[, c(1, grep("avg", names(vs)))]
vs <- vs[, -grep(":0", names(vs))]
vs <- vs[, -grep(":9", names(vs))]
vs <- vs[, -grep(":13", names(vs))]


ws.vs <- vs[, c(1:7)]
hojde <- c(76.50, 76.51, 72.3, 71.5, 52.8, 52.1)
names(ws.vs) <- c("Tid", hojde)
ws.vs$Site <- "Suduroy"

ws.vs$Var <- "Hastighed"
ws.vs_long <- ws.vs %>% gather(Hojde, Value, -Tid, -Site, -Var)

wd.vs <- vs[, c(1, 8:9)]
hojde <- c(69.3, 48.3)
names(wd.vs) <- c("Tid", hojde)
wd.vs$Site <- "Suduroy"
wd.vs$Var <- "Retning"
wd.vs_long <- wd.vs %>% gather(Hojde, Value, -Tid, -Site, -Var)
vs_long <- bind_rows(ws.vs_long, wd.vs_long)

# --------------------------------------------------------------------------- #
# sammenfat datafiler
w <- bind_rows(vs_long, vh_long)

w$Hojde <- as.numeric(w$Hojde)

w <- w %>% filter(Hojde != 52.1) # ikke til at stole på

# --------------------------------------------------------------------------- #

## Gennemsnitsserier for hastighed og retning per site
wd <- w %>% filter(Var == "Retning") %>% select(-Hojde) %>% group_by(Site, Var, Tid) %>% 
  summarize(mean = wdmean(Value)) 


wdd <- wd %>% spread(Site, mean) %>% group_by(Var, Tid) %>% 
  mutate(Suduroy = Husahagi - diff_wd(Husahagi, Suduroy)) %>% gather(Site, mean, -Var, -Tid)
ws <- w %>% filter(Var == "Hastighed" & Hojde < 100) %>% select(-Hojde) %>%
  group_by(Site, Var, Tid) %>% summarize(mean = mean(Value, na.rm = T))
wmean <- ungroup(bind_rows(ws, wd) %>% rename(Value = mean))
wmean_dum <- ungroup(bind_rows(ws, wdd) %>% rename(Value = mean)) # mindste afstand for vindretning mellem sites

rm(wd, ws, wdd)

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #
