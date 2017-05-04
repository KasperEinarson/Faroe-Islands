  # --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #
# vinddata for Husahagi
#vh <- read_excel("data/Vinddata_Husahagi.xlsx")
vh <- read_excel("data/Kopi af VindmÂling H˙sahagi aug - nov 2016.xlsx")

vh1 <- vh %>% select(1,ends_with(" avg"),-c(2)) 
vh <- vh1 %>% select(-c(10,11)) 

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
#vs <- read_excel("data/Vinddata_Suduroy.xlsx")
vs <- read_excel("data/Kopi af VindmÂling Porkeri aug - nov 2016.xlsx")

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

### -- Kasper 02-01-2017 ----------------------------

### ---------- Slet NA simultane steder Virker KUN fordi begge datasÊt har samme index(date):
Index1 <- which(is.na(vh_long$Value))
Index2 <- which(is.na(vs_long$Value))

Index.slet <- c(Index1,Index2)

vs_long1 <- vs_long[-Index.slet,]
vh_long1 <- vh_long[-Index.slet,]

# --------------------------------------------------------------------------- #
# sammenfat datafiler
w <- bind_rows(vs_long1, vh_long1)

w$Hojde <- as.numeric(w$Hojde)

w <- w %>% filter(Hojde != 52.1) # ikke til at stole p√•

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
