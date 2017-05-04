# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #

height = 6
width = 12
# --------------------------------------------------------------------------- #
## --
ggplot(w, aes(x = Tid, y = Value, color = Hojde)) + geom_line() + facet_grid(Var ~ ., scales = "free_y")
ggsave("figs/ts_ws_wd_hojde.png", height = height, width = width)

## --
ggplot(w, aes(x = Tid, y = Value, color = Hojde)) + geom_line() + facet_grid(Var ~ Site, scales = "free_y")
ggsave("figs/ts_ws_wd_hojde_site.png", height = height, width = width)

# --------------------------------------------------------------------------- #
ggplot(wmean, aes(x = Tid, y = Value, color = Site)) + geom_line() + facet_grid(Var ~ ., scales = "free_y")
ggsave("figs/ts_ws_wd_avg.png", height = height, width = width)

# --------------------------------------------------------------------------- #
# Dataanalyse - auto-korrelation

png("figs/acf.png", width = 10, height = 10, units = "cm", res = 300)
layout(matrix(c(1, 2, 3, 4, 5, 5), 3, 2, byrow = TRUE))
par(mar = c(2,2,1,1), oma = c(1,1,1,1))
acf(w2$Husahagi, lag.max = 15)
acf(w2$Suduroy, lag.max = 15)
pacf(w2$Husahagi, lag.max = 15)
pacf(w2$Suduroy, lag.max = 15)
ccf(w2$Husahagi, w2$Suduroy, lag.max = 30)
dev.off()

# --------------------------------------------------------------------------- #
ggplot(w_corall %>% filter(ws_niv == "Alle"), aes(x = reorder(factor(wd_niv), wd_num), y = cor)) + ylim(c(0, 1)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black", fill = "grey", alpha = 1) +
  labs(x = "Vindretning", y = "Korrelation") + scale_fill_manual(values = c("grey")) + 
  geom_text(aes(y = cor + .05, label = paste0(round(p*100), "%"), hjust = 0))


ggsave("figs/korrelation_vretning.png", width = 6, height = 5)

## --
ggplot(w_corall, aes(x = reorder(factor(wd_niv), wd_num), y = cor, fill = factor(ws_num))) + ylim(c(0, 1)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(fill = "Vindhastighed", x = "Vindretning", y = "Korrelation") +
  scale_fill_manual(values = c("grey","blue","purple","red"), labels = c("Alle","0-10 m/s", "10-20 m/s", "+20 m/s")) +
  geom_text(aes(y = cor+.02, label = paste0(round(p*100), "%"), hjust = 0.3, vjust = 0), position=position_dodge(width=0.9))


ggsave("figs/korrelation_vretning_vhastighed.png", width = 8, height = 5)



# --------------------------------------------------------------------------- #
ggplot(wp_corall %>% filter(ws_niv == "Alle"), aes(x = reorder(factor(wd_niv), wd_num), y = cor)) + ylim(c(0, 1)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black", fill = "grey", alpha = 1) +
  labs(x = "Vindretning", y = "Korrelation") + scale_fill_manual(values = c("grey")) + 
  geom_text(aes(y = cor + .05, label = paste0(round(p*100), "%"), hjust = 0))
ggsave("figs/korrelation_wp_vretning.png", width = 6, height = 5)

## --
ggplot(wp_corall, aes(x = reorder(factor(wd_niv), wd_num), y = cor, fill = factor(ws_num))) + ylim(c(NA, 1)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(fill = "Vindhastighed", x = "Vindretning", y = "Korrelation") +
  scale_fill_manual(values = c("grey","blue","purple","red"), labels = c("Alle","0-10 m/s", "10-20 m/s", "+20 m/s")) +
  geom_text(aes(y = cor+.02, label = paste0(round(p*100), "%"), hjust = 0.3, vjust = 0), position=position_dodge(width=0.9))
ggsave("figs/korrelation_wp_vretning_vhastighed.png", width = 8, height = 5)

# --------------------------------------------------------------------------- #
ggplot(wpr_corall %>% filter(ws_niv == "Alle"), aes(x = reorder(factor(wd_niv), wd_num), y = cor)) + ylim(c(0, 1)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black", fill = "grey", alpha = 1) +
  labs(x = "Vindretning", y = "Korrelation") + scale_fill_manual(values = c("grey")) + 
  geom_text(aes(y = cor + .05, label = paste0(round(p*100), "%"), hjust = 0))
ggsave("figs/korrelation_wpr_vretning.png", width = 6, height = 5)

## --
ggplot(wpr_corall, aes(x = reorder(factor(wd_niv), wd_num), y = cor, fill = factor(ws_num))) + ylim(c(NA, 1)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(fill = "Vindhastighed", x = "Vindretning", y = "Korrelation") +
  scale_fill_manual(values = c("grey","blue","purple","red"), labels = c("Alle","0-10 m/s", "10-20 m/s", "+20 m/s")) +
  geom_text(aes(y = cor+.02, label = paste0(round(p*100), "%"), hjust = 0.3, vjust = 0), position=position_dodge(width=0.9))
ggsave("figs/korrelation_wpr_vretning_vhastighed.png", width = 8, height = 5)

# --------------------------------------------------------------------------- #
ggplot(corall %>% filter(ws_niv == "Alle"), aes(x = reorder(factor(wd_niv), wd_num), y = cor, fill = factor(Data))) + ylim(c(0, 1)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 1) +
  labs(x = "Vindretning", y = "Korrelation", fill = "Data") + 
  scale_fill_manual(values = c(grey(.8), grey(.5), grey(.2)), labels = c("Vinddata", "Effektkurve 1", "Effektkurve 2"))# + 
ggsave("figs/korrelation_vhast_vpower.png", width = 7, height = 5)

## --
ggplot(corall %>% filter(ws_niv == "Alle"), aes(x = reorder(factor(wd_niv), wd_num), y = fejl, fill = factor(Data))) + 
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 1) +
  labs(x = "Vindretning", y = "Fejl (avg)", fill = "Data") + 
  scale_fill_manual(values = c(grey(.8), grey(.5), grey(.2)), labels = c("Vinddata", "Effektkurve 1", "Effektkurve 2")) +
  theme(legend.position = "none")
ggsave("figs/fejl_vhast_vpower.png", width = 6, height = 5)

##--
ggplot(corall %>% filter(ws_niv == "Alle"), aes(x = reorder(factor(wd_niv), wd_num), y = fejl_p, fill = factor(Data))) + 
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 1) +
  labs(x = "Vindretning", y = "Fejl (%)", fill = "Data") + 
  scale_fill_manual(values = c(grey(.8), grey(.5), grey(.2)), labels = c("Vinddata", "Effektkurve 1", "Effektkurve 2")) +
  theme(legend.position = "none")
ggsave("figs/fejl_p_vhast_vpower.png", width = 6, height = 5)

# --------------------------------------------------------------------------- #
# Plot effektkurver
## --
png("figs/powerkurve.png", width = 12, height = 10, units = "cm", res = 300)
par(mar = c(2,0,0,0) + .5, oma = rep(0, 4))
plot(y$ws, y$np, col = "grey", axes = F, ann = F, pch = "*")
xval <- seq(0, 40, 0.1)
yfit <- myfun(p = opar, x = xval, p0 = F)
lines(xval, yfit, col = "red", lwd = 2)
axis(1)
dev.off()

## --
png("figs/powerkurve1.png", width = 12, height = 10, units = "cm", res = 300)
par(mar = c(2,0,0,0) + .5, oma = rep(0, 4))
plot(y$ws, y$np, col = "grey", axes = F, ann = F, pch = "*")
axis(1)
lines(xval, yfit, col = "black")
apply(spypar, 1, function(op, xval) {
  yf <- myfun(p = op, x = xval, p0 = F)
  lines(xval, yf, col = "red", lwd = 2)
}, xval = xval)
dev.off()

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #
