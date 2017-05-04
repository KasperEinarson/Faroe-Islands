## Kør get_data og få "w"

#Tjek NAs
na.table <- w %>% group_by(Site,Var,Hojde) %>% summarise(Antal.NA = sum(is.na(Value))) 
print_pdf(maxrow = 15,Dataset = na.table,pdf.name = "hej.pdf")

 


# Plot tidsserier:
Sud_has <- w %>% filter(Site == "Suduroy",Var == "Hastighed")
Sud_ret <- w %>% filter(Site == "Suduroy",Var == "Retning")
Hus_has <- w%>% filter(Site == "Husahagi",Var == "Hastighed")
Hus_ret <- w%>% filter(Site == "Husahagi",Var == "Retning")

Hus_has_op <-  w %>% filter(Site == "Husahagi",Var == "Hastighed", Value < 500,!(is.na(Value)))


ggplot(data = Hus_ret,aes(x=Tid,y = Value,colour = as.factor(Hojde))) + geom_point() + ggtitle("Vindretning Husahagi") + labs(colour = "Hojde")   
ggplot(data = Hus_has_op,aes(x=Tid,y = Value,colour = as.factor(Hojde))) + geom_point() + ggtitle("Vindhastighed rå data Husahagi") + labs(colour = "Hojde")   




ggplot(data = Hus_has,aes(x=Tid,y = Value,colour = as.factor(Hojde))) + geom_point() + ggtitle("Vindhastighed rå data Husahagi") + labs(colour = "Hojde")   




wdt <- w %>% filter(Var == "Hastighed",!is.na(Value),Value < 500)
ggplot(data = wdt,aes(x=Tid,y = Value)) + geom_point() + ggtitle("Vindhastighed Sorteret data")

summary(w)

unique(diff(wdt$Tid))



