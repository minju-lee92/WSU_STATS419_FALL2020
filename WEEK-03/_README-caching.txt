new.denzel = denzel$movies.50
new.denzel$nmid = denzel$nmid
new.denzel$name = denzel$name
new.denzel$countfilms = denzel$countfilms$totalcount
new.denzel = new.denzel [,c(12,13,14, 1:11)]

df.will.denzel = rbind(new.will, new.denzel)

write.table(df.will.denzel, file = "WEEK-03/output/df-will-vs-denzel.txt", sep= "|", col.names=T, row.names = F)

df.will.denzel2  = read.csv("WEEK-03/output/df-will-vs-denzel.txt", sep= "|", header=T)
 
# if (cache) ...df...
# else ... above
# ... will = grabFilmsForPerson(nmid);
# if (personcache) ...
# my.source = "local" ... grab and