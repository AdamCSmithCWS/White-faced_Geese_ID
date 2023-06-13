library(tidyverse)



df <- read.csv("data/Copy of 2015 SCS CAGO tail measurements (sent to Adam) updated Nov 2017.csv",
               as.is = T)
hdrs <- read.csv("data/headers.csv",as.is = T)
df <- df[,which(hdrs$short.name != "")]
names(df) <- hdrs[which(hdrs$short.name != ""),"short.name"]


# rs <- rs[,which(hdrs$short.name != "")]
# names(rs) <- hdrs[which(hdrs$short.name != ""),"short.name"]
nhssample <- which(df$type == "NHS Barcoded")

rs <- df[nhssample,]

rs <- rs[-which(rs$species == "Branta leucopsis"),]
rs <- rs[-which(rs$species == "BOLD No result"),]
rs <- rs[-which(rs$age == ""),]


moltl <- which(rs$lrcode %in% c("","4A"))

moltc <- which(rs$crcode %in% c(""," 4A ","   "))
moltml <- which(rs$mlrcode %in% c("","4A"," "))

rs[-moltl,"lrl"] <- NA
rs[-moltl,"lrd"] <- NA

rs[-moltc,"crl"] <- NA
rs[-moltc,"crd"] <- NA

rs[-moltml,"mlrl"] <- NA
rs[-moltml,"mlrd"] <- NA




rs$datet <- strptime(rs$date,
                     format = "%d-%b-%y")




rs$species <- factor(rs$species)
rs$age <- factor(rs$age,levels = c("Immature","Adult"))
rs$prov <- factor(rs$prov,
                  levels = c("British Columbia",
                             "Alberta",
                             "Saskatchewan",
                             "Manitoba"))

## removing a few odd measures
rs[which(rs$crd < 0.05 | rs$crd > 0.175), "crd"] <- NA
rs[which(rs$mlrd < 0.05), "mlrd"] <- NA

rs$dhunt <- rs$datet$yday

############### aligning the full sample with teh rs df
df <- df[-which(df$species == "Branta leucopsis"),]
df <- df[-which(df$species == "BOLD No result"),]
df <- df[-which(df$species == ""),]
df <- df[-which(df$age == ""),]



df$datet <- strptime(df$date,
                     format = "%d-%b-%y")




df$species <- factor(df$species)
df$age <- factor(df$age,levels = c("Immature","Adult"))
df$prov <- factor(df$prov,
                  levels = c("British Columbia",
                             "Alberta",
                             "Saskatchewan",
                             "Manitoba"))

## removing a few odd measures
df[which(df$crd < 0.05 | df$crd > 0.175), "crd"] <- NA
df[which(df$mlrd < 0.05), "mlrd"] <- NA
df$dhunt <- df$datet$yday

dfjm <- df[which(df$type == "Leafloor collection- barcoded"),]



###################################### data from previous analysis

db1 <- read.csv(paste0("data/SCS CAGO tail measurements complete dataset (Sept 2011).csv"))
db2 <- read.csv(paste0("data/SCS_CAGO_bcrs.txt"))
db <- merge(db1,db2[,c("ID_seq_num","bcr")],by.x = "ID.seq.number", by.y = "ID_seq_num")
db <- db[which(db$bcr == "ERockies"),]
db <- db[-which(db$dateshot < 900),]
db <- db[which(db$race.tree != ""),]
db <- db[-which(db$Central.Rectrix.Length < 100),]
db <- db[-which(db$Central.Rectrix..Quill.Dia > 0.15 | db$Central.Rectrix..Quill.Dia < 0.075),]
db <- db[-which(db$Longest.Rectrix.Unfeathered.Quill..Length < 20),]
db <- db[-which(db$Central.Rectrix.Unfeathered.Quill..Length < 18 | db$Most.lateral.Rectrix.Unfeathered.Quill.Length > 33),]
dba <- db[which(db$age == "A"),]
dbi <- db[which(db$age == "I"),]
dbu <- db[which(db$age == "U"),]



vars <- c("ID.seq.number","age","race.tree",
          "dateshot",
          "Central.Rectrix.Length",
          "Central.Rectrix..Quill.Dia",
          "Longest.Rectrix.Length",
          "Longest.Rectrix.Quill.Dia",
          "Most.lateral.Rectrix.Length",
          "Most.lateral.Rectrix.Quill.Dia",
          "prov.kill",
          "longitude")    #,

#db[,"race.tree"] <- as.character(db[,"race.tree"])
#db[which(db[,"race.tree"] == "Branta canadensis"),"race.tree"] <- "CANG"
#db[which(db[,"race.tree"] == "Branta hutchinsii"),"race.tree"] <- "CACK"
#

dati <- dbi[which(dbi$race.tree != ""),vars]
dati[,"race.tree"] <- factor(dati[,"race.tree"],exclude = "")
dati <- na.omit(dati)

dat.a <- dba[which(dba$race.tree != ""),vars]
dat.a[,"race.tree"] <- factor(dat.a[,"race.tree"],exclude = "")
dat.a <- na.omit(dat.a)
dat <- db[which(db$race.tree != ""),vars]
dat[,"race.tree"] <- factor(dat[,"race.tree"],exclude = "")
dat <- na.omit(dat)

orig <- rbind(dat.a,dati)
names(orig) <- c("ID","age","species",
                 "date",
                 "crl",
                 "crd",
                 "lrl",
                 "lrd",
                 "mlrl",
                 "mlrd",
                 "prov",
                 "long")

orig$age <- as.character(orig$age)
orig[which(orig$age == "I"),"age"] <- "Immature"
orig[which(orig$age == "A"),"age"] <- "Adult"
orig$age <- factor(orig$age,levels = c("Immature","Adult"))

orig$ID <- as.character(orig$ID)

orig$prov <- as.character(orig$prov)
orig[which(orig$prov == "AB"),"prov"] <- "Alberta"
orig[which(orig$prov == "SK"),"prov"] <- "Saskatchewan"
orig[which(orig$prov == "MB"),"prov"] <- "Manitoba"
orig[which(orig$prov == "BC"),"prov"] <- "British Columbia"
orig$prov <- factor(orig$prov, levels = levels(df$prov))

orig$longd <- -1*floor(orig$long/100)
orig$longdd <- ((((orig$long/100)-(orig$longd*-1))*100)/60)*-1
orig$long <- orig$longd + orig$longdd

orig$mnth <- floor(orig$date/100)
orig$day <- orig$date-(orig$mnth*100)
orig$date <- paste(orig$day,orig$mnth,sep = "-")
orig$datet <- strptime(orig$date,
                       format = "%d-%m")

orig$dhunt <- orig$datet$yday



############## exclude BC

orig <- orig[-which(orig$prov == "British Columbia"),]
rs <- rs[-which(rs$prov == "British Columbia"),]


############## main datasets are:
## orig - the data from the original 2011 study
### non-random sample of small geese, plus some obviously large geese
saveRDS(orig,"data/original_data_2011_study.rds")

## dfjm - the data from Jim Leafloor, collected in Manitoba only
saveRDS(dfjm,"data/MB_data_JimLeafloor.rds")

## rs - the full sample of tails collected during the 2015 parts survey
saveRDS(rs,"data/all_tail_measures_2015.rds")


