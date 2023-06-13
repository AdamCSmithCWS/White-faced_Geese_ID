##### CANG CACK identification from feather measures
library(tidyverse)
library(randomForest)
library(boot)


rs <- readRDS("data/all_tail_measures_2015.rds")
dfjm <- readRDS("data/MB_data_JimLeafloor.rds")
orig <- readRDS("data/original_data_2011_study.rds")



### ### ### ### ### ### ### ### ### ### 
### ### logistic-regression

err.pl <- function(w = 1,
                   descr = "",
                   cola = c("red","blue"),
                   predcl = "predsp.m1",
                   dt = rs1,
                   mod = m1){
  
  confm <- table(dt[,c("species",predcl,"age","prov")])
  confmpsp <- prop.table(table(rs[,c("species","age","prov")]),
                         margin = 1) #p by species
  confmp <- prop.table(confm,
                       margin = c(1,3,4))#p by age and prov
  names(cola) <- c("Branta canadensis","Branta hutchinsii")
 
  for(a in c("Adult","Immature")){
    
    plot(x = 99,
         y = 99,
         xlim = c(0.5,9.5),
         ylim = c(0,1),
         xlab = "",
         xaxt = "n",
         ylab = paste("proportion correct"),
         #yaxt = "n",
         bty = "l",
         main = a,
         yaxs = "i"
    )
    if(a == "Adult" & descr == ""){

      mtext(side = 1,
            at = 4.5,
            line = 2.3,
            paste(paste(c(formula(mod)[[2]],
                    formula(mod)[[1]],
                    formula(mod)[[3]]),collapse = ""),
                  descr))
    }
    if(a == "Adult" & descr!= ""){
      mtext(side = 1,
            at = 4.5,
            line = 2.3,
            descr)
      
    }

    if(a == "Adult" & descr == "Leafloor"){
      legend("topleft",
           legend = names(cola),
           fill = cola,
           bty = "n",
           border = NA)
    }
    pj <- 1
    for(p in c("Alberta","Saskatchewan","Manitoba")){
      
      for(spt in c("Branta canadensis","Branta hutchinsii")){
        
        pc <- confmp[spt,spt,a,p]
        pw <- confmpsp[spt,a,p]
        polygon(y = c(0,
                      pc,
                      pc,
                      0),
                x = rep(c(pj-(w*pw),pj+(w*pw)),each = 2),
                border = NA,
                col = cola[spt])
        
        pj <- pj+1
      }#a
      if(a != "Adult"){
        mtext(side = 1,
            at = pj-1.5,
            line = 1.2,
            p)}
      pj <- pj+1
    }#p
  }#spt
  
  
}#end plotting funtion


################# testing all feather measures
var.mod <- c("species","crl","lrl","mlrl","age","dhunt","prov","crd","lrd","mlrd")
rstmp <- na.omit(rs[,var.mod])

mt1 <- glm(species ~ prov + lrl*age + lrd + dhunt*prov,
           data = rstmp,
           family = binomial)


mt2 <- glm(species ~ prov + crl*age + crd + dhunt*prov,
           data = rstmp,
           family = binomial)

mt3 <- glm(species ~ prov + mlrl*age + mlrd + dhunt*prov,
           data = rstmp,
           family = binomial)

# > summary(mt1)$aic
# [1] 204.443
# > summary(mt2)$aic
# [1] 215.3269
# > summary(mt3)$aic
# [1] 210.4609
# > 
# cvt1 <- cv.glm(data = rstmp,
#           glmfit = mt1)
# cvt2 <- cv.glm(data = rstmp,
#                glmfit = mt2)
# cvt3 <- cv.glm(data = rstmp,
#                glmfit = mt3)
#  cvt1$delta
# # [1] 0.04542831 0.04542445
#  cvt2$delta
# # [1] 0.04939468 0.04939054
#  cvt3$delta
# # [1] 0.04729543 0.04729200
 
#library(randomForest)

rft1 <- randomForest(species ~ prov + lrl + age + lrd + dhunt,
                     data = rstmp)
rft2 <- randomForest(species ~ prov + crl + age + crd + dhunt,
                     data = rstmp)
rft3 <- randomForest(species ~ prov + mlrl + age + mlrd + dhunt,
                     data = rstmp)
rft1
# Call:
#   randomForest(formula = species ~ prov + lrl + age + lrd + dhunt,      data = rstmp) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 6.44%
# Confusion matrix:
#                   Branta canadensis Branta hutchinsii class.error
# Branta canadensis               452                21  0.04439746
# Branta hutchinsii                20               144  0.12195122
rft2

# Call:
#   randomForest(formula = species ~ prov + crl + age + crd + dhunt,      data = rstmp) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 6.91%
# Confusion matrix:
#   Branta canadensis Branta hutchinsii class.error
# Branta canadensis               449                24  0.05073996
# Branta hutchinsii                20               144  0.12195122
 rft3
# 
 # Call:
 #   randomForest(formula = species ~ prov + mlrl + age + mlrd + dhunt,      data = rstmp) 
 # Type of random forest: classification
 # Number of trees: 500
 # No. of variables tried at each split: 2
 # 
 # OOB estimate of  error rate: 5.18%
 # Confusion matrix:
 #   Branta canadensis Branta hutchinsii class.error
 # Branta canadensis               456                17  0.03594080
 # Branta hutchinsii                16               148  0.09756098
 length(which(is.na(rs$mlrl)))
#[1] 161
 length(which(is.na(rs$lrl)))
#[1]  16
 length(which(is.na(rs$crl)))
#[1] 102


 ############## above shows that mlrl does a slightly better job when all rows with missing data are removed
 ##### however, there are many more missing mlrl rows than the other two
 ##### 
 
 #### following shows that lrl does a similarly good job as mlrl when only the 10
 ## rows missing lrl data are removed
 
 var.mod <- c("species","lrl","age","dhunt","prov","lrd")
 rstmp <- na.omit(rs[,var.mod])
 
 mt1 <- glm(species ~ prov + lrl*age + lrd + dhunt*prov,
            data = rstmp,
            family = binomial)
 
 
  summary(mt1)$aic
  cvt1 <- cv.glm(data = rstmp,
                 glmfit = mt1)
  cvt1$delta
 # [1]  0.04131366 0.04131185

 rft1 <- randomForest(species ~ prov + lrl + age + lrd + dhunt,
                      data = rstmp)
 rft1
 # Call:
 #   randomForest(formula = species ~ prov + lrl + age + lrd + dhunt,      data = rstmp) 
 # Type of random forest: classification
 # Number of trees: 500
 # No. of variables tried at each split: 2
 # 
 # OOB estimate of  error rate: 4.95%
 # Confusion matrix:
 #   Branta canadensis Branta hutchinsii class.error
 # Branta canadensis               671                23  0.03314121
 # Branta hutchinsii                21               174  0.10769231
######################################################



logreg <- data.frame(model = "",
                     AIC = 0,
                     cvdelta = 0,
                     stringsAsFactors = F)

# 
# var.mod <- c("species","crl","lrl","age","dhunt","prov","crd","lrd")

var.mod <- c("species","crl","lrl","mlrl","age","dhunt","prov","crd","lrd","mlrd")

rs1 <- na.omit(rs[,var.mod])
m1 <- glm(species~prov + crl*age +dhunt*prov,
          data = rs1,
          family = binomial(link = "logit"))
rs1[,"predicted.m1"] <- predict(m1,type = "response",
        newdata = rs1)

x11()
plot(rs1$species,rs1$predicted.m1)
summary(m1)
summary(m1)$aic
rs1$predsp.m1 <- "Branta canadensis"
rs1[which(rs1$predicted.m1 > 0.5),"predsp.m1"] <- "Branta hutchinsii"
#table(rs1[,c("species","predsp.m1","age","prov")])

dfjm$predicted.m1 <- predict(m1,
                             newdata = dfjm,
                             type = "response")
plot(dfjm$species,dfjm$predicted.m1)
dfjm$predsp.m1 <- "Branta canadensis"
dfjm[which(dfjm$predicted.m1 >= 0.5),"predsp.m1"] <- "Branta hutchinsii"
#table(dfjm[,c("species","predsp.m1","age","prov")])


orig$predicted.m1 <- predict(m1,
                             newdata = orig,
                             type = "response")
plot(orig$species,orig$predicted.m1)
orig$predsp.m1 <- "Branta canadensis"
orig[which(orig$predicted.m1 >= 0.5),"predsp.m1"] <- "Branta hutchinsii"
#table(orig[,c("species","predsp.m1","age","prov")])

sj <- 1
cv1 <- cv.glm(data = rs1,
              glmfit = m1)


logreg[sj,"model"] <- as.character(m1$formula[3])
logreg[sj,"AIC"] <-  summary(m1)$aic
logreg[sj,"cvdelta"] <-   cv1$delta[2]
logreg[sj,"p.miss.hutchinsii"] <- prop.table(table(rs1[,c("species","predsp.m1")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis"] <- prop.table(table(rs1[,c("species","predsp.m1")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.im"] <- prop.table(table(rs1[,c("species","predsp.m1","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Immature"]  
logreg[sj,"p.miss.canadensis.im"] <- prop.table(table(rs1[,c("species","predsp.m1","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Immature"]    

logreg[sj,"p.miss.hutchinsii.ad"] <- prop.table(table(rs1[,c("species","predsp.m1","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Adult"]  
logreg[sj,"p.miss.canadensis.ad"] <- prop.table(table(rs1[,c("species","predsp.m1","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Adult"]    

logreg[sj,"p.miss.hutchinsii.orig"] <- prop.table(table(orig[,c("species","predsp.m1")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.orig"] <- prop.table(table(orig[,c("species","predsp.m1")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m1")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m1")]),1)["Branta canadensis","Branta hutchinsii"]    


sj <- sj+1





rs2 <- na.omit(rs[,var.mod])
m2 <- glm(species~prov + lrl*age +dhunt*prov,
          data = rs2,
          family = binomial(link = "logit"))
rs2[,"predicted.m2"] <- predict(m2,type = "response",
                                    newdata = rs2)
# cv2 <- cv.glm(data = rs2,
#               glmfit = m2)
# cv2$delta
x11()
plot(rs2$species,rs2$predicted.m2)
summary(m2)
summary(m2)$aic
rs2$predsp.m2 <- "Branta canadensis"
rs2[which(rs2$predicted.m2 > 0.5),"predsp.m2"] <- "Branta hutchinsii"
#table(rs2[,c("species","predsp","age","prov")])




dfjm$predicted.m2 <- predict(m2,
                              newdata = dfjm,
                              type = "response")
plot(dfjm$species,dfjm$predicted.m2)
dfjm$predsp.m2 <- "Branta canadensis"
dfjm[which(dfjm$predicted.m2 >= 0.5),"predsp.m2"] <- "Branta hutchinsii"
table(dfjm[,c("species","predsp.m2","age","prov")])


orig$predicted.m2 <- predict(m2,
                              newdata = orig,
                              type = "response")
plot(orig$species,orig$predicted.m2)
orig$predsp.m2 <- "Branta canadensis"
orig[which(orig$predicted.m2 >= 0.5),"predsp.m2"] <- "Branta hutchinsii"
table(orig[,c("species","predsp.m2","age","prov")])

cv2 <- cv.glm(data = rs2,
              glmfit = m2)


logreg[sj,"model"] <- as.character(m2$formula[3])
logreg[sj,"AIC"] <-  summary(m2)$aic
logreg[sj,"cvdelta"] <-   cv2$delta[2]
logreg[sj,"p.miss.hutchinsii"] <- prop.table(table(rs2[,c("species","predsp.m2")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis"] <- prop.table(table(rs2[,c("species","predsp.m2")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.im"] <- prop.table(table(rs2[,c("species","predsp.m2","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Immature"]  
logreg[sj,"p.miss.canadensis.im"] <- prop.table(table(rs2[,c("species","predsp.m2","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Immature"]    

logreg[sj,"p.miss.hutchinsii.ad"] <- prop.table(table(rs2[,c("species","predsp.m2","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Adult"]  
logreg[sj,"p.miss.canadensis.ad"] <- prop.table(table(rs2[,c("species","predsp.m2","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Adult"]    

logreg[sj,"p.miss.hutchinsii.orig"] <- prop.table(table(orig[,c("species","predsp.m2")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.orig"] <- prop.table(table(orig[,c("species","predsp.m2")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m2")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m2")]),1)["Branta canadensis","Branta hutchinsii"]    


sj <- sj+1






### ### ### ### ### ### ### ### ### ### 
### ### logistic-regression with diameter
#var.mod <- c("species","crl","lrl","crd","lrd","long","age","dhunt")


rs1d <- na.omit(rs[,var.mod])
m1d <- glm(species~prov + crl*age + crd + dhunt*prov,
          data = rs1d,
          family = binomial(link = "logit"))
rs1d[,"predicted.m1d"] <- predict(m1d,type = "response",
                                    newdata = rs1d)
# cv1 <- cv.glm(data = rs1d,
#               glmfit = m1d)
# cv1$delta
x11()
plot(rs1d$species,rs1d$predicted.m1d)
summary(m1d)
summary(m1d)$aic
rs1d$predsp.m1d <- "Branta canadensis"
rs1d[which(rs1d$predicted.m1d > 0.5),"predsp.m1d"] <- "Branta hutchinsii"
table(rs1d[,c("species","predsp","age","prov")])



dfjm$predicted.m1d <- predict(m1d,
                              newdata = dfjm,
                              type = "response")
plot(dfjm$species,dfjm$predicted.m1d)
dfjm$predsp.m1d <- "Branta canadensis"
dfjm[which(dfjm$predicted.m1d >= 0.5),"predsp.m1d"] <- "Branta hutchinsii"
table(dfjm[,c("species","predsp.m1d","age","prov")])


orig$predicted.m1d <- predict(m1d,
                              newdata = orig,
                              type = "response")
plot(orig$species,orig$predicted.m1d)
orig$predsp.m1d <- "Branta canadensis"
orig[which(orig$predicted.m1d >= 0.5),"predsp.m1d"] <- "Branta hutchinsii"
table(orig[,c("species","predsp.m1d","age","prov")])


cv1d <- cv.glm(data = rs1d,
              glmfit = m1d)


logreg[sj,"model"] <- as.character(m1d$formula[3])
logreg[sj,"AIC"] <-  summary(m1d)$aic
logreg[sj,"cvdelta"] <-   cv1d$delta[2]
logreg[sj,"p.miss.hutchinsii"] <- prop.table(table(rs1d[,c("species","predsp.m1d")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis"] <- prop.table(table(rs1d[,c("species","predsp.m1d")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.im"] <- prop.table(table(rs1d[,c("species","predsp.m1d","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Immature"]  
logreg[sj,"p.miss.canadensis.im"] <- prop.table(table(rs1d[,c("species","predsp.m1d","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Immature"]    

logreg[sj,"p.miss.hutchinsii.ad"] <- prop.table(table(rs1d[,c("species","predsp.m1d","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Adult"]  
logreg[sj,"p.miss.canadensis.ad"] <- prop.table(table(rs1d[,c("species","predsp.m1d","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Adult"]    

logreg[sj,"p.miss.hutchinsii.orig"] <- prop.table(table(orig[,c("species","predsp.m1d")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.orig"] <- prop.table(table(orig[,c("species","predsp.m1d")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m1d")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m1d")]),1)["Branta canadensis","Branta hutchinsii"]    


sj <- sj+1





### ### ### ### ### ### ### ### ### ### 
### ### logistic-regression with diameter using longest feather
#var.mod <- c("species","crl","lrl","crd","lrd","long","age","dhunt")


rs2d <- na.omit(rs[,var.mod])
m2d <- glm(species~prov + lrl*age + lrd + dhunt*prov,
           data = rs2d,
           family = binomial(link = "logit"))
rs2d[,"predicted.m2d"] <- predict(m2d,type = "response",
                                     newdata = rs2d)
#  cv1 <- cv.glm(data = rs2d,
#                glmfit = m1d2)
#  cv1$delta
x11()
plot(rs2d$species,rs2d$predicted.m2d)
summary(m2d)
summary(m2d)$aic
rs2d$predsp.m2d <- "Branta canadensis"
rs2d[which(rs2d$predicted.m2d > 0.5),"predsp.m2d"] <- "Branta hutchinsii"
table(rs2d[,c("species","predsp","age","prov")])


dfjm$predicted.m2d <- predict(m2d,
                                 newdata = dfjm,
                                 type = "response")
plot(dfjm$species,dfjm$predicted.m2d)
dfjm$predsp.m2d <- "Branta canadensis"
dfjm[which(dfjm$predicted.m2d >= 0.5),"predsp.m2d"] <- "Branta hutchinsii"
table(dfjm[,c("species","predsp.m2d","age","prov")])


orig$predicted.m2d <- predict(m2d,
                                 newdata = orig,
                                 type = "response")
plot(orig$species,orig$predicted.m2d)
orig$predsp.m2d <- "Branta canadensis"
orig[which(orig$predicted.m2d >= 0.5),"predsp.m2d"] <- "Branta hutchinsii"
table(orig[,c("species","predsp.m2d","age","prov")])


cv2d <- cv.glm(data = rs2d,
               glmfit = m2d)


logreg[sj,"model"] <- as.character(m2d$formula[3])
logreg[sj,"AIC"] <-  summary(m2d)$aic
logreg[sj,"cvdelta"] <-   cv2d$delta[2]
logreg[sj,"p.miss.hutchinsii"] <- prop.table(table(rs2d[,c("species","predsp.m2d")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis"] <- prop.table(table(rs2d[,c("species","predsp.m2d")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.im"] <- prop.table(table(rs2d[,c("species","predsp.m2d","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Immature"]  
logreg[sj,"p.miss.canadensis.im"] <- prop.table(table(rs2d[,c("species","predsp.m2d","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Immature"]    

logreg[sj,"p.miss.hutchinsii.ad"] <- prop.table(table(rs2d[,c("species","predsp.m2d","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Adult"]  
logreg[sj,"p.miss.canadensis.ad"] <- prop.table(table(rs2d[,c("species","predsp.m2d","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Adult"]    

logreg[sj,"p.miss.hutchinsii.orig"] <- prop.table(table(orig[,c("species","predsp.m2d")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.orig"] <- prop.table(table(orig[,c("species","predsp.m2d")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m2d")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m2d")]),1)["Branta canadensis","Branta hutchinsii"]    


sj <- sj+1




############# log-regression with lrl simplified


rs2s <- na.omit(rs[,var.mod])
m2s <- glm(species~prov + lrl*age,
          data = rs2s,
          family = binomial(link = "logit"))
rs2s[,"predicted.m2s"] <- predict(m2s,type = "response",
                                    newdata = rs2s)
# cv2s <- cv.glm(data = rs2s,
#               glmfit = m2s)
# cv2s$delta
x11()
plot(rs2s$species,rs2s$predicted.m2s)
summary(m2s)
summary(m2s)$aic
rs2s$predsp.m2s <- "Branta canadensis"
rs2s[which(rs2s$predicted.m2s > 0.5),"predsp.m2s"] <- "Branta hutchinsii"
#table(rs2s[,c("species","predsp","age","prov")])











dfjm$predicted.m2s <- predict(m2s,
                             newdata = dfjm,
                             type = "response")
plot(dfjm$species,dfjm$predicted.m2s)
dfjm$predsp.m2s <- "Branta canadensis"
dfjm[which(dfjm$predicted.m2s >= 0.5),"predsp.m2s"] <- "Branta hutchinsii"
table(dfjm[,c("species","predsp.m2s","age","prov")])


orig$predicted.m2s <- predict(m2s,
                             newdata = orig,
                             type = "response")
plot(orig$species,orig$predicted.m2s)
orig$predsp.m2s <- "Branta canadensis"
orig[which(orig$predicted.m2s >= 0.5),"predsp.m2s"] <- "Branta hutchinsii"
table(orig[,c("species","predsp.m2s","age","prov")])


cv2s <- cv.glm(data = rs2s,
               glmfit = m2s)


logreg[sj,"model"] <- as.character(m2s$formula[3])
logreg[sj,"AIC"] <-  summary(m2s)$aic
logreg[sj,"cvdelta"] <-   cv2s$delta[2]
logreg[sj,"p.miss.hutchinsii"] <- prop.table(table(rs2s[,c("species","predsp.m2s")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis"] <- prop.table(table(rs2s[,c("species","predsp.m2s")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.im"] <- prop.table(table(rs2s[,c("species","predsp.m2s","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Immature"]  
logreg[sj,"p.miss.canadensis.im"] <- prop.table(table(rs2s[,c("species","predsp.m2s","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Immature"]    

logreg[sj,"p.miss.hutchinsii.ad"] <- prop.table(table(rs2s[,c("species","predsp.m2s","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Adult"]  
logreg[sj,"p.miss.canadensis.ad"] <- prop.table(table(rs2s[,c("species","predsp.m2s","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Adult"]    

logreg[sj,"p.miss.hutchinsii.orig"] <- prop.table(table(orig[,c("species","predsp.m2s")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.orig"] <- prop.table(table(orig[,c("species","predsp.m2s")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m2s")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m2s")]),1)["Branta canadensis","Branta hutchinsii"]    


sj <- sj+1




############# log-regression with mlrl simplified


rs2s <- na.omit(rs[,var.mod])
m2s <- glm(species~prov + mlrl*age,
           data = rs2s,
           family = binomial(link = "logit"))
rs2s[,"predicted.m2s"] <- predict(m2s,type = "response",
                                  newdata = rs2s)
# cv2s <- cv.glm(data = rs2s,
#               glmfit = m2s)
# cv2s$delta
x11()
plot(rs2s$species,rs2s$predicted.m2s)
summary(m2s)
summary(m2s)$aic
rs2s$predsp.m2s <- "Branta canadensis"
rs2s[which(rs2s$predicted.m2s > 0.5),"predsp.m2s"] <- "Branta hutchinsii"
#table(rs2s[,c("species","predsp","age","prov")])











dfjm$predicted.m2s <- predict(m2s,
                              newdata = dfjm,
                              type = "response")
plot(dfjm$species,dfjm$predicted.m2s)
dfjm$predsp.m2s <- "Branta canadensis"
dfjm[which(dfjm$predicted.m2s >= 0.5),"predsp.m2s"] <- "Branta hutchinsii"
table(dfjm[,c("species","predsp.m2s","age","prov")])


orig$predicted.m2s <- predict(m2s,
                              newdata = orig,
                              type = "response")
plot(orig$species,orig$predicted.m2s)
orig$predsp.m2s <- "Branta canadensis"
orig[which(orig$predicted.m2s >= 0.5),"predsp.m2s"] <- "Branta hutchinsii"
table(orig[,c("species","predsp.m2s","age","prov")])


cv2s <- cv.glm(data = rs2s,
               glmfit = m2s)


logreg[sj,"model"] <- as.character(m2s$formula[3])
logreg[sj,"AIC"] <-  summary(m2s)$aic
logreg[sj,"cvdelta"] <-   cv2s$delta[2]
logreg[sj,"p.miss.hutchinsii"] <- prop.table(table(rs2s[,c("species","predsp.m2s")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis"] <- prop.table(table(rs2s[,c("species","predsp.m2s")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.im"] <- prop.table(table(rs2s[,c("species","predsp.m2s","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Immature"]  
logreg[sj,"p.miss.canadensis.im"] <- prop.table(table(rs2s[,c("species","predsp.m2s","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Immature"]    

logreg[sj,"p.miss.hutchinsii.ad"] <- prop.table(table(rs2s[,c("species","predsp.m2s","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Adult"]  
logreg[sj,"p.miss.canadensis.ad"] <- prop.table(table(rs2s[,c("species","predsp.m2s","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Adult"]    

logreg[sj,"p.miss.hutchinsii.orig"] <- prop.table(table(orig[,c("species","predsp.m2s")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.orig"] <- prop.table(table(orig[,c("species","predsp.m2s")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m2s")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m2s")]),1)["Branta canadensis","Branta hutchinsii"]    


sj <- sj+1




##### very simple logistic age and lrl only

rs2ss <- na.omit(rs[,var.mod])
m2ss <- glm(species~ lrl*age,
           data = rs2ss,
           family = binomial(link = "logit"))
rs2ss[,"predicted.m2ss"] <- predict(m2ss,type = "response",
                                     newdata = rs2ss)
# cv2ss <- cv.glm(data = rs2ss,
#               glmfit = m2ss)
# cv2ss$delta
# x11()
# plot(rs2ss$species,rs2ss$predicted.m2ss)
summary(m2ss)
summary(m2ss)$aic
rs2ss$predsp.m2ss <- "Branta canadensis"
rs2ss[which(rs2ss$predicted.m2ss > 0.5),"predsp.m2ss"] <- "Branta hutchinsii"
#table(rs2ss[,c("species","predsp","age","prov")])



dfjm$predicted.m2ss <- predict(m2ss,
                              newdata = dfjm,
                              type = "response")
plot(dfjm$species,dfjm$predicted.m2ss)
dfjm$predsp.m2ss <- "Branta canadensis"
dfjm[which(dfjm$predicted.m2ss >= 0.5),"predsp.m2ss"] <- "Branta hutchinsii"
table(dfjm[,c("species","predsp.m2ss","age","prov")])


orig$predicted.m2ss <- predict(m2ss,
                              newdata = orig,
                              type = "response")
plot(orig$species,orig$predicted.m2ss)
orig$predsp.m2ss <- "Branta canadensis"
orig[which(orig$predicted.m2ss >= 0.5),"predsp.m2ss"] <- "Branta hutchinsii"
table(orig[,c("species","predsp.m2ss","age","prov")])


cv2ss <- cv.glm(data = rs2ss,
               glmfit = m2ss)


logreg[sj,"model"] <- as.character(m2ss$formula[3])
logreg[sj,"AIC"] <-  summary(m2ss)$aic
logreg[sj,"cvdelta"] <-   cv2ss$delta[2]
logreg[sj,"p.miss.hutchinsii"] <- prop.table(table(rs2ss[,c("species","predsp.m2ss")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis"] <- prop.table(table(rs2ss[,c("species","predsp.m2ss")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.im"] <- prop.table(table(rs2ss[,c("species","predsp.m2ss","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Immature"]  
logreg[sj,"p.miss.canadensis.im"] <- prop.table(table(rs2ss[,c("species","predsp.m2ss","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Immature"]    

logreg[sj,"p.miss.hutchinsii.ad"] <- prop.table(table(rs2ss[,c("species","predsp.m2ss","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Adult"]  
logreg[sj,"p.miss.canadensis.ad"] <- prop.table(table(rs2ss[,c("species","predsp.m2ss","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Adult"]    

logreg[sj,"p.miss.hutchinsii.orig"] <- prop.table(table(orig[,c("species","predsp.m2ss")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.orig"] <- prop.table(table(orig[,c("species","predsp.m2ss")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m2ss")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m2ss")]),1)["Branta canadensis","Branta hutchinsii"]    


sj <- sj+1



############# log-regression with crl simplified


rs1s <- na.omit(rs[,var.mod])
m1s <- glm(species~prov + crl*age,
           data = rs1s,
           family = binomial(link = "logit"))
rs1s[,"predicted.m1s"] <- predict(m1s,type = "response",
                                  newdata = rs1s)
# cv1s <- cv.glm(data = rs1s,
#               glmfit = m1s)
# cv1s$delta
x11()
plot(rs1s$species,rs1s$predicted.m1s)
summary(m1s)
summary(m1s)$aic
rs1s$predsp.m1s <- "Branta canadensis"
rs1s[which(rs1s$predicted.m1s > 0.5),"predsp.m1s"] <- "Branta hutchinsii"
#table(rs1s[,c("species","predsp","age","prov")])











dfjm$predicted.m1s <- predict(m1s,
                              newdata = dfjm,
                              type = "response")
plot(dfjm$species,dfjm$predicted.m1s)
dfjm$predsp.m1s <- "Branta canadensis"
dfjm[which(dfjm$predicted.m1s >= 0.5),"predsp.m1s"] <- "Branta hutchinsii"
table(dfjm[,c("species","predsp.m1s","age","prov")])


orig$predicted.m1s <- predict(m1s,
                              newdata = orig,
                              type = "response")
plot(orig$species,orig$predicted.m1s)
orig$predsp.m1s <- "Branta canadensis"
orig[which(orig$predicted.m1s >= 0.5),"predsp.m1s"] <- "Branta hutchinsii"
table(orig[,c("species","predsp.m1s","age","prov")])


cv1s <- cv.glm(data = rs1s,
               glmfit = m1s)


logreg[sj,"model"] <- as.character(m1s$formula[3])
logreg[sj,"AIC"] <-  summary(m1s)$aic
logreg[sj,"cvdelta"] <-   cv1s$delta[2]
logreg[sj,"p.miss.hutchinsii"] <- prop.table(table(rs1s[,c("species","predsp.m1s")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis"] <- prop.table(table(rs1s[,c("species","predsp.m1s")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.im"] <- prop.table(table(rs1s[,c("species","predsp.m1s","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Immature"]  
logreg[sj,"p.miss.canadensis.im"] <- prop.table(table(rs1s[,c("species","predsp.m1s","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Immature"]    

logreg[sj,"p.miss.hutchinsii.ad"] <- prop.table(table(rs1s[,c("species","predsp.m1s","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Adult"]  
logreg[sj,"p.miss.canadensis.ad"] <- prop.table(table(rs1s[,c("species","predsp.m1s","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Adult"]    

logreg[sj,"p.miss.hutchinsii.orig"] <- prop.table(table(orig[,c("species","predsp.m1s")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.orig"] <- prop.table(table(orig[,c("species","predsp.m1s")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m1s")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m1s")]),1)["Branta canadensis","Branta hutchinsii"]    


sj <- sj+1


##### very simple logistic age and crl only

rs1ss <- na.omit(rs[,var.mod])
m1ss <- glm(species~ crl*age,
            data = rs1ss,
            family = binomial(link = "logit"))
rs1ss[,"predicted.m1ss"] <- predict(m1ss,type = "response",
                                      newdata = rs1ss)
# cv1ss <- cv.glm(data = rs1ss,
#               glmfit = m1ss)
# cv1ss$delta
# x11()
# plot(rs1ss$species,rs1ss$predicted.m1ss)
summary(m1ss)
summary(m1ss)$aic
rs1ss$predsp.m1ss <- "Branta canadensis"
rs1ss[which(rs1ss$predicted.m1ss > 0.5),"predsp.m1ss"] <- "Branta hutchinsii"
table(rs1ss[,c("species","predsp.m1ss","age","prov")])



dfjm$predicted.m1ss <- predict(m1ss,
                               newdata = dfjm,
                               type = "response")
plot(dfjm$species,dfjm$predicted.m1ss)
dfjm$predsp.m1ss <- "Branta canadensis"
dfjm[which(dfjm$predicted.m1ss >= 0.5),"predsp.m1ss"] <- "Branta hutchinsii"
table(dfjm[,c("species","predsp.m1ss","age","prov")])


orig$predicted.m1ss <- predict(m1ss,
                               newdata = orig,
                               type = "response")
plot(orig$species,orig$predicted.m1ss)
orig$predsp.m1ss <- "Branta canadensis"
orig[which(orig$predicted.m1ss >= 0.5),"predsp.m1ss"] <- "Branta hutchinsii"
table(orig[,c("species","predsp.m1ss","age","prov")])


cv1ss <- cv.glm(data = rs1ss,
                glmfit = m1ss)


logreg[sj,"model"] <- as.character(m1ss$formula[3])
logreg[sj,"AIC"] <-  summary(m1ss)$aic
logreg[sj,"cvdelta"] <-   cv1ss$delta[2]
logreg[sj,"p.miss.hutchinsii"] <- prop.table(table(rs1ss[,c("species","predsp.m1ss")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis"] <- prop.table(table(rs1ss[,c("species","predsp.m1ss")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.im"] <- prop.table(table(rs1ss[,c("species","predsp.m1ss","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Immature"]  
logreg[sj,"p.miss.canadensis.im"] <- prop.table(table(rs1ss[,c("species","predsp.m1ss","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Immature"]    

logreg[sj,"p.miss.hutchinsii.ad"] <- prop.table(table(rs1ss[,c("species","predsp.m1ss","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Adult"]  
logreg[sj,"p.miss.canadensis.ad"] <- prop.table(table(rs1ss[,c("species","predsp.m1ss","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Adult"]    

logreg[sj,"p.miss.hutchinsii.orig"] <- prop.table(table(orig[,c("species","predsp.m1ss")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.orig"] <- prop.table(table(orig[,c("species","predsp.m1ss")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m1ss")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.m1ss")]),1)["Branta canadensis","Branta hutchinsii"]    


sj <- sj+1



pdf(file = "predictive accuracy logistic.pdf",
    width = 11,
    height = 8.5)
par(mfcol = c(2,3),
    mar = c(3,4,3,1),
    oma = c(1,0,0,0))



err.pl(mod = m2,
       dt = rs2,
       predcl = "predsp.m2")

err.pl(mod = m2,
       dt = orig,
       predcl = "predsp.m2",
       descr = "Original study")

err.pl(mod = m2,
       dt = dfjm,
       predcl = "predsp.m2",
       descr = "Leafloor")


err.pl(mod = m2d,
       dt = rs2d,
       predcl = "predsp.m2d")

err.pl(mod = m2d,
       dt = orig,
       predcl = "predsp.m2d",
       descr = "Original study")

err.pl(mod = m2d,
       dt = dfjm,
       predcl = "predsp.m2d",
       descr = "Leafloor")



err.pl(mod = m2s,
       dt = rs2s,
       predcl = "predsp.m2s")

err.pl(mod = m2s,
       dt = orig,
       predcl = "predsp.m2s",
       descr = "Original study")

err.pl(mod = m2s,
       dt = dfjm,
       predcl = "predsp.m2s",
       descr = "Leafloor")


err.pl(mod = m2ss,
       dt = rs2ss,
       predcl = "predsp.m2ss")

err.pl(mod = m2ss,
       dt = orig,
       predcl = "predsp.m2ss",
       descr = "Original study")

err.pl(mod = m2ss,
       dt = dfjm,
       predcl = "predsp.m2ss",
       descr = "Leafloor")


err.pl(mod = m1,
       dt = rs1,
       predcl = "predsp.m1")

err.pl(mod = m1,
       dt = orig,
       predcl = "predsp.m1",
       descr = "Original study")

err.pl(mod = m1,
       dt = dfjm,
       predcl = "predsp.m1",
       descr = "Leafloor")


err.pl(mod = m1d,
       dt = rs1d,
       predcl = "predsp.m1d")

err.pl(mod = m1d,
       dt = orig,
       predcl = "predsp.m1d",
       descr = "Original study")

err.pl(mod = m1d,
       dt = dfjm,
       predcl = "predsp.m1d",
       descr = "Leafloor")


err.pl(mod = m1s,
       dt = rs1s,
       predcl = "predsp.m1s")

err.pl(mod = m1s,
       dt = orig,
       predcl = "predsp.m1s",
       descr = "Original study")

err.pl(mod = m1s,
       dt = dfjm,
       predcl = "predsp.m1s",
       descr = "Leafloor")


err.pl(mod = m1ss,
       dt = rs1ss,
       predcl = "predsp.m1ss")

err.pl(mod = m1ss,
       dt = orig,
       predcl = "predsp.m1ss",
       descr = "Original study")

err.pl(mod = m1ss,
       dt = dfjm,
       predcl = "predsp.m1ss",
       descr = "Leafloor")

dev.off()






#################### exploring the best logistic regression model

### lowest cross validation error = m2

param.spacel <- expand.grid(age = levels(rs$age),
                           prov = c("Alberta","Saskatchewan","Manitoba"),
                           dhunt = seq(min(rs$dhunt,na.rm = T),max(rs$dhunt,na.rm = T),by = 7),
                           lrl = seq(min(rs$lrl,na.rm = T),max(rs$lrl,na.rm = T),by = 1))#,
                           #lrd = seq(min(rs$lrd,na.rm = T),max(rs$lrd,na.rm = T),by = 0.001))
 

mld <- lm(lrd ~lrl,
          data = rs) 

param.spacel[,"lrd"] <- predict(mld,newdata = param.spacel,type = "response")


param.spacel$predicted.m2 <- predict(m2,
                                    newdata = param.spacel,
                                    type = "response")

param.spacel$predsp.m2 <- "Branta canadensis"
param.spacel[which(param.spacel$predicted.m2 >= 0.5),"predsp.m2"] <- "Branta hutchinsii"


colsa <- c("darkorange","purple")
names(colsa) <- c("Adult","Immature")
x11()
plot(x = 1,
     y = 1,
     xlim = range(param.spacel$lrl),
     ylim = c(0,1),
     bty = "l",
     ylab = "Probability Branta hutchinsii",
     xlab = "Longest rectrix length")

for(a in levels(param.spacel$age)){
  for(p in levels(param.spacel$prov)){
    tmp <- param.spacel[which(param.spacel$dhunt == median(param.spacel$dhunt) &
                                param.spacel$age == a & param.spacel$prov == p),]
   
    lines(x = tmp$lrl,
          y = tmp$predicted.m2,
          col = colsa[a])
    if(a == "Adult"){
      text(x = tmp[which.min(abs(tmp$predicted.m2 - 0.5)),"lrl"],
         y = 0.5+runif(1,min = -0.1,max = 0.1),
         paste(substr(p,1,3)))
    }
    
    
  }#p
  
  tmp <- rs[which(rs$age == a),]
  points(x = tmp$lrl,
         y = (as.integer(tmp$species)-1)+runif(length(tmp$lrl),min = -0.03,0.03),
         col = transp.func(colsa[a],0.5))
  
  
}#a


library(RColorBrewer)
colsp <- brewer.pal(3,"Dark2")
names(colsp) <- c("Alberta",
                  "Saskatchewan",
                  "Manitoba")




x11()
plot(x = 1,
     y = 1,
     xlim = range(param.spacel$dhunt),
     ylim = c(0,1),
     bty = "l",
     ylab = "Probability Branta hutchinsii",
     xlab = "day hunt",
     main = "lrl held at 160 and lrdat 0.124")

for(a in levels(param.spacel$age)){

for(p in levels(param.spacel$prov)){
  
    tmp <- param.spacel[which(param.spacel$lrl == 160 &
                                param.spacel$age == a & param.spacel$prov == p),]
    
    lines(x = tmp$dhunt,
          y = tmp$predicted.m2,
          col = colsp[p])
      if(a == "Adult") {
        text(x = tmp[min(which.min(abs(tmp$predicted.m2 - 0.4))),"dhunt"],
           y = 0.4+runif(1,min = -0.05,max = 0.05),
           paste(a,substr(p,1,3)),
           col = colsp[p])}

  tmp <- rs[which(rs$prov == p),]
  points(x = tmp$dhunt,
         y = (as.integer(tmp$species)-1)+runif(length(tmp$lrl),min = -0.03,0.03),
         col = transp.func(colsp[p],0.9))
  
}#p

}#a


#########plotting the effect of lrd

param.spaced <- expand.grid(age = levels(rs$age),
                            prov = c("Alberta","Saskatchewan","Manitoba"),
                            dhunt = seq(min(rs$dhunt,na.rm = T),max(rs$dhunt,na.rm = T),by = 7),
                            lrd = seq(min(rs$lrd,na.rm = T),max(rs$lrd,na.rm = T),by = 0.001))#,
#lrd = seq(min(rs$lrd,na.rm = T),max(rs$lrd,na.rm = T),by = 0.001))


mdl <- lm(lrl ~lrd,
          data = rs) 

param.spaced[,"lrl"] <- predict(mdl,newdata = param.spaced,type = "response")


param.spaced$predicted.m2 <- predict(m2,
                                     newdata = param.spaced,
                                     type = "response")

param.spaced$predsp.m2 <- "Branta canadensis"
param.spaced[which(param.spaced$predicted.m2 >= 0.5),"predsp.m2"] <- "Branta hutchinsii"


colsa <- c("darkorange","purple")
names(colsa) <- c("Adult","Immature")
x11()
plot(x = 1,
     y = 1,
     xlim = range(param.spaced$lrd),
     ylim = c(0,1),
     bty = "l",
     ylab = "Probability Branta hutchinsii",
     xlab = "Longest rectrix diameter")

for(a in levels(param.spaced$age)){
  for(p in levels(param.spaced$prov)){
    tmp <- param.spaced[which(param.spaced$dhunt == median(param.spaced$dhunt) &
                                param.spaced$age == a & param.spaced$prov == p),]
    
    lines(x = tmp$lrd,
          y = tmp$predicted.m2,
          col = colsa[a])
    if(a == "Adult"){
      text(x = tmp[which.min(abs(tmp$predicted.m2 - 0.5)),"lrd"],
           y = 0.5+runif(1,min = -0.1,max = 0.1),
           paste(substr(p,1,3)))
    }
    
    
  }#p
  
  tmp <- rs[which(rs$age == a),]
  points(x = tmp$lrd,
         y = (as.integer(tmp$species)-1)+runif(length(tmp$lrd),min = -0.03,0.03),
         col = transp.func(colsa[a],0.5))
  
  
}#a



cov.plot(rs[,c("species",
               "lrl","crl","mlrl",
               "lrd","crd","mlrd")],
         incvif = F)































# 
# 
# ### plotting the final logistic regression model
# 
# confmat <- table(rs4[,c("species","predsp","age")])
# 
# imcangfcack <- confmat["Branta canadensis","hutchinsii","Immature"]
# imcackfcang <- confmat["Branta hutchinsii","canadensis","Immature"]
# imerror <- sum(c(imcangfcack,imcackfcang))/sum(confmat[,,"Immature"])
# imerror
# #[1] 0.1746725 - similar to values from random forest below
# imcangerror <- imcangfcack/sum(confmat["Branta canadensis",,"Immature"])
# imcackerror <- imcackfcang/sum(confmat["Branta hutchinsii",,"Immature"])
# 
# adcangfcack <- confmat["Branta canadensis","hutchinsii","Adult"]
# adcackfcang <- confmat["Branta hutchinsii","canadensis","Adult"]
# aderror <- sum(c(adcangfcack,adcackfcang))/sum(confmat[,,"Adult"])
# adcangerror <- adcangfcack/sum(confmat["Branta canadensis",,"Adult"])
# adcackerror <- adcackfcang/sum(confmat["Branta hutchinsii",,"Adult"])
# aderror
# #[1] 0.02923264 - similar to values from random forest below
# cangerror <- sum(c(adcangfcack,imcangfcack))/sum(confmat["Branta canadensis",,])
# cangerror
# # [1] 0.03206651
# cackerror <- sum(c(adcackfcang,imcackfcang))/sum(confmat["Branta hutchinsii",,])
# cackerror
# # [1] 0.1778846
# prop.table(table(rs4[,c("species","predsp")]),1)
# confmat
# # , , age = Immature
# # 
# # predsp
# # species             canadensis hutchinsii
# # Branta canadensis        125         16
# # Branta hutchinsii         24         64
# # 
# # , , age = Adult
# # 
# # predsp
# # species             canadensis hutchinsii
# # Branta canadensis        690         11
# # Branta hutchinsii         13        107
# 
# rs4[,"plotcol"] <- "purple"
# rs4[which(rs4$age == "Adult"),"plotcol"] <- "darkorange"
# rs4$spplot <- runif(length(rs4$species),-0.02,0.02)+as.integer(rs4$species)
# 
# 
# dfna <- expand.grid(lrl = seq(min(rs4$lrl),max(rs4$lrl),by = 0.1),
#                     age = "Adult")
# dfni <- expand.grid(lrl = seq(min(rs4$lrl),max(rs4$lrl),by = 0.1),
#                     age = "Immature")
# pla <- predict(m4,newdata = dfna,se.fit = T,type = "response")
# pli <- predict(m4,newdata = dfni,se.fit = T,type = "response")
# 
# 
# x11()
# plot(y = rs4$spplot,
#      x = rs4$lrl,
#      ylab = "",
#      xlab = "Longest Rectrix Length",
#      yaxt = "n",
#      col = rs4$plotcol,
#      bty = "l")
# axis(side = 2,
#      at = c(1,2),
#      labels = c("CANG",
#                 "CACK"),
#      las = 1)
# 
# 
# polygon(x = c(dfna$lrl,rev(dfna$lrl)),
#         y = 1+c((pla$fit-1.96*pla$se.fit),rev(pla$fit+1.96*pla$se.fit)),
#         col = transp.func("darkorange",0.1),
#         border = NA)
# 
# polygon(x = c(dfni$lrl,rev(dfni$lrl)),
#         y = 1+c((pli$fit-1.96*pli$se.fit),rev(pli$fit+1.96*pli$se.fit)),
#         col = transp.func("purple",0.1),
#         border = NA)
# 
# lines(y = 1+pla$fit,
#       x = dfna$lrl,
#       col = "darkorange",
#       lwd = 2)
# lines(y = 1+pli$fit,
#       x = dfni$lrl,
#       col = "purple",
#       lwd = 2)
# points(y = rs4$spplot,
#        x = rs4$lrl,
#        col = rs4$plotcol)
# lines(x = rep(dfna$lrl[which.min(pla$fit > 0.5)],2),
#       y = c(1,2),
#       col = transp.func("darkorange",0.5))
# lines(x = rep(dfni$lrl[which.min(pli$fit > 0.5)],2),
#       y = c(1,2),
#       col = transp.func("purple",0.5))
# text(x = dfna$lrl[which.min(pla$fit > 0.5)],
#      y = 1.5,
#      pos = 4,
#      paste0("Adult ",round(dfna$lrl[which.min(pla$fit > 0.5)]),"mm ",round(aderror,2)*100,"%"),
#      col = "darkorange")
# text(x = dfni$lrl[which.min(pli$fit > 0.5)],
#      y = 1.5,
#      pos = 2,
#      paste0("Immature ",round(dfni$lrl[which.min(pli$fit > 0.5)]),"mm ",round(imerror,2)*100,"%"),
#      col = "purple")
# text(x = dfna$lrl[which.min(pla$fit > 0.5)],
#      y = 1.4,
#      pos = 4,
#      paste0("CANG ",round(adcangerror,2)*100,"% CACK ",round(adcackerror,2)*100,"%"),
#      col = "darkorange")
# text(x = dfni$lrl[which.min(pli$fit > 0.5)],
#      y = 1.4,
#      pos = 2,
#      paste0("CANG ",round(imcangerror,2)*100,"% CACK ",round(imcackerror,2)*100,"%"),
#      col = "purple")
# 
# 
# 




# ### logistic-regression
# var.mod <- c("species","crl","lrl","mlrl","long","age","dhunt")
# 
# rs2 <- na.omit(rs[,var.mod])
# 
# m2 <- glm(species~lrl*long + lrl*age +dhunt,
#           data = rs2,
#           family = binomial(link = "logit"))
# summary(m2)
# 
# 
# rs2[,"predicted.logstc"] <- predict(m2,type = "response",
#                                     newdata = rs2)
# cv2 <- cv.glm(data = rs2,
#               glmfit = m2)
# cv2$delta
# 
# x11()
# plot(rs2$species,rs2$predicted.logstc)
# summary(m2)
# summary(m2)$aic
# rs2$predsp <- "canadensis"
# rs2[which(rs2$predicted.logstc > 0.5),"predsp"] <- "hutchinsii"
# table(rs2[,c("species","predsp")])
# 
# 
# 
# ### logistic-regression
# var.mod <- c("species","crl","lrl","crd","lrd","long","age","dhunt")
# 
# rs2d <- na.omit(rs[,var.mod])
# 
# m2d <- glm(species~lrl*long + lrl*age + lrd*age,# +dhunt,
#           data = rs2d,
#           family = binomial(link = "logit"))
# rs2d[,"predicted.logstc"] <- predict(m2d,type = "response",
#                                     newdata = rs2d)
# cv2d <- cv.glm(data = rs2d,
#               glmfit = m2d)
# cv2d$delta
# 
# x11()
# plot(rs2d$species,rs2d$predicted.logstc)
# summary(m2d)
# summary(m2d)$aic
# rs2d$predsp <- "canadensis"
# rs2d[which(rs2d$predicted.logstc > 0.5),"predsp"] <- "hutchinsii"
# table(rs2d[,c("species","predsp")])
# 
# ### logistic-regression
# var.mod <- c("species","crl","lrl","mlrl","long","age","dhunt")
# 
# rs3 <- na.omit(rs[,var.mod])
# 
# m3 <- glm(species~mlrl*long + mlrl*age +dhunt,
#           data = rs3,
#           family = binomial(link = "logit"))
# rs3[,"predicted.logstc"] <- predict(m3,type = "response",
#                                     newdata = rs3)
# cv3 <- cv.glm(data = rs3,
#               glmfit = m3)
# cv3$delta
# x11()
# plot(rs3$species,rs3$predicted.logstc)
# summary(m3)
# summary(m3)$aic
# rs3$predsp <- "canadensis"
# rs3[which(rs3$predicted.logstc > 0.5),"predsp"] <- "hutchinsii"
# table(rs3[,c("species","predsp")])

##############


############## final logisitic regression prediction models





var.mod <- c("species","lrl","age","prov")

rs4 <- na.omit(rs[,var.mod])

m4 <- glm(species~ lrl*age + prov ,
          data = rs4,
          family = binomial(link = "logit"))
rs4[,"predicted.m4"] <- predict(m4,type = "response",
                                    newdata = rs4)


dfjm$predicted.m4 <- predict(m4,
                     newdata = dfjm,
                     type = "response")
plot(dfjm$species,dfjm$predicted.m4)
dfjm$predsp.m4 <- "Branta canadensis"
dfjm[which(dfjm$predicted.m4 >= 0.5),"predsp.m4"] <- "Branta hutchinsii"
table(dfjm[,c("species","predsp.m4")])


orig$predicted.m4 <- predict(m4,
                                 newdata = orig,
                                 type = "response")
plot(orig$species,orig$predicted.m4)
orig$predsp.m4 <- "Branta canadensis"
orig[which(orig$predicted.m4 >= 0.5),"predsp.m4"] <- "Branta hutchinsii"
table(orig[,c("species","predsp.m4")])


cv4 <- cv.glm(data = rs4,
              glmfit = m4)

cv4$delta

x11()
plot(rs4$species,rs4$predicted.m4)
summary(m4)
summary(m4)$aic
rs4$predsp.m4 <- "Branta canadensis"
rs4[which(rs4$predicted.m4 > 0.5),"predsp.m4"] <- "Branta hutchinsii"

confmat <- table(rs4[,c("species","predsp.m4","age")])

imcangfcack <- confmat["Branta canadensis","Branta hutchinsii","Immature"]
imcackfcang <- confmat["Branta hutchinsii","Branta canadensis","Immature"]
imerror <- sum(c(imcangfcack,imcackfcang))/sum(confmat[,,"Immature"])
imerror
#[1] 0.1519608 - similar to values from random forest below
imcangerror <- imcangfcack/sum(confmat["Branta canadensis",,"Immature"])
imcackerror <- imcackfcang/sum(confmat["Branta hutchinsii",,"Immature"])

adcangfcack <- confmat["Branta canadensis","Branta hutchinsii","Adult"]
adcackfcang <- confmat["Branta hutchinsii","Branta canadensis","Adult"]
aderror <- sum(c(adcangfcack,adcackfcang))/sum(confmat[,,"Adult"])
adcangerror <- adcangfcack/sum(confmat["Branta canadensis",,"Adult"])
adcackerror <- adcackfcang/sum(confmat["Branta hutchinsii",,"Adult"])
aderror
#[1] 0.03244006 - similar to values from random forest below
cangerror <- sum(c(adcangfcack,imcangfcack))/sum(confmat["Branta canadensis",,])
cangerror
# [1] 0.03776224
cackerror <- sum(c(adcackfcang,imcackfcang))/sum(confmat["Branta hutchinsii",,])
cackerror
# [1] 0.1363636
prop.table(table(rs4[,c("species","predsp.m4")]),1)
confmat
# , , age = Immature
# 
# predsp.m4
# species             Branta canadensis Branta hutchinsii
# Branta canadensis               104                14
# Branta hutchinsii                17                69
# 
# , , age = Adult
# 
# predsp.m4
# species             Branta canadensis Branta hutchinsii
# Branta canadensis               584                13
# Branta hutchinsii                10               102

rs4[,"plotcol"] <- "purple"
rs4[which(rs4$age == "Adult"),"plotcol"] <- "darkorange"
rs4$spplot <- runif(length(rs4$species),-0.02,0.02)+as.integer(rs4$species)


dfna1 <- expand.grid(lrl = seq(min(rs4$lrl),max(rs4$lrl),by = 0.1),
                   age = "Adult",
                   prov = c("Alberta","Saskatchewan","Manitoba"))
dfni1 <- expand.grid(lrl = seq(min(rs4$lrl),max(rs4$lrl),by = 0.1),
                    age = "Immature",
                    prov = c("Alberta","Saskatchewan","Manitoba"))
pla1 <- predict(m4,newdata = dfna1,se.fit = T,type = "response")
pli1 <- predict(m4,newdata = dfni1,se.fit = T,type = "response")


x11()
plot(y = rs4$spplot,
     x = rs4$lrl,
     ylab = "",
     xlab = "Longest Rectrix Length",
     yaxt = "n",
     col = rs4$plotcol,
     bty = "l")
axis(side = 2,
     at = c(1,2),
     labels = c("CANG",
                "CACK"),
     las = 1)

for(p in levels(dfna1$prov)){
  dfna <- dfna1[which(dfna1$prov == p),]
  pla <- predict(m4,newdata = dfna,se.fit = T,type = "response")

  dfni <- dfni1[which(dfni1$prov == p),]
  pli <- predict(m4,newdata = dfni,se.fit = T,type = "response")
  
  polygon(x = c(dfna$lrl,rev(dfna$lrl)),
        y = 1+c((pla$fit-1.96*pla$se.fit),rev(pla$fit+1.96*pla$se.fit)),
        col = transp.func("darkorange",0.3),
        border = NA)

polygon(x = c(dfni$lrl,rev(dfni$lrl)),
        y = 1+c((pli$fit-1.96*pli$se.fit),rev(pli$fit+1.96*pli$se.fit)),
        col = transp.func("purple",0.3),
        border = NA)

lines(y = 1+pla$fit,
      x = dfna$lrl,
      col = "darkorange",
      lwd = 2)
lines(y = 1+pli$fit,
      x = dfni$lrl,
      col = "purple",
      lwd = 2)
points(y = rs4$spplot,
     x = rs4$lrl,
     col = rs4$plotcol)
lines(x = rep(dfna$lrl[which.min(pla$fit > 0.5)],2),
      y = c(1,2),
      col = transp.func("darkorange",0.5))
lines(x = rep(dfni$lrl[which.min(pli$fit > 0.5)],2),
      y = c(1,2),
      col = transp.func("purple",0.5))

text(x = dfna$lrl[which.min(pla$fit > 0.5)],
     y = 1.3+(which(levels(dfna1$prov) == p)/10),
     pos = 4,
     paste0(round(dfna$lrl[which.min(pla$fit > 0.5)]),"mm ",p),
     col = "darkorange")
text(x = dfni$lrl[which.min(pli$fit > 0.5)],
     y = 1.3+(which(levels(dfni1$prov) == p)/10),
     pos = 2,
     paste0(round(dfni$lrl[which.min(pli$fit > 0.5)]),"mm ",p),
     col = "purple")

# text(x = dfna$lrl[which.min(pla$fit > 0.5)],
#      y = 1.5,
#      pos = 4,
#      paste0("Adult ",round(dfna$lrl[which.min(pla$fit > 0.5)]),"mm ",round(aderror,2)*100,"%"),
#      col = "darkorange")
# text(x = dfni$lrl[which.min(pli$fit > 0.5)],
#      y = 1.5,
#      pos = 2,
#      paste0("Immature ",round(dfni$lrl[which.min(pli$fit > 0.5)]),"mm ",round(imerror,2)*100,"%"),
#      col = "purple")
# text(x = dfna$lrl[which.min(pla$fit > 0.5)],
#      y = 1.4,
#      pos = 4,
#      paste0("CANG ",round(adcangerror,2)*100,"% CACK ",round(adcackerror,2)*100,"%"),
#      col = "darkorange")
# text(x = dfni$lrl[which.min(pli$fit > 0.5)],
#      y = 1.4,
#      pos = 2,
#      paste0("CANG ",round(imcangerror,2)*100,"% CACK ",round(imcackerror,2)*100,"%"),
#      col = "purple")






}#p





################### same as above but split across three plots in pdf
logregthresh <- expand.grid(feather = c("lrl","mlrl","crl"),
                            age = levels(rs$age),
                            prov = levels(rs$prov)[2:4])


###########start here in teh am to fill in the above dataframe with the 
######## prov and age specific cutoffs in feather length



pdf(file = "best logistic regression by prov.pdf",
    height = 8,
    width = 8)
for(p in levels(dfna1$prov)){
  rs4t <- rs4[which(rs4$prov == as.character(p)),]
  plot(y = rs4t$spplot,
       x = rs4t$lrl,
       ylab = "",
       xlab = "Longest Rectrix Length",
       yaxt = "n",
       col = rs4t$plotcol,
       bty = "l",
       main = p,
       xlim = range(rs4$lrl))
  axis(side = 2,
       at = c(1,2),
       labels = c("CANG",
                  "CACK"),
       las = 1)
  
  dfna <- dfna1[which(dfna1$prov == p),]
  pla <- predict(m4,newdata = dfna,se.fit = T,type = "response")
  
  dfni <- dfni1[which(dfni1$prov == p),]
  pli <- predict(m4,newdata = dfni,se.fit = T,type = "response")
  
  polygon(x = c(dfna$lrl,rev(dfna$lrl)),
          y = 1+c((pla$fit-1.96*pla$se.fit),rev(pla$fit+1.96*pla$se.fit)),
          col = transp.func("darkorange",0.3),
          border = NA)
  
  polygon(x = c(dfni$lrl,rev(dfni$lrl)),
          y = 1+c((pli$fit-1.96*pli$se.fit),rev(pli$fit+1.96*pli$se.fit)),
          col = transp.func("purple",0.3),
          border = NA)
  
  lines(y = 1+pla$fit,
        x = dfna$lrl,
        col = "darkorange",
        lwd = 2)
  lines(y = 1+pli$fit,
        x = dfni$lrl,
        col = "purple",
        lwd = 2)
  points(y = rs4t$spplot,
         x = rs4t$lrl,
         col = rs4t$plotcol)
  
  feath = "lrl"
  
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Adult" &
                       logregthresh$prov == p),"cutoff minimum CANG"] = dfna$lrl[which.min(pla$fit > 0.5)]
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Immature" &
                       logregthresh$prov == p),"cutoff minimum CANG"] = dfni$lrl[which.min(pli$fit > 0.5)]
  
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Adult" &
                       logregthresh$prov == p),"lower quartile CACK"] = dfna$lrl[which.min(pla$fit > 0.75)]
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Immature" &
                       logregthresh$prov == p),"lower quartile CACK"] = dfni$lrl[which.min(pli$fit > 0.75)]
  
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Adult" &
                       logregthresh$prov == p),"upper quartile CANG"] = dfna$lrl[which.min(pla$fit > 0.25)]
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Immature" &
                       logregthresh$prov == p),"upper quartile CANG"] = dfni$lrl[which.min(pli$fit > 0.25)]
  
  
  lines(x = rep(dfna$lrl[which.min(pla$fit > 0.5)],2),
        y = c(1,2),
        col = transp.func("darkorange",0.7),
        lwd = 2)
  

  lines(x = rep(dfni$lrl[which.min(pli$fit > 0.5)],2),
        y = c(1,2),
        col = transp.func("purple",0.7),
        lwd = 2)

  lines(x = rep(dfna$lrl[which.min(pla$fit > 0.75)],2),
        y = c(1,2),
        col = transp.func("darkorange",0.3))
  lines(x = rep(dfni$lrl[which.min(pli$fit > 0.75)],2),
        y = c(1,2),
        col = transp.func("purple",0.3))

  
  lines(x = rep(dfna$lrl[which.min(pla$fit > 0.25)],2),
        y = c(1,2),
        col = transp.func("darkorange",0.3))
  lines(x = rep(dfni$lrl[which.min(pli$fit > 0.25)],2),
        y = c(1,2),
        col = transp.func("purple",0.3))
  
  text(x = dfna$lrl[which.min(pla$fit > 0.5)],
       y = 1.4+(which(levels(dfna1$prov) == p)/10),
       pos = 4,
       paste0(round(dfna$lrl[which.min(pla$fit > 0.5)]),"mm "),
       col = "darkorange")
  text(x = dfni$lrl[which.min(pli$fit > 0.5)],
       y = 1.4+(which(levels(dfni1$prov) == p)/10),
       pos = 2,
       paste0(round(dfni$lrl[which.min(pli$fit > 0.5)]),"mm "),
       col = "purple")
  
  
  text(x = dfna$lrl[which.min(pla$fit > 0.5)],
       y = 1.3+(which(levels(dfna1$prov) == p)/10),
       pos = 4,
       paste0(round(dfna$lrl[which.min(pla$fit > 0.75)])," : ",round(dfna$lrl[which.min(pla$fit > 0.25)]),"mm "),
       col = "darkorange")
  text(x = dfni$lrl[which.min(pli$fit > 0.5)],
       y = 1.3+(which(levels(dfni1$prov) == p)/10),
       pos = 2,
       paste0(round(dfni$lrl[which.min(pli$fit > 0.75)])," : ",round(dfni$lrl[which.min(pli$fit > 0.25)]),"mm "),
       col = "purple")
  

}#p

dev.off()

#### same as above, using CRL instead
##############
#### same as above, using CRL instead
##############
#### same as above, using CRL instead
##############
#### same as above, using CRL instead
##############
#### same as above, using CRL instead
##############
#### same as above, using CRL instead
##############
var.mod <- c("species","crl","age","prov")

rs4c <- na.omit(rs[,var.mod])

m4c <- glm(species~ crl*age + prov ,
          data = rs4c,
          family = binomial(link = "logit"))
rs4c[,"predicted.m4c"] <- predict(m4c,type = "response",
                                newdata = rs4c)


dfjm$predicted.m4c <- predict(m4c,
                             newdata = dfjm,
                             type = "response")
plot(dfjm$species,dfjm$predicted.m4c)
dfjm$predsp.m4c <- "Branta canadensis"
dfjm[which(dfjm$predicted.m4c >= 0.5),"predsp.m4c"] <- "Branta hutchinsii"
table(dfjm[,c("species","predsp.m4c")])


orig$predicted.m4c <- predict(m4c,
                             newdata = orig,
                             type = "response")
plot(orig$species,orig$predicted.m4c)
orig$predsp.m4c <- "Branta canadensis"
orig[which(orig$predicted.m4c >= 0.5),"predsp.m4c"] <- "Branta hutchinsii"
table(orig[,c("species","predsp.m4c")])


cv4c <- cv.glm(data = rs4c,
              glmfit = m4c)

cv4c$delta

x11()
plot(rs4c$species,rs4c$predicted.m4c)
summary(m4c)
summary(m4c)$aic
rs4c$predsp.m4c <- "Branta canadensis"
rs4c[which(rs4c$predicted.m4c > 0.5),"predsp.m4c"] <- "Branta hutchinsii"

confmat <- table(rs4c[,c("species","predsp.m4c","age")])

imcangfcack <- confmat["Branta canadensis","Branta hutchinsii","Immature"]
imcackfcang <- confmat["Branta hutchinsii","Branta canadensis","Immature"]
imerror <- sum(c(imcangfcack,imcackfcang))/sum(confmat[,,"Immature"])
imerror
#[1] 0.1519608 - similar to values from random forest below
imcangerror <- imcangfcack/sum(confmat["Branta canadensis",,"Immature"])
imcackerror <- imcackfcang/sum(confmat["Branta hutchinsii",,"Immature"])

adcangfcack <- confmat["Branta canadensis","Branta hutchinsii","Adult"]
adcackfcang <- confmat["Branta hutchinsii","Branta canadensis","Adult"]
aderror <- sum(c(adcangfcack,adcackfcang))/sum(confmat[,,"Adult"])
adcangerror <- adcangfcack/sum(confmat["Branta canadensis",,"Adult"])
adcackerror <- adcackfcang/sum(confmat["Branta hutchinsii",,"Adult"])
aderror
#[1] 0.03244006 - similar to values from random forest below
cangerror <- sum(c(adcangfcack,imcangfcack))/sum(confmat["Branta canadensis",,])
cangerror
# [1] 0.03776224
cackerror <- sum(c(adcackfcang,imcackfcang))/sum(confmat["Branta hutchinsii",,])
cackerror
# [1] 0.1363636
prop.table(table(rs4c[,c("species","predsp.m4c")]),1)
confmat
# , , age = Immature
# 
# predsp.m4
# species             Branta canadensis Branta hutchinsii
# Branta canadensis               104                14
# Branta hutchinsii                17                69
# 
# , , age = Adult
# 
# predsp.m4
# species             Branta canadensis Branta hutchinsii
# Branta canadensis               584                13
# Branta hutchinsii                10               102

rs4c[,"plotcol"] <- "purple"
rs4c[which(rs4c$age == "Adult"),"plotcol"] <- "darkorange"
rs4c$spplot <- runif(length(rs4c$species),-0.02,0.02)+as.integer(rs4c$species)


dfna1 <- expand.grid(crl = seq(min(rs4c$crl),max(rs4c$crl),by = 0.1),
                     age = "Adult",
                     prov = c("Alberta","Saskatchewan","Manitoba"))
dfni1 <- expand.grid(crl = seq(min(rs4c$crl),max(rs4c$crl),by = 0.1),
                     age = "Immature",
                     prov = c("Alberta","Saskatchewan","Manitoba"))
pla1 <- predict(m4c,newdata = dfna1,se.fit = T,type = "response")
pli1 <- predict(m4c,newdata = dfni1,se.fit = T,type = "response")


x11()
plot(y = rs4c$spplot,
     x = rs4c$crl,
     ylab = "",
     xlab = "Central Rectrix Length",
     yaxt = "n",
     col = rs4c$plotcol,
     bty = "l")
axis(side = 2,
     at = c(1,2),
     labels = c("CANG",
                "CACK"),
     las = 1)

for(p in levels(dfna1$prov)){
  dfna <- dfna1[which(dfna1$prov == p),]
  pla <- predict(m4c,newdata = dfna,se.fit = T,type = "response")
  
  dfni <- dfni1[which(dfni1$prov == p),]
  pli <- predict(m4c,newdata = dfni,se.fit = T,type = "response")
  
  polygon(x = c(dfna$crl,rev(dfna$crl)),
          y = 1+c((pla$fit-1.96*pla$se.fit),rev(pla$fit+1.96*pla$se.fit)),
          col = transp.func("darkorange",0.3),
          border = NA)
  
  polygon(x = c(dfni$crl,rev(dfni$crl)),
          y = 1+c((pli$fit-1.96*pli$se.fit),rev(pli$fit+1.96*pli$se.fit)),
          col = transp.func("purple",0.3),
          border = NA)
  
  lines(y = 1+pla$fit,
        x = dfna$crl,
        col = "darkorange",
        lwd = 2)
  lines(y = 1+pli$fit,
        x = dfni$crl,
        col = "purple",
        lwd = 2)
  points(y = rs4c$spplot,
         x = rs4c$crl,
         col = rs4c$plotcol)
  lines(x = rep(dfna$crl[which.min(pla$fit > 0.5)],2),
        y = c(1,2),
        col = transp.func("darkorange",0.5))
  lines(x = rep(dfni$crl[which.min(pli$fit > 0.5)],2),
        y = c(1,2),
        col = transp.func("purple",0.5))
  
  text(x = dfna$crl[which.min(pla$fit > 0.5)],
       y = 1.3+(which(levels(dfna1$prov) == p)/10),
       pos = 4,
       paste0(round(dfna$crl[which.min(pla$fit > 0.5)]),"mm ",p),
       col = "darkorange")
  text(x = dfni$crl[which.min(pli$fit > 0.5)],
       y = 1.3+(which(levels(dfni1$prov) == p)/10),
       pos = 2,
       paste0(round(dfni$crl[which.min(pli$fit > 0.5)]),"mm ",p),
       col = "purple")
  

  # text(x = dfna$crl[which.min(pla$fit > 0.5)],
  #      y = 1.5,
  #      pos = 4,
  #      paste0("Adult ",round(dfna$crl[which.min(pla$fit > 0.5)]),"mm ",round(aderror,2)*100,"%"),
  #      col = "darkorange")
  # text(x = dfni$crl[which.min(pli$fit > 0.5)],
  #      y = 1.5,
  #      pos = 2,
  #      paste0("Immature ",round(dfni$crl[which.min(pli$fit > 0.5)]),"mm ",round(imerror,2)*100,"%"),
  #      col = "purple")
  # text(x = dfna$crl[which.min(pla$fit > 0.5)],
  #      y = 1.4,
  #      pos = 4,
  #      paste0("CANG ",round(adcangerror,2)*100,"% CACK ",round(adcackerror,2)*100,"%"),
  #      col = "darkorange")
  # text(x = dfni$crl[which.min(pli$fit > 0.5)],
  #      y = 1.4,
  #      pos = 2,
  #      paste0("CANG ",round(imcangerror,2)*100,"% CACK ",round(imcackerror,2)*100,"%"),
  #      col = "purple")
  
  
  
  
  
  
}#p





################### same as above but split across three plots in pdf


pdf(file = "crl logistic regression by prov.pdf",
    height = 8,
    width = 8)
for(p in levels(dfna1$prov)){
  rs4ct <- rs4c[which(rs4c$prov == as.character(p)),]
  plot(y = rs4ct$spplot,
       x = rs4ct$crl,
       ylab = "",
       xlab = "Central Rectrix Length",
       yaxt = "n",
       col = rs4ct$plotcol,
       bty = "l",
       main = p,
       xlim = range(rs4c$crl))
  axis(side = 2,
       at = c(1,2),
       labels = c("CANG",
                  "CACK"),
       las = 1)
  
  dfna <- dfna1[which(dfna1$prov == p),]
  pla <- predict(m4c,newdata = dfna,se.fit = T,type = "response")
  
  dfni <- dfni1[which(dfni1$prov == p),]
  pli <- predict(m4c,newdata = dfni,se.fit = T,type = "response")
  
  polygon(x = c(dfna$crl,rev(dfna$crl)),
          y = 1+c((pla$fit-1.96*pla$se.fit),rev(pla$fit+1.96*pla$se.fit)),
          col = transp.func("darkorange",0.3),
          border = NA)
  
  polygon(x = c(dfni$crl,rev(dfni$crl)),
          y = 1+c((pli$fit-1.96*pli$se.fit),rev(pli$fit+1.96*pli$se.fit)),
          col = transp.func("purple",0.3),
          border = NA)
  
  lines(y = 1+pla$fit,
        x = dfna$crl,
        col = "darkorange",
        lwd = 2)
  lines(y = 1+pli$fit,
        x = dfni$crl,
        col = "purple",
        lwd = 2)
  points(y = rs4ct$spplot,
         x = rs4ct$crl,
         col = rs4ct$plotcol)
  
  
  feath = "crl"
  
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Adult" &
                       logregthresh$prov == p),"cutoff minimum CANG"] = dfna$crl[which.min(pla$fit > 0.5)]
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Immature" &
                       logregthresh$prov == p),"cutoff minimum CANG"] = dfni$crl[which.min(pli$fit > 0.5)]
  
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Adult" &
                       logregthresh$prov == p),"lower quartile CACK"] = dfna$crl[which.min(pla$fit > 0.75)]
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Immature" &
                       logregthresh$prov == p),"lower quartile CACK"] = dfni$crl[which.min(pli$fit > 0.75)]
  
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Adult" &
                       logregthresh$prov == p),"upper quartile CANG"] = dfna$crl[which.min(pla$fit > 0.25)]
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Immature" &
                       logregthresh$prov == p),"upper quartile CANG"] = dfni$crl[which.min(pli$fit > 0.25)]
  
  
  lines(x = rep(dfna$crl[which.min(pla$fit > 0.5)],2),
        y = c(1,2),
        col = transp.func("darkorange",0.7),
        lwd = 2)
  lines(x = rep(dfni$crl[which.min(pli$fit > 0.5)],2),
        y = c(1,2),
        col = transp.func("purple",0.7),
        lwd = 2)
  
  lines(x = rep(dfna$crl[which.min(pla$fit > 0.75)],2),
        y = c(1,2),
        col = transp.func("darkorange",0.3))
  lines(x = rep(dfni$crl[which.min(pli$fit > 0.75)],2),
        y = c(1,2),
        col = transp.func("purple",0.3))
  
  
  lines(x = rep(dfna$crl[which.min(pla$fit > 0.25)],2),
        y = c(1,2),
        col = transp.func("darkorange",0.3))
  lines(x = rep(dfni$crl[which.min(pli$fit > 0.25)],2),
        y = c(1,2),
        col = transp.func("purple",0.3))
  
  text(x = dfna$crl[which.min(pla$fit > 0.5)],
       y = 1.4+(which(levels(dfna1$prov) == p)/10),
       pos = 4,
       paste0(round(dfna$crl[which.min(pla$fit > 0.5)]),"mm "),
       col = "darkorange")
  text(x = dfni$crl[which.min(pli$fit > 0.5)],
       y = 1.4+(which(levels(dfni1$prov) == p)/10),
       pos = 2,
       paste0(round(dfni$crl[which.min(pli$fit > 0.5)]),"mm "),
       col = "purple")
  
  
  text(x = dfna$crl[which.min(pla$fit > 0.5)],
       y = 1.3+(which(levels(dfna1$prov) == p)/10),
       pos = 4,
       paste0(round(dfna$crl[which.min(pla$fit > 0.75)])," : ",round(dfna$crl[which.min(pla$fit > 0.25)]),"mm "),
       col = "darkorange")
  text(x = dfni$crl[which.min(pli$fit > 0.5)],
       y = 1.3+(which(levels(dfni1$prov) == p)/10),
       pos = 2,
       paste0(round(dfni$crl[which.min(pli$fit > 0.75)])," : ",round(dfni$crl[which.min(pli$fit > 0.25)]),"mm "),
       col = "purple")
  
  
}#p

dev.off()








##################### same as above but for most lateral feather


##############
var.mod <- c("species","mlrl","age","prov")

rs4m <- na.omit(rs[,var.mod])

m4m <- glm(species~ mlrl*age + prov ,
          data = rs4m,
          family = binomial(link = "logit"))
rs4m[,"predicted.m4m"] <- predict(m4m,type = "response",
                                newdata = rs4m)


dfjm$predicted.m4m <- predict(m4m,
                             newdata = dfjm,
                             type = "response")
plot(dfjm$species,dfjm$predicted.m4m)
dfjm$predsp.m4m <- "Branta canadensis"
dfjm[which(dfjm$predicted.m4m >= 0.5),"predsp.m4m"] <- "Branta hutchinsii"
table(dfjm[,c("species","predsp.m4m")])


orig$predicted.m4m <- predict(m4m,
                             newdata = orig,
                             type = "response")
plot(orig$species,orig$predicted.m4m)
orig$predsp.m4m <- "Branta canadensis"
orig[which(orig$predicted.m4m >= 0.5),"predsp.m4m"] <- "Branta hutchinsii"
table(orig[,c("species","predsp.m4m")])


cv4m <- cv.glm(data = rs4m,
              glmfit = m4m)

cv4m$delta

x11()
plot(rs4m$species,rs4m$predicted.m4m)
summary(m4m)
summary(m4m)$aic
rs4m$predsp.m4m <- "Branta canadensis"
rs4m[which(rs4m$predicted.m4m > 0.5),"predsp.m4m"] <- "Branta hutchinsii"

confmat <- table(rs4m[,c("species","predsp.m4m","age")])

imcangfcack <- confmat["Branta canadensis","Branta hutchinsii","Immature"]
imcackfcang <- confmat["Branta hutchinsii","Branta canadensis","Immature"]
imerror <- sum(c(imcangfcack,imcackfcang))/sum(confmat[,,"Immature"])
imerror
#[1] 0.1519608 - similar to values from random forest below
imcangerror <- imcangfcack/sum(confmat["Branta canadensis",,"Immature"])
imcackerror <- imcackfcang/sum(confmat["Branta hutchinsii",,"Immature"])

adcangfcack <- confmat["Branta canadensis","Branta hutchinsii","Adult"]
adcackfcang <- confmat["Branta hutchinsii","Branta canadensis","Adult"]
aderror <- sum(c(adcangfcack,adcackfcang))/sum(confmat[,,"Adult"])
adcangerror <- adcangfcack/sum(confmat["Branta canadensis",,"Adult"])
adcackerror <- adcackfcang/sum(confmat["Branta hutchinsii",,"Adult"])
aderror
#[1] 0.03244006 - similar to values from random forest below
cangerror <- sum(c(adcangfcack,imcangfcack))/sum(confmat["Branta canadensis",,])
cangerror
# [1] 0.03776224
cackerror <- sum(c(adcackfcang,imcackfcang))/sum(confmat["Branta hutchinsii",,])
cackerror
# [1] 0.1363636
prop.table(table(rs4m[,c("species","predsp.m4m")]),1)
confmat
# , , age = Immature
# 
# predsp.m4m
# species             Branta canadensis Branta hutchinsii
# Branta canadensis               104                14
# Branta hutchinsii                17                69
# 
# , , age = Adult
# 
# predsp.m4m
# species             Branta canadensis Branta hutchinsii
# Branta canadensis               584                13
# Branta hutchinsii                10               102

rs4m[,"plotcol"] <- "purple"
rs4m[which(rs4m$age == "Adult"),"plotcol"] <- "darkorange"
rs4m$spplot <- runif(length(rs4m$species),-0.02,0.02)+as.integer(rs4m$species)


dfna1 <- expand.grid(mlrl = seq(min(rs4m$mlrl),max(rs4m$mlrl),by = 0.1),
                     age = "Adult",
                     prov = c("Alberta","Saskatchewan","Manitoba"))
dfni1 <- expand.grid(mlrl = seq(min(rs4m$mlrl),max(rs4m$mlrl),by = 0.1),
                     age = "Immature",
                     prov = c("Alberta","Saskatchewan","Manitoba"))
pla1 <- predict(m4m,newdata = dfna1,se.fit = T,type = "response")
pli1 <- predict(m4m,newdata = dfni1,se.fit = T,type = "response")


x11()
plot(y = rs4m$spplot,
     x = rs4m$mlrl,
     ylab = "",
     xlab = "Most Lateral Rectrix Length",
     yaxt = "n",
     col = rs4m$plotcol,
     bty = "l")
axis(side = 2,
     at = c(1,2),
     labels = c("CANG",
                "CACK"),
     las = 1)

for(p in levels(dfna1$prov)){
  dfna <- dfna1[which(dfna1$prov == p),]
  pla <- predict(m4m,newdata = dfna,se.fit = T,type = "response")
  
  dfni <- dfni1[which(dfni1$prov == p),]
  pli <- predict(m4m,newdata = dfni,se.fit = T,type = "response")
  
  polygon(x = c(dfna$mlrl,rev(dfna$mlrl)),
          y = 1+c((pla$fit-1.96*pla$se.fit),rev(pla$fit+1.96*pla$se.fit)),
          col = transp.func("darkorange",0.3),
          border = NA)
  
  polygon(x = c(dfni$mlrl,rev(dfni$mlrl)),
          y = 1+c((pli$fit-1.96*pli$se.fit),rev(pli$fit+1.96*pli$se.fit)),
          col = transp.func("purple",0.3),
          border = NA)
  
  lines(y = 1+pla$fit,
        x = dfna$mlrl,
        col = "darkorange",
        lwd = 2)
  lines(y = 1+pli$fit,
        x = dfni$mlrl,
        col = "purple",
        lwd = 2)
  points(y = rs4m$spplot,
         x = rs4m$mlrl,
         col = rs4m$plotcol)
  lines(x = rep(dfna$mlrl[which.min(pla$fit > 0.5)],2),
        y = c(1,2),
        col = transp.func("darkorange",0.5))
  lines(x = rep(dfni$mlrl[which.min(pli$fit > 0.5)],2),
        y = c(1,2),
        col = transp.func("purple",0.5))
  
  text(x = dfna$mlrl[which.min(pla$fit > 0.5)],
       y = 1.3+(which(levels(dfna1$prov) == p)/10),
       pos = 4,
       paste0(round(dfna$mlrl[which.min(pla$fit > 0.5)]),"mm ",p),
       col = "darkorange")
  text(x = dfni$mlrl[which.min(pli$fit > 0.5)],
       y = 1.3+(which(levels(dfni1$prov) == p)/10),
       pos = 2,
       paste0(round(dfni$mlrl[which.min(pli$fit > 0.5)]),"mm ",p),
       col = "purple")
  
  # text(x = dfna$mlrl[which.min(pla$fit > 0.5)],
  #      y = 1.5,
  #      pos = 4,
  #      paste0("Adult ",round(dfna$mlrl[which.min(pla$fit > 0.5)]),"mm ",round(aderror,2)*100,"%"),
  #      col = "darkorange")
  # text(x = dfni$mlrl[which.min(pli$fit > 0.5)],
  #      y = 1.5,
  #      pos = 2,
  #      paste0("Immature ",round(dfni$mlrl[which.min(pli$fit > 0.5)]),"mm ",round(imerror,2)*100,"%"),
  #      col = "purple")
  # text(x = dfna$mlrl[which.min(pla$fit > 0.5)],
  #      y = 1.4,
  #      pos = 4,
  #      paste0("CANG ",round(adcangerror,2)*100,"% CACK ",round(adcackerror,2)*100,"%"),
  #      col = "darkorange")
  # text(x = dfni$mlrl[which.min(pli$fit > 0.5)],
  #      y = 1.4,
  #      pos = 2,
  #      paste0("CANG ",round(imcangerror,2)*100,"% CACK ",round(imcackerror,2)*100,"%"),
  #      col = "purple")
  
  
  
  
  
  
}#p





################### same as above but split across three plots in pdf


pdf(file = "mrl logistic regression by prov.pdf",
    height = 8,
    width = 8)
for(p in levels(dfna1$prov)){
  rs4mt <- rs4m[which(rs4m$prov == as.character(p)),]
  plot(y = rs4mt$spplot,
       x = rs4mt$mlrl,
       ylab = "",
       xlab = "Most Lateral Rectrix Length",
       yaxt = "n",
       col = rs4mt$plotcol,
       bty = "l",
       main = p,
       xlim = range(rs4m$mlrl))
  axis(side = 2,
       at = c(1,2),
       labels = c("CANG",
                  "CACK"),
       las = 1)
  
  dfna <- dfna1[which(dfna1$prov == p),]
  pla <- predict(m4m,newdata = dfna,se.fit = T,type = "response")
  
  dfni <- dfni1[which(dfni1$prov == p),]
  pli <- predict(m4m,newdata = dfni,se.fit = T,type = "response")
  
  polygon(x = c(dfna$mlrl,rev(dfna$mlrl)),
          y = 1+c((pla$fit-1.96*pla$se.fit),rev(pla$fit+1.96*pla$se.fit)),
          col = transp.func("darkorange",0.3),
          border = NA)
  
  polygon(x = c(dfni$mlrl,rev(dfni$mlrl)),
          y = 1+c((pli$fit-1.96*pli$se.fit),rev(pli$fit+1.96*pli$se.fit)),
          col = transp.func("purple",0.3),
          border = NA)
  
  lines(y = 1+pla$fit,
        x = dfna$mlrl,
        col = "darkorange",
        lwd = 2)
  lines(y = 1+pli$fit,
        x = dfni$mlrl,
        col = "purple",
        lwd = 2)
  points(y = rs4mt$spplot,
         x = rs4mt$mlrl,
         col = rs4mt$plotcol)
  
  
  feath = "mlrl"
  
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Adult" &
                       logregthresh$prov == p),"cutoff minimum CANG"] = dfna$mlrl[which.min(pla$fit > 0.5)]
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Immature" &
                       logregthresh$prov == p),"cutoff minimum CANG"] = dfni$mlrl[which.min(pli$fit > 0.5)]
  
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Adult" &
                       logregthresh$prov == p),"lower quartile CACK"] = dfna$mlrl[which.min(pla$fit > 0.75)]
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Immature" &
                       logregthresh$prov == p),"lower quartile CACK"] = dfni$mlrl[which.min(pli$fit > 0.75)]
  
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Adult" &
                       logregthresh$prov == p),"upper quartile CANG"] = dfna$mlrl[which.min(pla$fit > 0.25)]
  
  logregthresh[which(logregthresh$feather == feath &
                       logregthresh$age == "Immature" &
                       logregthresh$prov == p),"upper quartile CANG"] = dfni$mlrl[which.min(pli$fit > 0.25)]
  
  
  lines(x = rep(dfna$mlrl[which.min(pla$fit > 0.5)],2),
        y = c(1,2),
        col = transp.func("darkorange",0.7),
        lwd = 2)
  lines(x = rep(dfni$mlrl[which.min(pli$fit > 0.5)],2),
        y = c(1,2),
        col = transp.func("purple",0.7),
        lwd = 2)
  
  lines(x = rep(dfna$mlrl[which.min(pla$fit > 0.75)],2),
        y = c(1,2),
        col = transp.func("darkorange",0.3))
  lines(x = rep(dfni$mlrl[which.min(pli$fit > 0.75)],2),
        y = c(1,2),
        col = transp.func("purple",0.3))
  
  
  lines(x = rep(dfna$mlrl[which.min(pla$fit > 0.25)],2),
        y = c(1,2),
        col = transp.func("darkorange",0.3))
  lines(x = rep(dfni$mlrl[which.min(pli$fit > 0.25)],2),
        y = c(1,2),
        col = transp.func("purple",0.3))
  
  text(x = dfna$mlrl[which.min(pla$fit > 0.5)],
       y = 1.4+(which(levels(dfna1$prov) == p)/10),
       pos = 4,
       paste0(round(dfna$mlrl[which.min(pla$fit > 0.5)]),"mm "),
       col = "darkorange")
  text(x = dfni$mlrl[which.min(pli$fit > 0.5)],
       y = 1.4+(which(levels(dfni1$prov) == p)/10),
       pos = 2,
       paste0(round(dfni$mlrl[which.min(pli$fit > 0.5)]),"mm "),
       col = "purple")
  
  
  text(x = dfna$mlrl[which.min(pla$fit > 0.5)],
       y = 1.3+(which(levels(dfna1$prov) == p)/10),
       pos = 4,
       paste0(round(dfna$mlrl[which.min(pla$fit > 0.75)])," : ",round(dfna$mlrl[which.min(pla$fit > 0.25)]),"mm "),
       col = "darkorange")
  text(x = dfni$mlrl[which.min(pli$fit > 0.5)],
       y = 1.3+(which(levels(dfni1$prov) == p)/10),
       pos = 2,
       paste0(round(dfni$mlrl[which.min(pli$fit > 0.75)])," : ",round(dfni$mlrl[which.min(pli$fit > 0.25)]),"mm "),
       col = "purple")
  
  
}#p

dev.off()
 


########### merging all predicted values with the original rs data frame

rsout <- merge(rs,rs4[,c("predicted.m4","predsp.m4")],
               by = 0,
               all.x = T)
row.names(rsout) <- rsout$Row.names
rsout <- rsout[,-which(names(rsout) == "Row.names")]
rsout <- merge(rsout,rs4m[,c("predicted.m4m","predsp.m4m")],
               by = 0,
               all.x = T)
row.names(rsout) <- rsout$Row.names
rsout <- rsout[,-which(names(rsout) == "Row.names")]
rsout <- merge(rsout,rs4c[,c("predicted.m4c","predsp.m4c")],
               by = 0,
               all.x = T)
row.names(rsout) <- rsout$Row.names
rsout <- rsout[,-which(names(rsout) == "Row.names")]


write.csv(rsout,file = "rsout.csv")



rsout[which(is.na(rsout$predsp.m4)),c("lrl","prov")]
rsout[which(is.na(rsout$predsp.m4m)),c("mlrl","prov")]
rsout[which(is.na(rsout$predsp.m4c)),c("crl","prov")]

rsout[,"species.by.feather"] <- rsout[,"predsp.m4"]
rsout[,"pspecies.by.feather"] <- rsout[,"predicted.m4"]
notlrl <- which(is.na(rsout[,"predsp.m4"]))


rsout[notlrl,"species.by.feather"] <- rsout[notlrl,"predsp.m4m"]
rsout[notlrl,"pspecies.by.feather"] <- rsout[notlrl,"predicted.m4m"]

notlrlormlrl <- which(is.na(rsout[,"predsp.m4"]) & is.na(rsout[,"predsp.m4m"]))

rsout[notlrlormlrl,"species.by.feather"] <- rsout[notlrlormlrl,"predsp.m4c"]
rsout[notlrlormlrl,"pspecies.by.feather"] <- rsout[notlrlormlrl,"predicted.m4c"]


rsout[,"missid.by.feather"] <- "no"
  rsout[which(rsout$species != rsout$`species.by.feather`),"missid.by.feather"] <- "yes"

  
histogram(~pspecies.by.feather | missid.by.feather,
          data = rsout,
          type =  "count")

rsout[,"species.by.nhs"] <- "Branta canadensis"
rsout[which(rsout$sporig == 1721),"species.by.nhs"] <- "Branta hutchinsii"


table(rsout$species,rsout$species.by.nhs)

#                 Branta canadensis Branta hutchinsii
# Branta canadensis               704                26
# Branta hutchinsii                39               160

table(rsout$species,rsout$species.by.feather)
# 
#                     Branta canadensis Branta hutchinsii
# Branta canadensis               702                26
# Branta hutchinsii                25               174

table(rsout$species.by.feather,rsout$species.by.nhs)
#                       Branta canadensis Branta hutchinsii
# Branta canadensis               710                17
# Branta hutchinsii                33               167


# library(foreign)
# sashome <- "C:/Program Files/SASHome/SASFoundation/9.4"
# 
# scs14 <- read.ssd(getwd(),
#                   "scs14e",
#                   sascmd = file.path(sashome, "sas.exe"))


write.csv(row.names = F,
          logregthresh,
          "Cutoffs for CANG-CACK determination.csv")

######################### tree models




##########











######## show variable importance of all possible variables - Random Forest
library(randomForest)
var.mod <- c("species","lrl","lrd","prov","age","dhunt")
rs4a <- na.omit(rs[,var.mod])
#rs4a <- rs[,var.mod]

#### adults
rf1 <- randomForest(species~.,
                    data = rs4a,
                    na.action = na.omit, 
                    importance = T, 
                    localImp = T,
                    sampsize = rep(ceiling(as.numeric(table(rs4a$species)[2])*0.66),times = 2))



rs4a$predsp.rf1 <- predict(rf1,
                           newdata = rs4a,
                           type = "response")
rs4a$predicted.rf1 <- predict(rf1,
                           newdata = rs4a,
                           type = "prob")[,"Branta hutchinsii"]

dfjm$predsp.rf1 <- predict(rf1,
                                  newdata = dfjm,
                                  type = "response")
dfjm$predicted.rf1 <- predict(rf1,
                              newdata = dfjm,
                              type = "prob")[,"Branta hutchinsii"]

plot(dfjm$species,dfjm$predsp.rf)
# dfjm$predsp.rf1 <- "Branta canadensis"
# dfjm[which(dfjm$predicted.rf1 >= 0.5),"predsp.rf1"] <- "Branta hutchinsii"
#table(dfjm[,c("species","predsp.rf1","age")])

orig$predsp.rf1 <- predict(rf1,
                          newdata = orig,
                          type = "response")
orig$predicted.rf1 <- predict(rf1,
                              newdata = orig,
                              type = "prob")[,"Branta hutchinsii"]
plot(orig$species,orig$predsp.rf1)
# orig$predsp.rf1 <- "Branta canadensis"
# orig[which(orig$predicted.rf1 >= 0.5),"predsp.rf1"] <- "Branta hutchinsii"
#table(orig[,c("species","predsp.rf1","age")])

pdf(file = "predictive accuracy random forest.pdf",
    width = 11,
    height = 8.5)
par(mfcol = c(2,3),
    mar = c(3,4,3,1),
    oma = c(1,0,0,0))
err.pl(mod = rf1,
       dt = rs4a,
       predcl = "predsp.rf1")
err.pl(mod = rf1,
       dt = orig,
       predcl = "predsp.rf1",
       descr = "Original Study")
err.pl(mod = rf1,
       dt = dfjm,
       predcl = "predsp.rf1",
       descr = "Leafloor")
dev.off()



logreg[sj,"model"] <- paste("random forest")
#logreg[sj,"AIC"] <-  summary(rf1)$aic
#logreg[sj,"cvdelta"] <-   cv1ss$delta[2]
logreg[sj,"p.miss.hutchinsii"] <- prop.table(table(rs4a[,c("species","predsp.rf1")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis"] <- prop.table(table(rs4a[,c("species","predsp.rf1")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.im"] <- prop.table(table(rs4a[,c("species","predsp.rf1","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Immature"]  
logreg[sj,"p.miss.canadensis.im"] <- prop.table(table(rs4a[,c("species","predsp.rf1","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Immature"]    

logreg[sj,"p.miss.hutchinsii.ad"] <- prop.table(table(rs4a[,c("species","predsp.rf1","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Adult"]  
logreg[sj,"p.miss.canadensis.ad"] <- prop.table(table(rs4a[,c("species","predsp.rf1","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Adult"]    

logreg[sj,"p.miss.hutchinsii.orig"] <- prop.table(table(orig[,c("species","predsp.rf1")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.orig"] <- prop.table(table(orig[,c("species","predsp.rf1")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.rf1")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.rf1")]),1)["Branta canadensis","Branta hutchinsii"]    


sj <- sj+1










# 
usage <- varUsed(rf1)
names(usage) <- names(rs4a)[-c(1,7,8)]

rs4a[,"pred.rf1"] <- rf1$predicted
# 
# confmat <- table(rs4a[,c("species","pred.rf1","age")])
# 
# imcangfcack <- confmat["Branta canadensis","Branta hutchinsii","Immature"]
# imcackfcang <- confmat["Branta hutchinsii","Branta canadensis","Immature"]
# imerror <- sum(c(imcangfcack,imcackfcang))/sum(confmat[,,"Immature"])
# imerror
# #[1] 0.1342282
# adcangfcack <- confmat["Branta canadensis","Branta hutchinsii","Adult"]
# adcackfcang <- confmat["Branta hutchinsii","Branta canadensis","Adult"]
# aderror <- sum(c(adcangfcack,adcackfcang))/sum(confmat[,,"Adult"])
# aderror
# #[1] 0.02901354
# cangerror <- sum(c(adcangfcack,imcangfcack))/sum(confmat["Branta canadensis",,])
# cangerror
# #[1] 0.05697446
# cackerror <- sum(c(adcackfcang,imcackfcang))/sum(confmat["Branta hutchinsii",,])
# cackerror
# #[1] 0.03821656
# confmat
# # , , age = Immature
# # 
# # pred.rf1
# # species             Branta canadensis Branta hutchinsii
# # Branta canadensis                59                18
# # Branta hutchinsii                 2                70
# # 
# # , , age = Adult
# # 
# # pred.rf1
# # species             Branta canadensis Branta hutchinsii
# # Branta canadensis               421                11
# # Branta hutchinsii                 4                81
# 
# 
# table(rs4a[,c("species","pred.rf1")])
# 
# 
# x11()
# varImpPlot(rf1)
# 
# 
# 
# var.mod2 <- c("species","lrl","lrd","age")
# rs5 <- na.omit(rs[which(rs$prov != "British Columbia"),var.mod2])
# 
# #### adults
# rf2 <- randomForest(species~.,
#                     data = rs5,
#                     na.action = na.omit, 
#                     importance = T, 
#                     localImp = T,
#                     sampsize = rep(ceiling(as.numeric(table(rs2$species)[2])*0.66),times = 2))
# 
# dfjm$predsp.rf2 <- predict(rf2,
#                           newdata = dfjm,
#                           type = "response")
# plot(dfjm$species,dfjm$predsp.rf2)
# #dfjm$predsp.rf <- "Branta canadensis"
# #dfjm[which(dfjm$predicted.rf >= 0.5),"predsp.rf"] <- "Branta hutchinsii"
# table(dfjm[,c("species","predsp.rf2","age")])
# 
# orig$predsp.rf2 <- predict(rf2,
#                           newdata = orig,
#                           type = "response")
# plot(orig$species,orig$predsp.rf2)
# #orig$predsp.rf <- "Branta canadensis"
# #orig[which(orig$predicted.rf >= 0.5),"predsp.rf"] <- "Branta hutchinsii"
# table(orig[,c("species","predsp.rf2","age")])
# 
# 
# 
# usage <- varUsed(rf2)
# names(usage) <- names(rs5)[-1]
# rs5[,"pred.rf1"] <- rf2$predicted
# 
# x11()
# varImpPlot(rf2)
# table(rs5[,c("species","pred.rf1")])
# 
# 
# 


######### standard cart using rpart


library(rpart)

#var.mod <- c("species","crl","lrl","crd","lrd","long","age","dhunt","prov")
var.mod <- c("species","crl","lrl","age","dhunt","prov","crd","lrd")

#rs5d <- na.omit(rs[which(rs$age == "Immature"),var.mod])
#rs5dp <- na.omit(rs[which(rs$age == "Immature"),var.mod])
rs5d <- na.omit(rs[,var.mod])
mcart <- rpart(species~lrl+dhunt+prov+age+lrd,
               data = rs5d,
               method = "class",
               control = rpart.control(minsplit = 20))
x11()
plot(mcart,main = "Immature")
text(mcart,use.n = T,cex = 0.7,minlength = 0)
printcp(mcart)
rs5d$predsp.treei <- predict(mcart,type = "class")
rs5d$predicted.treei <- predict(mcart,type = "prob")[,"Branta hutchinsii"]
#table(rs5d[,c("species","predsp.treei")])

#dfjmi <- dfjm[which(dfjm$age == "Immature"),]
dfjm$predsp.treei <- predict(mcart,
                           newdata = dfjm,
                           type = "class")
dfjm$predicted.treei <- predict(mcart,
                             newdata = dfjm,
                             type = "prob")[,"Branta hutchinsii"]
#plot(dfjmi$species,dfjmi$predsp.treei)

#dfjm$predsp.rf <- "Branta canadensis"
#dfjm[which(dfjm$predicted.rf >= 0.5),"predsp.rf"] <- "Branta hutchinsii"
table(dfjm[,c("species","predsp.treei","age","prov")])



orig$predsp.treei <- predict(mcart,
                              newdata = orig,
                              type = "class")
orig$predicted.treei <- predict(mcart,
                             newdata = orig,
                             type = "prob")[,"Branta hutchinsii"]
#plot(orig$species,orig$predsp.treei)

#orig$predsp.rf <- "Branta canadensis"
#orig[which(orig$predicted.rf >= 0.5),"predsp.rf"] <- "Branta hutchinsii"
table(orig[,c("species","predsp.treei","age","prov")])


pdf(file = "predictive accuracy cart.pdf",
    width = 11,
    height = 8.5)
par(mfcol = c(2,3),
    mar = c(3,4,3,1),
    oma = c(1,0,0,0))
err.pl(mod = mcart,
       dt = rs5d,
       predcl = "predsp.treei")
err.pl(mod = mcart,
       dt = orig,
       predcl = "predsp.treei",
       descr = "Original Study")
err.pl(mod = mcart,
       dt = dfjm,
       predcl = "predsp.treei",
       descr = "Leafloor")
dev.off()






logreg[sj,"model"] <- paste("cart")
#logreg[sj,"AIC"] <-  summary(mcart)$aic
#logreg[sj,"cvdelta"] <-   cv1ss$delta[2]
logreg[sj,"p.miss.hutchinsii"] <- prop.table(table(rs5d[,c("species","predsp.treei")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis"] <- prop.table(table(rs5d[,c("species","predsp.treei")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.im"] <- prop.table(table(rs5d[,c("species","predsp.treei","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Immature"]  
logreg[sj,"p.miss.canadensis.im"] <- prop.table(table(rs5d[,c("species","predsp.treei","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Immature"]    

logreg[sj,"p.miss.hutchinsii.ad"] <- prop.table(table(rs5d[,c("species","predsp.treei","age")]),c(1,3))["Branta hutchinsii","Branta canadensis","Adult"]  
logreg[sj,"p.miss.canadensis.ad"] <- prop.table(table(rs5d[,c("species","predsp.treei","age")]),c(1,3))["Branta canadensis","Branta hutchinsii","Adult"]    

logreg[sj,"p.miss.hutchinsii.orig"] <- prop.table(table(orig[,c("species","predsp.treei")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.orig"] <- prop.table(table(orig[,c("species","predsp.treei")]),1)["Branta canadensis","Branta hutchinsii"]    

logreg[sj,"p.miss.hutchinsii.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.treei")]),1)["Branta hutchinsii","Branta canadensis"]  
logreg[sj,"p.miss.canadensis.leafloor"] <- prop.table(table(dfjm[,c("species","predsp.treei")]),1)["Branta canadensis","Branta hutchinsii"]    


sj <- sj+1

write.csv(logreg,"modelsummary.csv",
          row.names = F)


###################### boxplot of the predicted probabilities of hutchinsii


source("sphist func.r")

pdf("predicted species histograms logistic.pdf")
par(mfcol = c(9,2),
    mar = c(0,1,0,0),
    oma = c(5,4,3,1))

sphist(w = -0.2,
       descr = "",
       cola = c("red","blue"),
       predcl = "predicted.m2d",
       dt = rs2d,
       mod = m2d,
       bksize = 0.05)
sphist(w = -0.2,
       descr = "",
       cola = c("red","blue"),
       predcl = "predicted.m2",
       dt = rs2,
       mod = m2,
       bksize = 0.05)

sphist(w = -0.2,
       descr = "",
       cola = c("red","blue"),
       predcl = "predicted.m2s",
       dt = rs2s,
       mod = m2s,
       bksize = 0.05)
sphist(w = -0.2,
       descr = "",
       cola = c("red","blue"),
       predcl = "predicted.m2ss",
       dt = rs2ss,
       mod = m2ss,
       bksize = 0.05)
sphist(w = -0.2,
       descr = "",
       cola = c("red","blue"),
       predcl = "predicted.m1d",
       dt = rs1d,
       mod = m1d,
       bksize = 0.05)

sphist(w = -0.2,
       descr = "",
       cola = c("red","blue"),
       predcl = "predicted.m1",
       dt = rs1,
       mod = m1,
       bksize = 0.05)

sphist(w = -0.2,
       descr = "",
       cola = c("red","blue"),
       predcl = "predicted.m1s",
       dt = rs1s,
       mod = m1s,
       bksize = 0.05)

sphist(w = -0.2,
       descr = "",
       cola = c("red","blue"),
       predcl = "predicted.m1ss",
       dt = rs1ss,
       mod = m1ss,
       bksize = 0.05)
dev.off()





pdf("predicted species histograms random forest.pdf")
par(mfcol = c(9,2),
    mar = c(0,1,0,0),
    oma = c(5,4,3,1))

sphist(w = -0.2,
       descr = "",
       cola = c("red","blue"),
       predcl = "predicted.rf1",
       dt = rs4a,
       mod = rf1,
       bksize = 0.05)

dev.off()



pdf("predicted species histograms cart.pdf")
par(mfcol = c(9,2),
    mar = c(0,1,0,0),
    oma = c(5,4,3,1))

sphist(w = -0.2,
       descr = "",
       cola = c("red","blue"),
       predcl = "predicted.treei",
       dt = rs5d,
       mod = mcart,
       bksize = 0.05)

dev.off()





















# 
# 
# 
# 
# 
# ############### exploring adult only cart models
# rs5d <- na.omit(rs[which(rs$age == "Adult"),var.mod])
# 
# mcarta <- rpart(species~lrl+lrd+dhunt+prov,
#                data = rs5d,
#                method = "class",
#                control = rpart.control(minsplit = 10))
# x11()
# plot(mcarta,main = "Adult")
# text(mcarta,use.n = T,cex = 0.7,minlength = 0)
# printcp(mcarta)
# 
# 
# dfjma <- dfjm[which(dfjm$age == "Adult"),]
# dfjma$predsp.treea <- predict(mcarta,
#                               newdata = dfjma,
#                               type = "class")
# plot(dfjma$species,dfjma$predsp.treea)
# 
# table(dfjma[,c("species","predsp.treea","age")])
# 
# 
# 
# origa <- orig[which(orig$age == "Adult"),]
# origa$predsp.treea <- predict(mcarta,
#                               newdata = origa,
#                               type = "class")
# plot(origa$species,origa$predsp.treea)
# 
# table(origa[,c("species","predsp.treea","age")])
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ######### Bayesian CART ######### using tgp package                  
# library(tgp)
# preds <- c("lrl","long","dhunt","age")
# 
# rs6 <- na.omit(rs[,c("species",preds)])
# rs6$age <- as.integer(rs6$age)-1
# rs6$species <- as.integer(rs6$species)-1
# 
# 
# X <- rs6[,preds]
# Z <- rs6[,"species"]
# #XX <- dat.ab[,c("Longest.Rectrix.Length","Longest.Rectrix.Unfeathered.Quill..Length","Longest.Rectrix.Quill.Dia","longitude","prov.kill0")]  
# XX <- X
# 
# b <- bcart(X = X, 
#            Z = Z,
# #           XX = XX,
#            BTE = c(20000, 100000, 5), 
#            R = 50, 
#            m0r1 = FALSE, 
#            tree = c(0.5,10))##setting the beta to 10 here places emphasis on shallower trees
# x11()
# tgp.trees(b,heights = NULL, nodeinfo=T, print.levels = T)
# 
# rs6[,"pcack.bcart"] <- b$Zp.mean
# rs6[which(b$Zp.mean >= 0.5),"pred.bcart"] <- "Branta hutchinsii"
# rs6[which(b$Zp.mean < 0.5),"pred.bcart"] <- "Branta canadensis"
# 
# 
# 
# 
# 
# 
# #############################alternative
# 
# 
# preds <- c("lrl","long","lrd","age")
# 
# rs6d <- na.omit(rs[,c("species",preds)])
# rs6d$age <- as.integer(rs6d$age)-1
# rs6d$species <- as.integer(rs6d$species)-1
# 
# 
# X <- rs6d[,preds]
# Z <- rs6d[,"species"]
# #XX <- dat.ab[,c("Longest.Rectrix.Length","Longest.Rectrix.Unfeathered.Quill..Length","Longest.Rectrix.Quill.Dia","longitude","prov.kill0")]  
# XX <- X
# 
# bd <- bcart(X = X, 
#            Z = Z,
#            #           XX = XX,
#            BTE = c(20000, 100000, 5), 
#            R = 50, 
#            m0r1 = FALSE, 
#            tree = c(0.5,10))##setting the beta to 10 here places emphasis on shallower trees
# x11()
# tgp.trees(bd,heights = NULL, nodeinfo=T, print.levels = T)
# 
# rs6d[,"pcack.bcart"] <- bd$Zp.mean
# rs6d[which(bd$Zp.mean >= 0.5),"pred.bcart"] <- "Branta hutchinsii"
# rs6d[which(bd$Zp.mean < 0.5),"pred.bcart"] <- "Branta canadensis"
# 
# 
# 
# 
# 
# ############## playing with bcart parameters
# 
# psplit <- function(a = 0.5,b = 2,ds = 1:10){
#   a*(1+ds)^-b
#   }
# 
# 
# aexp <- seq(0.1,3,by = 0.1)
# 
# difa <- list()
# length(difa) <- length(aexp)
# names(difa) <- as.character(aexp)
# i = 0
# for(j in aexp){
#   i = i+1
#   difa[[i]] <- psplit(a = j)
#   difa[[i]] <- difa[[i]]/max(difa[[i]])
# }
# 
# 
# bexp <- seq(0.5,10,by = 0.5)
# 
# difb <- list()
# length(difb) <- length(bexp)
# names(difb) <- as.character(bexp)
# i = 0
# for(j in bexp){
#   i = i+1
#   difb[[i]] <- psplit(b = j)
#   difb[[i]] <- difb[[i]]/max(difb[[i]])
# }
# ############## playing with bcart parameters
# 
# 
# 
# confmat <- table(rs6[,c("species","pred.bcart","age")])
# 
# imcangfcack <- confmat["Branta canadensis","Branta hutchinsii","Immature"]
# imcackfcang <- confmat["Branta hutchinsii","Branta canadensis","Immature"]
# imerror <- sum(c(imcangfcack,imcackfcang))/sum(confmat[,,"Immature"])
# imerror
# #[1] 0.182243
# adcangfcack <- confmat["Branta canadensis","Branta hutchinsii","Adult"]
# adcackfcang <- confmat["Branta hutchinsii","Branta canadensis","Adult"]
# aderror <- sum(c(adcangfcack,adcackfcang))/sum(confmat[,,"Adult"])
# aderror
# #[1] 0.02755906
# cangerror <- sum(c(adcangfcack,imcangfcack))/sum(confmat["Branta canadensis",,])
# cangerror
# #[1] 0.05964467
# cackerror <- sum(c(adcackfcang,imcackfcang))/sum(confmat["Branta hutchinsii",,])
# cackerror
# #[1] 0.06914894
# confmat
# # , , age = Immature
# # 
# # pred.bcart
# # species             Branta canadensis Branta hutchinsii
# # Branta canadensis                95                37
# # Branta hutchinsii                 2                80
# # 
# # , , age = Adult
# # 
# # pred.bcart
# # species             Branta canadensis Branta hutchinsii
# # Branta canadensis               646                10
# # Branta hutchinsii                11                95
# 
# 
# 





