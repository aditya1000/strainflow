######################
Subject: Modeling new covid cases with blip dimentions
Author: Aditya Nagori

#######################

library(lubridate)
library(ROCR)
library(randomForest)
library(nlme)
library(lme4)
library(Boruta)
library(DMwR)
library(scales)
source("C:/aditya/work/work/variabilty_boruta/functions/reformulate_random.R")
#for(t in c("monthy"))
#list.files(C:/aditya/strainflow/blip CSVs/monthly/)
shift <- function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}

my_rsq <- function (x, y) cor(x, y) ^ 2

mean_absolute_percentage_error<- function(y_true, y_pred){
  mean(abs((y_true - y_pred) / y_true),na.rm = T) * 100
}


R0_df <- read.csv("C:/aditya/strainflow/R0_with_total_cases.csv")
levels(R0_df$country) <- c("Australia", "Brazil", "Canada", "China", "France",
                           "Germany", "India", "Italy", 
                           "Japan","Mexico", "South-Africa", 
                           "England", "USA")      


R0_df <- R0_df[which(apply(is.na(R0_df), 1, sum)==0),] 
R0_df$R0 <- as.numeric(as.character(unlist(R0_df$R0)))
R0_df <- R0_df[-which(R0_df$R0 > 10),]
# co <- c("US","United Kingdom","Australia", "France", "Japan" , "Germany", "Italy",
#         "India", "Canada")

#list_embs <- list.files("C:/aditya/strainflow/w2vec-monthly_avg/", pattern = "\\.csv")
list_ <- list.files("C:/aditya/strainflow/sampling_blips/", pattern = "\\.csv")


main_path <- "C:/aditya/strainflow/sampling_blips/"
main_path_embs <- "C:/aditya/strainflow/sampling_blips/"
list_ <- list_[-grep("Wales|Ireland|Scotland", list_, ignore.case = T)]

temp <- NULL
for(fil in list_){
  state_nam <- stringr::str_split(fil, n=2, pattern = "_")[[1]][1]  
  aus_month <- read.csv(paste0(main_path, fil))
  #colnames(aus_month)[2:37] <- gsub("X", "Blip_Dim", colnames(aus_month)[2:37]) 
  
  my_rescale <- function(x){rescale(x, c(0.0001 , 1))}
  
  aus_month$Collection_Date <- strptime(aus_month$Collection_Date, 
                                        "%Y-%m-%d")
  aus_month$Collection_Date <- format(aus_month$Collection_Date,
                                      format = "%y-%m-%d")
  
  aus_month$date <-  paste0(month(as.Date(aus_month$Collection_Date)),"-",
                            year(as.Date(aus_month$Collection_Date)))
  
  aus_month <- merge(aus_month, R0_df[R0_df$country ==state_nam,c(1,2,3,6)], by = "date" ,all= F)
  
  aus_month <- aus_month[order(aus_month$Collection_Date, decreasing = F),]
  aus_month$month <- month(as.Date(aus_month$Collection_Date))
  
  ##aus_month <- aus_month[,-grep("lag", colnames(aus_month))]
  aus_month$new_cases  <- c(aus_month$total_cases[1], diff(aus_month$total_cases))
  
  names_for_lag <- colnames(aus_month)[c(44)]
  names_taken <- names_for_lag
  for(k in 1:2){
    for(nam in names_taken){
      aus_month[,nam] <-  as.numeric(aus_month[,nam])
      aus_month <- data.frame(cbind(aus_month,lead = shift(aus_month[,nam],+k)))
      #air_pollution_cls_lag$clusters <- as.factor(air_pollution_cls_lag$clusters)
      colnames(aus_month)[which(colnames(aus_month)=="lead")] <- paste0(nam,"_lead_", k)
    }
  }
  # 
  #aus_month$total_cases_Diff  <- c(diff(aus_month$total_cases, 2),NA, NA) 
 # aus_month$total_cases_Diff  <- c(diff(aus_month$total_cases), NA) 
  
  #aus_month[,c(3:38, 43)] <-  apply(aus_month[,c(3:38, 43)], 2, my_rescale)
  temp <- rbind(temp, aus_month)
}



dat_merge <- temp[which(apply(is.na(temp), 1, sum) == 0), ] 
# colnames(dat_merge)
# dat_merge$tc_change <- 0
# dat_merge$tc_change[which((dat_merge$total_cases_Diff/dat_merge$total_cases)>1)] <- 1
# table(dat_merge$tc_change)

#boxplot(dat_merge$R0_lag_2)
# 
# clust <- kmeans(dat_merge$total_cases, 2)
# dat_merge$tc_clust <- clust$cluster
# 
# max(dat_merge$total_cases[dat_merge$tc_clust== 1]) #1.463393
# min(dat_merge$total_cases[dat_merge$tc_clust== 1]) #1.0002
# 
# max(dat_merge$total_cases[dat_merge$tc_clust== 2])#3.32335
# min(dat_merge$total_cases[dat_merge$tc_clust== 2])#1.66993
# 
# max(dat_merge$R0[dat_merge$tc_clust== 3])#5.987
# min(dat_merge$R0[dat_merge$tc_clust== 3])#5.987
# 
# 
# dat_merge$R0_clust_1 <- 0
# dat_merge$R0_clust_1[dat_merge$R0 > 2.356129] <- 1

# library(nlme)
# temp <- NULL
# 
# for(nam  in names_for_lag[-37]){
#   k = 1  
#   my_formula <- reformulate(termlabels = nam,
#                   response = paste0("R0"))
#                   
#   fit_lme <- lme(my_formula, random = ~1|country, data = dat_merge)
#   summ_ <- summary(fit_lme)
#   #str(summ_)
#   dt_res <-  c(my_formula, as.character(summ_$tTable[2,1]), 
#                as.character(summ_$tTable[2,2]),
#                as.character(summ_$tTable[2,5] ))
#   temp <- rbind(temp, dt_res)
# }
# 
# temp <- data.frame(temp)
# temp <- data.frame(sapply(temp , as.character))
# 
# colnames(temp) <- c("Formula", "coefficient", "Std_error", "p-value")
# 
# temp$Formula <-  (gsub("lag", "lead", temp$Formula))
# temp <- data.frame(sapply(temp, as.character))
# 
# write.csv(temp, paste0("C:/aditya/strainflow/results/lme_R0_blip_dim_pred.csv"),
#                        row.names = F)
################
#dat_merge <- dat_merge[which(dat_merge$Collection_Date > "20-02-29" &
#                         dat_merge$Collection_Date < "21-04-30"),]

write.csv(dat_merge, "C:/aditya/strainflow/results_sampling_blips/dat_merged_blip_dim_R0_cases_for_model_lead.csv", row.names = F)
          ##

termobj <- colnames(dat_merge)[c(3:38,40)] 
#train_dat <- train_dat[,c(3:38,40, ncol(train_dat))]


dat_merge$new_cases_lead_1 <- as.numeric(dat_merge$new_cases_lead_1)

temp_bor_per <- NULL
# for(i in seq(100, 2000, 200)){
#   for(j in c(4:18)){
# Bor <- Boruta(reformulate(termlabels = termobj[1:37],
#                           response = "new_cases"),
#               data= dat_merge, ntree = i, mtry = j)
# termobj_sel <- c(Boruta::getSelectedAttributes(Bor))
# 
# my_formula <- reformulate(termlabels = termobj_sel[-grep("country", termobj_sel)],
#                response = "new_cases")
# fit_rf <- randomForest::randomForest(my_formula, data= dat_merge, 
#                                      ntree = i, mtry = j)
# rsq_ = fit_rf$rsq[1]
# rsq_ = fit_rf$mse[1]
# temp_bor_per <- rbind(temp_bor_per, c(i,j, rsq_))
# }
# }
# 
# Bor <- Boruta(reformulate(termlabels = termobj[1:37],
#                           response = "new_cases"),
#               data= dat_merge, ntree = 1000)
# 
# termobj_sel <- c(Boruta::getSelectedAttributes(Bor))
# 
# 
set.seed(3)

# Bor <- Boruta(reformulate(termlabels = termobj[1:37],
#                           response = "new_cases"),
#               data= dat_merge, ntree = 1500, mtry =15)
# termobj_sel <- c(Boruta::getSelectedAttributes(Bor))

Bor <- Boruta(reformulate(termlabels = termobj[1:37],
                          response = "new_cases_lead_1"),
              data= dat_merge, ntree = 1000)
termobj_sel <- c(Boruta::getSelectedAttributes(Bor))


pdf(paste0("C:/aditya/strainflow/results_sampling_blips/bor_new_cases_lead2_sam_wto_rescaling.pdf"), width = 10, height = 6)
par(mar=c(10,5,5,5))
plot(Bor, las =3 , xlab= "", main = "Response:Next 2 Month total New cases")
dev.off()

my_formula <- reformulate_random(termlabels = termobj_sel[-length(termobj_sel)],
                                 "new_cases_lead_1" ,
                                 randeff = "(1|country)")

fit_lmer <- lmer(my_formula, data = dat_merge)
summary(fit_lmer)
stepc_lmer  <- cAIC4::stepcAIC(fit_lmer)
stepc_lmer$finalModel
#################################
my_formula <- reformulate(termlabels = termobj_sel[-length(termobj_sel)],
                          response = "new_cases_lead_1")

fit_lme <- lme(my_formula, random = ~1|country, data = dat_merge)
summ_ <- summary(fit_lme)

times_blip18 <- (summ_$coefficients$fixed/summ_$coefficients$fixed[1])*10 
summ_t <- data.frame(summ_$tTable)

df_per_change <-data.frame(cbind(round(times_blip18, 2), 
                    round((summ_t$Std.Error/summ_t$Std.Error[1])*10, 2),
                    round(summ_t$p.value,4)))


colnames(df_per_change) <- c("% change", "standard error", "p-value")
write.csv(df_per_change, "C:/aditya/strainflow/results_sampling_blips/per_in_new_cases_lead_1_wto_resc.csv")


################## Tempporal splits #########################  
test_dat <- dat_merge[dat_merge$date %in%c("12-20" , "1-21"),]
train_dat <- dat_merge[-which(dat_merge$date %in% c("12-20",  "1-21")),]

termobj <- colnames(dat_merge)[c(3:38,40)]
dat_merge$new_cases_lead_1 <- as.numeric(dat_merge$new_cases_lead_1)

# train_dat_br <- train_dat[train_dat$country == "England",]
# test_dat_br <- test_dat[test_dat$country == "England",]
set.seed(3)
Bor <- Boruta(reformulate(termlabels = termobj,
              response = "new_cases_lead_1"),
              data= train_dat, ntree = 1000)
              #ntree = 1400, mtry = 18)


pdf("C:/aditya/strainflow/results_sampling_blips/bor_train_lead2.pdf", width = 10, height = 6)
plot(Bor, las =2, xlab = "", main = "Total New Cases")
dev.off()

termobj_sel <- c(Boruta::getSelectedAttributes(Bor))
                 

fit_rf <- randomForest::randomForest(reformulate(termlabels = termobj_sel,
                          response = "new_cases_lead_1"),
              data= train_dat, ntree = 1000)

# fit_rf <- randomForest::randomForest(reformulate(termlabels = "Blip_Dim36",
#                                                  response = "new_cases_lead_1"),
#                                      data= train_dat_br, ntree = 1400)
pred_R0 <- predict(fit_rf, test_dat, type = "response")

cor_pred<- cor.test(pred_R0, test_dat$new_cases_lead_1)
(cor_pred$estimate)^2
mean_absolute_percentage_error(pred_R0[1:13] , test_dat$new_cases_lead_1)
######## without country var #######

Bor <- Boruta(reformulate(termlabels = termobj[-length(termobj)],
              response = "new_cases_lead_1"),
              data= train_dat, ntree = 1000)
fit_rf <- randomForest::randomForest(reformulate(termlabels = termobj_sel[-length(termobj_sel)],
                                                 response = "new_cases_lead_1"),
                                     data= train_dat, ntree = 1000)
pred_R0 <- predict(fit_rf, test_dat, type = "response")

cor_pred<- cor.test(pred_R0, test_dat$new_cases_lead_1)
(cor_pred$estimate)^2

mean_absolute_percentage_error(pred_R0[1:13] , test_dat$new_cases_lead_1)

###################

source("C:/aditya/work/work/variabilty_boruta/functions/reformulate_random.R")

my_formula <- reformulate(termlabels = termobj_sel[-length(termobj_sel)],
                                 response = "new_cases_lead_1")
                                
fit_lme <- lme(my_formula, random = ~1|country, data = train_dat)
summary(fit_lme)
pred_R0 <- predict(fit_lme, test_dat, type = "response")

cor_pred<- cor.test(pred_R0, test_dat$new_cases_lead_1)
(cor_pred$estimate)^2
mape <- mean_absolute_percentage_error(pred_R0[1:13] , test_dat$new_cases_lead_1)

############### wihout country var #################

fit_lm <- lm(my_formula, data = train_dat)
summ_lm <- summary(fit_lm)

pred_R0 <- predict(fit_lm, test_dat, type = "response")

cor_pred<- cor.test(pred_R0, test_dat$new_cases_lead_1)

plot(pred_R0, test_dat$new_cases_lead_1)
(cor_pred$estimate)^2

mape <- mean_absolute_percentage_error(pred_R0[1:13] , test_dat$new_cases_lead_1)
RMSE(pred_R0[1:13] , test_dat$new_cases_lead_1)

##############################
# my_formula <- reformulate_random(termlabels = termobj_sel[-length(termobj_sel)],
#                                  "new_cases_lead_1" ,
#                                  randeff = "(1|country)")
# fit_lmer <- lmer(my_formula, data = train_dat)
# summary(fit_lmer)
# stepc_lmer  <- cAIC4::stepcAIC(fit_lmer)
# 
# 
# pred_R0 <- predict(fit_lmer, test_dat, type = "response")
# 
# cor_pred<- cor.test(pred_R0, test_dat$new_cases)
# (cor_pred$estimate)^2

############### with all var ########
# my_formula <- reformulate_random(termlabels = termobj_sel[-length(termobj_sel)],
#                                  "new_cases" ,
#                                  randeff = "(1|country)")
# fit_lmer <- lmer(my_formula, data = train_dat)
# summary(fit_lmer)
# stepc_lmer  <- cAIC4::stepcAIC(fit_lmer,direction = "backward", trace = TRUE, data = Pastes)
# stepc_lmer$finalModel
# 
# 
# 
# pred_R0 <- predict(fit_lmer, test_dat, type = "response")
# 
# cor_pred<- cor.test(pred_R0, test_dat$new_cases)
# (cor_pred$estimate)^2

