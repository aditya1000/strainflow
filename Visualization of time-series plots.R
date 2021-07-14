############ 
Subject: Vizualization strainflow
Author:Aditya Nagori
############
library(lubridate)
library(ggplot2)
library(reshape2)
library(scales)
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
########################### Reading new cases data #############
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
list_ <- list.files("C:/aditya/strainflow/sampling_blips/", pattern = ".csv")
main_path <- "C:/aditya/strainflow/sampling_blips/"
list_ <- list_[-grep("Wales|Ireland|Scotland", list_, ignore.case = T)]

temp <- NULL
for(fil in list_){
  state_nam <- stringr::str_split(fil, n=2, pattern = "_")[[1]][1]  
  aus_month <- read.csv(paste0(main_path, fil))
  colnames(aus_month)[2:37] <- gsub("X", "Blip_Dim", colnames(aus_month)[2:37]) 
  
  my_rescale <- function(x){rescale(x, c(0.0001 , 1))}
  
  aus_month$Collection_Date <- strptime(aus_month$Collection_Date, 
                                         "%Y-%m-%d")
  aus_month$Collection_Date <- format(aus_month$Collection_Date,
                                      format = "%y-%m-%d")
  
  aus_month$date <-  paste0(month(as.Date(aus_month$Collection_Date)),"-",
                            year(as.Date(aus_month$Collection_Date)))
  
  aus_month <- merge(aus_month, R0_df[R0_df$country ==state_nam,c(1,2,3,6)], by = "date" ,all= F)
  
  aus_month <- aus_month[order(aus_month$Collection_Date, decreasing = F),] 
  
  aus_month[3:38] <-  apply(aus_month[3:38], 2, my_rescale)
  #colnames(dat) <- paste0("cum_", colnames(dat))
  my_diff <- function(x){
    y <- c(NA, diff(x))
    return(y)
  }

  dat_diff <-  apply(aus_month[3:38], 2, my_diff)
  colnames(dat_diff) <- paste0("diff_", colnames(dat_diff))
  
  dat_log <-  apply(aus_month[3:38], 2, log)
  colnames(dat_log) <- paste0("log_", colnames(dat_log))
  
  dat_log_diff <-  apply(dat_log, 2, my_diff)
  colnames(dat_log_diff) <- paste0("diff_", colnames(dat_log_diff))
  
  
  aus_month<- cbind(aus_month, dat_diff, dat_log_diff)
  
  
  aus_month$new_cases  <- c(aus_month$total_cases[1], diff(aus_month$total_cases))
  
  aus_month$diff_new_cases  <- c(NA, diff(aus_month$new_cases))
  aus_month$diff_log_new_cases  <- c(NA, diff(log(aus_month$new_cases)))
  
  temp <- rbind(temp, aus_month)
}

temp <- data.frame(temp)
dat_merge <- temp[which(apply(is.na(temp), 1, sum) == 0), ] 
colnames(dat_merge)
# dat_merge$R0_change <- 0
# dat_merge$R0_change[which((dat_merge$R0_diff/dat_merge$R0) > 0.20)] <- 1

############# Dimension 14 for Brazil ##########
write.csv(dat_merge, 
          "C:/aditya/strainflow/results_sampling_blips/R0_blip_data_merge_eng_diff_log.csv", row.names = F)

dat_merge_sel <- dat_merge[which(dat_merge$Collection_Date > "20-02-29" &
                                   dat_merge$Collection_Date < "21-04-30"),]

View(dat_merge_sel)

dat_merge_brazil <- dat_merge_sel[dat_merge_sel$country == "Brazil",]
# dat_merge_brazil$Collection_Date <- 
# as.Date(dat_merge_brazil$Collection_Date, format = "%y-%m-%d")



dat_merge_India <- dat_merge_sel[dat_merge_sel$country == "India",]
dat_merge_UK <- dat_merge_sel[dat_merge_sel$country == "England",]
############## Cumulattive  ##############
# dimto_pl<- c(9, 18, 27, 29, 35)
# 
# 
# dat_for_cum <- dat_merge[,grep("Collection_Date|country|cum_Blip_Dim2$|cum_Blip_Dim18|cum_Blip_Dim29|cum_Blip_Dim35|cum_Blip_Dim36",
#                           colnames(dat_merge))]
# melt_dat <- reshape2::melt(dat_for_cum, c("Collection_Date","country"))
# library(dplyr)
# melt_dat_sel <- melt_dat %>% filter(country %in% c("England", "Brazil", "India"))
# melt_dat_sel <- melt_dat_sel[which(melt_dat_sel$Collection_Date > "20-02-29" &
#                                       melt_dat_sel$Collection_Date < "21-04-30"),]
# 
# 
# png("c:/aditya/strainflow/results_new_cases/cum_plots.png",
#     width= 2400, height = 1050, res = 300)
# ggplot(melt_dat_sel,aes(Collection_Date,
#                     value,color=variable,
#                     group = variable,
#                     alpha=variable)) + 
# geom_line(size = 1) +  theme_bw() +
#   theme(axis.text.x = element_text(angle = 45))+
#   theme(axis.ticks.x = element_blank(),axis.ticks.length.x = unit(0.4, "cm"))+
#   theme(legend.position = "top")+
# scale_alpha_manual(values = c(1, 1, 0.3, 0.3,0.3)) +
# facet_wrap(~country)
# dev.off()
# 
##################################################################
##############################################all################
library(ggplot2)


pdf("C:/aditya/strainflow/final_plots_samp/CCF_new_cases_blip_Brazil_All.pdf", 
    width =6 , height = 4)
temp_ccf_br <- NULL
for(i in c(1:36)){

par(mar=c(7,4,4,4))
#par(mfrow=c(1,3))
ccf(dat_merge_brazil[, 2+i], dat_merge_brazil$new_cases, 
    main = paste0("Brazil, Blip Dim ",i , "Vs new_cases cross correlation"),
    cex = "4") 
ccf(dat_merge_brazil[, 42+i], dat_merge_brazil$diff_new_cases, 
    main = paste0("Brazil, diff(Blip Dim ",i , ")"),
    cex = "4")
ccf(dat_merge_brazil[, 78+i], dat_merge_brazil$diff_log_new_cases,
     main = paste0("Brazil, diff,log Blip Dim ",i),
    cex = "4")
c = ccf(dat_merge_brazil[, 78+i], dat_merge_brazil$diff_log_new_cases,
              main = paste0("Brazil, diff,log Blip Dim ",i),
              cex = "4")
temp_ccf_br <- rbind(temp_ccf_br, c$acf)
}


#temp_ccf_br <- as.data.frame(temp_ccf_br)
library("RColorBrewer")
col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)

colnames(temp_ccf_br) <- c$lag
rownames(temp_ccf_br) <- 1:36

png("C:/aditya/strainflow/final_plots_samp/Brazil_heatmap.png",
    height = 600, width = 600)
par(cex.lab = 6)
heatmap(temp_ccf_br, xlab = "lag", ylab = "Blip dimension",
          main = "Brazil", cexCol = 3, cexRow = 3,
        Colv=NA, Rowv=NA, scale='none', cex = 5)
dev.off()
heatmap.2(temp_ccf_br,dendrogram='none', Rowv=TRUE, Colv=TRUE,trace='none')

############ for line plot ############
pdf("C:/aditya/strainflow/final_plots_samp/line_new_cases_blip_Brazil_All.pdf", 
    width =5 , height = 3)
for(i in c(3,33,15,12,30)){
par(mar=c(10,5,5,5))
p <- ggplot(dat_merge_brazil, aes(x =Collection_Date))
p <- p + geom_line(aes(y = new_cases, colour = "new_cases", group = 1))

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = get(paste0("Blip_Dim",i))*2500000, colour = paste0("Blip Dimension",i), group = 1),
                   size =1)

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./2500000, name = paste0("Blip Dimension",i)))
# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red", "orange"))
p <- p + labs(y = "Monthly new cases",
              x = "Date",
              colour = "Brazil")
p <- p + theme_bw()
p <- p + theme(axis.text.x = element_text(angle = 45, size = 10))+
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.ticks.x = element_blank(),axis.ticks.length.x = unit(0.4, "cm"))+
  theme(legend.position = "top")
print(p)
#################### Diff plots ##########
p <- ggplot(dat_merge_brazil, aes(x =Collection_Date))
p <- p + geom_line(aes(y = diff_new_cases, colour = "diff_new_cases", group = 1))
#   
#   # adding the relative humidity data, transformed to match roughly the range of the temperature
#p <- p + geom_line(aes(y = get(paste0("diff_Blip_Dim",i)), colour = paste0("Diff Blip Dimension",i), group = 1),
#                   size = 1)
p <- p + geom_line(aes(y = get(paste0("diff_Blip_Dim",i))*500000, colour = paste0("Diff Blip Dimension",i), group = 1),
                      size =1)
#   
#   # now adding the secondary axis, following the example in the help file ?scale_y_continuous
#   # and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./500000, name = paste0("Blip Dimension",i)))
# modifying colours and theme options
#   #p <- p + scale_colour_manual(values = c("blue", "red", "orange"))
p <- p + labs(y = "Diff new cases",
              x = "Date",
              colour = "Brazil")
p <- p + theme_bw()
p <- p + theme(axis.text.x = element_text(angle = 45, size = 10))+
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.ticks.x = element_blank(),axis.ticks.length.x = unit(0.4, "cm"))+
  theme(legend.position = "top")
print(p)

#   
######################### Diff log plot #########
# 
p <- ggplot(dat_merge_brazil, aes(x =Collection_Date))
p <- p + geom_line(aes(y = diff_log_new_cases, colour = "Delta log new cases", group = 1))
#   
#   # adding the relative humidity data, transformed to match roughly the range of the temperature
# p <- p + geom_line(aes(y = get(paste0("diff_log_Blip_Dim",i)), colour = paste0("Diff log Blip Dimension",i), group = 1),
#                    size =1)
p <- p + geom_line(aes(y = get(paste0("diff_log_Blip_Dim",i)), colour = paste0("Diff log Blip Dimension",i), group = 1),
                      size =1)
#   
#   # now adding the secondary axis, following the example in the help file ?scale_y_continuous
#   # and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~., name = paste0("Diff log Blip Dimension",i)))   
# modifying colours and theme options
#   #p <- p + scale_colour_manual(values = c("blue", "red", "orange"))
p <- p + labs(y = "Delta log new cases",
              x = "Date",
              colour = "Brazil")
p <- p + theme_bw()
p <- p + theme(axis.text.x = element_text(angle = 45, size = 10))+
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.ticks.x = element_blank(),axis.ticks.length.x = unit(0.4, "cm"))+
  theme(legend.position = "top")
print(p)
#   
##################### Samples ###########
p <- ggplot(dat_merge_brazil, aes(x =Collection_Date))
p <- p + geom_line(aes(y = X..samples, colour = "Samples", group = 1))

# adding the relative humidity data, transformed to match roughly the range of the temperature
# p <- p + geom_line(aes(y = get(paste0("Blip_Dim",i)), colour = paste0("Blip Dimension",i), group = 1),
#                    size =1)

p <- p + geom_line(aes(y = get(paste0("Blip_Dim",i))*600, colour = paste0("Blip Dimension",i), group = 1),
                    size =1)

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./600, name = paste0("Blip Dimension",i)))
#modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red", "orange"))
p <- p + labs(y = "Values",
              x = "Date",
              colour = "Brazil")
p <- p + theme_bw()
p <- p + theme(axis.text.x = element_text(angle = 45, size = 10))+
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.ticks.x = element_blank(),axis.ticks.length.x = unit(0.4, "cm"))+
  theme(legend.position = "top")
print(p)

}
dev.off()

##################################################################################################
######################## INDIA ###########
pdf("C:/aditya/strainflow/final_plots_samp/CCF_new_cases_blip_India_All.pdf", 
    width =6 , height = 4)
temp_ccf_In <- NULL
for(i in c(1:36)){
  par(mar=c(7,4,4,4))
  #par(mfrow=c(1,3))
  #tr_Fac <- max(dat_merge_India$new_cases)/max(dat_merge_India[, 2+i])
  ccf(dat_merge_India[, 2+i], dat_merge_India$new_cases, 
      main = paste0("India, Blip Dim",i, "Vs new_cases cross correlation"),
      cex = "2") 
  ccf(dat_merge_India[, 42+i], dat_merge_India$diff_new_cases, 
      main = paste0("India, diff(Blip Dim",i, ")"),
      cex = "2")

  c = ccf(dat_merge_India[, 78+i], dat_merge_India$diff_log_new_cases,
     main = paste0("India, diff,log Blip Dim",i),
    cex = "2")
  temp_ccf_In <- rbind(temp_ccf_In, c$acf)
  
}
colnames(temp_ccf_In) <- c$lag
rownames(temp_ccf_In) <- 1:36

png("C:/aditya/strainflow/final_plots_samp/India_heatmap.png",
    height = 800, width = 800)
heatmap(temp_ccf_In, xlab = "lag", ylab = "Blip dimension",
        main = "India")
dev.off()
library(gplots)
heatmap.2(temp_ccf_In,dendrogram='none', Rowv=TRUE, Colv=TRUE,trace='none')

############## for line plot ##############
pdf("C:/aditya/strainflow/final_plots_samp/CCF_line_new_cases_blip_India_All.pdf", 
    width =6, height = 4)
for(i in c(3,33,15,12,30)){
  par(mar=c(10,5,5,5))
  p <- ggplot(dat_merge_India, aes(x =Collection_Date))
  p <- p + geom_line(aes(y = new_cases, colour = "new_cases", group = 1))

  # adding the relative humidity data, transformed to match roughly the range of the temperature
  p <- p + geom_line(aes(y = get(paste0("Blip_Dim",i))*2700000, colour = paste0("Blip Dimension",i), group = 1),
                     size =1)

  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(sec.axis = sec_axis(~./2700000, name = paste0("Blip Dimension",i)))
  # modifying colours and theme options
  #p <- p + scale_colour_manual(values = c("blue", "red", "orange"))
  p <- p + labs(y = "new_cases",
                x = "Date",
                colour = "India")
  p <- p + theme_bw()
  p <- p + theme(axis.text.x = element_text(angle = 45))+
    theme(axis.ticks.x = element_blank(),axis.ticks.length.x = unit(0.4, "cm"))+
    theme(legend.position = "top")
  print(p)
  #################### Diff plots ##########
  p <- ggplot(dat_merge_India, aes(x =Collection_Date))
  p <- p + geom_line(aes(y = diff_new_cases, colour = "diff_new_cases", group = 1))
  #
  #   # adding the relative humidity data, transformed to match roughly the range of the temperature
  # p <- p + geom_line(aes(y = get(paste0("diff_Blip_Dim",i)), colour = paste0("Diff Blip Dimension",i), group = 1),
  #                   size = 1)
  p <- p + geom_line(aes(y = get(paste0("diff_Blip_Dim",i))*500000, colour = paste0("Diff Blip Dimension",i), group = 1),
                        size =1)
  #
  #   # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  #   # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(sec.axis = sec_axis(~./500000, name = paste0("Blip Dimension",i)))
  #   # modifying colours and theme options
  #   #p <- p + scale_colour_manual(values = c("blue", "red", "orange"))
  p <- p + labs(y = "diff_new_cases",
                x = "Date",
                colour = "India")
  p <- p + theme_bw()
  p <- p + theme(axis.text.x = element_text(angle = 45))+
    theme(axis.ticks.x = element_blank(),axis.ticks.length.x = unit(0.4, "cm"))+
    theme(legend.position = "top")
  print(p)

  #
  # ######################## Diff log plot #########
  #
  p <- ggplot(dat_merge_India, aes(x =Collection_Date))
  p <- p + geom_line(aes(y = diff_log_new_cases, colour = "Delta log new cases", group = 1))
  #
  #   # adding the relative humidity data, transformed to match roughly the range of the temperature
  #p <- p + geom_line(aes(y = get(paste0("diff_log_Blip_Dim",i)), colour = paste0("Diff log Blip Dimension",i), group = 1),
  #                   size =1)
     p <- p + geom_line(aes(y = get(paste0("diff_log_Blip_Dim",i)), colour = paste0("Diff log Blip Dimension",i), group = 1),
                        size =1)
  #
  #   # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  #   # and, very important, reverting the above transformation
   p <- p + scale_y_continuous(sec.axis = sec_axis(~., name = paste0("Diff log Blip Dimension",i)))
  #   # modifying colours and theme options
  #   #p <- p + scale_colour_manual(values = c("blue", "red", "orange"))
  p <- p + labs(y = "Delta log new cases",
                x = "Date",
                colour = "India")
  p <- p + theme_bw()
  p <- p + theme(axis.text.x = element_text(angle = 45))+
    theme(axis.ticks.x = element_blank(),axis.ticks.length.x = unit(0.4, "cm"))+
    theme(legend.position = "top")
  print(p)
  #
  ##################### Samples ###########
  p <- ggplot(dat_merge_India, aes(x =Collection_Date))
  p <- p + geom_line(aes(y = X..samples, colour = "Samples", group = 1))

  # adding the relative humidity data, transformed to match roughly the range of the temperature
  # p <- p + geom_line(aes(y = get(paste0("Blip_Dim",i)), colour = paste0("Blip Dimension",i), group = 1),
  #                    size =1)

  p <- p + geom_line(aes(y = get(paste0("Blip_Dim",i))*600, colour = paste0("Blip Dimension",i), group = 1),
                      size =1)

  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(sec.axis = sec_axis(~./600, name = paste0("Blip Dimension",i)))
  # modifying colours and theme options
  #p <- p + scale_colour_manual(values = c("blue", "red", "orange"))
  p <- p + labs(y = "Values",
                x = "Date",
                colour = "India")
  p <- p + theme_bw()
  p <- p + theme(axis.text.x = element_text(angle = 45))+
    theme(axis.ticks.x = element_blank(),axis.ticks.length.x = unit(0.4, "cm"))+
    theme(legend.position = "top")
  print(p)

  
}
dev.off()
##################################################################
############################ England ##############
##################################################################
pdf("C:/aditya/strainflow/final_plots_samp/CCF_line_new_cases_blip_england_All.pdf", 
    width =6 , height = 4)

temp_ccf_eg <- NULL
for(i in c(1:36)){
  par(mar=c(7,4,4,4))
  #par(mfrow=c(1,3))
  ccf(dat_merge_UK[, 2+i], dat_merge_UK$new_cases, 
      main = paste0("England, Blip Dim",i, "Vs new_cases cross correlation"),
      cex = "2") 
  ccf(dat_merge_UK[, 42+i], dat_merge_UK$diff_new_cases, 
      main = paste0("England, diff(Blip Dim",i, ")"),
      cex = "2")
  c = ccf(dat_merge_UK[, 78+i], dat_merge_UK$diff_log_new_cases, 
      main = paste0("England, diff,log Blip Dim",i),
      cex = "2")
   temp_ccf_eg <- rbind(temp_ccf_eg, c$acf)
}

dev.off()

colnames(temp_ccf_eg) <- c$lag
rownames(temp_ccf_eg) <- 1:36
png("C:/aditya/strainflow/final_plots_samp/England_heatmap.png",
    height = 600, width = 600)
heatmap(temp_ccf_eg, xlab = "lag", ylab = "Blip dimension",
        main = "England", cexCol = 3, cexRow = 3,
        Colv=NA, Rowv=NA, scale='none', cex = 5)
dev.off()


  
  # dat_merge_UK$resc_new_cases <- scales::rescale(dat_merge_UK$new_cases, c(0,1))  
  # dat_merge_UK$resc_Blip_Dim36 <- scales::rescale(dat_merge_UK$Blip_Dim36, c(0,1))  
pdf("C:/aditya/strainflow/final_plots_samp/line_new_cases_blip_england_All.pdf", 
    width =5 , height = 3)

for(i in c(3,33,15,12,30)){
  
par(mar=c(7,3,3,3))
  p <- ggplot(dat_merge_UK, aes(x =Collection_Date))
  p <- p + geom_line(aes(y = new_cases, colour = "new_cases", group = 1))
  
  # adding the relative humidity data, transformed to match roughly the range of the temperature
  p <- p + geom_line(aes(y = get(paste0("Blip_Dim",i))*2000000, colour = paste0("Blip Dimension",i), group = 1),
                     size =1)
  
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(sec.axis = sec_axis(~./2000000, name = paste0("Blip Dimension",i)))
  # modifying colours and theme options
  #p <- p + scale_colour_manual(values = c("blue", "red", "orange"))
  p <- p + labs(y = "new_cases",
                x = "Date",
                colour = "England")
  p <- p + theme_bw()
  p <- p + theme(axis.text.x = element_text(angle = 45, size = 10))+
    theme(axis.text.y = element_text(size = 10)) +
    theme(axis.ticks.x = element_blank(),axis.ticks.length.x = unit(0.4, "cm"))+
    theme(legend.position = "top")
  print(p)
#################### Diff plots ##########
p <- ggplot(dat_merge_UK, aes(x =Collection_Date))
p <- p + geom_line(aes(y = diff_new_cases, colour = "diff_new_cases", group = 1))
#   
#   # adding the relative humidity data, transformed to match roughly the range of the temperature
# p <- p + geom_line(aes(y = get(paste0("diff_Blip_Dim",i)), colour = paste0("Diff Blip Dimension",i), group = 1),
#                   size = 1)
 p <- p + geom_line(aes(y = get(paste0("diff_Blip_Dim",i))*1500000, colour = paste0("Diff Blip Dimension",i), group = 1),
                      size =1)
#   
#   # now adding the secondary axis, following the example in the help file ?scale_y_continuous
#   # and, very important, reverting the above transformation
 p <- p + scale_y_continuous(sec.axis = sec_axis(~./1500000, name = paste0("Blip Dimension",i)))
#   # modifying colours and theme options
#   #p <- p + scale_colour_manual(values = c("blue", "red", "orange"))
  p <- p + labs(y = "diff_new_cases",
                x = "Date",
                colour = "England")
  p <- p + theme_bw()
  p <- p + theme(axis.text.x = element_text(angle = 45,size = 10))+
    theme(axis.text.y = element_text(size = 10)) +
    theme(axis.ticks.x = element_blank(),axis.ticks.length.x = unit(0.4, "cm"))+
    theme(legend.position = "top")
  print(p)

#   
# ######################## Diff log plot #########
# 
p <- ggplot(dat_merge_UK, aes(x =Collection_Date))
p <- p + geom_line(aes(y = diff_log_new_cases, colour = "Delta log new cases", group = 1))
#   
#   # adding the relative humidity data, transformed to match roughly the range of the temperature
#p <- p + geom_line(aes(y = get(paste0("diff_log_Blip_Dim",i)), colour = paste0("Diff log Blip Dimension",i), group = 1),
#                                    size =1)
p <- p + geom_line(aes(y = get(paste0("diff_log_Blip_Dim",i)), colour = paste0("Diff log Blip Dimension",i), group = 1),
                      size =1)
#   
#   # now adding the secondary axis, following the example in the help file ?scale_y_continuous
#   # and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~., name = paste0("Diff log Blip Dimension",i)))
   # modifying colours and theme options
#   #p <- p + scale_colour_manual(values = c("blue", "red", "orange"))
 p <- p + labs(y = "Delta log new cases",
                 x = "Date",
                 colour = "England")
   p <- p + theme_bw()
   p <- p + theme(axis.text.x = element_text(angle = 45,size = 10))+
     theme(axis.text.y = element_text(size = 10)) +
     theme(axis.ticks.x = element_blank(),axis.ticks.length.x = unit(0.4, "cm"))+
     theme(legend.position = "top")
   print(p)
  
##################### Samples ###########
  p <- ggplot(dat_merge_UK, aes(x =Collection_Date))
  p <- p + geom_line(aes(y = X..samples, colour = "Samples", group = 1))
  
  # adding the relative humidity data, transformed to match roughly the range of the temperature
  #p <- p + geom_line(aes(y = get(paste0("Blip_Dim",i)), colour = paste0("Blip Dimension",i), group = 1),
  #                    size =1)
  
  p <- p + geom_line(aes(y = get(paste0("Blip_Dim",i))*40000, colour = paste0("Blip Dimension",i), group = 1),
                 size =1)
  
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(sec.axis = sec_axis(~./40000, name = paste0("Blip Dimension",i)))
  # modifying colours and theme options
  #p <- p + scale_colour_manual(values = c("blue", "red", "orange"))
  p <- p + labs(y = "Values",
                x = "Date",
                colour = "England")
  p <- p + theme_bw()
  p <- p + theme(axis.text.x = element_text(angle = 45,size = 10))+
    theme(axis.text.y = element_text(size = 10)) +
    theme(axis.ticks.x = element_blank(),axis.ticks.length.x = unit(0.4, "cm"))+
    theme(legend.position = "top")
  print(p)
    
}
dev.off()

