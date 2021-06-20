# Install packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggpmisc")
install.packages("zoo")
install.packages("Kendall")
install.packages("factoextra")
install.packages("cluster")
install.packages("clustMixType")
install.packages("rgl")


# Load packages
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggpmisc)
library(zoo)
library(Kendall)
library(factoextra)
library(cluster)
library(clustMixType)


# Import dataset
filedir=setwd("D:/MDS/P2/IndiaTelco")
file_names=dir(filedir)

# Integrate dataset
all_data = do.call("rbind", lapply(file_names, function(x) {
  dat = read.csv(x, header=FALSE)
  dat$Months = tools::file_path_sans_ext(basename(x))
  dat
}))

# Assign variable name to each column
names(all_data)[1]="Service_Provider"
names(all_data)[2]="Technology"
names(all_data)[3]="Packet_Type"
names(all_data)[4]="Speed_kbps"
names(all_data)[5]="Signal_Strength"
names(all_data)[6]="State"


# Change str to factors
all_data$Service_Provider=factor(Service_Provider)
all_data$Technology=factor(Technology)
all_data$Packet_Type=factor(Packet_Type)
all_data$State=factor(State)

# Change str to numeric
all_data$Signal_Strength=as.numeric(all_data$Signal_Strength)

# Overview of dataset & attach to environment
summary(all_data)
data=all_data
attach(data)

# Rename Months
data$Months[data$Months == "19_9_publish"]= "2019-09"
data$Months[data$Months == "19_10_publish"]= "2019-10"
data$Months[data$Months == "19_11_publish"]= "2019-11"
data$Months[data$Months == "19_12_publish"]= "2019-12"
data$Months[data$Months == "20_1_publish"]= "2020-01"
data$Months[data$Months == "20_2_publish"]= "2020-02"
data$Months[data$Months == "20_3_publish"]= "2020-03"
data$Months[data$Months == "20_4_publish"]= "2020-04"
data$Months[data$Months == "20_5_publish"]= "2020-05"
data$Months[data$Months == "20_6_publish"]= "2020-06"
data$Months[data$Months == "20_7_publish"]= "2020-07"
data$Months[data$Months == "20_8_publish"]= "2020-08"

# Declare Date
data$Months=as.Date(as.yearmon(data$Months))
summary(data$Months)
summary(data)

# Filter dataset to meet project scope
data_new=data[data$Technology == "4G" & data$Packet_Type == "download", ]

# Remove 0 cases - not valid record
download=subset(data_new, Speed_kbps != 0)

# Check & handle missing Values
colnames(download)[ apply(download, 2, anyNA) ]
sum(is.na(download$State))  # only 1% missing values for State column
df=subset(download, !is.na(download$State))
summary(df)

# Data imputation - per state
sum_state <- df %>% 
              group_by(State) %>% 
              summarise(min = min(Signal_Strength,na.rm = T),
                        median = median(Signal_Strength,na.rm = T),
                        max = max(Signal_Strength,na.rm = T),
                        avg = mean(Signal_Strength,na.rm = T))

df_new <- df %>%
  group_by(State)%>%
  mutate_at(vars(Signal_Strength),~replace_na(.,median(Signal_Strength,na.rm = T)))
  
# Convert speed unit from kbps to mbps
df_new$Speed_kbps = df_new$Speed_kbps/1000
names(df_new)[4]="Speed_Mbps"

# Remove outliers
boxplot(Speed_Mbps~Service_Provider,data=df_new)

# Identify number of outliers to be removed
outliers <- df_new %>%
        group_by(State) %>%
        mutate(new_col = ifelse(Speed_Mbps > quantile(Speed_Mbps)[4] + 1.5*IQR(Speed_Mbps),
                            "outlier",
                            ifelse(Speed_Mbps  < quantile(Speed_Mbps)[2] - 1.5*IQR(Speed_Mbps),
                                   "outlier",
                                   Speed_Mbps)))

dim(outliers[outliers$new_col == "outlier",])


# create function to remove outlier
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Remove outlier
clean_df <- df_new %>%
              group_by(State) %>%
              mutate_at(vars(Speed_Mbps), remove_outliers)%>%
              na.omit()


# Extract average value by month & ISP
mean = aggregate(clean_df$Speed_Mbps, by=list(clean_df$Months,clean_df$Service_Provider), FUN=mean)
mean = mean %>% rename(Date = Group.1, Service_Provider = Group.2, Speed_Mbps = x)	

# Convert to matrix data frame
df_download=xtabs(Speed_Mbps ~ Date  + Service_Provider , data = mean)
df_download=as.data.frame.matrix(df_download,stringsAsFactors = FALSE)
df_download=data.frame(Date = row.names(df_download), df_download)
rownames(df_download) = NULL

# EDA
summary_byMonth <- data.frame(
  aggregate(Speed_Mbps~Months, data=clean_df, FUN="length"),
  aggregate(Speed_Mbps~Months, data=clean_df, FUN="min"),
  aggregate(Speed_Mbps~Months, data=clean_df, FUN="max"),
  aggregate(Speed_Mbps~Months, data=clean_df, FUN="mean"),
  aggregate(Speed_Mbps~Months, data=clean_df, FUN="sd"))

summary_byMonth <- summary_byMonth[,c(1,2,4,6,8,10)]
colnames(summary_byMonth) <- c("Months", "No. of Test", "min", "max", "mean","SD")


summary_byISP <- data.frame(
  aggregate(Speed_Mbps~Service_Provider, data=clean_df, FUN="length"),
  aggregate(Speed_Mbps~Service_Provider, data=clean_df, FUN="min"),
  aggregate(Speed_Mbps~Service_Provider, data=clean_df, FUN="max"),
  aggregate(Speed_Mbps~Service_Provider, data=clean_df, FUN="mean"),
  aggregate(Speed_Mbps~Service_Provider, data=clean_df, FUN="sd"))

summary_byISP <- summary_byISP[,c(1,2,4,6,8,10)]
colnames(summary_byISP) <- c("ISP", "No. of Test", "min", "max", "mean","SD")


summary_all <- clean_df %>%
                  group_by(Months, Service_Provider)%>%
                  summarise(`No. of Test` = length(Speed_Mbps),
                            min = min(Speed_Mbps),
                            max = max(Speed_Mbps),
                            mean = mean(Speed_Mbps),
                            SD = sd(Speed_Mbps))
 
# Convert to matrix data frame
matrix_all=xtabs(`No. of Test` ~ Months  + Service_Provider , data = summary_all)
matrix_all=as.data.frame.matrix(matrix_all,stringsAsFactors = FALSE)
matrix_all = cbind(matrix_all,rownames(matrix_all))
names(matrix_all)[6] = "Date"
rownames(matrix_all)=NULL

glimpse(matrix_all)


##ANOVA

##Service Provider

# Compute the analysis of variance for download
res.aov <- aov(Speed_kbps ~ Service_Provider, data = download)

# Summary of the analysis
summary(res.aov)

#Post-Hoc Analysis
TukeyHSD(res.aov)

##State##

# Compute the analysis of variance for download
res.aov <- aov(Speed_kbps ~ State, data = download)

# Summary of the analysis
summary(res.aov)

#Post-Hoc Analysis
TukeyHSD(res.aov)


##Trend Analysis##
MannKendall(df_download$AIRTEL)
MannKendall(df_download$CELLONE)
MannKendall(df_download$IDEA)
MannKendall(df_download$JIO)
MannKendall(df_download$VODAFONE)

MannKendall(matrix_all$AIRTEL)
MannKendall(matrix_all$CELLONE)
MannKendall(matrix_all$IDEA)
MannKendall(matrix_all$JIO)
MannKendall(matrix_all$VODAFONE)

## Trend Plot ##

#df_trend=as.tibble(df_download)
df_download$Date =as.Date(df_download$Date)

ggplot(df_download, aes(x = Date, y = AIRTEL)) +
	ggtitle("Plot of AIRTEL Download Speed (Mbps) by Months") +
  	geom_point() +
  	geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  	stat_poly_eq(formula = x ~ y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)

ggplot(df_download, aes(x = Date, y = CELLONE)) +
	ggtitle("Plot of CELLONE Download Speed (Mbps) by Months") +
  	geom_point() +
  	geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  	stat_poly_eq(formula = x ~ y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)

ggplot(df_download, aes(x = Date, y = IDEA)) +
	ggtitle("Plot of IDEA Download Speed (Mbps) by Months") +
  	geom_point() +
  	geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  	stat_poly_eq(formula = x ~ y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)

ggplot(df_download, aes(x = Date, y = JIO)) +
	ggtitle("Plot of JIO Download Speed (Mbps) by Months") +
  	geom_point() +
  	geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  	stat_poly_eq(formula = x ~ y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)

ggplot(df_download, aes(x = Date, y = VODAFONE)) +
	ggtitle("Plot of VODAFONE Download Speed (Mbps) by Months") +
  	geom_point() +
  	geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  	stat_poly_eq(formula = x ~ y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)


## mann kendall by no of test

matrix_all$Date =as.Date(matrix_all$Date)

ggplot(matrix_all, aes(x = Date, y = AIRTEL)) +
  ggtitle("Plot of AIRTEL Speed Test by Months") +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = x ~ y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)

ggplot(matrix_all, aes(x = Date, y = CELLONE)) +
  ggtitle("Plot of CELLONE Speed Test by Months") +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = x ~ y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)

ggplot(matrix_all, aes(x = Date, y = IDEA)) +
  ggtitle("Plot of IDEA Speed Test by Months") +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = x ~ y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)

ggplot(matrix_all, aes(x = Date, y = JIO)) +
  ggtitle("Plot of JIO Speed Test by Months") +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = x ~ y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)

ggplot(matrix_all, aes(x = Date, y = VODAFONE)) +
  ggtitle("Plot of VODAFONE Speed Test by Months") +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = x ~ y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)



# Trend for All
ggplot(mean, aes(x = Date, y = Speed_Mbps)) + 
  geom_line(aes(color = Service_Provider), size = 1) +
  ggtitle("Trend of Download Speed (Mbps) by Service Provider") +
  xlab("Date") + ylab("Download Speed (Mbps)")+
  theme(plot.title = element_text(hjust = 0.5))

# Speed achieved trend per ISP

ggplot(mean[mean$Service_Provider == "AIRTEL",], aes(x = Date, y = Speed_Mbps)) + 
	geom_line(size = 1) +
	ggtitle("Trend of Download Speed (Mbps) for AIRTEL") +
	xlab("Date") + ylab("Download Speed (Mbps)")+
	theme(plot.title = element_text(hjust = 0.5))

ggplot(mean[mean$Service_Provider == "CELLONE",], aes(x = Date, y = Speed_Mbps)) + 
  geom_line(size = 1) +
  ggtitle("Trend of Download Speed (Mbps) for CELLONE") +
  xlab("Date") + ylab("Download Speed (Mbps)")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(mean[mean$Service_Provider == "IDEA",], aes(x = Date, y = Speed_Mbps)) + 
  geom_line(size = 1) +
  ggtitle("Trend of Download Speed (Mbps) for IDEA") +
  xlab("Date") + ylab("Download Speed (Mbps)")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(mean[mean$Service_Provider == "JIO",], aes(x = Date, y = Speed_Mbps)) + 
  geom_line(size = 1) +
  ggtitle("Trend of Download Speed (Mbps) for JIO") +
  xlab("Date") + ylab("Download Speed (Mbps)")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(mean[mean$Service_Provider == "VODAFONE",], aes(x = Date, y = Speed_Mbps)) + 
  geom_line(size = 1) +
  ggtitle("Trend of Download Speed (Mbps) for VODAFONE") +
  xlab("Date") + ylab("Download Speed (Mbps)")+
  theme(plot.title = element_text(hjust = 0.5))


# Test trend for all ISP
ggplot(summary_all, aes(x = Months, y = `No. of Test`)) + 
  geom_line(aes(color = Service_Provider), size = 1) +
  ggtitle("Trend of Speed Test by Service Provider") +
  xlab("Month") + ylab("No. of Speed Test")+
  theme(plot.title = element_text(hjust = 0.5))

# Test trend per ISP

ggplot(summary_all[summary_all$Service_Provider == "AIRTEL",], aes(x = Months, y = `No. of Test`)) + 
  geom_line(size = 1) +
  ggtitle("Trend of Speed Test for AIRTEL") +
  xlab("Months") + ylab("No. of Speed Test")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(summary_all[summary_all$Service_Provider == "CELLONE",], aes(x = Months, y = `No. of Test`)) + 
  geom_line(size = 1) +
  ggtitle("Trend of Speed Test for CELLONE") +
  xlab("Months") + ylab("No. of Speed Test")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(summary_all[summary_all$Service_Provider == "IDEA",], aes(x = Months, y = `No. of Test`)) + 
  geom_line(size = 1) +
  ggtitle("Trend of Speed Test for IDEA") +
  xlab("Months") + ylab("No. of Speed Test")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(summary_all[summary_all$Service_Provider == "JIO",], aes(x = Months, y = `No. of Test`)) + 
  geom_line(size = 1) +
  ggtitle("Trend of Speed Test for JIO") +
  xlab("Months") + ylab("No. of Speed Test")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(summary_all[summary_all$Service_Provider == "VODAFONE",], aes(x = Months, y = `No. of Test`)) + 
  geom_line(size = 1) +
  ggtitle("Trend of Speed Test for VODAFONE") +
  xlab("Months") + ylab("No. of Speed Test")+
  theme(plot.title = element_text(hjust = 0.5))


###################### subscriber's category ##########################
###K-means clustering

cluster_df <- clean_df[c(4,5,6)]
glimpse(cluster_df)

# str to factor
cluster_df$State=factor(cluster_df$State)
glimpse(cluster_df)


# Elbow Method for finding the optimal number of clusters
set.seed(123)

# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kproto(cluster_df, k)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     main="Elbow Method",
     xlab="Number of Cluster",
     ylab="Total within-clusters sum of squares (wcss)")


# Apply k prototypes
a <- lambdaest(cluster_df)
res <- kproto(cluster_df, k= 3, lambda = a)

# View data distributed over cluster
table(res$cluster)

# Match record with cluster number
cluster <- data.frame(cluster_df, res$cluster)
write.csv(cluster,"D:/MDS/P2/cluster3.csv")

# Extract min and max value per cluster
cluster %>% group_by (res.cluster) %>% summarize(min = min(Speed_Mbps),
                                                      max = max(Speed_Mbps))

# Count of each cluster in speed range of clsuter 1
cluster %>% 
    filter (Speed_Mbps > 0.001 & Speed_Mbps < 45.3)%>% 
    group_by(res.cluster)%>%
    summarise(n())

# Count of each cluster in speed range of clsuter 2
cluster %>% 
  filter (Speed_Mbps > 0.001 & Speed_Mbps < 37.5)%>% 
  group_by(res.cluster)%>%
  summarise(n())

# Count of each cluster in speed range of clsuter 3
cluster %>% 
  filter (Speed_Mbps > 23.6 & Speed_Mbps < 94.7)%>% 
  group_by(res.cluster)%>%
  summarise(n())

# Plot cluster
ggplot() +
  geom_point(data = cluster, 
             mapping = aes(x = Signal_Strength, 
                           y = Speed_Mbps, 
                           colour = res.cluster)) +
  geom_point(mapping = aes_string(x = res$centers[, "Signal_Strength"], 
                                  y = res$centers[, "Speed_Mbps"]),
             color = "red", size = 4)+
  geom_text(mapping = aes_string(x = res$centers[, "Signal_Strength"], 
                                 y = res$centers[, "Speed_Mbps"],
                                 label = 1:3),
            color = "black", size = 4) +
  theme_light()


# View centre / avg speed for each cluster
res$centers

standard_speed <- cluster %>% 
                    group_by(res.cluster) %>% 
                    summarise(avg = mean(Speed_Mbps))%>%
                    rename("Cluster" = res.cluster,
                           "Speed Standard" = avg)

# Assign speed standard value to each observations
all_cluster <- data.frame(clean_df, cluster$res.cluster)
all_cluster <- all_cluster[c(1,4,5,6,7,8)]
names(all_cluster)[6]="cluster"
all_cluster$Speed_Mbps <- round(all_cluster$Speed_Mbps)

final_df <- all_cluster %>%
              mutate('Speed Standard (Mbps)' = ifelse(cluster == "1",
                                               "4",
                                               ifelse(cluster == "2",
                                                      "51",
                                                      ifelse(cluster == "3",
                                                             "2",
                                                             "NA"))),
                     'CE' = ifelse(Speed_Mbps > `Speed Standard (Mbps)`, "above",
                                   ifelse(Speed_Mbps == `Speed Standard (Mbps)`, "meet",
                                          ifelse(Speed_Mbps < `Speed Standard (Mbps)`, "below","NA")))
              )


a <- final_df %>%
  group_by(CE)%>%
  summarise(`Total Count` = n())

b <- final_df %>%
  group_by(cluster,CE)%>%
  summarise(`Total Count` = n())

c <- final_df %>%
  group_by(Service_Provider, CE)%>%
  summarise(`Total Count` = n())
c <- xtabs(`Total Count` ~ CE + Service_Provider , data = c)
c=as.data.frame.matrix(c,stringsAsFactors = FALSE)


glimpse(final_df)
final_df$`Speed Standard (Mbps)`=factor(final_df$`Speed Standard (Mbps)`)
final_df$CE=factor(final_df$CE)
final_df$Months=factor(final_df$Months)
glimpse(final_df)

before = c("2019-09-01","2019-10-01","2019-11-01","2019-12-01","2020-01-01","2020-02-01","2020-03-01")
after = c("2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01")

d <- final_df %>%
  filter(Months %in% before)%>%
  group_by(Service_Provider, CE)%>%
  summarise(`Total Count` = n())
d <- xtabs(`Total Count` ~ CE + Service_Provider , data = d)
d=as.data.frame.matrix(d,stringsAsFactors = FALSE)

e <- final_df %>%
  filter(Months %in% after)%>%
  group_by(Service_Provider, CE)%>%
  summarise(`Total Count` = n())
e <- xtabs(`Total Count` ~ CE + Service_Provider , data = e)
e=as.data.frame.matrix(e,stringsAsFactors = FALSE)

# overall whole india
f<-final_df %>% group_by(`Speed Standard (Mbps)`) %>% summarise(`Total Count` = n())
g<-final_df %>% group_by(CE) %>% summarise(`Total Count` = n())
h<-final_df %>% group_by(Months,CE) %>% summarise(`Total Count` = n())
h <- xtabs(`Total Count` ~ Months + CE , data = h)
h=as.data.frame.matrix(h,stringsAsFactors = FALSE)

output <- final_df %>%
  group_by(Service_Provider)%>%
  summarise(common_speed_standard_Mbps = names(table(`Speed Standard (Mbps)`))[which.max(table(`Speed Standard (Mbps)`))],
            common_CE = names(table(CE))[which.max(table(CE))])


write.csv(final_df,"D:/MDS/P2/final_df3.csv")