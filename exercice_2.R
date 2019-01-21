library(tidyverse)
library(zoo)
library(lubridate)
df <- read_tsv("/Users/beneverad/Documents/MSc/1. Semester/data_collection_storage/Data/10760763.tsv",skip=5)
df1<-df
options(pillar.sigfig = 4)
#Hobo range
hobmin = -20
hobmax = 70

#creating dttm
df1$dttm <- paste(df1$date, df1$hm)
df1$dttm <- ymd_hms(df1$dttm)
df1 <- df1 %>% select (-c(id,date,hm))
#reorder
df1 <- df1[c("dttm", "ta", "lux")] 
df1 <- df1 %>% filter(dttm>= date("2018-12-10"))
#check if ta outside hobo measurable area


df1 <- df1 %>% mutate(qc1 = ifelse(ta>hobmax | ta<hobmin,1,0)) #| = OR

df2 <- df1 %>% mutate(qc2 = ifelse(abs(rollapply(ta, 
                               width = 2,
                               FUN = "diff",
                               align = "right",
                               fill = 0))>=1,1,0))# %>% filter(qc2==1)


allSame <- function(x) length(unique(x)) == 1
df3 <- df2 %>% mutate(qc3 = 0+rollapply(ta,
                              width = 6,
                              FUN = allSame,
                              align ="right",fill=0))

#calculating the standard deviation for the 6 ones above without the actual one width=list(-1:-6)
df4 <-df3 %>% mutate(stev = rollapply(ta,
                              width=list(-1:-6),
                              FUN = sd,
                              align = "right",
                              fill = 0))
diff_calc <- function (x) {
  if (is.na(x[1])){return(2*abs(x[3]-x[2]))} else {
  if (is.na(x[3])){return(2*abs(x[1]-x[2]))} else {
    return(abs(x[1]-x[2])+abs(x[2]-x[3]))}}
  
  
}
df4 <- df4 %>% mutate(diffleftright = rollapply(ta,
                                        width = 3,
                                        FUN = diff_calc,
                                        align = "center",
                                        fill = 0))

df4<- df4 %>% mutate(qc4 = 0+(diffleftright > 6*stev) )
#delete temp columns
df4 <- df4 %>% select(-c(stev,diffleftright))

#Lux influence
lux_th1 <- 10000
lux_th2 <- 15000

oneabove1 <- function (x){ 
  return(TRUE %in% (x>lux_th1))}
oneabove2 <- function(x){return(TRUE %in% (x>lux_th2))}
df5 <- df4 %>% mutate(qc5.1 = 0+rollapply(lux,
                                 width = 3,
                                 FUN = oneabove1,
                                 align = "center",
                                 fill=0))
df5 <- df5 %>% mutate(qc5.2 = 0+rollapply(lux,
                                 width = 7,
                                 FUN = oneabove2,
                                 align = "center",
                                 fill=0))

#check whether lux filter is doing somthing dump during night
df5 %>% filter(hour(dttm)<6 | hour(dttm)>=18) %>% filter(qc5.1 == TRUE | qc5.2 == TRUE)


#total of quality checks
df5$qc_total <- df5$qc1+df5$qc2+df5$qc3+df5$qc4+df5$qc5.1+df5$qc5.2

#sum of qc-flags
sum(df5$qc1)
sum(df5$qc2)
sum(df5$qc3)
sum(df5$qc4)
sum(df5$qc5.1)
sum(df5$qc5.2)

max(df5$qc_total)


#part 2 transform to hour
df6 <- df5 %>% mutate(avgta = rollapply(ta,
                                 width = 6,
                                 align = "left",
                                 FUN = mean,
                                 fill=NA
                                 ))
df6 <- df6 %>% mutate(sumerror = rollapply(qc_total,
                                 width = 6,
                                 align = "left",
                                 FUN = sum,
                                 fill=NA
))
df6 <- df6 %>% filter(minute(dttm)==0)
#deleting those with to many flags
df6 <- df6 %>% mutate(avgta = ifelse(sumerror>1,NA,avgta))
df7 <- df6[c("dttm", "avgta")]

#part3
#importing dwd_data equal formatting
df_dwd <- as_tibble(read.csv("/Users/beneverad/Documents/MSc/1. Semester/data_collection_storage/Data/stundenwerte_air_temperature_13667_akt/produkt_air_temperature_13667_akt.txt",dec=".",sep=";"))

df_dwd$dttm <- strptime(df_dwd$MESS_DATUM, "%Y%m%d%H")
df_dwd$dttm <- ymd_hms(df_dwd$dttm)
df_dwd2 <- df_dwd %>% filter(df_dwd$dttm >= date("2018-12-10") & df_dwd$dttm < date("2019-01-07"))
df_dwd2 <- df_dwd2 %>% select(-c("STATIONS_ID","MESS_DATUM","QUALITAETS_NIVEAU","STRUKTUR_VERSION","REL_FEUCHTE","eor"))

df_comb <- inner_join(df7,df_dwd2)
names(df_comb) <- c("dttm","ta", "ta_dwd" )

#calculating lm
lm_calc <- lm(ta~ta_dwd, data = df_comb)
intcpt = lm_calc$coefficients[1]
slpe = lm_calc$coefficients[2]

df_final <- df_comb %>% mutate(date = date(dttm),
                   hour = hour(dttm),
                   th = ifelse(is.na(ta),intcpt + slpe*ta_dwd,ta),
                   origin = ifelse(is.na(ta),"R","H")) %>% select(-c(dttm,ta,ta_dwd))

write_tsv(df_final %>% mutate(th = signif(th,digits = 4)),"/Users/beneverad/Documents/MSc/1. Semester/data_collection_storage/Data/10760763_Th.tsv")
