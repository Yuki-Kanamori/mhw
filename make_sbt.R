require(tidyverse)
require(ggh4x)
require(gridExtra)

dir_sbt = "/Users/Yuki/Dropbox/community/sbt/"
dir_save = "/Users/Yuki/Dropbox/community/"

# 水温データの読み込み・成型 -----------------------------------------------------------
n_year = 1995:2021
month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

df = read_csv("/Users/Yuki/Dropbox/community/sbt/1995/01/output19950101.txt") %>% dplyr::filter(temp. != 999.99, depth > 17)
df = df %>% mutate(tag = paste(df$lonE, df$latE, sep = "_"))
col_names = colnames(df)

df_summary_temp = NULL
df_summary = NULL
for(i in 1995:2021){
  for(j in 1:length(month)){
    dir_data = paste0(dir_sbt, i, "/",　month[j], "/")
    setwd(dir_data)
    file_list = list.files(dir_data, pattern="txt")
    
    df2 = NULL
    for(k in 2:length(file_list)){
      df = read_csv(paste0(dir_data, file_list[k]), col_names = col_names)
      df2 = rbind(df, df2)
    }
    head(df2)
    df2_2 = df2[-1, ]
    df_summary = rbind(df_summary, df2_2)
  }
}

setwd(dir_sbt)
write.csv(df_summary, "df_summary_daily.csv", fileEncoding = "CP932", row.names = F)

df_summary = read_gcsv("df_summary_daily.csv")
df_summary$temp. = as.numeric(df_summary$temp.)
df_summary$YEAR = as.numeric(df_summary$YEAR)
df_summary$MON = as.numeric(df_summary$MON)
df_summary$DAY = as.numeric(df_summary$DAY)
summary(df_summary)
unique(df_summary$tag)

df_summary[is.na(df_summary)] = 0
check = df_summary %>% filter(YEAR == 0)
unique(check$tag)

df_summary = df_summary %>% filter(YEAR != 0)

mhw = read.csv("mhw.csv")

df = df_summary %>% group_by(YEAR, MON, DAY) %>% summarize(mean = mean(temp.))
df$lmean = mean(df$mean)
df$ano = df$lmean - df$mean
df$q90 = quantile(df$mean, 0.9)

df2 = df %>% filter(mean > q90)

df_mhw = NULL
for(i in 1995:2021){
  for(j in 1:12){
    df3 = df2 %>% filter(YEAR == i, MON == j)
    if(nrow(df3) > 4){
      # df3 = left_join(df3, mhw, by = "DAY")
      df_mhw = rbind(df_mhw, df3)
    }
  }
}


tag = unique(df_summary$tag)
for(i in 1:length(tag)){
  df1 = df_summary %>% filter(tag == tag[i])
  df1$temp. = as.numeric(df1$temp.)
  df1$YEAR = as.numeric(df1$YEAR)
  df1$MON = as.numeric(df1$MON)
  df1$DAY = as.numeric(df1$DAY)
  df1$mean = mean(df1$temp.)
  df1$ano = df1$temp.-df1$mean
  
  q = quantile(df1$temp., c(0.1, 0.9))
  # df1$q10 = q[1]
  df1$q90 = q[2]
  
  df2 = df1 %>% filter(temp. > q[2]) %>% arrange(YEAR, MON, DAY)
}