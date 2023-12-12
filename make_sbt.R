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
    # head(df2)
    # df_summary_temp = df2 %>% group_by(tag) %>% summarize(mean = mean(as.numeric(temp.)), sd = sd(as.numeric(temp.))) %>% mutate(month = as.numeric(paste(j)), year = as.numeric(paste(i)))
    # df_summary = rbind(df_summary, df_summary_temp)
  }
  head(df2)
  df2_2 = df2[-1, ]
  # df_summary_temp = df2_2 %>% group_by(tag) %>% summarize(mean = mean(as.numeric(temp.)), sd = sd(as.numeric(temp.))) %>% mutate(year = as.numeric(paste(i)))
  df_summary = rbind(df_summary, df2_2)
}

setwd(dir_sbt)
write.csv(df_summary, "df_summary_daily.csv", fileEncoding = "CP932", row.names = F)

