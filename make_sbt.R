require(tidyverse)
require(ggh4x)
require(gridExtra)
require(maps)
require(mapdata)

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
df_summary$lonE = as.numeric(df_summary$lonE)
df_summary$latE = as.numeric(df_summary$latE)
summary(df_summary)
unique(df_summary$tag)

df_summary[is.na(df_summary)] = 0
check = df_summary %>% filter(YEAR == 0)
unique(check$tag)

df_summary = df_summary %>% filter(YEAR != 0) %>% filter(between(lonE, 140.5, 143)) %>% filter(between(latE, 36, 42))

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

df4 = df_summary %>% mutate(mean = mean(temp.)) %>% mutate(ano = mean - temp.)

col = scale_fill_gradient2(low='blue', mid='gray80', high='red4', midpoint = 0)
pal = c("midnightblue", "steelblue2", "gray80", "palegoldenrod", "tan1", "tomato", "red", "red4")
pal = c("midnightblue", "steelblue2", "gray80", "palegoldenrod", "red", "red4")
col = scale_fill_gradientn(colors = pal)

world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 35 & jap$lat < 45 & jap$long > 130 & jap$long < 146, ]
pol = geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="black", fill="black")
c_map = coord_map(projection = "azequidistant", xlim = c(min(df4$lonE), max(df4$lonE)), ylim = c(min(df4$latE), max(df4$latE)))
# g+pol+c_map

g = ggplot()
t = geom_tile(data = df4 %>% filter(YEAR == 2021) %>% filter(MON == 8), aes(x = lonE, y = latE, z = ano, fill = ano))
c = geom_contour(data = df4, aes(x = lonE, y = latE, z = ano, fill = ano), color='black', bins = 50)
f = facet_wrap(~ MON, ncol = 6)
labs = labs(x = "Longitude", y = "Latitude", fill = "Anomaly")

fig = g+t+c+labs+pol+c_map+theme_bw()+col+f
ggsave(filename = paste0("ano2021.pdf"), plot = fig, units = "in", width = 8.27, height = 11.69)
