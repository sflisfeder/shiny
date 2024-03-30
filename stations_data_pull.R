library(weathercan)
library(sp)
library(dplyr)
library(tidyr)
#list of stations of interest

#Ottawa Airport
#load previous day
load("yow.RData")


#fetch most recent data
if (!((Sys.Date()-1) %in% yow$date)){
  yow_temp <-  weather_dl(station_ids = 49568, start = (tail(yow$date, n = 1)+1),  interval = "day", string_as = NULL)
  yow <- rbind(yow, yow_temp)
  rm(list="yow_temp")
}

#save the most up to date data
save(yow, file = "yow.RData")


# yow1 <- weather_dl(station_ids = 4337, interval = "day", string_as = NULL)
# yow2 <- weather_dl(station_ids = 49568, interval = "day", string_as = NULL)
# yow <- rbind(yow1, yow2)
# yow <- yow[(order(as.Date(yow$date))),]

#Pearson Airport
#load previous day
load("yyz.RData")


#fetch most recent data
if (!((Sys.Date()-1) %in% pearson$date)){
  yyz_temp <-  weather_dl(station_ids = 51459, start = (tail(pearson$date, n = 1)+1),  interval = "day", string_as = NULL)
  pearson <- rbind(pearson, yyz_temp)
  rm(list="yyz_temp")
}

#save the most up to date data
save(pearson, file = "yyz.RData")

# pearson1 <- weather_dl(station_ids = 51459, interval = "day", string_as = NULL)
# pearson2 <- weather_dl(station_ids = 5097, interval = "day", string_as = NULL)
# pearson <- rbind(pearson1,pearson2)
# pearson <- pearson[(order(as.Date(pearson$date))),]


#Toronto City
#load previous day
load("xto.RData")


#fetch most recent data
if (!((Sys.Date()-1) %in% xto$date)){
  xto_temp <-  weather_dl(station_ids = 31688, start = (tail(xto$date, n = 1)+1),  interval = "day", string_as = NULL)
  xto <- rbind(xto, xto_temp)
  rm(list="xto_temp")
}

#save the most up to date data
save(xto, file = "xto.RData")

# xto1 <- weather_dl(station_ids = 31688, interval = "day", string_as = NULL)
# xto2 <- weather_dl(station_ids = 5051, interval = "day", string_as = NULL)
# xto2 <- xto2[!(xto2$date %in% xto1$date),]
# xto <- rbind(xto1, xto2)
# xto <- xto[(order(as.Date(xto$date))),]

#Hamilton Airport
load("yhm.RData")


#fetch most recent data
if (!((Sys.Date()-1) %in% yhm$date)){
  yhm_temp <-  weather_dl(station_ids = 49908, start = (tail(yhm$date, n = 1)+1),  interval = "day", string_as = NULL)
  yhm <- rbind(yhm, yhm_temp)
  rm(list="yhm_temp")
}

#save the most up to date data
save(yhm, file = "yhm.RData")

# yhm1 <- weather_dl(station_ids = 4932, interval = "day", string_as = NULL)
# yhm2 <- weather_dl(station_ids = 49908, interval = "day", string_as = NULL)
# yhm <- rbind(yhm1, yhm2)
# yhm <- yhm[(order(as.Date(yhm$date))),]

#Windsor Airport
#load previous day
load("yqg.RData")


#fetch most recent data
if (!((Sys.Date()-1) %in% yqg$date)){
  yqg_temp <-  weather_dl(station_ids = 54738, start = (tail(yqg$date, n = 1)+1),  interval = "day", string_as = NULL)
  yqg <- rbind(yqg, yqg_temp)
  rm(list="yqg_temp")
}

#save the most up to date data
save(yqg, file = "yqg.RData")


#yqg1 <- weather_dl(station_ids = 4716, interval = "day", string_as = NULL)
#yqg2 <- weather_dl(station_ids = 52838, interval = "day", string_as = NULL)
#yqg3 <- weather_dl(station_ids = 54738, interval = "day", string_as = NULL)
#yqg <- rbind(yqg1, yqg2)
# yqg <- rbind(yqg, yqg3)
# yqg <- yqg[(order(as.Date(yqg$date))),]

#Thunder Bay (frankenstein)
load("yqt.RData")

if (!((Sys.Date()-1) %in% yqt$date)){
  yqt_temp <-  weather_dl(station_ids = 50132, start = (tail(yqt$date, n = 1)+1),  interval = "day", string_as = NULL)
  yqt <- rbind(yqt, yqt_temp)
  rm(list="yqt_temp")
}

# yqt1 <- weather_dl(station_ids = 49389, interval = "day", string_as = NULL)
# yqt2 <- weather_dl(station_ids = 4055, interval = "day", string_as = NULL)
# yqt2 <- yqt2[!(yqt2$date %in% yqt1$date),]
# yqt <- rbind(yqt1,yqt2)
# yqt3 <- weather_dl(station_ids = 50132, interval = "day", string_as = NULL)
# yqt3 <- yqt3[!(yqt3$date %in% yqt$date),]
# yqt <- rbind(yqt, yqt3)
# ztb <- weather_dl(station_ids = 30682, interval = "day", string_as = NULL)
# ztb <- ztb[!(ztb$date %in% yqt$date),]
# yqt <- rbind(yqt,ztb)

save(yqt, file = "yqt.RData")

#Kenora Airport
#load previous day
load("yqk.RData")


#fetch most recent data
if (!((Sys.Date()-1) %in% yqk$date)){
  yqk_temp <-  weather_dl(station_ids = 51137, start = (tail(yqk$date, n = 1)+1),  interval = "day", string_as = NULL)
  yqk <- rbind(yqk, yqk_temp)
  rm(list="yqk_temp")
}

#save the most up to date data
save(yqk, file = "yqk.RData")

# yqk1 <- weather_dl(station_ids = 3960, interval = "day", string_as = NULL)
# yqk2 <- weather_dl(station_ids = 51137, interval = "day", string_as = NULL)
# yqk <- rbind(yqk1, yqk2)

#Montreal Airport
#load previous day
load("yul.RData")


#fetch most recent data
if (!((Sys.Date()-1) %in% yul$date)){
  yul_temp <-  weather_dl(station_ids = 51157, start = (tail(yul$date, n = 1)+1),  interval = "day", string_as = NULL)
  yul <- rbind(yul, yul_temp)
  rm(list="yul_temp")
}

#save the most up to date data
save(yul, file = "yul.RData")


# yul1 <- weather_dl(station_ids = 5415, interval = "day", string_as = NULL)
# yul2 <- weather_dl(station_ids = 51157, interval = "day", string_as = NULL)
# yul <- rbind(yul1, yul2)
# yul <- yul[(order(as.Date(yul$date))),]

#London Airport

load("yxu.RData")


#fetch most recent data
if (!((Sys.Date()-1) %in% yxu$date)){
  yxu_temp <-  weather_dl(station_ids = 50093, start = (tail(yxu$date, n = 1)+1),  interval = "day", string_as = NULL)
  yxu <- rbind(yxu, yxu_temp)
  rm(list="yxu_temp")
}
# 
# yxu1 <- weather_dl(station_ids = 4789, interval = "day", string_as = NULL)
# yxu2 <- weather_dl(station_ids = 50093, interval = "day", string_as = NULL)
# yxu2 <- yxu2[!(yxu2$date %in% yxu1$date),]
# yxu <- rbind(yxu1, yxu2)
# yxu <- yxu[(order(as.Date(yxu$date))),]

save(yxu, file = "yxu.RData")


#Winnipeg
load("ywg.RData")


#fetch most recent data
if (!((Sys.Date()-1) %in% ywg$date)){
  ywg_temp <-  weather_dl(station_ids = 51097, start = (tail(ywg$date, n = 1)+1),  interval = "day", string_as = NULL)
  ywg <- rbind(ywg, ywg_temp)
  rm(list="ywg_temp")
}
# 
# ywg1 <- weather_dl(station_ids = 3703, interval = "day", string_as = NULL)
# ywg2 <- weather_dl(station_ids = 3698, interval = "day", string_as = NULL)
# ywg3 <- weather_dl(station_ids = 47407, interval = "day", string_as = NULL)
# ywg4 <- weather_dl(station_ids = 51097, interval = "day", string_as = NULL)
# ywg5 <- weather_dl(station_ids = 28051, interval = "day", string_as = NULL)
# ywg5 <- ywg5[!(ywg5$date %in% ywg4$date),]
# 
# ywg <- rbind(ywg1, ywg2)
# ywg <- rbind(ywg,ywg3)
# ywg <- rbind(ywg,ywg4)
# ywg <- rbind(ywg, ywg5)
# ywg <- ywg[(order(as.Date(ywg$date))),]

save(ywg, file = "ywg.RData")

#Quebec City
load("yqb.RData")


#fetch most recent data
if (!((Sys.Date()-1) %in% yqb$date)){
  yqb_temp <-  weather_dl(station_ids = 51457, start = (tail(yqb$date, n = 1)+1),  interval = "day", string_as = NULL)
  yqb <- rbind(yqb, yqb_temp)
  rm(list="yqb_temp")
}
# 
# yqb1 <- weather_dl(station_ids = 5251, interval = "day", string_as = NULL)
# yqb2 <- weather_dl(station_ids = 51457, interval = "day", string_as = NULL)
# # 
# yqb <- rbind(yqb1, yqb2)
# 
# yqb <- yqb[(order(as.Date(yqb$date))),]

save(yqb, file = "yqb.RData")

#Timmins
load("yts.RData")


#fetch most recent data
if (!((Sys.Date()-1) %in% yts$date)){
  yts_temp <-  weather_dl(station_ids = 50460, start = (tail(yqb$date, n = 1)+1),  interval = "day", string_as = NULL)
  yts <- rbind(yts, yts_temp)
  rm(list="yts_temp")
}
# 
# yts1 <- weather_dl(station_ids = 4179, interval = "day", string_as = NULL)
# yts2 <- weather_dl(station_ids = 4180, interval = "day", string_as = NULL)
# yts2 <- yts2[!(yts2$date %in% yts1$date),]
# 
# yts3 <- weather_dl(station_ids = 50460, interval = "day", string_as = NULL)
# #
# yts <- rbind(yts1, yts2)
# yts <- rbind(yts, yts3)
# 
# yts <- yts[(order(as.Date(yts$date))),]

save(yts, file = "yts.RData")


# yyz <- pearson %>% mutate(Julian = format(as.Date(date), format = "%j"))
# yyz %>%
#   filter(Julian < 364 & year >= "1991" & year <= "2020") %>%
#   ggplot() + geom_jitter(aes(x = as.Date(as.numeric(Julian), origin = "2020-01-01"), y = max_temp), alpha = 0.2) +
#   geom_vline(xintercept = as.Date(199, origin = "2020-01-01"), color = "blue", linetype = "dotted") +
#   geom_vline(xintercept = as.Date(201, origin = "2020-01-01"), color = "blue", linetype = "dotted") +
#   scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0.005,0.005))  + xlab("") +
#   ylab(expression("Maximum Daily Temperature"*~degree*C)) + theme_ipsum() #+
# #geom_line(data = yyz_91_20_daily, aes(x = as.Date(as.numeric(Julian), origin = "2020-01-01"), y = avg_max), color = "blue")
# annotate("text", x = 266, y = yyz$max_temp[266], label = "test")
# 
# 
# yxu <- yxu %>% mutate(Julian = format(as.Date(date), format = "%j"))
# yxu %>%
#   filter(Julian < 364 & year >= "1991" & year <= "2020") %>%
#   ggplot() + geom_jitter(aes(x = as.Date(as.numeric(Julian), origin = "2020-01-01"), y = max_temp), alpha = 0.2) +
#   geom_vline(xintercept = as.Date(199, origin = "2020-01-01"), color = "blue", linetype = "dotted") +
#   geom_vline(xintercept = as.Date(201, origin = "2020-01-01"), color = "blue", linetype = "dotted") +
#   scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0.005,0.005))  + xlab("") +
#   ylab(expression("Maximum Daily Temperature"*~degree*C)) + theme_ipsum()

# ggplot(data = yyz_dec_snow) +
#   geom_tile(aes(x = as.numeric(day), y = as.numeric(year), fill = snow_grnd), color = "white") +
#   theme_ipsum() + scale_fill_viridis(option = "magma") +
#   labs(title = "December Snow on Ground", subtitle = "Pearson Int'l 1955-Present",
#        x = "Day", y = "Year", fill = "Snow on Ground") +
#   annotate("rect", xmin = 24.5, xmax = 25.5, ymin = 1954, ymax = 2024, color = "red",
#            fill = "red", alpha = 0.25) + theme(legend.position = "bottom",
#                                                legend.key.size = unit(1.25, 'cm'),
#                                                legend.title = element_text(size = 16))



# yyz_mar <- pearson %>%
#   filter(month == "03") %>%
#   group_by(year) %>%
#   summarise(avgMax = mean(max_temp, na.rm = TRUE),
#             avgMin = mean(min_temp,na.rm = TRUE),
#             avgMean = mean(mean_temp, na.rm = TRUE),
#             maxT = max(max_temp, na.rm = TRUE),
#             minT = min(min_temp, na.rm = TRUE),
#             totRA = sum(total_rain, na.rm = TRUE),
#             totSN = sum(total_snow, na.rm = TRUE),
#             missSN = sum(is.na(total_snow)),
#             totPCP = sum(total_precip, na.rm = TRUE),
#             maxGust = max(spd_max_gust, na.rm = TRUE),
#             above0 = sum(min_temp > 0, na.rm = TRUE),
#             above10 = sum(max_temp > 10, na.rm = TRUE),
#             max_above0 = sum(max_temp >0, na.rm = TRUE),
#             below0 = sum(max_temp < 0, na.rm = TRUE),
#             miss_SN = sum(is.na(total_snow)))

# yyz_snow_year <- pearson %>% 
#   mutate(snow_year = ifelse(month >= "07", year, as.numeric(year)-1))
# 
# windsor_snow_year <- windsor %>% 
#   mutate(snow_year = ifelse(month >= "07", year, as.numeric(year)-1))

# xto_snow_year <- xto %>% 
#   mutate(snow_year = ifelse(month >= "07", year, as.numeric(year)-1))
# 
# yow_snow_year <- yow %>% 
#   mutate(snow_year = ifelse(month >= "07", year, as.numeric(year)-1))
# 
# ywg_snow_year <- ywg %>% 
#   mutate(snow_year = ifelse(month >= "07", year, as.numeric(year)-1))
# 
# yul_snow_year <- yul %>% 
#   mutate(snow_year = ifelse(month >= "07", year, as.numeric(year)-1))
# 
# yqg_snow_year <- yqg %>% 
#   mutate(snow_year = ifelse(month >= "07", year, as.numeric(year)-1))

# yyz_winter <- yyz_snow_year %>% 
#   filter(month %in% c("12", "01", "02")) %>% 
#   group_by(snow_year) %>% 
#   summarise(avgMax = mean(max_temp, na.rm = TRUE),
#             avgMin = mean(min_temp,na.rm = TRUE),
#             avgMean = mean(mean_temp, na.rm = TRUE),
#             maxT = max(max_temp, na.rm = TRUE),
#             maxT_date = date[which(max_temp == maxT)],
#             maxGust = max(spd_max_gust, na.rm = TRUE),
#             miss_max = sum(is.na(max_temp)),
#             minT = min(min_temp, na.rm = TRUE),
#             totRA = sum(total_rain, na.rm = TRUE),
#             totSN = sum(total_snow, na.rm = TRUE),
#             totPCP = sum(total_precip, na.rm = TRUE),
#             above0 = sum(min_temp > 0, na.rm = TRUE),
#             above10 = sum(max_temp > 10, na.rm = TRUE),
#             below0 = sum(max_temp < 0, na.rm = TRUE))
