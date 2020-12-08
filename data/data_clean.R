library(tidyverse)
XuechunLi <- read_csv("XuechunLi.csv")
XuechunLi$time <- round(difftime(XuechunLi$time, XuechunLi$time[1],units = 'secs'),3)
write_csv(XuechunLi,"XuechunLi.csv")


YunzhaoWu <- read_csv("YunzhaoWu.csv")
YunzhaoWu$time <- round(difftime(YunzhaoWu$time, YunzhaoWu$time[1],units = 'secs'),3)
write_csv(YunzhaoWu,"YunzhaoWu.csv")

TianyuChang <- read_csv("TianyuChang.csv")
TianyuChang$time <- round(difftime(TianyuChang$time, TianyuChang$time[1],units = 'secs'),3)
write_csv(TianyuChang,"TianyuChang.csv")


ZhihaoGuo <- read_csv("ZhihaoGuo.csv")
ZhihaoGuo$time <- round(difftime(ZhihaoGuo$time, ZhihaoGuo$time[1],units = 'secs'),3)
write_csv(ZhihaoGuo,"ZhihaoGuo.csv")

YuxingLu <- read_csv("YuxingLu.csv")
YuxingLu$time <- round(difftime(YuxingLu$time, YuxingLu$time[1],units = 'secs'),3)
write_csv(YuxingLu,"YuxingLu.csv")
