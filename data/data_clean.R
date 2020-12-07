library(tidyverse)
XuechunLi <- read_csv("XuechunLi.csv")
XuechunLi$time <- round(difftime(XuechunLi$time, XuechunLi$time[1],units = 'secs'),3)
write_csv(XuechunLi,"XuechunLi.csv")


XuechunLi <- read_csv("YunzhaoWu.csv")
XuechunLi$time <- round(difftime(XuechunLi$time, XuechunLi$time[1],units = 'secs'),3)
write_csv(XuechunLi,"YunzhaoWu.csv")

TianyuChang <- read_csv("TianyuChang.csv")
TianyuChang$time <- round(difftime(TianyuChang$time, TianyuChang$time[1],units = 'secs'),3)
write_csv(TianyuChang,"TianyuChang.csv")
