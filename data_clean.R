library(tidyverse)

## loading the data
student_files <- list.files('./data')
student_files <- student_files[grep(".csv",student_files,)]

## students statistics data
student_name <- c()
time <- c()
mean_roll <- c()
sd_roll <- c()
mean_yaw <- c()
sd_yaw <- c()
mean_pitch <- c()
sd_pitch <- c()
for (s in student_files){
  student_name <- c(student_name, str_replace(s,'.csv',''))
  file_name <- paste('./data','/', s,sep = '')
  file <- read.csv(file_name)
  time <- c(time, round(max(file$time),3))
  mean_roll <- c(mean_roll,round(mean(file$roll),3))
  sd_roll <- c(sd_roll,round(sd(file$roll),3))
  mean_yaw <- c(mean_yaw,round(mean(file$yaw),3))
  sd_yaw <- c(sd_yaw,round(sd(file$yaw),3))
  mean_pitch <- c(mean_pitch,round(mean(file$pitch),3))
  sd_pitch <- c(sd_pitch,round(sd(file$pitch),3))
}

student_stat <- data.frame(name = student_name, time = time, mean_roll = mean_roll,
                           sd_roll = sd_roll, mean_yaw = mean_yaw, sd_yaw = sd_yaw, 
                           mean_pitch = mean_pitch, sd_pitch = sd_pitch)

write_csv(student_stat,'./results/student_stat.csv')

