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




## data statistics based on time
name <- c()
yaw_quantile1_mean <- c()
yaw_quantile1_sd <- c()
yaw_quantile2_mean <- c()
yaw_quantile2_sd <- c()
yaw_quantile3_mean <- c()
yaw_quantile3_sd <- c()
yaw_quantile4_mean <- c()
yaw_quantile4_sd <- c()
yaw_quantile5_mean <- c()
yaw_quantile5_sd <- c()

pitch_quantile1_mean <- c()
pitch_quantile1_sd <- c()
pitch_quantile2_mean <- c()
pitch_quantile2_sd <- c()
pitch_quantile3_mean <- c()
pitch_quantile3_sd <- c()
pitch_quantile4_mean <- c()
pitch_quantile4_sd <- c()
pitch_quantile5_mean <- c()
pitch_quantile5_sd <- c()

roll_quantile1_mean <- c()
roll_quantile1_sd <- c()
roll_quantile2_mean <- c()
roll_quantile2_sd <- c()
roll_quantile3_mean <- c()
roll_quantile3_sd <- c()
roll_quantile4_mean <- c()
roll_quantile4_sd <- c()
roll_quantile5_mean <- c()
roll_quantile5_sd <- c()

for (s in student_files){
  name <- c(name, str_replace(s,'.csv',''))
  file_name <- paste('./data','/', s,sep = '')
  file <- read.csv(file_name)
  quant <- round(quantile(1:nrow(file),probs = seq(from=0,to=1,by=0.2)))
  yaw_quantile1_mean <- c(yaw_quantile1_mean,round(mean(file[quant[1]:quant[2],2]),3))
  yaw_quantile1_sd <- c(yaw_quantile1_sd,round(sd(file[quant[1]:quant[2],2]),3))
  yaw_quantile2_mean <- c(yaw_quantile2_mean,round(mean(file[quant[2]:quant[3],2]),3))
  yaw_quantile2_sd <- c(yaw_quantile2_sd,round(sd(file[quant[2]:quant[3],2]),3))
  yaw_quantile3_mean <- c(yaw_quantile3_mean,round(mean(file[quant[3]:quant[4],2]),3))
  yaw_quantile3_sd <- c(yaw_quantile3_sd,round(sd(file[quant[3]:quant[4],2]),3))
  yaw_quantile4_mean <- c(yaw_quantile4_mean,round(mean(file[quant[4]:quant[5],2]),3))
  yaw_quantile4_sd <- c(yaw_quantile4_sd,round(sd(file[quant[4]:quant[5],2]),3))
  yaw_quantile5_mean <- c(yaw_quantile5_mean,round(mean(file[quant[5]:quant[6],2]),3))
  yaw_quantile5_sd <- c(yaw_quantile5_sd,round(sd(file[quant[5]:quant[6],2]),3))
  
  pitch_quantile1_mean <- c(pitch_quantile1_mean,round(mean(file[quant[1]:quant[2],3]),3))
  pitch_quantile1_sd <- c(pitch_quantile1_sd,round(sd(file[quant[1]:quant[2],3]),3))
  pitch_quantile2_mean <- c(pitch_quantile2_mean,round(mean(file[quant[2]:quant[3],3]),3))
  pitch_quantile2_sd <- c(pitch_quantile2_sd,round(sd(file[quant[2]:quant[3],3]),3))
  pitch_quantile3_mean <-  c(pitch_quantile3_mean,round(mean(file[quant[3]:quant[4],3]),3))
  pitch_quantile3_sd <- c(pitch_quantile3_sd,round(sd(file[quant[3]:quant[4],3]),3))
  pitch_quantile4_mean <- c(pitch_quantile4_mean,round(mean(file[quant[4]:quant[5],3]),3))
  pitch_quantile4_sd <- c(pitch_quantile4_sd,round(sd(file[quant[4]:quant[5],3]),3))
  pitch_quantile5_mean <- c(pitch_quantile5_mean,round(mean(file[quant[5]:quant[6],3]),3))
  pitch_quantile5_sd <- c(pitch_quantile5_sd,round(sd(file[quant[5]:quant[6],3]),3))
  
  roll_quantile1_mean <- c(roll_quantile1_mean,round(mean(file[quant[1]:quant[2],4]),3))
  roll_quantile1_sd <- c(roll_quantile1_sd,round(sd(file[quant[1]:quant[2],4]),3))
  roll_quantile2_mean <- c(roll_quantile2_mean,round(mean(file[quant[2]:quant[3],4]),3))
  roll_quantile2_sd <- c(roll_quantile2_sd,round(sd(file[quant[2]:quant[3],4]),3))
  roll_quantile3_mean <- c(roll_quantile3_mean,round(mean(file[quant[3]:quant[4],4]),3))
  roll_quantile3_sd <- c(roll_quantile3_sd,round(sd(file[quant[3]:quant[4],4]),3))
  roll_quantile4_mean <- c(roll_quantile4_mean,round(mean(file[quant[4]:quant[5],4]),3))
  roll_quantile4_sd <- c(roll_quantile4_sd,round(sd(file[quant[4]:quant[5],4]),3))
  roll_quantile5_mean <-  c(roll_quantile5_mean,round(mean(file[quant[5]:quant[6],4]),3))
  roll_quantile5_sd <- c(roll_quantile5_sd,round(sd(file[quant[5]:quant[6],4]),3))
  
}

student_stat <- data.frame(name = name,
                           yaw_quantile1_mean = yaw_quantile1_mean,
                           yaw_quantile1_sd = yaw_quantile1_sd, 
                           yaw_quantile2_mean = yaw_quantile2_mean,
                           yaw_quantile2_sd =yaw_quantile2_sd,
                           yaw_quantile3_mean =yaw_quantile3_mean,
                           yaw_quantile3_sd =yaw_quantile3_sd,
                           yaw_quantile4_mean =yaw_quantile4_mean,
                           yaw_quantile4_sd =yaw_quantile4_sd,
                           yaw_quantile5_mean =yaw_quantile5_mean,
                           yaw_quantile5_sd=yaw_quantile5_sd,
                           pitch_quantile1_mean=pitch_quantile1_mean,
                           pitch_quantile1_sd=pitch_quantile1_sd,
                           pitch_quantile2_mean=pitch_quantile2_mean,
                           pitch_quantile2_sd=pitch_quantile2_sd,
                           pitch_quantile3_mean=pitch_quantile3_mean,
                           pitch_quantile3_sd=pitch_quantile3_sd,
                           pitch_quantile4_mean=pitch_quantile4_mean,
                           pitch_quantile4_sd=pitch_quantile4_sd,
                           pitch_quantile5_mean=pitch_quantile5_mean,
                           pitch_quantile5_sd=pitch_quantile5_sd,
                           roll_quantile1_mean=roll_quantile1_mean,
                           roll_quantile1_sd=roll_quantile1_sd,
                           roll_quantile2_mean=roll_quantile2_mean,
                           roll_quantile2_sd=roll_quantile2_sd,
                           roll_quantile3_mean=roll_quantile3_mean,
                           roll_quantile3_sd=roll_quantile3_sd,
                           roll_quantile4_mean=roll_quantile4_mean,
                           roll_quantile4_sd=roll_quantile4_sd,
                           roll_quantile5_mean=roll_quantile5_mean,
                           roll_quantile5_sd=roll_quantile5_sd)

write_csv(student_stat,'./results/student_quantile.csv')


