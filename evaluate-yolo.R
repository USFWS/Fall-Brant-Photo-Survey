#evaluate YOLO training model results
#adapted from Weiser script, script 02 - evaluate YOLO counts.r
library(tidyverse)
library(ggpubr)
################################################################################
# three years, multiple replicate per year
# only for the replicate on 2017-10-03 were all photos manually counts
# on all other replicates, only those photos where CountThings found geese were manually counted
# read in data for manually counted photos
files <- list.files(path="goose_fallSurveyCounts_izembek_weiser/goose_fallSurveyCounts_izembek_weiser", 
                    full.names = TRUE,
                    recursive = TRUE,
                    pattern = "csv")
#the first csv file seems to have redundant column, delete and add
dat1 <- read_csv(files[1], col_select = 1:20) %>%
  mutate(File = 1)
dat <- map_dfr(files[-1], read_csv, .id = "File") %>%
  mutate(File = as.numeric(File)+1)
#rearrange dat1
dat1 <- dat1[,c(names(dat1)[21],names(dat1)[-21])]
names(dat1) <- names(dat)
dat <- bind_rows(dat1, dat)
rm(dat1)
dat <- dat %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>%
  # mutate(Replicate = if_else(File %in% 1:2, 1, 
  #                            if_else(File %in% 3:4, 2,
  #                                    if_else(File %in% 5:6, 3, 
  #                                            if_else(File %in% 7:8, 4, 
  #                                                    if_else(File %in% 9:10, 5, NA)))))) %>%
  #add common file name to manually counted data
  mutate(FileName = paste0(JPG, ".JPG"))
#in dat there seems to be a duplicate row for CAM10003 on 2017-10-03
#there are actually three duplicate rows, try duplicate(dat), we will remove these
dat <- distinct(dat)
################################################################################
#read in yolo results
# there are three trained models as described in email from Dan:
#   (1) ...e200-best... was the one produced after in Sep of 2023 after a bug 
#       was discovered in original training run
#   (2) ...2023-09-02-best... was a 'checkpoint' that Dan saved during training 
#       and thought this might be better than the final model (see email)
#   (3) ...ss.best... was the original trained model with bug. Dan is sure this 
#       one is worse than either of the first two. 
# each model was then used to count photos from an (1) "evaluation set" and 
#  (2) all the photos for the 2017-10-03 survey
# We will combine these for evaluation
#first from model usgs-geese-yolov5x-230820-b8-img1280-e200-best and "eval" set, see email
path1 <- "YOLO-training-inference-results/2023-09-11-results/counts-e200-best-eval.csv"
yolo1 <- read_csv(file = path1) 
path2 <- "YOLO-training-inference-results/2023-09-11-results/counts-e200-best-rep.csv"
yolo_best <- read_csv(file = path2) %>%
  rbind(yolo1) %>%
  mutate(FileName = str_sub(filename, -12, -1), 
         Date = as.Date(str_sub(filename, 34, 43))) %>% 
  left_join(dat, by = c("FileName", "Date")) %>%
  filter(!is.na(Latitude)) #drop yolo count not in CountThings or manually counted set
#now load next model:
# ...2023-09-02-best...
path1 <- "YOLO-training-inference-results/2023-09-11-results/counts-2023.09.02-best-eval.csv"
yolo1 <- read_csv(file = path1) 
path2 <- "YOLO-training-inference-results/2023-09-11-results/counts-2023.09.02-best-rep.csv"
yolo_best2 <- read_csv(file = path2) %>%
  rbind(yolo1) %>%
  mutate(FileName = str_sub(filename, -12, -1), 
         Date = as.Date(str_sub(filename, 34, 43))) %>% 
  left_join(dat, by = c("FileName", "Date")) %>%
  filter(!is.na(Latitude)) #drop yolo count not in CountThings or manually counted set
#Now load original model with bug
# ...ss-best...
path1 <- "YOLO-training-inference-results/2023-09-11-results/counts-ss-best-eval.csv"
yolo1 <- read_csv(file = path1) 
path2 <- "YOLO-training-inference-results/2023-09-11-results/counts-ss-best-rep.csv"
yolo_ss <- read_csv(file = path2) %>%
  rbind(yolo1) %>%
  mutate(FileName = str_sub(filename, -12, -1), 
         Date = as.Date(str_sub(filename, 34, 43))) %>% 
  left_join(dat, by = c("FileName", "Date")) %>%
  filter(!is.na(Latitude)) #drop yolo count not in CountThings or manually counted set
################################################################################
################################################################################
#plot some results
df <- select(yolo_best, FileName, Date, threshold_brant, 
             YOLO_Brant = count_brant, Manual_Brant) %>%
  mutate(threshold_brant = factor(threshold_brant)) %>%
  filter(!is.na(Manual_Brant))
p1 <- ggplot(data = df, aes(x=Manual_Brant, y = YOLO_Brant, color = threshold_brant)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  labs(title = "e200-best")
df <- select(yolo_best2, FileName, Date, threshold_brant, 
             YOLO_Brant = count_brant, Manual_Brant) %>%
  mutate(threshold_brant = factor(threshold_brant)) %>%
  filter(!is.na(Manual_Brant))
p2 <- ggplot(data = df, aes(x=Manual_Brant, y = YOLO_Brant, color = threshold_brant)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  labs(title = "checkpoint-best")
df <- select(yolo_ss, FileName, Date, threshold_brant, 
             YOLO_Brant = count_brant, Manual_Brant) %>%
  mutate(threshold_brant = factor(threshold_brant)) %>%
  filter(!is.na(Manual_Brant))
p3 <- ggplot(data = df, aes(x=Manual_Brant, y = YOLO_Brant, color = threshold_brant)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  labs(title = "ss-original")
ggarrange(p1, p2, p3, common.legend = TRUE)
# #what about where yolo found something but CountThings did not?
# #exclude 2017-10-03 replicate
# #Are these false positives or correct detections?
# df <- yolo %>% 
#   filter(is.na(Manual_Brant)) %>%
#   select(FileName, Date, threshold_brant, count_brant, Manual_Brant) %>%
#   mutate(threshold_brant = threshold_brant, Replicate = factor(Date)) %>%
#   group_by(threshold_brant, Replicate) %>%
#   summarise(YOLO_Count = sum(count_brant), n = n())
# ggplot(data = df, aes(x=threshold_brant, y = YOLO_Count, col=Replicate)) +
#   geom_line() + 
#   scale_x_continuous(breaks = 1:9/10)
# #look only at replicate 4 where everything was manually counted
# df <- yolo %>% 
#   filter(Date == "2017-10-03") %>%
#   select(FileName, Date, threshold_brant, count_brant, Manual_Brant) %>%
#   mutate(Replicate = factor(Date)) %>%
#   group_by(threshold_brant) %>%
#   summarise(YOLO_Count = sum(count_brant), Manual_Count = sum(Manual_Brant), 
#             n = n())
# ggplot(data = df, aes(x=threshold_brant, y = YOLO_Count)) +
#   geom_line() + 
#   geom_abline(aes(slope = 0, intercept = Manual_Count)) + 
#   scale_x_continuous(breaks = 1:9/10)
################################################################################
#drop everything without a manual count (don't really know 'truth' here)
df <- yolo_best %>% 
  filter(!is.na(Manual_Brant)) %>%
  select(FileName, Date, threshold_brant, count_brant, Manual_Brant) %>%
  mutate(Replicate = factor(Date)) %>%
  group_by(threshold_brant, Replicate) %>%
  summarise(YOLO_Count = sum(count_brant), Manual_Count = sum(Manual_Brant), 
            pCount = YOLO_Count/Manual_Count,  
            n = n())
# ggplot(data = df, aes(x=threshold_brant, y = YOLO_Count, col=Replicate)) +
#   geom_line() + 
#   geom_abline(aes(slope = 0, intercept = Manual_Count)) + 
#   scale_x_continuous(breaks = 1:9/10)
p1 <- ggplot(data = df, aes(x=threshold_brant, y = pCount, col=Replicate)) +
  geom_line() + 
  geom_abline(aes(slope = 0, intercept = 1)) + 
  scale_x_continuous(breaks = 1:9/10) + 
  labs(title = "e200-best")
df <- yolo_best2 %>% 
  filter(!is.na(Manual_Brant)) %>%
  select(FileName, Date, threshold_brant, count_brant, Manual_Brant) %>%
  mutate(Replicate = factor(Date)) %>%
  group_by(threshold_brant, Replicate) %>%
  summarise(YOLO_Count = sum(count_brant), Manual_Count = sum(Manual_Brant), 
            pCount = YOLO_Count/Manual_Count,  
            n = n())
p2 <- ggplot(data = df, aes(x=threshold_brant, y = pCount, col=Replicate)) +
  geom_line() + 
  geom_abline(aes(slope = 0, intercept = 1)) + 
  scale_x_continuous(breaks = 1:9/10) + 
  labs(title = "checkpoint-best")
df <- yolo_ss %>% 
  filter(!is.na(Manual_Brant)) %>%
  select(FileName, Date, threshold_brant, count_brant, Manual_Brant) %>%
  mutate(Replicate = factor(Date)) %>%
  group_by(threshold_brant, Replicate) %>%
  summarise(YOLO_Count = sum(count_brant), Manual_Count = sum(Manual_Brant), 
            pCount = YOLO_Count/Manual_Count,  
            n = n())
p3 <- ggplot(data = df, aes(x=threshold_brant, y = pCount, col=Replicate)) +
  geom_line() + 
  geom_abline(aes(slope = 0, intercept = 1)) + 
  scale_x_continuous(breaks = 1:9/10) + 
  labs(title = "ss-original")
ggarrange(p1, p2, p3, common.legend = TRUE, ncol = 3, nrow = 1)
################################################################################
#look at false positives
#How many photos does YOLO say have geese but none were found in manual counts
#this includes where CountThings did not find any, Manual_Brant == 0
df <- yolo_best %>% 
  filter(count_brant > 0 & (Manual_Brant == 0 | is.na(Manual_Brant))) %>%
  mutate(Replicate = factor(Date)) %>%
  group_by(threshold_brant, Replicate) %>%
  summarise(YOLO_Brant = sum(count_brant), n = n())
p1 <- ggplot(data = df, aes(x=threshold_brant, y = YOLO_Brant, col=Replicate)) +
  geom_line() + 
  scale_x_continuous(breaks = 1:9/10) + 
  scale_y_continuous(trans = "log10") + 
  labs(title = "e200-best")
#find total number of photos manually counted by replicate
mtot <- yolo_best %>% 
  #filter(! is.na(Manual_Brant)) %>%
  mutate(Replicate = factor(Date)) %>%
  group_by(threshold_brant, Replicate) %>%
  summarise(n_Manual = n())
df <- left_join(mtot, df) %>%
  replace_na(list(YOLO_Brant = 0, n = 0)) %>%
  mutate(pFalsePositives = n/n_Manual)
#plot proportion of photos that are false positive
pp1 <- ggplot(data = df, aes(x=threshold_brant, y = pFalsePositives, col=Replicate)) +
  geom_line() + 
  scale_x_continuous(breaks = 1:9/10) + 
  labs(title = "e200-best")
df <- yolo_best2 %>% 
  filter(count_brant > 0 & (Manual_Brant == 0 | is.na(Manual_Brant))) %>%
  mutate(Replicate = factor(Date)) %>%
  group_by(threshold_brant, Replicate) %>%
  summarise(YOLO_Brant = sum(count_brant), n = n())
p2 <- ggplot(data = df, aes(x=threshold_brant, y = YOLO_Brant, col=Replicate)) +
  geom_line() + 
  scale_x_continuous(breaks = 1:9/10) + 
  scale_y_continuous(trans = "log10") + 
  labs(title = "checkpoint-best")
#find total number of photos manually counted by replicate
mtot <- yolo_best2 %>% 
  #filter(! is.na(Manual_Brant)) %>%
  mutate(Replicate = factor(Date)) %>%
  group_by(threshold_brant, Replicate) %>%
  summarise(n_Manual = n())
df <- left_join(mtot, df) %>%
  replace_na(list(YOLO_Brant = 0, n = 0)) %>%
  mutate(pFalsePositives = n/n_Manual)
#plot proportion of photos that are false positive
pp2 <- ggplot(data = df, aes(x=threshold_brant, y = pFalsePositives, col=Replicate)) +
  geom_line() + 
  scale_x_continuous(breaks = 1:9/10) + 
  labs(title = "checkpoint-best")
df <- yolo_ss %>% 
  filter(count_brant > 0 & (Manual_Brant == 0 | is.na(Manual_Brant))) %>%
  mutate(Replicate = factor(Date)) %>%
  group_by(threshold_brant, Replicate) %>%
  summarise(YOLO_Brant = sum(count_brant), n = n())
p3 <- ggplot(data = df, aes(x=threshold_brant, y = YOLO_Brant, col=Replicate)) +
  geom_line() + 
  scale_x_continuous(breaks = 1:9/10) + 
  scale_y_continuous(trans = "log10") + 
  labs(title = "ss-original")
#find total number of photos manually counted by replicate
mtot <- yolo_ss %>% 
  #filter(! is.na(Manual_Brant)) %>%
  mutate(Replicate = factor(Date)) %>%
  group_by(threshold_brant, Replicate) %>%
  summarise(n_Manual = n())
df <- left_join(mtot, df) %>%
  replace_na(list(YOLO_Brant = 0, n = 0)) %>%
  mutate(pFalsePositives = n/n_Manual)
#plot proportion of photos that are false positive
pp3 <- ggplot(data = df, aes(x=threshold_brant, y = pFalsePositives, col=Replicate)) +
  geom_line() + 
  scale_x_continuous(breaks = 1:9/10) + 
  labs(title = "ss-original")
ggarrange(p1, p2, p3, common.legend = TRUE, ncol = 3, nrow = 1)
ggarrange(pp1, pp2, pp3, common.legend = TRUE, ncol = 3, nrow = 1)
################################################################################
#Now look at false negatives
df <- yolo_best %>% 
  filter(Manual_Brant > 0 & count_brant < Manual_Brant) %>%
  mutate(Replicate = factor(Date), Missed_Brant = Manual_Brant - count_brant) %>%
  group_by(threshold_brant, Replicate) %>%
  summarise(YOLO_Brant = sum(count_brant), Manual_Brant = sum(Manual_Brant),
            Missed_Brant = sum(Missed_Brant),
            n = n())
p1 <- ggplot(data = df, aes(x=threshold_brant, y = Missed_Brant, col=Replicate)) +
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = 1:9/10) + 
  scale_y_continuous(trans = "log10") + 
  labs(title = "e200-best")
mtot <- yolo_best %>% 
  #filter(! is.na(Manual_Brant)) %>%
  mutate(Replicate = factor(Date)) %>%
  group_by(threshold_brant, Replicate) %>%
  summarise(n_Manual = n())
df <- left_join(mtot, df) %>%
  replace_na(list(Manual_Brant = 0, n = 0)) %>%
  mutate(pFalseNegatives = n/n_Manual)
#plot proportion of photos that are false negatives
pp1 <- ggplot(data = df, aes(x=threshold_brant, y = pFalseNegatives, col=Replicate)) +
  geom_line() + 
  scale_x_continuous(breaks = 1:9/10) + 
  labs(title = "e200-best")
df <- yolo_best2 %>% 
  filter(Manual_Brant > 0 & count_brant < Manual_Brant) %>%
  mutate(Replicate = factor(Date), Missed_Brant = Manual_Brant - count_brant) %>%
  group_by(threshold_brant, Replicate) %>%
  summarise(YOLO_Brant = sum(count_brant), Manual_Brant = sum(Manual_Brant),
            Missed_Brant = sum(Missed_Brant),
            n = n())
p2 <- ggplot(data = df, aes(x=threshold_brant, y = Missed_Brant, col=Replicate)) +
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = 1:9/10) + 
  scale_y_continuous(trans = "log10") + 
  labs(title = "checkpoint-best")
mtot <- yolo_best2 %>% 
  #filter(! is.na(Manual_Brant)) %>%
  mutate(Replicate = factor(Date)) %>%
  group_by(threshold_brant, Replicate) %>%
  summarise(n_Manual = n())
df <- left_join(mtot, df) %>%
  replace_na(list(Manual_Brant = 0, n = 0)) %>%
  mutate(pFalseNegatives = n/n_Manual)
#plot proportion of photos that are false negatives
pp2 <- ggplot(data = df, aes(x=threshold_brant, y = pFalseNegatives, col=Replicate)) +
  geom_line() + 
  scale_x_continuous(breaks = 1:9/10) + 
  labs(title = "checkpoint-best")
df <- yolo_ss %>% 
  filter(Manual_Brant > 0 & count_brant < Manual_Brant) %>%
  mutate(Replicate = factor(Date), Missed_Brant = Manual_Brant - count_brant) %>%
  group_by(threshold_brant, Replicate) %>%
  summarise(YOLO_Brant = sum(count_brant), Manual_Brant = sum(Manual_Brant),
            Missed_Brant = sum(Missed_Brant),
            n = n())
p3 <- ggplot(data = df, aes(x=threshold_brant, y = Missed_Brant, col=Replicate)) +
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = 1:9/10) + 
  scale_y_continuous(trans = "log10") + 
  labs(title = "ss-original")
mtot <- yolo_best2 %>% 
  #filter(! is.na(Manual_Brant)) %>%
  mutate(Replicate = factor(Date)) %>%
  group_by(threshold_brant, Replicate) %>%
  summarise(n_Manual = n())
df <- left_join(mtot, df) %>%
  replace_na(list(Manual_Brant = 0, n = 0)) %>%
  mutate(pFalseNegatives = n/n_Manual)
#plot proportion of photos that are false negatives
pp3 <- ggplot(data = df, aes(x=threshold_brant, y = pFalseNegatives, col=Replicate)) +
  geom_line() + 
  scale_x_continuous(breaks = 1:9/10) + 
  labs(title = "ss-original")
ggarrange(p1, p2, p3, common.legend = TRUE, ncol = 3, nrow = 1)
ggarrange(pp1, pp2, pp3, common.legend = TRUE, ncol = 3, nrow = 1)
