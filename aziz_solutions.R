library(tidyverse)
library(lme4)
library(lmerTest)
library(afex)

# colors for the graph ----
cbbPalette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To do  ----
# read in data ----
myPath = paste(getwd(), "/p_dat/", sep = "")
filenames = list.files(myPath)

all_dat = lapply(paste(myPath, filenames, sep = ""), read.csv)
all_dat = do.call(plyr::rbind.fill, all_dat)

#----
myPath = paste(getwd(), "/pilot_20_01/", sep = "")
filenames = list.files(myPath)

all_dat2 = lapply(paste(myPath, filenames, sep = ""), read.csv)
all_dat2 = do.call(plyr::rbind.fill, all_dat2)

all_dat = rbind(all_dat, all_dat2)

all_dat %>%
  summarize(n= n_distinct(pid))

all_dat
str(all_dat)
summary(all_dat)

# here we look at which trials are valid
all_dat %>%
  group_by(pid) %>%
  summarize(bad_trials = sum(VALIDITY=="False"),
            good_trials = sum(VALIDITY == "True")) %>% 
  print(n = Inf)

# here we make some new variables of interest
all_dat2 = all_dat %>%
  mutate(
    agent = ifelse(ï..Holder == 'right', 'clamp', ifelse(
      ï..Holder == "left", 'iCub', NA)),
    ssd = ifelse(SOUND_DELAY_MS_LEFT == -1, NA,
                 SOUND_DELAY_MS_LEFT)) %>%
  mutate(avg_ssd = mean(ssd, na.rm = T))

# here we look at which trials are correct
clean_dat = all_dat2 %>%
  group_by(pid, Action, agent) %>%
  summarize(error_rate = sum(CORRECT == 1)/length(CORRECT),
            rt = mean(RELEASE_TIME),
            touch_rt = mean(TOUCH_TIME))

clean_dat %>%
  ggplot(aes(y = rt, agent, color = Action))+
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(.2), size = 3)+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(.2))+
  theme_bw()

clean_dat %>%
  ggplot(aes(y = touch_rt, agent, color = Action))+
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(.2), size = 3)+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(.2))+
  theme_bw()

clean_dat %>%
  ggplot(aes(y = error_rate, agent))+
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(.2), size = 3)+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(.2))+
  theme_bw()+
  facet_wrap(~Action, scales = "free_y")

# SRT ----
#   Performance on GO trials was examined via a two-tailed t test on mean RTs. 
#   The behavioral performance of the SST was quantified as the stop-signal reaction time 
#   (SSRT (Congdon et al., 2012; Logan and Cowan, 1984). The SSRTs were computed for each 
#   condition using the integration method (Logan and Cowan, 1984; Verbruggen and Logan, 2009), 
#   known as being more reliable than the alternative mean method (Verbruggen et al., 2013). 
#   
#   First, we ranked the RT to reach the bottle in GO trials and selected the Nth RT (representative RT), 
#   where N was calculated by multiplying the number of GO trials by the probability of mistakes in STOP 
#   trials. We then estimated SSRT by subtracting the average SSD from the representative RT. Accuracy, 
#   expressed as percent of correct inhibitions (%CIST) in the SST for each conditions, ranged between 0.4 
#   and 0.6 (Hilt and Cardellicchio, 2018; Verbruggen et al., 2008) and is only used to evaluate 
#   efficacy of the staircase procedure. No subjects had SSD staircases that continued to increase
#   or decrease over the whole experiment. To assess the efficacy of the stop signal delay staircasing 
#   algorithm across
#   blocks, the accuracy in the SST was quantified block by block. These values were then tested against 
#   0.5 using a Wilcoxon signed rank test, showing that all subjects fulfilled the criteria and were thus 
#   all were included in the analyses. Normality was evaluated via the Kolmogorov–Smirnov test. Two-tailed 
#   t test followed by Bonferroni corrections were performed to evaluate differences (alpha level p < 0.05)
#   between the two conditions (JA vs. no-JA). Statistical analyses were conducted using STATISTICA 9 
#   (StatSoft, Inc.).

# mean method = mean rt - SSD ----
mean_mthd = all_dat2 %>%
  filter(VALIDITY == "True") %>%
  group_by(pid, agent, Action) %>%
  summarize(
    mean_rt = mean(TOUCH_TIME),
    avg_ssd = mean(ssd, na.rm = T)
  ) %>%
  mutate(
    avg_ssd2 = avg_ssd[Action=="stop"]
  ) %>%
  mutate(
    ssrt = mean_rt - avg_ssd2
  ) %>%
  filter(Action=="go")

mean_mthd %>%
  ggplot(aes(y = ssrt, x = agent))+
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(.2), size = 3)+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(.2))+
  theme_bw()+
  ggtitle("mean method")


# integration method = more complex. For now, this is incorrect. We need to do more reading. 
# integration method for avg SSD 
# ssrt_dat = all_dat2 %>%
#   filter(VALIDITY == "True") %>%
#   group_by(
#     pid, agent, Action
#   ) %>%
#   summarize(
#     mean_err = sum(CORRECT == 0)/length(CORRECT),
#     n_trial_pre = length(sort(TOUCH_TIME)),
#     avg_ssd = mean(ssd, na.rm = T)
#   ) %>%
#   mutate(
#     prob_err = mean_err[Action=="stop"],
#     avg_ssd2 = avg_ssd[Action=="stop"]
#   ) %>%
#   mutate(
#     ssrt_pre = (n_trial_pre*prob_err)) %>%
#   mutate(
#     ssrt = ssrt_pre-avg_ssd2
#   ) %>%
#   filter(Action == "stop")

# integration method for each SSD. 
ssrt_dat = all_dat2 %>%
  filter(VALIDITY == "True") %>%
  group_by(
    pid, agent, Action, ssd
  ) %>%
  summarize(
    prob_err = sum(CORRECT == 0)/length(CORRECT),
    n_trial_pre = length(sort(TOUCH_TIME[Action=="go"])),
  ) %>%
  mutate(n_trial_pre1 = ifelse(n_trial_pre == 0, NA, n_trial_pre)) %>%
  ungroup(
    pid, agent, Action, ssd
  ) %>%
  fill(n_trial_pre1, .direction = "down") %>%
  mutate(srt = prob_err*n_trial_pre1) %>%
  mutate(ssrt = srt-mean(ssd, na.rm = T)) %>%
  filter(Action == "stop")

ssrt_dat %>%
  ggplot(aes(y = ssrt, x = agent))+
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(.2), size = 3)+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(.2), width = .1)+
  theme_bw()

ssrt_dat %>%
  ggplot(aes(y = ssrt, x = agent))+
  geom_point(alpha = .08)+
  geom_boxplot(position = position_nudge(.2), width = .1)+
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(.2), size = 3)+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(.2), width = .1)+
  theme_bw()

ssrt_dat %>%
  ggplot(aes(y = ssrt, x = agent, color = pid))+
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(.5), size = 3)+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(.5), width = .1)+
  theme_bw()

# integration try 2:
ssrt_dat_a = all_dat2 %>%
  filter(VALIDITY == "True",
         Action == "stop") %>%
  group_by(
    pid, agent, ssd
  ) %>%
  summarize(
    prob_err = sum(CORRECT == 0)/length(CORRECT),
    #avg_ssd = mean(ssd)
  ) 

ssrt_dat_b = all_dat2 %>%
  filter(VALIDITY == "True",
         Action == "go") %>%
  group_by(
    pid, agent
  ) %>%
  summarize(
    n_trial_pre = length(sort(TOUCH_TIME))
            ) %>%
  left_join(ssrt_dat_a, by = c("pid", "agent")) %>%
  mutate(n_trial_pre1 = ifelse(n_trial_pre == 0, NA, n_trial_pre)) %>%
  mutate(ssrt = (prob_err*n_trial_pre1)-mean(ssd))
  
ssrt_dat_b %>%
  filter(!pid == "pp_3") %>%
  ggplot(aes(y = ssrt, x = agent))+
  geom_jitter(aes(color = pid), width = .2)+
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(.5), size = 3)+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(.5), width = .1)+
  theme_bw()

ssrt_dat_b %>%
  filter(!pid == "pp_3") %>%
  ggplot(aes(y = ssrt, x = agent))+
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(.5), size = 3)+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(.5), width = .1)+
  theme_bw()
