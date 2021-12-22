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
  ) 

mean_mthd2 = mean_mthd %>%
  mutate(
    avg_ssd2 = avg_ssd[Action=="stop"]
  ) %>%
  mutate(
    ssrt = mean_rt - avg_ssd2
  ) %>%
  filter(Action=="go")

mean_mthd2 %>%
  ggplot(aes(y = ssrt, x = agent))+
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(.2), size = 3)+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(.2))+
  theme_bw()


# integration method = more complex. For now, this is incorrect. We need to do more reading. 
ssrt_dat = all_dat2 %>%
  filter(VALIDITY == "True") %>%
  group_by(pid, agent, Action) %>%
  summarize(
    mean_err = mean(CORRECT),
    n_trial_pre = length(sort(TOUCH_TIME)),
    avg_ssd = mean(ssd, na.rm = T)
  )
ssrt_dat2 = ssrt_dat %>%
  mutate(
    prob_err = mean_err[Action=="stop"],
    avg_ssd2 = avg_ssd[Action=="stop"]
    ) %>%
  mutate(
    ssrt = avg_ssd-(n_trial_pre*prob_err)
  ) %>%
  filter(Action == "stop")

ssrt_dat2 %>%
  filter(ssrt<1000) %>%
  ggplot(aes(y = ssrt, x = agent))+
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(.2), size = 3)+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(.2))+
  theme_bw()

ssrt_dat2 %>%
  filter(ssrt<1000) %>%
  ggplot(aes(y = ssrt, x = agent, color = pid))+
  geom_jitter(width = .1)+
  theme_bw()
