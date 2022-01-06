library(tidyverse)
library(lme4)
library(lmerTest)
library(afex)
library(PupillometryR)

# colors for the graph ----
cbbPalette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To do  ----
# read in data ----
myPath = paste(getwd(), "/exp_data/", sep = "")
filenames = list.files(myPath)

all_dat = lapply(paste(myPath, filenames, sep = ""), read.csv)
all_dat = do.call(plyr::rbind.fill, all_dat)
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
# mean method = mean rt - SSD ----

all_dat2 = all_dat %>%
  mutate(
    agent = ifelse(ï..Holder == 'right', 'clamp', ifelse(
      ï..Holder == "left", 'iCub', NA)),
    ssd = ifelse(SOUND_DELAY_MS_LEFT == -1, NA,
                 SOUND_DELAY_MS_LEFT))


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


# integration method = more complex  ----
# integration method for avg SSD . This is so far the best method

# integration method

ssrt_dat_a = all_dat %>%
  mutate(
    agent = ifelse(ï..Holder == 'right', 'clamp', ifelse(
      ï..Holder == "left", 'iCub', NA)),
    ssd_new = ifelse(agent == "iCub", SOUND_DELAY_MS_LEFT,
                     ifelse(agent == "clamp", SOUND_DELAY_MS_RIGHT, NA))
    ) %>%
  filter(
    VALIDITY == "True",
         Action == "stop"
    ) %>%
  group_by(
    pid, agent
  ) %>%
  summarize(
    prob_err = sum(CORRECT == 0)/length(CORRECT),
    avg_ssd = mean(ssd_new)
  )

ssrt_dat_b = all_dat2 %>%
  filter(VALIDITY == "True",
         Action == "go") %>%
  group_by(
    pid, agent
  ) %>%
  select(pid, agent, TOUCH_TIME) %>%
  left_join(
    ssrt_dat_a, by = c("pid", "agent")
    ) %>%
  #nest() %>%
  summarize(
    nth_trial = mean(quantile(TOUCH_TIME, 
                        probs = prob_err, type = 6)),
    avg_ssd = mean(avg_ssd)
    ) %>%
  mutate(ssrt = nth_trial-avg_ssd)
  
ssrt_dat_b$pid[which(ssrt_dat_b$ssrt>600)]

ssrt_dat_b %>%
  filter(!pid %in% bad_pees) %>%
  ggplot(aes(y = ssrt, x = agent))+
  geom_jitter(aes(color = pid), width = .2)+
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(.5), size = 3)+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(.5), width = .1)+
  theme_bw()

bad_pees = c("3", "pp_7", "pp_16")
ssrt_dat_b %>%
  filter(!pid %in% bad_pees) %>%
  ggplot(aes(y = ssrt, x = agent, color = agent))+
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(.5), size = 3)+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(.5), width = .1)+
  theme_bw()

ssrt_dat_ana = ssrt_dat_b %>% filter(!pid %in% bad_pees)
t.test(ssrt_dat_ana$ssrt[ssrt_dat_ana$agent == "iCub"],
       ssrt_dat_ana$ssrt[ssrt_dat_ana$agent == "clamp"], paired = T)

ssrt_dat_b %>%
  filter(!pid %in% bad_pees) %>%
  ggplot(aes(y = ssrt, x = agent, color = agent, fill = agent))+
  geom_flat_violin(position = position_nudge(.12), alpha = .4, lwd = .5, color = "black")+theme_bw()+
  geom_point(position = position_jitter(width = .1), alpha = .4)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))

ssrt_dat_b %>%
  ggplot(aes(y = ssrt, x = agent))+
  geom_point(position = position_jitter(width = .1), alpha = .4)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .05, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "bar", width = .1,
               position = position_dodge(.5), alpha = .5)+
  theme_bw()

ssrt_dat_b %>%
  ggplot(aes(y = ssrt, x = agent))+
  geom_point(position = sdamr::position_jitternudge(jitter.width = .1, nudge.x = .2), 
             alpha = .4)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .05, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5)+
  theme_bw()

ssrt_dat_b %>%
  ggplot(aes(y = ssrt, x = agent))+
  geom_point(alpha = .2)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .05, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5)+
  theme_bw()+
  geom_line(aes(group = pid))

range(ssrt_dat_b$ssrt)
mean(ssrt_dat_b$ssrt)

# lets do the same but calculate a single p(respond|signal) for both agents
ssrt_dat_a2 = all_dat %>%
  mutate(
    agent = ifelse(ï..Holder == 'right', 'clamp', ifelse(
      ï..Holder == "left", 'iCub', NA)),
    ssd_new = ifelse(agent == "iCub", SOUND_DELAY_MS_LEFT,
                     ifelse(agent == "clamp", SOUND_DELAY_MS_RIGHT, NA))
  ) %>%
  filter(
    VALIDITY == "True",
    Action == "stop"
  ) %>%
  group_by(
    pid
  ) %>%
  summarize(
    prob_err = sum(CORRECT == 0)/length(CORRECT),
  )

ssrt_dat_b2 = all_dat %>%
  mutate(
    agent = ifelse(ï..Holder == 'right', 'clamp', ifelse(
      ï..Holder == "left", 'iCub', NA)),
    ssd_new = ifelse(agent == "iCub", SOUND_DELAY_MS_LEFT,
                     ifelse(agent == "clamp", SOUND_DELAY_MS_RIGHT, NA))
  ) %>%
  filter(
    VALIDITY == "True",
    Action == "stop"
  ) %>%
  group_by(
    pid, agent
  ) %>%
  summarize(
    avg_ssd = mean(ssd_new)
  ) %>%
  left_join(
    ssrt_dat_a2
  )

ssrt_dat_c2 = all_dat2 %>%
  filter(VALIDITY == "True",
         Action == "go") %>%
  group_by(
    pid, agent
  ) %>%
  select(pid, agent, TOUCH_TIME) %>%
  left_join(
    ssrt_dat_b2, by = c("pid", "agent")
  ) %>%
  #nest() %>%
  summarize(
    nth_trial = mean(quantile(TOUCH_TIME, 
                              probs = prob_err, type = 6)),
    avg_ssd = mean(avg_ssd)
  ) %>%
  mutate(ssrt = nth_trial-avg_ssd)

ssrt_dat_c2 %>%
  ggplot(aes(y = ssrt, x = agent))+
  geom_point(position = sdamr::position_jitternudge(jitter.width = .1, nudge.x = .2), 
             alpha = .4)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .05, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5)+
  theme_bw()

ssrt_dat_c2 %>%
  ggplot(aes(y = ssrt, x = agent))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .05, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5)+
  theme_bw()

t.test(ssrt_dat_c2$ssrt[ssrt_dat_c2$agent == "iCub"],
       ssrt_dat_c2$ssrt[ssrt_dat_c2$agent == "clamp"], paired = T)
