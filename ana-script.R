library(tidyverse)
library(lme4)
library(lmerTest)
library(afex)

# colors for the graph ----
cbbPalette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To do  ----
# read in data ----
myPath = paste(getwd(), "/data/", sep = "")
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
  summarize(no_touch_onGo = sum(TOUCH_TIME == -1 &
                             #is.na(SOUND_DELAY_MS_LEFT) == F &
                             Action == "go"),
            not_sure = sum(RELEASE_TIME == -1), # <---- we don't know what this is. 
            early_release = sum(is.na(SOUND_DELAY_MS_LEFT)),
            touch_wrong_bottle = sum(BOTTLE_RESPONSE_POSITION != -1 &
                                       BOTTLE_RESPONSE_POSITION != ï..Holder)) %>% 
  print(n = Inf)


# here we look at which trials are correct
clean_dat = all_dat %>%
    # filter(TOUCH_TIME != -1 | Action == "go",
    #        RELEASE_TIME != -1,
    #        is.na(SOUND_DELAY_MS_LEFT) == F,
    #        BOTTLE_RESPONSE_POSITION == -1 | BOTTLE_RESPONSE_POSITION == ï..Holder
    #        ) %>%
  mutate(agent = ifelse(ï..Holder == 'right', 'clamp', ifelse(
    ï..Holder == "left", 'iCub', NA)),
    corr_new = ifelse(Action == "stop" | TOUCH_TIME > 0, "incor", 
                      ifelse(Action == "stop" |
                      TOUCH_TIME == -1, "cor", NA))) %>%
  group_by(pid, Action, agent) %>%
  summarize(error_rate = sum(corr_new == "cor")/length(corr_new),
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

fhuftyuiu
