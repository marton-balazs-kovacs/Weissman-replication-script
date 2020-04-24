## Analysis of trial types and congruency by position in block
source('replication-issue-rewrite/simon_flanker_stroop_recalculation.R')

# simon variable holds fairly raw data
tt <- simon %>%
  mutate(
    # make the flags clear by assigning to individual variables
    flagPreCong = bitwAnd(typeCode, 8) == 8,
    flagCurCong = bitwAnd(typeCode, 4) == 4,
    flagStimSet = bitwAnd(typeCode, 2) == 2,
    flagStimCor = bitwAnd(typeCode, 1) == 1
  ) %>%
  nest(data = -id) %>%
  mutate(
    data = map(data, ~ nest(., d = -block) %>%
                 mutate(d = map(d, ~ arrange(., trialId) %>%
                                  mutate(number = 1:n()))) %>%
                 unnest(cols = c(d)))
  ) %>% 
  unnest(cols = c(data))
  
# Visual check that we have the right number of trials with each number in each block
tt %>% 
  ggplot(aes(x = number)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~block)

# Check the average for each of the flags by trial number and block
flags <- tt %>% 
  select(id, number, block, starts_with("flag")) %>%
  pivot_longer(starts_with("flag"), names_to = "flag")

ggplot(flags, aes(x = number, y = value)) +
  geom_point(alpha = .05) + 
  facet_grid(flag~block)

# Kinda hard to see anything on the above, so let's instead only look at the first and last 5 trials in a block
first_and_last <- c(1:5, (max(tt$number)-5):max(tt$number))
flags %>% filter(number %in% first_and_last) %>%
  ggplot(aes(x = factor(number), y = value)) + 
  geom_point(alpha = .05, position = position_jitter(.1, .1)) +
  facet_grid(flag ~ block)
