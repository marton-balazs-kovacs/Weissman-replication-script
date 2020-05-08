rm(list = ls())
######## CONFLICT TASK DATA ########
#### Pre-process data ####

library(testthat)
library(tidyverse)
#Load the merged data files
df <- read_delim("replication-issue-rewrite/simon_data.tsv", delim = "\t")

names(df)[names(df) == "task"] <- "taskname" #We can ignore this variable, the only reason it's here
#is because I'd normally load multiple tasks within this script

#SIMON
#Create a variable that codes which task the data is coming from 
df <- df[df$isPractice == 0, ] #Remove practice trials
df$task <- rep(0, length(df$taskname))



#Remove the second response of duplicate participants
participant_duplicate <- df %>% 
  select(participant_id, id, trialId, consentTime) %>%
  group_by(participant_id, id) %>% 
  count() %>%
  group_by(participant_id) %>% 
  mutate(N = n()) %>% 
  filter(N != 1)

participant_duplicate_drop <-
  participant_duplicate %>%
  filter(id %in% c(82, 77, 57, 8, 84)) %>% 
  select(participant_id, id)

df <- df %>% anti_join(., participant_duplicate_drop, by = c("participant_id", "id")) %>%
  mutate(RT = case_when(responseContent != "TIMEOUT" ~ responseTime - stimOnset,
                        responseContent == "TIMEOUT" ~ NA_real_))

#Remove outlier participants
#Participant level outlier screening here is based on ALL RTS OF A PARTICIPANT,
#except for timeouts
ppt_to_drop <- df %>%
  group_by(participant_id) %>% 
  summarise(rtParticipantMean = mean(RT, na.rm = T), 
            rtParticipantSd = sd(RT, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(rtGrandMean = mean(rtParticipantMean, na.rm = T),
         rtGrandSd = sd(rtParticipantMean, na.rm = T))

df <- left_join(df, ppt_to_drop, by = "participant_id") %>% 
  group_by(participant_id) %>% 
  mutate(dropParticipantRt = case_when(rtGrandMean + 2.5 * rtGrandSd < rtParticipantMean ~ 1L,
                                       rtGrandMean - 2.5 * rtGrandSd > rtParticipantMean ~ 1L,
                                       TRUE ~ 0L)) %>%
  mutate(Acc = case_when(responseTarget == responseContent ~ 1,
                         responseTarget != responseContent ~ 0)) %>% 
  group_by(participant_id, id) %>% 
  mutate(prop = sum(Acc) / n() * 100,
         dropParticipantAcc = case_when(prop < 70 ~ 1L,
                                        TRUE ~ 0L))%>% 
  filter(dropParticipantAcc == 0L,
         dropParticipantRt  == 0L) %>% 
  mutate(ageDrop = case_when(age < 18 ~ 1L,
                             age > 99 ~ 1L,
                             TRUE ~ 0L)) %>%
  filter(ageDrop != 1L) %>% 
  ungroup()

#This next step should be completed after deleting practice trials AND
#after removing participants
sblocks <- length(unique(df$block))          #should be 4
expect_equal(sblocks, 4)
stnum <- length(unique(df$trialId))          #should be 324
expect_equal(stnum, 324)
stperb <- stnum / sblocks                       #should be 81
expect_equal(stperb, 81)
ssubnum <- length(unique(df$participant_id)) #should be 118
expect_equal(ssubnum, 118)

#simon$trial <- rep(rep(c(1:stperb), sblocks),ssubnum)
#the column trial now counts every trial WITHIN a block, so trial == 1
#would be the first trial of each BLOCK for a person

#AN ALTERNATIVE WAY TO CALCULATE WITHIN-BLOCK TRIAL NUMBER:
df$trial <- (df$trialId-23) - (df$block-1)*81 

# Check this matches with Matt's non-hardcoded version
expect_equal(
  df$trial,
  df %>% 
    nest(d = c(-id, -block)) %>%
    mutate(d = map(d, ~ arrange(., trialId) %>% rowid_to_column('n'))) %>%
    unnest(cols = c(d)) %>%
    .$n)

#Some renaming and recoding so that the new variables are
#in line with the old code I had
df$subid <- df$participant_id
df$cong <- 1-df$isCongruent
df$resp <- df$responseContent
df$trialid <- df$trialId - 23
#IMPORTANT: trialid codes trial number WITHIN a participant, but
#ACROSS blocks, so trialid == 1 is the first RT of a PERSON

merged <- df #I know this assignment seems pointless, this is because originally
#this code was set up to loop over different tasks

#Preprocess data
by_block <- 0 #set to 1 to switch to filtering by block


if (by_block == 1) {
 merged$trialid <- merged$trial 
}

# First create previous trial congruency and accuracy
#NOTE - IMPORTANT: I am doing this the way I think it's done
#in Weissman-replication-raw-processed - however, I am not
#convinced that is the correct way - right now it's only ex-
#cluding the first trial of each participant (i.e., their very
#first trial), but leaves the first trial of each subsequent 
#block. This might be suboptimal since the first trial of a 
#block doesn't really have a previous congruency.

#I will make a note of every occurrence of the variable
#that codes which trials are excluded: the first of each
#subject or the first of each block (trialid vs. trial).

#Create previous congruency and previous accuracy
merged$precong <- rep(NA, length(merged$subid))
merged$preacc <- rep(NA, length(merged$subid))

for (d in 1:length(merged$subid)) {
  if (merged$trialid[d] < 2) { 
    merged$precong[d] <- NA
    merged$preacc[d] <- NA
  } else {
    merged$precong[d] <- merged$cong[d - 1]
    merged$preacc[d] <- merged$Acc[d - 1]
  }
}


# Exclude the first trial
#SAME NOTE AS ABOVE
merged <- merged %>% mutate(trim = ifelse(trialid>1,RT,NA_real_)) 

# Calculations for trial level exclusion
#Conditional mean RTs are calculated after removing the first trial
condinfo <- merged %>% 
  filter(is.na(precong) == F) %>% 
  group_by(subid, cong, precong) %>% 
  summarize(condm = mean(trim, na.rm = T),
            condsd = sd(trim, na.rm = T))

merged <- left_join(merged, condinfo, by = c("subid", "cong", "precong"))

# RT outliers
merged <- merged %>% 
  filter(trialid>1) %>%  #this should be (trial > 1)
  mutate(zscore = (trim-condm)/condsd) %>% 
  mutate(outlier = case_when(zscore < 2.5 & zscore > -2.5 ~ 0,
                             TRUE ~ 1))

trial_num_check1 <- merged %>% 
  group_by(cong,precong) %>% 
  summarize(N = n())
#Trial numbers after excluding the first trial
#if by block, numbers should be (ssubnum * (stperb-1) * sblocks)/4
#if not by block, numbers should be ((ssubnum * stperb * sblocks)-ssubnum)/4

# Post-outlier outliers
merged$post_outlier <- rep(NA, length(merged$subid))
for (b in 1:length(merged$subid)) {
  if (merged$trialid[b] < 2) { 
    merged$post_outlier[b] <- NA
  } else {
    if (merged$trialid[b] == 2) {merged$post_outlier[b] <- 1} 
    #the reason why this additional line is necessary is that all first trials have been removed BEFORE
    #the calculation of z-scores, so we won't know if those are outliers or not
    #NOTE: we can only get to 33 239 if we set merged$post_outlier[b] to 1, i.e., if we treat it as an
    #outlier, always - in other words, if we always remove a participant's second trial too
    else {merged$post_outlier[b] <- merged$outlier[b - 1]}
  }
}


#### RT analyses ####
taskdat <-  subset(merged, outlier == 0 & post_outlier == 0 & preacc == 1 & Acc == 1 & is.na(trim) == FALSE)

#Trial number checks
trial_num_check2 <- taskdat %>% 
  group_by(cong,precong) %>% 
  summarize(N = n())


anova_data <- taskdat %>%
  group_by(subid, cong, precong) %>%
  summarize(mean_rt = mean(trim, na.rm = TRUE))

anova_data$subid <- as.factor(anova_data$subid)
anova_data$cong <- as.factor(anova_data$cong)
anova_data$precong <- as.factor(anova_data$precong)

rtanova <- aov(mean_rt ~ cong*precong + Error(subid/(cong*precong)), data=anova_data)
summary(rtanova)

#### Plot ####
library(ggplot2)
th <- theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            strip.background =element_rect(fill="white", colour = "black"),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            axis.text = element_text(color = "black", size = 8),
            legend.position = "right",
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key = element_rect(colour = "transparent", fill = "white"),
            text = element_text(face = "bold", size = 12))


interim_data <- taskdat %>%
  mutate(prevcong = case_when(precong == 1L ~ "Incongruent",
                              precong == 0L ~ "Congruent"),
         congruent = case_when(cong == 1L ~ "Incongruent",
                               cong == 0L ~ "Congruent")) %>%
  group_by(subid, congruent, prevcong) %>%
  summarize(mean_rt = mean(trim, na.rm = TRUE))

CSE_plot_data <- interim_data %>%
  group_by(congruent, prevcong) %>%
  summarize(N = n(),
            mean = mean(mean_rt, na.rm = TRUE),
            sd = sd(mean_rt, na.rm = TRUE),
            se = sd / sqrt(N))

#tiff('simon_reanal.tiff', units="in", width=8, height=6, res = 300, compression = 'lzw') #this line and dev.off() are for saving the figure
ggplot(CSE_plot_data, aes(x =prevcong, y = mean,
                          group = congruent, shape = congruent)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  geom_line() +
  geom_point(size = 2) +
  scale_shape_manual(values=c(4, 16)) +
  xlab("Congruency of Trial N-1")+
  ylab("Reaction time (ms)") +
  ylim(500, 1000) +
  guides(shape = guide_legend(title="Congruency of \n Trial N")) +
  th +
  theme(legend.title.align=0.5) +
  theme(legend.position = "right") 
#dev.off()


#### Accuracy analyses ####
taskdat <-  subset(merged, outlier == 0 & post_outlier == 0)

anova_data <- taskdat %>%
  group_by(subid, cong, precong) %>%
  summarize(mean_acc = mean(Acc, na.rm = TRUE))

anova_data$subid <- as.factor(anova_data$subid)
anova_data$cong <- as.factor(anova_data$cong)
anova_data$precong <- as.factor(anova_data$precong)

accanova <- aov(mean_acc ~ cong*precong + Error(subid/(cong*precong)), data=anova_data)
summary(accanova)

#### Plot ####
library(ggplot2)
th <- theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            strip.background =element_rect(fill="white", colour = "black"),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            axis.text = element_text(color = "black", size = 8),
            legend.position = "right",
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key = element_rect(colour = "transparent", fill = "white"),
            text = element_text(face = "bold", size = 12))


interim_data <- taskdat %>%
  mutate(prevcong = case_when(precong == 1L ~ "Incongruent",
                              precong == 0L ~ "Congruent"),
         congruent = case_when(cong == 1L ~ "Incongruent",
                               cong == 0L ~ "Congruent")) %>%
  group_by(subid, congruent, prevcong) %>%
  summarize(mean_acc = mean(Acc, na.rm = TRUE))

CSE_plot_data <- interim_data %>%
  group_by(congruent, prevcong) %>%
  summarize(N = n(),
            mean = mean(mean_acc, na.rm = TRUE),
            sd = sd(mean_acc, na.rm = TRUE),
            se = sd / sqrt(N))

#tiff('simon_reanal.tiff', units="in", width=8, height=6, res = 300, compression = 'lzw') #this line and dev.off() are for saving the figure
ggplot(CSE_plot_data, aes(x =prevcong, y = mean,
                          group = congruent, shape = congruent)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  geom_line() +
  geom_point(size = 2) +
  scale_shape_manual(values=c(4, 16)) +
  xlab("Congruency of Trial N-1")+
  ylab("Proportion correct") +
  ylim(.95, 1) +
  guides(shape = guide_legend(title="Congruency of \n Trial N")) +
  th +
  theme(legend.title.align=0.5) +
  theme(legend.position = "right") 
#dev.off()