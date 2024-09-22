
### Load packages ####

pacman::p_load(tidyverse, data.table, irr, cowplot)

### Load data ####
load(file = "data/question_asking/combined_session_talk_question_all_long_withtreatment.RData")

# introduce new column to all_data
all_data_treatment <- all_data_treatment %>% mutate(proportion_audience_women = audience_women/audience_total,
                                                    proportion_audience_men = audience_men/audience_total, 
                                                    hands_women = as.numeric(hands_total) - as.numeric(hands_men),
                                                    talk_id = paste0(session_id, "_", talk_nr),
                                                    question_id = paste0(session_id, "_", talk_nr, "_", question_nr))


# select only relevant data
session <- all_data_treatment %>% select(c(session_id, observer_talk, host_1_gender, host_1_age)) %>% unique()
talk <- all_data_treatment %>% select(c(session_id, talk_nr, talk_id, observer_talk, speaker_gender_infer, speaker_age, 
                                        audience_total, audience_men, duration_qa)) %>% unique()
question <- all_data_treatment %>% select(session_id, talk_nr, talk_id, question_id, observer_talk, question_nr, room_size, hands_total,
                                          hands_men, questioner_age, questioner_gender) %>% unique()


### Calculate ICC ####

## continuous variables: ICC
## factors: Fleiss' kappa statistic = between -1 and 1, threshold is tricky but > 0.41 in medicine as 'good'
## adjustment to Cohen's kappa, as not all raters are the same and there are more than two
## values ≤ 0 as indicating no agreement and 0.01–0.20 as none to slight, 0.21–0.40 as fair, 
## 0.41– 0.60 as moderate, 0.61–0.80 as substantial, and 0.81–1.00 as almost perfect agreement

##### Session level ####
# errors occur for those sessions where the moderating host (host 1) swapped in the session, so we'd have a M and a F
# hosting the session which is 'not possible'
# exclude those sessions
session <- subset(session, session_id != 36 & session_id != 94 & session_id != 44) # exclude changing of hosts

###### Host gender ####
host_gender <- subset(session, !is.na(host_1_gender)) # exclude missing data

wide_host_gender <- spread(host_gender[,c("session_id", "observer_talk", "host_1_gender")], 
                           observer_talk, host_1_gender)

# treat observer 1 and 2 as one unit, and observer 3 and 4 as one unit
wide_host_gender_a <- wide_host_gender[,c(1:3)]
wide_host_gender_b <- wide_host_gender[,c(1,4,5)]
names(wide_host_gender_a) <- c("session_id", "observer_a", "observer_b")
names(wide_host_gender_b) <- c("session_id", "observer_a", "observer_b")

# merge
wide_host_gender <- rbind(wide_host_gender_a, wide_host_gender_b)

# exclude sessions without double sampling
wide_host_gender <- subset(wide_host_gender, !is.na(observer_a) & !is.na(observer_b)) 

kappa_host_gender <- kappa2(wide_host_gender[,2:3], weight = "unweighted")

###### Host age ####
host_age <- subset(session, !is.na(host_1_age)) # exclude missing data

wide_host_age <- spread(host_age[,c("session_id", "observer_talk", "host_1_age")], 
                        observer_talk, host_1_age)

# treat observer 1 and 2 as one unit, and observer 3 and 4 as one unit
wide_host_age_a <- wide_host_age[,c(1:3)]
wide_host_age_b <- wide_host_age[,c(1,4,5)]
names(wide_host_age_a) <- c("session_id", "observer_a", "observer_b")
names(wide_host_age_b) <- c("session_id", "observer_a", "observer_b")

# merge
wide_host_age <- rbind(wide_host_age_a, wide_host_age_b)

# exclude sessions without double sampling
wide_host_age <- subset(wide_host_age, !is.na(observer_a) & !is.na(observer_b)) 

kappa_host_age <- kappa2(wide_host_age[,2:3], weight = "unweighted")

##### Talk level ####

###### Speaker gender ####
speaker_gender <- subset(talk, !is.na(speaker_gender_infer)) # exclude missing data

wide_speaker_gender <- spread(speaker_gender[,c("talk_id", "observer_talk", "speaker_gender_infer")], 
                              observer_talk, speaker_gender_infer)

# treat observer 1 and 2 as one unit, and observer 3 and 4 as one unit
wide_speaker_gender_a <- wide_speaker_gender[,c(1:3)]
wide_speaker_gender_b <- wide_speaker_gender[,c(1,4,5)]
names(wide_speaker_gender_a) <- c("talk_id", "observer_a", "observer_b")
names(wide_speaker_gender_b) <- c("talk_id", "observer_a", "observer_b")

# merge
wide_speaker_gender <- rbind(wide_speaker_gender_a, wide_speaker_gender_b)

# exclude sessions without double sampling
wide_speaker_gender <- subset(wide_speaker_gender, !is.na(observer_a) & !is.na(observer_b)) 

kappa_speaker_gender <- kappa2(wide_speaker_gender[,2:3], weight = "unweighted")

###### Speaker age ####
speaker_age <- subset(talk, !is.na(speaker_age)) # exclude missing data

wide_speaker_age <- spread(speaker_age[,c("talk_id", "observer_talk", "speaker_age")], 
                           observer_talk, speaker_age)

# treat observer 1 and 2 as one unit, and observer 3 and 4 as one unit
wide_speaker_age_a <- wide_speaker_age[,c(1:3)]
wide_speaker_age_b <- wide_speaker_age[,c(1,4,5)]
names(wide_speaker_age_a) <- c("talk_id", "observer_a", "observer_b")
names(wide_speaker_age_b) <- c("talk_id", "observer_a", "observer_b")

# merge
wide_speaker_age <- rbind(wide_speaker_age_a, wide_speaker_age_b)

# exclude sessions without double sampling
wide_speaker_age <- subset(wide_speaker_age, !is.na(observer_a) & !is.na(observer_b)) 

kappa_speaker_age <- kappa2(wide_speaker_age[,2:3], weight = "unweighted")

#### Audience total
audience_total <- subset(talk, !is.na(audience_total)) # exclude missing data

wide_audience_total <- spread(audience_total[,c("talk_id", "observer_talk", "audience_total")], 
                              observer_talk, audience_total)

# treat observer 1 and 2 as one unit, and observer 3 and 4 as one unit
wide_audience_total_a <- wide_audience_total[,c(1:3)]
wide_audience_total_b <- wide_audience_total[,c(1,4,5)]
names(wide_audience_total_a) <- c("talk_id", "observer_a", "observer_b")
names(wide_audience_total_b) <- c("talk_id", "observer_a", "observer_b")

# merge
wide_audience_total <- rbind(wide_audience_total_a, wide_audience_total_b)

# exclude sessions without double sampling
wide_audience_total <- subset(wide_audience_total, !is.na(observer_a) & !is.na(observer_b)) 

icc_audience_total <- icc(wide_audience_total[,2:3],
                          model = "twoway", #both column (observer) and row (talk) are random
                          type = c("agreement"),
                          unit = c("single"))

#### Audience men
audience_men <- subset(talk, !is.na(audience_men)) # exclude missing data

wide_audience_men <- spread(audience_men[,c("talk_id", "observer_talk", "audience_men")], 
                            observer_talk, audience_men)

# treat observer 1 and 2 as one unit, and observer 3 and 4 as one unit
wide_audience_men_a <- wide_audience_men[,c(1:3)]
wide_audience_men_b <- wide_audience_men[,c(1,4,5)]
names(wide_audience_men_a) <- c("talk_id", "observer_a", "observer_b")
names(wide_audience_men_b) <- c("talk_id", "observer_a", "observer_b")

# merge
wide_audience_men <- rbind(wide_audience_men_a, wide_audience_men_b)

# exclude sessions without double sampling
wide_audience_men <- subset(wide_audience_men, !is.na(observer_a) & !is.na(observer_b)) 

icc_audience_men <- icc(wide_audience_men[,2:3],
                        model = "twoway", #both column (observer) and row (talk) are random
                        type = c("agreement"),
                        unit = c("single"))
#### Duration QA
duration_qa <- subset(talk, !is.na(duration_qa)) # exclude missing data

wide_duration_qa <- spread(duration_qa[,c("talk_id", "observer_talk", "duration_qa")], 
                           observer_talk, duration_qa)

# treat observer 1 and 2 as one unit, and observer 3 and 4 as one unit
wide_duration_qa_a <- wide_duration_qa[,c(1:3)]
wide_duration_qa_b <- wide_duration_qa[,c(1,4,5)]
names(wide_duration_qa_a) <- c("talk_id", "observer_a", "observer_b")
names(wide_duration_qa_b) <- c("talk_id", "observer_a", "observer_b")

# merge
wide_duration_qa <- rbind(wide_duration_qa_a, wide_duration_qa_b)

# exclude sessions without double sampling
wide_duration_qa <- subset(wide_duration_qa, !is.na(observer_a) & !is.na(observer_b)) 

icc_duration_qa <- icc(wide_duration_qa[,2:3],
                       model = "twoway", #both column (observer) and row (talk) are random
                       type = c("agreement"),
                       unit = c("single"))

##### Question-level ####

###### Hands total ####
# NB: hands IOR is a bit more difficult because in large rooms, hands total was counted by one observer and hands men were counted by
# the other, meaning that two observers make up one observations, and a double-observation in large rooms requires four observers
# but, many observers still noted both hands (men only and total) anyway, meaning we will have a larger sample size for IOR

hands_total <- subset(question, !is.na(hands_total)) # exclude missing data

wide_hands_total <- spread(hands_total[,c("question_id", "observer_talk", "hands_total")], 
                           observer_talk, hands_total)

# treat observer 1 and 2 as one unit, and observer 3 and 4 as one unit
wide_hands_total_a <- wide_hands_total[,c(1:3)]
wide_hands_total_b <- wide_hands_total[,c(1,4,5)]
names(wide_hands_total_a) <- c("talk_id", "observer_a", "observer_b")
names(wide_hands_total_b) <- c("talk_id", "observer_a", "observer_b")

# merge
wide_hands_total <- rbind(wide_hands_total_a, wide_hands_total_b)

# exclude sessions without double sampling
wide_hands_total$observer_a <- as.numeric(wide_hands_total$observer_a)
wide_hands_total$observer_b <- as.numeric(wide_hands_total$observer_b)

wide_hands_total <- subset(wide_hands_total, !is.na(observer_a) & !is.na(observer_b)) 

icc_hands_total <- icc(wide_hands_total[,2:3],
                       model = "twoway", #both column (observer) and row (talk) are random
                       type = c("agreement"),
                       unit = c("single"))

###### Hands men ####

hands_men <- subset(question, !is.na(hands_men)) # exclude missing data

wide_hands_men <- spread(hands_men[,c("question_id", "observer_talk", "hands_men")], 
                         observer_talk, hands_men)

# treat observer 1 and 2 as one unit, and observer 3 and 4 as one unit
wide_hands_men_a <- wide_hands_men[,c(1:3)]
wide_hands_men_b <- wide_hands_men[,c(1,4,5)]
names(wide_hands_men_a) <- c("talk_id", "observer_a", "observer_b")
names(wide_hands_men_b) <- c("talk_id", "observer_a", "observer_b")

# merge
wide_hands_men <- rbind(wide_hands_men_a, wide_hands_men_b)

# exclude sessions without double sampling
wide_hands_men$observer_a <- as.numeric(wide_hands_men$observer_a)
wide_hands_men$observer_b <- as.numeric(wide_hands_men$observer_b)

wide_hands_men <- subset(wide_hands_men, !is.na(observer_a) & !is.na(observer_b)) 

icc_hands_men <- icc(wide_hands_men[,2:3],
                     model = "twoway", #both column (observer) and row (talk) are random
                     type = c("agreement"),
                     unit = c("single"))

######  Questioner gender ####
questioner_gender <- subset(question, !is.na(questioner_gender)) # exclude missing data

wide_questioner_gender <- spread(questioner_gender[,c("question_id", "observer_talk", "questioner_gender")], 
                                 observer_talk, questioner_gender)

# treat observer 1 and 2 as one unit, and observer 3 and 4 as one unit
wide_questioner_gender_a <- wide_questioner_gender[,c(1:3)]
wide_questioner_gender_b <- wide_questioner_gender[,c(1,4,5)]
names(wide_questioner_gender_a) <- c("session_id", "observer_a", "observer_b")
names(wide_questioner_gender_b) <- c("session_id", "observer_a", "observer_b")

# merge
wide_questioner_gender <- rbind(wide_questioner_gender_a, wide_questioner_gender_b)

# exclude sessions without double sampling
wide_questioner_gender <- subset(wide_questioner_gender, !is.na(observer_a) & !is.na(observer_b)) 

kappa_questioner_gender <- kappa2(wide_questioner_gender[,2:3], weight = "unweighted")

######  Questioner age ####
questioner_age <- subset(question, !is.na(questioner_age)) # exclude missing data

wide_questioner_age <- spread(questioner_age[,c("question_id", "observer_talk", "questioner_age")], 
                              observer_talk, questioner_age)

# treat observer 1 and 2 as one unit, and observer 3 and 4 as one unit
wide_questioner_age_a <- wide_questioner_age[,c(1:3)]
wide_questioner_age_b <- wide_questioner_age[,c(1,4,5)]
names(wide_questioner_age_a) <- c("session_id", "observer_a", "observer_b")
names(wide_questioner_age_b) <- c("session_id", "observer_a", "observer_b")

# merge
wide_questioner_age <- rbind(wide_questioner_age_a, wide_questioner_age_b)

# exclude sessions without double sampling
wide_questioner_age <- subset(wide_questioner_age, !is.na(observer_a) & !is.na(observer_b)) 

kappa_questioner_age <- kappa2(wide_questioner_age[,2:3], weight = "unweighted")


#### Collect results ####
ior <- data.frame(variable = c("Host gender", "Host age", "Speaker gender", "Speaker age",
                               "Total audience size", "Men in audience", "Duration Q&A",
                               "Total hands raised", "Hands raised by men",
                               "Questioner gender", "Questioner age"),
                  level = c("Session", "Session", "Talk", "Talk", "Talk", "Talk", "Talk",
                            "Question", "Question", "Question", "Question"),
                  n = c(kappa_host_gender$subjects, kappa_host_age$subjects, kappa_speaker_gender$subjects, 
                        kappa_speaker_age$subjects, icc_audience_total$subjects, icc_audience_men$subjects,
                        icc_duration_qa$subjects, icc_hands_total$subjects, icc_hands_men$subjects,
                        kappa_questioner_gender$subjects, kappa_questioner_age$subjects),
                  test = c("Cohen's kappa", "Cohen's kappa", "Cohen's kappa", "Cohen's kappa", "ICC", "ICC", "ICC",
                           "ICC", "ICC", "Cohen's kappa", "Cohen's kappa"),
                  statistic = c(kappa_host_gender$value, kappa_host_age$value, kappa_speaker_gender$value, 
                                kappa_speaker_age$value, icc_audience_total$value, icc_audience_men$value,
                                icc_duration_qa$value, icc_hands_total$value, icc_hands_men$value,
                                kappa_questioner_gender$value, kappa_questioner_age$value),
                  pval = c(kappa_host_gender$p.value, kappa_host_age$p.value, kappa_speaker_gender$p.value, 
                           kappa_speaker_age$p.value, icc_audience_total$p.value, icc_audience_men$p.value,
                           icc_duration_qa$p.value, icc_hands_total$p.value, icc_hands_men$p.value,
                           kappa_questioner_gender$p.value, kappa_questioner_age$p.value))

## plot results
source("scripts/EDI_ggplot_theme.R")

col_sig <- clrs[c(10,2)] %>%
  color() %>% 
  set_names(nm = c("sig", "nonsig"))

ior <- ior %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

ggplot(subset(ior, level == "Session"), aes(x = statistic, y = variable)) + 
  geom_point(size = 6, aes(col = test)) + 
  scale_color_manual(values = c(clrs[3], clrs[7])) +
  xlim(0,1)+
  theme(legend.position = "none",
        plot.title = element_text(vjust=5))+
  geom_vline(xintercept = 0.81, col = clrs[3], linetype = "dotted", linewidth=1) +
 # geom_vline(xintercept = 0.75, col = clrs[7], linetype = "dotted", linewidth=1) +
  labs(x = "Statistic", y = "Variable", col = "Test", title = "a) Session level") +
  geom_text(aes(label = paste0("N = ", n), y = variable, x = 0.1),
            position=position_dodge(width=0.9), size = 6) -> session_plot

ggplot(subset(ior, level == "Talk"), 
       aes(x = statistic, y = fct_relevel(variable,
                                          c("Duration Q&A", 
                                            "Speaker age",
                                            "Speaker gender",
                                            "Men in audience",
                                            "Total audience size")))) + 
  geom_point(size = 6,  aes(col = test)) + 
  scale_color_manual(values = c(clrs[3], clrs[7])) +
  xlim(0,1)+
  theme(legend.position = "none",
        plot.title = element_text(vjust=5))+
  geom_vline(xintercept = 0.81, col = clrs[3], linetype = "dotted", linewidth=1) +
  geom_vline(xintercept = 0.75, col = clrs[7], linetype = "dotted", linewidth=1) +
  labs(x = "Statistic", y = "Variable", col = "Test", title = "b) Talk level") +
  geom_text(aes(label = paste0("N = ", n), y = variable, x = 0.1),
            position=position_dodge(width=0.9), size = 6)-> talk_plot

ggplot(subset(ior, level == "Question"), 
       aes(x = statistic, y = fct_relevel(variable,
                                          c("Questioner age", 
                                            "Questioner gender",
                                            "Hands raised by men",
                                            "Total hands raised")))) + 
  geom_point(size = 6, aes(col = test)) + 
  scale_color_manual(values = c(clrs[3], clrs[7])) +
  xlim(0,1)+
  geom_vline(xintercept = 0.81, col = clrs[3], linetype = "dotted", linewidth=1) +
  geom_vline(xintercept = 0.75, col = clrs[7], linetype = "dotted", linewidth=1) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=16),
        plot.title = element_text(vjust=5))+
  labs(x = "Statistic", y = "Variable", col = "Test", title = "c) Question level") +
  geom_text(aes(label = paste0("N = ", n), y = variable, x = 0.1),
            position=position_dodge(width=0.9), size = 6) -> question_plot

plot_grid(session_plot, talk_plot, question_plot,
          ncol = 1, rel_heights = c(0.8,1,1),
          align = "hv", axis="lb",
          label_fontface = "plain", label_size = 22) -> ior_plots

ggsave(ior_plots, file = "plots/supplementary/IOR.png", height=12, width=10)

