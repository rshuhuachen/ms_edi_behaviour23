### Question asking in general ####

# load packages
pacman::p_load(lme4, tidyverse, data.table, performance, DHARMa, jtools)

### Load data ####
system("cp ../EDI-behaviour2023/data/clean/question_asking/question_asking_data_condensed_for_analysis.RData ./data/question_asking/")
load("data/question_asking/question_asking_data_condensed_for_analysis.RData")
load("data/post_survey/clean_survey_all.RData")

### Set theme ####
source("scripts/EDI_ggplot_theme.R")

### Load function to output model results
source("scripts/function_collect_model_output.R")

# change gender level
# clean_survey$gender <- factor(clean_survey$gender, levels = c("Male", "Female", "Non-binary", "Prefer_not_say"))
# clean_survey$gender <- gsub("Prefer_not_say", NA, clean_survey$gender)
survey <- clean_survey
save(survey, file = "data/post_survey/clean_survey_all.RData")

## plenary
load("data/question_asking/combined_session_talk_question_all_long_plenary.RData")

### select
plenary <- plenary_treatment %>% dplyr::select(c(observer_session,
                                          session_id, speaker_pronoun,
                                          question_nr, questioner_gender,
                                          compliment, question_type_e,
                                          duration_qa, notes_sheet_session,
                                          notes_sheet_talk, notes_sheet_question))

## manually correct IOR
plenary <- plenary %>% arrange(session_id, observer_session, question_nr)
#write.csv(plenary, file="data/question-asking/plenary_raw_to_correct.csv", quote=F,row.names = F)

plenary_clean <- read_excel("data/question_asking/plenary_corrected_manual.xlsx")
plenary_clean$id <- paste0(plenary_clean$observer_session, "_", plenary_clean$session_id)
plenary_clean <- subset(plenary_clean, id != "Rebecca_P4")

plenary <- unique(plenary_clean[,c("session_id", "speaker_pronoun",
                                   "question_nr", "questioner_gender")])

plenary <- plenary %>% mutate(questioner_female = case_when(
  questioner_gender == "M" ~ 0,
  questioner_gender == "F" ~ 1,
))

# load in registration to get audience prop per gender
reg <- fread("data/pre_survey/Registration_clean.tsv")
summary(as.factor(reg$Pronouns))
nrow(subset(reg, Pronouns=="Female")) / nrow(reg) -> prop_audience_female
plenary$prop_audience_female = prop_audience_female

save(plenary, file="data/question_asking/plenary.RData")


### Adding hands proportion ####
data_analysis$hands_prop_men <- data_analysis$hands_men/data_analysis$hands_total
data_analysis$hands_prop_men[which(data_analysis$hands_prop_men == Inf)] <- NA
data_analysis$hands_prop_men[which(data_analysis$hands_prop_men == "NaN")] <- NA

data_analysis$hands_prop_women <- data_analysis$hands_women/data_analysis$hands_total
data_analysis$hands_prop_women[which(data_analysis$hands_prop_women == Inf)] <- NA
data_analysis$hands_prop_women[which(data_analysis$hands_prop_women == "NaN")] <- NA

### Adding student age category ####
#simply whether student or not
data_analysis <- data_analysis %>% mutate(
  host_1_student = as.factor(case_when(
    host_1_career_short == "Early career" ~ "Student",
    host_1_career_short == "Mid career" |  host_1_career_short == "Late career" ~ "No student")),
  speaker_student = as.factor(case_when(
    speaker_career_short == "Early career" ~ "Student",
    speaker_career_short == "Mid career" |  speaker_career_short == "Late career" ~ "No student")))

# add 'women questioner'

data_analysis <- data_analysis %>% mutate(gender_questioner_female = case_when(
  gender_questioner_male == 0 ~ 1,
  gender_questioner_male == 1 ~ 0,
  is.na(gender_questioner_male) ~ NA))

# add talk_id for random effect structure
data_analysis$talk_id <- as.factor(paste0(data_analysis$session_id,
                                          "_",
                                          data_analysis$talk_nr))


save(data_analysis, file = "data/question_asking/question_asking_data_condensed_for_analysis.RData")
