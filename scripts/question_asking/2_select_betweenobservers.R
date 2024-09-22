#### NOTE THIS SCRIPT IS NOT FUNCTIONAL ####
#### as it refers to data sheets not made public, and contains names of observers etc. plus sensitive information
#### only the processed, anonymised data are publicly available, however this script is included to demonstrate 
#### how the data were cleaned

##### In this script, we combine the data of the different observers into
##### the data that will be used for the models: i.e. collapsing the 
##### multiple observer data into one variable

##### Depending on the variable, we will take a mean, max, one at random, etc. of the 
##### different observers


#### packages ####
pacman::p_load(tidyverse, data.table, matrixStats)

#### load data ####
load("data/clean/question_asking/combined_session_talk_question_all_treatment_wide.RData")
data <- data_wide_clean #rename to make it easier
rm(data_wide_clean)

#### mutate a new column for all data ####

names(data)

### a lot of data is replicated that isn't needed to be
### also leave out non-anonymous data and only select what will be used in the models
data <- as.data.frame(data)

data_addcol <- data %>% rowwise() %>%
  mutate(
    ## things that are general
    day = day_observer1,
    ## host 1
    host_1_gender = case_when(
      is.na(host_1_gender_observer2) ~ host_1_gender_observer1,
      !is.na(host_1_gender_observer2) & is.na(host_1_gender_observer3) & 
        host_1_gender_observer1 == host_1_gender_observer2 ~ host_1_gender_observer1,
      !is.na(host_1_gender_observer2) & !is.na(host_1_gender_observer3) & 
        is.na(host_1_gender_observer4) & host_1_gender_observer1 == host_1_gender_observer2 & 
        host_1_gender_observer2 == host_1_gender_observer3 ~ host_1_gender_observer1,
      !is.na(host_1_gender_observer2) & !is.na(host_1_gender_observer3) & 
        !is.na(host_1_gender_observer4) & host_1_gender_observer1 == host_1_gender_observer2 & 
        host_1_gender_observer2 == host_1_gender_observer3 & 
        host_1_gender_observer3 == host_1_gender_observer4 ~ host_1_gender_observer1),
    host_1_pronoun = case_when(
      is.na(host_1_pronoun_observer2) ~ host_1_pronoun_observer1,
      !is.na(host_1_pronoun_observer2) & is.na(host_1_pronoun_observer3) & 
        host_1_pronoun_observer1 == host_1_pronoun_observer2 ~ host_1_pronoun_observer1,
      !is.na(host_1_pronoun_observer2) & !is.na(host_1_pronoun_observer3) & 
        is.na(host_1_pronoun_observer4) & host_1_pronoun_observer1 == host_1_pronoun_observer2 & 
        host_1_pronoun_observer2 == host_1_pronoun_observer3 ~ host_1_pronoun_observer1,
      !is.na(host_1_pronoun_observer2) & !is.na(host_1_pronoun_observer3) & 
        !is.na(host_1_pronoun_observer4) & host_1_pronoun_observer1 == host_1_pronoun_observer2 & 
        host_1_pronoun_observer2 == host_1_pronoun_observer3 & 
        host_1_pronoun_observer3 == host_1_pronoun_observer4 ~ host_1_pronoun_observer1),
    host_1_career = host_1_career_observer1,
    host_1_age = case_when( #### change to highest agreement instead??
      is.na(host_1_age_observer2) ~ host_1_age_observer1,
      !is.na(host_1_age_observer2) & is.na(host_1_age_observer3) & 
        host_1_age_observer1 == host_1_age_observer2 ~ host_1_age_observer1,
      !is.na(host_1_age_observer2) & !is.na(host_1_age_observer3) & 
        is.na(host_1_age_observer4) & host_1_age_observer1 == host_1_age_observer2 & 
        host_1_age_observer2 == host_1_age_observer3 ~ host_1_age_observer1,
      !is.na(host_1_age_observer2) & !is.na(host_1_age_observer3) & 
        !is.na(host_1_age_observer4) & host_1_age_observer1 == host_1_age_observer2 & 
        host_1_age_observer2 == host_1_age_observer3 & 
        host_1_age_observer3 == host_1_age_observer4 ~ host_1_age_observer1),
    ## skip all other hosts: won't do anything with this anyway
    ## speaker
    speaker_career = speaker_career_observer1,
    speaker_age = case_when( #### change to highest agreement instead??
      is.na(speaker_age_observer2) ~ speaker_age_observer1,
      !is.na(speaker_age_observer2) & is.na(speaker_age_observer3) & 
        speaker_age_observer1 == speaker_age_observer2 ~ speaker_age_observer1,
      !is.na(speaker_age_observer2) & !is.na(speaker_age_observer3) & 
        is.na(speaker_age_observer4) & speaker_age_observer1 == speaker_age_observer2 & 
        speaker_age_observer2 == speaker_age_observer3 ~ speaker_age_observer1,
      !is.na(speaker_age_observer2) & !is.na(speaker_age_observer3) & 
        !is.na(speaker_age_observer4) & speaker_age_observer1 == speaker_age_observer2 & 
        speaker_age_observer2 == speaker_age_observer3 & 
        speaker_age_observer3 == speaker_age_observer4 ~ speaker_age_observer1),
    speaker_gender_infer = case_when(
      is.na(speaker_gender_infer_observer2) ~ speaker_gender_infer_observer1,
      !is.na(speaker_gender_infer_observer2) & is.na(speaker_gender_infer_observer3) & 
        speaker_gender_infer_observer1 == speaker_gender_infer_observer2 ~ speaker_gender_infer_observer1,
      !is.na(speaker_gender_infer_observer2) & !is.na(speaker_gender_infer_observer3) & 
        is.na(speaker_gender_infer_observer4) & speaker_gender_infer_observer1 == speaker_gender_infer_observer2 & 
        speaker_gender_infer_observer2 == speaker_gender_infer_observer3 ~ speaker_gender_infer_observer1,
      !is.na(speaker_gender_infer_observer2) & !is.na(speaker_gender_infer_observer3) & 
        !is.na(speaker_gender_infer_observer4) & speaker_gender_infer_observer1 == speaker_gender_infer_observer2 & 
        speaker_gender_infer_observer2 == speaker_gender_infer_observer3 & 
        speaker_gender_infer_observer3 == speaker_gender_infer_observer4 ~ speaker_gender_infer_observer1),
    speaker_pronoun = case_when(
      is.na(speaker_pronoun_observer2) ~ speaker_pronoun_observer1,
      !is.na(speaker_pronoun_observer2) & is.na(speaker_pronoun_observer3) & 
        speaker_pronoun_observer1 == speaker_pronoun_observer2 ~ speaker_pronoun_observer1,
      !is.na(speaker_pronoun_observer2) & !is.na(speaker_pronoun_observer3) & 
        is.na(speaker_pronoun_observer4) & speaker_pronoun_observer1 == speaker_pronoun_observer2 & 
        speaker_pronoun_observer2 == speaker_pronoun_observer3 ~ speaker_pronoun_observer1,
      !is.na(speaker_pronoun_observer2) & !is.na(speaker_pronoun_observer3) & 
        !is.na(speaker_pronoun_observer4) & speaker_pronoun_observer1 == speaker_pronoun_observer2 & 
        speaker_pronoun_observer2 == speaker_pronoun_observer3 & 
        speaker_pronoun_observer3 == speaker_pronoun_observer4 ~ speaker_pronoun_observer1),
    ## audience counts -> take mean and check SD for big deviations
    audience_total = mean(c(audience_total_observer1, audience_total_observer2, audience_total_observer3,
                        audience_total_observer4), na.rm = TRUE),
    audience_total_sd = sd(c(audience_total_observer1, audience_total_observer2, audience_total_observer3,
                         audience_total_observer4), na.rm = TRUE),
    audience_men = mean(c(audience_men_observer1, audience_men_observer2, audience_men_observer3,
                        audience_men_observer4), na.rm = TRUE),
    audience_men_sd = sd(c(audience_men_observer1, audience_men_observer2, audience_men_observer3,
                        audience_men_observer4), na.rm = TRUE),
    ## QA time
    duration_qa = mean(c(duration_qa_observer1, duration_qa_observer2, duration_qa_observer3,
                       duration_qa_observer4), na.rm=TRUE),
    # if one noted overtime, then overtime, if not then no
    overtime = case_when(
      overtime_observer1 == "Y" |overtime_observer2 == "Y" | overtime_observer3 == "Y"|
        overtime_observer4 == "Y" ~ "Y",
      TRUE ~ "N"),
    ## hands -> max!
    hands_total = max(c(hands_total_observer1, hands_total_observer2, hands_total_observer3,
                      hands_total_observer4), na.rm=TRUE),
    hands_men = max(c(hands_men_observer1, hands_men_observer2, hands_men_observer3,
                      hands_men_observer4), na.rm=TRUE),
    
    ## questioner info
    questioner_gender = case_when(
      is.na(questioner_gender_observer2) ~ questioner_gender_observer1,
      !is.na(questioner_gender_observer2) & is.na(questioner_gender_observer3) & 
        questioner_gender_observer1 == questioner_gender_observer2 ~ questioner_gender_observer1,
      !is.na(questioner_gender_observer2) & !is.na(questioner_gender_observer3) & 
        is.na(questioner_gender_observer4) & questioner_gender_observer1 == questioner_gender_observer2 & 
        questioner_gender_observer2 == questioner_gender_observer3 ~ questioner_gender_observer1,
      !is.na(questioner_gender_observer2) & !is.na(questioner_gender_observer3) & 
        !is.na(questioner_gender_observer4) & questioner_gender_observer1 == questioner_gender_observer2 & 
        questioner_gender_observer2 == questioner_gender_observer3 & 
        questioner_gender_observer3 == questioner_gender_observer4 ~ questioner_gender_observer1),
    questioner_age = case_when( #### change to highest agreement instead??
      is.na(questioner_age_observer2) ~ questioner_age_observer1,
      !is.na(questioner_age_observer2) & is.na(questioner_age_observer3) & 
        questioner_age_observer1 == questioner_age_observer2 ~ questioner_age_observer1,
      !is.na(questioner_age_observer2) & !is.na(questioner_age_observer3) & 
        is.na(questioner_age_observer4) & questioner_age_observer1 == questioner_age_observer2 & 
        questioner_age_observer2 == questioner_age_observer3 ~ questioner_age_observer1,
      !is.na(questioner_age_observer2) & !is.na(questioner_age_observer3) & 
        !is.na(questioner_age_observer4) & questioner_age_observer1 == questioner_age_observer2 & 
        questioner_age_observer2 == questioner_age_observer3 & 
        questioner_age_observer3 == questioner_age_observer4 ~ questioner_age_observer1),
    ## compliment, if one noted then yes
    compliment = case_when(
      compliment_observer1 == "Y" |compliment_observer2 == "Y" | compliment_observer3 == "Y"|
        compliment_observer4 == "Y" ~ "Y",
      TRUE ~ "N"),
    ## question type -> only focus on category E, if one says yes then yes
    ## if nobody noted a question type, then NA
    question_type_e = case_when(
      question_type_e_observer1 == "1" |question_type_e_observer2 == "1" | 
        question_type_e_observer3 == "1"|
        question_type_e_observer4 == "1" ~ "1",
      is.na(question_type_e_observer1) & is.na(question_type_e_observer2) &
        is.na(question_type_e_observer3) &
        is.na(question_type_e_observer4) ~ NA,
      TRUE ~ "0"),
    ## questioner info -> jumper important!
    questioner_info = paste(questioner_info_observer1, questioner_info_observer2 ,
                              questioner_info_observer3,questioner_info_observer4, sep = ", "),
    ## alternating hosts, if one noted then yes
    alternating_hosts = paste(alternating_hosts_observer1, alternating_hosts_observer2 ,
                               alternating_hosts_observer3,alternating_hosts_observer4, sep = ", "),
    ## uncertainty, if one yes then yes
    uncertainty_gender_host = case_when(
      uncertainty_gender_host_observer1 == 1 |uncertainty_gender_host_observer2 == 1 |
        uncertainty_gender_host_observer3 == 1 | uncertainty_gender_host_observer4 == 1 ~ 1,
      TRUE ~ 0),
    uncertainty_gender_speaker = case_when(
      uncertainty_gender_speaker_observer1 == 1 |uncertainty_gender_speaker_observer2 == 1 |
        uncertainty_gender_speaker_observer3 == 1 | uncertainty_gender_speaker_observer4 == 1 ~ 1,
      TRUE ~ 0),
    uncertainty_gender_questioner = case_when(
      uncertainty_gender_questioner_observer1 == 1 |uncertainty_gender_questioner_observer2 == 1 |
        uncertainty_gender_questioner_observer3 == 1 | uncertainty_gender_questioner_observer4 == 1 ~ 1,
      TRUE ~ 0),
    uncertainty_count_audience = case_when(
      uncertainty_count_audience_observer1 == 1 |uncertainty_count_audience_observer2 == 1 |
        uncertainty_count_audience_observer3 == 1 | uncertainty_count_audience_observer4 == 1 ~ 1,
      TRUE ~ 0),
    uncertainty_count_hands = case_when(
      uncertainty_count_hands_observer1 == 1 |uncertainty_count_hands_observer2 == 1 |
        uncertainty_count_hands_observer3 == 1 | uncertainty_count_hands_observer4 == 1 ~ 1,
      TRUE ~ 0),
    ## allocate question, concatenate
    allocator_question = paste(allocator_question_observer1, allocator_question_observer2 ,
                                allocator_question_observer3, allocator_question_observer4, sep = ", "),
    ## lecture hall
    lecture_hall = lecture_hall_observer1,
    ## room size
    room_size = room_size_observer1,
    ## treatment
    treatment = Treatment_observer1,
    condition = Condition_observer1,
    ## number of observers
    no_observers = case_when(
      !is.na(observer_session_observer4) ~ 4,
      !is.na(observer_session_observer3) ~ 3,
      !is.na(observer_session_observer2) ~ 2,
      !is.na(observer_session_observer1) ~ 1,
      TRUE ~ NA
    ),
    double_sampled = case_when(
      room_size == "Large" & !is.na(observer_session_observer4) ~ "double_sampled",
      room_size != "Large" & !is.na(observer_session_observer2) ~ "double_sampled",
      TRUE ~"no_double_sampled")) %>% ungroup()

data_addcol <- data_addcol %>% mutate(
  hands_women_raw = as.numeric(hands_total) - as.numeric(hands_men),
  hands_women = case_when(
    hands_women_raw >= 0 ~ hands_women_raw,
    TRUE ~ NA),
  audience_women = audience_total - audience_men,
)

data_addcol$allocator_question <- gsub(", NA", "", data_addcol$allocator_question)
data_addcol$alternating_hosts <- gsub(", NA", "", data_addcol$alternating_hosts)
data_addcol$questioner_info <- gsub(", NA", "", data_addcol$questioner_info)

## add columns: jumper, followup, repeat, host asks
data_addcol <- data_addcol %>% mutate(
  jumper = as.factor(case_when(
    grepl("J|jumper|Jumper", questioner_info) ~ 1
  )),
  followup = as.factor(case_when(
    grepl("followup|Followup|Follow up| follow up", questioner_info) ~ 1
  )),
  "repeat" = as.factor(case_when(
    grepl("Repeat|repeat", questioner_info) ~ 1
  )),
  host_asks = as.factor(case_when(
    grepl("chair|Host|host|Chair|H", questioner_info) ~ 1
  )),
  )
#### Select for data analysis ####

data_select <- data_addcol %>%
  select(c(session_id, talk_nr, question_nr, room_size, day:host_asks))

data_select[data_select == "NaN"] <- NA
data_select[data_select == "NA"] <- NA

### Add a few columns: from metadata time of day and general vs symposium ###
treatments <- read.csv("data/metadata/General_treatments_by_Session.csv")
treatments$Session.ID <- as.character(treatments$Session.ID)
data_select <- left_join(data_select, treatments[,c("Session.ID", "Time", "Symposium.general")], by = c("session_id" = "Session.ID"))
data_select <- data_select %>% rename(time = Time, symp_general = Symposium.general)

### Add column: treatment successful?
#first take all first questions from treatment sessions
first_q <- subset(data_select, question_nr == 1 & treatment == "Treatment")

first_q <- first_q %>% mutate(
  treatment_success = as.factor(case_when(
    condition == "First question to a man" & questioner_gender == "M" ~ "Successful",
    condition == "First question to a woman" & questioner_gender == "F" ~ "Successful",
    condition == "First question to a man" & questioner_gender == "F" ~ "Unsuccessful",
    condition == "First question to a woman" & questioner_gender == "M" ~ "Unsuccessful",
    TRUE ~ "NA"
  )))

data_select <- left_join(data_select, first_q[,c("session_id", "talk_nr", "treatment_success")])

### correct/condense career stages

data_select <- data_select %>% mutate(
  host_1_career_short = as.factor(case_when(
    host_1_career == "Students (BSc, MSc)" | host_1_career == "PhD student" ~ "Early career",
    host_1_career == "Lecturer" | host_1_career == "Post doctorate" | host_1_career == "Researcher"  ~
      "Mid career",
    host_1_career == "Associate professor" | host_1_career == "Professor" ~ "Late career")),
  speaker_career_short = as.factor(case_when(
    speaker_career == "Students (BSc, MSc)" | speaker_career == "PhD student" ~ "Early career",
    speaker_career == "Lecturer" | speaker_career == "Post doctorate" | speaker_career == "Researcher"  ~
      "Mid career",
    speaker_career == "Associate professor" | speaker_career == "Professor" ~ "Late career"))
  )

## add day number

data_select <- data_select %>% mutate(
  day_nr = case_when(
    day == "Monday" ~ 1,
    day == "Tuesday" ~ 2,
    day == "Wednesday" ~ 3,
    day == "Thursday" ~ 4,
    day == "Friday" ~ 5,
    day == "Saturday" ~ 6
  )
)
### Rearrange columns
#probably a better way but want to do it manually
data_select_clean <- data_select[,c("session_id", "talk_nr", "question_nr", "day_nr", "time", "lecture_hall", "room_size", "symp_general",
                                    "treatment", "condition", "treatment_success", "audience_total", "audience_total_sd", 
                                    "audience_men", "audience_men_sd", "audience_women", "duration_qa", "overtime",
                                    "host_1_gender", "host_1_career_short", "host_1_age", 
                                    "speaker_career_short", "speaker_age", "speaker_gender_infer",  
                                    "alternating_hosts", "hands_total", "hands_men", "hands_women", "questioner_gender", 
                                    "questioner_age", "compliment", "question_type_e",
                                    "jumper", "followup", "repeat", "host_asks", "allocator_question",
                                    "no_observers", "double_sampled",
                                    "uncertainty_count_audience", "uncertainty_count_hands")] #just drop hands_women_raw and pronoun (just keep inferred gender)

## remove those with huge SD in audience counts
data_select_clean <- data_select_clean %>% rename(speaker_gender = speaker_gender_infer)
data_select_clean$audience_total[which(data_select_clean$audience_total_sd > 20)] <- NA
data_select_clean$audience_men[which(data_select_clean$audience_total_sd > 20)]<- NA
data_select_clean$audience_women[which(data_select_clean$audience_total_sd > 20)]<- NA

data_select_clean$audience_total[which(data_select_clean$audience_men_sd > 10)]<- NA
data_select_clean$audience_men[which(data_select_clean$audience_men_sd > 10)]<- NA
data_select_clean$audience_women[which(data_select_clean$audience_men_sd > 10)]<- NA

## add audience proportions

data_select_clean <- data_select_clean %>% mutate(
  audience_men_prop = audience_men / audience_total,
  audience_women_prop = audience_women / audience_total, .after = audience_women)

# put in correct data class

nums <- c(1:4, 12:19, 28:30,40)
factors <- c(5:11, 20:26, 31:39,41:42)

data_select_clean[nums] <- lapply(data_select_clean[nums], as.numeric)
data_select_clean[factors] <- lapply(data_select_clean[factors], as.factor)

## for model: gender questioner male = 1
data_select_clean <- data_select_clean %>% mutate(
  gender_questioner_male = as.factor(case_when(
    questioner_gender == "M" ~ "1",
    questioner_gender == "F" ~ "0",
    TRUE ~ NA)), .after=questioner_gender
  )

### save dataframe, rename first
data_analysis <- data_select_clean
save(data_analysis, file = "data/clean/question_asking/question_asking_data_condensed_for_analysis.RData")
# this is the file that is made public and can be found under data/question_asking!