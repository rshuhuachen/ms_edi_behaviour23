#### NOTE THIS SCRIPT IS NOT FUNCTIONAL ####
#### as it refers to data sheets not made public, and contains names of observers etc. plus sensitive information
#### only the processed, anonymised data are publicly available, however this script is included to demonstrate 
#### how the data were cleaned

## packages

pacman::p_load(readxl, tidyverse)

### load in files ###
files <- list.files(path = "data/raw/question-asking/digitized_corrected_sheets/", 
                    pattern = "_edit_corrected", full.names = T) 

## function to load in data

load_data <- function(sheet_no, file_list){
  file_all <- data.frame()
  for (i in 1:length(files)){
    one_file <- read_excel(file_list[i], sheet = sheet_no, skip = 1)
    file_all <- rbind(file_all, one_file)}
  return(file_all)
}

#load in data per type
all_sessions <- load_data(sheet_no = 1, file_list = files) 
all_talks <- load_data(sheet_no = 2, file_list = files)
all_questions <- load_data(sheet_no = 3, file_list = files)

#cleaning

all_sessions$Day[which(all_sessions$Day == "Wedenesday")] <- "Wednesday"
all_sessions$Day[which(all_sessions$Day == "Fri.")] <- "Friday"
all_sessions$Day[which(all_sessions$Day == "Mon.")] <- "Monday"
all_sessions$Day[which(all_sessions$Day == "Sat.")] <- "Saturday"
all_sessions$Day[which(all_sessions$Day == "Thu.")] <- "Thursday"
all_sessions$Day[which(all_sessions$Day == "Thurday")] <- "Thursday"
all_sessions$Day[which(all_sessions$Day == "Tue.")] <- "Tuesday"
all_sessions$Day[which(all_sessions$Day == "Wed.")] <- "Wednesday"

all_sessions[all_sessions == "he/him"] <- "He/him"
all_sessions[all_sessions == "she/her"] <- "She/her"

all_sessions[all_sessions == "lecturer"] <- "Lecturer"
all_sessions[all_sessions == "post doctorate"] <- "Post doctorate"
all_sessions[all_sessions == "Postdoctoral Researcher "] <- "Post doctorate"
all_sessions[all_sessions == "professor"] <- "Professor"
all_sessions[all_sessions == "researcher"] <- "Researcher"
all_sessions[all_sessions == "students (BSc, MSc)"] <- "Students (BSc, MSc)"

# combine everything in a big file
all_data <- right_join(all_sessions, all_talks, by = c("Session ID", "Observer name"), 
                      suffix = c("_session", "_talk"))

all_data <- left_join(all_data, all_questions, 
                      by = c("Session ID", "Talk number", "Observer name" = "Observer"))

all_data %>% select(c(`Observer name`, `Session ID`, `Talk number`, `Question number`)) %>%
  group_by(`Observer name`, `Session ID`, `Talk number`, `Question number`) %>%
  mutate(n = row_number()) %>% filter(n>1)%>% nrow() #should be 0/empty

# combine with 'observer number'

sessions <- read_excel("data/Sessions_information.xlsx")
sessions$`Session ID` <- as.character(sessions$`Session ID`)
all_data <- left_join(all_data, sessions[,c("Session ID", "Name observer", "Observer")],
                      by = c("Session ID", "Observer name" = "Name observer"))

all_data <- all_data %>% rename("transcriber" = "Transcriber name",
                                "observer_session" = "Observer name",
                                "day" = "Day",
                                "session_id" = "Session ID",
                                "host_1_name" = "Primary host name",
                                "host_1_gender" = "Host 1 gender",
                                "host_1_pronoun" = "Host 1 confirmed pronouns",
                                "host_1_career" = "Host 1 confirmed career stage",
                                "how_host_1_career" = "How was career stage confirmed H1",
                                "host_1_age" = "Host 1 age cat",
                                "host_2_name" = "Host 2 name",
                                "host_2_gender" = "Host 2 gender",
                                "host_2_pronoun" = "Host 2 confirmed pronouns",
                                "host_2_career" = "Career stage H2",
                                "how_host_2_career" = "How was career stage confirmed H2",
                                "host_2_age" = "Host 2 age cat",
                                "host_3_name" = "Host 3 name",
                                "host_3_gender" = "Host 3 gender",
                                "host_3_pronoun" = "Host 3 confirmed pronouns",
                                "host_3_career" = "Career stage H3",
                                "how_host_3_career" = "How was career stage confirmed H3",
                                "host_3_age" = "Host 3 age cat",
                                "notes_sheet_session" = "Notes sheet_session",
                                "notes_digitize_session" = "Notes digitizing_session",
                                "talk_nr" = "Talk number",
                                "speaker_name" = "Speaker last name",
                                "speaker_career" = "Career stage speaker",
                                "how_speaker_career" = "How was career stage confirmed",
                                "speaker_age" = "Age class speaker (1-3)",
                                "speaker_gender_infer" = "Inferred gender speaker",
                                "speaker_pronoun" = "Confirmed pronouns",
                                "how_speaker_pronoun" = "How were pronouns confirmed",
                                "audience_total" = "Total audience size",
                                "audience_men" = "Men in audience",
                                "start_qa" = "Time start Q&A",
                                "end_qa" = "Time end Q&A",
                                "overtime" = "Talked over time? (Y/N)",
                                "notes_sheet_talk" = "Notes sheet_talk",
                                "notes_digitize_talk" = "Notes digitizing_talk",
                                "question_nr" = "Question number",
                                "hands_total" = "Hand raised total",
                                "hands_men" = "Hands raised men",
                                "questioner_gender" = "Gender questioner",
                                "questioner_age" = "Age class questioner (1-3)",
                                "compliment" = "Positive words (Y/N)",
                                "question_type_a" = "Type A",
                                "question_type_b" = "Type B",
                                "question_type_c" = "Type C",
                                "question_type_d" = "Type D",
                                "question_type_e" = "Type E",
                                "question_type_f" = "Type F",
                                "question_type_g" = "Type G",
                                "questioner_info" = "Host/ Repeat / Speaker",
                                "notes_sheet_question" = "Notes sheet",
                                "notes_digitize_question" = "Notes digitizing",
                                "observer_talk" = "Observer")

#### Clean numeric/factor data ####

#check if the columns that should be numeric are indeed

#audience numbers -> numeric
summary(as.factor(all_data$audience_total))
summary(as.factor(all_data$audience_men))

all_data$notes_sheet_talk[which(all_data$audience_total == "123 (+/-)")] <- paste0(all_data$notes_sheet_talk[which(all_data$audience_total == "123 (+/-)")], ", total audience size approx")
all_data$audience_total[which(all_data$audience_total == "123 (+/-)")] <- "123"

all_data$notes_sheet_talk[which(all_data$audience_men == "32 (+/-)")] <- paste0(all_data$notes_sheet_talk[which(all_data$audience_men == "32 (+/-)")], ", men in audience is approx")
all_data$audience_men[which(all_data$audience_men == "32 (+/-)")] <- "32"

all_data$notes_sheet_talk[which(all_data$audience_men == "50 (+/-)")] <- paste0(all_data$notes_sheet_talk[which(all_data$audience_men == "50 (+/-)")], ", men in audience is approx")
all_data$audience_men[which(all_data$audience_men == "50 (+/-)")] <- "50"

all_data$notes_sheet_talk[which(all_data$audience_men == "3 or 4")] <- paste0(all_data$notes_sheet_talk[which(all_data$audience_men == "3 or 4")], ", men in audience 3 or 4")
all_data$audience_men[which(all_data$audience_men == "3 or 4")] <- "3"

#age class
summary(as.factor(all_data$host_1_age))
summary(as.factor(all_data$host_2_age))
summary(as.factor(all_data$host_3_age))
summary(as.factor(all_data$speaker_age))
summary(as.factor(all_data$questioner_age))

all_data$notes_sheet_talk[which(all_data$questioner_age == "1 or 2")] <- paste0(all_data$notes_sheet_talk[which(all_data$questioner_age == "1 or 2")], ", questioner age 1 or 2")
all_data$questioner_age[which(all_data$questioner_age == "1 or 2")] <- "1"

all_data$notes_sheet_talk[which(all_data$questioner_age == "1?")] <- paste0(all_data$notes_sheet_talk[which(all_data$questioner_age == "1?")], ", uncertainty questioner age")
all_data$questioner_age[which(all_data$questioner_age == "1?")] <- "1"

all_data$notes_sheet_talk[which(all_data$questioner_age == "2?")] <- paste0(all_data$notes_sheet_talk[which(all_data$questioner_age == "2?")], ", uncertainty questioner age")
all_data$questioner_age[which(all_data$questioner_age == "2?")] <- "2"

all_data$notes_sheet_talk[which(all_data$questioner_age == "not sure, 1 or 2")] <- paste0(all_data$notes_sheet_talk[which(all_data$questioner_age == "not sure, 1 or 2")], ", uncertainty questioner age, 1 or 2")
all_data$questioner_age[which(all_data$questioner_age == "not sure, 1 or 2")] <- "1"

#career stage
summary(as.factor(all_data$host_1_career))
summary(as.factor(all_data$host_2_career))
summary(as.factor(all_data$host_3_career))
summary(as.factor(all_data$speaker_career))

all_data$host_1_career <- gsub("Post docterate", "Post doctorate", all_data$host_1_career)
all_data$host_1_career <- gsub("Postdoctoral Researcher", "Post doctorate", all_data$host_1_career)

all_data$host_2_career <- gsub("PhD$", "PhD student", all_data$host_2_career)
all_data$host_2_career <- gsub("Postdoc", "Post doctorate", all_data$host_2_career)

all_data$host_3_career <- gsub("PhD$", "PhD student", all_data$host_3_career)

all_data$speaker_career <- gsub("associate professor", "Associate professor", all_data$speaker_career)
all_data$speaker_career <- gsub("Affiliated Scientist", "Researcher", all_data$speaker_career)
all_data$speaker_career <- gsub("Associate Professor", "Associate professor", all_data$speaker_career)
all_data$speaker_career <- gsub("Associative professor", "Associate professor", all_data$speaker_career)
all_data$speaker_career <- gsub("Buisness", "Non-academia", all_data$speaker_career)
all_data$speaker_career <- gsub("Companion animal behaviourist", NA, all_data$speaker_career)
all_data$speaker_career <- gsub("Doctor", "Post doctorate", all_data$speaker_career)
all_data$speaker_career <- gsub("lecturer", "Lecturer", all_data$speaker_career)
all_data$speaker_career <- gsub("PhD$", "PhD student", all_data$speaker_career)
all_data$speaker_career <- gsub("PhD Students", "PhD student", all_data$speaker_career)
all_data$speaker_career <- gsub("Post docotorate", "Post doctorate", all_data$speaker_career)
all_data$speaker_career <- gsub("Post docotrate", "Post doctorate", all_data$speaker_career)
all_data$speaker_career <- gsub("Post docterate", "Post doctorate", all_data$speaker_career)
all_data$speaker_career <- gsub("post doctorate", "Post doctorate", all_data$speaker_career)
all_data$speaker_career <- gsub("Postdoctoral Research Associate", "Post doctorate", all_data$speaker_career)
all_data$speaker_career <- gsub("professor", "Professor", all_data$speaker_career)
all_data$speaker_career <- gsub("Research associate", "Researcher", all_data$speaker_career)
all_data$speaker_career <- gsub("researcher", "Researcher", all_data$speaker_career)
all_data$speaker_career <- gsub("Researcher \\(PhD\\)", "PhD student", all_data$speaker_career)
all_data$speaker_career <- gsub("Students  \\(BS., MSc.\\)", "Students (Bsc, MSc)", all_data$speaker_career)
all_data$speaker_career <- gsub("Students \\(BSc,MSc\\)", "Students (Bsc, MSc)", all_data$speaker_career)
all_data$speaker_career <- gsub("Students \\(BSc, MSc\\)", "Students (Bsc, MSc)", all_data$speaker_career)
all_data$speaker_career <- gsub("veterenary", "Non-academic", all_data$speaker_career)

all_data$notes_sheet_talk[which(all_data$speaker_career == "not sure about the current carreer stage- post doctorate?")] <- paste0(all_data$notes_sheet_talk[which(all_data$speaker_career == "not sure about the current carreer stage- post doctorate?")], ", uncertainty about questioner career stage")
all_data$speaker_career[which(all_data$speaker_career == "not sure about the current carreer stage- post doctorate?")] <- "Post doctorate"

all_data$notes_sheet_talk[which(all_data$speaker_career == "not sure about the current carreer stage- Post doctorate?")] <- paste0(all_data$notes_sheet_talk[which(all_data$speaker_career == "not sure about the current carreer stage- post doctorate?")], ", uncertainty about questioner career stage")
all_data$speaker_career[which(all_data$speaker_career == "not sure about the current carreer stage- Post doctorate?")] <- "Post doctorate"

## gender
summary(as.factor(all_data$host_1_gender)) # seems high but just due to one X gender host with 34 questions
summary(as.factor(all_data$host_2_gender))
summary(as.factor(all_data$host_3_gender))
summary(as.factor(all_data$speaker_gender_infer))
summary(as.factor(all_data$questioner_gender))

all_data$questioner_gender <- gsub("f", "F", all_data$questioner_gender)

all_data$notes_sheet_talk[which(all_data$speaker_gender_infer == "F, F")] <- paste0(all_data$notes_sheet_talk[which(all_data$speaker_gender_infer == "F, F")], ", both speakers F")
all_data$speaker_gender_infer[which(all_data$speaker_gender_infer == "F, F")] <- "F"

all_data$notes_sheet_talk[which(all_data$speaker_gender_infer == "X^2")] <- paste0(all_data$notes_sheet_talk[which(all_data$speaker_gender_infer == "X^2")], ", X^2 gender speaker")
all_data$speaker_gender_infer[which(all_data$speaker_gender_infer == "X^2")] <- "X"

#pronouns
summary(as.factor(all_data$host_1_pronoun))
summary(as.factor(all_data$host_2_pronoun))
summary(as.factor(all_data$host_3_pronoun))
summary(as.factor(all_data$speaker_pronoun))

all_data$host_1_pronoun <- gsub("N", NA, all_data$host_1_pronoun)
all_data$host_2_pronoun <- gsub("N", NA, all_data$host_2_pronoun)
all_data$host_2_pronoun <- gsub("N", NA, all_data$host_2_pronoun)

all_data$speaker_pronoun <- gsub("he/him", "He/him", all_data$speaker_pronoun)
all_data$speaker_pronoun <- gsub("N", NA, all_data$speaker_pronoun)
all_data$speaker_pronoun <- gsub("No answer", NA, all_data$speaker_pronoun)
all_data$speaker_pronoun <- gsub("not confirmed", NA, all_data$speaker_pronoun)
all_data$speaker_pronoun <- gsub("she/her", "She/her", all_data$speaker_pronoun)
all_data$speaker_pronoun <- gsub("She/Her", "She/her", all_data$speaker_pronoun)
all_data$speaker_pronoun <- gsub("she/them", "She/them", all_data$speaker_pronoun)
all_data$speaker_pronoun <- gsub("they/them", "They/them", all_data$speaker_pronoun)
all_data$speaker_pronoun <- gsub("Y", "She/her", all_data$speaker_pronoun)
all_data$speaker_pronoun <- gsub("They/any", "They/them", all_data$speaker_pronoun)

#caro misunderstood the pronoun columns, recode:
#speaker
all_data$speaker_pronoun[which(all_data$speaker_pronoun == "Y" & all_data$how_speaker_pronoun == "Registration" & all_data$speaker_gender_infer == "F")] <- "She/her"
all_data$speaker_pronoun[which(all_data$speaker_pronoun == "Y" & all_data$how_speaker_pronoun == "Registration" & all_data$speaker_gender_infer == "M")] <- "He/him"

all_data$speaker_pronoun[which(all_data$speaker_pronoun == "Y" & all_data$how_speaker_pronoun == "Joe knows speaker" & all_data$speaker_gender_infer == "F")] <- "She/her"
all_data$speaker_pronoun[which(all_data$speaker_pronoun == "Y" & all_data$how_speaker_pronoun == "Joe knows speaker" & all_data$speaker_gender_infer == "M")] <- "He/him"

all_data$speaker_pronoun[which(all_data$speaker_pronoun == "Y" & all_data$how_speaker_pronoun == "Twitter" & all_data$speaker_gender_infer == "F")] <- "She/her"
all_data$speaker_pronoun[which(all_data$speaker_pronoun == "Y" & all_data$how_speaker_pronoun == "Twitter" & all_data$speaker_gender_infer == "M")] <- "He/him"

all_data$speaker_pronoun[which(all_data$speaker_pronoun == "Y" & is.na(all_data$how_speaker_pronoun) & all_data$speaker_gender_infer == "F")] <- NA
all_data$speaker_pronoun[which(all_data$speaker_pronoun == "Y" & is.na(all_data$how_speaker_pronoun) & all_data$speaker_gender_infer == "M")] <- NA

#keep the ones from registration, twitter and personal com as confirmed, rest NA

#hosts
all_data$host_1_pronoun[which(all_data$host_1_pronoun == "Y" & all_data$host_1_gender == "F")] <- "She/her"
all_data$host_1_pronoun[which(all_data$host_1_pronoun == "Y" & all_data$host_1_gender == "M")] <- "He/him"
all_data$host_1_pronoun[which(all_data$host_1_pronoun == "Y" & all_data$host_1_gender == "X")] <- "They/them" #only one person

all_data$host_2_pronoun[which(all_data$host_2_pronoun == "Y" & all_data$host_2_gender == "F")] <- "She/her"
all_data$host_2_pronoun[which(all_data$host_2_pronoun == "Y" & all_data$host_2_gender == "M")] <- "He/him"

all_data$host_3_pronoun[which(all_data$host_3_pronoun == "Y" & all_data$host_3_gender == "F")] <- "She/her"
all_data$host_3_pronoun[which(all_data$host_3_pronoun == "Y" & all_data$host_3_gender == "M")] <- "He/him"


#### Manual corrections #####
#### 1 -> correct anything
#### 2 -> add info about alternating hosts
#### 3 -> add uncertainty column
all_data$alternating_hosts <- NA
all_data$uncertainty_gender_host <- NA
all_data$uncertainty_gender_speaker <- NA
all_data$uncertainty_gender_questioner <- NA
all_data$uncertainty_count_audience <- NA
all_data$uncertainty_count_hands <- NA
#don't include age uncertainty as probably uncertain anyway

#### Add some columns ####
# add lecture room!
rooms <- read_excel("data/Sessions_information.xlsx") %>% select(c(`Session ID`, `Lecture hall`, `Room size`)) %>% unique()
rooms$`Session ID` <- as.character(rooms$`Session ID`)
rooms <- subset(rooms, !is.na(`Room size`))
all_data <- left_join(all_data, rooms, by = c("session_id" = "Session ID"))
all_data <- all_data %>% rename(lecture_hall = `Lecture hall`)
all_data <- all_data %>% rename(room_size = `Room size`)

### Women in audience 

#change data class
all_data$audience_total <- as.numeric(all_data$audience_total)
all_data$audience_men <- as.numeric(all_data$audience_men)

all_data$audience_women = all_data$audience_total - all_data$audience_men

all_data$sample_id <- paste0(all_data$session_id, "_",
                             all_data$talk_nr, "_",
                             all_data$question_nr, "_",
                             all_data$observer_session)

all_data <- all_data %>% relocate(sample_id, .before=transcriber)

### duration QA
all_data$duration_qa <- as.integer(all_data$end_qa - all_data$start_qa)/60
all_data %>% select(start_qa, end_qa, duration_qa) %>% View()

all_data <- unique(all_data)

### question type cleaning ####
# only edit in all_data, not in treatment too as not necessary

# first: change NA to 0 if it was not ticked vs NA = none was filled out
all_data$question_type_a <- as.numeric(all_data$question_type_a)
all_data$question_type_b <- as.numeric(all_data$question_type_b)

all_data <- all_data %>% 
  mutate(no_q_type = rowSums(.[grep("question_type", names(.))], na.rm = TRUE))

# change question types to 0 if no_q_type is >0, to NA if no_q_type == 0

all_data <- all_data %>% mutate(
  question_type_a = case_when(
    is.na(question_type_a) & no_q_type > 0 ~ 0,
    is.na(question_type_a) & no_q_type == 0 ~ NA,
    !is.na(question_type_a) ~ 1
  ),
  question_type_b = case_when(
    is.na(question_type_b) & no_q_type > 0 ~ 0,
    is.na(question_type_b) & no_q_type == 0 ~ NA,
    !is.na(question_type_b) ~ 1
  ),
  question_type_c = case_when(
    is.na(question_type_c) & no_q_type > 0 ~ 0,
    is.na(question_type_c) & no_q_type == 0 ~ NA,
    !is.na(question_type_c) ~ 1
  ),
  question_type_d = case_when(
    is.na(question_type_d) & no_q_type > 0 ~ 0,
    is.na(question_type_d) & no_q_type == 0 ~ NA,
    !is.na(question_type_d) ~ 1
  ),
  question_type_e = case_when(
    is.na(question_type_e) & no_q_type > 0 ~ 0,
    is.na(question_type_e) & no_q_type == 0 ~ NA,
    !is.na(question_type_e) ~ 1
  ),
  question_type_f = case_when(
    is.na(question_type_f) & no_q_type > 0 ~ 0,
    is.na(question_type_f) & no_q_type == 0 ~ NA,
    !is.na(question_type_f) ~ 1
  ),
  question_type_g = case_when(
    is.na(question_type_g) & no_q_type > 0 ~ 0,
    is.na(question_type_g) & no_q_type == 0 ~ NA,
    !is.na(question_type_g) ~ 1
  ))


#### add treatment column ####
treatment <- read_excel("data/metadata/Treatments_by_Talk.xlsx")
treatment$`Session ID` <- as.character(treatment$`Session ID`)

## there were a few talks presented by 2 people, exclude them
all_data <- subset(all_data, !grepl("52_7_*", sample_id))
all_data <- subset(all_data, !grepl("52_8_*", sample_id))

all_data_treatment <- left_join(all_data, treatment[,c("Session ID", "Talk_Number", "Treatment", "Condition")],
                                by = c("session_id" = "Session ID", "talk_nr" = "Talk_Number"))

# correct a control session, due to change in schedule wrong no of talks
all_data_treatment$Treatment[which(all_data_treatment$session_id==34 & all_data_treatment$talk_nr == 6)] <- "Control"
all_data_treatment$Treatment[which(all_data_treatment$session_id==71 & all_data_treatment$talk_nr == 5)] <- "Treatment"
all_data_treatment$Condition[which(all_data_treatment$session_id==71 & all_data_treatment$talk_nr == 5)] <- NA

#### Save long df #####

plenary <- subset(all_data, grepl("P", session_id)& !is.na(talk_nr) & !is.na(question_nr))
all_data <- subset(all_data, !grepl("P", session_id) & !is.na(talk_nr) & !is.na(question_nr))

plenary_treatment <- subset(all_data_treatment, grepl("P", session_id) & !is.na(talk_nr) & !is.na(question_nr))
all_data_treatment <- subset(all_data_treatment, !grepl("P", session_id) & !is.na(talk_nr) & !is.na(question_nr))

save(all_data, file = "data/clean/question_asking/combined_session_talk_question_all_long.RData")
save(all_data_treatment, file = "data/clean/question_asking/combined_session_talk_question_all_long_withtreatment.RData")

save(plenary_treatment, file = "data/clean/question_asking/combined_session_talk_question_all_long_plenary.RData")
save(all_data_treatment, file = "data/clean/question_asking/combined_session_talk_question_all_long_withtreatment_plenary.RData")

### Make a wide df ###
# where the inter-observer reliability is spread out over columns: observer 1 data is on same line
# as observer 2 data
# can't do with regular spread function as there's too many columns, need to have their own
# suffix
load(file = "data/clean/question_asking/combined_session_talk_question_all_long_withtreatment.RData")

data_wide_a <- left_join(subset(all_data_treatment, observer_talk == "Observer 1"),
                       subset(all_data_treatment, observer_talk == "Observer 2"),
                       by = c("session_id", "talk_nr", "question_nr"), 
                       suffix = c("_observer1", "_observer2"))

data_wide_b <- left_join(subset(all_data_treatment, observer_talk == "Observer 3"),
                         subset(all_data_treatment, observer_talk == "Observer 4"),
                         by = c("session_id", "talk_nr", "question_nr"), 
                         suffix = c("_observer3", "_observer4"))

data_wide <- left_join(data_wide_a, data_wide_b, by = c("session_id", "talk_nr", "question_nr"))
data_wide <- data_wide %>% relocate(session_id, .before=transcriber_observer1)

### Women hands raised
data_wide <- left_join(data_wide, unique(sessions[,c("Session ID", "Room size")]),
                       by = c("session_id" = "Session ID")) 

names(data_wide)[ncol(data_wide)] <- "room_size"

# data_wide %>% select(c("session_id", "talk_nr", "question_nr", "room_size", 
#                        "hands_total_observer1", "hands_total_observer2", "hands_total_observer3",
#                        "hands_total_observer4",
#                        "hands_men_observer1", "hands_men_observer2", "hands_men_observer3",
#                        "hands_men_observer4")) %>% View()

## needs to be calculated differently for large vs medium and small rooms
data_wide_large <- subset(data_wide, room_size == "Large")
data_wide_med_sm <- subset(data_wide, room_size != "Large")

# first for large rooms: either total or men hands raised
# was noted by an observer, so first need to make a column for which one
# it actually was, and then calculate women hands raised

data_wide_large$hands_total_observer1 <- as.numeric(data_wide_large$hands_total_observer1)
data_wide_large$hands_total_observer2 <- as.numeric(data_wide_large$hands_total_observer2)
data_wide_large$hands_total_observer3 <- as.numeric(data_wide_large$hands_total_observer3) #? to NA
data_wide_large$hands_total_observer4 <- as.numeric(data_wide_large$hands_total_observer4)

data_wide_large$hands_men_observer1 <- as.numeric(data_wide_large$hands_men_observer1)
data_wide_large$hands_men_observer2 <- as.numeric(data_wide_large$hands_men_observer2)
data_wide_large$hands_men_observer3 <- as.numeric(data_wide_large$hands_men_observer3)
data_wide_large$hands_men_observer4 <- as.numeric(data_wide_large$hands_men_observer4)

data_wide_large <- data_wide_large %>% rowwise()%>%
  mutate(
    hands_total_observer12 = as.numeric(case_when(
      !is.na(hands_total_observer1) & is.na(hands_total_observer2) ~ hands_total_observer1,
      !is.na(hands_total_observer2) & is.na(hands_total_observer1) ~ hands_total_observer2,
      !is.na(hands_total_observer2) & !is.na(hands_total_observer1) ~ max(hands_total_observer1, hands_total_observer2),
      TRUE ~ NA)),
    hands_men_observer12 = as.numeric(case_when(
      !is.na(hands_men_observer1) & is.na(hands_men_observer2)~ hands_men_observer1,
      !is.na(hands_men_observer2) & is.na(hands_men_observer1)~ hands_men_observer2,
      !is.na(hands_men_observer1) & !is.na(hands_men_observer2) ~ max(hands_men_observer1, hands_men_observer2),
      TRUE ~ NA)),
    hands_total_observer34 = as.numeric(case_when(
      !is.na(hands_total_observer3) & is.na(hands_total_observer4)~ hands_total_observer3,
      !is.na(hands_total_observer4) & is.na(hands_total_observer3)~ hands_total_observer4,
      !is.na(hands_total_observer3) & !is.na(hands_total_observer4) ~ max(hands_total_observer3,hands_total_observer4),
      TRUE ~ NA)),
    hands_men_observer34 = as.numeric(case_when(
      !is.na(hands_men_observer3) & is.na(hands_men_observer4)~ hands_men_observer3,
      !is.na(hands_men_observer4) & is.na(hands_men_observer3)~ hands_men_observer4,
      !is.na(hands_men_observer3) & !is.na(hands_men_observer4) ~ max(hands_men_observer3,hands_men_observer4),
      TRUE ~ NA)))

data_wide_large$hands_women_observer12 <- data_wide_large$hands_total_observer12 - data_wide_large$hands_men_observer12
data_wide_large$hands_women_observer34 <- data_wide_large$hands_total_observer34 - data_wide_large$hands_men_observer34

#make empty columns to merge later
data_wide_large$hands_women_observer1 <- NA
data_wide_large$hands_women_observer2 <- NA
data_wide_large$hands_women_observer3 <- NA
data_wide_large$hands_women_observer4 <- NA

# then for other rooms: simply subtract
#first make empty columns for merge
data_wide_med_sm$hands_total_observer12 <- NA
data_wide_med_sm$hands_total_observer34 <- NA
data_wide_med_sm$hands_men_observer12 <- NA
data_wide_med_sm$hands_men_observer34 <- NA
data_wide_med_sm$hands_women_observer12 <- NA
data_wide_med_sm$hands_women_observer34 <- NA

data_wide_med_sm$hands_women_observer1 <- as.numeric(data_wide_med_sm$hands_total_observer1) - as.numeric(data_wide_med_sm$hands_men_observer1)
data_wide_med_sm$hands_women_observer2 <- as.numeric(data_wide_med_sm$hands_total_observer2) - as.numeric(data_wide_med_sm$hands_men_observer2)
data_wide_med_sm$hands_women_observer3 <- as.numeric(data_wide_med_sm$hands_total_observer3) - as.numeric(data_wide_med_sm$hands_men_observer3)
data_wide_med_sm$hands_women_observer4 <- as.numeric(data_wide_med_sm$hands_total_observer4) - as.numeric(data_wide_med_sm$hands_men_observer4)

## merge large and other rooms
data_wide_clean <- rbind(data_wide_large, data_wide_med_sm)
data_wide_clean %>% select(c("hands_total_observer1", "hands_total_observer2", "hands_total_observer12",
                             "hands_men_observer1", "hands_men_observer2","hands_men_observer12",
                             "hands_women_observer1", "hands_women_observer2","hands_women_observer12")) %>% View()

save(data_wide_clean, file = "data/clean/question_asking/combined_session_talk_question_all_treatment_wide.RData")

