##### Figures 

# packages
pacman::p_load(tidyverse, data.table, cowplot)

source("scripts/EDI_ggplot_theme.R")

col_sig <- clrs[c(10,2)] %>%
  color() %>% 
  set_names(nm = c("sig", "nonsig"))

#### Figure 2: question asking ####

##### Figure 2a: behaviour gender disparity #####
general <- read.csv("results/question-asking/model_QA_justIC.csv")
hands <- read.csv("results/question-asking/model_QA_hands.csv")
chosen <- read.csv("results/question-asking/model_QA_chosen.csv")

out_how <- rbind(general, hands, chosen)
out_how$name <- c("Asking questions", "Raising hands", "Getting chosen") 
out_how$name <- factor(out_how$name , levels = c("Getting chosen", "Raising hands", "Asking questions"))
out_how <- out_how %>% mutate(sig = case_when(intercept_pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

ggplot(out_how) +
  geom_point(aes(x = intercept_estimate, y = name, col = sig), size = 6) + 
  geom_segment(aes(x = intercept_ci_lower, xend = intercept_ci_higher, y = name),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Intercept and 95% CI", y = "Model") + 
  xlim(-2.5, 2.5) +
  scale_color_manual(values = col_sig) +
  annotate("text", x = -1, y = 3.5, label = "Male bias", size = 6)+
  annotate("text",label = "Female bias", y = 3.5, x = 1, size = 6)+
  annotate("segment", x = -0.2, xend = -2.3, y = 3.3,linewidth=1,
           arrow = arrow(length=unit(0.2, "cm")))+
  annotate("segment", x = 0.2, xend = 2.3, y = 3.3,linewidth=1,
           arrow = arrow(length=unit(0.2, "cm")))+
  theme(legend.position = "none") -> plot_2a

plot_2a

##### Figure 2b: survey gender disparity #####

m_survey_out <- read.csv("results/question-asking/model_qa_survey_general.csv")
m_survey_out$name <- c("Asked a question") 

m_survey_out_long <- data.frame(factor = c("Female", "Non-binary"),
                                estimate = c(m_survey_out$est_genderFemale, m_survey_out$`est_genderNon-binary`),
                                lower = c(m_survey_out$lowerCI_genderFemale, m_survey_out$`lowerCI_genderNon-binary`),
                                upper = c(m_survey_out$higherCI_genderFemale, m_survey_out$`higherCI_genderNon-binary`),
                                pval = c(m_survey_out$pval_genderFemale, m_survey_out$pval_genderNon.binary))

m_survey_out_long <- m_survey_out_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

ggplot(m_survey_out_long) +
  geom_point(aes(x = estimate, y = factor, col = sig), size = 6) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Gender") + 
  xlim(-2, 1.5) +
  scale_color_manual(values = col_sig) +
  annotate("text", 2.5, x = -1, label = "Male bias", size = 6)+
  annotate("segment", -0.3, xend = -1.8, y = 2.3, linewidth=1,
           arrow = arrow(length=unit(0.2, "cm")))+
 theme(legend.position = "none") -> plot_2b

plot_2b

##### Figure 2c + 2d: probabilities #####
# load data
load("data/question_asking/question_asking_data_condensed_for_analysis.RData")

data_control <- subset(data_analysis, treatment == "Control" &
                         is.na(followup) & is.na(jumper)&
                         is.na(host_asks) & !is.na(audience_women_prop) & 
                         !grepl("speaker|questioner", allocator_question))

data_control <- droplevels(data_control)

data_hands <- subset(data_control, hands_prop_men < 1 & hands_prop_women > 0 &
                       hands_prop_women < 1)

##### Figure 2c: probability raising hands ####

#### raising hand
hand_effect <- out_how[2,"intercept_estimate"]

# Prepare the data
data_control$audience_women_prop_logit <- jitter(boot::logit(data_control$audience_women_prop), 0.5)
data_control$jittered_hands <- jitter(data_control$hands_prop_women, 0.5)

# Add the null model line
null_model_hands <- function(x) {
  1 / (1 + exp(-(0 + 1 * x)))}

actual_model_hands <- function(x) {
  1 / (1 + exp(-(hand_effect + 1 * x)))}

# Create the base plot
ggplot(data_control, aes(x = audience_women_prop_logit, y = jittered_hands)) +
  stat_function(fun = null_model_hands, geom = "area", fill = "grey90", alpha = 0.5) +
  geom_point(alpha = 0.4, size = 2.5) +
  labs(x = "Logit(proportion female audience)",
       y = "Probability of a woman raising their hand") +
  xlim(-1.5, 2.5) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     limits=c(0, 1))+
  stat_function(fun = null_model_hands, color = clrs[1], linetype = "dashed") +
 
  annotate("text", x = -1.1, y = 0.45, label = "null
hypothesis", color = clrs[1], angle = 0, size = 6)+
  # Add the actual data model line
  stat_function(fun = actual_model_hands, color = clrs[9], linewidth=1) +
  annotate("text", x = -0.6, y = 0.1, label = "emperical
data", color = clrs[9], angle = 0, size = 6) + 
  # Add text annotations for biases
  annotate("text", x = -0.8, y = 0.8, label = "bias towards 
 females",  color = "black", size = 6) +
  annotate("text", x = 1.8, y = 0.15, label = "bias towards 
males",  color = "black", size = 6)+
  theme(plot.margin=margin(2,1,1,1,"cm")) -> plot_2c
plot_2c

##### Figure 2d: probability getting chosen ####
chosen_effect <- out_how[3,"intercept_estimate"]

# Prepare the data
data_hands$logit_hands_prop_women <- jitter(boot::logit(data_hands$hands_prop_women), 0.5)
data_hands$jittered_gender <- jitter(data_hands$gender_questioner_female, 0.5)

# Add the null model line
null_model_chosen <- function(x) {
  1 / (1 + exp(-(0 + 1 * x)))}

actual_model_chosen <- function(x) {
  1 / (1 + exp(-(chosen_effect + 1 * x)))}

# Create the base plot
ggplot(data_hands, aes(x = logit_hands_prop_women, y = jittered_gender)) +
  stat_function(fun = null_model_chosen, geom = "area", fill = "grey90", alpha = 0.5) +
  geom_point(alpha = 0.4, size = 2.5) +
  labs(x = "Logit(proportion female hands)",
       y = "Probability of a woman getting chosen") +
  xlim(-1.5, 2.5) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     limits=c(0, 1))+

 stat_function(fun = null_model_chosen, color = clrs[1], linetype = "dashed") +
  annotate("text", x = -1.1, y = 0.45, label = "null 
hypothesis",  color = clrs[1], angle = 0, size = 6)+
  # Add the actual data model line
  stat_function(fun = actual_model_chosen, color = clrs[9], linewidth=1) +
  annotate("text", x = -0.4, y = 0.2, label = "emperical 
data",  color = clrs[9], angle = 0, size = 6) + 
  # Add text annotations for biases
  annotate("text", x = -0.8, y = 0.75, label = "bias towards 
 females",  color = "black", size = 6) +
  annotate("text", x = 1.8, y = 0.15, label = "bias towards 
males",  color = "black", size = 6) +
  theme(plot.margin=margin(2,1,1,1,"cm"))-> plot_2d
plot_2d

##### Figure 2: combine #####

cowplot::plot_grid(plot_2a,
                   plot_2b,
                   plot_2c,
                   plot_2d,
                   ncol = 2,
                   align = "hv", axis="lb",
                   labels = c("a) Behavioural data", "b) Survey data", 
                              "c) Raising hands", "d) Getting chosen"),
                   label_fontface = "plain", label_size = 22) -> qa

ggsave(qa, file="plots/main/fig2_questionasking_behaviour.png", 
       width=16, height=12)


### Figure 3: motivations and hesitations ####
##### Figure 3a: motivations gender effects #####

conditions_survey_motivation_gender <- read.csv("results/question-asking/question_asking_why_survey_result.csv")
conditions_survey_motivation <- read.csv("results/question-asking/model_qa_conditions_motivations_gender_effect.csv")

conditions_survey_motivation_gender <- subset(conditions_survey_motivation_gender, grepl("reason_question",response))
conditions_survey_motivation_gender <- conditions_survey_motivation_gender %>% 
  mutate(sig = case_when(lrt_qval < 0.05 ~ "sig", TRUE ~ "nonsig"))

conditions_survey_motivation_gender$model <- c("Interest in topic",
                                               "Making voice heard",
                                               "Appreciate work",
                                               "Deeper understanding",
                                               "Relevance own research")

ggplot(conditions_survey_motivation_gender) +
  geom_point(aes(x = coef_female, y = model, col = sig), size = 6) + 
  geom_segment(aes(x = lower_female, xend = upper_female, y = model),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate for women and 95% CI", y="Response") + 
  scale_y_discrete(labels = scales::label_wrap(20)) +
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> plot_3a

plot_3a

##### Figure 3b: probability ask question motivations #####

# probability to ask question
conditions_survey_motivation <- subset(conditions_survey_motivation, grepl("reason_question",predictor))

conditions_survey_motivation <- conditions_survey_motivation %>% 
  mutate(sig = case_when(lrt_qval < 0.05 ~ "sig", TRUE ~ "nonsig"))

conditions_survey_motivation$model <- c("Interest in topic",
                                        "Making voice heard",
                                        "Appreciate work",
                                        "Deeper understanding",
                                        "Relevance own research")

ggplot(conditions_survey_motivation) +
  geom_point(aes(x = coef_reason, y = model, col = sig), size = 6) + 
  geom_segment(aes(x = lower_reason, xend = upper_reason, y = model),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y="Motivation") + 
  scale_y_discrete(labels = scales::label_wrap(20)) +
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> plot_3b

plot_3b

##### Figure 3c: hesitations gender effects #####

conditions_survey_hesitation_gender <- read.csv("results/question-asking/question_asking_why_survey_result.csv")
conditions_survey_hesitation <- read.csv("results/question-asking/model_qa_conditions_motivations_gender_effect.csv")

conditions_survey_hesitation_gender <- subset(conditions_survey_hesitation_gender, grepl("reason_noquestion",response))
conditions_survey_hesitation_gender <- conditions_survey_hesitation_gender %>% 
  mutate(sig = case_when(lrt_qval < 0.05 ~ "sig", TRUE ~ "nonsig"))

conditions_survey_hesitation_gender$model <- c("Not clever",
                                               "Misunderstand",
                                               "Intimidated audience",
                                               "Intimidated setting",
                                               "Irrelevance / unimportant",
                                               "Phrasing",
                                               "No time",
                                               "Too introverted",
                                               "Rather in private",
                                               "No confidence")

ggplot(conditions_survey_hesitation_gender) +
  geom_point(aes(x = coef_female, y = model, col = sig), size = 6) + 
  geom_segment(aes(x = lower_female, xend = upper_female, y = model),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate for women and 95% CI", y="Response") + 
  scale_y_discrete(labels = scales::label_wrap(20)) +
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> plot_3c

plot_3c

##### Figure 3d: probability ask question hesitations #####

conditions_survey_hesitation <- subset(conditions_survey_hesitation, grepl("reason_noquestion",predictor))
conditions_survey_hesitation <- conditions_survey_hesitation %>% 
  mutate(sig = case_when(lrt_qval < 0.05 ~ "sig", TRUE ~ "nonsig"))

conditions_survey_hesitation$model <- c("Not clever",
                                        "Misunderstand",
                                        "Intimidated audience",
                                        "Intimidated setting",
                                        "Irrelevance / unimportant",
                                        "Phrasing",
                                        "No time",
                                        "Too introverted",
                                        "Rather in private",
                                        "No confidence")

ggplot(conditions_survey_hesitation) +
  geom_point(aes(x = coef_reason, y = model, col = sig), size = 6) + 
  geom_segment(aes(x = lower_reason, xend = upper_reason, y = model),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y="Hesitation") + 
  scale_color_manual(values = col_sig) +
  scale_y_discrete(labels = scales::label_wrap(20)) +
  theme(legend.position = "none") -> plot_3d

plot_3d

##### Figure 4: combine #####

cowplot::plot_grid(plot_3a, plot_3b, plot_3c, plot_3d, 
                   rel_heights = c(0.7,1),
                   ncol = 2, labels=c("a) Gender effect on motivations", 
                                      "b) Motivation effects on question asking", 
                                      "c) Gender effect on hesitations", 
                                      "d) Hesitation effects on question asking"),
                   label_fontface = "plain", label_size = 22,
                   align = "hv", axis = "lb") -> plot_3

ggsave(plot_3, file = "plots/main/fig3_reasons_question_asking.png", 
       width=18, height=14)

### Figure 4: conditions, motivations and hesitations ####

##### Figure 4a: conditions behavioral #####

room <- read.csv("results/question-asking/model_qa_room_size.csv")
audience_size <- read.csv("results/question-asking/model_qa_audience_size.csv")
gender_audience <- read.csv("results/question-asking/model_qa_audience_gender.csv")
gender_speaker <- read.csv("results/question-asking/model_qa_speaker_gender.csv")
gender_host <- read.csv("results/question-asking/model_qa_host_gender.csv")

comfort_qa_reasons_long <- data.frame(factor = c("Female audience proportion", 
                                                 "Female speaker", 
                                                 "Female host", 
                                                 "Audience size",
                                                 "Small room", ""),
                                      estimate = c(gender_audience$est_audience_women_prop,
                                                   gender_speaker$est_speaker_genderF, 
                                                   gender_host$est_host_1_genderF,
                                                   audience_size$est_audience_total,
                                                   room$est_room_sizeSmall, NA),
                                      lower = c(gender_audience$lowerCI_audience_women_prop,
                                                gender_speaker$lowerCI_speaker_genderF, 
                                                gender_host$lowerCI_host_1_genderF,
                                                audience_size$lowerCI_audience_total,
                                                room$lowerCI_room_sizeSmall, NA),
                                      upper = c(gender_audience$higherCI_audience_women_prop,
                                                gender_speaker$higherCI_speaker_genderF, 
                                                gender_host$higherCI_host_1_genderF,
                                                audience_size$higherCI_audience_total,
                                                room$higherCI_room_sizeSmall, NA),
                                      pval = c(gender_audience$pval_audience_women_prop,
                                               gender_speaker$pval_speaker_genderF, 
                                               gender_host$pval_host_1_genderF,
                                               audience_size$pval_audience_total,
                                               room$pval_room_sizeSmall, NA),
                                      lrt_pval = c(gender_audience$lrt_pval,
                                                   gender_speaker$lrt_pval, 
                                                   gender_host$lrt_pval,
                                                   audience_size$lrt_pval,
                                                   room$lrt_pval, NA))

comfort_qa_reasons_long <- comfort_qa_reasons_long %>% 
  mutate(sig = case_when(lrt_pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

comfort_qa_reasons_long$factor  <- factor(comfort_qa_reasons_long$factor,
                                          levels=c("Small room",
                                                   "Audience size",
                                                   "Female host", 
                                                   "Female speaker", 
                                                   "Female audience proportion", ""))



ggplot(comfort_qa_reasons_long) +
  geom_point(aes(x = estimate, y = factor, col = sig), size = 6) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y="Variable") + 
  scale_color_manual(values = col_sig) +
  scale_y_discrete(labels = scales::label_wrap(20)) +
  annotate(geom="text", label = "Male bias", y = 6.3, x = -2, size = 6)+
  annotate(geom="segment", x = -0.5, xend = -4, y = 6, linewidth=1,arrow = arrow(length=unit(0.2, "cm")))+
  theme(legend.position = "none",
        plot.margin=margin(1.5,1,1,1, "cm")) -> plot_4a

plot_4a

##### Figure 4b: conditions survey #####

conditions_survey_comfort <- read.csv("results/question-asking/question_asking_why_survey_result.csv")

conditions_survey_comfort <- subset(conditions_survey_comfort, grepl("comfort",response))
conditions_survey_comfort <- subset(conditions_survey_comfort, !grepl("know_speaker", response))
conditions_survey_comfort$model <- c("... speaker is of my own gender", "... audience is of my own gender", "... audience size is small", "... host is of my own gender")
conditions_survey_comfort <- conditions_survey_comfort %>% 
  mutate(sig = case_when(lrt_qval < 0.05 ~ "sig", TRUE ~ "nonsig"))

conditions_survey_comfort$model  <- factor(conditions_survey_comfort$model,
                                           levels=c("... audience size is small", "... host is of my own gender",
                                                    "... speaker is of my own gender", "... audience is of my own gender"))
ggplot(conditions_survey_comfort) +
  geom_point(aes(x = coef_female, y = model, col = sig), size = 6) + 
  geom_segment(aes(x = lower_female, xend = upper_female, y = model),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  xlim(-0.5, 2)+
  labs(x = "Estimate for women and 95% CI", y="Response", 
       subtitle = "I feel more comfortable asking 
questions if the...") + 
  scale_y_discrete(labels = scales::label_wrap(20)) +
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 16),
        plot.margin=margin(1.5,1,1,1, "cm")) -> plot_4b

plot_4b

##### Figure 4: combine #####

cowplot::plot_grid(plot_4a, plot_4b,
                   ncol = 2, labels=c("a) Behavioural data", "b) Survey data"),
                   label_fontface = "plain", label_size = 22,
                   align = "hv", axis = "lb") -> plot_4

ggsave(plot_4, file = "plots/main/fig4_conditions_question_asking.png", 
       width=14, height=6)

### Figure 5: experience ####

##### Figure 5a: heard #####

heard_final <- read.csv("results/survey/model_feeling_heard_final.csv")

heard_final_long <- data.frame(factor = c("English comfort", "Expertise"),
                               est = c(heard_final$est_english_comfort_rating,
                                       heard_final$est_expertise_rating),
                               lower = c(heard_final$lowerCI_english_comfort_rating,
                                         heard_final$lowerCI_expertise_rating),
                               upper = c(heard_final$higherCI_english_comfort_rating,
                                         heard_final$higherCI_expertise_rating),
                               pval = c(heard_final$pval_english_comfort_rating,
                                        heard_final$pval_expertise_rating))
heard_final_long <- heard_final_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

ggplot(heard_final_long) +
  geom_point(aes(x = est, y = factor, col = sig), size = 6) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  xlim(-0.5, 0.5)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> plot_5a

##### Figure 5b: yourself #####

yourself_final <- read.csv("results/survey/model_comf_yourself_final.csv")

yourself_final_long <- data.frame(factor = c("English comfort", "Expertise", "Gender - non-binary", "Gender - female"),
                                  est = c(yourself_final$est_english_comfort_rating,
                                          yourself_final$est_expertise_rating,
                                          yourself_final$est_genderNon.binary,
                                          yourself_final$est_genderFemale),
                                  lower = c(yourself_final$lowerCI_english_comfort_rating,
                                            yourself_final$lowerCI_expertise_rating,
                                            yourself_final$lowerCI_genderNon.binary,
                                            yourself_final$lowerCI_genderFemale),
                                  upper = c(yourself_final$higherCI_english_comfort_rating,
                                            yourself_final$higherCI_expertise_rating,
                                            yourself_final$higherCI_genderNon.binary,
                                            yourself_final$higherCI_genderFemale),
                                  pval = c(yourself_final$pval_english_comfort_rating,
                                           yourself_final$pval_expertise_rating,
                                           yourself_final$pval_genderNon.binary,
                                           yourself_final$pval_genderFemale))

yourself_final_long <- yourself_final_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

ggplot(yourself_final_long) +
  geom_point(aes(x = est, y = factor, col = sig), size = 6) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  xlim(-3.6, 1)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> plot_5b

##### Figure 5c: sense of belonging #####

belong_final <- read.csv("results/survey/model_belonging_final.csv")

belong_final_long <- data.frame(factor = c("Affiliation - Asia",
                                  #         "Affiliation - Africa", 
                                           "Affiliation - North America", 
                                           "Affiliation - Oceania",
                                   #        "Affiliation - South America",
                                           "English comfort", "Expertise"),
                                est = c(belong_final$est_affiliation_continentAsia,
                                   #     belong_final$est_affiliation_continentAfrica,
                                        belong_final$est_affiliation_continentNorth.America,
                                        belong_final$est_affiliation_continentOceania,
                                    #    belong_final$est_affiliation_continentSouth.America,                                  belong_final$est_english_comfort_rating,
                                    belong_final$est_english_comfort_rating,
                                    belong_final$est_expertise_rating),
                                lower = c(belong_final$lowerCI_affiliation_continentAsia,
                                    #      belong_final$lowerCI_affiliation_continentAfrica,
                                          belong_final$lowerCI_affiliation_continentNorth.America,
                                          belong_final$lowerCI_affiliation_continentOceania,
                                     #     belong_final$lowerCI_affiliation_continentSouth.America,                                  belong_final$lowerCI_english_comfort_rating,
                                     belong_final$lowerCI_english_comfort_rating,
                                     belong_final$lowerCI_expertise_rating),
                                upper = c(belong_final$higherCI_affiliation_continentAsia,
                                    #      belong_final$higherCI_affiliation_continentAfrica,
                                          belong_final$higherCI_affiliation_continentNorth.America,
                                          belong_final$higherCI_affiliation_continentOceania,
                                    #      belong_final$higherCI_affiliation_continentSouth.America,      
                                          belong_final$higherCI_english_comfort_rating,
                                          belong_final$higherCI_expertise_rating),
                                pval = c(belong_final$pval_affiliation_continentAsia,
                                    #     belong_final$pval_affiliation_continentAfrica,
                                         belong_final$pval_affiliation_continentNorth.America,
                                         belong_final$pval_affiliation_continentOceania,
                                     #    belong_final$pval_affiliation_continentSouth.America,
                                         belong_final$pval_english_comfort_rating,
                                         belong_final$pval_expertise_rating))

belong_final_long <- belong_final_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

ggplot(belong_final_long) +
  geom_point(aes(x = est, y = factor, col = sig), size = 6) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
#  xlim(-4, 3)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> plot_5c

##### Figure 5: combine #####
cowplot::plot_grid(plot_5a, plot_5b, plot_5c,  
                   ncol = 1, labels=c("a) Feeling heard", 
                                      "b) Comfortable being yourself", 
                                      "c) Attending increased feeling of belonging"),
                   label_fontface = "plain", label_size = 22,
                   hjust=c(-0.1,-0.06,-0.045),
                   rel_heights = c(0.8, 0.9, 1),
                   align = "hv", axis = "lb") -> plot_5

ggsave(plot_5, file = "plots/main/fig5_experience.png", 
       width=8, height=12)

### Figure 6: EDI perceptions ####

##### Figure 6a: diversity ####

diversity_final <- read.csv("results/survey/model_diversity_final.csv")

diversity_final_long <- data.frame(factor = c("Gender - female", "Gender - non binary",
                                              "LGBTQ+"),
                                   est = c(diversity_final$est_genderFemale,
                                           diversity_final$est_genderNon.binary,
                                           diversity_final$est_lgbtqYes),
                                   lower = c(diversity_final$lowerCI_genderFemale,
                                             diversity_final$lowerCI_genderNon.binary,
                                             diversity_final$lowerCI_lgbtqYes),
                                   upper = c(diversity_final$higherCI_genderFemale,
                                             diversity_final$higherCI_genderNon.binary,
                                             diversity_final$higherCI_lgbtqYes),
                                   pval = c(diversity_final$pval_genderFemale,
                                            diversity_final$pval_genderNon.binary,
                                            diversity_final$pval_lgbtqYes))

diversity_final_long <- diversity_final_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

diversity_final_long$factor <- factor(diversity_final_long$factor, 
                                      levels=c("LGBTQ+",  "Gender - non binary",
                                               "Gender - female"))
ggplot(diversity_final_long) +
  geom_point(aes(x = est, y = factor, col = sig), size = 6) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  # xlim(-1.5, 1.5)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> plot_6a

##### Figure 6b: EDI issues ####
edi_issues_final <- read.csv("results/survey/model_edi_issues_final.csv")

edi_issues_final_long <- data.frame(factor = c("Gender - female", "Gender - non binary",
                                               "LGBTQ+", "Expat",
                                               "Nationality - Asia", 
                                               "Nationality - North America",
                                               "Nationality - Oceania"
                                               #"Nationality - South America"
                                               ),
                                    est = c(edi_issues_final$est_genderFemale,
                                            edi_issues_final$est_genderNon.binary,
                                            edi_issues_final$est_lgbtqYes,
                                            edi_issues_final$est_expatExpat,
                                            edi_issues_final$est_nationality_continentAsia,
                                            edi_issues_final$est_nationality_continentNorth.America,
                                            edi_issues_final$est_nationality_continentOceania
                                            #edi_issues_final$est_nationality_continentSouth.America
                                            ),
                                    lower = c(edi_issues_final$lowerCI_genderFemale,
                                              edi_issues_final$lowerCI_genderNon.binary,
                                              edi_issues_final$lowerCI_lgbtqYes,
                                              edi_issues_final$lowerCI_expatExpat,
                                              edi_issues_final$lowerCI_nationality_continentAsia,
                                              edi_issues_final$lowerCI_nationality_continentNorth.America,
                                              edi_issues_final$lowerCI_nationality_continentOceania
                                              #edi_issues_final$lowerCI_nationality_continentSouth.America
                                              ),
                                    upper = c(edi_issues_final$higherCI_genderFemale,
                                              edi_issues_final$higherCI_genderNon.binary,
                                              edi_issues_final$higherCI_lgbtqYes,
                                              edi_issues_final$higherCI_expatExpat,
                                              edi_issues_final$higherCI_nationality_continentAsia,
                                              edi_issues_final$higherCI_nationality_continentNorth.America,
                                              edi_issues_final$higherCI_nationality_continentOceania
                                              #edi_issues_final$higherCI_nationality_continentSouth.America
                                              ),
                                    pval = c(edi_issues_final$pval_genderFemale,
                                             edi_issues_final$pval_genderNon.binary,
                                             edi_issues_final$pval_lgbtqYes,
                                             edi_issues_final$pval_expatExpat,
                                             edi_issues_final$pval_nationality_continentAsia,
                                             edi_issues_final$pval_nationality_continentNorth.America,
                                             edi_issues_final$pval_nationality_continentOceania
                                             #edi_issues_final$pval_nationality_continentSouth.America
                                             ))

edi_issues_final_long <- edi_issues_final_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

edi_issues_final_long$factor <- factor(edi_issues_final_long$factor, 
                                       levels=c( 
                                                 "Nationality - Oceania",  "Nationality - North America",
                                                 #"Nationality - South America",
                                                 "Nationality - Asia",
                                                 "Expat", "LGBTQ+",  "Gender - non binary",
                                                 "Gender - female"))
ggplot(edi_issues_final_long) +
  geom_point(aes(x = est, y = factor, col = sig), size = 6) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  # xlim(-1.5, 1.5)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> plot_6b

##### Figure 6c: gender disparity QA ####
gender_qa_final <- read.csv("results/survey/model_gender_qa_final.csv")

gender_qa_final_long <- data.frame(factor = c("Gender - female", "Gender - non binary",
                                              "LGBTQ+", "English comfort",
                                              "Nationality - Asia", 
                                              "Nationality - North America",
                                              "Nationality - Oceania", 
                                          #    "Nationality - South America",
                                              "Affiliation - Asia", 
                                              "Affiliation - North America",
                                              "Affiliation - Oceania"
                                              #"Affiliation - South America"
                                          ),
                                   est = c(gender_qa_final$est_genderFemale,
                                           gender_qa_final$est_genderNon.binary,
                                           gender_qa_final$est_lgbtqYes,
                                           gender_qa_final$est_english_comfort_rating,
                                           gender_qa_final$est_nationality_continentAsia,
                                           gender_qa_final$est_nationality_continentNorth.America,
                                           gender_qa_final$est_nationality_continentOceania,
                                      #     gender_qa_final$est_nationality_continentSouth.America,
                                           gender_qa_final$est_affiliation_continentAsia,
                                           gender_qa_final$est_affiliation_continentNorth.America,
                                           gender_qa_final$est_affiliation_continentOceania
                                        #   gender_qa_final$est_affiliation_continentSouth.America
                                      ),
                                   lower = c(gender_qa_final$lowerCI_genderFemale,
                                             gender_qa_final$lowerCI_genderNon.binary,
                                             gender_qa_final$lowerCI_lgbtqYes,
                                             gender_qa_final$lowerCI_english_comfort_rating,
                                             gender_qa_final$lowerCI_nationality_continentAsia,
                                             gender_qa_final$lowerCI_nationality_continentNorth.America,
                                             gender_qa_final$lowerCI_nationality_continentOceania,
                                            # gender_qa_final$lowerCI_nationality_continentSouth.America,
                                             gender_qa_final$lowerCI_affiliation_continentAsia,
                                             gender_qa_final$lowerCI_affiliation_continentNorth.America,
                                             gender_qa_final$lowerCI_affiliation_continentOceania
                                            # gender_qa_final$lowerCI_affiliation_continentSouth.America
                                            ),
                                   upper = c(gender_qa_final$higherCI_genderFemale,
                                             gender_qa_final$higherCI_genderNon.binary,
                                             gender_qa_final$higherCI_lgbtqYes,
                                             gender_qa_final$higherCI_english_comfort_rating,
                                             gender_qa_final$higherCI_nationality_continentAsia,
                                             gender_qa_final$higherCI_nationality_continentNorth.America,
                                             gender_qa_final$higherCI_nationality_continentOceania,
                                           #  gender_qa_final$higherCI_nationality_continentSouth.America,
                                             gender_qa_final$higherCI_affiliation_continentAsia,
                                             gender_qa_final$higherCI_affiliation_continentNorth.America,
                                             gender_qa_final$higherCI_affiliation_continentOceania
                                            # gender_qa_final$higherCI_affiliation_continentSouth.America
                                           ),
                                   pval = c(gender_qa_final$pval_genderFemale,
                                            gender_qa_final$pval_genderNon.binary,
                                            gender_qa_final$pval_lgbtqYes,
                                            gender_qa_final$pval_english_comfort_rating,
                                            gender_qa_final$pval_nationality_continentAsia,
                                            gender_qa_final$pval_nationality_continentNorth.America,
                                            gender_qa_final$pval_nationality_continentOceania,
                                            #gender_qa_final$pval_nationality_continentSouth.America,
                                            gender_qa_final$pval_affiliation_continentAsia,
                                            gender_qa_final$pval_affiliation_continentNorth.America,
                                            gender_qa_final$pval_affiliation_continentOceania
                                           # gender_qa_final$pval_affiliation_continentSouth.America
                                           ))

gender_qa_final_long <- gender_qa_final_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

gender_qa_final_long$factor <- factor(gender_qa_final_long$factor, 
                                      levels=c("Affiliation - Oceania", "Affiliation - North America",
                                               "Affiliation - Asia",
                                               "Nationality - Oceania", "Nationality - North America",
                                               "Nationality - Asia","English comfort", "LGBTQ+", 
                                               "Gender - non binary","Gender - female"))
ggplot(gender_qa_final_long) +
  geom_point(aes(x = est, y = factor, col = sig), size = 6) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  # xlim(-1.5, 1.5)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> plot_6c

##### Figure 6: combine #####
cowplot::plot_grid(plot_6a, plot_6b,   
                   ncol = 1, labels=c("a) Diversity represented", 
                                      "b) EDI issues"),
                   label_fontface = "plain", label_size = 22,
                   rel_heights = c(0.7,1),
                   hjust=c(-0.1, -0.18),
                   align = "hv", axis = "lb") -> plot_6_left

cowplot::plot_grid(plot_6_left, plot_6c,  
                   ncol = 2, labels=c("",  "c) No gender disparity question asking"),
                   hjust=c(0, -0.1),
                   label_fontface = "plain", label_size = 22) -> plot_6

ggsave(plot_6, file = "plots/main/fig6_issue_perception.png", 
       width=16, height=10)


#### Qualitative barchart ####
qual <- fread("data/post_survey/qual.coding.csv")

ggplot(qual, aes(x = n, y = what, fill = valency)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values=c(clrs[1], clrs[6], clrs[9]))+
  labs(x = "Number of comments", y = "Code", fill = "Category")+
  theme(legend.position = "top",
        legend.text = element_text(size=14))+
  scale_y_discrete(labels = scales::label_wrap(20)) -> qual_plot

ggsave(qual_plot, file = "plots/main/fig7_qual.png", width=8, height=8)



