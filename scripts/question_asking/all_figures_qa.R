#### packages ####

pacman::p_load(tidyverse, data.table, jtools, cowplot)
source("scripts/EDI_ggplot_theme.R")

col_sig <- clrs[c(10,2)] %>%
  color() %>% 
  set_names(nm = c("sig", "nonsig"))

### question asking general ####
general <- read.csv("results/question-asking/model_QA_justIC.csv")
general <- general %>% mutate(sig = case_when(intercept_pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

ggplot(general) +
  geom_point(aes(x = intercept_estimate, y = model_name, col = sig), size = 5) + 
  geom_segment(aes(x = intercept_ci_lower, xend = intercept_ci_higher, y = model_name),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Intercept and 95% CI", y = "Unmanipulated 
sessions") + 
  theme(axis.text.y= element_blank())+
  xlim(-1.5, 1.5) +
  scale_color_manual(values = col_sig) +
  geom_text(aes(label = "Male bias", y = 1.5, x = -0.7, size = 4))+
  geom_text(aes(label = "Female bias", y = 1.5, x = 0.7, size = 4)) +
  geom_segment(aes(x = -0.2, xend = -1.3, y = 1.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm")))+
  geom_segment(aes(x = 0.2, xend = 1.3, y = 1.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm"))) +
  theme(legend.position = "none") -> general_gender_bias

ggsave(general_gender_bias, file = "plots/main/single/general_gender_bias.png", width=5, height=4)

### question asking plenary ####
plenary <- read.csv("results/question-asking/model_QA_plenary_justIC.csv")
plenary <- plenary %>% mutate(sig = case_when(intercept_pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

ggplot(plenary) +
  geom_point(aes(x = intercept_estimate, y = model_name, col = sig), size = 5) + 
  geom_segment(aes(x = intercept_ci_lower, xend = intercept_ci_higher, y = model_name),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Intercept and 95% CI", y = "Plenaries") + 
  theme(axis.text.y= element_blank())+
  xlim(-2.5, 2.5) +
  scale_color_manual(values = col_sig) +
  geom_text(aes(label = "Male bias", y = 1.5, x = -1.2, size = 4))+
  geom_text(aes(label = "Female bias", y = 1.5, x = 1.2, size = 4)) +
  geom_segment(aes(x = -0.5, xend = -2, y = 1.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm")))+
  geom_segment(aes(x = 0.5, xend = 2, y = 1.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm"))) +
  theme(legend.position = "none") -> plenary_gender_bias

ggsave(plenary_gender_bias, file = "plots/main/single/plenary_gender_bias.png", width=5, height=4)

### survey question asking gender ####
m_survey_out <- read.csv("results/question-asking/model_qa_survey_general.csv")
m_survey_out$name <- c("Asked a question") 

m_survey_out_long <- data.frame(factor = c("Female", "Non-binary"),
                                estimate = c(m_survey_out$est_genderFemale, m_survey_out$`est_genderNon-binary`),
                                lower = c(m_survey_out$lowerCI_genderFemale, m_survey_out$`lowerCI_genderNon-binary`),
                                upper = c(m_survey_out$higherCI_genderFemale, m_survey_out$`higherCI_genderNon-binary`),
                                pval = c(m_survey_out$pval_genderFemale, m_survey_out$pval_genderNon.binary))

m_survey_out_long <- m_survey_out_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

ggplot(m_survey_out_long) +
  geom_point(aes(x = estimate, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Gender") + 
  xlim(-2, 1.5) +
  scale_color_manual(values = col_sig) +
  geom_text(aes(label = "Male bias", y = 2.5, x = -1, size = 4))+
  geom_segment(aes(x = -0.3, xend = -1.8, y = 2.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm")))+
  theme(legend.position = "none") -> survey_gender_qa

ggsave(survey_gender_qa, file = "plots/main/single/survey_gender_bias.png", width=5, height=4)

### how gender disparity question asking ####
general <- read.csv("results/question-asking/model_QA_justIC.csv")
hands <- read.csv("results/question-asking/model_QA_hands.csv")
chosen <- read.csv("results/question-asking/model_QA_chosen.csv")

out_how <- rbind(general, hands, chosen)
out_how$name <- c("Asking questions", "Raising hands", "Getting chosen") 
out_how$name <- factor(out_how$name , levels = c("Getting chosen", "Raising hands", "Asking questions"))
out_how <- out_how %>% mutate(sig = case_when(intercept_pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

ggplot(out_how) +
  geom_point(aes(x = intercept_estimate, y = name, col = sig), size = 5) + 
  geom_segment(aes(x = intercept_ci_lower, xend = intercept_ci_higher, y = name),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Intercept and 95% CI", y = "Model") + 
  xlim(-2.5, 2.5) +
  scale_color_manual(values = col_sig) +
  geom_text(aes(label = "Male bias", y = 3.5, x = -1, size = 4))+
  geom_text(aes(label = "Female bias", y = 3.5, x = 1, size = 4)) +
  geom_segment(aes(x = -0.2, xend = -2.3, y = 3.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm")))+
  geom_segment(aes(x = 0.2, xend = 2.3, y = 3.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm"))) +
  theme(legend.position = "none") -> gender_gender_bias_how

ggsave(gender_gender_bias_how, file = "plots/main/single/gender_gender_bias_why.png", 
       width=7, height=5)

##### probability plots ####

#### raising hand
hand_effect <- out_how[2,"intercept_estimate"]

# load data
#behavioural
load("data/question_asking/question_asking_data_condensed_for_analysis.RData")

data_control <- subset(data_analysis, treatment == "Control" &
                         is.na(followup) & is.na(jumper)&
                         is.na(host_asks) & !is.na(audience_women_prop))

data_control <- droplevels(data_control)

data_hands <- subset(data_control, hands_prop_men < 1 & hands_prop_women > 0 &
                       hands_prop_women < 1)

#plot prediction and model based on true data
png("plots/main/qa_hands_probability.png", width=800, height=800, units="px")
par(oma=rep(0,4),mar=c(5.1,5.1,0.5,0.5))
plot(y=jitter(data_control$hands_prop_women,0.2),cex=0.5,
     x=boot::logit(data_control$audience_women_prop),type='n',
     xlab="logit(proportion female audience)",
     ylab="Probability of female raising hand",pch=19,las=1,xlim=c(-1.5,2.5))

# polygon for theoretical null model
xrange<-range(boot::logit(data_control$audience_women_prop))
xvalues <- seq(xrange[1]-4,xrange[2]+5,by=0.01)
yvalues <- c(1/(1+exp(-(0+1*xvalues))),rep(2.0,length(xvalues)))
polygon(x=c(xvalues,rev(xvalues)),y=yvalues,col = "grey90",border = NA)
text(x=-0.8,y=0.9,col="black","bias towards females",cex=0.8)
text(x=1.8,y=0.1,col="black","bias towards males",cex=0.8)
box("plot")
points(y=jitter(data_control$hands_prop_women,0.2),cex=0.5,
       x=boot::logit(data_control$audience_women_prop),type='p',
       xlab="logit(proportion female audience)",
       ylab="Probability of female raising hand",pch=1,las=1,xlim=c(-1.5,2.5))

# this line gives the null model, with random question allocation (i.e. offset one, intercept = 0)
curve(lty=2,lend=2,expr=1/(1+exp(-(0+1*x))),from=-1.3,to=2,col="dodgerblue3",add=TRUE)
text(x=-1,y=0.5,col="dodgerblue3","null hypothesis: 
no bias",cex=0.8)

# fit model
curve(lty=1,expr=1/(1+exp(-(hand_effect+1*x))),from=xrange[1],to=xrange[2],col="black",add=TRUE)
text(x=1.75,y=0.62,col="black","actual data: 
male bias",cex=0.8)

probability_hands <- recordPlot()

dev.off()

#### getting chosen
chosen_effect <- out_how[3,"intercept_estimate"]

#plot prediction and model based on true data
png("plots/main/qa_chosen_probability.png", width=800, height=800, units="px")
par(oma=rep(0,4),mar=c(5.1,5.1,0.5,0.5))
plot(y=jitter(data_hands$gender_questioner_female,0.2),cex=0.5,
     x=boot::logit(data_hands$hands_prop_women),type='n',
     xlab="logit(proportion female hands)",
     ylab="Probability of female asking question",pch=19,las=1,xlim=c(-1.5,2.5))

# polygon for theoretical null model
xrange<-range(boot::logit(data_hands$hands_prop_women))
xvalues <- seq(xrange[1]-4,xrange[2]+5,by=0.01)
yvalues <- c(1/(1+exp(-(0+1*xvalues))),rep(2.0,length(xvalues)))
polygon(x=c(xvalues,rev(xvalues)),y=yvalues,col = "grey90",border = NA)
text(x=-0.8,y=0.9,col="black","bias towards females",cex=0.8)
text(x=1.8,y=0.1,col="black","bias towards males",cex=0.8)
box("plot")
points(y=jitter(data_hands$gender_questioner_female,0.2),cex=0.5,
       x=boot::logit(data_hands$hands_prop_women),type='p',
       xlab="logit(proportion female hands)",
       ylab="Probability of female asking question",pch=1,las=1,xlim=c(-1.5,2.5))

# this line gives the null model, with random question allocation (i.e. offset one, intercept = 0)
curve(lty=2,lend=2,expr=1/(1+exp(-(0+1*x))),from=xrange[1],to=xrange[2],col="dodgerblue3",add=TRUE)
text(x=0.7,y=0.70,col="dodgerblue3","null hypothesis: no bias",srt=+35,cex=0.8)

# fit model
curve(lty=1,expr=1/(1+exp(-(chosen_effect+1*x))),from=xrange[1],to=xrange[2],col="black",add=TRUE)
text(x=0.7,y=0.60,col="black","actual data: no bias",srt=+35,cex=0.8)

probability_chosen <- recordPlot()

dev.off()


# redo this with ggplot

# Prepare the data
data_hands$logit_hands_prop_women <- boot::logit(data_hands$hands_prop_women)
data_hands$jittered_gender <- jitter(data_hands$gender_questioner_female, 0.2)

# Define the theoretical null model polygon
xrange <- range(boot::logit(data_hands$hands_prop_women))
xvalues <- seq(xrange[1] - 4, xrange[2] + 5, by = 0.01)
yvalues <- c(1 / (1 + exp(-(0 + 1 * xvalues))), rep(2.0, length(xvalues)))
polygon_data <- data.frame(x = c(xvalues, rev(xvalues)), y = yvalues)

# Add the null model line
null_model <- function(x) {
  1 / (1 + exp(-(0 + 1 * x)))}

actual_model <- function(x) {
  1 / (1 + exp(-(chosen_effect + 1 * x)))}

# Create the base plot
ggplot(data_hands, aes(x = logit_hands_prop_women, y = jittered_gender)) +
  geom_point(alpha = 0.5) +
  labs(x = "logit(proportion female 
hands)",
       y = "Probability of female 
asking question") +
  xlim(-1.5, 2.5) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     limits=c(-0.2, 1.2))+
  # Add the polygon for the theoretical null model
  geom_polygon(data = polygon_data, aes(x = x, y = y), 
               fill = "grey90", color = NA)+
  stat_function(fun = null_model, color = "dodgerblue3", linetype = "dashed") +
  annotate("text", x = 0.7, y = 0.75, label = "null hypothesis: no bias", 
           color = "dodgerblue3", angle = 20, size = 7)+
  # Add the actual data model line
  stat_function(fun = actual_model, color = "black") +
  annotate("text", x = 0.7, y = 0.55, label = "actual data: no bias", 
           color = "black", angle = 20, size = 7) + 
  # Add text annotations for biases
  annotate("text", x = -0.8, y = 0.8, label = "bias towards 
 females",  color = "black", size = 7) +
  annotate("text", x = 1.8, y = 0.1, label = "bias towards 
males",  color = "black", size = 7) 


#### combine behaviour and survey in a plot on gender ####

cowplot::plot_grid(probability_hands, 
                   probability_chosen,
                   ncol = 2,
                   align = "hv", axis = "lb",
                   labels = c("c", "d")) -> probabilities_qa

cowplot::plot_grid(gender_gender_bias_how,
                   survey_gender_qa,
                   ncol = 2,
                   labels = c("a", "b")) -> qa_estimates

cowplot::plot_grid(qa_estimates,
                   probabilities_qa,
                   ncol = 1) -> qa

ggsave(qa, file="plots/main/fig2_questionasking_behaviour.png", 
       width=14, height=14)

### are women less comfortable? ####

comfort_gender <- read.csv("results/survey/model_comfort_ask_question_gender.csv")

comfort_gender_long <- data.frame(factor = c("Female", "Non-binary"),
                                estimate = c(comfort_gender$est_genderFemale, comfort_gender$`est_genderNon-binary`),
                                lower = c(comfort_gender$lowerCI_genderFemale, comfort_gender$`lowerCI_genderNon-binary`),
                                upper = c(comfort_gender$higherCI_genderFemale, comfort_gender$`higherCI_genderNon-binary`),
                                pval = c(comfort_gender$pval_genderFemale, comfort_gender$pval_genderNon.binary))

comfort_gender_long <- comfort_gender_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

ggplot(comfort_gender_long) +
  geom_point(aes(x = estimate, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Gender", subtitle = "I am comfortable asking questions") + 
  xlim(-2, 1.5) +
  scale_color_manual(values = col_sig) +
  geom_text(aes(label = "Male bias", y = 2.5, x = -1, size = 4))+
  geom_segment(aes(x = -0.3, xend = -1.8, y = 2.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm")))+
  theme(legend.position = "none") -> survey_comfort_gender

## is comfort predictive of asking a question?
comfort_qa <- read.csv("results/survey/model_comfort_ask_question.csv")

comfort_qa_long <- data.frame(factor = c("Comfort", "Female", "Non-binary", "Mid career", "Late career"),
                                  estimate = c(comfort_qa$est_comfort_asking_rating,
                                               comfort_qa$est_genderFemale, 
                                               comfort_qa$est_genderNon.binary,
                                               comfort_qa$est_career_3catMid.career,
                                               comfort_qa$est_career_3catLate.career),
                                  lower = c(comfort_qa$lowerCI_comfort_asking_rating, 
                                            comfort_qa$lowerCI_genderFemale,
                                            comfort_qa$lowerCI_genderNon.binary,
                                            comfort_qa$lowerCI_career_3catMid.career,
                                            comfort_qa$lowerCI_career_3catLate.career),
                                  upper = c(comfort_qa$higherCI_comfort_asking_rating, 
                                            comfort_qa$higherCI_genderFemale,
                                            comfort_qa$higherCI_genderNon.binary,
                                            comfort_qa$higherCI_career_3catMid.career,
                                            comfort_qa$higherCI_career_3catLate.career),
                                  pval = c(comfort_qa$pval_comfort_asking_rating, 
                                           comfort_qa$pval_genderFemale,
                                           comfort_qa$pval_genderNon.binary,
                                           comfort_qa$pval_career_3catMid.career,
                                           comfort_qa$pval_career_3catLate.career))

comfort_qa_long <- comfort_qa_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

ggplot(comfort_qa_long) +
  geom_point(aes(x = estimate, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", subtitle = "Probability to ask a question",y="Factor") + 
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> survey_comfort_ask

cowplot::plot_grid(survey_comfort_gender, survey_comfort_ask,
                   ncol = 1,
                   align = "hv", axis = "lb") -> survey_comfort

ggsave(survey_comfort, file = "plots/main/single/survey_comfortable_qa.png", 
       width=6, height=7)

### conditions why ####
conditions_survey_comfort <- read.csv("results/question-asking/question_asking_why_survey_result.csv")

conditions_survey_comfort <- subset(conditions_survey_comfort, grepl("comfort",response))
conditions_survey_comfort <- subset(conditions_survey_comfort, !grepl("know_speaker", response))
conditions_survey_comfort$model <- c("Speaker of own gender", "Audience of own gender", "Small audience size", "Host own gender")
conditions_survey_comfort <- conditions_survey_comfort %>% 
  mutate(sig = case_when(lrt_qval < 0.05 ~ "sig", TRUE ~ "nonsig"))

conditions_survey_comfort$model  <- factor(conditions_survey_comfort$model,
                                           levels=c("Small audience size", "Host own gender",
                                                    "Speaker of own gender", "Audience of own gender"))
ggplot(conditions_survey_comfort) +
  geom_point(aes(x = coef_female, y = model, col = sig), size = 5) + 
  geom_segment(aes(x = lower_female, xend = upper_female, y = model),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Female estimate and 95% CI", y="Response", 
       subtitle = "I feel more comfortable asking 
questions if...") + 
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> survey_comfort_conditions

## behavioural
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
  geom_point(aes(x = estimate, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y="Variable") + 
  scale_color_manual(values = col_sig) +
  geom_text(aes(label = "Male bias", y = 6.2, x = -2, size = 4)) +
  geom_segment(aes(x = -0.5, xend = -4, y = 6),
               col = "black", arrow = arrow(length=unit(0.2, "cm")))+
  theme(legend.position = "none")  -> qa_conditions

cowplot::plot_grid(survey_comfort_conditions, qa_conditions,
                   ncol = 2,
                   align = "hv", axis = "lb") -> qa_conditions_both

ggsave(qa_conditions_both, file = "plots/main/single/qa_conditions_both.png", 
       width=14, height=6)

#### motivations and hesitations ####

# 4 plots: 2 motivations, 2 hesitations, 2 for gender effect on it
# 2 for motivator/hesitator effect on probability to ask a question

conditions_survey_motivation_gender <- read.csv("results/question-asking/question_asking_why_survey_result.csv")
conditions_survey_motivation <- read.csv("results/question-asking/model_qa_conditions_motivations_gender_effect.csv")

conditions_survey_motivation_gender <- subset(conditions_survey_motivation_gender, grepl("reason_question",response))
conditions_survey_motivation_gender <- conditions_survey_motivation_gender %>% 
  mutate(sig = case_when(lrt_qval < 0.05 ~ "sig", TRUE ~ "nonsig"))


conditions_survey_motivation_gender$model <- c("Interest in topic",
                                               "Making voice heard",
                                               "Appreciate work",
                                               "Deeper understanding",
                                               "Show understanding",
                                               "Relevance own research")

ggplot(conditions_survey_motivation_gender) +
  geom_point(aes(x = coef_female, y = model, col = sig), size = 5) + 
  geom_segment(aes(x = lower_female, xend = upper_female, y = model),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Female estimate and 95% CI", y="Response", 
       subtitle = "Gender effect on motivations") + 
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> motivate_gender

# probability to ask question
conditions_survey_motivation <- subset(conditions_survey_motivation, grepl("reason_question",predictor))
conditions_survey_motivation <- conditions_survey_motivation %>% 
  mutate(sig = case_when(lrt_qval < 0.05 ~ "sig", TRUE ~ "nonsig"))


conditions_survey_motivation$model <- c("Interest in topic",
                                               "Making voice heard",
                                               "Appreciate work",
                                               "Deeper understanding",
                                               "Show understanding",
                                               "Relevance own research")

ggplot(conditions_survey_motivation) +
  geom_point(aes(x = coef_reason, y = model, col = sig), size = 5) + 
  geom_segment(aes(x = lower_reason, xend = upper_reason, y = model),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y="Motivator", 
       subtitle = "Motivation predictor of question asking") + 
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> motivate_question_prob

### hesitations

conditions_survey_hesitation_gender <- read.csv("results/question-asking/question_asking_why_survey_result.csv")
conditions_survey_hesitation <- read.csv("results/question-asking/model_qa_conditions_motivations_gender_effect.csv")

conditions_survey_hesitation_gender <- subset(conditions_survey_hesitation_gender, grepl("reason_noquestion",response))
conditions_survey_hesitation_gender <- conditions_survey_hesitation_gender %>% 
  mutate(sig = case_when(lrt_qval < 0.05 ~ "sig", TRUE ~ "nonsig"))

conditions_survey_hesitation_gender$model <- c("Not clever",
                                               "Misunderstand",
                                               "Intimidated speaker",
                                               "Intimidated audience",
                                               "Intimidated setting",
                                               "Intimidated host",
                                               "Irrelevance / unimportant",
                                               "Phrasing",
                                               "Not chosen",
                                               "No time",
                                               "Too introverted",
                                               "Rather in private",
                                               "No confidence")

ggplot(conditions_survey_hesitation_gender) +
  geom_point(aes(x = coef_female, y = model, col = sig), size = 5) + 
  geom_segment(aes(x = lower_female, xend = upper_female, y = model),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Female estimate and 95% CI", y="Response", 
       subtitle = "Gender effect on hesitations") + 
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> hesitate_gender

# probability to ask question
conditions_survey_hesitation <- subset(conditions_survey_hesitation, grepl("reason_noquestion",predictor))
conditions_survey_hesitation <- conditions_survey_hesitation %>% 
  mutate(sig = case_when(lrt_qval < 0.05 ~ "sig", TRUE ~ "nonsig"))

conditions_survey_hesitation$model <- c("Not clever",
                                        "Misunderstand",
                                        "Intimidated speaker",
                                        "Intimidated audience",
                                        "Intimidated setting",
                                        "Intimidated host",
                                        "Irrelevance / unimportant",
                                        "Phrasing",
                                        "Not chosen",
                                        "No time",
                                        "Too introverted",
                                        "Rather in private",
                                        "No confidence")

ggplot(conditions_survey_hesitation) +
  geom_point(aes(x = coef_reason, y = model, col = sig), size = 5) + 
  geom_segment(aes(x = lower_reason, xend = upper_reason, y = model),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y="Motivator", 
       subtitle = "Hesitation predictor of question asking") + 
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> hesitate_question_prob

## combine in plot

cowplot::plot_grid(motivate_gender, motivate_question_prob,
                   ncol = 2,
                   align = "hv", axis = "lb") -> motivations

ggsave(motivations, file = "plots/main/single/survey_motivations.png", 
       width=14, height=6)

cowplot::plot_grid(hesitate_gender, hesitate_question_prob,
                   ncol = 2,
                   align = "hv", axis = "lb") -> hesitations

ggsave(hesitations, file = "plots/main/single/survey_hesitations.png", 
       width=16, height=12)

cowplot::plot_grid(motivations, hesitations,
                   ncol = 1, rel_heights = c(0.5,1),
                   align = "hv", axis = "lb") -> hesitations_motivations

ggsave(hesitations_motivations, file = "plots/main/single/survey_hesitations_motivations.png", 
       width=14, height=18)


#### other disparities ####
##### compliment ####
compliment <- read.csv("results/question-asking/model_m_compliment.csv")

compliment_long <- data.frame(factor = c("Question number", "Questioner age - 35-50",
                                         "Questioner age - > 50"),
                              est = c(compliment$est_question_nr,
                                      compliment$est_questioner_age2,
                                      compliment$est_questioner_age3),
                              lower = c(compliment$lowerCI_question_nr,
                                        compliment$lowerCI_questioner_age2,
                                        compliment$lowerCI_questioner_age3),
                              upper = c(compliment$higherCI_question_nr,
                                        compliment$higherCI_questioner_age2,
                                        compliment$higherCI_questioner_age3),
                              pval = c(compliment$pval_question_nr,
                                       compliment$pval_questioner_age2,
                                       compliment$pval_questioner_age3))

compliment_long <- compliment_long %>% 
  mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

compliment_long$factor <- factor(compliment_long$factor, 
                                 levels=c("Questioner age - > 50", 
                                          "Questioner age - 35-50",
                                          "Question number"))

ggplot(compliment_long) +
  geom_point(aes(x = est, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y="Variable") + 
  xlim(-1.5, 1)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> compliment

##### overtime ####
overtime <- read.csv("results/question-asking/model_m_overtime.csv")

overtime_long <- data.frame(factor = c("Speaker career stage - late", 
                                         "Speaker career stage - mid"),
                              est = c(overtime$est_speaker_career_shortLate.career,
                                      overtime$est_speaker_career_shortMid.career),
                              lower = c(overtime$lowerCI_speaker_career_shortLate.career,
                                        overtime$lowerCI_speaker_career_shortMid.career),
                              upper = c(overtime$higherCI_speaker_career_shortLate.career,
                                        overtime$higherCI_speaker_career_shortMid.career),
                              pval = c(overtime$pval_speaker_career_shortLate.career,
                                       overtime$pval_speaker_career_shortMid.career))

overtime_long <- overtime_long %>% 
  mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

overtime_long$factor <- factor(overtime_long$factor, 
                                 levels=c("Speaker career stage - mid", 
                                          "Speaker career stage - late"))

ggplot(overtime_long) +
  geom_point(aes(x = est, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y="Variable") + 
  xlim(-1, 1.2)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> overtime

##### critical ####
critical <- read.csv("results/question-asking/model_m_critical_q_age.csv")

critical_long <- data.frame(factor = c("Age questioner - 35-50", 
                                       "Age questioner - >50"),
                            est = c(critical$est_questioner_age2,
                                    critical$est_questioner_age3),
                            lower = c(critical$lowerCI_questioner_age2,
                                      critical$lowerCI_questioner_age3),
                            upper = c(critical$higherCI_questioner_age2,
                                      critical$higherCI_questioner_age3),
                            pval = c(critical$pval_questioner_age2,
                                     critical$pval_questioner_age3))

critical_long <- critical_long %>% 
  mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

critical_long$factor <- factor(critical_long$factor, 
                               levels=c("Age questioner - >50", 
                                        "Age questioner - 35-50"))

ggplot(critical_long) +
  geom_point(aes(x = est, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y="Variable") + 
#  xlim(-1, 1.2)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> critical

##### combine ####

cowplot::plot_grid(critical, compliment, overtime,
                   ncol = 2, labels=c("a) Criticism", "b) Compliment", "c) Overtime"),
                   align = "hv", axis = "lb") -> other_disparities

ggsave(other_disparities, file = "plots/main/single/other_disparities.png", 
       width=14, height=8)


### gender first Q plots #####
#### manipulated ####

load("data/question_asking/question_asking_data_condensed_for_analysis.RData")
data_analysis[data_analysis == "NA"] <- NA
data_analysis$ID_talk<-as.factor(paste(data_analysis$session_id,data_analysis$talk_nr,sep="-")) #create random factor

data_clean <- subset(data_analysis, is.na(data_analysis$jumper))
data_clean <- subset(data_clean, is.na(data_clean$followup))
data_clean <- subset(data_clean, is.na(data_clean$host_asks))
nrow(data_clean) #1037

#excluding irrelevant columns
colnames(data_clean)
data_clean<-data_clean[,c("ID_talk","session_id","talk_nr","treatment","treatment_success","condition",
                          "question_nr","questioner_gender","gender_questioner_male","audience_women_prop",
                          "hands_men","hands_women","hands_total","questioner_age")]

#question by female is treated as "success" (1), question by male as unsuccessful (0)
data_clean <- data_clean %>% 
  mutate(gender_questioner_female = if_else(questioner_gender == "F", 1, 0))
data_clean$gender_questioner_male <- NULL

#exclude NAs proportion audience
data_clean <- subset(data_clean, !is.na(data_clean$audience_women_prop))
rm(data_analysis)

### manipulated data 

data_analysis_tr <- subset(data_clean, data_clean$treatment != "Control") #select only treatment sessions
data_analysis_tr <- subset(data_analysis_tr, data_analysis_tr$treatment_success == "Successful") #select only succesful sessions

#simplify column for the gender of the first questioner in the talk
data_analysis_tr$condition <- gsub(data_analysis_tr$condition, pattern = "First question to a woman", replacement = "F")
data_analysis_tr$condition <- gsub(data_analysis_tr$condition, pattern = "First question to a man", replacement = "M")

#exclude first question
data_analysis_tr <- subset(data_analysis_tr, data_analysis_tr$question_nr != 1)

#run model, does the gender of the first question-asker affect the gender bias in who asks questions during the rest of the Q&A?
library(lme4);library(lmerTest) #glmer() function in the lme4 package
data_analysis_tr$condition<-as.factor(data_analysis_tr$condition)
data_analysis_tr$session_id<-as.factor(data_analysis_tr$session_id)
data_analysis_tr$ID_talk<-as.factor(data_analysis_tr$ID_talk)

null <- glmer(gender_questioner_female ~ (1|session_id/ID_talk), data = data_analysis_tr, family = binomial, offset = boot::logit(audience_women_prop))

# general
Mt1 <- glmer(gender_questioner_female ~ -1 + condition + (1|session_id/ID_talk), data = data_analysis_tr, family = binomial, offset = boot::logit(audience_women_prop))

lrt_mt1 <- as.data.frame(drop1(test="Chisq",Mt1) )
ci_mt1 <- as.data.frame(confint(Mt1))
summary_mt1 <- as.data.frame(summary(Mt1)$coef)
out_QA_treatment <- data.frame(model_name = "QA_treatment_hands",
                                  AIC = AIC(Mt1),
                                  n_obs = nobs(Mt1),
                                  lrt_pval = lrt_mt1$`Pr(Chi)`[2],
                                  condition_female_estimate = summary_mt1["conditionF", "Estimate"],
                                  condition_female_lower = ci_mt1["conditionF", "2.5 %"],
                                  condition_female_upper = ci_mt1["conditionF", "97.5 %"],
                                  condition_female_pval = summary_mt1["conditionF", "Pr(>|z|)"],
                                  condition_male_estimate = summary_mt1["conditionM", "Estimate"],
                                  condition_male_lower = ci_mt1["conditionM", "2.5 %"],
                                  condition_male_upper = ci_mt1["conditionM", "97.5 %"],
                                  condition_male_pval = summary_mt1["conditionM", "Pr(>|z|)"])

out_QA_treatment_long <- data.frame(condition = c("Female first question", "Male first question"),
                                       est = c(out_QA_treatment$condition_female_estimate,
                                               out_QA_treatment$condition_male_estimate),
                                       lower = c(out_QA_treatment$condition_female_lower,
                                                 out_QA_treatment$condition_male_lower),
                                       upper = c(out_QA_treatment$condition_female_upper,
                                                 out_QA_treatment$condition_male_upper),
                                       pval = c(out_QA_treatment$condition_female_pval,
                                                out_QA_treatment$condition_male_pval))

out_QA_treatment_long$condition <- factor(out_QA_treatment_long$condition,
                                             levels=c("Male first question", "Female first question"))
out_QA_treatment_long <- out_QA_treatment_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

# raise hands
Mt2 <- glmer(cbind(hands_women, hands_men) ~ -1 + condition + (1|session_id), data = data_analysis_tr, family = binomial, offset = boot::logit(audience_women_prop))

lrt_mt2 <- as.data.frame(drop1(test="Chisq",Mt2) )
ci_mt2 <- as.data.frame(confint(Mt2))
summary_mt2 <- as.data.frame(summary(Mt2)$coef)
out_hands_treatment <- data.frame(model_name = "QA_treatment_hands",
                                  AIC = AIC(Mt2),
                                  n_obs = nobs(Mt2),
                                  lrt_pval = lrt_mt2$`Pr(Chi)`[2],
                                  condition_female_estimate = summary_mt2["conditionF", "Estimate"],
                                  condition_female_lower = ci_mt2["conditionF", "2.5 %"],
                                  condition_female_upper = ci_mt2["conditionF", "97.5 %"],
                                  condition_female_pval = summary_mt2["conditionF", "Pr(>|z|)"],
                                  condition_male_estimate = summary_mt2["conditionM", "Estimate"],
                                  condition_male_lower = ci_mt2["conditionM", "2.5 %"],
                                  condition_male_upper = ci_mt2["conditionM", "97.5 %"],
                                  condition_male_pval = summary_mt2["conditionM", "Pr(>|z|)"])

out_hands_treatment_long <- data.frame(condition = c("Female first question", "Male first question"),
                                       est = c(out_hands_treatment$condition_female_estimate,
                                               out_hands_treatment$condition_male_estimate),
                                       lower = c(out_hands_treatment$condition_female_lower,
                                                 out_hands_treatment$condition_male_lower),
                                       upper = c(out_hands_treatment$condition_female_upper,
                                                 out_hands_treatment$condition_male_upper),
                                       pval = c(out_hands_treatment$condition_female_pval,
                                                out_hands_treatment$condition_male_pval))

out_hands_treatment_long$condition <- factor(out_hands_treatment_long$condition,
                                             levels=c("Male first question", "Female first question"))
out_hands_treatment_long <- out_hands_treatment_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

# get chosen
data_analysis_tr$hands_women_prop <- data_analysis_tr$hands_women / data_analysis_tr$hands_total
data_analysis_tr <- subset(data_analysis_tr, hands_women_prop > 0 & hands_women_prop < 1)

Mt3 <- glmer(gender_questioner_female ~ -1 + condition + (1|session_id/ID_talk),
             data_analysis_tr, family = "binomial", offset = boot::logit(hands_women_prop))

lrt_mt3 <- as.data.frame(drop1(test="Chisq",Mt3) )
ci_mt3 <- as.data.frame(confint(Mt3))
summary_mt3 <- as.data.frame(summary(Mt3)$coef)
out_chosen_treatment <- data.frame(model_name = "QA_treatment_chosen",
                                  AIC = AIC(Mt3),
                                  n_obs = nobs(Mt3),
                                  lrt_pval = lrt_mt3$`Pr(Chi)`[2],
                                  condition_female_estimate = summary_mt3["conditionF", "Estimate"],
                                  condition_female_lower = ci_mt3["conditionF", "2.5 %"],
                                  condition_female_upper = ci_mt3["conditionF", "97.5 %"],
                                  condition_female_pval = summary_mt3["conditionF", "Pr(>|z|)"],
                                  condition_male_estimate = summary_mt3["conditionM", "Estimate"],
                                  condition_male_lower = ci_mt3["conditionM", "2.5 %"],
                                  condition_male_upper = ci_mt3["conditionM", "97.5 %"],
                                  condition_male_pval = summary_mt3["conditionM", "Pr(>|z|)"])

out_chosen_treatment_long <- data.frame(condition = c("Female first question", "Male first question"),
                                       est = c(out_chosen_treatment$condition_female_estimate,
                                               out_chosen_treatment$condition_male_estimate),
                                       lower = c(out_chosen_treatment$condition_female_lower,
                                                 out_chosen_treatment$condition_male_lower),
                                       upper = c(out_chosen_treatment$condition_female_upper,
                                                 out_chosen_treatment$condition_male_upper),
                                       pval = c(out_chosen_treatment$condition_female_pval,
                                                out_chosen_treatment$condition_male_pval))

out_chosen_treatment_long$condition <- factor(out_chosen_treatment_long$condition,
                                             levels=c("Male first question", "Female first question"))
out_chosen_treatment_long <- out_chosen_treatment_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

# collect info

### plotting all three in one plot
ggplot(out_QA_treatment_long) +
  geom_point(aes(x = est, y = condition, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = condition),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Condition",
       title = "Asking questions") + 
  # xlim(-2.5, 2.5) +
 # ylim(0,0.8)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none")+
  geom_text(aes(label = "Male bias", y = 2.5, x = -0.7, size = 4))+
  geom_text(aes(label = "Female bias", y = 2.5, x = 0.7, size = 4)) +
  geom_segment(aes(x = -0.2, xend = -1.3, y = 2.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm")))+
  geom_segment(aes(x = 0.2, xend = 1.3, y = 2.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm"))) -> treatment_QA

ggplot(out_hands_treatment_long) +
  geom_point(aes(x = est, y = condition, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = condition),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Condition",
       title = "Raising hands") + 
 # xlim(-2.5, 2.5) +
 # ylim(0,0.8)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none")+
  geom_text(aes(label = "Male bias", y = 2.5, x = -0.7, size = 4))+
  geom_text(aes(label = "Female bias", y = 2.5, x = 0.7, size = 4)) +
  geom_segment(aes(x = -0.2, xend = -1.3, y = 2.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm")))+
  geom_segment(aes(x = 0.2, xend = 1.3, y = 2.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm"))) -> treatment_hands


ggplot(out_chosen_treatment_long) +
  geom_point(aes(x = est, y = condition, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = condition),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Condition",
       title = "Getting chosen") + 
  # xlim(-2.5, 2.5) +
  #ylim(0,0.8)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none")+
  geom_text(aes(label = "Male bias", y = 2.5, x = -0.7, size = 4))+
  geom_text(aes(label = "Female bias", y = 2.5, x = 0.7, size = 4)) +
  geom_segment(aes(x = -0.2, xend = -1.3, y = 2.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm")))+
  geom_segment(aes(x = 0.2, xend = 1.3, y = 2.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm"))) -> treatment_chosen


cowplot::plot_grid(treatment_QA, treatment_hands,treatment_chosen,
                   ncol = 1,
                   align = "hv", axis = "lb") -> treatment

ggsave(treatment, file="plots/main/treatment_results.png", height=10,width=8)

#### unmanipulated ####
data_analysis_c <- subset(data_clean, data_clean$treatment == "Control")

#unneccesary columns
data_analysis_c$treatment_success<-NULL
data_analysis_c$condition<-NULL

#create "condition" column by creating separate dataframe for first questions (firstq_c)
# and creating "condition" column in data_analysis_c using the information in firstq_c
firstq_c <- subset(data_analysis_c, data_analysis_c$question_nr == 1)
colnames(firstq_c)[colnames(firstq_c) == "gender_questioner_female"] ="FIRST_gender_questioner_female"
colnames(firstq_c)[colnames(firstq_c) == "questioner_gender"] ="FIRST_questioner_gender"
data_analysis_c$FIRST_gender_questioner_female <- firstq_c$FIRST_gender_questioner_female[match(data_analysis_c$ID_talk, firstq_c$ID_talk)]
data_analysis_c$FIRST_questioner_gender <- firstq_c$FIRST_questioner_gender[match(data_analysis_c$ID_talk, firstq_c$ID_talk)]
rm(firstq_c)

#exclude first question
data_analysis_c <- subset(data_analysis_c, data_analysis_c$question_nr != 1)

#exclude NAs
data_analysis_c <- subset(data_analysis_c, !is.na(data_analysis_c$FIRST_questioner_gender))
data_analysis_c <- subset(data_analysis_c, !is.na(data_analysis_c$audience_women_prop))

data_analysis_c$FIRST_questioner_gender <- as.factor(data_analysis_c$FIRST_questioner_gender)
data_analysis_c$session_id <- as.factor(data_analysis_c$session_id)
data_analysis_c$ID_talk <- as.factor(data_analysis_c$ID_talk)

# general
Mc1 <- glmer(gender_questioner_female ~ -1 + FIRST_questioner_gender + (1|session_id/ID_talk), family = binomial, offset = boot::logit(audience_women_prop), data = data_analysis_c)

lrt_mc1 <- as.data.frame(drop1(test="Chisq",Mc1) )
ci_mc1 <- as.data.frame(confint(Mc1))
summary_mc1 <- as.data.frame(summary(Mc1)$coef)
out_QA_control <- data.frame(model_name = "QA_control_hands",
                               AIC = AIC(Mc1),
                               n_obs = nobs(Mc1),
                               lrt_pval = lrt_mc1$`Pr(Chi)`[2],
                               condition_female_estimate = summary_mc1["FIRST_questioner_genderF", "Estimate"],
                               condition_female_lower = ci_mc1["FIRST_questioner_genderF", "2.5 %"],
                               condition_female_upper = ci_mc1["FIRST_questioner_genderF", "97.5 %"],
                               condition_female_pval = summary_mc1["FIRST_questioner_genderF", "Pr(>|z|)"],
                               condition_male_estimate = summary_mc1["FIRST_questioner_genderM", "Estimate"],
                               condition_male_lower = ci_mc1["FIRST_questioner_genderM", "2.5 %"],
                               condition_male_upper = ci_mc1["FIRST_questioner_genderM", "97.5 %"],
                               condition_male_pval = summary_mc1["FIRST_questioner_genderM", "Pr(>|z|)"])

out_QA_control_long <- data.frame(condition = c("Female first question", "Male first question"),
                                    est = c(out_QA_control$condition_female_estimate,
                                            out_QA_control$condition_male_estimate),
                                    lower = c(out_QA_control$condition_female_lower,
                                              out_QA_control$condition_male_lower),
                                    upper = c(out_QA_control$condition_female_upper,
                                              out_QA_control$condition_male_upper),
                                    pval = c(out_QA_control$condition_female_pval,
                                             out_QA_control$condition_male_pval))

out_QA_control_long$condition <- factor(out_QA_control_long$condition,
                                          levels=c("Male first question", "Female first question"))
out_QA_control_long <- out_QA_control_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

# raise hands
Mc2 <- glmer(cbind(hands_women, hands_men) ~ -1 + FIRST_questioner_gender + (1|session_id/ID_talk), data = data_analysis_c, family = binomial, offset = boot::logit(audience_women_prop))

lrt_mc2 <- as.data.frame(drop1(test="Chisq",Mc2) )
ci_mc2 <- as.data.frame(confint(Mc2))
summary_mc2 <- as.data.frame(summary(Mc2)$coef)
out_hands_control <- data.frame(model_name = "QA_control_hands",
                                  AIC = AIC(Mc2),
                                  n_obs = nobs(Mc2),
                                  lrt_pval = lrt_mc2$`Pr(Chi)`[2],
                                  condition_female_estimate = summary_mc2["FIRST_questioner_genderF", "Estimate"],
                                  condition_female_lower = ci_mc2["FIRST_questioner_genderF", "2.5 %"],
                                  condition_female_upper = ci_mc2["FIRST_questioner_genderF", "97.5 %"],
                                  condition_female_pval = summary_mc2["FIRST_questioner_genderF", "Pr(>|z|)"],
                                  condition_male_estimate = summary_mc2["FIRST_questioner_genderM", "Estimate"],
                                  condition_male_lower = ci_mc2["FIRST_questioner_genderM", "2.5 %"],
                                  condition_male_upper = ci_mc2["FIRST_questioner_genderM", "97.5 %"],
                                  condition_male_pval = summary_mc2["FIRST_questioner_genderM", "Pr(>|z|)"])

out_hands_control_long <- data.frame(condition = c("Female first question", "Male first question"),
                                       est = c(out_hands_control$condition_female_estimate,
                                               out_hands_control$condition_male_estimate),
                                       lower = c(out_hands_control$condition_female_lower,
                                                 out_hands_control$condition_male_lower),
                                       upper = c(out_hands_control$condition_female_upper,
                                                 out_hands_control$condition_male_upper),
                                       pval = c(out_hands_control$condition_female_pval,
                                                out_hands_control$condition_male_pval))

out_hands_control_long$condition <- factor(out_hands_control_long$condition,
                                             levels=c("Male first question", "Female first question"))
out_hands_control_long <- out_hands_control_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

# get chosen
data_analysis_c$hands_women_prop <- data_analysis_c$hands_women / data_analysis_c$hands_total
data_analysis_c <- subset(data_analysis_c, hands_women_prop > 0 & hands_women_prop < 1)

Mc3 <- glmer(gender_questioner_female ~ -1 + FIRST_questioner_gender + (1|session_id/ID_talk),
             data_analysis_c, family = "binomial", offset = boot::logit(hands_women_prop))

lrt_mc3 <- as.data.frame(drop1(test="Chisq",Mc3) )
ci_mc3 <- as.data.frame(confint(Mc3))
summary_mc3 <- as.data.frame(summary(Mc3)$coef)
out_chosen_control <- data.frame(model_name = "QA_control_chosen",
                                   AIC = AIC(Mc3),
                                   n_obs = nobs(Mc3),
                                   lrt_pval = lrt_mc3$`Pr(Chi)`[2],
                                   condition_female_estimate = summary_mc3["FIRST_questioner_genderF", "Estimate"],
                                   condition_female_lower = ci_mc3["FIRST_questioner_genderF", "2.5 %"],
                                   condition_female_upper = ci_mc3["FIRST_questioner_genderF", "97.5 %"],
                                   condition_female_pval = summary_mc3["FIRST_questioner_genderF", "Pr(>|z|)"],
                                   condition_male_estimate = summary_mc3["FIRST_questioner_genderM", "Estimate"],
                                   condition_male_lower = ci_mc3["FIRST_questioner_genderM", "2.5 %"],
                                   condition_male_upper = ci_mc3["FIRST_questioner_genderM", "97.5 %"],
                                   condition_male_pval = summary_mc3["FIRST_questioner_genderM", "Pr(>|z|)"])

out_chosen_control_long <- data.frame(condition = c("Female first question", "Male first question"),
                                        est = c(out_chosen_control$condition_female_estimate,
                                                out_chosen_control$condition_male_estimate),
                                        lower = c(out_chosen_control$condition_female_lower,
                                                  out_chosen_control$condition_male_lower),
                                        upper = c(out_chosen_control$condition_female_upper,
                                                  out_chosen_control$condition_male_upper),
                                        pval = c(out_chosen_control$condition_female_pval,
                                                 out_chosen_control$condition_male_pval))

out_chosen_control_long$condition <- factor(out_chosen_control_long$condition,
                                              levels=c("Male first question", "Female first question"))
out_chosen_control_long <- out_chosen_control_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

# collect info

### plotting all three in one plot
ggplot(out_QA_control_long) +
  geom_point(aes(x = est, y = condition, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = condition),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Condition",
       title = "Asking questions") + 
  # xlim(-2.5, 2.5) +
  #ylim(0,0.8)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none")+
  geom_text(aes(label = "Male bias", y = 2.5, x = -0.7, size = 4))+
  geom_text(aes(label = "Female bias", y = 2.5, x = 0.7, size = 4)) +
  geom_segment(aes(x = -0.2, xend = -1.3, y = 2.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm")))+
  geom_segment(aes(x = 0.2, xend = 1.3, y = 2.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm"))) -> control_QA

ggplot(out_hands_control_long) +
  geom_point(aes(x = est, y = condition, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = condition),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Condition",
       title = "Raising hands") + 
  # xlim(-2.5, 2.5) +
  #ylim(0,0.8)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none")+
  geom_text(aes(label = "Male bias", y = 2.5, x = -0.7, size = 4))+
  geom_text(aes(label = "Female bias", y = 2.5, x = 0.7, size = 4)) +
  geom_segment(aes(x = -0.2, xend = -1.3, y = 2.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm")))+
  geom_segment(aes(x = 0.2, xend = 1.3, y = 2.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm"))) -> control_hands


ggplot(out_chosen_control_long) +
  geom_point(aes(x = est, y = condition, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = condition),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Condition",
       title = "Getting chosen") + 
  # xlim(-2.5, 2.5) +
  #ylim(0,0.8)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none")+
  geom_text(aes(label = "Male bias", y = 2.5, x = -0.7, size = 4))+
  geom_text(aes(label = "Female bias", y = 2.5, x = 0.7, size = 4)) +
  geom_segment(aes(x = -0.2, xend = -1.3, y = 2.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm")))+
  geom_segment(aes(x = 0.2, xend = 1.3, y = 2.3),
               col = "black", arrow = arrow(length=unit(0.2, "cm"))) -> control_chosen


cowplot::plot_grid(control_QA, control_hands,control_chosen,
                   ncol = 1, 
                   align = "hv", axis = "lb") -> control

ggsave(control, file="plots/main/control_results.png", height=10,width=8)

cowplot::plot_grid(treatment, control,
                   ncol = 2, labels=c("Manipulated", "Unmanipulated"),
                   align = "hv", axis = "lb") -> control_treatment

ggsave(control_treatment, file="plots/main/treatment_control_results.png", 
       height=10,width=14)
