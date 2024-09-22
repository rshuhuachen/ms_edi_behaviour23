#### packages ####

pacman::p_load(tidyverse, data.table)
source("scripts/EDI_ggplot_theme.R")

col_sig <- clrs[c(10,2)] %>%
  color() %>% 
  set_names(nm = c("sig", "nonsig"))

#### individual plots ####

### feeling heard ####
heard_gender <- read.csv("results/survey/model_feeling_heard_gender.csv")
heard_english <- read.csv("results/survey/model_feeling_heard_english.csv")
heard_expat <- read.csv("results/survey/model_feeling_heard_expat.csv")
heard_lgbtq <- read.csv("results/survey/model_feeling_heard_lgbtq.csv")
heard_nat <- read.csv("results/survey/model_feeling_heard_nat.csv")
heard_expert <- read.csv("results/survey/model_feeling_heard_expert.csv")

heard <- data.frame(factor = c("Expertise", "English comfort", "Expat", "LGBTQ+", "Gender - male"),
                    est = c(heard_expert$est_expertise_rating,
                            heard_english$est_english_comfort_rating,
                            heard_expat$est_expatExpat,
                            heard_lgbtq$est_lgbtqYes,
                            heard_gender$est_genderMale),
                    lower = c(heard_expert$lowerCI_expertise_rating,
                              heard_english$lowerCI_english_comfort_rating,
                              heard_expat$lowerCI_expatExpat,
                              heard_lgbtq$lowerCI_lgbtqYes,
                              heard_gender$lowerCI_genderMale),
                    upper = c(heard_expert$higherCI_expertise_rating,
                              heard_english$higherCI_english_comfort_rating,
                              heard_expat$higherCI_expatExpat,
                              heard_lgbtq$higherCI_lgbtqYes,
                              heard_gender$higherCI_genderMale),
                    lrt_pval = c(heard_expert$lrt_pval,
                                 heard_english$lrt_pval,
                                 heard_expat$lrt_pval,
                                 heard_lgbtq$lrt_pval,
                                 heard_gender$lrt_pval))

heard <- heard %>% mutate(sig = case_when(lrt_pval < 0.05 ~ "sig", TRUE ~ "nonsig"))
heard$factor <- factor(heard$factor, levels=c("Expertise", "English comfort", "Expat", "LGBTQ+", "Gender - male"))

ggplot(heard) +
  geom_point(aes(x = est, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none")+
  xlim(-1,1) -> initial_model_heard

# final model
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
  geom_point(aes(x = est, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  xlim(-0.5, 0.5)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> final_model_heard

#combine

cowplot::plot_grid(initial_model_heard, final_model_heard,
                   ncol = 1, rel_heights = c(1,0.5),
                   align = "hv", axis = "lb") -> heard

ggsave(initial_model_heard, file = "plots/main/single/feeling_heard_initial.png", width=6, height=8)
ggsave(final_model_heard, file = "plots/main/single/feeling_heard_final.png", width=6, height=6)
ggsave(heard, file = "plots/main/single/feeling_heard_initial_final.png", width=8, height=10)

### feeling comfortable being yourself ####

yourself_gender <- read.csv("results/survey/model_comf_yourself_gender.csv")
yourself_english <- read.csv("results/survey/model_comf_yourself_english.csv")
yourself_expat <- read.csv("results/survey/model_comf_yourself_expat.csv")
yourself_lgbtq <- read.csv("results/survey/model_comf_yourself_lgbtq.csv")
yourself_nat <- read.csv("results/survey/model_comf_yourself_nat.csv")
yourself_expert <- read.csv("results/survey/model_comf_yourself_expert.csv")

yourself <- data.frame(factor = c("Expertise", "English comfort", "Expat", "LGBTQ+", "Gender - male"),
                    est = c(yourself_expert$est_expertise_rating,
                            yourself_english$est_english_comfort_rating,
                            yourself_expat$est_expatExpat,
                            yourself_lgbtq$est_lgbtqYes,
                            yourself_gender$est_genderMale),
                    lower = c(yourself_expert$lowerCI_expertise_rating,
                              yourself_english$lowerCI_english_comfort_rating,
                              yourself_expat$lowerCI_expatExpat,
                              yourself_lgbtq$lowerCI_lgbtqYes,
                              yourself_gender$lowerCI_genderMale),
                    upper = c(yourself_expert$higherCI_expertise_rating,
                              yourself_english$higherCI_english_comfort_rating,
                              yourself_expat$higherCI_expatExpat,
                              yourself_lgbtq$higherCI_lgbtqYes,
                              yourself_gender$higherCI_genderMale),
                    lrt_pval = c(yourself_expert$lrt_pval,
                                 yourself_english$lrt_pval,
                                 yourself_expat$lrt_pval,
                                 yourself_lgbtq$lrt_pval,
                                 yourself_gender$lrt_pval))

yourself <- yourself %>% mutate(sig = case_when(lrt_pval < 0.05 ~ "sig", TRUE ~ "nonsig"))
yourself$factor <- factor(yourself$factor, levels=c("Expertise", "English comfort", "Expat", "LGBTQ+", "Gender - male"))

ggplot(yourself) +
  geom_point(aes(x = est, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none")+
  xlim(-1,1) -> initial_model_yourself

# final model
yourself_final <- read.csv("results/survey/model_comf_yourself_final.csv")

yourself_final_long <- data.frame(factor = c("English comfort", "Expertise", "Gender - male"),
                               est = c(yourself_final$est_english_comfort_rating,
                                       yourself_final$est_expertise_rating,
                                       yourself_final$est_genderMale),
                               lower = c(yourself_final$lowerCI_english_comfort_rating,
                                         yourself_final$lowerCI_expertise_rating,
                                         yourself_final$lowerCI_genderMale),
                               upper = c(yourself_final$higherCI_english_comfort_rating,
                                         yourself_final$higherCI_expertise_rating,
                                         yourself_final$higherCI_genderMale),
                               pval = c(yourself_final$pval_english_comfort_rating,
                                        yourself_final$pval_expertise_rating,
                                        yourself_final$pval_genderMale))

yourself_final_long <- yourself_final_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

ggplot(yourself_final_long) +
  geom_point(aes(x = est, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  xlim(-0.5, 1)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> final_model_yourself

#combine

cowplot::plot_grid(initial_model_yourself, final_model_yourself,
                   ncol = 1, rel_heights = c(1,0.5),
                   align = "hv", axis = "lb") -> yourself

ggsave(initial_model_yourself, file = "plots/main/single/feeling_yourself_initial.png", width=6, height=8)
ggsave(final_model_yourself, file = "plots/main/single/feeling_yourself_final.png", width=6, height=6)
ggsave(yourself, file = "plots/main/single/feeling_yourself_initial_final.png", width=8, height=10)

### sense of belonging ####

belong_gender <- read.csv("results/survey/model_belonging_gender.csv")
belong_english <- read.csv("results/survey/model_belonging_english.csv")
belong_expat <- read.csv("results/survey/model_belonging_expat.csv")
belong_lgbtq <- read.csv("results/survey/model_belonging_lgbtq.csv")
belong_nat <- read.csv("results/survey/model_belonging_nat.csv")
belong_expert <- read.csv("results/survey/model_belonging_expert.csv")

belong <- data.frame(factor = c("Expertise", "English comfort", "Expat", "LGBTQ+", "Gender - male"),
                       est = c(belong_expert$est_expertise_rating,
                               belong_english$est_english_comfort_rating,
                               belong_expat$est_expatExpat,
                               belong_lgbtq$est_lgbtqYes,
                               belong_gender$est_genderMale),
                       lower = c(belong_expert$lowerCI_expertise_rating,
                                 belong_english$lowerCI_english_comfort_rating,
                                 belong_expat$lowerCI_expatExpat,
                                 belong_lgbtq$lowerCI_lgbtqYes,
                                 belong_gender$lowerCI_genderMale),
                       upper = c(belong_expert$higherCI_expertise_rating,
                                 belong_english$higherCI_english_comfort_rating,
                                 belong_expat$higherCI_expatExpat,
                                 belong_lgbtq$higherCI_lgbtqYes,
                                 belong_gender$higherCI_genderMale),
                       lrt_pval = c(belong_expert$lrt_pval,
                                    belong_english$lrt_pval,
                                    belong_expat$lrt_pval,
                                    belong_lgbtq$lrt_pval,
                                    belong_gender$lrt_pval))

belong <- belong %>% mutate(sig = case_when(lrt_pval < 0.05 ~ "sig", TRUE ~ "nonsig"))
belong$factor <- factor(belong$factor, levels=c("Expertise", "English comfort", "Expat", "LGBTQ+", "Gender - male"))

ggplot(belong) +
  geom_point(aes(x = est, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none")+
  xlim(-1,1) -> initial_model_belong

# final model
belong_final <- read.csv("results/survey/model_belonging_final.csv")

belong_final_long <- data.frame(factor = c("English comfort", "Expertise"),
                               est = c(belong_final$est_english_comfort_rating,
                                       belong_final$est_expertise_rating),
                               lower = c(belong_final$lowerCI_english_comfort_rating,
                                         belong_final$lowerCI_expertise_rating),
                               upper = c(belong_final$higherCI_english_comfort_rating,
                                         belong_final$higherCI_expertise_rating),
                               pval = c(belong_final$pval_english_comfort_rating,
                                        belong_final$pval_expertise_rating))
belong_final_long <- belong_final_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

ggplot(belong_final_long) +
  geom_point(aes(x = est, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  xlim(-0.5, 1)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> final_model_belong

#combine

cowplot::plot_grid(initial_model_belong, final_model_belong,
                   ncol = 1, rel_heights = c(1,0.5),
                   align = "hv", axis = "lb") -> belong

ggsave(initial_model_belong, file = "plots/main/single/feeling_belong_initial.png", width=6, height=8)
ggsave(final_model_belong, file = "plots/main/single/feeling_belong_final.png", width=6, height=6)
ggsave(belong, file = "plots/main/single/feeling_belong_initial_final.png", width=8, height=10)
