#### packages ####

pacman::p_load(tidyverse, data.table)
source("scripts/EDI_ggplot_theme.R")

col_sig <- clrs[c(10,2)] %>%
  color() %>% 
  set_names(nm = c("sig", "nonsig"))

#### individual plots ####

### feeling diversity ####
diversity_gender <- read.csv("results/survey/model_diversity_gender.csv")
diversity_english <- read.csv("results/survey/model_diversity_english.csv")
diversity_expat <- read.csv("results/survey/model_diversity_expat.csv")
diversity_lgbtq <- read.csv("results/survey/model_diversity_lgbtq.csv")
diversity_nat <- read.csv("results/survey/model_diversity_nat.csv")
diversity_career <- read.csv("results/survey/model_diversity_career.csv")

diversity <- data.frame(factor = c("Career stage - late", "English comfort", "Expat", "LGBTQ+", "Gender - male"),
                    est = c(diversity_career$est_career_3catLate,
                            diversity_english$est_english_comfort_rating,
                            diversity_expat$est_expatExpat,
                            diversity_lgbtq$est_lgbtqYes,
                            diversity_gender$est_genderMale),
                    lower = c(diversity_career$lowerCI_career_3catLate,
                              diversity_english$lowerCI_english_comfort_rating,
                              diversity_expat$lowerCI_expatExpat,
                              diversity_lgbtq$lowerCI_lgbtqYes,
                              diversity_gender$lowerCI_genderMale),
                    upper = c(diversity_career$higherCI_career_3catLate,
                              diversity_english$higherCI_english_comfort_rating,
                              diversity_expat$higherCI_expatExpat,
                              diversity_lgbtq$higherCI_lgbtqYes,
                              diversity_gender$higherCI_genderMale),
                    lrt_pval = c(diversity_career$lrt_pval,
                                 diversity_english$lrt_pval,
                                 diversity_expat$lrt_pval,
                                 diversity_lgbtq$lrt_pval,
                                 diversity_gender$lrt_pval))

diversity <- diversity %>% mutate(sig = case_when(lrt_pval < 0.05 ~ "sig", TRUE ~ "nonsig"))
diversity$factor <- factor(diversity$factor, levels=c("Career stage - late", "English comfort", "Expat", "LGBTQ+", "Gender - male"))

ggplot(diversity) +
  geom_point(aes(x = est, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none")+
  xlim(-1.5,1.5) -> initial_model_diversity

# final model
diversity_final <- read.csv("results/survey/model_diversity_final.csv")

diversity_final_long <- data.frame(factor = c("Gender - male", "Gender - non binary",
                                              "LGBTQ+"),
                               est = c(diversity_final$est_genderMale,
                                       diversity_final$est_genderNon.binary,
                                       diversity_final$est_lgbtqYes),
                               lower = c(diversity_final$lowerCI_genderMale,
                                         diversity_final$lowerCI_genderNon.binary,
                                         diversity_final$lowerCI_lgbtqYes),
                               upper = c(diversity_final$higherCI_genderMale,
                                         diversity_final$higherCI_genderNon.binary,
                                         diversity_final$higherCI_lgbtqYes),
                               pval = c(diversity_final$pval_genderMale,
                                        diversity_final$pval_genderNon.binary,
                                        diversity_final$pval_lgbtqYes))
diversity_final_long <- diversity_final_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

diversity_final_long$factor <- factor(diversity_final_long$factor, 
                                      levels=c("LGBTQ+",  "Gender - non binary",
                                               "Gender - male"))
ggplot(diversity_final_long) +
  geom_point(aes(x = est, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
 # xlim(-1.5, 1.5)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> final_model_diversity

#combine

cowplot::plot_grid(initial_model_diversity, final_model_diversity,
                   ncol = 1, rel_heights = c(1,0.5),
                   align = "hv", axis = "lb") -> diversity

ggsave(initial_model_diversity, file = "plots/main/single/perception_diversity_initial.png", width=6, height=8)
ggsave(final_model_diversity, file = "plots/main/single/perception_diversity_final.png", width=6, height=6)
ggsave(diversity, file = "plots/main/single/perception_diversity_initial_final.png", width=8, height=10)


### edi issues ####
edi_issues_gender <- read.csv("results/survey/model_edi_issues_gender.csv")
edi_issues_english <- read.csv("results/survey/model_edi_issues_english.csv")
edi_issues_expat <- read.csv("results/survey/model_edi_issues_expat.csv")
edi_issues_lgbtq <- read.csv("results/survey/model_edi_issues_lgbtq.csv")
edi_issues_nat <- read.csv("results/survey/model_edi_issues_nat.csv")
edi_issues_career <- read.csv("results/survey/model_edi_issues_career.csv")

edi_issues <- data.frame(factor = c("Career stage - late", "English comfort", 
                                    "Expat", "LGBTQ+", "Gender - male"),
                        est = c(edi_issues_career$est_career_3catLate,
                                edi_issues_english$est_english_comfort_rating,
                                edi_issues_expat$est_expatExpat,
                                edi_issues_lgbtq$est_lgbtqYes,
                                edi_issues_gender$est_genderMale),
                        lower = c(edi_issues_career$lowerCI_career_3catLate,
                                  edi_issues_english$lowerCI_english_comfort_rating,
                                  edi_issues_expat$lowerCI_expatExpat,
                                  edi_issues_lgbtq$lowerCI_lgbtqYes,
                                  edi_issues_gender$lowerCI_genderMale),
                        upper = c(edi_issues_career$higherCI_career_3catLate,
                                  edi_issues_english$higherCI_english_comfort_rating,
                                  edi_issues_expat$higherCI_expatExpat,
                                  edi_issues_lgbtq$higherCI_lgbtqYes,
                                  edi_issues_gender$higherCI_genderMale),
                        lrt_pval = c(edi_issues_career$lrt_pval,
                                     edi_issues_english$lrt_pval,
                                     edi_issues_expat$lrt_pval,
                                     edi_issues_lgbtq$lrt_pval,
                                     edi_issues_gender$lrt_pval))

edi_issues <- edi_issues %>% mutate(sig = case_when(lrt_pval < 0.05 ~ "sig", TRUE ~ "nonsig"))
edi_issues$factor <- factor(edi_issues$factor, levels=c("Career stage - late", "English comfort", "Expat", "LGBTQ+", "Gender - male"))

ggplot(edi_issues) +
  geom_point(aes(x = est, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none")+
  xlim(-1.5,1.5) -> initial_model_edi_issues

# final model
edi_issues_final <- read.csv("results/survey/model_edi_issues_final.csv")

edi_issues_final_long <- data.frame(factor = c("Gender - male", "Gender - non binary",
                                              "LGBTQ+", "Expat",
                                              "Nationality - Asia", "Nationality - North America",
                                              "Nationality - Oceania", "Nationality - South America"),
                                   est = c(edi_issues_final$est_genderMale,
                                           edi_issues_final$est_genderNon.binary,
                                           edi_issues_final$est_lgbtqYes,
                                           edi_issues_final$est_expatExpat,
                                           edi_issues_final$est_nationality_continentAsia,
                                           edi_issues_final$est_nationality_continentNorth.America,
                                           edi_issues_final$est_nationality_continentOceania, 
                                           edi_issues_final$est_nationality_continentSouth.America),
                                   lower = c(edi_issues_final$lowerCI_genderMale,
                                             edi_issues_final$lowerCI_genderNon.binary,
                                             edi_issues_final$lowerCI_lgbtqYes,
                                             edi_issues_final$lowerCI_expatExpat,
                                             edi_issues_final$lowerCI_nationality_continentAsia,
                                             edi_issues_final$lowerCI_nationality_continentNorth.America,
                                             edi_issues_final$lowerCI_nationality_continentOceania,
                                             edi_issues_final$lowerCI_nationality_continentSouth.America),
                                   upper = c(edi_issues_final$higherCI_genderMale,
                                             edi_issues_final$higherCI_genderNon.binary,
                                             edi_issues_final$higherCI_lgbtqYes,
                                             edi_issues_final$higherCI_expatExpat,
                                             edi_issues_final$higherCI_nationality_continentAsia,
                                             edi_issues_final$higherCI_nationality_continentNorth.America,
                                             edi_issues_final$higherCI_nationality_continentOceania,
                                             edi_issues_final$higherCI_nationality_continentSouth.America),
                                   pval = c(edi_issues_final$pval_genderMale,
                                            edi_issues_final$pval_genderNon.binary,
                                            edi_issues_final$pval_lgbtqYes,
                                            edi_issues_final$pval_expatExpat,
                                            edi_issues_final$pval_nationality_continentAsia,
                                            edi_issues_final$pval_nationality_continentNorth.America,
                                            edi_issues_final$pval_nationality_continentOceania,
                                            edi_issues_final$pval_nationality_continentSouth.America))

edi_issues_final_long <- edi_issues_final_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

edi_issues_final_long$factor <- factor(edi_issues_final_long$factor, 
                                      levels=c( "Nationality - Asia", "Nationality - North America",
                                                "Nationality - Oceania", "Nationality - South America",
                                                "Expat", "LGBTQ+",  "Gender - non binary",
                                               "Gender - male"))
ggplot(edi_issues_final_long) +
  geom_point(aes(x = est, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  # xlim(-1.5, 1.5)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> final_model_edi_issues

#combine

cowplot::plot_grid(initial_model_edi_issues, final_model_edi_issues,
                   ncol = 1, rel_heights = c(1,0.5),
                   align = "hv", axis = "lb") -> edi_issues

ggsave(initial_model_edi_issues, file = "plots/main/single/perception_edi_issues_initial.png", width=6, height=8)
ggsave(final_model_edi_issues, file = "plots/main/single/perception_edi_issues_final.png", width=6, height=10)
ggsave(edi_issues, file = "plots/main/single/perception_edi_issues_initial_final.png", width=8, height=10)


### gender disparity ####
gender_qa_gender <- read.csv("results/survey/model_gender_qa_gender.csv")
gender_qa_english <- read.csv("results/survey/model_gender_qa_english.csv")
gender_qa_expat <- read.csv("results/survey/model_gender_qa_expat.csv")
gender_qa_lgbtq <- read.csv("results/survey/model_gender_qa_lgbtq.csv")
gender_qa_nat <- read.csv("results/survey/model_gender_qa_nat.csv")
gender_qa_career <- read.csv("results/survey/model_gender_qa_career.csv")

gender_qa <- data.frame(factor = c("Career stage - late", "English comfort", "Expat", "LGBTQ+", "Gender - male"),
                         est = c(gender_qa_career$est_career_3catLate,
                                 gender_qa_english$est_english_comfort_rating,
                                 gender_qa_expat$est_expatExpat,
                                 gender_qa_lgbtq$est_lgbtqYes,
                                 gender_qa_gender$est_genderMale),
                         lower = c(gender_qa_career$lowerCI_career_3catLate,
                                   gender_qa_english$lowerCI_english_comfort_rating,
                                   gender_qa_expat$lowerCI_expatExpat,
                                   gender_qa_lgbtq$lowerCI_lgbtqYes,
                                   gender_qa_gender$lowerCI_genderMale),
                         upper = c(gender_qa_career$higherCI_career_3catLate,
                                   gender_qa_english$higherCI_english_comfort_rating,
                                   gender_qa_expat$higherCI_expatExpat,
                                   gender_qa_lgbtq$higherCI_lgbtqYes,
                                   gender_qa_gender$higherCI_genderMale),
                         lrt_pval = c(gender_qa_career$lrt_pval,
                                      gender_qa_english$lrt_pval,
                                      gender_qa_expat$lrt_pval,
                                      gender_qa_lgbtq$lrt_pval,
                                      gender_qa_gender$lrt_pval))

gender_qa <- gender_qa %>% mutate(sig = case_when(lrt_pval < 0.05 ~ "sig", TRUE ~ "nonsig"))
gender_qa$factor <- factor(gender_qa$factor, levels=c("Career stage - late", "English comfort", "Expat", "LGBTQ+", "Gender - male"))

ggplot(gender_qa) +
  geom_point(aes(x = est, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none")+
  xlim(-1.5,1.5) -> initial_model_gender_qa

# final model
gender_qa_final <- read.csv("results/survey/model_gender_qa_final.csv")


gender_qa_final_long <- data.frame(factor = c("Gender - male", "Gender - non binary",
                                              "LGBTQ+", "English comfort",
                                              "Nationality - North Europe", "Nationality - South Europe",
                                              "Nationality - East Europe", "Nationality - North America",
                                              "Nationality - Oceania", "Nationality - South America",
                                              "Nationality - Asia", 
                                              "Affiliation - Asia", "Affiliation - North America",
                                              "Affiliation - Oceania", "Affiliation - South America"),
                                   est = c(gender_qa_final$est_genderMale,
                                            gender_qa_final$est_genderNon.binary,
                                            gender_qa_final$est_lgbtqYes,
                                            gender_qa_final$est_english_comfort_rating,
                                            gender_qa_final$est_nationality_subcontinentNorth.Europe,
                                           gender_qa_final$est_nationality_subcontinentSouth.Europe,
                                           gender_qa_final$est_nationality_subcontinentEast.Europe,
                                           gender_qa_final$est_nationality_subcontinentNorth.America,
                                           gender_qa_final$est_nationality_subcontinentOceania,
                                           gender_qa_final$est_nationality_subcontinentSouth.American,
                                           gender_qa_final$est_nationality_subcontinentAsia,
                                            gender_qa_final$est_affiliation_continentAsia,
                                           gender_qa_final$est_affiliation_continentNorth.America,
                                           gender_qa_final$est_affiliation_continentOceania,
                                           gender_qa_final$est_affiliation_continentSouth.America),
                                    lower = c(gender_qa_final$lowerCI_genderMale,
                                              gender_qa_final$lowerCI_genderNon.binary,
                                              gender_qa_final$lowerCI_lgbtqYes,
                                              gender_qa_final$lowerCI_english_comfort_rating,
                                              gender_qa_final$lowerCI_nationality_subcontinentNorth.Europe,
                                              gender_qa_final$lowerCI_nationality_subcontinentSouth.Europe,
                                              gender_qa_final$lowerCI_nationality_subcontinentEast.Europe,
                                              gender_qa_final$lowerCI_nationality_subcontinentNorth.America,
                                              gender_qa_final$lowerCI_nationality_subcontinentOceania,
                                              gender_qa_final$lowerCI_nationality_subcontinentSouth.American,
                                              gender_qa_final$lowerCI_nationality_subcontinentAsia,
                                              gender_qa_final$lowerCI_affiliation_continentAsia,
                                              gender_qa_final$lowerCI_affiliation_continentNorth.America,
                                              gender_qa_final$lowerCI_affiliation_continentOceania,
                                              gender_qa_final$lowerCI_affiliation_continentSouth.America),
                                    upper = c(gender_qa_final$higherCI_genderMale,
                                              gender_qa_final$higherCI_genderNon.binary,
                                              gender_qa_final$higherCI_lgbtqYes,
                                              gender_qa_final$higherCI_english_comfort_rating,
                                              gender_qa_final$higherCI_nationality_subcontinentNorth.Europe,
                                              gender_qa_final$higherCI_nationality_subcontinentSouth.Europe,
                                              gender_qa_final$higherCI_nationality_subcontinentEast.Europe,
                                              gender_qa_final$higherCI_nationality_subcontinentNorth.America,
                                              gender_qa_final$higherCI_nationality_subcontinentOceania,
                                              gender_qa_final$higherCI_nationality_subcontinentSouth.American,
                                              gender_qa_final$higherCI_nationality_subcontinentAsia,
                                              gender_qa_final$higherCI_affiliation_continentAsia,
                                              gender_qa_final$higherCI_affiliation_continentNorth.America,
                                              gender_qa_final$higherCI_affiliation_continentOceania,
                                              gender_qa_final$higherCI_affiliation_continentSouth.America),
                                    pval = c(gender_qa_final$pval_genderMale,
                                             gender_qa_final$pval_genderNon.binary,
                                             gender_qa_final$pval_lgbtqYes,
                                             gender_qa_final$pval_english_comfort_rating,
                                             gender_qa_final$pval_nationality_subcontinentNorth.Europe,
                                             gender_qa_final$pval_nationality_subcontinentSouth.Europe,
                                             gender_qa_final$pval_nationality_subcontinentEast.Europe,
                                             gender_qa_final$pval_nationality_subcontinentNorth.America,
                                             gender_qa_final$pval_nationality_subcontinentOceania,
                                             gender_qa_final$pval_nationality_subcontinentSouth.American,
                                             gender_qa_final$pval_nationality_subcontinentAsia,
                                             gender_qa_final$pval_affiliation_continentAsia,
                                             gender_qa_final$pval_affiliation_continentNorth.America,
                                             gender_qa_final$pval_affiliation_continentOceania,
                                             gender_qa_final$pval_affiliation_continentSouth.America))

gender_qa_final_long <- gender_qa_final_long %>% mutate(sig = case_when(pval < 0.05 ~ "sig", TRUE ~ "nonsig"))

gender_qa_final_long$factor <- factor(gender_qa_final_long$factor, 
                                       levels=c("Affiliation - Asia", "Affiliation - North America",
                                                "Affiliation - Oceania", "Affiliation - South America",
                                                "Nationality - North Europe", "Nationality - South Europe",
                                                "Nationality - East Europe", "Nationality - North America",
                                                "Nationality - Oceania", "Nationality - South America",
                                                "Nationality - Asia","English comfort", "LGBTQ+", 
                                                "Gender - non binary","Gender - male"))
ggplot(gender_qa_final_long) +
  geom_point(aes(x = est, y = factor, col = sig), size = 5) + 
  geom_segment(aes(x = lower, xend = upper, y = factor),
               linewidth=0.5) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
  labs(x = "Estimate and 95% CI", y = "Social identity") + 
  # xlim(-1.5, 1.5)+
  scale_color_manual(values = col_sig) +
  theme(legend.position = "none") -> final_model_gender_qa

#combine

#cowplot::plot_grid(initial_model_gender_qa, final_model_gender_qa,
#                   ncol = 1, rel_heights = c(1,0.5),
#                   align = "hv", axis = "lb") -> gender_qa

ggsave(initial_model_gender_qa, file = "plots/main/single/perception_gender_qa_initial.png", width=6, height=8)
ggsave(final_model_gender_qa, file = "plots/main/single/perception_gender_qa_final.png", width=8, height=12)
#ggsave(gender_qa, file = "plots/main/single/perception_gender_qa_initial_final.png", width=8, height=10)
