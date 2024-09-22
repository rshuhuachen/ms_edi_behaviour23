#just use source("teams/EDI_ggplot_theme.R") at the start of your script to use the plotting theme
theme_set(theme_classic() + theme(title = element_text(size=16),
                                  plot.subtitle = element_text(size=14),
                                  axis.title = element_text(size = 18, family = "Arial"),
                                  axis.text = element_text(size = 16, family = "Arial"),
                                  text=element_text(size=14, family = "Arial"),
                                  axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),
                                                              color = "black"),
                                  plot.margin = margin(1.5,1,1,1, "cm"),
                                  axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                                              color = "black")))

pacman::p_load(prismatic, viridis)
clrs <- viridis::plasma(n = 12) %>% color()
#clrs <- MoMAColors::moma.colors("VanGogh", 12) %>% color()
