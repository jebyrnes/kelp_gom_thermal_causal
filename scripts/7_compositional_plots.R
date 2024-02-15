#' -------------------------------------------
#' Plot benthic community composition data
#' 
#' @author Jarrett Byrnes
#' @date 2023-10-30 last update
#' -------------------------------------------

#--- load libraries and get environment setup
setwd(here::here()) 
library(tidyverse)

comp_data <- read_csv("derived_data/compositional_change_data.csv") %>%
    mutate( region = factor(region,
                            levels = rev(c("Downeast", "MDI", "Penobscot Bay",
                                       "Midcoast", "Casco Bay", "York"))),
            year = factor(year, levels = c(2018, 2004)))

#labeller function
make_kelp_name <- function(string){
    string <- gsub("agar", "Agarum", string)
    string <- gsub("ldig", "Laminaria", string)
    string <- gsub("alar", "Alaria", string)
    string <- gsub("sac", "Saccharina", string)
    string
}

ggplot(comp_data %>% filter(type == "kelp"), 
       aes(x = region, y = cover, color = year)) +
    geom_point(alpha = 0.2, position = position_dodge(width = 1)) +
    stat_summary(alpha = 1, position = position_dodge(width = 1),
                 fun.data = mean_cl_boot) +
    coord_flip() +
    facet_wrap(vars(sp_code),
               labeller = labeller(sp_code = make_kelp_name),
               nrow = 1) +
    labs(color = "Year",
         y = "% Cover",
         x = "") +
    ggthemes::theme_clean() +
    scale_color_brewer(palette = "Accent")+
    guides(colour = guide_legend(reverse=TRUE)) +  #for the legend flip
    theme(strip.text = element_text(face = "italic"))
ggsave("figures/kelp_composition_2004_2018.jpg", dpi = 600)



#labeller function
make_algae_name <- function(string){
    stringi::stri_replace_all_fixed(string,
                                   pattern = c("ccrisp",
                                     "chaet",
                                     "codm",
                                     "coral",
                                     "desm",
                                     "palm",
                                     "phyc",
                                     "poly",
                                     "porph",
                                     "ptilo",
                                     "rhod",
                                     "ulva"),
                                   replacement = c(
                                     "Chondrus",
                                     "Chaetomorpha",
                                     "Codium",
                                     "Corallina",
                                     "Desmarestia",
                                     "Palmaria",
                                     "Phycodrys",
                                     "Filamentous Reds",
                                     "Porphyra",
                                     "Ptilota",
                                     "Rhodomela",
                                     "Ulva"),
                                   vectorize_all = FALSE)
    
}
ggplot(comp_data %>% filter(type != "kelp"), 
       aes(x = region, y = cover, color = factor(year))) +
    geom_point(alpha = 0.2, position = position_dodge(width = 1)) +
    stat_summary(alpha = 1, position = position_dodge(width = 1),
                 fun.data = mean_cl_boot) +
    coord_flip() +
    facet_wrap(vars(sp_code),
               labeller = labeller(sp_code = make_algae_name)) +
    labs(color = "Year",
         y = "% Cover",
         x = "") +
    ggthemes::theme_clean() +
    scale_color_brewer(palette = "Accent")+
    guides(colour = guide_legend(reverse=TRUE)) + 
    theme(strip.text = element_text(face = "italic"))

ggsave("figures/understory_composition_2004_2018.jpg", 
       height = 5.2,
       width = 6.8,
       dpi = 600)

