#' ---------------------------------------
#' Add interpolated values for  Downeast using the MDI Buoy
#' ---------------------------------------

 #when downeast has no data, interpolate from MDI
#sum(is.na(mdi_down$downeast))/nrow(mdi_down) #how much is missing
mdi_down <- temp_regional %>%
    select(-variable) %>%
    filter(as.character(region) %in% c("mdi", "downeast")) %>%
    tidyr::pivot_wider(names_from = "region",
                       values_from = "value")

ggplot(mdi_down,
       aes(x = mdi, y = downeast)) +
    geom_point(shape = 1) +
    #    geom_abline(slope = 1, intercept = 0, lty = 2, color = "red") +
    stat_smooth(method = "lm", color = "blue") +
    geom_hline(yintercept = 17, color = "orange") + facet_wrap(~year)

#fit a model detailing the relationship between the MDI temps and downeast
mdi_down_lm <- lm(downeast ~ mdi , data = mdi_down)
anova(mdi_down_lm)
summary(mdi_down_lm)$r.squared

# interpolate missing temperatures
mdi_down_int <- mdi_down %>%
    modelr::add_predictions(mdi_down_lm) %>%
    mutate(downeast = ifelse(is.na(downeast), pred, downeast)) %>%
    select(-pred) %>%
    tidyr::pivot_longer(cols =  c("mdi", "downeast"),
                        names_to = "region",
                        values_to = "value") %>%
    mutate(variable = ifelse(region == "mdi", "I01.1m", "noaa44027.1m"))

# add interpolated data back
temp_regional_interpolated <- temp_regional %>%
    filter(!(as.character(region) %in% c("mdi", "downeast"))) %>%
    bind_rows(mdi_down_int)
