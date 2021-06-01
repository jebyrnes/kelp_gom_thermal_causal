
# heatmap-a-palooza
make_kelp_heatmap <- function(mean_temp_mn = 15, urchin_mn = 0,
                              xlim = c(0,30), mod = mod_urchin_add){
    
    visreg::visreg2d(mod, xvar = "urchin_dev", yvar = "mean_temp_dev", 
                     scale = "response", plot.type="gg", 
                     cond = list(urchin_mn = urchin_mn,
                                 mean_temp_mn = mean_temp_mn)) +
        scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                             midpoint = 0.5, limits = c(0,1)) +
        xlim(xlim) +
        labs(fill = "Percent Cover of Kelp",
             y = "Deviation from Regional Temperature Mean",
             x = "Deviation from Regional Urchin Mean",
             subtitle = paste0("Assumes regional mean summer temp of ",
                               mean_temp_mn, "C \nand urchin abundance of ", urchin_mn," per sq m"))
    
    
}
