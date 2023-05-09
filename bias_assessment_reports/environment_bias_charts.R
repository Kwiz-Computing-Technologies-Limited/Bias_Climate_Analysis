library(here)


if(dir.exists(here("bias_assessment_reports", "envBias_ellipses"))) {
  dir.create(here("bias_assessment_reports", "envBias_ellipses"))
}

env_bias_tables = list.files(here("13.  bias assessment results"))
env_bias_tables = env_bias_tables[! (env_bias_tables %in% c("run_bias_assessments-invaded-server.R",
                       "run_bias_assessments-native-server.R",
                       "run_bias_assessments.Rmd",
                       "spatial_thin_log.txt" ,
                       "run_bias_assessments.R",
                       "gbr-native-filtered",
                       "irl-nir-native-filtered",
                       "nzl-Glonaf-native-filtered",
                       "nzl_glonaf_invaded_clean",
                       "usa-native-filtered",
                       "usa_invaded_clean",
                       "aus_glonaf-native-filtered",
                       "aus_glonaf_invaded_clean-v1"
                       ))]



charts = function(region) {

  path_to_file = here("13.  bias assessment results", region, stringr::str_c(region, "_periods_length_10_assessEnvBias_output.csv"))  
  data = readr::read_csv(path_to_file) |> as.data.frame()
  
  
  # plot the ellipses
  p <- ggplot2::ggplot(data = data, 
                       ggplot2::aes(x = data[, (2 + xPC)],
                                    y = data[, (2 + yPC)], 
                                    colour = Period, group = Period,
                                    fill = factor(ifelse(Period == "background", "background", "samples")))) + 
    
    ggplot2::stat_ellipse(type = "norm", geom = "polygon", alpha = 0.25) +
    ggplot2::scale_fill_manual(aesthetics = "fill", values = c("red", "white")) + ggplot2::facet_wrap(~identifier) +
    ggplot2::labs(x = paste0("PC", xPC, " (", round(data$xVar[1], 2), "%)"),
                  y = paste0("PC", yPC, " (", round(data$yVar[1], 2), "%)"),
                  fill = "") +
    ggplot2::theme_linedraw()
  
  # chart name
  name = here("bias_assessment_reports", "envBias_ellipses", stringr::str_c(region, "envBias_ellipses.png", sep = "_"))
  
  # save chart
  
  ggsave(name, p, width = 25, height = 25, units = c("in"))
}



for (i in 1:length(env_bias_tables)) {
  charts(region = env_bias_tables[i])
}

