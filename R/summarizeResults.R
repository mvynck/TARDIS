#' Summarize results of targeted integration
#'
#' @param results A `data.frame` object containing all the metrics for
#'     every compound in every sample
#'
#' @examples
#' @author Pablo Vangeenderhuysen
.summarizeResults <- function(results, output_directory) {
  auc_table <- results %>%
    select(Component, Sample, AUC) %>%
    spread(Sample, AUC, fill = NA, drop = FALSE)
  write.csv(auc_table, file = paste0(output_directory, "auc_table.csv"))
  pop_table <- results %>%
    select(Component, Sample, pop) %>%
    spread(Sample, pop, fill = NA, drop = FALSE)
  write.csv(pop_table, file = paste0(output_directory, "pop_table.csv"))
  SNR_table <- results %>%
    select(Component, Sample, SNR) %>%
    spread(Sample, SNR, fill = NA, drop = FALSE)
  write.csv(SNR_table, file = paste0(output_directory, "snr_table.csv"))
  int_table <- results %>%
    select(Component, Sample, MaxInt) %>%
    spread(Sample, MaxInt, fill = NA, drop = FALSE)
  write.csv(int_table, file = paste0(output_directory, "int_table.csv"))
  peakcor_table <- results %>%
    select(Component, Sample, peak_cor) %>%
    spread(Sample, peak_cor, fill = NA, drop = FALSE)
  write.csv(peakcor_table,
            file = paste0(output_directory,
                          "peakcor_table.csv"))
  ## summarize feature table based on QC's
  QC_results <-  results[grep("QC", results$Sample), ]
  mean_metrics_table <- QC_results %>%
    group_by(Component) %>%
    summarise_at(vars(-Sample), list(~ if (is.numeric(.))
      mean(., na.rm = TRUE)
      else
        dplyr::first(.)))
  sd_metrics_table <- QC_results %>%
    group_by(Component) %>%
    summarise_at(vars(AUC), list(~ if (is.numeric(.))
      sd(., na.rm = TRUE)
      else
        dplyr::first(.)))
  mean_metrics_table$CV <-
    sd_metrics_table$AUC / mean_metrics_table$AUC
  mean_metrics_table$mean <- NULL
  mean_metrics_table$ID <- NULL
  mean_metrics_table$na.rm <- NULL
  for(i in 1:nrow(mean_metrics_table))
    if(!is.na(mean_metrics_table$CV[i]) && mean_metrics_table$CV[i] > 0.4)
      mean_metrics_table$warning[i] = "CV is > 0.3"
  write_xlsx(avg_metrics_table,
             paste0(output_directory,
                    "feat_table.xlsx"))
}
