#' Summarize results of targeted integration
#'
#' @param results A `data.frame` object containing all the metrics for
#'     every compound in every sample
#' @param output_directory description
#' @param int_std_id description
#' @examples
#' @author Pablo Vangeenderhuysen
.summarizeResults <- function(results, output_directory,int_std_id) {
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
  NAs <- QC_results %>%
    group_by(Component) %>%
    summarise_at(vars(AUC),funs(sum(is.na(.))))
  mean_metrics_table$NFs <- NAs$AUC
  mean_metrics_table$CV <-
    sd_metrics_table$AUC / mean_metrics_table$AUC
  mean_metrics_table$mean <- NULL
  mean_metrics_table$ID <- NULL
  mean_metrics_table$na.rm <- NULL
  mean_metrics_table$foundRT <- NULL
  int_std_metrics <-
    mean_metrics_table[which(mean_metrics_table$Component %in% int_std_id),]
  mean_metrics_table$warning <- NA
  for(i in 1:nrow(mean_metrics_table)){
    if(mean_metrics_table$peak_cor[i] >= 0.9 * mean(int_std_metrics$peak_cor)){
      mean_metrics_table$warning[i] <- "Good"
    }
        else if(mean_metrics_table$peak_cor[i] >= 0.8 * mean(int_std_metrics$peak_cor)){
          mean_metrics_table$warning[i] <- "Ambiguous"
        }
        else{
          mean_metrics_table$warning[i] <- "Bad"
        }
  }
  mean_metrics_table$trold <- mean_metrics_table$trold / 60
  mean_metrics_table$tr <- round(mean_metrics_table$tr / 60, digits = 2)
  mean_metrics_table$CV <- round(mean_metrics_table$CV,2)
  mean_metrics_table$peak_cor <- round(mean_metrics_table$peak_cor,2)
  mean_metrics_table$SNR <- round(mean_metrics_table$SNR,2)
  write_xlsx(avg_metrics_table,
             paste0(output_directory,
                    "feat_table.xlsx"))
}
