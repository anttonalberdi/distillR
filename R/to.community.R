#' Converts genome-xpecific GIFTs into the community level
#'
#' @param GIFT_table GIFT table at any hierarchical level
#' @param abundance_table (Relative) abundance table with samples in columns and Genomes in rows. Required for computing sample-specific GIFT values
#' @param GIFT_db Table containing definitions and metadata of GIFTs (default: database provided by distillR)
#' @import tidyverse
#' @import dplyr
#' @return A matirx with community-level GIFTs per sample
#' @examples
#' to.community(functions_table,GIFT_db)
#' @export


to.community <- function(GIFT_table,abundance_table,GIFT_db){

  #Declare TSS function
  tss <- function(abund){sweep(abund, 2, colSums(abund), FUN="/")}

  #Convert tables into data frames
  if(!missing(GIFT_table)){GIFT_table <- as.data.frame(GIFT_table)}
  if(!missing(abundance_table)){abundance_table <- as.data.frame(abundance_table)}
  if(!missing(GIFT_db)){GIFT_db <- as.data.frame(GIFT_db)}

  if(missing(abundance_table)){
      #If abundance table does not exist, create a mock abundance table of a single even community
      cat("\tAs no relative abundance information was provided distillR\n")
      cat("will generate a single community with evenly weighed genomes\n")
      abundance_table <- rep(1/nrow(GIFT_table),nrow(GIFT_table))
      names(abundance_table) <- rownames(abundance_table)
      abundance_table <- t(t(abundance_table))
      colnames(abundance_table) <- "Community"

      #Declare single community
      communities <- "Community"
    }else{
      #Declare communities from abundance table
      communities <- colnames(abundance_table)
    }


  community_table <- t(as.data.frame(t(functions_table)) %>%
    rownames_to_column('Code_function') %>%
    left_join(GIFT_db[,c('Code_function', 'Type')], by = 'Code_function') %>%
    #Calculate Biosynthesis and Degradation MCIs
    group_by(Type) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    #Calculate overall MCI
    ungroup %>%
    summarise(Type = c(Type, 'Overall'), across(where(is.numeric), ~ c(., mean(.)))) %>%
    arrange(factor(Type, levels = unique(GIFT_db$Type))) %>%
    column_to_rownames('Type'))

  return(community_table)
}
