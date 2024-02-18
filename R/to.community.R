#' Converts genome-xpecific GIFTs into the community level
#'
#' @param GIFT_table GIFT table at any hierarchical level
#' @param abundance_table (Relative) abundance table with samples in columns and Genomes in rows. Required for computing sample-specific GIFT values
#' @param GIFT_db Table containing definitions and metadata of GIFTs (default: database provided by distillR)
#' @import tidyverse
#' @import dplyr
#' @return A matirx with community-level GIFTs per sample
#' @examples
#' to.community(functions_table,abundance_table,GIFT_db)
#' @export


to.community <- function(GIFT_table,abundance_table,GIFT_db=GIFT_db){

  #Declare TSS function
  tss <- function(abund){sweep(abund, 2, colSums(abund), FUN="/")}

  #Convert tables into data frames
  if(!missing(GIFT_table)){GIFT_table <- as.data.frame(GIFT_table)}
  if(!missing(abundance_table)){abundance_table <- as.data.frame(abundance_table)}
  if(!missing(GIFT_db)){GIFT_db <- as.data.frame(GIFT_db)}

  if(missing(abundance_table)){
      #If abundance table does not exist, create a mock abundance table of a single even community
      cat("\tWARNING: As no relative abundance information was provided distillR\n")
      cat("\twill generate a single community with evenly weighed genomes\n")
      abundance_table <- rep(1/nrow(GIFT_table),nrow(GIFT_table))
      abundance_table <- t(t(abundance_table))
      rownames(abundance_table) <- rownames(GIFT_table)
      colnames(abundance_table) <- "GIFT"

      #Declare single community
      communities <- "GIFT"
    }else{
      #Declare communities from abundance table
      communities <- colnames(abundance_table)
    }

  GIFT_community_table <- c()
  m=0
  for(community in communities){
    m=m+1
    community_GIFT <- colSums(sweep(GIFT_table,1,abundance_table[,m], FUN="*"))
    GIFT_community_table <- rbind(GIFT_community_table,community_GIFT)
  }
  rownames(GIFT_community_table) <- communities

  return(GIFT_community_table)
}
