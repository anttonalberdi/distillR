#' Generates a gene presence-based Genome-Inferred Functional Trait (GIFT) table at the gene bundle level from a bacterial genome annotation table
#'
#' @param annotation_table Table containing Genome identifiers and gene annotations
#' @param GIFT_db Table containing definitions and metadata of GIFTs (default: database provided by distillR)
#' @param genomecol Column index (number) of the annotation_table containing the genome identifiers
#' @param annotcol Column index(es) of the annotation_table in which to search for gene identifiers (e.g., c(3,4,5))
#' @param stats Whether to calculate and print distillation statistics
#' @importFrom stringr str_extract
#' @return A gene bundle-level GIFT table
#' @examples
#' distill(annotation_table,GIFT_db,genomecol,annotcol, stats)
#' distill(annotation_table,GIFT_db,genomecol=2,annotcol=c(9,10,19),stats=T)
#' @export

distill <- function(annotation_table,GIFT_db,genomecol=2,annotcol=c(9,10,19),stats=T){

  #Sanity check
  if(missing(annotation_table)) stop("Genome annotation table is missing")
  if(missing(GIFT_db)) stop("Pathway database is missing")
  if(length(genomecol)!=1) stop("The argument genomecol must be an integer indicating the number of the column containing the Genome identifiers in the annotations table")
  if(missing(annotcol)) stop("Specify at least one column containing functional annotations")

  #Convert annotation table to data frame
  annotation_table <- as.data.frame(annotation_table)

  #Convert pathway database to data frame
  GIFT_db <- as.data.frame(GIFT_db)

  #List Genomes
  if(!missing(genomecol)){
    Genomes <- unique(annotation_table[,genomecol])
  }else{
    Genomes <- "GIFT"
  }

  #Verbosity
  if(length(Genomes)>1){
    cat("Calculating GIFTs for",length(Genomes),"genomes:\n")
  }else{
    cat("Calculating GIFTs for a single genome.\n")
    cat("\tNote: If you were expecting multiple genomes\n")
    cat("\tensure the genome identifier column is correctly specified.\n")
  }

  #Calculate GIFTs for each Genome iteratively
  GIFT_table <- c()
  m=0
  for(Genome in Genomes){
    m=m+1
    if(length(Genomes)>1){
      cat("\t",Genome," (",m,"/",length(Genomes),")\n", sep = "")
      #Fetch Genome annotations
      annotations_Genome <- annotation_table[annotation_table[,genomecol] == Genome,]
    }else{
      annotations_Genome <- annotation_table
    }

    #Create vector of identifiers
    Identifier_vector <- str_extract(c(unlist(c(annotations_Genome[,annotcol]))), "K[0-9][0-9][0-9][0-9][0-9]|(?<=\\[EC:).+?(?=\\])") %>% #Parse identifiers (KEGG|EC)
          unique() %>% #Dereplicate
          .[!is.na(.)] %>% #Remove NAs
          strsplit(., " ") %>% unlist() %>%#Split multiple identifiers
          .[!grepl("-", ., fixed = TRUE)] #Remove ambiguous EC codes

    #Calculate GIFTs for each Pathway and append to vector
    GIFT_vector <- c()
    suppressWarnings(
      for(f in c(1:nrow(GIFT_db))){
        definition=GIFT_db[f,"Definition"]
        GIFT <- compute_GIFT(definition,Identifier_vector)
        GIFT_vector <- c(GIFT_vector,GIFT)
      }
    )
    #Append GIFT vector of the Genome to the GIFT table containing GIFT values of all Genomes
    GIFT_table <- rbind(GIFT_table,GIFT_vector)

  }

  #Report statistics
  if(stats == TRUE){
    db_identifiers <- unique(unlist(strsplit(paste(GIFT_db$Definition, collapse = " ")," |\\,|\\)|\\(|\\+")))
    length_db <- length(db_identifiers)
    length_data <- length(Identifier_vector)
    length_intersect <- length(intersect(db_identifiers,Identifier_vector))
    cat("\nIdentifiers in the annotation table:",length_data,"\n")
    cat("Identifiers in the database:",length_db,"\n")
    cat("Identifiers in both:",length_intersect,"\n")
    cat(paste0("Percentage of annotation table identifiers used for distillation: ",round(length_intersect/length_data*100,2),"%\n"))
    cat(paste0("Percentage of database identifiers used for distillation: ",round(length_intersect/length_db*100,2),"%\n"))
  }

  #Format output GIFT table
  rownames(GIFT_table) <- Genomes
  colnames(GIFT_table) <- GIFT_db$Code_bundle
  GIFT_table[is.na(GIFT_table)] <- 0

  #Output GIFT table
  return(GIFT_table)
}
