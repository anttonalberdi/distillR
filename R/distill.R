#' Generates a gene presence-based Genome-Inferred Functional Trait (GIFT) table at the gene bundle level from a bacterial genome annotation table
#'
#' @param annotation_table Table containing Genome identifiers and gene annotations
#' @param GIFT_db Table containing definitions and metadata of GIFTs (default: database provided by distillR)
#' @param genomecol Column index (number) of the annotation_table containing the genome identifiers
#' @param keggcol Column index(es) of the annotation_table in which to search for KEGG KO annotations
#' @param eccol Column index(es) of the annotation_table in which to search for Enzyme Commision (EC) annotations
#' @param pepcol Column index(es) of the annotation_table in which to search for Peptidase annotations
#' @importFrom stringr str_extract str_match_all
#' @return A gene bundle-level GIFT table
#' @examples
#' distill(annotation_table,GIFT_db,genomecol,keggcol,eccol,pepcol)
#' distill(annotation_table,GIFT_db,genomecol=2,keggcol=9,eccol=c(10,19),pepcol=12)
#' @export

distill <- function(annotation_table,GIFT_db,genomecol=2,keggcol=9,eccol=c(10,19),pepcol=12){

  #Sanity check
  if(missing(annotation_table)) stop("Genome annotation table is missing")
  if(missing(GIFT_db)) stop("Pathway database is missing")
  if(length(genomecol)!=1) stop("The argument genomecol must be an integer indicating the number of the column containing the Genome identifiers in the annotations table")
  if(missing(keggcol) & missing(eccol) & missing(pepcol)) stop("Specify at least one column containing functional annotations")

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

    #Declare vector of identifiers
    Identifier_vector <- c()

    #KEGG identifiers
    #K00000
    if(!missing(keggcol)){
    kegg <- str_extract(c(unlist(c(annotations_Genome[,keggcol]))), "K[0-9]+")
    kegg <- unique(kegg[!is.na(kegg)])
    }else{
    kegg <- c()
    }
    Identifier_vector <- c(Identifier_vector,kegg)

    #Enzyme Commission codes
    #[EC:0.0.0.0]
    if(!missing(eccol)){
    EC <- unlist(str_match_all(c(unlist(c(annotations_Genome[,eccol]))), "(?<=\\[EC:).+?(?=\\])")) #Extract ECs
    EC <- unique(unlist(strsplit(EC, " "))) #Dereplicate
    EC <- EC[!grepl("-", EC, fixed = TRUE)] #Remove ambiguous codes
    EC <- EC[grepl(".", EC, fixed = TRUE)] #Remove NAs and inproperly formatted codes
    }else{
    EC <- c()
    }
    Identifier_vector <- c(Identifier_vector,EC)

    #Peptidases
    if(!missing(pepcol)){
    pep <- unique(c(unlist(c(annotations_Genome[,pepcol]))))
    pep <- pep[pep != ""]
    }else{
    pep <- c()
    }
    Identifier_vector <- c(Identifier_vector,pep)

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
  #db_identifiers <- unique(unlist(strsplit(paste(GIFT_db$Definition, collapse = " ")," |\\,|\\)|\\(|\\+")))
  #db_identifiers <- db_identifiers[grepl(".", db_identifiers, fixed = TRUE)]

  #Format output GIFT table
  rownames(GIFT_table) <- Genomes
  colnames(GIFT_table) <- GIFT_db$Code_bundle
  GIFT_table[is.na(GIFT_table)] <- 0

  #Output GIFT table
  return(GIFT_table)
}
