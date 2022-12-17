#' Generates a list of gene count-based Genome-Inferred Functional Trait (GIFT) tables at the gene bundle level from bacterial genome annotation and gene count tables
#'
#' @param gene_count_table Table containing normalised gene expression data with gene identifiers in rows and sample identifiers in columns
#' @param annotation_table Table containing gene and genome identifiers, and gene annotations
#' @param GIFT_db Table containing definitions and metadata of metabolic functions (provided by DAMMA)
#' @param genecol Column index (number) of the annotations table containing the gene identifiers
#' @param genomecol Column index (number) of annotation_table containing the genome identifiers
#' @param annotcol Column index(es) of the annotation_table in which to search for gene identifiers (e.g., c(3,4,5))
#' @importFrom stringr str_extract str_match_all str_count
#' @importFrom reshape2 colsplit
#' @return A list of quantitative GIFT tables (one table per genome)
#' @examples
#' distillq(gene_count_table,annotation_table,GIFT_db,genecol,genomecol,keggcol,eccol,pepcol)
#' distillq(gene_count_table,annotation_table,GIFT_db,genecol=1,genomecol=2,keggcol=9,eccol=c(10,19),pepcol=12)
#' @export

#UNDER DEVELOPMENT
distillq <- function(gene_count_table,annotation_table,GIFT_db,genecol=1,genomecol=2,annotcol=c(9,10,19)){

  #Sanity check
  if(missing(gene_count_table)) stop("Gene expression table is missing")
  if(missing(annotation_table)) stop("Genome annotation table is missing")
  if(missing(GIFT_db)) stop("GIFT database is missing")
  if(missing(genecol)) stop("Specify a column containing Gene identifiers")
  if(missing(genomecol)) stop("Specify a column containing Genome identifiers")
  if(length(genecol)!=1) stop("The argument genecol must be an integer indicating the number of the column containing the Gene identifiers in the annotations table")
  if(length(genomecol)!=1) stop("The argument genomecol must be an integer indicating the number of the column containing the Genome identifiers in the annotations table")
  if(missing(annotcol)) stop("Specify at least one column containing functional annotations")

  cat("Starting distillR quantitative analysis\n(Note this may take a while)...\n")

  #Filter and convert to data frame
  annotation_table <- as.data.frame(annotation_table)
  annotation_table <- as.data.frame(annotation_table[annotation_table[,genecol] %in% rownames(gene_count_table),])
  gene_count_table <- as.data.frame(gene_count_table)
  gene_count_table <- as.data.frame(gene_count_table[rownames(gene_count_table) %in% unique(annotation_table[,genecol]),])

  #Merge annotations and expression information
  annotation_gene_count_table <- merge(annotation_table,gene_count_table,by.x=genecol,by.y="row.names",all=FALSE)

  #List Genomes
  Genomes <- unique(annotation_gene_count_table[,genomecol])

  #Declare index (column numbers) of the expression data
  expression_index <- grep(paste(colnames(gene_count_table),collapse="|"), colnames(annotation_gene_count_table))

  #Calculate expression values for each Genome
  cat("Calculating gene expression-based MCIs for Genome:\n")
  m=0
  GIFTq_table_list <- list()
  for(Genome in Genomes){
    m=m+1
    cat("\t",Genome," (",m,"/",length(Genomes),")\n", sep = "")

    #Subset annotation data for the specific Genome
    annotation_gene_count_Genome <- annotation_gene_count_table[annotation_gene_count_table[,genomecol] == Genome,]

    #Declare expression table
    GIFTq_table <- data.frame()

      for(col in annotcol){
        column <- annotation_gene_count_Genome[,col]
        identifier_detect <- str_detect(column, "K[0-9][0-9][0-9][0-9][0-9]|(?<=\\[EC:).+?(?=\\])")
        identifier_detect[is.na(identifier_detect)] <- FALSE
        column_sub <- column[identifier_detect]
        identifier_codes <- unlist(str_match_all(column_sub, "K[0-9][0-9][0-9][0-9][0-9]|(?<=\\[EC:).+?(?=\\])"))
        if(length(identifier_codes)>0){
          annotation_gene_count_Genome_sub <- annotation_gene_count_Genome[identifier_detect,c(col,1,expression_index)]
          annotation_gene_count_Genome_sub[,1] <- identifier_codes
          colnames(annotation_gene_count_Genome_sub)[1] <- "ID"

          #Disambiguation
          annotation_gene_count_Genome_sub$ambiguity <- str_count(annotation_gene_count_Genome_sub[,1], "\\S+")
          if(max(annotation_gene_count_Genome_sub$ambiguity,na.rm=T) > 1){
            for(a in c(2:max(annotation_gene_count_Genome_sub$ambiguity,na.rm=T))){
      	        origin <- annotation_gene_count_Genome_sub[annotation_gene_count_Genome_sub$ambiguity == a,]
                if(nrow(origin)>0){
                	disambiguation <- origin[rep(1:nrow(origin),a-1),]
                	identifiers <- colsplit(string=origin[,1], pattern=" ",names=c(1:a))
                	origin[,1] <- identifiers[,1]
                	disambiguation[,1] <- unlist(identifiers[,c(2:a)])
        	        #Modify origin rows
        	        annotation_gene_count_Genome_sub[annotation_gene_count_Genome_sub$ambiguity == a,] <- origin
                  #Append extra rows
                  annotation_gene_count_Genome_sub <- rbind(annotation_gene_count_Genome_sub,disambiguation)
                }
            }
          }

          #Aggregate IDs
          if(nrow(annotation_gene_count_Genome_sub)>0){
            annotation_gene_count_Genome_agg <- aggregate(annotation_gene_count_Genome_sub[,c(3:(ncol(annotation_gene_count_Genome_sub)-1))],by=list(annotation_gene_count_Genome_sub[,1]),FUN=sum)
            colnames(annotation_gene_count_Genome_agg)[1] <- "ID"
          }else{
            annotation_gene_count_Genome_agg <- annotation_gene_count_Genome_sub
          }

          if(nrow(annotation_gene_count_Genome_agg)>0){
            GIFTq_table <- rbind(GIFTq_table,annotation_gene_count_Genome_agg)
          }
        }
      }
    

    if(nrow(GIFTq_table)>0){

      rownames(GIFTq_table) <- GIFTq_table[,1]
      GIFTq_table <- GIFTq_table[,-1]

      suppressWarnings(
        for(f in c(1:nrow(GIFT_db))){
          definition=GIFT_db[f,"Definition"]
          GIFTq <- compute_GIFTq(definition,GIFTq_table)
          if(f == 1){
            #Create list if it is the first function
            GIFTq_list <- GIFTq
          }else{
            #Append to list if it is not the first function
            GIFTq_list <- Map(c, GIFTq_list, GIFTq)
          }
        }
      )
      #Convert sample list to matrix
      GIFTq_list <- lapply(GIFTq_list,function(x) as.numeric(x))
      GIFTq_table <- do.call(rbind, GIFTq_list)
      colnames(GIFTq_table) <- GIFT_db$Code_bundle

      #Append to Genome list
      GIFTq_table_list[[Genome]] <- GIFTq_table
    }
  }

  return(GIFTq_table_list)

}
