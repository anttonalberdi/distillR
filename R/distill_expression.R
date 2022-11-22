#' Generates a gene abundance/expression-based Metabolic Capacity Index (MCI) table from a bacterial genome annotation table and a related gene expression table
#'
#' @param expression_table Table containing normalised gene expression data with gene identifiers in rows and sample identifiers in columns
#' @param annotation_table Table containing gene and genome identifiers, and gene annotations
#' @param pathway_db Table containing definitions and metadata of metabolic functions (provided by DAMMA)
#' @param genecol Column index (number) of the annotations table containing the gene identifiers
#' @param genomecol Column index (number) of annotation_table containing the genome identifiers
#' @param keggcol Column index(es) of annotation_table in which to search for KEGG KO annotations
#' @param eccol Column index(es) of annotation_table in which to search for Enzyme Commision (EC) annotations
#' @param pepcol Column index(es) of annotation_table in which to search for Peptidase annotations
#' @importFrom stringr str_extract str_match_all
#' @return A list of pathway-expression matrices (one table per genome)
#' @examples
#' distill_expression(expression_table,annotation_table,pathway_db,genecol,genomecol,keggcol,eccol,pepcol)
#' distill_expression(expression_table,annotation_table,pathway_db,genecol=1,genomecol=2,keggcol=9,eccol=c(10,19),pepcol=12)
#' @export

#UNDER DEVELOPMENT
distill_expression <- function(expression_table,annotation_table,pathway_db,genecol=1,genomecol=2,keggcol=9,eccol=c(10,19),pepcol=12){

  #Sanity check
  if(missing(expression_table)) stop("Gene expression table is missing")
  if(missing(annotation_table)) stop("Genome annotation table is missing")
  if(missing(pathway_db)) stop("Pathway table is missing")
  if(missing(genecol)) stop("Specify a column containing Gene identifiers")
  if(missing(genomecol)) stop("Specify a column containing Genome identifiers")
  if(length(genecol)!=1) stop("The argument genecol must be an integer indicating the number of the column containing the Gene identifiers in the annotations table")
  if(length(genomecol)!=1) stop("The argument genomecol must be an integer indicating the number of the column containing the Genome identifiers in the annotations table")
  if(missing(keggcol) & missing(eccol) & missing(pepcol)) stop("Specify at least one column containing functional annotations")

  cat("Starting distillR expression analysis\n(Note this may take a while)...\n")

  #Filter and convert to data frame
  annotation_table <- as.data.frame(annotation_table[annotation_table[,genecol] %in% rownames(expression_table),])
  expression_table <- as.data.frame(expression_table[rownames(expression_table) %in% unique(annotation_table[,genecol]),])

  #Merge annotations and expression information
  annotation_expression_table <- merge(annotation_table,expression_table,by.x=genecol,by.y="row.names",all=FALSE)

  #List Genomes
  Genomes <- unique(annotation_expression_table[,genomecol])

  #Declare index (column numbers) of the expression data
  expression_index <- grep(paste(colnames(expression_table),collapse="|"), colnames(annotation_expression_table))

  #Calculate expression values for each Genome
  cat("Calculating gene expression-based MCIs for Genome:\n")
  m=0
  expression_MCI_table_list <- list()
  for(Genome in Genomes){
    m=m+1
    cat("\t",Genome," (",m,"/",length(Genomes),")\n", sep = "")

    #Subset annotation data for the specific Genome
    annotations_expression_Genome <- annotation_expression_table[annotation_expression_table[,genomecol] == Genome,]

    #Declare expression table
    expression_MCI_table <- data.frame()

    #KEGG identifiers
    #K00000
    if(!missing(keggcol)){
      for(col in keggcol){
        column <- annotations_expression_Genome[,col]
        kegg_detect <- str_detect(column, "K[0-9]+")
        kegg_detect[is.na(kegg_detect)] <- FALSE
        column_sub <- column[kegg_detect]
        kegg_codes <- unlist(str_match_all(column_sub, "K[0-9]+"))
        annotations_expression_Genome_sub <- annotations_expression_Genome[kegg_detect,c(col,1,expression_index)]
        annotations_expression_Genome_sub[,1] <- kegg_codes
        colnames(annotations_expression_Genome_sub)[1] <- "ID"

        #Disambiguation
        annotations_expression_Genome_sub$ambiguity <- str_count(annotations_expression_Genome_sub[,1], "\\S+")
        if(max(annotations_expression_Genome_sub$ambiguity,na.rm=T) > 1){
          for(a in c(2:max(annotations_expression_Genome_sub$ambiguity,na.rm=T))){
    	        origin <- annotations_expression_Genome_sub[annotations_expression_Genome_sub$ambiguity == a,]
              if(nrow(origin)>0){
              	disambiguation <- origin[rep(1:nrow(origin),a-1),]
              	identifiers <- colsplit(string=origin[,1], pattern=" ",names=c(1:a))
              	origin[,1] <- identifiers[,1]
              	disambiguation[,1] <- unlist(identifiers[,c(2:a)])
      	        #Modify origin rows
      	        annotations_expression_Genome_sub[annotations_expression_Genome_sub$ambiguity == a,] <- origin
                #Append extra rows
                annotations_expression_Genome_sub <- rbind(annotations_expression_Genome_sub,disambiguation)
              }
          }
        }

        #Aggregate IDs
        if(nrow(annotations_expression_Genome_sub)>0){
          annotations_expression_Genome_agg <- aggregate(annotations_expression_Genome_sub[,c(3:(ncol(annotations_expression_Genome_sub)-1))],by=list(annotations_expression_Genome_sub[,1]),FUN=sum)
          colnames(annotations_expression_Genome_agg)[1] <- "ID"
        }else{
          annotations_expression_Genome_agg <- annotations_expression_Genome_sub
        }

        if(nrow(annotations_expression_Genome_agg)>0){
          expression_MCI_table <- rbind(expression_MCI_table,annotations_expression_Genome_agg)
        }
      }
    }


    if(!missing(eccol)){
      for(col in eccol){
        column <- annotations_expression_Genome[,col]
        EC_detect <- str_detect(column, "(?<=\\[EC:).+?(?=\\])")
        EC_detect[is.na(EC_detect)] <- FALSE
        column_sub <- column[EC_detect]
        EC_codes <- unlist(str_match_all(column_sub, "(?<=\\[EC:).+?(?=\\])"))
        annotations_expression_Genome_sub <- annotations_expression_Genome[EC_detect,c(col,1,expression_index)]
        annotations_expression_Genome_sub[,1] <- EC_codes
        colnames(annotations_expression_Genome_sub)[1] <- "ID"

        #Disambiguation
        annotations_expression_Genome_sub$ambiguity <- str_count(annotations_expression_Genome_sub[,1], "\\S+")
        if(max(annotations_expression_Genome_sub$ambiguity,na.rm=T) > 1){
          for(a in c(2:max(annotations_expression_Genome_sub$ambiguity,na.rm=T))){
    	        origin <- annotations_expression_Genome_sub[annotations_expression_Genome_sub$ambiguity == a,]
              if(nrow(origin)>0){
              	disambiguation <- origin[rep(1:nrow(origin),a-1),]
              	identifiers <- colsplit(string=origin[,1], pattern=" ",names=c(1:a))
              	origin[,1] <- identifiers[,1]
              	disambiguation[,1] <- unlist(identifiers[,c(2:a)])
      	        #Modify origin rows
      	        annotations_expression_Genome_sub[annotations_expression_Genome_sub$ambiguity == a,] <- origin
                #Append extra rows
                annotations_expression_Genome_sub <- rbind(annotations_expression_Genome_sub,disambiguation)
              }
          }
        }
        #Aggregate IDs
        if(nrow(annotations_expression_Genome_sub)>0){
          annotations_expression_Genome_agg <- aggregate(annotations_expression_Genome_sub[,c(3:(ncol(annotations_expression_Genome_sub)-1))],by=list(annotations_expression_Genome_sub[,1]),FUN=sum)
          colnames(annotations_expression_Genome_agg)[1] <- "ID"
        }else{
          annotations_expression_Genome_agg <- annotations_expression_Genome_sub
        }

        if(nrow(annotations_expression_Genome_agg)>0){
          expression_MCI_table <- rbind(expression_MCI_table,annotations_expression_Genome_agg)
        }
      }
    }

    if(!missing(pepcol)){
      for(col in pepcol){
        column <- annotations_expression_Genome[,col]
        pep_codes <- unique(c(unlist(c(annotations_expression_Genome[,col]))))
        pep_codes <- pep_codes[!is.na(pep_codes)]
        pep_codes <- pep_codes[pep_codes != ""]
        annotations_expression_Genome_sub <- annotations_expression_Genome[annotations_expression_Genome[,col] %in% pep_codes, c(col,1,expression_index)]
        colnames(annotations_expression_Genome_sub)[1] <- "ID"

        #Disambiguation
        annotations_expression_Genome_sub$ambiguity <- str_count(annotations_expression_Genome_sub[,1], "\\S+")
        if(max(annotations_expression_Genome_sub$ambiguity,na.rm=T) > 1){
          for(a in c(2:max(annotations_expression_Genome_sub$ambiguity,na.rm=T))){
    	        origin <- annotations_expression_Genome_sub[annotations_expression_Genome_sub$ambiguity == a,]
              if(nrow(origin)>0){
              	disambiguation <- origin[rep(1:nrow(origin),a-1),]
              	identifiers <- colsplit(string=origin[,1], pattern=" ",names=c(1:a))
              	origin[,1] <- identifiers[,1]
              	disambiguation[,1] <- unlist(identifiers[,c(2:a)])
      	        #Modify origin rows
      	        annotations_expression_Genome_sub[annotations_expression_Genome_sub$ambiguity == a,] <- origin
                #Append extra rows
                annotations_expression_Genome_sub <- rbind(annotations_expression_Genome_sub,disambiguation)
              }
          }
        }
        #Aggregate IDs
        if(nrow(annotations_expression_Genome_sub)>0){
          annotations_expression_Genome_agg <- aggregate(annotations_expression_Genome_sub[,c(3:(ncol(annotations_expression_Genome_sub)-1))],by=list(annotations_expression_Genome_sub[,1]),FUN=sum)
          colnames(annotations_expression_Genome_agg)[1] <- "ID"
        }else{
          annotations_expression_Genome_agg <- annotations_expression_Genome_sub
        }

        if(nrow(annotations_expression_Genome_agg)>0){
          expression_MCI_table <- rbind(expression_MCI_table,annotations_expression_Genome_agg)
        }
      }
    }

    rownames(expression_MCI_table) <- expression_MCI_table[,1]
    expression_MCI_table <- expression_MCI_table[,-1]

    #Compute expression scores
    #cat("\t\tCalculating gene expression-based MCIs for\n")
    #cat("\t\t",nrow(pathway_db),"pathways in",ncol(expression_MCI_table),"samples...\n")
    suppressWarnings(
      for(f in c(1:nrow(pathway_db))){
        definition=pathway_db[f,"Definition"]
        expression_MCI <- compute_MCI_expression(definition,expression_MCI_table)
        if(f == 1){
          #Create list if it is the first function
          expression_MCI_list <- expression_MCI
        }else{
          #Append to list if it is not the first function
          expression_MCI_list <- Map(c, expression_MCI_list, expression_MCI)
        }
      }
    )
    #Convert sample list to matrix
    expression_MCI_list <- lapply(expression_MCI_list,function(x) as.numeric(x))
    expression_MCI_table <- do.call(rbind, expression_MCI_list)
    colnames(expression_MCI_table) <- pathway_db$Code_pathway

    #Append to Genome list
    expression_MCI_table_list[[Genome]] <- expression_MCI_table
  }

  return(expression_MCI_table_list)

}
