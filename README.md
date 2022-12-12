# distillR

distillR is an R package for distilling functional annotations of bacterial genomes and metagenomes into meaningful quantitative metrics defined as Genome-Inferred Functional Traits (GIFT).

## Quickstart
The package distillR contains mock data and the GIFT database required to test all functions.

```
#Install and load the package distillR
install.packages("devtools")
library(devtools)
install_github("anttonalberdi/distillR")
library(distillR)

#Run distillation
GIFTs <- distill(gene_annotations,GIFT_db,genomecol=2,keggcol=9,eccol=c(10,19),pepcol=12)

#Aggregate bundle-level GIFTs into the compound level
GIFTs_elements <- to.elements(GIFTs,GIFT_db)

#Aggregate element-level GIFTs into the function level
GIFTs_functions <- to.functions(GIFTs_elements,GIFT_db)

#Aggregate function-level GIFTs into overall Biosynthesis, Degradation and Structural GIFTs
GIFTs_domains <- to.domains(GIFTs_functions,GIFT_db)

#Get community-weighed average GIFTs per sample
GIFTs_elements_community <- to.community(GIFTs_elements,genome_counts,GIFT_db)
GIFTs_functions_community <- to.community(GIFTs_functions,genome_counts,GIFT_db)
GIFTs_domains_community <- to.community(GIFTs_domains,genome_counts,GIFT_db)
```
