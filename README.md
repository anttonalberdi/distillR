# distillR

distillR is an R package for distilling functional annotations of bacterial genomes and metagenomes into meaningful quantitative metrics defined as Metabolic Capacity Indices (MCIs).

## Quickstart
The package distillR contains mock data and the metabolic pathway database required to test all functions.
```
#Install and load the package distillR
install.packages("devtools")
library(devtools)
install_github("anttonalberdi/distillR")
library(distillR)

#Run distillation
MCIs <- distill(gene_annotations,pathway_db,genomecol=2,keggcol=9,eccol=c(10,19),pepcol=12)

#Aggregate pathway-level MCIs into the compound level
MCI_compounds <- distill_compounds(MCIs,pathway_db)

#Aggregate compound-level MCIs into function level
MCI_functions <- distill_functions(MCI_compounds,pathway_db)
```
