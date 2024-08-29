# distillR

distillR is an R package for distilling functional annotations of bacterial genomes and metagenomes into meaningful quantitative metrics defined as Genome-Inferred Functional Traits (GIFT). The package relies on a curated database of ~500 metabolic pathways and gene clusters (collectivelly refered to as 'bundles') to calculate standardised genome-inferred functional traits using KEGG and Enzyme Commission (EC) identifiers. distillR can process functional annotations from complete bacterial genomes, as well as incomplete bacterial genomes derived from genome-resolved metagenomics. The package can provide genome-specific as well as community-level estimations of functional traits to facilitate downstream statistical analyses.

## Quickstart
The package distillR contains mock data and the GIFT database required to test all functions.

### Install and load the package distillR

distillR can be installed from this same Github repository using the package devtools.

```r
install.packages("devtools")
library(devtools)
install_github("anttonalberdi/distillR")
library(distillR)
```

### Run distillation

This is the main function of the package, which transforms raw functional annotation data into basal quantitative genome-inferred functional traits (GIFTs).

```r
GIFTs <- distill(gene_annotations,GIFT_db,genomecol=2,annotcol=c(9,10,19))
```

### Aggregate GIFTs

Relying on the hierarchical structure of the distillR database, basal GIFTs can be aggregated into larger clusters at the compound (217 elements), function (30 functions) and domain (4 domains) levels.

```r
#Aggregate bundle-level GIFTs into the compound level
GIFTs_elements <- to.elements(GIFTs,GIFT_db)

#Aggregate element-level GIFTs into the function level
GIFTs_functions <- to.functions(GIFTs_elements,GIFT_db)

#Aggregate function-level GIFTs into overall Biosynthesis, Degradation and Structural GIFTs
GIFTs_domains <- to.domains(GIFTs_functions,GIFT_db)
```

### Metabolic Capacity Indices (MCI) of genomes

By averaging the GIFT values of each genome, one can obtain the so-called Metabolic Capacity Indices (MCI) of each bacteria.

```r
# Averaged at the function level (each function is weighed equally)
rowMeans(GIFTs_functions)

# Averaged at the domain level (each domain is weighed equally)
rowMeans(GIFTs_domains)
```

### Community-weighed average GIFTs of genomes

Lastly, microbiome-wide GIFTs can also be computed by relying on the relative abundance data of each genome in each sample.

```r
GIFTs_elements_community <- to.community(GIFTs_elements,genome_counts,GIFT_db)
GIFTs_functions_community <- to.community(GIFTs_functions,genome_counts,GIFT_db)
GIFTs_domains_community <- to.community(GIFTs_domains,genome_counts,GIFT_db)
```

## Input data structure
distillR can perform operations with four types of input data.

### Gene annotations (mandatory)

distillR requires gene annotations to be stored in a tabular format, each row representing one gene. The minimum structure of such a table contains two columns: genomes and annotations.

| Gene | Genome | Annotation |
| --- | --- | --- |
| Gene01 | Genome1 | K00791 |
| Gene02 | Genome1 | tRNA dimethylallyltransferase [EC:2.5.1.75] |
| Gene03 | Genome2 | K24846 |
| Gene04 | Genome2 | NA |

The function **distill()** requires the column numbers containing information about genomes (a single genome) and annotations (one or multiple genomes) to be specified using the arguments ***genomecol*** and ***annotcol***, respectivelly.

```r
GIFTs <- distill(gene_annotations,GIFT_db,genomecol=2,annotcol=c(9,10,19))
```

### GIFT database (mandatory)

The default distillR database is a table containing 315 gene bundles (also known as pathways or modules in other databases) stored as a data frame object named ***GIFT_db***.

```r
head(GIFT_db)
```

| Code_bundle | Code_element | Code_function | Domain | Function | Element | Definition |
| --- | --- | --- | --- | --- | --- | --- |
B010301 | B0103 | B01 | Biosynthesis | Nucleic acid biosynthesis | UDP/UTP | (K13800,K13809,K09903) |
B010401 | B0104 | B01 | Biosynthesis | Nucleic acid biosynthesis | CDP/CTP | (K00940,K18533) K01937 |
B010501 | B0105 | B01 | Biosynthesis | Nucleic acid biosynthesis | ADP/ATP | K01939 K01756 (K00939,K18532,K18533,K00944) K00940 |
B010601 | B0106 | B01 | Biosynthesis | Nucleic acid biosynthesis | GDP/GTP | K00088 K01951 K00942 (K00940,K18533) |

The bundles are collections of related genes that encode for enzymes that collectively perform a metabolic function. Bundles are organised in three hierarchical levels: elements, functions and domains.

- **Elements:** collections of bundles that produce or metabolise different compounds. The default distillR database contains ~300 elements.
- **Functions:** overall metabolic functions to biosynthesise or degrade different types of molecules. The default distillR database contains 3' functions, such as Amino acid degradation, Nucleic acid biosynthesis, or Spore formation.
- **Domains:** clusters of functions. The default distillR database contains four domains: biosynthesis, degradation, transport and structure.
  - ***Biosynthesis:*** pathways to produce the compounds (elements) of interest.
  - ***Degradation:*** pathways to degrade the compounds (elements) of interest.
  - ***Transport:*** genes and gene clusters involved in transmembranic transport of compounds (elements) of interest.
  - ***Structure:*** genes and gene clusters involved in the biosynthesis of structural elementsof interest.
  
The hierarchical structure of the database enables baseline GIFTs calculated at the gene bundle level to be aggregated into broader categories.

### Gene counts (required for quantitative GIFTs)
The quantitative mode of distillR computes GIFT activity metrics from metatranscriptomic data. This analysis requires gene count data with unique gene identifiers matching the gene identifiers in the annotation file specified in the first column.

| Gene | Sample1 | Sample2 | Sample3 |
| --- | --- | --- | --- |
| Gene01 | 0 | 0 | 2332 |
| Gene02 | 123 | 115342 | 74544 |
| Gene03 | 0 | 124 | 1231 |
| Gene04 | 56343 | 1233 | 0 |

### Genome counts (required for community GIFTs)
The community mode of distillR computes community-averaged GIFT values for the entire microbiome in each sample. This analysis requires a genome count data table with genomes as rows and samples as columns. Note that the row names of the genome counts table needs to match the genome names in the genomes column of the annotation file.

| Genome | Sample1 | Sample2 | Sample3 |
| --- | --- | --- | --- |
| Genome01 | 21 | 0 | 3453 |
| Genome02 | 1141 | 0 | 2345 |
| Genome03 | 123 | 454 | 0 |
| Genome04 | 1665 | 34 | 5235 |

## Chart plotting
Genome-specific Genome-Inferred Functional Trait (GIFT) profiles can be plotted using ggplot2. The below example plots element-level genome-specific GIFTs.
```
library(ggplot2)
library(RColorBrewer)
library(reshape2)

GIFTs_elements %>%
  reshape2::melt() %>%
  rename(Genome = Var1, Code_element = Var2, GIFT = value) %>%
  inner_join(GIFT_db,by="Code_element") %>%
  ggplot(., aes(x=Code_element, y=Genome, fill=GIFT, group=Code_function))+
    geom_tile()+
    scale_y_discrete(guide = guide_axis(check.overlap = TRUE))+
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
    scale_fill_gradientn(limits = c(0,1), colours=brewer.pal(7, "YlGnBu"))+
    facet_grid(. ~ Code_function, scales = "free", space = "free")+
    theme_grey(base_size=8)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),strip.text.x = element_text(angle = 90))
```
![GIFT heatmap](figures/GIFT_heatmap.png)

Community-averaged Genome-Inferred Functional Trait (GIFT) profiles per sample can be also plotted using ggplot2. The below example plots function-level community GIFTs.
```
library(ggplot2)
library(RColorBrewer)
library(reshape2)

GIFTs_functions_community %>%
  reshape2::melt() %>%
  rename(Sample = Var1, Code_function = Var2, GIFT = value) %>%
  inner_join(GIFT_db,by="Code_function") %>%
  ggplot(., aes(x=Code_function, y=Sample, fill=GIFT))+
    geom_tile()+
    scale_y_discrete(guide = guide_axis(check.overlap = TRUE))+
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
    scale_fill_gradientn(limits = c(0,1), colours=brewer.pal(7, "YlGnBu"))+
    theme_grey(base_size=8)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),strip.text.x = element_text(angle = 90))
```
![Community-level GIFT heatmap](figures/GIFT_community_heatmap.png)

## Community GIFTs
Using distillR, community GIFTs can be calculated using two contrasting approaches.

In the aggregative approach, the entire community is considered as a single genome and GIFTs are computed using all identifiers present in the community data. The main caveat of this approach is that it ignores gene bundles enabling different functions are encoded in individual genomes. Assuming that the annotation file contains all the gene annotations of a community, aggregative community GIFTs are calculated by simply avoiding specifying the genome identifier column in the function distill().

In the separative approach, GIFTs are calculated for each genome independently, and then community-weighed averages calculated optionally based on the relative abundances of the genomes. The caveat of this approach is that it ignores potential collaborative outputs of bacteria with complementary gene sets. This approach requires running distill() and to.community() functions.
```
#Aggregative approach
GIFTs_aggregative <- distill(gene_annotations,GIFT_db,annotcol=c(9,10,19))

#Separative approach (without considering relative abundances)
GIFTs_separative_even <- distill(gene_annotations,GIFT_db,genomecol=2,annotcol=c(9,10,19)) %>%
  to.community()

#Separative approach (considering relative abundances)
GIFTs_separative_ra <- distill(gene_annotations,GIFT_db,genomecol=2,annotcol=c(9,10,19)) %>%
    to.community(.,genome_counts[,2],GIFT_db)

#Combine GIFT tables
community_comparison <- as.data.frame(cbind(t(GIFTs_aggregative),t(GIFTs_separative_even),t(GIFTs_separative_ra)))
colnames(community_comparison) = c("AGG", "SEE","SER")

#Compute correlations
cor.test(~ AGG + SEE,community_comparison)
cor.test(~ SEE + SER,community_comparison)
cor.test(~ AGG + SER,community_comparison)
```

The three approaches can be compared through scatter plots with regression lines.
```
library(ggplot2)

#AGG vs SEE
ggplot(community_comparison, aes(x=AGG, y=SEE))+
  geom_point()+
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  theme_minimal()

#SEE vs SER
ggplot(community_comparison, aes(x=SEE, y=SER))+
  geom_point()+
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  theme_minimal()

#SER vs AGG
ggplot(community_comparison, aes(x=AGG, y=SER))+
  geom_point()+
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  theme_minimal()
```
![Community-level GIFT regressions](figures/GIFT_community_regressions.png)
