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
GIFTs <- distill(gene_annotations,GIFT_db,genomecol=2,annotcol=c(9,10,19))

#Aggregate bundle-level GIFTs into the compound level
GIFTs_elements <- to.elements(GIFTs,GIFT_db)

#Aggregate element-level GIFTs into the function level
GIFTs_functions <- to.functions(GIFTs_elements,GIFT_db)

#Aggregate function-level GIFTs into overall Biosynthesis, Degradation and Structural GIFTs
GIFTs_domains <- to.domains(GIFTs_functions,GIFT_db)

#Get overall metabolic capacity indices per MAG (at the domain level)
rowMeans(GIFTs_functions) # averaged at the function level (each function is weighed equally)
rowMeans(GIFTs_domains) # averaged at the domain level (each domain is weighed equally)

#Get community-weighed average GIFTs per sample
GIFTs_elements_community <- to.community(GIFTs_elements,genome_counts,GIFT_db)
GIFTs_functions_community <- to.community(GIFTs_functions,genome_counts,GIFT_db)
GIFTs_domains_community <- to.community(GIFTs_domains,genome_counts,GIFT_db)
```

## Input data structure
distillR can perform operations with four types of input data

### Gene annotations (mandatory)

### GIFT database (mandatory)

### Gene counts (requited for quantitative GIFTs)

### Genome counts (requited for community GIFTs)

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

## Quantitative GIFTs
Quantitative GIFTs are genome-inferred functional traits that are based on quantitative gene data rather than gene presence (as regular GIFTs). Such data can be derived from shotgun gene-expression analyses (RNAseq) or can be generated for an entire community by mapping sequencing reads to a catalogue of genes derived from a metagenomic (co)assembly. Quantitative GIFTs are calculated using function distillq(), which requires two more bits of information compared to distill(): a gene count table containing quantitative gene data per sample, and the column number of the annotation table in which gene identifiers can be found.

```
#Run distillation
qGIFTs <- distillq(gene_expression,gene_annotations,GIFT_db,genecol=1,genomecol=2,annotcol=c(9,10,19))

#Sweep list structure from genome-based to sample-based
qGIFTs_persample <- sweep_matrix_list(qGIFTs)
```

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
