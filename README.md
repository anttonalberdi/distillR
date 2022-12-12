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

## Chart plotting
Genome-specific Genome-Inferred Functional Trait (GIFT) profiles can be plotted using ggplot2 R
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

Community-averaged Genome-Inferred Functional Trait (GIFT) profiles per sample can be also plotted using ggplot2 R
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
