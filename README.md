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

## Community GIFT computation
Using distillR community GIFTs can be calculated using two contrasting approaches. In the aggregative approach, the entire community is considered as a single genome and GIFTs are computed using all identifiers present in the community data. This can be achieved by avoiding specifying the genome identifier column in the function distill(). In the separative approach, GIFTs are calculated for each genome independently, and then community-weighed averages calculated optionally based on the relative abundances of the genomes. This approach requires running distill() and to.community() functions.
```
#Aggregative approach
GIFTs_aggregative <- distill(gene_annotations,GIFT_db,keggcol=9,eccol=c(10,19),pepcol=12)

#Separative approach (without considering relative abundances)
GIFTs_separative_even <- distill(gene_annotations,GIFT_db,genomecol=2,keggcol=9,eccol=c(10,19),pepcol=12) %>%
  to.community()

#Separative approach (considering relative abundances)
GIFTs_separative_ra <- distill(gene_annotations,GIFT_db,genomecol=2,keggcol=9,eccol=c(10,19),pepcol=12) %>%
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
