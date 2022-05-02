# 
source("ggcdkit.R")
setwd("C:/Users/moje4671/Documents/Recherche/R_development/ggcdkit")

data(sazava)
accessVar("sazava")
# 
# ds <- as.tibble(WR) %>% add_column(labels,.before = T)  
# 
# ggbinary(ds, SiO2, K2O-`A/CNK`)
# 


ggplotDiagram(OConnorVolc)

ee <- ggbinary(ds,Rb,Sr)
ee+coord_trans(x="log10")



chdr <- selectNorm("Anders & Grevesse 1989")
pm <- selectNorm("REE Primitive mantle (McDonough & Sun 1995)")

chdr %<>% as_tibble(rownames = "Reservoir") %>% mutate(Reservoir = "chondrite")
pm %<>% as_tibble(rownames = "Reservoir") %>% mutate(Reservoir = "Prim.Mantle")

df <- df[,c("Reservoir",REE)] %>% add_row(chdr[,c("Reservoir",REE)]) %>% add_row(pm[,c("Reservoir",REE)])


df %>% 
  filter(Reservoir == "chondrite" | Reservoir == "Prim.Mantle" | Reservoir == "DMM") %>%
  mutate(across(where(is.numeric), ~./.[Reservoir == "chondrite"])) %>%
  gather(REE,key="Element",value=Concentration) %>% 
  ggplot(aes(x=Element,y=Concentration,col=Reservoir,group=Reservoir))+
  geom_point(size=3)+
  xlim(REE)+
  labs(y="Concentration normalisée",title="Normalisé aux chondrites")+
  scale_y_log10()+
  geom_path(size=1)+
  theme_gcdkit()+theme(panel.grid.major=element_line(colour="grey",linetype="dashed"))

#######################
library(ggplot2)
library(magrittr)
library(tibble)
my.better.plot <- function(){
  x.coords <- 1
  y.coords <- 1
  environment(package.plot) <- environment()
  
  bmp(tempfile())
  package.plot()
  dev.off()
  
  print(tibble(x.coords,y.coords) %>% ggplot(aes(x=x.coords,y=y.coords))+geom_point()) # etc. 
}

my.better.plot()
#creates only the ggplot in the current device

ls(globalenv())
#[1] "my.better.plot" "package.plot"   "the.data" 