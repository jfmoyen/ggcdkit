library(tidyverse)
library(gridExtra)
library(GCDkitDevelop)
library(clipr)


theme_gcdkit <- function () { 
  theme_bw(base_size=12, base_family="Avenir") %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_blank(), 
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA),
      panel.grid = element_blank(),
      axis.title.x = element_text(size=14, face="bold", colour = "black"),
      axis.title.y = element_text(size=14, face="bold", colour = "black",angle=90),
      axis.text.x = element_text(size=12, colour = "black"),
      axis.text.y = element_text(size=12, colour = "black")
    )
}

setwd("C:\\Users\\moje4671\\Documents\\enseignement\\Saint-Etienne\\L3ST_S5_PetroMag")

# loadData("some_reservoirs.xlsx")

df <- read_clip_tbl() %>% tibble()
#accessVar("df")

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

p2<- df %>% 
  filter(Reservoir == "chondrite" | Reservoir == "Prim.Mantle" | Reservoir == "DMM") %>%
  mutate(across(where(is.numeric), ~./.[Reservoir == "Prim.Mantle"])) %>%
  gather(REE,key="Element",value=Concentration) %>% 
  ggplot(aes(x=Element,y=Concentration,col=Reservoir,group=Reservoir))+
  geom_point(size=3)+
  xlim(REE)+
  labs(y="Concentration normalisée",title="Normalisé au manteau primitif")+
  scale_y_log10()+
  geom_path(size=1)+
  theme_gcdkit()+theme(panel.grid.major=element_line(colour="grey",linetype="dashed"))

p3 <- df %>% 
  filter(Reservoir == "chondrite" | Reservoir == "Prim.Mantle" | Reservoir == "DMM") %>%
  mutate(across(where(is.numeric), ~./.[Reservoir == "DMM"])) %>%
  gather(REE,key="Element",value=Concentration) %>% 
  ggplot(aes(x=Element,y=Concentration,col=Reservoir,group=Reservoir))+
  geom_point(size=3)+
  xlim(REE)+
  labs(y="Concentration normalisée",title="Normalisé au manteau appauvri")+
  scale_y_log10()+
  geom_path(size=1)+
  theme_gcdkit()+theme(panel.grid.major=element_line(colour="grey",linetype="dashed"))

########
df %>% 
  mutate(across(where(is.numeric), ~./.[Reservoir == "NMORB"])) %>%
  filter(Reservoir == "arc igneous, 50 < SiO2 < 55 %" | Reservoir == "avg OIB" | Reservoir == "NMORB" | Reservoir == "Prim.Mantle" | Reservoir == "DMM") %>%
  gather(REE,key="Element",value=Concentration) %>% 
  ggplot(aes(x=Element,y=Concentration,col=Reservoir,group=Reservoir))+
  geom_point(size=3)+
  xlim(REE)+
  labs(y="Concentration normalisée",title="Normalisé au MORB")+
  scale_y_log10()+
  scale_color_manual(values=c("orange","darkgreen","slategrey","royalblue","black"))+
  geom_path(size=1)+
  theme_gcdkit()+theme(panel.grid.major=element_line(colour="grey",linetype="dashed"))


#####################


ff <- seq(0,1,0.02)

dd <- c(0.1,0.5,2,10)
cc <- c(10,100)

melting<-expand_grid(D=dd,c0=cc,F=ff) %>% mutate(cL = c0 / (D + F * (1-D)) )

melting %>% 
  ggplot(aes(x=F,y=cL,colour=factor(D),linetype=factor(c0),group=interaction(c0,D)))+
  geom_line(size=2)+
  scale_y_log10()+
  scale_color_viridis_d(option="D",guide_colorbar(title="D"))+
  scale_linetype(guide_legend(title="C0"))

############

ff <- seq(0,1,0.1)

d <- 0.1

DM <- df[8,REE]
MORB <- df[7,REE]

## Melting
map_dfr(ff, ~ DM / (d + .x * (1-d) )) %>% tibble() %>% add_column(F=ff,.before=T)%>%
  add_row(DM) %>% 
  mutate(across(REE, ~. / .[12])) %>%
  gather(REE,key="Element",value=Concentration) %>% 
  ggplot(aes(x=Element,y=Concentration,colour=F,group=F))+
  geom_point(size=3)+
  xlim(REE)+
  labs(y="Concentration normalisée (DM)",title="Fusion partielle, D = 0.1")+
  scale_y_log10()+
  geom_path(size=1)+
  scale_colour_gradient(low="darkblue",high="red")+
  theme_gcdkit()+theme(panel.grid.major=element_line(colour="grey",linetype="dashed"))

## CF
map_dfr(ff, ~ df[6,REE] * .x ^ (d -1) ) %>% tibble() %>% add_column(F=ff,.before=T)%>%
  add_row(DM) %>% 
  mutate(across(REE, ~. / .[12])) %>%
  filter(La < 1e6) %>%
  gather(REE,key="Element",value=Concentration) %>% 
  ggplot(aes(x=Element,y=Concentration,colour=F,group=F))+
  geom_point(size=3)+
  xlim(REE)+
  labs(y="Concentration normalisée (DM)",title="Crist. fractionnée d'un OIB, D = 0.1")+
  scale_y_log10(expand = c(0.1,0))+
  geom_path(size=1)+
  scale_colour_gradient(low="darkblue",high="red")+
  theme_gcdkit()+theme(panel.grid.major=element_line(colour="grey",linetype="dashed"))

map_dfr(ff, ~ df[6,REE] * .x ^ (d -1) ) %>% tibble() %>% add_column(F=ff,.before=T)%>%
  add_row(DM) %>% 
  mutate(across(REE, ~. / .[12])) %>%
  filter(La < 1e6 & La >1) %>%
  gather(REE,key="Element",value=Concentration) %>% 
  ggplot(aes(x=Element,y=Concentration,colour=F,group=F))+
  geom_point(size=3)+
  xlim(REE)+
  labs(y="Concentration normalisée (DM)",title="Crist. fractionnée d'un OIB, D = 0.1")+
  scale_y_log10(expand = c(0.1,0))+
  geom_path(size=1)+
  scale_colour_gradient(low="darkblue",high="red")+
  theme_gcdkit()+theme(panel.grid.major=element_line(colour="grey",linetype="dashed"))

## Melting with variable D
dvar <- seq(0.1,0.5,length.out = 14)
map_dfr(ff, ~ DM / (dvar + .x * (1-dvar) )) %>% tibble() %>% add_column(F=ff,.before=T)%>%
  add_row(DM) %>% 
  mutate(across(REE, ~. / .[12])) %>%
  gather(REE,key="Element",value=Concentration) %>% 
  ggplot(aes(x=Element,y=Concentration,colour=F,group=F))+
  geom_point(size=3)+
  xlim(REE)+
  labs(y="Concentration normalisée (DM)",title="Fusion partielle, D varie de 0.1 (La) à 0.5 (Lu)")+
  scale_y_log10()+
  geom_path(size=1)+
  scale_colour_gradient(low="darkblue",high="red")+
  theme_gcdkit()+theme(panel.grid.major=element_line(colour="grey",linetype="dashed"))

map_dfr(ff, ~ DM / (dvar + .x * (1-dvar) )) %>% tibble() %>% add_column(F=ff,.before=T) %>%
  ggplot(aes(x=F,y=La/Yb,colour = F))+
  geom_point(size=3)+
  geom_line(size=1)+
  scale_colour_gradient(low="darkblue",high="red")+
  theme_gcdkit()

#############

df %>% 
  mutate(across(where(is.numeric), ~./.[Reservoir == "DMM"])) %>%
  filter(Reservoir == "NMORB" | Reservoir == "Prim.Mantle" | Reservoir == "DMM") %>%
  gather(REE,key="Element",value=Concentration) %>% 
  ggplot(aes(x=Element,y=Concentration,col=Reservoir,group=Reservoir))+
  geom_point(size=3)+
  xlim(REE)+
  labs(y="Concentration normalisée",title="Normalisé au DM")+
  scale_y_log10()+
  coord_cartesian(ylim=c(1,80))+
  scale_color_manual(values=c("slategrey","royalblue","black"))+
  geom_path(size=1)+
  theme_gcdkit()+theme(panel.grid.major=element_line(colour="grey",linetype="dashed"))

df %>% 
  mutate(Tb = ifelse(Reservoir=="avg OIB",0.9,Tb)) %>%
  mutate(across(where(is.numeric), ~./.[Reservoir == "DMM"])) %>%
  filter(Reservoir == "avg OIB" | Reservoir == "NMORB" | Reservoir == "Prim.Mantle" | Reservoir == "DMM") %>%
  gather(REE,key="Element",value=Concentration) %>% 
  ggplot(aes(x=Element,y=Concentration,col=Reservoir,group=Reservoir))+
  geom_point(size=3)+
  xlim(REE)+
  labs(y="Concentration normalisée",title="Normalisé au DM")+
  scale_y_log10()+
  coord_cartesian(ylim=c(1,80))+
  scale_color_manual(values=c("orange","slategrey","royalblue","black"))+
  geom_path(size=1)+
  theme_gcdkit()+theme(panel.grid.major=element_line(colour="grey",linetype="dashed"))

df %>% 
  mutate(Tb = ifelse(Reservoir=="avg OIB",0.9,Tb)) %>%
  mutate(across(where(is.numeric), ~./.[Reservoir == "DMM"])) %>%
  filter(Reservoir == "arc igneous, 50 < SiO2 < 55 %" | Reservoir == "avg OIB" | Reservoir == "NMORB" | Reservoir == "Prim.Mantle" | Reservoir == "DMM") %>%
  gather(REE,key="Element",value=Concentration) %>% 
  ggplot(aes(x=Element,y=Concentration,col=Reservoir,group=Reservoir))+
  geom_point(size=3)+
  xlim(REE)+
  labs(y="Concentration normalisée",title="Normalisé au DM")+
  scale_y_log10()+
  coord_cartesian(ylim=c(1,80))+
  scale_color_manual(values=c("lightgreen","orange","slategrey","royalblue","black"))+
  geom_path(size=1)+
  theme_gcdkit()+theme(panel.grid.major=element_line(colour="grey",linetype="dashed"))

