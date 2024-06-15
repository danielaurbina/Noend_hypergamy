# Figure 4 PDR article. Urbina, Frye, and Lopus. 

library("foreign")
library("readstata13")
library("ggplot2")
library("wesanderson")
library("maps")
library("mapdata")
library("plm")
library("lmtest")
library("multiwayvcov")
library("stargazer")
library("memisc")
library("reshape2")
library("dplyr")
#library("plyr")
library("tidyverse")
library("gridExtra")
library("ggpubr")
library(RColorBrewer)
library("wesanderson")
library("cowplot")

setwd("~/Dropbox/ResearchNote_Hypergamy/Data/Married_data")

# Loading datasets. Census data, married population. 
mexico <-data.frame(read.csv("mexico_un.csv"))
argentina <-data.frame(read.csv("argentina_un.csv"))
brazil <-data.frame(read.csv("brazil_un.csv"))
chile <-data.frame(read.csv("chile_un.csv"))
colombia <-data.frame(read.csv("colombia_un.csv"))
ecuador <-data.frame(read.csv("ecuador_un.csv"))
nicaragua <-data.frame(read.csv("nicaragua_un.csv"))
panama <-data.frame(read.csv("panama_un.csv"))
paraguay <-data.frame(read.csv("paraguay_un.csv"))
peru <-data.frame(read.csv("peru_un.csv"))
salvador <-data.frame(read.csv("salvador_un.csv"))
uruguay <-data.frame(read.csv("uruguay_un.csv"))
dominicana <-data.frame(read.csv("dominicana_un.csv"))
honduras <-data.frame(read.csv("honduras_un.csv"))
costarica <-data.frame(read.csv("costarica_un.csv"))
bolivia <-data.frame(read.csv("bolivia_un.csv"))

#########################################################################################
# Logged Ratio of Hypergamy to Hypogamy                                                  #
#########################################################################################

# Keeping only relevant variables and creating percent of couples in each marital pairing by Female advantage.

edu_pair_c <- function(x) {
  x%>% 
    filter(mating != "NA") %>% 
    filter(FA_women != "NA") %>% 
    filter(Cohort_W_10 <= "1980") %>% 
    filter(Cohort_W_10 >= "1920") %>% 
    filter(age_W >= "25" & age_W <= "60") %>%  
    group_by(FA_women,mating) %>% 
    summarise(count=n()) %>% 
    mutate(perc=count/sum(count))
}

# Applying function to every country data set 

d <- c("argentina", "bolivia", "brazil", "chile", "colombia", "costarica", "dominicana", 
       "ecuador", "honduras", "mexico", "nicaragua", "panama", "paraguay", "peru",  "salvador",
       "uruguay")
names <- c("argentina", "bolivia", "brazil", "chile", "colombia", "costarica", "dominicana", 
           "ecuador", "honduras", "mexico", "nicaragua", "panama", "paraguay", "peru",  "salvador",
           "uruguay")
dts <- list(argentina, bolivia, brazil, chile, colombia, costarica, dominicana, 
            ecuador, honduras, mexico, nicaragua, panama, paraguay, peru,  salvador,
            uruguay)
gg <- list()
for (d in 1: length(d)) {
  gg[[d]]<- edu_pair_c(dts[[d]])
  assign(paste0("df3", names[d]), gg[[d]])
}

df3argentina$country	<-	"argentina"
df3bolivia$country	<-	"bolivia"
df3brazil$country	<-	"brazil"
df3chile$country	<-	"chile"
df3colombia$country	<-	"colombia"
df3costarica$country	<-	"costarica"
df3dominicana$country	<-	"dominicana"
df3ecuador$country	<-	"ecuador"
df3honduras$country	<-	"honduras"
df3mexico$country	<-	"mexico"
df3nicaragua$country	<-	"nicaragua"
df3panama$country	<-	"panama"
df3paraguay$country	<-	"paraguay"
df3peru$country	<-	"peru"
df3salvador$country	<-	"salvador"
df3uruguay$country	<-	"uruguay"

# Generating upper plot.

# Pasting all countries together.

Latam1<-rbind(df3argentina, df3bolivia, df3brazil, df3chile, df3colombia, df3costarica, df3dominicana, 
              df3ecuador, df3honduras, df3mexico, df3nicaragua, df3panama, df3paraguay, df3peru, 
              df3salvador, df3uruguay)

Latam1$mating=recode(Latam1$mating, '1'='1', '2'='3', '3'='2')

FAdv_Hom_LA_restricted_60 <- ggplot(Latam1, aes(x = FA_women, y = perc*100)) +
  geom_point(aes(color = factor(mating)), show.legend = TRUE) +
  stat_smooth(aes(group = factor(mating), color = factor(mating), fill = factor(mating)), method="lm", size=0.7, se=T, alpha = 0.1) +
  labs(x = "Female Advantage Index", y = "% of marriages", fill = "") +
  ylim(-3, 100) +
  scale_color_manual(values = c("palevioletred3","#999999","#E69F00"),
                     labels = c("Hypergamy","Homogamy", "Hypogamy")) +
  scale_fill_manual(values = c("palevioletred3","#999999","#E69F00"),
                    labels = c("Hypergamy","Homogamy", "Hypogamy")) +
  theme_classic() +
  scale_x_continuous(breaks=c(0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.6), limits=c(.33, .618)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size= 9)) +
  ggtitle("Prevalence of Hypergamy, Homogamy, and Hypogamy") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=8),
        panel.background = element_rect(colour = "black", size=0.5)) +
  theme(legend.title=element_blank()) +
  guides(fill = FALSE) +
theme(legend.text = element_text(size = 7)) +
  theme(axis.text=element_text(size=8)) + 
  theme(axis.title=element_text(size=8)) 

legend <- get_legend(FAdv_Hom_LA_restricted_60 + theme(legend.position="bottom"))

FAdv_Hom_LA_restricted_60



###############################################################
#Prevalence of Hypergamy, Hypogamy, and Homogamy Across Educational Female Advantage #
##############################################################

# Generating bottom plot.

# Using data set that contains female advantage index wide format.
latamwideR<-data.frame(read.csv("/Users/danielar.urbina/Dropbox/ResearchNote_Hypergamy/Data/LatamFA_wide_60.csv"))
latamwideR$ratio <- log(latamwideR$count_Hypergamy/latamwideR$count_Hypogamy)

FAdv_H_Index_RandR <-ggplot(latamwideR, aes(x = FA_women, y = ratio)) +
  geom_point(aes (color = "lightskyblue3"), show.legend = FALSE) +
  scale_color_manual(values = c("lightskyblue3"),
                     labels = c("Prevalence of Hypergamy")) +
  geom_smooth(aes(color =  "lightskyblue3"),  method = "lm", size=0.7, se= T , fill = "lightskyblue3", alpha = 0.2) + 
  labs(x = "Female Advantage Index", y = "ln(hypergamy/hypogamy)", fill = "") +
  theme_classic() +
  scale_x_continuous(breaks=c(0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.6)) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size= 9)) +
  ggtitle("Logged Ratio of Hypergamy to Hypogamy") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=0.5)) +
  scale_y_continuous(breaks=c(-1.0, -0.5, 0.00, 0.5, 1.0, 1.5, 2.0)) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=0.5)) +
  theme(legend.text = element_text(size = 7), axis.title  = element_text(size=8), axis.text  = element_text(size=8)) +
  theme(legend.title=element_blank())

FAdv_H_Index_RandR


#Putting plots together

F4_FARandR <- grid.arrange(arrangeGrob(FAdv_H_Index_RandR + theme(legend.position="none"),
                                                 FAdv_Hom_LA_restricted_60 + theme(legend.position="bottom"), nrow=2))

annotate_figure(F4_FARandR, 
                top = text_grob("FIGURE 4   Logged Ratio of Hypergamy to Hypogamy and Prevalence of \n Hypergamy, Hypogamy, and Homogamy Across Educational Female \n Advantage.",
                color = "black", face = "bold", size = 9, x = 0, hjust = 0),
                bottom = text_grob("Note: Best-fit lines estimated using linear regressions, with 95% confidence intervals displayed using shading", 
                                   color = "black", hjust = 0 , x = 0, size = 7),)






