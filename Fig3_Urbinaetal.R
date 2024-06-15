# Figure 1 PDR article. Urbina, Frye, and Lopus. 

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
argentina <-data.frame(read.csv("argentina_un.csv"))
brazil <-data.frame(read.csv("brazil_un.csv"))
chile <-data.frame(read.csv("chile_un.csv"))
colombia <-data.frame(read.csv("colombia_un.csv"))
ecuador <-data.frame(read.csv("ecuador_un.csv"))
mexico <-data.frame(read.csv("mexico_un.csv"))
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
#                           Plot Pairings by Birth Cohort                       #
#########################################################################################

edu_coh <- function(x) {
  x%>% 
    filter(mating != "NA") %>%   
    filter(Cohort_W_10 != "NA") %>%   
    filter(age_W >= "25" & age_W <= "60") %>%   
    filter(Cohort_W_10 <= "1980") %>% 
    filter(Cohort_W_10 >= "1920") %>% 
    filter(Cohort_W_10 != "NA") %>% 
    group_by(Cohort_W_10,mating) %>% 
    summarise(count=n()) %>% 
    mutate(perc=count/sum(count))
}

edu_coh_Het <- function(x) {
  x%>% 
    filter(mating != "NA") %>% 
    filter(mating != "3")  %>% 
    filter(age_W >= "25" & age_W <= "60") %>% 
    filter(Cohort_W_10 <= "1980") %>% 
    filter(Cohort_W_10 >= "1920") %>% 
    filter(Cohort_W_10 != "NA") %>% 
    group_by(Cohort_W_10,mating) %>% 
    summarise(count=n()) %>% 
    mutate(perc=count/sum(count))
}


# Function edu_coh
d <- c("argentina", "bolivia", "brazil", "chile", "colombia", "costarica", "dominicana", 
       "ecuador", "honduras", "mexico", "nicaragua", "panama", "paraguay", "peru", "salvador",
       "uruguay")
names <- c("argentina", "bolivia", "brazil", "chile", "colombia", "costarica", "dominicana", 
           "ecuador", "honduras", "mexico", "nicaragua", "panama", "paraguay", "peru", "salvador",
           "uruguay")
dts <- list(argentina, bolivia, brazil, chile, colombia, costarica, dominicana, 
            ecuador, honduras, mexico, nicaragua, panama, paraguay, peru, salvador,
            uruguay)
gg <- list()

for (d in 1: length(d)) {
  gg[[d]]<- edu_coh(dts[[d]])
  assign(paste0("df1", names[d]), gg[[d]])
}

#addying country names

df1argentina$country	<-	"argentina"
df1bolivia$country	<-	"bolivia"
df1brazil$country	<-	"brazil"
df1chile$country	<-	"chile"
df1colombia$country	<-	"colombia"
df1costarica$country	<-	"costarica"
df1dominicana$country	<-	"dominicana"
df1ecuador$country	<-	"ecuador"
df1honduras$country	<-	"honduras"
df1mexico$country	<-	"mexico"
df1nicaragua$country	<-	"nicaragua"
df1panama$country	<-	"panama"
df1paraguay$country	<-	"paraguay"
df1peru$country	<-	"peru"
df1salvador$country	<-	"salvador"
df1uruguay$country	<-	"uruguay"


# Using function edu_coh_Het

d <- c("argentina", "bolivia", "brazil", "chile", "colombia", "costarica", "dominicana", 
       "ecuador", "honduras", "mexico", "nicaragua", "panama", "paraguay", "peru", "salvador",
       "uruguay")
names <- c("argentina", "bolivia", "brazil", "chile", "colombia", "costarica", "dominicana", 
           "ecuador", "honduras", "mexico", "nicaragua", "panama", "paraguay", "peru", "salvador",
           "uruguay")
dts <- list(argentina, bolivia, brazil, chile, colombia, costarica, dominicana, 
            ecuador, honduras, mexico, nicaragua, panama, paraguay, peru, salvador,
            uruguay)
gg <- list()


for (d in 1: length(d)) {
  gg[[d]]<- edu_coh_Het(dts[[d]])
  assign(paste0("df2", names[d]), gg[[d]])
}


df2argentina$country	<-	"argentina"
df2bolivia$country	<-	"bolivia"
df2brazil$country	<-	"brazil"
df2chile$country	<-	"chile"
df2colombia$country	<-	"colombia"
df2costarica$country	<-	"costarica"
df2dominicana$country	<-	"dominicana"
df2ecuador$country	<-	"ecuador"
df2honduras$country	<-	"honduras"
df2mexico$country	<-	"mexico"
df2nicaragua$country	<-	"nicaragua"
df2panama$country	<-	"panama"
df2paraguay$country	<-	"paraguay"
df2peru$country	<-	"peru"
df2salvador$country	<-	"salvador"
df2uruguay$country	<-	"uruguay"

# Recoding a.mating categories
# df1_country contains three categories (homogamy, hypergamy, hypogamy); df2_country two categories (hypergamy and hypogamy)

# Argentina

df2argentina$mating <- recode_factor(df2argentina$mating, "1" = "Hypergamy/Heterogamy", 
                                     "2" = "Hypogamy/Heterogamy")
df1argentina$mating <- recode_factor(df1argentina$mating, '1'='1', '2'='3', '3'='2')
df1argentina$mating <- recode_factor(df1argentina$mating, "1" = "Hypergamy", 
                                     "2" = "Homogamy", "3" = "Hypogamy")
df2argentina<-subset(df2argentina, mating == "Hypergamy/Heterogamy") 
dfargentina <- rbind(df1argentina, df2argentina)
dfargentina$Cohort_W_10 <- recode(dfargentina$Cohort_W_10, '1920'='1925', '1930'='1935',
                                  '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985')
dfargentina$Cohort_W_10 <-as.numeric(as.character(dfargentina$Cohort_W_10))

# Bolivia  
df2bolivia$mating <- recode_factor(df2bolivia$mating, "1" = "Hypergamy/Heterogamy", 
                                   "2" = "Hypogamy/Heterogamy")
df1bolivia$mating <- recode_factor(df1bolivia$mating, '1'='1', '2'='3', '3'='2')
df1bolivia$mating <- recode_factor(df1bolivia$mating, "1" = "Hypergamy", 
                                   "2" = "Homogamy", "3" = "Hypogamy")
df2bolivia<-subset(df2bolivia, mating == "Hypergamy/Heterogamy") 
dfbolivia <- rbind(df1bolivia, df2bolivia)
dfbolivia$Cohort_W_10 <- recode_factor(dfbolivia$Cohort_W_10, '1920'='1925', '1930'='1935',
                                       '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985')
dfbolivia$Cohort_W_10 <-as.numeric(as.character(dfbolivia$Cohort_W_10))

# Brazil
df2brazil$mating <- recode_factor(df2brazil$mating, "1" = "Hypergamy/Heterogamy", 
                                  "2" = "Hypogamy/Heterogamy")
df1brazil$mating <- recode_factor(df1brazil$mating, '1'='1', '2'='3', '3'='2')
df1brazil$mating <- recode_factor(df1brazil$mating, "1" = "Hypergamy", 
                                  "2" = "Homogamy", "3" = "Hypogamy")
df2brazil<-subset(df2brazil, mating == "Hypergamy/Heterogamy") 
dfbrazil <- rbind(df1brazil, df2brazil)
dfbrazil$Cohort_W_10 <- recode_factor(dfbrazil$Cohort_W_10, '1920'='1925', '1930'='1935',
                                      '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985')
dfbrazil$Cohort_W_10 <-as.numeric(as.character(dfbrazil$Cohort_W_10))

# Chile
df2chile$mating <- recode_factor(df2chile$mating, "1" = "Hypergamy/Heterogamy", 
                                 "2" = "Hypogamy/Heterogamy")
df1chile$mating <- recode_factor(df1chile$mating, '1'='1', '2'='3', '3'='2')
df1chile$mating <- recode_factor(df1chile$mating, "1" = "Hypergamy", 
                                 "2" = "Homogamy", "3" = "Hypogamy")
df2chile<-subset(df2chile, mating == "Hypergamy/Heterogamy") 
dfchile <- rbind(df1chile, df2chile)
dfchile$Cohort_W_10 <- recode_factor(dfchile$Cohort_W_10, '1920'='1925', '1930'='1935',
                                     '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985')
dfchile$Cohort_W_10 <-as.numeric(as.character(dfchile$Cohort_W_10))

# Colombia
df2colombia$mating <- recode_factor(df2colombia$mating, "1" = "Hypergamy/Heterogamy", 
                                    "2" = "Hypogamy/Heterogamy")
df1colombia$mating <- recode_factor(df1colombia$mating, '1'='1', '2'='3', '3'='2')
df1colombia$mating <- recode_factor(df1colombia$mating, "1" = "Hypergamy", 
                                    "2" = "Homogamy", "3" = "Hypogamy")
df2colombia<-subset(df2colombia, mating == "Hypergamy/Heterogamy") 
dfcolombia <- rbind(df1colombia, df2colombia)
dfcolombia$Cohort_W_10 <- recode_factor(dfcolombia$Cohort_W_10, '1920'='1925', '1930'='1935',
                                        '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985')
dfcolombia$Cohort_W_10 <-as.numeric(as.character(dfcolombia$Cohort_W_10))

#Costa Rica
df2costarica$mating <- recode_factor(df2costarica$mating, "1" = "Hypergamy/Heterogamy", 
                                     "2" = "Hypogamy/Heterogamy")
df1costarica$mating <- recode_factor(df1costarica$mating, '1'='1', '2'='3', '3'='2')
df1costarica$mating <- recode_factor(df1costarica$mating, "1" = "Hypergamy", 
                                     "2" = "Homogamy", "3" = "Hypogamy")
df2costarica<-subset(df2costarica, mating == "Hypergamy/Heterogamy") 
dfcostarica <- rbind(df1costarica, df2costarica)
dfcostarica$Cohort_W_10 <- recode_factor(dfcostarica$Cohort_W_10, '1920'='1925', '1930'='1935',
                                         '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985')
dfcostarica$Cohort_W_10 <-as.numeric(as.character(dfcostarica$Cohort_W_10))

# R. Dominicana
df2dominicana$mating <- recode_factor(df2dominicana$mating, "1" = "Hypergamy/Heterogamy", 
                                      "2" = "Hypogamy/Heterogamy")
df1dominicana$mating <- recode_factor(df1dominicana$mating, '1'='1', '2'='3', '3'='2')
df1dominicana$mating <- recode_factor(df1dominicana$mating, "1" = "Hypergamy", 
                                      "2" = "Homogamy", "3" = "Hypogamy")
df2dominicana<-subset(df2dominicana, mating == "Hypergamy/Heterogamy") 
dfdominicana <- rbind(df1dominicana, df2dominicana)
dfdominicana$Cohort_W_10 <- recode_factor(dfdominicana$Cohort_W_10, '1920'='1925', '1930'='1935',
                                          '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985')
dfdominicana$Cohort_W_10 <-as.numeric(as.character(dfdominicana$Cohort_W_10))

# Ecuador
df2ecuador$mating <- recode_factor(df2ecuador$mating, "1" = "Hypergamy/Heterogamy", 
                                   "2" = "Hypogamy/Heterogamy")
df1ecuador$mating <- recode_factor(df1ecuador$mating, '1'='1', '2'='3', '3'='2')
df1ecuador$mating <- recode_factor(df1ecuador$mating, "1" = "Hypergamy", 
                                   "2" = "Homogamy", "3" = "Hypogamy")
df2ecuador<-subset(df2ecuador, mating == "Hypergamy/Heterogamy") 
dfecuador <- rbind(df1ecuador, df2ecuador)
dfecuador$Cohort_W_10 <- recode_factor(dfecuador$Cohort_W_10, '1920'='1925', '1930'='1935',
                                       '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985')
dfecuador$Cohort_W_10 <-as.numeric(as.character(dfecuador$Cohort_W_10))

# Honduras
df2honduras$mating <- recode_factor(df2honduras$mating, "1" = "Hypergamy/Heterogamy", 
                                    "2" = "Hypogamy/Heterogamy")
df1honduras$mating <- recode_factor(df1honduras$mating, '1'='1', '2'='3', '3'='2')
df1honduras$mating <- recode_factor(df1honduras$mating, "1" = "Hypergamy", 
                                    "2" = "Homogamy", "3" = "Hypogamy")
df2honduras<-subset(df2honduras, mating == "Hypergamy/Heterogamy") 
dfhonduras <- rbind(df1honduras, df2honduras)
dfhonduras$Cohort_W_10 <- recode_factor(dfhonduras$Cohort_W_10, '1920'='1925', '1930'='1935',
                                        '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985')
dfhonduras$Cohort_W_10 <-as.numeric(as.character(dfhonduras$Cohort_W_10))

# Mexico
df2mexico$mating <- recode_factor(df2mexico$mating, "1" = "Hypergamy/Heterogamy", 
                                  "2" = "Hypogamy/Heterogamy")
df1mexico$mating <- recode_factor(df1mexico$mating, '1'='1', '2'='3', '3'='2')
df1mexico$mating <- recode_factor(df1mexico$mating, "1" = "Hypergamy", 
                                  "2" = "Homogamy", "3" = "Hypogamy")
df2mexico<-subset(df2mexico, mating == "Hypergamy/Heterogamy") 
dfmexico <- rbind(df1mexico, df2mexico)
dfmexico$Cohort_W_10 <- recode_factor(dfmexico$Cohort_W_10, '1920'='1925', '1930'='1935',
                                      '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985', '1990' = '1995')
dfmexico$Cohort_W_10 <-as.numeric(as.character(dfmexico$Cohort_W_10))

# Nicaragua
df2nicaragua$mating <- recode_factor(df2nicaragua$mating, "1" = "Hypergamy/Heterogamy", 
                                     "2" = "Hypogamy/Heterogamy")
df1nicaragua$mating <- recode_factor(df1nicaragua$mating, '1'='1', '2'='3', '3'='2')
df1nicaragua$mating <- recode_factor(df1nicaragua$mating, "1" = "Hypergamy", 
                                     "2" = "Homogamy", "3" = "Hypogamy")
df2nicaragua<-subset(df2nicaragua, mating == "Hypergamy/Heterogamy") 
dfnicaragua <- rbind(df1nicaragua, df2nicaragua)
dfnicaragua$Cohort_W_10 <- recode_factor(dfnicaragua$Cohort_W_10, '1920'='1925', '1930'='1935',
                                         '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985')
dfnicaragua$Cohort_W_10 <-as.numeric(as.character(dfnicaragua$Cohort_W_10))

# Panama
df2panama$mating <- recode_factor(df2panama$mating, "1" = "Hypergamy/Heterogamy", 
                                  "2" = "Hypogamy/Heterogamy")
df1panama$mating <- recode_factor(df1panama$mating, '1'='1', '2'='3', '3'='2')
df1panama$mating <- recode_factor(df1panama$mating, "1" = "Hypergamy", 
                                  "2" = "Homogamy", "3" = "Hypogamy")
df2panama<-subset(df2panama, mating == "Hypergamy/Heterogamy") 
dfpanama <- rbind(df1panama, df2panama)
dfpanama$Cohort_W_10 <- recode_factor(dfpanama$Cohort_W_10, '1920'='1925', '1930'='1935',
                                      '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985')
dfpanama$Cohort_W_10 <-as.numeric(as.character(dfpanama$Cohort_W_10))

# Paraguay
df2paraguay$mating <- recode_factor(df2paraguay$mating, "1" = "Hypergamy/Heterogamy", 
                                    "2" = "Hypogamy/Heterogamy")
df1paraguay$mating <- recode_factor(df1paraguay$mating, '1'='1', '2'='3', '3'='2')
df1paraguay$mating <- recode_factor(df1paraguay$mating, "1" = "Hypergamy", 
                                    "2" = "Homogamy", "3" = "Hypogamy")
df2paraguay<-subset(df2paraguay, mating == "Hypergamy/Heterogamy") 
dfparaguay <- rbind(df1paraguay, df2paraguay)
dfparaguay$Cohort_W_10 <- recode_factor(dfparaguay$Cohort_W_10, '1920'='1925', '1930'='1935',
                                        '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985')
dfparaguay$Cohort_W_10 <-as.numeric(as.character(dfparaguay$Cohort_W_10))

# Peru
df2peru$mating <- recode_factor(df2peru$mating, "1" = "Hypergamy/Heterogamy", 
                                "2" = "Hypogamy/Heterogamy")

df1peru$mating <- recode_factor(df1peru$mating, '1'='1', '2'='3', '3'='2')

df1peru$mating <- recode_factor(df1peru$mating, "1" = "Hypergamy", 
                                "2" = "Homogamy", "3" = "Hypogamy")
df2peru<-subset(df2peru, mating == "Hypergamy/Heterogamy") 
dfperu <- rbind(df1peru, df2peru)
dfperu$Cohort_W_10 <- recode_factor(dfperu$Cohort_W_10, '1920'='1925', '1930'='1935',
                                    '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985')
dfperu$Cohort_W_10 <-as.numeric(as.character(dfperu$Cohort_W_10))

# El Salvador 
df2salvador$mating <- recode_factor(df2salvador$mating, "1" = "Hypergamy/Heterogamy", 
                                    "2" = "Hypogamy/Heterogamy")
df1salvador$mating <- recode_factor(df1salvador$mating, '1'='1', '2'='3', '3'='2')

df1salvador$mating <- recode_factor(df1salvador$mating, "1" = "Hypergamy", 
                                    "2" = "Homogamy", "3" = "Hypogamy")
df2salvador<-subset(df2salvador, mating == "Hypergamy/Heterogamy") 
dfsalvador <- rbind(df1salvador, df2salvador)
dfsalvador$Cohort_W_10 <- recode_factor(dfsalvador$Cohort_W_10, '1920'='1925', '1930'='1935',
                                        '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985')
dfsalvador$Cohort_W_10 <-as.numeric(as.character(dfsalvador$Cohort_W_10))

# Uruguay
df2uruguay$mating <- recode_factor(df2uruguay$mating, "1" = "Hypergamy/Heterogamy", 
                                   "2" = "Hypogamy/Heterogamy")
df1uruguay$mating <- recode_factor(df1uruguay$mating, '1'='1', '2'='3', '3'='2')
df1uruguay$mating <- recode_factor(df1uruguay$mating, "1" = "Hypergamy", 
                                   "2" = "Homogamy", "3" = "Hypogamy")
df2uruguay<-subset(df2uruguay, mating == "Hypergamy/Heterogamy") 
dfuruguay <- rbind(df1uruguay, df2uruguay)
dfuruguay$Cohort_W_10 <- recode_factor(dfuruguay$Cohort_W_10, '1920'='1925', '1930'='1935',
                                       '1940'='1945', '1950' = '1955', '1960' = '1965', '1970' = '1975', '1980' = '1985')
dfuruguay$Cohort_W_10 <-as.numeric(as.character(dfuruguay$Cohort_W_10))

##############################
# Multi-Country Plot: Trends in Hypergamy, Homogamy, and Hypogamy Across Birth Cohorts in 16 Latin American Countries. 
##############################

## Generating plots

list <-c("dfargentina", "dfbolivia", "dfbrazil", "dfchile", "dfcolombia", "dfcostarica", "dfdominicana", 
         "dfecuador", "dfhonduras", "dfmexico", "dfnicaragua", "dfpanama", "dfparaguay", "dfperu", "dfsalvador", "dfuruguay")
titles <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Dom. Republic", 
            "Ecuador", "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru",
            "El Salvador", "Uruguay")
myplots <- list() 


myplot_1 <- function(data,title) {
  print(ggplot(data, aes(x = Cohort_W_10, y = perc*100)) +
          geom_line(aes(linetype = factor(mating),size = factor(mating), color = factor(mating)), se = FALSE, show.legend = TRUE) +
          scale_linetype_manual(values=c("solid", "solid", "solid", "twodash"),
                                labels = c("% Hypergamy",  "% Homogamy", "% Hypogamy", "% Hypergamy (Homogamy excluded)"))+
          scale_size_manual(values = c(1, 0.5, 0.5, 1),
                            labels = c("% Hypergamy",  "% Homogamy", "% Hypogamy", "% Hypergamy (Homogamy excluded)"))+
          scale_color_manual(values = c("palevioletred3", "#999999","#E69F00", "lightskyblue3"),
                             labels = c("% Hypergamy", "% Homogamy", "% Hypogamy", "% Hypergamy (Homogamy excluded)"))+
          labs(title = title, x = "Wives'Cohort", y = "% of marriages", fill = "") +
          ylim(0, 100) +
          #scale_x_continuous(breaks=c(1920, 1940, 1960, 1980), limits=c(1920, 1985)) +
          scale_x_continuous(breaks=seq(1925, 1985, 20),limits=c(1925, 1985), labels=c("1920s", "1940s", "1960s", "1980s")) +
          #theme_light() +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size=7)) +
          theme(legend.text = element_text(size = 6), axis.title  = element_text(size=6), axis.text  = element_text(size=6))  +
          #theme_minimal(base_size = 14) +
          theme(axis.text.x=element_text(angle=45)) +
          theme(legend.position="none") +
          theme(legend.title=element_blank()))
  
}

for(i in seq_along(list)){ 
  myplots[[i]] <- print(myplot_1(get(list[i]), titles[i]))
}


# Arranging plot for PDR

F3Cohort_LA_RandR<-ggarrange(myplots[[1]], myplots[[2]],myplots[[3]], myplots[[4]],
                      myplots[[5]], myplots[[6]], myplots[[7]], myplots[[8]],myplots[[15]],
                      myplots[[9]],myplots[[10]], myplots[[11]], myplots[[12]], myplots[[13]],
                      myplots[[14]], myplots[[16]], common.legend = TRUE, legend="bottom")

F3Cohort_LA_RandR

annotate_figure(F3Cohort_LA_RandR,
                top = text_grob("FIGURE 3  Trends in Hypergamy, Homogamy, and Hypogamy Across Birth \n Cohorts in 16 Latin American Countries.", color = "black", face = "bold", size = 9, x = 0, hjust = 0),
)


