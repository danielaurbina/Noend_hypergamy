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
library("tidyverse")
library("gridExtra")
library("ggpubr")
library(RColorBrewer)
library("wesanderson")
library("cowplot")
library("ggrepel")
library(lubridate)
library(ggpp)
library(grid)
library(pals)
pal.bands(alphabet, alphabet2, cols25, glasbey, kelly, polychrome, 
          stepped, tol, watlington,
          show.names=FALSE)

#setwd
argentina <-data.frame(read.csv("argentina_univ.csv"))
brazil <-data.frame(read.csv("brazil_univ.csv"))
chile <-data.frame(read.csv("chile_univ.csv"))
colombia <-data.frame(read.csv("colombia_univ.csv"))
ecuador <-data.frame(read.csv("ecuador_univ.csv"))
mexico <-data.frame(read.csv("mexico_univ_min.csv"))
nicaragua <-data.frame(read.csv("nicaragua_univ.csv"))
panama <-data.frame(read.csv("panama_univ.csv"))
paraguay <-data.frame(read.csv("paraguay_univ.csv"))
peru <-data.frame(read.csv("peru_univ.csv"))
salvador <-data.frame(read.csv("salvador_univ.csv"))
uruguay <-data.frame(read.csv("uruguay_univ.csv"))
dominicana <-data.frame(read.csv("dominicana_univ.csv"))
honduras <-data.frame(read.csv("honduras_univ.csv"))
costarica <-data.frame(read.csv("costarica_univ.csv"))
bolivia <-data.frame(read.csv("bolivia_univ.csv"))


#########################################################################################
#                         Figure 1: Plot Cumulative Educational Attainment                      #
#########################################################################################

# Function for gap by educational level


women_Cprimary <- function(x) {
  x  %>%
    filter(Cohort_10 <= "1980") %>% 
    filter(Cohort_10 >= "1920") %>% 
    filter(edattain != "9") %>% 
    filter(FA != "NA") %>%
    filter(sex == 2) %>%
    group_by(Cohort_10, country, edattain) %>% 
    summarise(count_T=n()) %>%
    mutate(perc_W= sum(count_T[edattain > 1])/sum(count_T))  
}


women_Csecondary <- function(x) {
  x  %>%
    filter(Cohort_10 <= "1980") %>% 
    filter(Cohort_10 >= "1920") %>% 
    filter(edattain != "9") %>% 
    filter(FA != "NA") %>%
    filter(sex == 2) %>%
    group_by(Cohort_10, country, edattain) %>% 
    summarise(count_T=n()) %>%
    mutate(perc_W= sum(count_T[edattain > 2])/sum(count_T))  
}


#########################################################################################
#                               Plot cumulative attainment primary                          #
#########################################################################################

## Generating data sets for plot 
h <- c("argentina", "bolivia", "brazil", "chile", "colombia", "costarica", "dominicana", 
       "ecuador", "honduras",  "mexico", "nicaragua", "panama", "paraguay", "peru", "salvador",
       "uruguay")
names <- c("argentina", "bolivia", "brazil", "chile", "colombia", "costarica", "dominicana", 
           "ecuador", "honduras", "mexico", "nicaragua", "panama", "paraguay", "peru", "salvador",
           "uruguay")
tes <- list(argentina, bolivia, brazil, chile, colombia, costarica, dominicana, 
            ecuador, honduras, mexico, nicaragua, panama, paraguay, peru, salvador,
            uruguay)

cc <- list()
for (h in 1: length(h)) {
  cc[[h]]<- women_Cprimary(tes[[h]])
  assign(paste0("df3", names[h]), cc[[h]])
}

Latam2<-rbind(df3argentina, df3bolivia, df3brazil, df3chile, df3colombia, df3costarica, df3dominicana, 
              df3ecuador, df3honduras, df3mexico, df3nicaragua, df3panama, df3paraguay, df3peru 
              , df3salvador, df3uruguay)

Latam2$country <- factor(Latam2$country,
                         levels = c(32, 68, 76, 152, 170, 188, 214, 218, 340, 484, 558, 591, 600, 604, 222, 858),
                         labels = c("ARG", "BOL", "BRA", "CHI", "COL", "CR", "DR", 
                                    "ECU", "HON", "MEX", "NIC", "PAN", "PAR", "PER",
                                    "SAL", "URU"))
#For labels 
data_ends <- Latam2 %>% 
  filter(edattain == "1") %>% 
  group_by(country) %>% 
  top_n(1, Cohort_10) 


# Graph primary completion

women_Cprimary <-ggplot(Latam2, aes(x = Cohort_10, y = perc_W*100)) +
  geom_line(aes(color = factor(country)), lwd=0.45, show.legend = TRUE) +
  scale_x_continuous(breaks=seq(1920, 1980,10),labels=c("1920s", "1930s", "1940s", "1950s","1960s", "1970s", "1980s")) +
  geom_text_repel(data = data_ends,
                  aes(label = country),
                  size = 2.0,
                  segment.color = 'white',
                  col=as.vector(cols25(16)),
                  point.size = 0,
                  box.padding = 0.05,
                  point.padding = 0.05,
                  fontface=("bold"),
                  hjust = "left",
                  direction = "y") +
  labs(title= "Completed primary \n education or more", x = "10-year birth cohort", y = "Share of women (%)", fill = "") +
  ylim(0, 100) +  
  scale_color_manual(values=as.vector(cols25(16))) +
  theme_classic() +
  theme(legend.text = element_text(size = 9)) +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold")) +
  theme(axis.text=element_text(size=8)) + 
  theme(axis.text.x = element_text(angle=45)) +
  theme(axis.title=element_text(size=8)) +
  theme(legend.position="none") +
  theme(legend.title=element_blank())

women_Cprimary



#########################################################################################
#                               Plot cumulative attainment secondary                         #
#########################################################################################

## Generating data sets for plot 
h <- c("argentina", "bolivia", "brazil", "chile", "colombia", "costarica", "dominicana", 
       "ecuador", "honduras",  "mexico", "nicaragua", "panama", "paraguay", "peru", "salvador",
       "uruguay")
names <- c("argentina", "bolivia", "brazil", "chile", "colombia", "costarica", "dominicana", 
           "ecuador", "honduras", "mexico", "nicaragua", "panama", "paraguay", "peru", "salvador",
           "uruguay")
tes <- list(argentina, bolivia, brazil, chile, colombia, costarica, dominicana, 
            ecuador, honduras, mexico, nicaragua, panama, paraguay, peru, salvador,
            uruguay)

cc <- list()
for (h in 1: length(h)) {
  cc[[h]]<- women_Csecondary(tes[[h]])
  assign(paste0("df4", names[h]), cc[[h]])
}

Latam4<-rbind(df4argentina, df4bolivia, df4brazil, df4chile, df4colombia, df4costarica, df4dominicana, 
              df4ecuador, df4honduras, df4mexico, df4nicaragua, df4panama, df4paraguay, df4peru 
              , df4salvador, df4uruguay)

Latam4$country <- factor(Latam4$country,
                         levels = c(32, 68, 76, 152, 170, 188, 214, 218, 340, 484, 558, 591, 600, 604, 222, 858),
                         labels = c("ARG", "BOL", "BRA", "CHI", "COL", "CR", "DR", 
                                    "ECU", "HON", "MEX", "NIC", "PAN", "PAR", "PER",
                                    "SAL", "URU"))
#For labels 
data_ends4 <- Latam4 %>% 
  filter(edattain == "1") %>% 
  group_by(country) %>% 
  top_n(1, Cohort_10) 


# Graph secondary completion

women_Csecondary <-ggplot(Latam4, aes(x = Cohort_10, y = perc_W*100)) +
  geom_line(aes(color = factor(country)), lwd=0.45, show.legend = TRUE) +
  #scale_x_continuous(breaks = Latam2$Cohort_10 ) +
  scale_x_continuous(breaks=seq(1920, 1980,10),labels=c("1920s", "1930s", "1940s", "1950s","1960s", "1970s", "1980s")) +
  #scale_x_discrete(labels = paste0(c("1920s", "1930s", "1940s", "1950s","1960s", "1970s", "1980s"))) +
  geom_text_repel(data = data_ends4,
                  aes(label = country),
                  size = 2.0,
                  segment.color = 'white',
                  col=as.vector(cols25(16)),
                  point.size = 0,
                  box.padding = 0.05,
                  point.padding = 0.07,
                  fontface=("bold"),
                  hjust = "left",
                  direction = "y") +
  labs(title= "Completed secondary \n education or more", x = "10-year birth cohort", y = "Share of women (%)", fill = "") +
  #geom_hline(yintercept = 50, color = "black", linetype = "dashed") +
  ylim(0, 100) +  
  #labs(tag = "C") +
  scale_color_manual(values=as.vector(cols25(16))) +
  theme_classic() +
  theme(legend.text = element_text(size = 9)) +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold")) +
  theme(axis.text=element_text(size=8)) + 
  theme(axis.text.x = element_text(angle=45)) +
  theme(axis.title=element_text(size=8)) +
  theme(legend.position="none") +
  theme(legend.title=element_blank())

women_Csecondary

#########################################################################################
#                                  Combined Figure 1                        #
#########################################################################################

#combine plots
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(gtable)


Figure1_combined <- grid.arrange(women_Cprimary, women_Csecondary, nrow=1,
             top = textGrob("FIGURE 1   Women’s Educational Attainment by Birth Cohort for 16 Latin American \n Countries.",  x = 0, hjust = 0, gp=gpar(fontsize=9)))


Figure1_notitle <- grid.arrange(women_Cprimary, women_Csecondary, nrow=1)



