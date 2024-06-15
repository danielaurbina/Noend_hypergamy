
# Figure 2 PDR article. Urbina, Frye, and Lopus. 

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
#      Figure 2: Gender Gap in the Proportion Completing Secondary Education or More by Birth Cohort for 16 Latin American Countries.                       #
#########################################################################################

# Function for graph
gap_secondary <- function(x) {
  x %>% 
    filter(Cohort_10 <= "1980" & Cohort_10 >= "1920" & edattain != "NA" & edattain != "9") %>% 
    filter(edattain > 2)  %>% 
    group_by(Cohort_10, country, sex) %>%
    mutate(count_s = n()) %>%
    ungroup() %>% 
    group_by(Cohort_10, country) %>%
    mutate(count_t = n()) %>%
    group_by(Cohort_10, country, sex)  %>%
    mutate(perc_W= sum(count_s[sex ==2])/sum(count_t)) %>% 
    filter(sex ==2)  %>% 
    mutate(perc_M= 1 - perc_W) %>% 
    mutate(gap_sec= perc_M - perc_W)
            
}



d <- c("argentina", "bolivia", "brazil", "chile", "colombia", "costarica", "dominicana", 
       "ecuador", "honduras", "mexico",  "nicaragua", "panama", "paraguay", "peru", "salvador",
       "uruguay")
names <- c("argentina", "bolivia", "brazil", "chile", "colombia", "costarica", "dominicana", 
           "ecuador", "honduras",  "mexico", "nicaragua", "panama", "paraguay", "peru", "salvador",
           "uruguay")
dda <- list(argentina, bolivia, brazil, chile, colombia, costarica, dominicana, 
            ecuador, honduras, mexico, nicaragua, panama, paraguay, peru, salvador,
            uruguay)

cc <- list()
for (d in 1: length(d)) {
  cc[[d]]<- gap_secondary(dda[[d]])
  assign(paste0("df7", names[d]), cc[[d]])
}

df7nicaragua$`9` <- NULL

Latam7<-rbind(df7argentina, df7bolivia, df7brazil, df7chile, df7colombia, df7costarica, df7dominicana, 
              df7ecuador, df7honduras, df7mexico, df7nicaragua, df7panama, df7paraguay, df7peru, 
              df7salvador, df7uruguay)

# Excluding Argentina 1980, no information on partner's education. 

df7argentina70 <- df7argentina %>% filter(Cohort_10 < "1980")
Latam7_corrected<-rbind(df7argentina70, df7bolivia, df7brazil, df7chile, df7colombia, df7costarica, df7dominicana, 
                        df7ecuador, df7honduras, df7mexico, df7nicaragua, df7panama, df7paraguay, df7peru, 
                        df7salvador, df7uruguay)


Latam7_corrected$country <- factor(Latam7_corrected$country,
                                   levels = c(32, 68, 76, 152, 170, 188, 214, 218, 340, 484, 558, 591, 600, 604, 222, 858),
                                   labels = c("ARG", "BOL", "BRA", "CHI", "COL", "CR", "DR", 
                                              "ECU", "HON", "MEX", "NIC", "PAN", "PAR", "PER",
                                              "SAL", "URU"))

Latam7$country <- factor(Latam7$country,
                         levels = c(32, 68, 76, 152, 170, 188, 214, 218, 340, 484, 558, 591, 600, 604, 222, 858),
                         labels = c("ARG", "BOL", "BRA", "CHI", "COL", "CR", "DR", 
                                    "ECU", "HON", "MEX", "NIC", "PAN", "PAR", "PER",
                                    "SAL", "URU"))

# Final dataset for graph
Latam_sec<- aggregate(cbind(gap_sec) ~ country + Cohort_10, data = Latam7, FUN = mean, na.action = na.omit)


#For labels 

data_ends7 <- Latam_sec %>% 
  group_by(country) %>% 
  top_n(1, Cohort_10) 

F2gender_gap_sec <- ggplot(Latam_sec, aes(x = Cohort_10, y = gap_sec, color = factor(country))) +
  geom_line(lwd = 0.45, show.legend = TRUE) +
  scale_x_continuous(breaks = seq(1920, 1980, 10), labels = c("1920s", "1930s", "1940s", "1950s", "1960s", "1970s", "1980s")) +
  scale_color_manual(values = as.vector(cols25(16))) +
  geom_text_repel(data = data_ends7,
                  aes(label = country),
                  size = 2.5,
                  point.size = 0,
                  box.padding = 0.05,
                  point.padding = 0.07,
                  fontface = "bold",
                  hjust = "left",
                  direction = "y") +
  labs(title = " ", x = "10-year birth cohort", y = "Difference in Proportion (Men - Women)", fill = "") +
  geom_hline(yintercept = 0.0, color = "black", linetype = "dashed") +
  theme_classic() +
  theme(legend.text = element_text(size = 9)) +
  theme(axis.text=element_text(size=8)) + 
  #theme(axis.text.x = element_text(angle=45)) +
  theme(axis.title=element_text(size=8)) +
  theme(legend.position="none") 
  #ggtitle("FIGURE 2 Gender Gap in the Proportion Completing Secondary or More \n by Birth Cohort.") +
  #theme(plot.title = element_text(size = 9, face = "bold", hjust = 0)) 

# Print the plot
print(F2gender_gap_sec)

annotate_figure(F2gender_gap_sec,
                top = text_grob("FIGURE 2 Gender Gap in the Proportion Completing Secondary or More by \n Birth Cohort.", color = "black", face = "bold", size = 9, x = 0, hjust = 0),)




