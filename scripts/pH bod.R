library(dplyr)
library(readxl)
df <- read_xlsx("data/Report Measurement Data.xlsx", 
                sheet = "Sheet1")
df$Coliform <-  as.numeric(df$Coliform)
df$COD<-  as.numeric(df$COD)
df$Sampling_Site  <-  as.factor(df$Sampling_Site)
  
summary(df[,-c(1,2)])

library(car)
library(agricolae)

mod <- aov(BOD~Sampling_Site, df)
h2 <- HSD.test(mod, 'Sampling_Site',  alpha = 0.05)
workthis <- cbind(inner_join(h2$means, h2$groups), names=row.names(h2$means))

library(ggplot2)
library(ggpattern)
library(extrafont)
loadfonts()
guidline <- 30
bodp <- workthis %>% 
     ggplot(aes(factor(names), BOD))+
     geom_bar_pattern(stat = 'identity',
                      fill='white',
                      color = 'black',
                      pattern_density=0.5,
                      aes(pattern = names,
                          pattern_angle = BOD),
                      show.legend = F)+
     geom_errorbar(aes(ymin=BOD-(2*se), ymax = BOD+(2*se)), width = 0.2)+
     geom_text(aes(y= BOD + 3, label=groups),
               color = '#000000', hjust = -0.5,
               family = "Times New Roman", size =4)+
     #geom_text(x = 3, y=guidline+guidline*0.11, label = "Mynmar Emission Standard #for BOD = 30", col = 'red')+
     geom_hline(yintercept = guidline, col = 'red' , lty = 4)+
     labs(x="Sampling Site",
          y= "BOD (mg/L), mean +/- 2 SE",
          caption = "Red line: Mynmar Emission Standard for BOD = 30")+
     theme_bw()+
     theme(text = element_text(family = "Times New Roman", size =12))

ggsave(filename="figures/bod.png", bodp , width = 4, height = 3, dpi = 300)
######################

mod2 <- aov(pH~Sampling_Site, df)
h3 <- duncan.test(mod2, 'Sampling_Site',  alpha = 0.05)
workthis <- cbind(inner_join(h3$means, h3$groups), names=row.names(h3$means))
guidline1 <- 6.5
guidline2 <- 9

php <- workthis %>% 
     ggplot(aes(factor(names), pH))+
     geom_bar_pattern(stat = 'identity',
                      fill='white',
                      color = 'black',
                      pattern_density=0.5,
                      aes(pattern = names,
                         pattern_angle = pH),
                      show.legend = F)+
     geom_errorbar(aes(ymin=pH-(2*se), ymax = pH+(2*se)), width = 0.2)+
     geom_text(aes(y= pH + 3, label=groups),
               color = '#000000', hjust = -0.5,
               family = "Times New Roman", size =4)+
     #geom_text(x = 3, y=guidline+guidline*0.11, label = "Mynmar Emission Standard #for BOD = 30", col = 'red')+
     geom_hline(yintercept = guidline1, col = 'red' , lty = 3)+
     geom_hline(yintercept = guidline2, col = 'orange' , lty = 5)+
     labs(x="Sampling Site",
          y= "pH, mean +/- 2 SE",
          caption = "Red line: Mynmar Emission Standard for pH (lower limit) = 6.5\n
          Orange Line: Mmyanmar Emissiono Standard for pH (upper limit) = 9")+
     theme_bw()+
     theme(text = element_text(family = "Times New Roman", size =12),
           plot.caption = element_text(size = 8, color = 'grey50'))

ggsave(filename="figures/pH.png", php , width = 4, height = 3, dpi = 300)
