# Mechanisms-of-Indirect-Genetic-Effects-on-Sociability---R-code
R code for the figures made. 
This is code to be used in R. 
Still working on how to actually use github, will update this but for now just copy and paste the following code into R if you want to actually use it. 





library(tidyr)
library(ggplot2)
library(dplyr)
library(forcats)


# custom standard error formula
std <- function(x) sd(x)/sqrt(length(x))


# Note that for the paper, all titles will be cropped out, I just left them
# here for ease of organizing

# Experiment 1!
# 8 sectioned arenas sociability (Sec5 gene validation) 



data8 <- read.csv("8 Sectioned arena sociability csv.csv") # data framework for the 8-sectioned arenas




# Create averages
data8ave <- data.frame(
  Sex=c("Male","Male", "Female", "Female") ,  
  
  Lineage = c("Sec5 knockdown", "Control","Sec5 knockdown", "Control"), 
  
  value = c((mean(as.numeric(data8[data8$Treatment == "Sec5" & data8$Sex == "M", "Soc_index"]))), 
            (mean(as.numeric(data8[data8$Treatment == "Control" & data8$Sex == "M", "Soc_index"]))),
            (mean(as.numeric(data8[data8$Treatment == "Sec5" & data8$Sex == "F", "Soc_index"]))),
             (mean(as.numeric(data8[data8$Treatment == "Control" & data8$Sex == "F", "Soc_index"]))) ), 
  
  errorbars = c((std(data8[data8$Treatment == "Sec5" & data8$Sex == "M", "Soc_index"])), 
                (std(data8[data8$Treatment == "Control" & data8$Sex == "M", "Soc_index"])),
                (std(data8[data8$Treatment == "Sec5" & data8$Sex == "F", "Soc_index"])),
                (std(data8[data8$Treatment == "Control" & data8$Sex == "F", "Soc_index"])) )
)



# Graph the data 
data8ave %>%
  mutate(Sex = fct_relevel(Sex, 
                           "Female", "Male"),
         Lineage = fct_relevel(Lineage, 
                               "Control", "Sec5 knockdown")) %>%
  ggplot(aes(fill = Lineage , x=Sex, y=value)) + 
  geom_bar(position="dodge", stat = "identity") +
  ggtitle("Experiment 1 
          \nAverage Sociability Scores as a Function of Lineage and Sex") +
  xlab("Sex") +
  ylab("Sociability score") +
  scale_fill_manual(name = "Treatment",
                    values = c("#d80033", "#3f5aa9"))  +
  geom_errorbar( aes(x=Sex, ymin=value-errorbars, ymax=value+errorbars), width=0.4,
                 colour="#000000", alpha=0.5, size=0.7, position=position_dodge(0.9)) +
  theme_bw() +
  theme(legend.position="right",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank() 
  ) 



# Now onto Experiment 2! 
# Sociability scores for the 4-sectioned arenas (IGE experiment)

data4 <- read.csv("Soc scores (4 days) cvs.csv") # data framework



# Create averages
data4ave <- data.frame(
  Sex=c("Male","Male", "Female", "Female") ,  
  
  Lineage = c("Sec5 knockdown", "Control","Sec5 knockdown", "Control"), 
  
  value = c((mean(as.numeric(data4[data4$Treatment == "Sec5" & data4$Sex == "M", "Soc_index"]))), 
            (mean(as.numeric(data4[data4$Treatment == "Control" & data4$Sex == "M", "Soc_index"]))),
            (mean(as.numeric(data4[data4$Treatment == "Sec5" & data4$Sex == "F", "Soc_index"]))),
            (mean(as.numeric(data4[data4$Treatment == "Control" & data4$Sex == "F", "Soc_index"]))) ), 
  
  errorbars = c((std(data4[data4$Treatment == "Sec5" & data4$Sex == "M", "Soc_index"])), 
                (std(data4[data4$Treatment == "Control" & data4$Sex == "M", "Soc_index"])),
                (std(data4[data4$Treatment == "Sec5" & data4$Sex == "F", "Soc_index"])),
                (std(data4[data4$Treatment == "Control" & data4$Sex == "F", "Soc_index"])) )
)





# Graph the data 
data4ave %>%
  mutate(Sex = fct_relevel(Sex, 
                           "Female", "Male"),
         Lineage = fct_relevel(Lineage, 
                               "Control", "Sec5 knockdown")) %>%
  ggplot(aes(fill = Lineage , x=Sex, y=value)) + 
  geom_bar(position="dodge", stat = "identity") +
  ggtitle("Experiment 2 
          \nAverage Sociability Scores as a Function of Lineage and Sex") +
  xlab("Sex") +
  ylab("Sociability score") +
  scale_fill_manual(name = "Treatment",
                    values = c("#d80033", "#3f5aa9"))  +
  geom_errorbar( aes(x=Sex, ymin=value-errorbars, ymax=value+errorbars), width=0.4,
                 colour="#000000", alpha=0.5, size=0.7, position=position_dodge(0.9)) +
  theme_bw() +
  theme(legend.position="right",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank() 
  ) 



# Boris analysis! 
# I only have data from day 1 
# Lunges 

lungedata <- read.csv("Lunges cvs.csv") # data framework

# Create averages
lungedataave <- data.frame(
  Sex=c("Male","Male", "Female", "Female") ,  
  
  Lineage = c("Sec5 knockdown", "Control","Sec5 knockdown", "Control"), 
  
  value = c((mean(as.numeric(lungedata[lungedata$Treatment == "Sec5" & lungedata$Sex == "M", "Total.lunges"]))), 
            (mean(as.numeric(lungedata[lungedata$Treatment == "Control" & lungedata$Sex == "M", "Total.lunges"]))),
            (mean(as.numeric(lungedata[lungedata$Treatment == "Sec5" & lungedata$Sex == "F", "Total.lunges"]))),
            (mean(as.numeric(lungedata[lungedata$Treatment == "Control" & lungedata$Sex == "F", "Total.lunges"]))) ), 
  
  errorbars = c((std(lungedata[lungedata$Treatment == "Sec5" & lungedata$Sex == "M", "Total.lunges"])), 
                (std(lungedata[lungedata$Treatment == "Control" & lungedata$Sex == "M", "Total.lunges"])),
                (std(lungedata[lungedata$Treatment == "Sec5" & lungedata$Sex == "F", "Total.lunges"])),
                (std(lungedata[lungedata$Treatment == "Control" & lungedata$Sex == "F", "Total.lunges"])) )
)



lungedataave %>%
  mutate(Sex = fct_relevel(Sex, 
                           "Female", "Male"),
         Lineage = fct_relevel(Lineage, 
                               "Control", "Sec5 knockdown")) %>%
  ggplot(aes(fill = Lineage , x=Sex, y=value)) + 
  geom_bar(position="dodge", stat = "identity") +
  ggtitle("Average Lunges as a Function of Lineage and Sex") +
  xlab("Sex") +
  ylab("Lunges") +
  scale_fill_manual(name = "Treatment",
                    values = c("#d80033", "#3f5aa9"))  +
  geom_errorbar( aes(x=Sex, ymin=value-errorbars, ymax=value+errorbars), width=0.4,
                 colour="#000000", alpha=0.5, size=0.7, position=position_dodge(0.9)) +
  theme_bw() +
  theme(legend.position="right",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank() 
  ) 


# Lunges by focal fly 

# Create averages
focallunges <- data.frame(
  Sex=c("Male","Male", "Female", "Female") ,  
  
  Lineage = c("Sec5 knockdown", "Control","Sec5 knockdown", "Control"), 
  
  value = c((mean(as.numeric(lungedata[lungedata$Treatment == "Sec5" & lungedata$Sex == "M", "Lunges.by.focal"]))), 
            (mean(as.numeric(lungedata[lungedata$Treatment == "Control" & lungedata$Sex == "M", "Lunges.by.focal"]))),
            (mean(as.numeric(lungedata[lungedata$Treatment == "Sec5" & lungedata$Sex == "F", "Lunges.by.focal"]))),
            (mean(as.numeric(lungedata[lungedata$Treatment == "Control" & lungedata$Sex == "F", "Lunges.by.focal"]))) ), 
  
  errorbars = c((std(lungedata[lungedata$Treatment == "Sec5" & lungedata$Sex == "M", "Lunges.by.focal"])), 
                (std(lungedata[lungedata$Treatment == "Control" & lungedata$Sex == "M", "Lunges.by.focal"])),
                (std(lungedata[lungedata$Treatment == "Sec5" & lungedata$Sex == "F", "Lunges.by.focal"])),
                (std(lungedata[lungedata$Treatment == "Control" & lungedata$Sex == "F", "Lunges.by.focal"])) )
)


focallunges %>%
  mutate(Sex = fct_relevel(Sex, 
                           "Female", "Male"),
         Lineage = fct_relevel(Lineage, 
                               "Control", "Sec5 knockdown")) %>%
  ggplot(aes(fill = Lineage , x=Sex, y=value)) + 
  geom_bar(position="dodge", stat = "identity") +
  ggtitle("Average Lunges by focal fly as a Function of Lineage and Sex") +
  xlab("Sex") +
  ylab("Focal Lunges") +
  scale_fill_manual(name = "Treatment",
                    values = c("#d80033", "#3f5aa9"))  +
  geom_errorbar( aes(x=Sex, ymin=value-errorbars, ymax=value+errorbars), width=0.4,
                 colour="#000000", alpha=0.5, size=0.7, position=position_dodge(0.9)) +
  theme_bw() +
  theme(legend.position="right",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank() 
  ) 

# Lunges at focal fly 

# Create averages
stimlunges <- data.frame(
  Sex=c("Male","Male", "Female", "Female") ,  
  
  Lineage = c("Sec5 knockdown", "Control","Sec5 knockdown", "Control"), 
  
  value = c((mean(as.numeric(lungedata[lungedata$Treatment == "Sec5" & lungedata$Sex == "M", "Lunges.by.stimulus"]))), 
            (mean(as.numeric(lungedata[lungedata$Treatment == "Control" & lungedata$Sex == "M", "Lunges.by.stimulus"]))),
            (mean(as.numeric(lungedata[lungedata$Treatment == "Sec5" & lungedata$Sex == "F", "Lunges.by.stimulus"]))),
            (mean(as.numeric(lungedata[lungedata$Treatment == "Control" & lungedata$Sex == "F", "Lunges.by.stimulus"]))) ), 
  
  errorbars = c((std(lungedata[lungedata$Treatment == "Sec5" & lungedata$Sex == "M", "Lunges.by.stimulus"])), 
                (std(lungedata[lungedata$Treatment == "Control" & lungedata$Sex == "M", "Lunges.by.stimulus"])),
                (std(lungedata[lungedata$Treatment == "Sec5" & lungedata$Sex == "F", "Lunges.by.stimulus"])),
                (std(lungedata[lungedata$Treatment == "Control" & lungedata$Sex == "F", "Lunges.by.stimulus"])) )
)


stimlunges %>%
  mutate(Sex = fct_relevel(Sex, 
                           "Female", "Male"),
         Lineage = fct_relevel(Lineage, 
                               "Control", "Sec5 knockdown")) %>%
  ggplot(aes(fill = Lineage , x=Sex, y=value)) + 
  geom_bar(position="dodge", stat = "identity") +
  ggtitle("Average Lunges by Stimulus flies as a Function of Lineage and Sex") +
  xlab("Sex") +
  ylab("Stimulus Lunges at Focal") +
  scale_fill_manual(name = "Treatment",
                    values = c("#d80033", "#3f5aa9"))  +
  geom_errorbar( aes(x=Sex, ymin=value-errorbars, ymax=value+errorbars), width=0.4,
                 colour="#000000", alpha=0.5, size=0.7, position=position_dodge(0.9)) +
  theme_bw() +
  theme(legend.position="right",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank() 
  ) 



# Now to analyze how much time focal flies spend with stimulus flies 
# Have to split up female and male


# Female
femaletimedata <- read.csv("Female time.csv") # data framework
# I had to pre calculate the averages and stuff



femaletimedata %>%
  mutate(Lineage = fct_relevel(Lineage, 
                               "Control", "Knockdown")) %>%
  ggplot(aes(fill = Lineage, x = Number.of.Flies, y = Value)) + 
  geom_bar(position="dodge", stat = "identity") +
  ggtitle("Average Time Spent with 0, 1, 2 or 3 Other Flies - Female") +
  xlab("Number of stimulus flies") +
  ylab("Time spent (s)") +
  geom_errorbar( aes(x=Number.of.Flies, ymin=Value-std, ymax=Value+std), width=0.4,
                 colour="#000000", alpha=0.5, size=0.7, position=position_dodge(0.9)) +
  scale_fill_manual(name = "Treatment", 
                    labels = c("Control", "Sec5 knockdown"),
                    values = c("#d80033", "#3f5aa9"))  +
  theme_bw() +
  theme(legend.position="right",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank() 
  ) 





# Male
maletimedata <- read.csv("Male time.csv") # data framework
# I had to pre calculate the averages and stuff



maletimedata %>%
  mutate(Lineage = fct_relevel(Lineage, 
                                 "Control", "Knockdown")) %>%
  ggplot(aes(fill = Lineage, x = Number.of.Flies, y = Value)) + 
  geom_bar(position="dodge", stat = "identity") +
  ggtitle("Average Time Spent with 0, 1, 2 or 3 Other Flies - Male") +
  xlab("Number of stimulus flies") +
  ylab("Time spent (s)") +
  geom_errorbar( aes(x=Number.of.Flies, ymin=Value-std, ymax=Value+std), width=0.4,
                 colour="#000000", alpha=0.5, size=0.7, position=position_dodge(0.9)) +
  scale_fill_manual(name = "Treatment", 
                    labels = c("Control", "Sec5 knockdown"),
                    values = c("#d80033", "#3f5aa9"))  +
  theme_bw() +
  theme(legend.position="right",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank() 
  ) 



# Now to analyze/compare overall movements


movedata <- read.csv("Fly Movement count.csv") # data framework

# Create averages
movedataave <- data.frame(
  Sex=c("Male","Male", "Female", "Female") ,  
  
  Lineage = c("Sec5 knockdown", "Control","Sec5 knockdown", "Control"), 
  
  value = c((mean(as.numeric(movedata[movedata$Treatment == "Sec5" & movedata$Sex == "M", "Number.of.movements"]))), 
            (mean(as.numeric(movedata[movedata$Treatment == "Control" & movedata$Sex == "M", "Number.of.movements"]))),
            (mean(as.numeric(movedata[movedata$Treatment == "Sec5" & movedata$Sex == "F", "Number.of.movements"]))),
            (mean(as.numeric(movedata[movedata$Treatment == "Control" & movedata$Sex == "F", "Number.of.movements"]))) ), 
  
  errorbars = c((std(movedata[movedata$Treatment == "Sec5" & movedata$Sex == "M", "Number.of.movements"])), 
                (std(movedata[movedata$Treatment == "Control" & movedata$Sex == "M", "Number.of.movements"])),
                (std(movedata[movedata$Treatment == "Sec5" & movedata$Sex == "F", "Number.of.movements"])),
                (std(movedata[movedata$Treatment == "Control" & movedata$Sex == "F", "Number.of.movements"])) )
)



movedataave %>%
  mutate(Sex = fct_relevel(Sex, 
                           "Female", "Male"),
         Lineage = fct_relevel(Lineage, 
                               "Control", "Sec5 knockdown")) %>%
  ggplot(aes(fill = Lineage , x=Sex, y=value)) + 
  geom_bar(position="dodge", stat = "identity") +
  ggtitle("Average Number of Movements as a Function of Lineage and Sex") +
  xlab("Sex") +
  ylab("Movements") +
  scale_fill_manual(name = "Treatment",
                    values = c("#d80033", "#3f5aa9"))  +
  geom_errorbar( aes(x=Sex, ymin=value-errorbars, ymax=value+errorbars), width=0.4,
                 colour="#000000", alpha=0.5, size=0.7, position=position_dodge(0.9)) +
  theme_bw() +
  theme(legend.position="right",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank() 
  ) 

# Control vs knockdown to compare

movedatatogether <- data.frame(
  
  Lineage = c("Sec5 knockdown", "Control"), 
  
  value = c((mean(as.numeric(movedata[movedata$Treatment == "Sec5", "Number.of.movements"]))), 
            (mean(as.numeric(movedata[movedata$Treatment == "Control", "Number.of.movements"])))),
  
  errorbars = c((std(movedata[movedata$Treatment == "Sec5", "Number.of.movements"])), 
                (std(movedata[movedata$Treatment == "Control", "Number.of.movements"])))
                )


movedatatogether %>%
  mutate(Lineage = fct_relevel(Lineage, 
                               "Control", "Sec5 knockdown")) %>%
  ggplot(aes(fill = Lineage, x=Lineage, y=value)) + 
  geom_bar(position="dodge", stat = "identity") +
  ggtitle("Average Number of Movements as a Function of Treatment") +
  xlab("Treatment") +
  ylab("Movements") +
  scale_fill_manual(values = c("#d80033", "#3f5aa9"))  +
  geom_errorbar( aes(x=Lineage, ymin=value-errorbars, ymax=value+errorbars), width=0.4,
                 colour="#000000", alpha=0.5, size=0.7, position=position_dodge(0.9)) +
  theme_bw() +
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank() 
  ) 

              
