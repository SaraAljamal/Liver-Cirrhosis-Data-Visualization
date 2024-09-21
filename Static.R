library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(data.table)
library(gridExtra)
library(corrplot)
library() 
#--------------------------------------------------------------------------------
#--------------------------------Importing data----------------------------------

df <- read.csv("C:/Users/Sara Aljamal/Desktop/HTU/Third year/Second semester/Data visualization/Assignment/liver_cirrhosis_preprocessed.csv")
df_encoded<- read.csv("C:/Users/Sara Aljamal/Desktop/HTU/Third year/Second semester/Data visualization/Assignment/liver_cirrhosis_encoded.csv")
#--------------------------------------------------------------------------------
#------------------------Sex, Status, and Stage bar plots------------------------
plot1 <- ggplot(df, aes(x = Sex)) +
  geom_bar(fill = '#41E685') +
  labs(title = "Sex frequency", x = "Sex", y = "Count") +
  theme_classic()

plot2 <- ggplot(df, aes(x = Status)) +
  geom_bar(fill = '#5688AD') +
  labs(title = "Status frequency", x = "Status", y = "Count")+
  theme_classic()

plot3 <- ggplot(df, aes(x = Stage)) +
  geom_bar(fill = '#8F56AD') +
  labs(title = "Stage frequency", x = "Stage", y = "Count")+
  theme_classic()

grid.arrange(plot1, plot2, plot3, nrow = 1)

#--------------------------------------------------------------------------------
#------------------------Drug, with status and stage-----------------------------
custom_palette <- c("1" = "#41E685", "2" = "#5688AD", "3" = "#8F56AD")
custom_palette_S <- c("C" = "#41E685", "CL" = "#5688AD", "D" = "#8F56AD")

plot1 <- ggplot(df, aes(x =Drug)) +
  geom_bar(fill = '#41E685') +
  labs(title = "Drug frequency", x = "Drug", y = "Count") +
  theme_classic()

plot2 <- ggplot(df, aes(x = Drug, fill = factor(Status))) +
  geom_bar(position = 'dodge') +
  scale_fill_manual(values = custom_palette_S) +
  labs(title = "Drug and status", x = "Drug", y = "Count")+
  theme_classic()

plot3 <- ggplot(df, aes(x = Drug, fill = factor(Stage))) +
  geom_bar(position = 'dodge') +
  scale_fill_manual(values = custom_palette) +
  labs(title = "Drug and stage", x = "Drug", y = "Count")+
  theme_classic()

grid.arrange(plot1, plot2, plot3, nrow = 1)

#--------------------------------------------------------------------------------
#----------------------------Correlation heatmap---------------------------------

corr_matrix <- cor(df_encoded)
corr_melt <- melt(corr_matrix)
head(corr_melt)

ggplot(data = corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#8F56AD", high = "#5688AD", mid = "#F0F0F7", 
                       midpoint = 0, limit = c(-1, 1), 
                       name="Correlation") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  labs(title = "Correlation Heatmap", fill ="Correlation")+
  coord_fixed()

#--------------------------------------------------------------------------------
#----------------------------Status and stage------------------------------------

custom_palette_S <- c("C" = "#41E685", "CL" = "#5688AD", "D" = "#8F56AD")
plot <- ggplot(df, aes(x = Stage, fill = factor(Status))) +
  geom_bar(position = 'dodge') +
  scale_fill_manual(values = custom_palette_S) +
  labs(title = "Stage and status", x = "Stage", y = "Count")+
  theme_bw()
plot

#--------------------------------------------------------------------------------
#----------------------------Ascites and Stage---------------------------------

library(reshape2)
library(dplyr)
custom_palette_A <- c("Y" = "#41E685", "N" = "#5688AD")
plot1<- ggplot(df, aes(x = Stage, fill = factor(Ascites))) +
  geom_bar(position = 'dodge') +
  scale_fill_manual(values = custom_palette_A) +
  labs(title = "Stage and Ascites frequency", x = "Stage", y = "Count")+
  theme_bw()

ct <- table(df$Ascites, df$Stage)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Ascites", "Stage", "Frequency")
plot2<- ggplot(ct_df, aes(x = Stage, y = Ascites, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Ascites and Stage",
       x = "Stage", y = "Ascites") +
  theme_classic()

grid.arrange(plot1, plot2, nrow = 1)

#--------------------------------------------------------------------------------
#----------------------------Stage and Hepatomegaly------------------------------


custom_palette_A <- c("Y" = "#41E685", "N" = "#5688AD")
plot1<- ggplot(df, aes(x = Stage, fill = factor(Hepatomegaly))) +
  geom_bar(position = 'dodge') +
  scale_fill_manual(values = custom_palette_A) +
  labs(title = "Stage and Hepatomegaly", x = "Stage", y = "Count")+
  theme_bw()

ct <- table(df$Hepatomegaly, df$Stage)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Hepatomegaly", "Stage", "Frequency")
plot2<- ggplot(ct_df, aes(x = Stage, y = Hepatomegaly, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Hepatomegaly and Stage",
       x = "Stage", y = "Hepatomegaly") +
  theme_classic()

grid.arrange(plot1, plot2, nrow = 1)

#--------------------------------------------------------------------------------
#----------------------------Stage and spiders-----------------------------------

custom_palette_A <- c("Y" = "#41E685", "N" = "#5688AD")
plot1<- ggplot(df, aes(x = Stage, fill = factor(Spiders))) +
  geom_bar(position = 'dodge') +
  scale_fill_manual(values = custom_palette_A) +
  labs(title = "Stage and Spiders", x = "Stage", y = "Count")+
  theme_bw()

ct <- table(df$Spiders, df$Stage)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Spiders", "Stage", "Frequency")
plot2<- ggplot(ct_df, aes(x = Stage, y = Spiders, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Spiders and Stage",
       x = "Stage", y = "Spiders") +
  theme_classic()
grid.arrange(plot1, plot2, nrow = 1)

#--------------------------------------------------------------------------------
#----------------------------Stage and Edema-------------------------------------

custom_palette_A <- c("Y" = "#41E685", "N" = "#5688AD", "S"= "#8F56AD")
plot1<- ggplot(df, aes(x = Stage, fill = factor(Edema))) +
  geom_bar(position = 'dodge') +
  scale_fill_manual(values = custom_palette_A) +
  labs(title = "Stage and Edema", x = "Stage", y = "Count")+
  theme_bw()

ct <- table(df$Edema, df$Stage)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Edema", "Stage", "Frequency")
plot2<- ggplot(ct_df, aes(x = Stage, y = Edema, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Edema and Stage",
       x = "Stage", y = "Edema") +
  theme_classic()

grid.arrange(plot1, plot2, nrow = 1)

#--------------------------------------------------------------------------------
#---------------------Ascites and other categorical columns----------------------

ct <- table(df$Hepatomegaly, df$Ascites)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Hepatomegaly", "Ascites", "Frequency")
plot1<- ggplot(ct_df, aes(x = Ascites, y = Hepatomegaly, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Ascites and Hepatomegaly",
       x = "Ascites", y = "Hepatomegaly") +
  theme_classic()

ct <- table(df$Spiders, df$Ascites)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Spiders", "Ascites", "Frequency")
plot2<- ggplot(ct_df, aes(x = Ascites, y = Spiders, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Ascites and Spiders",
       x = "Ascites", y = "Spiders") +
  theme_classic()

ct <- table(df$Edema, df$Ascites)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Edema", "Ascites", "Frequency")
plot3<- ggplot(ct_df, aes(x = Ascites, y = Edema, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Ascites and Edema",
       x = "Ascites", y = "Edema") +
  theme_classic()

grid.arrange(plot1, plot2, plot3, nrow = 1)

#--------------------------------------------------------------------------------
#---------------------Hepatomegal and other categorical columns------------------


ct <- table(df$Ascites, df$Hepatomegaly)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Ascites", "Hepatomegaly", "Frequency")
plot1<- ggplot(ct_df, aes(x = Hepatomegaly, y = Ascites, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Hepatomegaly and Ascites",
       x = "Hepatomegaly", y = "Ascites") +
  theme_classic()

ct <- table(df$Spiders, df$Hepatomegaly)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Spiders", "Hepatomegaly", "Frequency")
plot2<- ggplot(ct_df, aes(x = Hepatomegaly, y = Spiders, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Hepatomegaly and Spiders",
       x = "Hepatomegaly", y = "Spiders") +
  theme_classic()

ct <- table(df$Edema, df$Hepatomegaly)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Edema", "Hepatomegaly", "Frequency")
plot3<- ggplot(ct_df, aes(x = Hepatomegaly, y = Edema, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Hepatomegaly and Edema",
       x = "Hepatomegaly", y = "Edema") +
  theme_classic()

grid.arrange(plot1, plot2, plot3, nrow = 1)

#--------------------------------------------------------------------------------
#---------------------Spiders and other categorical columns----------------------

ct <- table(df$Ascites, df$Spiders)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Ascites", "Spiders", "Frequency")
plot1<- ggplot(ct_df, aes(x = Spiders, y = Ascites, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Spiders and Ascites",
       x = "Spiders", y = "Ascites") +
  theme_classic()

ct <- table(df$Hepatomegaly, df$Spiders)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Spiders", "Hepatomegaly", "Frequency")
plot2<- ggplot(ct_df, aes(x = Spiders, y = Hepatomegaly, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Spiders and Hepatomegaly",
       x = "Spiders", y = "Hepatomegaly") +
  theme_classic()

ct <- table(df$Edema, df$Spiders)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Edema", "Spiders", "Frequency")
plot3<- ggplot(ct_df, aes(x = Spiders, y = Edema, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Spiders and Edema",
       x = "Spiders", y = "Edema") +
  theme_classic()

grid.arrange(plot1, plot2, plot3, nrow = 1)

#--------------------------------------------------------------------------------
#---------------------Edema and other categorical columns------------------------

ct <- table(df$Ascites, df$Edema)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Ascites", "Edema", "Frequency")
plot1<- ggplot(ct_df, aes(x = Edema, y = Ascites, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Edema and Ascites",
       x = "Edema", y = "Ascites") +
  theme_classic()

ct <- table(df$Hepatomegaly, df$Edema)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Hepatomegaly", "Edema", "Frequency")
plot2<- ggplot(ct_df, aes(x = Edema, y = Hepatomegaly, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Edema and Hepatomegaly",
       x = "Edema", y = "Hepatomegaly") +
  theme_classic()

ct <- table(df$Spiders, df$Edema)
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Spiders", "Edema", "Frequency")
plot3<- ggplot(ct_df, aes(x = Edema, y =Spiders, fill=Frequency)) +
  geom_tile(color = "#DDD9DE",alpha = 0.8) +
  scale_fill_gradient(low = "#DDD9DE", high = "#5688AD") +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Relationship between Edema and Spiders",
       x = "Edema", y = "Spiders") +
  theme_classic()

grid.arrange(plot1, plot2, plot3, nrow = 1)

#--------------------------------------------------------------------------------
#----------------Scatter plot for the Prothrombin and Platelets------------------

plot<- ggplot(df, aes(x = Prothrombin, y = Platelets)) +
  geom_point(color= "#41E685") +
  labs(title = "Scatter Plot of Prothrombin and Platelets",
       x = "Prothrombin",
       y = "Platelets")
plot

#--------------------------------------------------------------------------------
#----------------Scatter plot for the Bilirubin and Copper-----------------------

plot<- ggplot(df, aes(x = Bilirubin, y = Copper)) +
  geom_point(color= "#5688AD") +
  ylim(0, 300)+
  xlim(0, 10)+
  labs(title = "Scatter Plot of Bilirubin and Copper",
       x = "Bilirubin",
       y = "Copper")
plot


