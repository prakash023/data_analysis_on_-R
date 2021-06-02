
packages = c("ggpubr","tidyverse","readr",
             "ggplot2","datasets", "reshape2",
             "gridExtra", "sf", "tmap" , "sn")


package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
##Aufgabe 1#
#Loading the dataset and saving it as a .rds file with matrikelnummer
#-----------------------------------------------------------------------------------------> 

setwd("D:/Multivariate Datenanalyse/klausuruebung/Klausur-2PRZ") #setting the working folder

df <-  read.csv2("863643.csv", FALSE,  skip = 2, sep = ";", 
                  encoding = "UTF-8") #CSV data are loaded here
colnames(df) <-c("Kennziffer", "Raumeinheit", "Aggregat",
                 "Ausgaben für Sachinvestitionen", "Kinderarmut") #add columns names back

## problem: the kennziffer from shapefile have leading zeros while the dataframe does not
##to solve this we add leading zeros to the rows which have kennziffer<1000 (319 rows)

df$Kennziffer[0:318] <- paste0("0", df$Kennziffer[0:318])



#print(df)

#Renaming the columnnames so that the data remains simple to use it
colnames(df)[colnames(df) == "Ausgaben für Sachinvestitionen"] <- "A_Sachinvestitionen"
#colnames(df)[colnames(df) == "Kinderarmut"] --> leaving it with the same name
print(df)

#Finding the correlation  
##------------------------------------------------------------------------------------------>


## cleaning up the data. removing the german format and changing to international number format
## replace , with . and remove the . used for thousands place.


df$A_Sachinvestitionen <-gsub("\\.", "",df$A_Sachinvestitionen)                                             
df$A_Sachinvestitionen <- as.numeric(gsub(",",  ".",df$A_Sachinvestitionen))

## interpret value as numeric value 
df$Kinderarmut <-as.numeric(gsub(",",  ".",df$Kinderarmut))



### basic correlation between data

#cor(df$A_Sachinvestitionen, df$Kinderarmut)
#which is -0.7012575

#cor(new_df$Transfered_Sachinvestitionen, new_df$Kinderarmut)
#which is-0.7106327


## save the dataframe as a RDS file.

saveRDS(df, file = "s863643.rds")
#--------------------------------------------------------------------------------------------->

#Aufgabe 2
#Creating histogramm and Boxplots in ggplot2
#---------------------------------------------------------------------------------------------
#histogram

## we crate a histogram for each variable. We notice that the variables are linearly ordered
## i.e. values lie within a interval and do not rise exponentially from lower to higher or 
## vice-versa. Due to this, further transformation of these variables is not deemed necessary.
## We can however divide e.g. Investment by 100 to make it the same scale as child poverty,
## but since its already noticable in histogram (scale of 100, 200, etc.) we don't do that either.

ggplot(df, aes( x=A_Sachinvestitionen)) +
  geom_histogram(binwidth =  10, 
                 color= "salmon", 
                  fill= "Green",
                  #breaks =20,
                  #bins =  50,
                   )+
  #xlim(0, 1400) +
 
  xlab( "Expenditure on capital Investment in €") +
  ylab ("Frequency") +
  theme_bw()
#summary(df)


#Kinderarmut 
ggplot(df, aes( x=Kinderarmut, )) +
  geom_histogram(binwidth = 0.5,
                 color= "salmon", 
                 fill= "cyan")+
  xlab( "Child poverty in [] ") +
  ylab ("Density")


#summary(df$Kinderarmut)
#----------------------------------------------------------------->
#new_invest <- melt( df[, c( "A_Sachinvestitionen", "invest_categories")])
#print(new_invest)
#------------------------------------------------------------------------------------->

#Boxplot ggplot2

index <- df$A_Sachinvestitionen> median( df$A_Sachinvestitionen)

Child <- df$Kinderarmut> median( df$Kinderarmut)


### divide the investment amount into two categories to see if there is a significant
## difference in the trend of child poverty between these two clusters.

#creating a new column and naming it as k_armut

df$K_armut <- Child * 1
df$K_armut <- factor( df$K_armut, labels = c( "Low", "High")) #

print(df)


Poverty_line <- melt( df[, c( "Kinderarmut", "K_armut")])

#print(Poverty_line)

ggplot( Poverty_line, aes( variable, value)) + 
  geom_boxplot( colour = "Blue", fill = "Green", alpha = 0.9) + 
  
  facet_wrap( ~K_armut) +
  
  xlab("Child Poverty") + ylab("Frequency") + 
  theme_dark() +
  ggtitle("comparisions of higher and lower Poverty rate") +
  theme_bw()



summary(df$K_armut)

ggsave("comparisions_boxplots.png")
#summary(df)

#------------------------------------------------------------------------------------------>

#Aufgabe 3
#------------------------------------------------------------------------------------------->

#new_df<-filter(df,is.finite(Transfered_Sachinvestitionen))%>%   #new df is created and all the datas are transferred
#  filter(df, A_Sachinvestitionen<900)


##since we don't have NA values in data, we don't need to filter out these
## but there outliers and to remove them we set a limit of 900, this roots out one outlier.
## we build a subset of the data with above conditions met.

new_df <- filter(df, A_Sachinvestitionen<900)
print(new_df)


summary(new_df$A_Sachinvestitionen)
summary(new_df$Kinderarmut)

#-----------------------------------------------------------------------#
#Aufgabe 4
#scatterplot

### build a scatter plot to show the relatoinship between the two variables.
ggplot2::ggplot(new_df, aes(x= A_Sachinvestitionen, 
                        y= Kinderarmut))+
  geom_point(aes(x=A_Sachinvestitionen, 
                y =Kinderarmut))+
  stat_smooth(method = "lm", 
              col = "Red", 
              se = FALSE, size = 0.5,
              alpha = 0.5)+
  theme_bw()+labs(title = "Scatterplot diagramm", 
      subtitle = "Dependency of Child Poverty with Capital investment from the year 2017",
    # scale_x_continuous(breaks = seq(100, 900, breaks = 200)),
     main ="Scatterplot: Distribution of Capital investment",
     x = "Investment Amount in Euro", 
     y = "Child Poverty")  
  
 
##finding the possible varianz and covarianz values 
#var(df$Kinderarmut) 
#cov(df$Kinderarmut, df$A_Sachinvestitionen)

## save the plot as png data.
ggsave("Scatterplot_diagramm.png")

#----------------------------------------------------------------------------
#Aufgabe5
#---------------------------------------------------------------------------------->  
#Linear Regression

regressionmodel <- lm( Kinderarmut~A_Sachinvestitionen, data =new_df)
#Here Kinderarmut is used as a predictor and the Sachinvetition is used as a reponse factor
  

#Y = 22.9695 -0.0298X

#summary(regressionmodel)

#--------------------------------------------------------------->
#Aufg 6
# NUTS3_sf file   

## TODO: describe how you import the file, where you get it and the nature of the data.
        
file_sf <- sf:: st_read(
  "Kreisgrenzen_2017_mit_Einwohnerzahl.shp")[, 
    c( "RS",  "GEN", "geometry" )] 

view(file_sf)

shapefile <- merge( file_sf, new_df , by.x = "RS", by.y = "Kennziffer")

#------------------------------------------------------------------------------------>
#Aufgabe 7

tmap_style("beaver")
tmap::tmap_mode( "plot")  
tmap::tm_shape( shapefile) + tm_fill("A_Sachinvestitionen", alpha = 0.9) +         
  tm_borders(alpha = 0.1) +       
  tm_layout(main.title = "Expenditure on capital investment in € per capita ", 
            main.title.size = 1.2 ,                 
            legend.position = c("left", "top"), 
            legend.title.size = 0.85, frame.lwd = 5) +     
  tm_compass(type = "arrow", position = c("left", "bottom")) +         
  tm_scale_bar(breaks = c(0, 50, 100, 150, 200), text.size = 1)




tmap_style("beaver")
tmap::tmap_mode( "plot")  
tmap::tm_shape( shapefile) + tm_fill("Kinderarmut", alpha = 0.9) +         
  tm_borders(alpha = 0.1) +       
  tm_layout(main.title = "Child poverty in percentage", 
            main.title.size = 1.2 ,                 
            legend.position = c("left", "top"), 
            legend.title.size = 0.85, frame.lwd = 5) +     
  tm_compass(type = "arrow", position = c("left", "bottom")) +         
  tm_scale_bar(breaks = c(0, 50, 100, 150, 200), text.size = 1)

