# This is a trial to extract data from the SEAFOOD DATABASE directly from the web.
# You have to search for a nutrient / contaminant and DOWNLOAD DE WEBPAGE AS .HTML.
# You have to save the .html and the folder associated, all in the same folder.

# Steps:

#1) Set the working directory where the .html are

#2) Library

library(tidyverse) # To work with the data
library(dplyr) # To work with the data
library(rvest)# To extract data from the webs.

filenames = Sys.glob("*.html") # This creates a "character" with the name of all the files .html in the folder

counter = 0

complete_table = data.frame() # An empty dataframe to add all the tables


for (i in filenames){ # This is the loop to read all .html files, one by one
  
  tables <- read_html(i) # This object contains the complete table
  
  xpath = "/html/body/div[1]/div/section/section[1]/div[4]/table/tbody" # This is the shared way to the table
  
  nodes <- html_nodes(tables, xpath = xpath) # To extract the indicated part of the web, the table
  
  table_name = str_sub(i, end=-6) # The name of the table is going to be the name of the file without .html
  
  table = as.data.frame(html_table(nodes)) # The proper table
  
  names = c("Species", "Measured", "Unit_per_kilos", "Maximum_level", "Mean", "Min", "Max", "Median", "Analyses", "Below_LOQ")
  
  names(table) = names # To add the previous names to work with the data
  
  # Some data are "< 0.06" or similar. I propose to substract the < and left the number.
  
  table$Min = gsub("<","",table$Min) # To eliminate the < in the data
  table$Min = as.numeric(table$Min) # To transform into numeric
  table$Max = gsub("<","",table$Max)
  table$Max = as.numeric(table$Max)
  table$Mean = gsub("<","",table$Mean)
  table$Mean = as.numeric(table$Mean)
  table$Median = gsub("<","",table$Median)
  table$Median = as.numeric(table$Median)
  
  # In the original table, there are different measures for the same specie.
  # I propose to show a mean of "Mean", "Max", "Min" and "Median", and the sum of the number of Analyses and the number of samples below LOQ.
  # This mean is automatic, another option could be to weight regarding the number of Analyses.
  
  table <- table %>% group_by(Species) %>% mutate(Mean = mean(Mean)) # Mean of Means by specie
  table <- table %>% group_by(Species) %>% mutate(Max = mean(Max)) # Mean of Max by specie
  table <- table %>% group_by(Species) %>% mutate(Min = mean(Min)) # Mean of Mins by specie
  table <- table %>% group_by(Species) %>% mutate(Median = mean(Median)) # Mean of Medians by specie
  table <- table %>% group_by(Species) %>% mutate(Analyses = sum(Analyses)) # Sum of Analyses by specie
  table <- table %>% group_by(Species) %>% mutate(Below_LOQ = sum(Below_LOQ)) # Sum of Below_LOQ by specie
  
  table <- select(table, -c(Measured)) # To eliminate the colum of the date, otherwise cannot combine the values in a unique row
  
  table <- distinct(table) # To have the unique row
  
  file_name = paste(table_name,".txt") # To create the name of the future .txt file
  file_name = gsub(" ","",file_name) # To eliminate the white space in the name.
  
  # Next lines are to put new names to the columns, adding the name of the file (ej "Arsenic_Unit_per_kilos")
  
  names = c("Species", (paste(table_name,"Unit_per_kilos")), (paste(table_name,"Maximum_level")), (paste(table_name,"Mean")), (paste(table_name,"Min")), (paste(table_name,"Max")), (paste(table_name,"Median")), (paste(table_name,"Analyses")), (paste(table_name,"Below_LOQ")))
  names = gsub(" ","_",names)
  
  names(table) = names
  
  # This if is to create a table which merge the table of each .html file
  
  if (counter == 0){
    complete_table = table
  } else {
    complete_table = full_join(complete_table, table, by = "Species")
  }
  
  counter = counter + 1
  
  # This if is to save the complete table only if it´s really complete
  
  if (counter == length(filenames)){
    write.table(complete_table,"complete_table.txt", sep = "\t", row.names = FALSE)
    print("The extraction finish")
  }
  
  write.table(table, file_name, sep = "\t",row.names = FALSE) # To extract each one of the tables
}
  