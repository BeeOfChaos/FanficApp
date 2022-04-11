library(stringr)

source("pdf.R")
source("prepare_data.R")



IA  <-  TRUE
#   User Interaction
while(IA) {
    readFiles <- readline(prompt = "Read files?\n1: No\n2: Yes - New Files\n3: Yes - All Files\n4: Stop Programm (stop Program with ctrl+C if this doesn't work)\n")
         if (readFiles == "1")   IA     <-  if (file.exists("fic_data.csv")) {
                                                FALSE 
                                            } else {
                                                cat("\n")
                                                print("You don't have any previously read-in fics. Please choose option 3 or stop the program")
                                                cat("\n")
                                                TRUE
                                            }
    else if (readFiles == "2")   IA     <-  readNewPdfs()
    else if (readFiles == "3")   IA     <-  readAllPdfs()
    else if (readFiles == "4")   stop("\nProgram stopped. Hope you have a good day!")
    else if (readFiles == "5")   IA     <-  is.null(prepare_data(read.csv("fic_data.csv")))
}





#   prepare data for app.R

complete_Table_all_data <-  read.csv("fic_data.csv")       #   needed to remove time-column
complete_Table          <-  complete_Table_all_data[c("title", "rating", "fandom", "relationship", "persons", "tags", "author", "words", "series", "category", "fin", "summary")]

tag_col		            <-	which( colnames(complete_Table)=="tags" )    +1   #   needed in app.R/+1 for nesting
word_col	            <-	which( colnames(complete_Table)=="words" )   +1   #   needed in app.R/+1 for nesting
fand_col	            <-	which( colnames(complete_Table)=="fandom" )  +1   #   needed in app.R/+1 for nesting
pers_col	            <-	which( colnames(complete_Table)=="persons" ) +1   #   needed in app.R/+1 for nesting



