library(pdftools)
library(stringr)

source("prepare_data.R")



###     read all files that have been made/copied after the fic_data.csv file has last been created  =^= switch/case 2
readNewPdfs <- function() {
    if (fileExists())       old_df  <-  read.csv("fic_data.csv")        else     return(readAllPdfs())     
    file_vector                     <-  getFiles()
    if (is.null(file_vector))           return(stopOnNull("", NULL))        #   stop if no pdfs are found                                                                         
    fic_df                          <-  readPdfs("start reading new files ...", max(old_df$time), file_vector)
    if (stopOnNull("NEW ", fic_df))     return(TRUE)                        #   stop if no readable new pdfs are found

    write.csv(rbind(old_df[, c(colnames(old_df) %in% colnames(fic_df))], fic_df), "fic_data.csv")       ### backup all read-in fic_data/add to old data 
    print("finished reading new files")

    fic_df                          <-  prepare_data(fic_df)
    fic_df                          <-  rbind(old_df[, c(colnames(old_df) %in% colnames(fic_df))], fic_df)
    write.csv(fic_df, "fic_data.csv")
    
    writeDoubles(fic_df)
    return(FALSE)
}




#   read all files  =^= switch/case 3
readAllPdfs <- function() {
    file_vector                     <-  getFiles()
    if (is.null(file_vector))           return(stopOnNull("", NULL))        #   stop if no pdfs are found
    fic_df                          <-  readPdfs("start reading all files ...", as.POSIXct("2000-01-01 00:00:00 UTC"), file_vector)
    if (stopOnNull("", fic_df))         return(TRUE)                        #   stop if no readable pdfs are found

    write.csv(fic_df, "fic_data.csv")       ### backup all read-in fic_data
    print("finished reading all files")

    fic_df                          <-  prepare_data(fic_df)
    write.csv(fic_df, "fic_data.csv")

    writeDoubles(fic_df)
    return(FALSE)
}



###     read all files into one vector and check if there are any pdfs in fics-directory at all
getFiles    <-  function() {
    #   make vector of every file in directory above
    file_vector                     <-  list.files(path="../fics/")
    file_vector                     <-  file_vector[grepl(".pdf",file_vector)]
    if (length(file_vector) == 0)       return(NULL)
    file_vector                     <-  paste("../fics/", file_vector, sep = "")
    return(file_vector)
}



###     function that actually reads the pdfs
readPdfs <- function(startLine, time, file_vector) {
    print(startLine)


    #   for pdfs that arent properly formatted (don't have a title)
    failed_df   <-  data.frame(file_number = c(NA), file_name = c(NA))
    #   for properly read-in pdfs
    fic_df      <-  data.frame(file_name = c(NA), title = c(NA), author = c(NA), rating = c(NA), category = c(NA), fandom = c(NA), relationship = c(NA), persons = c(NA), series = c(NA), words = c(NA), tags = c(NA), summary = c(NA), fin = c(NA), time = c(NA))


    #   for all given fics
    for (file in 1:length(file_vector)) {
        if(file.info(file_vector[file])$ctime < time ) next     #   check whether the fic is supposed to be read (new/all)         
        print(file_vector[file]) 

        #   check if the pdf has a title -> if not probably not an AO3 fic and may crash program, therefore write to failed data and skip to the next fic in the directory
        fic_df[file, c("title")]    <-  if (length(pdf_info(file_vector[file])$keys$Title) != 0)     pdf_info(file_vector[file])$keys$Title     else    ""
        if (nchar(fic_df[file, c("title")]) == 0) {
            failed_df   <-  rbind(failed_df, c(file, file_vector[file]))
            next
        }


        #   add standard data to allow filtering for "no series/ no relationship", as well as name of fic-file to better find it later
        #   tags/fandom/persons needed so the filtering doesn't crash because of fics without tags/fandoms/persons
        fic_df[file, c("series")]                                       <-  "---"
        fic_df[file, c("file_name")]                                    <-  file_vector[file]
        fic_df[file, c("tags", "fandom", "persons", "relationship")]    <-  " "         
        fic_df[file, c("time")]                                         <-  Sys.time()


        #   get data of first page
        file_text       <-  pdf_text(file_vector[file])
        first_page      <-  file_text[1]
        head_text       <-  str_trim(unlist(strsplit(first_page, "\n")))
        

        #   go through full first page line by line)
        for (line in 1:length(head_text)) {
            work_line   <-  ""
            if (grepl("^Notes", head_text[line]))                   break           #   maybe TEMP! check whether this works or needs to be reworked        # && (!is.na(fic_df[file, c("tags")]) || (!grepl(fic_df[file, c("tags")], "non-con"))) )      break          ### Does this work? //breaks without "break" in summary on merging
            else if (head_text[line] == "")                         next
            else if (grepl("^Rating:", head_text[line]) )           fic_df[file, c("rating")]       <- str_trim(substr(head_text[line], 8, nchar(head_text[line])))               
            else if (grepl("^Category:", head_text[line]) )         fic_df[file, c("category")]     <- str_trim(substr(head_text[line], 10, nchar(head_text[line])))
            else if (grepl("^by( )[0-9a-zA-Z]", head_text[line]))   fic_df[file, c("author")]       <- str_trim(substr(head_text[line], 3, nchar(head_text[line])))
            else if (grepl("^Summary", head_text[line]))            fic_df[file, c("summary")]      <- getSummary(head_text, line)
            else if (grepl("^Archive Warning:", head_text[line]))  {
                if (grepl("Rape", head_text[line]))                         fic_df[file, c("tags")]     <-  if (fic_df[file, c("tags")] == " ")  "AW: Non-Con"               else    paste(fic_df[file, c("tags")], "AW: Non-Con", sep = ",")            ### if empty replace with warning else add warning
                if (grepl("Violence", head_text[line]))                     fic_df[file, c("tags")]     <-  if (fic_df[file, c("tags")] == " ")  "AW: Graphic Violence"      else    paste(fic_df[file, c("tags")], "AW: Graphic Violence", sep = ",")
                if (grepl("Choose", head_text[line]))                       fic_df[file, c("tags")]     <-  if (fic_df[file, c("tags")] == " ")  "AW: Author choose not to"  else    paste(fic_df[file, c("tags")], "AW: Author choose not to", sep = ",")
                if (grepl("Death", head_text[line]))                        fic_df[file, c("tags")]     <-  if (fic_df[file, c("tags")] == " ")  "AW: Major Chara Death"     else    paste(fic_df[file, c("tags")], "AW: Major Chara Death", sep = ",")
            }         
            
            #   tackled differently because very likely to be more than one line
            else if (grepl("^Fandom:|^Relationship:|^Character:|^Additional Tags|^Series:|^Stats:", head_text[line])) {     
                work_line   <-  getFullLine(head_text, line)
                if (grepl("^Fandom:", work_line))                   fic_df[file, c("fandom")]       <- str_trim(substr(work_line, 8, nchar(work_line)))
                else if (grepl("^Relationship:", work_line))        fic_df[file, c("relationship")] <- str_trim(substr(work_line, 14, nchar(work_line)))
                else if (grepl("^Character:", work_line))           fic_df[file, c("persons")]      <- str_trim(substr(work_line, 11, nchar(work_line))) 
                else if (grepl("^Additional Tags:", work_line))     fic_df[file, c("tags")]         <- if (fic_df[file, c("tags")] == " ")      str_trim(substr(work_line, 17, nchar(work_line)))   else    paste(fic_df[file, c("tags")], str_trim(substr(work_line, 17, nchar(work_line)))  , sep = ",")    
                else if (grepl("^Series:", work_line) )             fic_df[file, c("series")]       <- checkSeries(work_line)            
                else if (grepl("^Stats:", work_line)) {
                                                                    fic_df[file, c("words")]        <- str_trim(substr(work_line, unlist(gregexpr("Words:", work_line))+6, nchar(work_line)))      
                                                                    fic_df[file, c("fin")]          <- isFinished(work_line)             
                }

            } 
        }
    }
    #   check whether any new data has been read-in -> if not, return NULL to return to UserInteraction
    if ((nrow(fic_df) == 1) && (is.na(fic_df[1, c("title")]) ))     return(NULL)

    #   remove all failed file_data and write all failed file names to csv table
    fic_df  <-  subset(fic_df, nchar(title)>0)
    write.csv(failed_df[-c(1),], "../failed_fics.csv") 
    print("finished writing failed fics")

    return(fic_df)
}





###     extra functions     ###


###     check if fic is finished
isFinished      <-  function (sample_string) {
    if (grepl("Chapters:", sample_string)) {
        c       <-  unlist(gregexpr("Chapters:", sample_string))
        w       <-  unlist(gregexpr("Words:", sample_string))
        csplit  <-  strsplit(str_trim(substr(sample_string, c + 10, w-1)), "/")
        return(     unlist(csplit)[1] == unlist(csplit)[2]  )
    } else {
        return(TRUE)
    }
}


###     treated differently because commas in series names have to be removed so one series doesn't get accidentally read as two seperate series later
checkSeries     <-  function (sample_string) {
    series_vector               <- strsplit(substr(sample_string, 8, nchar(sample_string)), "Part [0-9]+ of ")
    series_vector               <- str_trim(unlist(series_vector)[c(2:length(unlist(series_vector)))])
    series_vector_fin           <- c()
    for (i in 1:length(series_vector)) {
        series_vector_fin           <- c(series_vector_fin, str_replace(series_vector[i], ",", "")) 
    }
    return(paste(str_trim(unlist(strsplit(series_vector_fin, ","))), collapse = ", "))
}


###     get summary -> likely to have more than one line
###     cannot be added to getFullLine because of different end condition
getSummary      <-  function (sample_string_vector, line) {
    sample_string <- sample_string_vector[line]
    while(line < length(sample_string_vector) & (!grepl("^Notes", sample_string_vector[line + 1])) ) {
        sample_string   <-  paste(sample_string, sample_string_vector[line + 1])
        line            <-  line + 1
    }
    return(str_trim(substr(sample_string, 8, nchar(sample_string))))
}


###     get work_line for relationship/tags etc where line is likely to be longer than one
getFullLine     <-  function (sample_string_vector, line) {
    sample_string   <- sample_string_vector[line]
    while((sample_string_vector[line + 1] != "") & (!grepl("^Category:|^Fandom:|^Relationship:|^Character:|^Additional Tags|^Series:|^Stats:|^Archive Warning:|^Collections:", sample_string_vector[line + 1])) ) {
        sample_string   <- paste(sample_string, sample_string_vector[line + 1], sep = "   ")     #   to allow checking whether the linebreak separated a name/relationship
        line            <- line + 1
    }
    #   correct sample_string if a name/relationship got separated by linebreak, s. last comment
    if (grepl("(-)+   |   (-)+", sample_string))        sample_string <- str_replace(sample_string, "(-)+   |   (-)+", "-")
    if (grepl("   ", sample_string))                    sample_string <- str_replace_all(sample_string, "   ", " ")
    return(sample_string)
}





###     find all fics where title and author are the same, then write them to csv-file "doubles.csv"
writeDoubles    <-  function(fic_df)    {
    doubles         <-  fic_df[duplicated(fic_df[, c("title", "author")]) | duplicated(fic_df[, c("title", "author")], fromLast=TRUE), ]
    write.csv(doubles[order(doubles$title),], "../doubles.csv")
    print("finished writing possible doubles")
}


###     return to User Interaction if no (new) fics were found
stopOnNull      <-  function(string, df) {
    if ((is.null(df)) || (nrow(df) == 0)) {
        print(paste("No ", string, "files could be found. Please check that you have added ", string, "pdf-files to the directory 'fics' and that you used pdf-files downloaded from AO3.", sep = ""))
        cat("\n")
        return(TRUE)
    } else {
        return(FALSE)
    }
}


###     check whether old fic-data exists when chosing option 2
fileExists      <-  function()  {
    if (file.exists("fic_data.csv")) {
        return(TRUE)
    } else {
        print("No old fic-data was found. Start reading in ALL fic files ...")
        cat("\n")
        return(FALSE)
    }
}

