
library(stringr)



#   function to allow tweaking of character names
character_function <- function(character_vector) {
    for (row in 1:length(character_vector)) {
        if (!is.na(character_vector[row])) {
            temp <- c()
            x <- unlist(strsplit(character_vector[row], ","))
            for (chara in x) {
                newChara <- chara
                if (grepl("Orion Pax|Optimus", chara)) newChara                     <- "Optimus Prime"
                else if (grepl("Deadlock|Drift", chara)) newChara                   <- "Drift | Deadlock"

                else if (grepl("James Rhodes|Rhodey", chara)) newChara              <- "James 'Rhodey' Rhodes"
                else if (grepl(" loki|^loki", chara, ignore.case=TRUE)) newChara    <- paste(newChara, "Loki", sep = ",")
                ### SAMPLE_LINE  else if (grepl("NAME1|NAME2", chara)) newChara       <- "ACTUAL_NAME"

                if (grepl("Original|OMC|OFC|\\(OC\\)| OC^", chara)) newChara        <- "OC"
                if (grepl("\\([-0-9a-zA-Z ]+\\)", chara)) newChara                  <- str_trim(str_replace_all(newChara, "\\([-0-9a-zA-Z ]+\\)", ""))

                temp <- c(temp, newChara)
            } 
            #character_vector[row]     <-  if ((length(temp) == 1) && (temp[1] == " "))    " "     else    paste(unique(str_trim(unlist(strsplit(temp, ",")))), collapse=", ")
            character_vector[row]     <-  paste(unique(str_trim(unlist(strsplit(temp, ",")))), collapse=", ")
        }
    }
    print("finished converting persons")
    return(character_vector)
}


#   function to allow tweaking of relationship names
relationship_function <- function(relationship_vector) {
    for (row in 1:length(relationship_vector)) {
        temp <- c()
        x <- unlist(strsplit(relationship_vector[row], ","))
        for (relationship in x) {
            newRelationship <- relationship
            if (grepl("Tomki|frostpudding", relationship, ignore.case=TRUE)) newRelationship        <- "Tom Hiddleston / Loki"
            else if (grepl("frostiron|ironfrost", relationship, ignore.case=TRUE)) newRelationship  <- "Loki / Tony Stark"
            else if (grepl("Thorki", relationship, ignore.case=TRUE)) newRelationship               <- "Loki / Thor"
            else if (grepl("royed", relationship, ignore.case=TRUE)) newRelationship                <- "Edward Elric / Roy Mustang"
            ### SAMPLE_LINE else if (grepl("RELATIONSHIPNAME", relationship, ignore.case=TRUE)) newRelationship                <- "CHARA1 / CHARA2 / CHARA3"

            if (grepl("\\([-0-9a-zA-Z ]+\\)", relationship)) newRelationship    <- str_trim(str_replace_all(newRelationship, "\\([-0-9a-zA-Z ]+\\)", ""))
            if (grepl("( )*/( )*", relationship)) newRelationship               <- str_replace_all(newRelationship, "( )*/( )*", " / ")
            if (grepl("( )*&( )*", relationship)) newRelationship               <- str_replace_all(newRelationship, "( )*&( )*", " & ")
            temp <- c(temp, newRelationship)
        } 
        relationship_vector[row] <- paste(unique(str_trim(unlist(strsplit(temp, ",")))), collapse=", ")
    }
    print("finished converting relationships")
    return(relationship_vector)
}


#   function to allow tweaking of fandom names
fandom_function <- function(fandom_vector) {
    for (row in 1:length(fandom_vector[])) {
        temp <- c()
        x <- unlist(strsplit(fandom_vector[row], ","))
        for (fandom in x) {
            newFandom <- fandom
            if (grepl("Spiderman|Thor|Iron Man|Captain America|Marvel|Steven Strange|Dr. Strange|Marvel|Hiddleston|Loki|Avengers|Tony Stark|Frostiron|tomki|frostpudding|IronFrost|Ironman|Hulk|S.H.I.E.L.D|MCU|Tony - Fandom|Aven - Fandom|IronHammer|Journey into Mystery|spider-man", fandom, ignore.case=TRUE)) newFandom <- "Marvel Cinematic Universe"
            else if (grepl("Transformer", fandom, ignore.case=TRUE)) newFandom <- "Transformers - All Media Types"
            else if (grepl("Star Trek", fandom, ignore.case=TRUE)) newFandom <- "Star Trek - All Media Types"
            else if (grepl("Star Wars|clone war", fandom, ignore.case=TRUE)) newFandom <- "Star Wars"
            else if (grepl("Sherlock|Holmes", fandom, ignore.case=TRUE)) newFandom <- "Sherlock Holmes"
            else if (grepl("Harry Potter", fandom, ignore.case=TRUE)) newFandom <- "Harry Potter"
            else if (grepl("FMA|Fullmetal Alchemist", fandom, ignore.case=TRUE)) newFandom <- "FMA - All Media Types"
            else if (grepl("DCU|Justice league|Man of Steel|Batman|Superman|Smallville|DC Animated|Green Arrow|Wonder Woman|World's Finest|DC Comic|Injustice: Gods Among Us", fandom, ignore.case=TRUE)) newFandom <- "DCU - All Media Types"
            else if (grepl("supernatural", fandom, ignore.case=TRUE)) newFandom <- "Supernatural"
            else if (grepl("Skyfall|James Bond", fandom, ignore.case=TRUE)) newFandom <- "James Bond"
            else if (grepl("How To Train Your Dragon|HTTYD", fandom, ignore.case=TRUE)) newFandom <- "How To Train Your Dragon"
            else if (grepl("Riddick|Pitch Black", fandom, ignore.case=TRUE)) newFandom <- "Riddick"
            else if (grepl("RPF|Real Person", fandom, ignore.case=TRUE)) newFandom <- "RPF"
            else if (grepl("Norse", fandom, ignore.case=TRUE)) newFandom <- "Norse Mythology"
            else if (grepl("pacific rim", fandom, ignore.case=TRUE)) newFandom <- "Pacific Rim"  
            else if (grepl("x-men", fandom, ignore.case=TRUE)) newFandom <- "X-Men"    
            else if (grepl("hikaru no go", fandom, ignore.case=TRUE)) newFandom <- "Hikaru no Go" 
            else if (grepl("Venom \\([-0-9a-zA-Z ]+\\)", fandom, ignore.case=TRUE)) newFandom <- "Venom" 
            else if (grepl("Elementary (TV)", fandom, ignore.case=TRUE)) newFandom <- paste("Sherlock Holmes", newFandom, sep=",")
            else if (grepl("Fantastic Four", fandom, ignore.case=TRUE)) newFandom <- "Fantastic Four"
            else if (grepl("Beauty and the Beast", fandom, ignore.case=TRUE)) newFandom <- "Beauty and the Beast"
            ### SAMPLE_LINE else if (grepl("FANDOMNAME1|FANDOMNAME2", fandom, ignore.case=TRUE)) newFandom <- "ACTUAL_FANDOMNAME"     

            temp <- c(temp, newFandom)
        } 
        fandom_vector[row]     <-  if ((length(temp) == 1) && (temp[1] == " "))    " "     else    paste(unique(str_trim(unlist(strsplit(temp, ",")))), collapse=", ")
    }
    print("finished converting fandoms")
    return(fandom_vector)
}


#   function to allow tweaking of tags
tag_function <- function(tag_vector) {
    for (row in 1:length(tag_vector[])) {
        temp <- c()
        x <- unlist(strsplit(tag_vector[row], ","))
        for (tag in x) {
            newTag <- tag
            if (grepl("H/C", tag, ignore.case=TRUE)) newTag <- paste(tag, "hurt/comfort", sep = ",")
            else if (grepl("a/b|alpha|omega", tag, ignore.case=TRUE)) newTag <- paste(tag, "a/b/o", sep = ",")
            else if (grepl("dubcon", tag, ignore.case=TRUE)) newTag <- paste(tag, "dubious consent", sep = ",")
            else if (grepl("d/s|dom/sub|dom!|sub!", tag, ignore.case=TRUE)) newTag <- paste(tag, "bdsm", sep = ",")
            else if (grepl("^noncon|^rape|mindrape|mind rape|^non con|no consent", tag, ignore.case=TRUE)) newTag <- paste(tag, "non-con", sep = ",")
            else if ((grepl(" fluff|fluff |fluffy", tag, ignore.case=TRUE)) && (!grepl("no fluff", tag, ignore.case=TRUE))) newTag <- paste(tag, "fluff", sep = ",")
            ### SAMPLE-LINE else if (grepl("TAG1|TAG2|TAG3", tag, ignore.case=TRUE)) newTag <- paste(tag, "ADDED_TAG", sep = ",")
            
            if (grepl("au - |AU - |AU|au-|AU-|Alternate Universe | Alternate Universe|Alternate Universe-|-Alternate Universe", tag)) newTag <- paste(tag, "Alternate Universe", sep = ",")
            temp <- c(temp, newTag)
        } 
        tag_vector[row]     <-  if ((length(temp) == 1) && (temp[1] == " "))    " "     else    paste(unique(str_trim(unlist(strsplit(temp, ",")))), collapse=", ")
    }
    print("finished converting tags")
    return(tag_vector)
}



#   super function for preparing data
prepare_data <- function(fic_df) {
    cat("\n")
    print("start converting data ...")
    
    fic_df$fandom <- fandom_function(fic_df$fandom)    
    fic_df$persons <- character_function(fic_df$persons)
    fic_df$tags <- tag_function(fic_df$tags)
    fic_df$relationship <- relationship_function(fic_df$relationship)

    print("finished converting data")
    cat("\n")

    return(fic_df)
}
