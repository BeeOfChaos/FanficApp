# FanficApp


HOW TO RUN:


1.  install R (and possibly RStudio, though that's not necessary) from here: https://cran.r-project.org/bin/
    (I only tried this on a Linux system and Windows, so I can only give instructions for those)


2.  download packages:
        shiny
        stringr
        htmltools
        DT
        pdftools

    to do that you'll have to open R by
        Linux:      in your terminal type in:   R
        Windows:    open your R App (should be R x64 4.1.3 or R x32 4.1.3 or w/e version of R you use)    
    and type in:   install.packages("PACKAGENAME")  
        , then follow the instructions. If you are not an admin you'll be asked if a different library is ok. Yes, yes it is.


3.  choose a directory for your fics 
    unzip the skript into this directory
    you should have two directories in your chosen directory now:   src (full of files)     and     fics (empty)


4.  move or copy all your fic pdfs into the fics directory
    ATTENTION: this skript will only work with AO3 fics. Any other pdfs will be ignored


5.  start the skript by
    Linux:      open a terminal and traverse into your fic-directory
                to start R type into your terminal:     R
                type in:    source("src/main.R")

    Windows:    open your R App (should be R x64 4.1.3 or R x32 4.1.3 or w/e version of R you use)
                type in:    setwd("/PATH/TO/DIRECTORY")         (if you copy it directly from the filemanager you have to revert the slashes)
                                eg.: C:\Users\USERNAME\myDir    becomes /Users/USERNAME/myDir
                                you can check by typing in:     getwd()     when it says "/Users/USERNAME/myDir" it worked
                type in:    source("src/main.R")


6.  if everything went as it should you will now see a menu asking you wheather to read files. Congratulations!


7.  choose
    1)  if you have already used this before and just want to find one of your read-in fics
    2)  if you added new fics and want to read in only those new fics
    3)  if you are here for the first time or accidentally removed your fic_data.csv file
    4)  if you remembered that you don't actually have the time to read.


8.  choose your fic.
    1)  use the Searchpanes (big filters) to search for tags, persons, fandoms or filter by word-count
        These are NOT exclusive filters. 
        If you choose "Fluff" and "Angst" you will get all fics tagged EITHER "Fluff" OR "Angst"

    2)  if you want to search for "Fluff" AND "Angst" you can either use both the Searchpanes for the first and the small filter in the table for the second tag/person/ w/e
        OR you use the Custom Search Builder directly above the table. (You need to ENTER your choice for the Searchbuilder) (this one also allowes you to filter things OUT)

    3)  AO3 Archive Warnings are added automatically as tags: AW: Non-Con, AW: Graphic Violence, AW: Author choose not to, AW: Major Chara Death

    4)  click on the circle at the beginning of the row to show the summary of the fic


9.  Close the app by clicking on the "Quit" button at the top of the page or press [c] + [ctrl] in the terminal.


10. You will find your NOT read-in fics: 
    In your main directory is now a file named failed_fics.csv that you can open with any table-programm (e.g. Excel). 
    It will be generated every time you read new fics in, so no worries if you delete it after taking care of those pdfs
    File is empty? Congratulations, everything has been read in!


11. You will find any fics with same title and author:
    In your main directory is a file named doubles.csv that you can open with any table-programm (e.g. Excel). 
    It will be generated every time you read new fics in, so no worries if you delete it after taking care of those pdfs
    File is empty? Congratulations, everything has been read in!
    (All fics in the doubles.csv have been read-in and added to your fic_data.csv)

