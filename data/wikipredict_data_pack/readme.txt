====================================================================
Márton Mestyán, Taha Yasseri, János Kertész:
Longtime Prediction of Movie Box Office Success based on Wikipedia Activity Big Data
====================================================================
===================================================================
Supplementary Data pack for the article
===================================================================
====================================================================
This data pack contains the data used for our investigations.
See details and explanations in the article.
====================================================================
This pack contains the data we gathered for two samples:
     a.) our sample of 312 movies
     b.) the sample of Asur and Huberman (2010) consisting of 24
         movies (see article for citation)
====================================================================
The structure of the data in both samples is the following:
    1) An index file named after the sample, in which each row
       represents a movie. The contents are:
       i.) an arbitrary ID number for the movie
       ii.) the title of the movie
       iii.) the title of the corresponding page in Wikipedia
            (may be subject to change)
       iv.) the first weekend revenue in the U.S. (USD)
       v.) the number of theaters (first weekend, U.S.)
       vi.) the date of release
       vii.) the time of the inception of the corresponding page
       	     in Wikipedia (in days, movie time)
    2) A folder containing the values of Wikipedia-based predictor
       variables
       - each file corresponds to a movie, the name of the file is 
         the ID of the movie (see 1) i.) )
       - each file lists the values of different predictors as a
         function of time (first column)
       - WP-based predictors are: number of views, number of users,
         rigor, number of edits
====================================================================
Fields of the data files are tab-separated and rows are terminated 
in UNIX style ('\n')
====================================================================
