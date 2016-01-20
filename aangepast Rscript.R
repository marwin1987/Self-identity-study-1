###Enige aanpassing nodig voor pdf te laten werken!!
### Specify author names for PDF generation later on
authorName <- "Marwin Snippe \\and Gjalt-Jorn Peters \\and Gerjo Kok";

### Load packages (install userfriendlyscience first yourself
### if they're not installed yet, using:
# install.packages('userfriendlyscience');
require('userfriendlyscience');
safeRequire('plyr');
safeRequire('psych');   
safeRequire('parallel'); ### For mclapply, which 'psych' needs but doesn't load
safeRequire('lavaan');
require(car)

### Target behaviors and countries to analyse
behaviors <- c("alcohol", "condoms", "exercise");
countries <- c("USA", "India");

### Set parameters for running the script

### Number of iterations to use when bootstrapping the confidence
### intervals for the factor analysis
n.iter <- 100;

### Method to use for the factor analyses, e.g. minres, ml, wls, etc (see ?fa)
faMethod <- 'minres';

### Set working paths - adjusts the paths depending
### on the PC (Marwin or GJ).
if (file.exists("B:/Shared/Shared folder of Marwin & GJ/Onderzoek drie gedragingen/Ruwe data in CVS")) {
  workingPath <- "B:/Shared/Shared folder of Marwin & GJ/Onderzoek drie gedragingen/Ruwe data in CVS";
  pdfLatexPath <- "B:/Apps/MiKTeX/miktex/bin";
  rnwPath <- "B:/Shared/Shared folder of Marwin & GJ/Onderzoek drie gedragingen/rnw";
  
} else {
  workingPath <- "C:/Users/iMac/Dropbox/Shared folder of Marwin & GJ/Onderzoek drie gedragingen/Ruwe data in CVS";
  rnwPath <- "C:/Users/iMac/Dropbox/Shared/Shared folder of Marwin & GJ/Onderzoek drie gedragingen/rnw"
}

### Setting up a working directionary
setwd(workingPath);

### Specify a vector with datafile names
datafileNames <- c('[raw]alcohol.csv',
                   '[raw]condomUse.csv',
                   '[raw]exercise.csv');

###########################################################################
###########################################################################
### Specify functions
###########################################################################
###########################################################################

rnwString.section <- function(rnwString, title, newpage=TRUE,
                              section="section") {
  if (newpage) {
    rnwString <- paste0(rnwString, '\n\\newpage');
  }
  return(paste0(rnwString, '\n\\', section, '{',
                sanitizeLatexString(title), '}\n'));
}

rnwString.subsection <- function(rnwString, title, newpage=FALSE) {
  return(rnwString.section(rnwString, title, newpage, section="subsection"));
}

rnwString.verbatim <- function(rnwString, text) {
  return(paste0(rnwString, "\n\\begin{verbatim}\n",
                text, "\n\\end{verbatim}\n"));
}

rnwString.add <- function(rnwString, text, sanitize=TRUE, landscape=FALSE) {
  if (sanitize) {
    text <- sanitizeLatexString(text);
  }
  if (landscape) {
    text <- paste0('\\begin{landscape}\n\\maxsizebox{277mm}{190mm}{\n',
                   text,
                   '}\n\\end{landscape}\n');
  }
  return(paste0(rnwString, "\n", text));
}

rnwString.chunk <- function(rnwString, chunk, label=NULL,
                            echo=FALSE, eval=TRUE, dev='pdf') {
  if (is.null(label)) {
    return(paste0(rnwString, "\n<<echo=", as.character(echo),
                  ", eval=", as.character(eval),
                  ", dev='", dev, "'>>=\n",
                  chunk,
                  "\n@\n"));
  } else {  
    return(paste0(rnwString, "\n<<", label,
                  ", echo=", as.character(echo),
                  ", eval=", as.character(eval),
                  ", dev='", dev, "'>>=\n",
                  chunk,
                  "\n@\n"));
  }
}

###########################################################################
###########################################################################
### Load data
###########################################################################
###########################################################################

### Create object to store datafiles
dat <- list();

### Load datafiles into objects
for (currentDataset in datafileNames) {
  ### Read datafile from disk
  dat[[currentDataset]] <-
    getData(file.path(workingPath, currentDataset), skip=1);
}

dat.raw <- dat;

### Delete last variable (Qualtrics apparently ends lines with a comma?)
dat <- lapply(dat, function(x) { return(x[, names(x) != 'X']);});

### Apply nrow to each list element to see number of rows in the dataframes
lapply(dat, nrow);

### Sum sample sizes to get total sample size
sum(unlist(lapply(dat, nrow)));

### Verify ranges for those variables with fixes answer options
### for each questionnaire (Qualtrics sometimes has odd minimum and
### maximum values)
lapply(dat.raw, function(x) { rws <- 16:(ncol(x)-13);
                              x <- massConvertToNumeric(x[, rws]);
                              lapply(x, range, na.rm=TRUE); });

###########################################################################
###########################################################################
### Rename variables to a consistent naming scheme
###########################################################################
###########################################################################

names(dat[[1]])[1] <- 'ResponseID ';
names(dat[[1]])[2] <- 'ResponseSet';
names(dat[[1]])[3] <- 'Name ';
names(dat[[1]])[4] <- 'ExternalDataReference  ';
names(dat[[1]])[5] <- 'EmailAddress  ';
names(dat[[1]])[6] <- 'IPAddress  ';
names(dat[[1]])[7] <- 'Status  ';
names(dat[[1]])[8] <- 'StartDate  ';
names(dat[[1]])[9] <- 'EndDate ';
names(dat[[1]])[10] <- 'Finished';
names(dat[[1]])[11] <- 'informedConsent';
names(dat[[1]])[12] <- 'age';
names(dat[[1]])[13] <- 'studyExit';
names(dat[[1]])[14] <- 'sex';
names(dat[[1]])[15] <- 'country';
names(dat[[1]])[16] <- 'selectionAlcohol';
names(dat[[1]])[17] <- 'negativeresponsSelectionAlcohol';
names(dat[[1]])[18] <- 'informationAlcohol';
names(dat[[1]])[19] <- 'Intention_intend';
names(dat[[1]])[20] <- 'Intention2willing';
names(dat[[1]])[21] <- 'Intention3expect';
names(dat[[1]])[22] <- 'curBeh';
names(dat[[1]])[23] <- 'Attitude_bad_good';
names(dat[[1]])[24] <- 'Attitude_unpleasant_pleasant';
names(dat[[1]])[25] <- 'Attitude_harmful_beneficial';
names(dat[[1]])[26] <- 'Attitude_boring_interesting';
names(dat[[1]])[27] <- 'Importancescale_unimportant_important';
names(dat[[1]])[28] <- 'Importancescale_notessential_essential';
names(dat[[1]])[29] <- 'Importancescale_notsignificant_significant';
names(dat[[1]])[30] <- 'information';
names(dat[[1]])[31] <- 'Injunctivenorm_importantpeople';
names(dat[[1]])[32] <- 'Injunctivenorm_mostpeopleapprove';
names(dat[[1]])[33] <- 'Descriptivenorm_closefriends';
names(dat[[1]])[34] <- 'Descriptivenorm_peoplelikeme';
names(dat[[1]])[35] <- 'Perceivedcontrol_forme';
names(dat[[1]])[36] <- 'Perceivedcontrol_reallywantto';
names(dat[[1]])[37] <- 'Perceivedcontrol_confident';
names(dat[[1]])[38] <- 'information';
names(dat[[1]])[39] <- 'Selfidentity_rarelythinkabout';
names(dat[[1]])[40] <- 'SelfidentityRestrainedAlcohol';
names(dat[[1]])[41] <- 'Selfidentity_kindofperson';
names(dat[[1]])[42] <- 'Selfidentity_restrainedalcoholimportant';
names(dat[[1]])[43] <- 'Selfidentity_doingbehaviorimportant';
names(dat[[1]])[44] <- 'Selfidentity_importantpart';
names(dat[[1]])[45] <- 'Selfidentity_seemyselfas';
names(dat[[1]])[46] <- 'Selfidentity_seemyselffollowingthebehaviorguideline';
names(dat[[1]])[47] <- 'Selfidentity_givingupalcoholdrinking';
names(dat[[1]])[48] <- 'Selfidentity_restrainedalcoholdrinkingmeansmore';
names(dat[[1]])[49] <- 'Selfidentity_wrongbehaviormeansmorethanjusttheact';
names(dat[[1]])[50] <- 'Selfidentity_behaviormeansmoretantheactself';
names(dat[[1]])[51] <- 'Selfidentity_concernedwithnotdoingthebehaviorenough';
names(dat[[1]])[52] <- 'Selfidentity_concernedwithdoingtherightbehavior';
names(dat[[1]])[53] <- 'Selfidentity_wouldfeelatalossgivingupwrongbehavior';
names(dat[[1]])[54] <- 'information';
names(dat[[1]])[55] <- 'Past_haveused';
names(dat[[1]])[56] <- 'Past_howoften';
names(dat[[1]])[57] <- 'VerificationWorkerID';
names(dat[[1]])[58] <- 'Endscreen';
names(dat[[1]])[59] <- 'Browser.Meta.Info.Browser ';
names(dat[[1]])[60] <- 'Browser.Meta.Info.Version ';
names(dat[[1]])[61] <- 'Browser.Meta.Info.Operating.System ';
names(dat[[1]])[62] <- 'Browser.Meta.Info.Screen.Resolution';
names(dat[[1]])[63] <- 'Browser.Meta.Info.Flash.Version ';
names(dat[[1]])[64] <- 'Browser.Meta.Info.Java.Support  ';
names(dat[[1]])[65] <- 'Browser.Meta.Info.User.Agent ';
names(dat[[1]])[66] <- 'LocationLatitude ';
names(dat[[1]])[67] <- 'LocationLongitude';
names(dat[[1]])[68] <- 'LocationAccuracy ';

names(dat[[2]])[1] <- 'ResponseID ';
names(dat[[2]])[2] <- 'ResponseSet';
names(dat[[2]])[3] <- 'Name ';
names(dat[[2]])[4] <- 'ExternalDataReference  ';
names(dat[[2]])[5] <- 'EmailAddress  ';
names(dat[[2]])[6] <- 'IPAddress  ';
names(dat[[2]])[7] <- 'Status  ';
names(dat[[2]])[8] <- 'StartDate  ';
names(dat[[2]])[9] <- 'EndDate ';
names(dat[[2]])[10] <- 'Finished';
names(dat[[2]])[11] <- 'informedConsent';
names(dat[[2]])[12] <- 'age';
names(dat[[2]])[13] <- 'studyExit';
names(dat[[2]])[14] <- 'sex';
names(dat[[2]])[15] <- 'country';
names(dat[[2]])[16] <- 'selectionCondom';
names(dat[[2]])[17] <- 'studyExit';
names(dat[[2]])[18] <- 'information';
names(dat[[2]])[19] <- 'Intention_intend';
names(dat[[2]])[20] <- 'Intention2willing';
names(dat[[2]])[21] <- 'Intention3expect';
names(dat[[2]])[22] <- 'curBeh';
names(dat[[2]])[23] <- 'Attitude_bad_good';
names(dat[[2]])[24] <- 'Attitude_unpleasant_pleasant';
names(dat[[2]])[25] <- 'Attitude_harmful_beneficial';
names(dat[[2]])[26] <- 'Attitude_boring_interesting';
names(dat[[2]])[27] <- 'Importancescale_unimportant_important';
names(dat[[2]])[28] <- 'Importancescale_notessential_essential';
names(dat[[2]])[29] <- 'Importancescale_notsignificant_significant';
names(dat[[2]])[30] <- 'information';
names(dat[[2]])[31] <- 'Injunctivenorm_importantpeople';
names(dat[[2]])[32] <- 'Injunctivenorm_mostpeopleapprove';
names(dat[[2]])[33] <- 'Descriptivenorm_closefriends';
names(dat[[2]])[34] <- 'Descriptivenorm_peoplelikeme';
names(dat[[2]])[35] <- 'Perceivedcontrol_forme';
names(dat[[2]])[36] <- 'Perceivedcontrol_reallywantto';
names(dat[[2]])[37] <- 'Perceivedcontrol_confident';
names(dat[[2]])[38] <- 'information';
names(dat[[2]])[39] <- 'Selfidentity_rarelythinkabout';
names(dat[[2]])[40] <- 'Selfidentity_kindofperson';
names(dat[[2]])[41] <- 'Selfidentity_importantpart';
names(dat[[2]])[42] <- 'Selfidentity_doingbehaviorimportant';
names(dat[[2]])[43] <- 'Selfidentity_seemyselfas';
names(dat[[2]])[44] <- 'Selfidentity_behaviormeansmoretantheactself';
names(dat[[2]])[45] <- 'Selfidentity_seemyselffollowingthebehaviorguideline';
names(dat[[2]])[46] <- 'Selfidentity_concernedwithnotdoingthebehaviorenough';
names(dat[[2]])[47] <- 'Selfidentity_concernedwithdoingtherightbehavior';
names(dat[[2]])[48] <- 'Selfidentity_wouldfeelatalossgivingupwrongbehavior';
names(dat[[2]])[49] <- 'Selfidentity_wrongbehaviormeansmorethanjusttheact';
names(dat[[2]])[50] <- 'information';
names(dat[[2]])[51] <- 'Past_haveused';
names(dat[[2]])[52] <- 'Past_howoften';
names(dat[[2]])[53] <- 'VerificationWorkerID';
names(dat[[2]])[54] <- 'Endscreen';
names(dat[[2]])[55] <- 'Browser.Meta.Info.Browser ';
names(dat[[2]])[56] <- 'Browser.Meta.Info.Version ';
names(dat[[2]])[57] <- 'Browser.Meta.Info.Operating.System ';
names(dat[[2]])[58] <- 'Browser.Meta.Info.Screen.Resolution';
names(dat[[2]])[59] <- 'Browser.Meta.Info.Flash.Version ';
names(dat[[2]])[60] <- 'Browser.Meta.Info.Java.Support  ';
names(dat[[2]])[61] <- 'Browser.Meta.Info.User.Agent ';
names(dat[[2]])[62] <- 'LocationLatitude ';
names(dat[[2]])[63] <- 'LocationLongitude';
names(dat[[2]])[64] <- 'LocationAccuracy ';

names(dat[[3]])[1] <- 'ResponseID ';
names(dat[[3]])[2] <- 'ResponseSet';
names(dat[[3]])[3] <- 'Name ';
names(dat[[3]])[4] <- 'ExternalDataReference  ';
names(dat[[3]])[5] <- 'EmailAddress  ';
names(dat[[3]])[6] <- 'IPAddress  ';
names(dat[[3]])[7] <- 'Status  ';
names(dat[[3]])[8] <- 'StartDate  ';
names(dat[[3]])[9] <- 'EndDate ';
names(dat[[3]])[10] <- 'Finished';
names(dat[[3]])[11] <- 'informedConsent';
names(dat[[3]])[12] <- 'age';
names(dat[[3]])[13] <- 'studyExit';
names(dat[[3]])[14] <- 'sex';
names(dat[[3]])[15] <- 'country';
names(dat[[3]])[16] <- 'information';
names(dat[[3]])[17] <- 'Intention_intend';
names(dat[[3]])[18] <- 'Intention2willing';
names(dat[[3]])[19] <- 'Intention3expect';
names(dat[[3]])[20] <- 'curBeh';
names(dat[[3]])[21] <- 'Attitude_bad_good';
names(dat[[3]])[22] <- 'Attitude_unpleasant_pleasant';
names(dat[[3]])[23] <- 'Attitude_harmful_beneficial';
names(dat[[3]])[24] <- 'Attitude_boring_interesting';
names(dat[[3]])[25] <- 'Importancescale_unimportant_important';
names(dat[[3]])[26] <- 'Importancescale_notessential_essential';
names(dat[[3]])[27] <- 'Importancescale_notsignificant_significant';
names(dat[[3]])[28] <- 'information';
names(dat[[3]])[29] <- 'Injunctivenorm_importantpeople';
names(dat[[3]])[30] <- 'Injunctivenorm_mostpeopleapprove';
names(dat[[3]])[31] <- 'Descriptivenorm_closefriends';
names(dat[[3]])[32] <- 'Descriptivenorm_peoplelikeme';
names(dat[[3]])[33] <- 'Perceivedcontrol_forme';
names(dat[[3]])[34] <- 'Perceivedcontrol_reallywantto';
names(dat[[3]])[35] <- 'Perceivedcontrol_confident';
names(dat[[3]])[36] <- 'information';
names(dat[[3]])[37] <- 'Selfidentity_rarelythinkabout';
names(dat[[3]])[38] <- 'Selfidentity_kindofperson';
names(dat[[3]])[39] <- 'Selfidentity_exercisingenoughimportant';
names(dat[[3]])[40] <- 'Selfidentity_notthinkingaboutexercisingregularly';
names(dat[[3]])[41] <- 'Selfidentity_seemyselfas';
names(dat[[3]])[42] <- 'Selfidentity_concernedwithnotdoingthebehaviorenough';
names(dat[[3]])[43] <- 'Selfidentity_doingbehaviorimportant';
names(dat[[3]])[44] <- 'Selfidentity_importantpart';
names(dat[[3]])[45] <- 'Selfidentity_seemyselffollowingthebehaviorguideline';
names(dat[[3]])[46] <- 'Selfidentity_wouldfeelatalossgivingupwrongbehavior';
names(dat[[3]])[47] <- 'Selfidentity_lossgivingupexercising';
names(dat[[3]])[48] <- 'Selfidentity_concernedwithdoingtherightbehavior';
names(dat[[3]])[49] <- 'Selfidentity_exercisingenoughmeansmore';
names(dat[[3]])[50] <- 'Selfidentity_wrongbehaviormeansmorethanjusttheact';
names(dat[[3]])[51] <- 'Selfidentity_behaviormeansmoretantheactself';
names(dat[[3]])[52] <- 'information';
names(dat[[3]])[53] <- 'Past_haveused';
names(dat[[3]])[54] <- 'Past_howoften';
names(dat[[3]])[55] <- 'VerificationWorkerID';
names(dat[[3]])[56] <- 'Endscreen';
names(dat[[3]])[57] <- 'Browser.Meta.Info.Browser ';
names(dat[[3]])[58] <- 'Browser.Meta.Info.Version ';
names(dat[[3]])[59] <- 'Browser.Meta.Info.Operating.System ';
names(dat[[3]])[60] <- 'Browser.Meta.Info.Screen.Resolution';
names(dat[[3]])[61] <- 'Browser.Meta.Info.Flash.Version ';
names(dat[[3]])[62] <- 'Browser.Meta.Info.Java.Support  ';
names(dat[[3]])[63] <- 'Browser.Meta.Info.User.Agent ';
names(dat[[3]])[64] <- 'LocationLatitude ';
names(dat[[3]])[65] <- 'LocationLongitude';
names(dat[[3]])[66] <- 'LocationAccuracy ';

###########################################################################
###########################################################################
### Recode variables
###########################################################################
###########################################################################

### First store original version
dat.prerecoding <- dat;




### dat[[1]] is alcohol, [[2]] is condoms, [[3]] is exercise

#######TOT HIER WERKT HET NOG ALLEMAAL!

# ### Then process recoding that applies to all dataframes USING CAR PACKAGE
 dat <- lapply(dat, function(x) {
    x$Intention_intend <- recode(x$Intention_intend, "20=1; 21=2; 22=3; 23=4; 24=5; 25=6; 26=7");
    x$Intention2willing <- recode(x$Intention2willing, "43=1; 44=2; 45=3; 46=4; 47=5; 27=6; 28=7"); 
    x$Intention3expect <- recode(x$Intention3expect, "14=1; 15=2; 16=3; 17=4; 18=5; 19=6; 20=7");
    x$curBeh <- recode(x$curBeh, "9=1; 10=2; 11=3; 12=4; 14=5; 15=6; 16=7");
    x$Injunctivenorm_importantpeople <- recode(x$Injunctivenorm_importantpeople, "40=1; 41=2; 42=3; 43=4; 44=5; 45=6; 46=7; 47=NA");
    x$Descriptivenorm_peoplelikeme <- recode(x$Descriptivenorm_peoplelikeme, "15=1; 16=2; 17=3; 18=4; 19=5; 20=6; 21=7; 22=NA");
    x$Perceivedcontrol_forme <- recode(x$Perceivedcontrol_forme, "22=1; 23=2; 24=3; 25=4; 26=5; 27=6; 28=7");
    x$Perceivedcontrol_reallywantto <- recode(x$Perceivedcontrol_reallywantto, "9=1; 18=2; 10=3; 11=4; 12=5; 13=6; 14=7");
    x$Past_howoften <- recode(x$Past_howoften, "28=1; 29=2; 30=3; 31=4; 32=5; 33=6; 34=7"); 
    x$Descriptivenorm_closefriends <- recode(x$Descriptivenorm_closefriends, "8=NA");
    x$Injunctivenorm_mostpeopleapprove <- recode(x$Injunctivenorm_mostpeopleapprove, "8=NA");

    ##replacing NA with mean (van de gehele variabele of alleen dat item??? - heb nu op item niveau)
    x$Injunctivenorm_importantpeople[which(is.na(x$Injunctivenorm_importantpeople))]<- mean(x$Injunctivenorm_importantpeople, na.rm = TRUE);
    x$Descriptivenorm_peoplelikeme[which(is.na(x$Descriptivenorm_peoplelikeme))] <- mean(x$Descriptivenorm_peoplelikeme, na.rm = TRUE);
    x$Descriptivenorm_closefriends[which(is.na(x$Descriptivenorm_closefriends))] <- mean(x$Descriptivenorm_closefriends, na.rm = TRUE);
    x$Injunctivenorm_mostpeopleapprove[which(is.na(x$Injunctivenorm_mostpeopleapprove))] <- mean(x$Injunctivenorm_mostpeopleapprove, na.rm = TRUE);
    
    return(x);
    });

### Alcohol only
 dat[[1]]$selectionAlcohol <-
   invertItem(dat[[1]]$selectionAlcohol - 9);
 dat[[1]]$SelfidentityRestrainedAlcohol <-
   invertItem(dat[[1]]$SelfidentityRestrainedAlcohol);
 dat[[1]]$Selfidentity_importantpart <-
   invertItem(dat[[1]]$Selfidentity_importantpart, c(1, 6));
 dat[[1]]$Selfidentity_givingupalcoholdrinking <-
   invertItem(dat[[1]]$Selfidentity_givingupalcoholdrinking, c(1, 6));
 dat[[1]]$Selfidentity_behaviormeansmoretantheactself <-
   invertItem(dat[[1]]$Selfidentity_behaviormeansmoretantheactself, c(1, 6));
 
### Exercise only
 dat[[3]]$Selfidentity_notthinkingaboutexercisingregularly <-
   invertItem(dat[[3]]$Selfidentity_notthinkingaboutexercisingregularly, c(1, 6));
 dat[[3]]$Selfidentity_exercisingenoughimportant <-
   invertItem(dat[[3]]$Selfidentity_exercisingenoughimportant, c(1, 6));
 dat[[3]]$Selfidentity_exercisingenoughmeansmore <-
   invertItem(dat[[3]]$Selfidentity_exercisingenoughmeansmore, c(1, 6));




  
  
###OUDE Code van GJ
#   x$Intention_intend <- x$Intention_intend - 19;
#   x$Intention2willing <- ifelse(x$Intention2willing < 30, x$Intention2willing - 21,
#                                 x$Intention2willing - 42);
#   x$Intention3expect <- x$Intention3expect - 13;
#   x$curBeh <- ifelse(x$curBeh > 13, x$curBeh - 1, x$curBeh);
#   x$curBeh <- x$curBeh - 8;

#   x$Injunctivenorm_importantpeople <- x$Injunctivenorm_importantpeople - 39
#   x$Descriptivenorm_closefriends <- x$Descriptivenorm_closefriends - 14
#    <- x$Perceivedcontrol_forme - 21
#   x$Perceivedcontrol_reallywantto <- ifelse(
#     ifelse(x$Perceivedcontrol_reallywantto < 18, x$Perceivedcontrol_reallywantto - 8,
#                                 x$Perceivedcontrol_reallywantto - 9);
#   x$Selfidentity_wrongbehaviormeansmorethanjusttheact <- 
#     invertItem(x$Selfidentity_wrongbehaviormeansmorethanjusttheact, c(1, 6));
#   x$Selfidentity_wouldfeelatalossgivingupwrongbehavior <-
#     invertItem(x$Selfidentity_wouldfeelatalossgivingupwrongbehavior, c(1, 6));
#   x$Past_howoften <- x$Past_howoften - 27;
#   return(x);
# });
# 
# ### Alcohol only
# dat[[1]]$selectionAlcohol <-
#   invertItem(dat[[1]]$selectionAlcohol - 9);
# dat[[1]]$SelfidentityRestrainedAlcohol <-
#   invertItem(dat[[1]]$SelfidentityRestrainedAlcohol);
# dat[[1]]$Selfidentity_importantpart <-
#   invertItem(dat[[1]]$Selfidentity_importantpart, c(1, 6));
# dat[[1]]$Selfidentity_givingupalcoholdrinking <-
#   invertItem(dat[[1]]$Selfidentity_givingupalcoholdrinking, c(1, 6));
# dat[[1]]$Selfidentity_behaviormeansmoretantheactself <-
#   invertItem(dat[[1]]$Selfidentity_behaviormeansmoretantheactself, c(1, 6));
# 
# ### Exercise only
# dat[[3]]$Selfidentity_notthinkingaboutexercisingregularly <-
#   invertItem(dat[[3]]$Selfidentity_notthinkingaboutexercisingregularly, c(1, 6));
# dat[[3]]$Selfidentity_exercisingenoughimportant <-
#   invertItem(dat[[3]]$Selfidentity_exercisingenoughimportant, c(1, 6));
# dat[[3]]$Selfidentity_exercisingenoughmeansmore <-
#   invertItem(dat[[3]]$Selfidentity_exercisingenoughmeansmore, c(1, 6));




readline(prompt = "Is alle recoding gefixt?")




###########################################################################
###########################################################################
### Lists with relevant variables
###########################################################################
###########################################################################

scales <- list();

scales$selfIdentity <- c('Selfidentity_rarelythinkabout', 
                         'Selfidentity_kindofperson',
                         'Selfidentity_seemyselfas', 
                         'Selfidentity_concernedwithnotdoingthebehaviorenough', 
                         'Selfidentity_doingbehaviorimportant', 
                         'Selfidentity_importantpart', 
                         'Selfidentity_seemyselffollowingthebehaviorguideline', 
                         'Selfidentity_wouldfeelatalossgivingupwrongbehavior', 
                         'Selfidentity_concernedwithdoingtherightbehavior',
                         'Selfidentity_wrongbehaviormeansmorethanjusttheact', 
                         'Selfidentity_behaviormeansmoretantheactself');

scales$attitude <- c('Attitude_bad_good', 'Attitude_unpleasant_pleasant',
                     'Attitude_harmful_beneficial', 'Attitude_boring_interesting');

scales$importance <- c('Importancescale_unimportant_important',
                       'Importancescale_notessential_essential',
                       'Importancescale_notsignificant_significant');

scales$attitudeImportance <- c(scales$attitude, scales$importance);

scales$perceivedNorms <- c('Injunctivenorm_importantpeople',
                           'Injunctivenorm_mostpeopleapprove',
                           'Descriptivenorm_closefriends',
                           'Descriptivenorm_peoplelikeme');
                           
scales$pbc <- c('Perceivedcontrol_forme',
                'Perceivedcontrol_reallywantto',
                'Perceivedcontrol_confident');

scales$intention <- c('Intention_intend',
                      'Intention2willing',
                      'Intention3expect');

scales$pastBehavior <- c('Past_haveused', 'Past_howoften');

scales$currentBehavior <- c('curBeh');

###########################################################################
###########################################################################
### Model specification for self-identity CFA
###########################################################################
###########################################################################

### CFA model
si.cfaModel <- paste0("selfIdentity =~ ",
                      paste(scales$selfIdentity, collapse=" + "));

###########################################################################
###########################################################################
### Merge dataframes
###########################################################################
###########################################################################

dat[[1]]$targetBehavior <- 'alcohol';
dat[[2]]$targetBehavior <- 'condoms';
dat[[3]]$targetBehavior <- 'exercise';

dat <- rbind.fill(dat);

by(dat[, c('age', 'sex')], dat$targetBehavior, colMeans, na.rm=TRUE);

###########################################################################
###########################################################################
### Some cleaning and recoding
###########################################################################
###########################################################################

dat$country <- factor(dat$country, levels=c(1,2), labels=c("USA", "India"));

###########################################################################
###########################################################################
### Conduct factor analyses
###########################################################################
###########################################################################

### Initiate an rnwString for knitr to process into a PDF
### (we set the page to landscape for the correlation tables)
rnwString <- rnwString.initiate("Self-identity factor analysesPlayground",
                                authorName,
                                docClassArgs = 'a4paper,portrait,6pt');

### Create object to store results
si <- list();

### Loop through cultures
for (curCult in countries) {
  
  ### Create object for this county
  si[[curCult]] <- list();

  ### Loop through target behaviors
  for (curBeh in behaviors) {
    
    ### Add section title
    rnwString <- rnwString.section(rnwString, paste0(curCult, " - ", curBeh));
    
    ### Create object for this target behavior
    si[[curCult]][[curBeh]] <- list();
    
    ### Store correlation matrix for self-identity items
    si[[curCult]][[curBeh]]$cor <-
      cor(dat[dat$targetBehavior == curBeh & dat$country == curCult,
              scales$selfIdentity],
          use="complete.obs");
    
    ### Add correlations to the PDF (go to landscape)
    rnwString <- rnwString.subsection(rnwString, "Correlations and eigen values");
    rnwString <- rnwString.add(rnwString, 
      paste0(capture.output(print(rMatrix(dat[dat$targetBehavior == curBeh & dat$country == curCult, ],
                                          x=scales$selfIdentity,
                                          output="LaTeX",
                                          pboxWidthMultiplier=.1))),
             collapse="\n"), sanitize=FALSE, landscape=TRUE);
    
    ### Eigen values
    si[[curCult]][[curBeh]]$eigen <-
      eigen(si[[curCult]][[curBeh]]$cor)$values;

    ### Parallel Analysis
    rnwString <- rnwString.subsection(rnwString, "Parallel Analysis");
    rnwString <- rnwString.chunk(rnwString=rnwString, chunk=paste0(
      "fa.parallel(x = dat[dat$targetBehavior == '", curBeh,
      "' & dat$country == '", curCult,
      "', scales$selfIdentity], fm='ml', main='Parallel Analysis');"));

    ### Very Simple Structure
    rnwString <- rnwString.subsection(rnwString, "Very Simple Structure");
    rnwString <- rnwString.chunk(rnwString=rnwString, chunk=paste0(
      "print(vss(x = dat[dat$targetBehavior == '", curBeh,
      "' & dat$country == '", curCult,
      "', scales$selfIdentity], fm='mle', rotate='oblimin', n=6,
          title='Very Simple Structure'));"));

    ### Exploratory factor analyses
    si[[curCult]][[curBeh]]$efa <- list();
    for (nfactors in 1:3) {
      si[[curCult]][[curBeh]]$efa[[nfactors]] <-
        fa(r = dat[dat$targetBehavior == curBeh & dat$country == curCult,
                   scales$selfIdentity], nfactors = nfactors,
           rotate = "oblimin", n.iter = n.iter, fm="ml"); ### Was minres?
    }
    
    ### Add EFA to PDF (with section titles)
    rnwString <- rnwString.subsection(rnwString, "EFA extracting 1 factor");
    rnwString <- rnwString.chunk(rnwString=rnwString,
                                 chunk=paste0("print(si[['",
                                              curCult, "']][['",
                                              curBeh, "']]$efa[[1]]);"));
    rnwString <- rnwString.subsection(rnwString, "EFA extracting 2 factors");
    rnwString <- rnwString.chunk(rnwString=rnwString,
                                 chunk=paste0("print(si[['",
                                              curCult, "']][['",
                                              curBeh, "']]$efa[[2]]);"));
    rnwString <- rnwString.subsection(rnwString, "EFA extracting 3 factors");
    rnwString <- rnwString.chunk(rnwString=rnwString,
                                 chunk=paste0("print(si[['",
                                              curCult, "']][['",
                                              curBeh, "']]$efa[[3]]);"));

    ### Confirmatory factor analysis
    si[[curCult]][[curBeh]]$cfa <- lavaan(si.cfaModel,
                                          data=dat[dat$targetBehavior == curBeh & dat$country == curCult,
                                                   scales$selfIdentity],
                                          auto.var=TRUE, auto.fix.first=TRUE,
                                          auto.cov.lv.x=TRUE);
    
    ### Add CFA to PDF (with section titles)
    rnwString <- rnwString.subsection(rnwString, "CFA with one factor");
    rnwString <- rnwString.chunk(rnwString=rnwString,
                                 chunk=paste0("print(summary(si[['",
                                              curCult, "']][['",
                                              curBeh, "']]$cfa, fit.measures=TRUE));"));

  }
}

### Terminate the rnwString and generate the PDF.
rnwString <- rnwString.terminate(rnwString);
rnwString.generate(rnwString, fileName = "self-identity factor analysesPlayground",
                   rnwPath=rnwPath, pdfLatexPath=pdfLatexPath);

###########################################################################
###########################################################################
###
### Select the four self-identity items
###
###########################################################################
###########################################################################

### There are four items that consistently load on the same factor in both
### cultures and for all three behaviors:
scales$selfidentity_selected <- scales$selfIdentity[c(2, 3, 7)];
scales$selfidentity_selected4 <- scales$selfIdentity[c(2, 3, 7, 9)];

###########################################################################
###########################################################################
### Model specification for self-identity CFA
###########################################################################
###########################################################################

### CFA model
si.cfaModel <- paste0("selfIdentity =~ ",
                      paste(scales$selfIdentity, collapse=" + "));

### Measurement models
si.measurements <- list();
for (currentScale in names(scales)) {
  si.measurements[[currentScale]] <- paste0(currentScale, ' =~ ',
                                            paste0(scales[[currentScale]],
                                                   collapse=" + "));
}

### Structural models
si.models <- list(basicModel = list(),
                  siSeparate = list(),
                  impSeparate = list(),
                  impInAttitude = list(),
                  siANDimpInAttitude = list());

### SEM models
si.models$basicModel$sem <- paste0("### Measurement model \n",
                                   si.measurements$intention, "\n",
                                   si.measurements$attitude, "\n",
                                   si.measurements$perceivedNorms, "\n",
                                   si.measurements$pbc, "\n",
                                   si.measurements$pastBehavior, "\n",
                                   "### Residual correlations \n",
                                   "attitude ~~ perceivedNorms", "\n",
                                   "attitude ~~ pbc", "\n",
                                   "perceivedNorms ~~ pbc", "\n",
                                   "perceivedNorms ~~ selfidentity_selected", "\n",
                                   "### Structural model \n",
                                   "attitude ~ pastBehavior", "\n",
                                   "perceivedNorms ~ pastBehavior", "\n",
                                   "pbc ~ pastBehavior", "\n",
                                   "intention ~ attitude + perceivedNorms + pbc");
si.models$siSeparate$sem <- paste0("### Measurement model \n",
                                   si.measurements$intention, "\n",
                                   si.measurements$attitude, "\n",
                                   si.measurements$perceivedNorms, "\n",
                                   si.measurements$pbc, "\n",
                                   si.measurements$selfidentity_selected, "\n",
                                   si.measurements$pastBehavior, "\n",
                                   "### Residual correlations \n",
                                   "attitude ~~ perceivedNorms", "\n",
                                   "attitude ~~ pbc", "\n",
                                   "attitude ~~ selfidentity_selected", "\n",
                                   "perceivedNorms ~~ pbc", "\n",
                                   "perceivedNorms ~~ selfidentity_selected", "\n",
                                   "pbc ~~ selfidentity_selected", "\n",
                                   "### Structural model \n",
                                   "attitude ~ pastBehavior", "\n",
                                   "perceivedNorms ~ pastBehavior", "\n",
                                   "pbc ~ pastBehavior", "\n",
                                   "selfidentity_selected ~ pastBehavior", "\n",
                                   "intention ~ attitude + perceivedNorms + pbc + selfidentity_selected");
si.models$impSeparate$sem <- paste0("### Measurement model \n",
                                    si.measurements$intention, "\n",
                                    si.measurements$attitude, "\n",
                                    si.measurements$perceivedNorms, "\n",
                                    si.measurements$pbc, "\n",
                                    si.measurements$importance, "\n",
                                    si.measurements$pastBehavior, "\n",
                                    "### Residual correlations \n",
                                    "attitude ~~ perceivedNorms", "\n",
                                    "attitude ~~ pbc", "\n",
                                    "attitude ~~ importance", "\n",
                                    "perceivedNorms ~~ pbc", "\n",
                                    "perceivedNorms ~~ importance", "\n",
                                    "pbc ~~ importance", "\n",
                                    "### Structural model \n",
                                    "attitude ~ pastBehavior", "\n",
                                    "perceivedNorms ~ pastBehavior", "\n",
                                    "pbc ~ pastBehavior", "\n",
                                    "importance ~ pastBehavior", "\n",
                                    "intention ~ attitude + perceivedNorms + pbc + importance");
si.models$impInAttitude$sem <- paste0("### Measurement model \n",
                                      si.measurements$intention, "\n",
                                      si.measurements$attitudeImportance, "\n",
                                      si.measurements$perceivedNorms, "\n",
                                      si.measurements$pbc, "\n",
                                      si.measurements$pastBehavior, "\n",
                                      "### Residual correlations \n",
                                      "attitudeImportance ~~ perceivedNorms", "\n",
                                      "attitudeImportance ~~ pbc", "\n",
                                      "perceivedNorms ~~ pbc", "\n",
                                      "### Structural model \n",
                                      "attitudeImportance ~ pastBehavior", "\n",
                                      "perceivedNorms ~ pastBehavior", "\n",
                                      "pbc ~ pastBehavior", "\n",
                                      "intention ~ attitudeImportance + perceivedNorms + pbc");
si.models$siANDimpInAttitude$sem <- paste0("### Measurement model \n",
                                           si.measurements$intention, "\n",
                                           si.measurements$attitudeImportance, "\n",
                                           si.measurements$perceivedNorms, "\n",
                                           si.measurements$pbc, "\n",
                                           si.measurements$selfidentity_selected, "\n",
                                           si.measurements$pastBehavior, "\n",
                                           "### Residual correlations \n",
                                           "attitudeImportance ~~ perceivedNorms", "\n",
                                           "attitudeImportance ~~ pbc", "\n",
                                           "attitudeImportance ~~ selfidentity_selected", "\n",
                                           "perceivedNorms ~~ pbc", "\n",
                                           "perceivedNorms ~~ selfidentity_selected", "\n",
                                           "pbc ~~ selfidentity_selected", "\n",
                                           "### Structural model \n",
                                           "attitudeImportance ~ pastBehavior", "\n",
                                           "perceivedNorms ~ pastBehavior", "\n",
                                           "pbc ~ pastBehavior", "\n",
                                           "selfidentity_selected ~ pastBehavior", "\n",
                                           "intention ~ attitudeImportance + perceivedNorms + pbc + selfidentity_selected");

### Regression models without past behavior
si.models$basicModel$lm1 <- intention ~ attitude + perceivedNorms + pbc;
si.models$siSeparate$lm1 <- intention ~ attitude + perceivedNorms + pbc + selfidentity_selected;
si.models$impSeparate$lm1 <- intention ~ attitude + perceivedNorms + pbc + importance;
si.models$impInAttitude$lm1 <- intention ~ attitudeImportance + perceivedNorms + pbc;
si.models$siANDimpInAttitude$lm1 <- intention ~ attitudeImportance + perceivedNorms + pbc + selfidentity_selected;

### Regression models with past behavior
si.models$basicModel$lm2 <- intention ~ attitude + perceivedNorms + pbc + pastBehavior;
si.models$siSeparate$lm2 <- intention ~ attitude + perceivedNorms + pbc + selfidentity_selected + pastBehavior;
si.models$impSeparate$lm2 <- intention ~ attitude + perceivedNorms + pbc + importance + pastBehavior;
si.models$impInAttitude$lm2 <- intention ~ attitudeImportance + perceivedNorms + pbc + pastBehavior;
si.models$siANDimpInAttitude$lm2 <- intention ~ attitudeImportance + perceivedNorms + pbc + selfidentity_selected + pastBehavior;

###########################################################################
###########################################################################
###
### Conceptual independence
###
### If the self-identity items are added to the typical RAA constructs in
### factor analyses, do they end up in a different factor?
###
###########################################################################
###########################################################################

### Generate a new PDF
rnwString <- rnwString.initiate("Self-identity conceptual independence2",
                                authorName,
                                docClassArgs = 'a4paper,portrait,10pt');

### Inspect population-wide factor loadings, correlations, and
### internal consistency/reliability estmimates
rnwString <- rnwString.section(rnwString, "One factor population-wide characteristics");
for (colIndex in 1:length(names(scales)[names(scales) != "selfIdentity"])) {
  rnwString <- rnwString.subsection(rnwString,
                                 paste0("Scale characteristics for ", names(scales)[[colIndex]]));
  rnwString <- rnwString.chunk(rnwString,
                               paste0('fa(r = dat[, scales[["', names(scales)[[colIndex]],
                                      '"]]], nfactor = 1,',
                                      'rotate = "oblimin", n.iter = n.iter, fm=faMethod);\n',
                                      'rMatrix(dat, x=scales[["', names(scales)[[colIndex]],
                                      '"]]);\n',
                                      'scaleReliability(dat[, scales[["', names(scales)[[colIndex]],
                                      '"]]]);'));
}

tmpScales <- scales[names(scales)!='selfIdentity'];

### Loop through variables, first the column variables
for (colIndex in 1:length(tmpScales)) {
  rnwString <- rnwString.section(rnwString,
                                 paste0("Conceptual independence of ", names(tmpScales)[[colIndex]]));
  ### Then the row variables
  for (rowIndex in 2:length(tmpScales)) {
    if (colIndex < rowIndex) {
    rnwString <- rnwString.subsection(rnwString,
                                        paste0("Independence from ", names(tmpScales)[[rowIndex]]));
      variableNames <- paste0("c('",
                              paste0(tmpScales[[colIndex]], collapse="', '"),
                              "', '",
                              paste0(tmpScales[[rowIndex]], collapse="', '"),
                              "')");
      rnwString <- rnwString.chunk(rnwString,
                                   paste0('fa(r = dat[, ', variableNames, '], nfactor = 2,',
                                          'rotate = "oblimin", n.iter = n.iter, fm=faMethod);'));
    }
  }
}

### Terminate the rnwString and generate the PDF.
rnwString <- rnwString.terminate(rnwString);
rnwString.generate(rnwString, fileName = "self-identity conceptual independence2",
                   rnwPath=rnwPath, pdfLatexPath=pdfLatexPath);

###########################################################################
###########################################################################
###
### Structural models
###
### Running regression analysis and structural equation models to explore
### added value of self-identity measure
###
###########################################################################
###########################################################################

### Generate the scales and add them to the dataframe
dat <- makeScales(dat, scales);

addStructuralModels <- function(rnwString, dat, vars, si.models,
                                curCult = countries, curBeh = behaviors) {

  ### String with rows to select
  selectionRows.beh <- paste0("c('", paste0(curBeh, collapse="', '"), "')");
  selectionRows.cult <- paste0("c('", paste0(curCult, collapse="', '"), "')");
  selectionRows.pastBehValid <- paste0("complete.cases(dat$",
                                       paste0(scales$pastBehavior,
                                              collapse=", dat$"),
                                       ")");
  selectionRows <- paste0("dat$targetBehavior %in% ",
                          selectionRows.beh,
                          " & dat$country %in% ",
                          selectionRows.cult,
                          " & ", selectionRows.pastBehValid);
  
  ### Construct name of object to store SEM result in
  semObjectCountryBit <- paste0(curCult, collapse="_");
  semObjectBehBit <- paste0(curBeh, collapse="_");
  semObjectName <- paste0("si.sem[['", semObjectCountryBit, "']][['",
                          semObjectBehBit, "']]");
  
  ### Select temporary smaller dataset
  subDat <- dat[dat$targetBehavior %in% curBeh &
                  dat$country %in% curCult &
                    complete.cases(dat[, scales$pastBehavior]),
                vars];
  
  rnwString <- rnwString.section(rnwString,
                                 paste0(paste0(curCult, collapse=", "),
                                        " - ",
                                        paste0(curBeh, collapse=", ")));
  rnwString <- rnwString.add(rnwString,
                             paste0("Number of participants in analysis:", nrow(subDat)));
  
  ### Add correlations to the PDF (go to landscape)
  rnwString <- rnwString.subsection(rnwString, "Correlations");
  rnwString <- rnwString.add(rnwString, 
                             paste0(capture.output(print(rMatrix(subDat,
                                                                 x=vars,
                                                                 output="LaTeX",
                                                                 pboxWidthMultiplier=.1))),
                                    collapse="\n"), sanitize=FALSE, landscape=TRUE);

  ### Loop through (initially five) models
  for (curModel in names(si.models)) {
    
    rnwString <- rnwString.subsection(rnwString, paste0(curModel, ' - LM1'));
    #rnwString <- rnwString.chunk(paste0(curBeh, '-', curCult, '-', curModel, '-LM1'),
    rnwString <- rnwString.chunk(rnwString,
                                 paste0("regr(si.models[['", curModel, "']]$lm1",
                                        ", dat=dat[", selectionRows, ", ]);"));

    rnwString <- rnwString.subsection(rnwString, paste0(curModel, ' - LM2'));
    ###rnwString <- rnwString.chunk(paste0(curBeh, '-', curCult, '-', curModel, '-LM2'),
    rnwString <- rnwString.chunk(rnwString,
                                 paste0("regr(si.models[['", curModel, "']]$lm2",
                                        ", dat=dat[", selectionRows, ", ]);"));

    rnwString <- rnwString.subsection(rnwString, paste0(curModel, ' - Comparison'));
    #rnwString <- rnwString.chunk(paste0(curBeh, '-', curCult, '-', curModel, '-Comp'),
    rnwString <- rnwString.chunk(rnwString,
                                 paste0("anova(lm(si.models[['", curModel, "']]$lm1",
                                        ", data=dat[", selectionRows, ", ]), ",
                                        "lm(si.models[['", curModel, "']]$lm2",
                                        ", data=dat[", selectionRows, ", ]));"));
    
    rnwString <- rnwString.subsection(rnwString, paste0(curModel, ' - SEM'));
    #rnwString <- rnwString.chunk(paste0(curBeh, '-', curCult, '-', curModel, '-SEM'),
    rnwString <- rnwString.chunk(rnwString,
                                 paste0("summary(lavaan(si.models[['", curModel, "']]$sem",
                                        ", data=dat[", selectionRows,
                                        ", ], auto.var=TRUE, auto.fix.first=TRUE), ",
                                        "fit.measures=TRUE);\n",
                                        "tempObject <- ",
                                        "lavaan(si.models[['", curModel, "']]$sem",
                                        ", data=dat[", selectionRows,
                                        ", ], auto.var=TRUE, auto.fix.first=TRUE);\n",
                                        semObjectName,
                                        "[['", curModel, "']] <- list();\n",
                                        semObjectName,
                                        "[['", curModel, "']] <- tempObject;"));
  }
  
  return(rnwString);
  
}

### Generate a new PDF
rnwString <- rnwString.initiate("Self-identity structural models2",
                                authorName,
                                docClassArgs = 'a4paper,portrait,10pt');

### Generate object to store SEM models in
si.sem <- list();

### Loop through cultures
for (curCult in countries) {
  si.sem[[curCult]] <- list();
  ### Loop through target behaviors
  for (curBeh in behaviors) {
    ### Add models for this combination    
    rnwString <- addStructuralModels(rnwString=rnwString,
                                     dat=dat,
                                     vars=names(scales),
                                     si.models = si.models,
                                     curCult=curCult,
                                     curBeh=curBeh);
  }
}

### Per culture
for (curCult in countries) {
  rnwString <- addStructuralModels(rnwString=rnwString,
                                   dat=dat,
                                   vars=names(scales),
                                   si.models = si.models,
                                   curCult=curCult);
}

### Per target behaviors
for (curBeh in behaviors) {
  rnwString <- addStructuralModels(rnwString=rnwString,
                                   dat=dat,
                                   vars=names(scales),
                                   si.models = si.models,
                                   curBeh=curBeh);
}

### For entire population
rnwString <- addStructuralModels(rnwString=rnwString,
                                 dat=dat,
                                 vars=names(scales),
                                 si.models = si.models);

### Terminate the rnwString and generate the PDF.
rnwString <- rnwString.terminate(rnwString);
rnwString.generate(rnwString, fileName = "self-identity structural models2",
                   rnwPath=rnwPath, pdfLatexPath=pdfLatexPath);

### Generate a new PDF
rnwString <- rnwString.initiate("Self-identity model comparisons2",
                                authorName,
                                docClassArgs = 'a4paper,portrait,10pt');

for (curCult in names(si.sem)) {  
  for (curBeh in names(si.sem[[curCult]])) {
    rnwString <- rnwString.subsection(rnwString,
                                      paste0("Model comparisons for: ", curCult,
                                             " --- ", curBeh));
    rnwString <- rnwString.chunk(rnwString,
                                 paste0("anova(si.sem$", curCult,
                                        "$", curBeh, "$basicModel,
                                        si.sem$", curCult,
                                        "$", curBeh, "$siSeparate);\n",
                                        "anova(si.sem$", curCult,
                                        "$", curBeh, "$basicModel,
                                        si.sem$", curCult,
                                        "$", curBeh, "$impSeparate);\n",
                                        "anova(si.sem$", curCult,
                                        "$", curBeh, "$basicModel,
                                        si.sem$", curCult,
                                        "$", curBeh, "$impInAttitude);\n",
                                        "anova(si.sem$", curCult,
                                        "$", curBeh, "$impInAttitude,
                                        si.sem$", curCult,
                                        "$", curBeh, "$siANDimpInAttitude);\n"));
  }
}

### Terminate the rnwString and generate the PDF.
rnwString <- rnwString.terminate(rnwString);
rnwString.generate(rnwString, fileName = "self-identity model comparisons2",
                   rnwPath=rnwPath, pdfLatexPath=pdfLatexPath);

###########################################################################
###########################################################################
### Correlations
###########################################################################
###########################################################################

associationMatrix(dat[, names(scales)[which(names(scales) != 'selfIdentity')]],
                  file = file.path(rnwPath, "algemene correlatiematrix.html"), type="html");

associationMatrix(dat[, scales$selfIdentity],
                  file = file.path(rnwPath, "SI items correlatiematrix.html"), type="html");

### Object to store correlation matrices in
si.corMatrices <- list();
si.datasets <- list();

### Per country
for (curCult in countries) {
  si.corMatrices[[curCult]] <- list();
  si.datasets[[curCult]] <- list();
  ### Per target behaviors
  for (curBeh in behaviors) {
    si.datasets[[curCult]][[curBeh]] <-
      na.omit(dat[dat$targetBehavior %in% curBeh &
                    dat$country %in% curCult,
                  names(scales)[which(names(scales) != 'selfIdentity')]]);
    si.corMatrices[[curCult]][[curBeh]] <- cor(si.datasets[[curCult]][[curBeh]]);
    print(associationMatrix(si.datasets[[curCult]][[curBeh]],
                      type="html",
                      file = file.path(rnwPath, paste0("correlatiematrix - ",
                                                       curCult, ", ",
                                                       curBeh,
                                                       ".html"))));
  }
}

### Compare correlation matrices
si.corMatrixComparisons <- list();
for (primaryCurCult in countries) {
  for (primaryCurBeh in behaviors) {
    for (secondaryCurCult in countries) {
      for (secondaryCurBeh in behaviors) {
        if ((primaryCurCult != secondaryCurCult) ||
              (primaryCurBeh != secondaryCurBeh)) {
          currentComparison <- paste0(primaryCurCult, "-", primaryCurBeh,
                                      "_vs_",
                                      secondaryCurCult, "-", secondaryCurBeh);
          si.corMatrixComparisons[[currentComparison]] <- list();
          si.corMatrixComparisons[[currentComparison]]$cortest.normal <-
            cortest.normal(R1=si.corMatrices[[primaryCurCult]][[primaryCurBeh]],
                           R2=si.corMatrices[[secondaryCurCult]][[secondaryCurBeh]],
                           n1=nrow(si.datasets[[primaryCurCult]][[primaryCurBeh]]),
                           n2=nrow(si.datasets[[secondaryCurCult]][[secondaryCurBeh]]));
          si.corMatrixComparisons[[currentComparison]]$cortest.jennrich <-
            cortest.jennrich(R1=si.corMatrices[[primaryCurCult]][[primaryCurBeh]],
                             R2=si.corMatrices[[secondaryCurCult]][[secondaryCurBeh]],
                             n1=nrow(si.datasets[[primaryCurCult]][[primaryCurBeh]]),
                             n2=nrow(si.datasets[[secondaryCurCult]][[secondaryCurBeh]]));
        }
      }
    }
  }
}

print(si.corMatrixComparisons);

###########################################################################
### Descriptives
###########################################################################

si.describe <- list();
### Per country
for (curCult in countries) {
  si.describe[[curCult]] <- list();
  ### Per target behaviors
  for (curBeh in behaviors) {
    si.describe[[curCult]][[curBeh]] <-
      describe(si.datasets[[curCult]][[curBeh]]);
    cat("\n\n### DESCRIPTIVES FOR", curCult, "AND", curBeh, "\n\n");
    print(si.describe[[curCult]][[curBeh]]);
  }
}

cat("\n\n### DESCRIPTIVES FOR TOTAL SAMPLE\n\n");
print(describe(na.omit(dat[, names(scales)[which(names(scales) != 'selfIdentity')]])));






