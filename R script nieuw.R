### URL with datafile
fileUrl <- "https://www.dropbox.com/s/wf4600x184bgz9n/Condom_use__New.csv?dl=1"

basePath <- 'C:/Users/iMac/Dropbox/Shared folder of Marwin & GJ/R les';
#pathBase <- 'B:/Shared/Shared folder of Marwin & GJ/';

workingPath <- basePath;
outputPath <- file.path(basePath, 'output');
scriptPath <- basePath;
dataPath <- basePath;

### Whether to update the userfriendlyscience package,
### and which versions were used for this script
userfriendlyscienceVersion <- "0.4-0";
updateUserfriendlyscience = FALSE;

### Function to check for, maybe upload, maybe download, install, and load1 
### a package
updatePackageInProgress <-
  function(packagename, packageversion, packageWorkingPath,
           packageBuildingPath = "B:/Data/statistics/R/library",
           userfriendlyscienceURL = "http://userfriendlyscience.com",
           silent=FALSE, type=c('bin', 'source')) {
    
    fullRepositories <- c(c('userfriendlyscience'=userfriendlyscienceURL),
                          getOption("repos"));
    
    ### Establish name of the package file
    srcFile <- paste0(packagename, "_", packageversion, ".tar.gz");
    binFile <- paste0(packagename, "_", packageversion, ".zip");
    
    ### Copy potentially newly created and compiled versions of the package
    ### to the directory from where we'll include it
    if (file.exists(file.path(packageBuildingPath, binFile))) {
      if (!silent) cat("Copied '", file.path(packageBuildingPath, binFile),
                       "' to '", packageWorkingPath, "'.\n", sep="");
      file.copy(file.path(packageBuildingPath, binFile),
                packageWorkingPath, overwrite = TRUE);
      if (require('RCurl')) {
        if (url.exists('userfriendlyscience.com')) {
          ftpUpload(file.path(packageBuildingPath, binFile),
                    paste0("ftp://userfriendlyscience.com/", binFile),
                    userpwd="package@userfriendlyscience.com:uploadPackage");
          if (!silent) cat("Uploaded '", file.path(packageBuildingPath, binFile),
                           "' to 'ftp://userfriendlyscience.com'.\n", sep="");
        }
      }
    }
    if (file.exists(file.path(packageBuildingPath, srcFile))) {
      if (!silent) cat("Copied '", file.path(packageBuildingPath, srcFile),
                       "' to '", packageWorkingPath, "'.\n", sep="");
      file.copy(file.path(packageBuildingPath, srcFile),
                packageWorkingPath, overwrite = TRUE);
      if (require('RCurl')) {
        if (url.exists('userfriendlyscience.com')) {
          ftpUpload(file.path(packageBuildingPath, srcFile),
                    paste0("ftp://userfriendlyscience.com/", srcFile),
                    userpwd="package@userfriendlyscience.com:uploadPackage");
          if (!silent) cat("Uploaded '", file.path(packageBuildingPath, srcFile),
                           "' to 'ftp://userfriendlyscience.com'.\n", sep="");
        }
      }
    }
    
    if (paste0("package:", packagename) %in% search()) {
      if (!silent) cat("Package '", packagename,
                       "' is loaded; unloading.\n", sep="");
      detach(paste0("package:", packagename),
             character.only=TRUE, unload=TRUE);
      if (!silent) cat("Detached package '", packagename,
                       "'.\n", sep="");
    }
    if (is.element(packagename, installed.packages()[, 1])) {
      remove.packages(packagename);
      if (!silent) cat("Uninstalled package '", packagename,
                       "'.\n", sep="");
    }
    
    ### If we should try to load the bin package, try that first
    if (file.exists(file.path(packageWorkingPath, binFile)) &&
        ('bin' %in% type)) {
      install.packages(file.path(packageWorkingPath, binFile),
                       type='win.binary', repos=NULL);
      cat("Installed package '", packagename, "'.\n");
    } else if (file.exists(file.path(packageWorkingPath, srcFile)) &&
               ('source' %in% type)) {
      install.packages(file.path(packageWorkingPath, srcFile),
                       type='source', repos=NULL);
      cat("Installed package '", packagename, "'.\n");
    } else {
      ### Otherwise download it from the userfriendlyscience repository
      if ('bin' %in% type) {
        install.packages(packagename, type='win.binary', repos=fullRepositories);
      } else if ('source' %in% type) {
        install.packages(packagename, type='source', repos=fullRepositories);
      }
    }
    require(package=packagename, character.only=TRUE);
  }

if (updateUserfriendlyscience) {
  updatePackageInProgress('userfriendlyscience', userfriendlyscienceVersion,
                          packageWorkingPath=workingPath);
} else {
  require('userfriendlyscience');
}

### Getting other packages
library(userfriendlyscience);
library(plyr)
library(dplyr)
library(car);
library(rmarkdown);

# ### Download file from public dropbox and store in current directory
# download.file(fileUrl, 
#               destfile = file.path(workingPath, "Condom_use__New.csv"));

### Read the data and store in a dataframe
condom.raw <- read.csv(file.path(dataPath, "Condom_use__New.csv"),
                       header = TRUE, sep = ",", row.names=NULL,
                       skip=1, stringsAsFactors = FALSE);

### Remove last variable
condom.raw <- condom.raw[, names(condom.raw) != 'X'];

### Renaming variables
names(condom.raw)[1] <- 'ResponseID ';
names(condom.raw)[2] <- 'ResponseSet';
names(condom.raw)[3] <- 'Name ';
names(condom.raw)[4] <- 'ExternalDataReference  ';
names(condom.raw)[5] <- 'EmailAddress  ';
names(condom.raw)[6] <- 'IPAddress  ';
names(condom.raw)[7] <- 'Status  ';
names(condom.raw)[8] <- 'StartDate  ';
names(condom.raw)[9] <- 'EndDate ';
names(condom.raw)[10] <- 'Finished';
names(condom.raw)[11] <- 'informedConsent';
names(condom.raw)[12] <- 'age';
names(condom.raw)[13] <- 'studyExit';
names(condom.raw)[14] <- 'sex';
names(condom.raw)[15] <- 'country';
names(condom.raw)[16] <- 'selectionCondom';
names(condom.raw)[17] <- 'studyExit';
names(condom.raw)[18] <- 'information';
names(condom.raw)[19] <- 'Intention_intend';
names(condom.raw)[20] <- 'Intention2willing';
names(condom.raw)[21] <- 'Intention3expect';
names(condom.raw)[22] <- 'curBeh';
names(condom.raw)[23] <- 'Attitude_bad_good';
names(condom.raw)[24] <- 'Attitude_unpleasant_pleasant';
names(condom.raw)[25] <- 'Attitude_harmful_beneficial';
names(condom.raw)[26] <- 'Attitude_boring_interesting';
names(condom.raw)[27] <- 'Importancescale_unimportant_important';
names(condom.raw)[28] <- 'Importancescale_notessential_essential';
names(condom.raw)[29] <- 'Importancescale_notsignificant_significant';
names(condom.raw)[30] <- 'information';
names(condom.raw)[31] <- 'Injunctivenorm_importantpeople';
names(condom.raw)[32] <- 'Injunctivenorm_mostpeopleapprove';
names(condom.raw)[33] <- 'Descriptivenorm_closefriends';
names(condom.raw)[34] <- 'Descriptivenorm_peoplelikeme';
names(condom.raw)[35] <- 'Perceivedcontrol_forme';
names(condom.raw)[36] <- 'Perceivedcontrol_reallywantto';
names(condom.raw)[37] <- 'Perceivedcontrol_confident';
names(condom.raw)[38] <- 'information';
names(condom.raw)[39] <- 'Selfidentity_rarelythinkabout';
names(condom.raw)[40] <- 'Selfidentity_kindofperson';
names(condom.raw)[41] <- 'Selfidentity_importantpart';
names(condom.raw)[42] <- 'Selfidentity_doingbehaviorimportant';
names(condom.raw)[43] <- 'Selfidentity_seemyselfas';
names(condom.raw)[44] <- 'Selfidentity_behaviormeansmoretantheactself';
names(condom.raw)[45] <- 'Selfidentity_seemyselffollowingthebehaviorguideline';
names(condom.raw)[46] <- 'Selfidentity_concernedwithnotdoingthebehaviorenough';
names(condom.raw)[47] <- 'Selfidentity_concernedwithdoingtherightbehavior';
names(condom.raw)[48] <- 'Selfidentity_wouldfeelatalossgivingupwrongbehavior';
names(condom.raw)[49] <- 'Selfidentity_wrongbehaviormeansmorethanjusttheact';
names(condom.raw)[50] <- 'information';
names(condom.raw)[51] <- 'Past_haveused';
names(condom.raw)[52] <- 'Past_howoften';
names(condom.raw)[53] <- 'VerificationWorkerID';
names(condom.raw)[54] <- 'Endscreen';
names(condom.raw)[55] <- 'Browser.Meta.Info.Browser ';
names(condom.raw)[56] <- 'Browser.Meta.Info.Version ';
names(condom.raw)[57] <- 'Browser.Meta.Info.Operating.System ';
names(condom.raw)[58] <- 'Browser.Meta.Info.Screen.Resolution';
names(condom.raw)[59] <- 'Browser.Meta.Info.Flash.Version ';
names(condom.raw)[60] <- 'Browser.Meta.Info.Java.Support  ';
names(condom.raw)[61] <- 'Browser.Meta.Info.User.Agent ';
names(condom.raw)[62] <- 'LocationLatitude ';
names(condom.raw)[63] <- 'LocationLongitude';
names(condom.raw)[64] <- 'LocationAccuracy ';

### Converting datafile to dplyr package
#condom.clean <- tbl_df(condom.raw);

### copy to new version to clean up
condom.clean <- condom.raw;

### First selection: removing the unnecessary variables to get a cleaner datafile
### These are the variables from 'age' to 'Past_howoften'
condom.clean <- condom.clean[, which(names(condom.clean) == 'age'):which(names(condom.clean) == 'Past_howoften')];

### Remove studyExit and information
condom.clean <- condom.clean[, names(condom.clean) != 'studyExit'];
condom.clean <- condom.clean[, names(condom.clean) != 'information'];

### Then remove participants who didn't complete the entire questionnaire
condom.clean <- condom.clean[!is.na(condom.clean$Past_howoften), ];



condom.clean$Intention_intend <-
  recode(condom.clean$Intention_intend, "20=1; 21=2; 22=3; 23=4; 24=5; 25=6; 26=7");
condom.clean$Intention2willing <-
  recode(condom.clean$Intention2willing, "43=1; 44=2; 45=3; 46=4; 47=5; 27=6; 28=7"); 
condom.clean$Intention3expect <-
  recode(condom.clean$Intention3expect, "14=1; 15=2; 16=3; 17=4; 18=5; 19=6; 20=7");
condom.clean$curBeh <- 
  recode(condom.clean$curBeh, "9=1; 10=2; 11=3; 12=4; 14=5; 15=6; 16=7");
condom.clean$Injunctivenorm_importantpeople <-
  recode(condom.clean$Injunctivenorm_importantpeople, "40=1; 41=2; 42=3; 43=4; 44=5; 45=6; 46=7; 47=NA");
condom.clean$Descriptivenorm_peoplelikeme <-
  recode(condom.clean$Descriptivenorm_peoplelikeme, "15=1; 16=2; 17=3; 18=4; 19=5; 20=6; 21=7; 22=NA");
condom.clean$Perceivedcontrol_forme <-
  recode(condom.clean$Perceivedcontrol_forme, "22=1; 23=2; 24=3; 25=4; 26=5; 27=6; 28=7");
condom.clean$Perceivedcontrol_reallywantto <-
  recode(condom.clean$Perceivedcontrol_reallywantto, "9=1; 18=2; 10=3; 11=4; 12=5; 13=6; 14=7");
condom.clean$Past_howoften <-
  recode(condom.clean$Past_howoften, "28=1; 29=2; 30=3; 31=4; 32=5; 33=6; 34=7"); 
condom.clean$Descriptivenorm_closefriends <-
  recode(condom.clean$Descriptivenorm_closefriends, "8=NA");
condom.clean$Injunctivenorm_mostpeopleapprove <-
  recode(condom.clean$Injunctivenorm_mostpeopleapprove, "8=NA");




### Viewing the datafile to make sure everything is there
# if (interactive()) {
#   View(condom.clean);
# }

########################################################################
########################################################################
### Voorbereiden 
########################################################################
########################################################################

### We want to view scatter matrices for the associations between the
### importance scale items and the self-identity items.
scatterMatricesVector <- list(importanceScale = c("Importancescale_unimportant_important",
                                                  "Importancescale_notessential_essential",
                                                  "Importancescale_notsignificant_significant"),
                              selfIdentity = c("Selfidentity_rarelythinkabout",
                                               "Selfidentity_kindofperson",
                                               "Selfidentity_importantpart",
                                               "Selfidentity_doingbehaviorimportant",
                                               "Selfidentity_seemyselfas"));

# associationMatrix(condom.clean[, lapply(dat, var) > 0],
#                   type="html",
#                   file=file.path(outputPath,
#                                  paste0('correlatiematrix.html')));

### Generate paginated scattermatrices for the associations between
### MARWIN: AANVULLEN EN UITLEGGEN ETC
selfIdentityItems <- paginatedAsymmetricalScatterMatrix(condom.clean,
                                                        y = scatterMatricesVector$selfIdentity,
                                                        x = scatterMatricesVector$importanceScale,
                                                        maxRows = 4);

### Knit and render rmarkdown report
render(file.path(scriptPath, 'report.Rmd'),
       output_file = file.path(workingPath, 'report.html'),
       intermediates_dir = file.path(workingPath, 'output'));


### Reminder: als we straks gemiddelden gaan uitrekenen,
### dan moeten we rekening houden met dat er missing values kunnen
### en mogen zijn.
