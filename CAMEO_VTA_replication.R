##################################################
## Translating CAMEO Verbs forAutomated Coding of Event Data
## International Interactions
## 2019
## 
## Grant: Modernizing Political Event Data for Big Data Social Science Research 
## NSF - RIDIR Award No. 1539302
## 
## Javier Osorio
## University of Arizona
## February, 2019
## 
##################################################


##################################################
# Set up 

# Define your own working directory
setwd("D:/Dropbox/RIDIR/Verb translation app/Data/CAMEO_VTA_replication/VTA Data Files")
#setwd("C:/Users/javie/Dropbox/RIDIR/Verb translation app/Data/CAMEO_VTA_replication/VTA Data Files")


# Load the required packages
library("rjson")
library("jsonlite")
library("plyr")
library("httr")
library("gmodels")
library("RJSONIO")
library("ggplot2")
library("scales")
library("mlr")
library("psych")
library("plot3D")
library("stargazer")


#####################################################################
#####################################################################
# Load data
  # These are all the .json files downloaded from the VTA server
  # The file "Relational Schema fv.pdf" is an intuitive diagram of the relations between the files    

    CameoEntry <- fromJSON("CameoEntry.json")
    CameoRule <- fromJSON("CameoRule.json")
    CameoTranslatedRule <- fromJSON("CameoTranslatedRule.json")
    FeedbackOnSynsetWord <- fromJSON("FeedbackOnSynsetWord.json")
    ForumMessage <- fromJSON("ForumMessage.json")
    Submission <- fromJSON("Submission.json")
    SynsetEntry <- fromJSON("SynsetEntry.json")
    SynsetExample <- fromJSON("SynsetExample.json")
    SynsetExampleVerdict <- fromJSON("SynsetExampleVerdict.json")
    SynsetStatistics <- fromJSON("SynsetStatistics.json")
    SynsetVerdict <- fromJSON("SynsetVerdict.json")
    SynsetWord <- fromJSON("SynsetWord.json")
    UserInfo <- fromJSON("UserInfo.json")
    VerdictOnRule <- fromJSON("VerdictOnRule.json") 
    Word <- fromJSON("Word.json")

  # These are additional .txt files with the original and augmented rule synsets for verb patterns
    RuleSynsetOrig <-read.csv("CAMEO_rule_synsets_original.csv", header = FALSE, sep = "", dec = ".")
    RuleSynsetNew <-read.csv("CAMEO_rule_synsets_augmented.csv", header = FALSE, sep = "", dec = ".")
    

    

##################################################
# Convert json files into data frames

CameoEntry<- do.call("rbind.fill", lapply(CameoEntry, as.data.frame))
CameoRule<- do.call("rbind.fill", lapply(CameoRule, as.data.frame))
CameoTranslatedRule<- do.call("rbind.fill", lapply(CameoTranslatedRule, as.data.frame))
FeedbackOnSynsetWord<- do.call("rbind.fill", lapply(FeedbackOnSynsetWord, as.data.frame))
ForumMessage<- do.call("rbind.fill", lapply(ForumMessage, as.data.frame))
Submission<- do.call("rbind.fill", lapply(Submission, as.data.frame))
SynsetEntry<- do.call("rbind.fill", lapply(SynsetEntry, as.data.frame))
SynsetExample<- do.call("rbind.fill", lapply(SynsetExample, as.data.frame))
SynsetExampleVerdict<- do.call("rbind.fill", lapply(SynsetExampleVerdict, as.data.frame))
SynsetStatistics<- do.call("rbind.fill", lapply(SynsetStatistics, as.data.frame))
SynsetVerdict<- do.call("rbind.fill", lapply(SynsetVerdict, as.data.frame))
SynsetWord<- do.call("rbind.fill", lapply(SynsetWord, as.data.frame))
UserInfo<- do.call("rbind.fill", lapply(UserInfo, as.data.frame))
VerdictOnRule<- do.call("rbind.fill", lapply(VerdictOnRule, as.data.frame))
Word<- do.call("rbind.fill", lapply(Word, as.data.frame))

RuleSynsetOrig <-data.frame(RuleSynsetOrig)
RuleSynsetNew <-data.frame(RuleSynsetNew)






#####################################################################
#####################################################################
# Replicate Figure 4. Coder Verdicts for Synsets and Translations

# Panel (a). Verdicts for English synsets
freq_SynsetVerdict <- count(SynsetVerdict$verdict)
freq_SynsetVerdict

# Graph
bar_SynsetVerdict<-ggplot(data=SynsetVerdict, aes(x=factor(verdict, labels = c("Incorrect", "Correct", "Ambiguous")))) +
  geom_bar(aes(y = (..count..)/sum(..count..)*100))+
  theme_minimal() +
  xlab("Verdict") + ylab("Percent") +
  scale_y_continuous(limits = c(0,100))
bar_SynsetVerdict
ggsave("graphs/bar_SynsetVerdict.pdf", width = 4, height = 4)



################
# Panel (b). Verdicts for Translated Words
freq_FeedbackOnSynsetWord <- count(FeedbackOnSynsetWord$verdict)
# Eliminate NAs using subset
FeedbackOnSynsetWord <- subset(FeedbackOnSynsetWord, (!is.na(FeedbackOnSynsetWord$verdict)))
freq_FeedbackOnSynsetWord <- count(FeedbackOnSynsetWord$verdict)
freq_FeedbackOnSynsetWord

# Graph
bar_FeedbackOnSynsetWord<-ggplot(data=FeedbackOnSynsetWord, aes(x=factor(verdict, labels = c("Correct", "Incorrect", "Ambiguous")))) +
  geom_bar(aes(y = (..count..)/sum(..count..)*100))+
  theme_minimal() +
  xlab("Verdict") + ylab("Percent")+
  scale_y_continuous(limits = c(0,100))
bar_FeedbackOnSynsetWord
ggsave("graphs/bar_FeedbackOnSynsetWord.pdf", width = 4, height = 4)





#####################################################################
#####################################################################
# Replicate Table 1. Dictionary Additions

################
# Row 1: Verbs

# Total words
TotalWords <- sum(freq_FeedbackOnSynsetWord$freq)
TotalWords

# Number of new words
# TRUE oputput = new words
freq_SynsetWord <- count(!is.na(SynsetWord$submissionId))
freq_SynsetWord

# Generate scalar of new words
NewWords <- freq_SynsetWord[2,2]
NewWords

# Percent of new words wrt total words
NewWordsPercent<-(NewWords/TotalWords)*100
NewWordsPercent



################
# Row 2: Rules 

# Number of new rules
freq_CameoRule <- count(CameoRule$source)
freq_CameoRule

# Create rule categories: old / new
CameoRule$new <- ifelse(CameoRule$source=="CAMEO2", c("old"), c("new")) 
freq_CameoRuleNew <- count(CameoRule$new)
freq_CameoRuleNew

# Generate scalar of new rules
NewRules <- freq_CameoRuleNew[1,2]
NewRules

# Generate scalar of new rules
OldRules <- freq_CameoRuleNew[2,2]
OldRules

# Percent new rules
NewRulesPercent<-(NewRules/OldRules)*100
NewRulesPercent



################
# Row 3: Rule Synsets

# Number of old rule synsets
RuleSynOrig <- nrow(RuleSynsetOrig)
RuleSynOrig

# Number of old rule synsets
RuleSynNew <- nrow(RuleSynsetNew) 
RuleSynNew <- (RuleSynNew-4)      # Apply small correction
RuleSynNew

# Percent new rules
NewRuleSynPercent<-(RuleSynNew/RuleSynOrig)*100
NewRuleSynPercent



###########################
# Putting all pieaces together to create Table 1 

DicAdds <-matrix(c( TotalWords , NewWords , NewWordsPercent, OldRules , NewRules , NewRulesPercent, RuleSynOrig , RuleSynNew, NewRuleSynPercent),ncol=3,byrow=TRUE)
colnames(DicAdds) <-c("Original"	,	"New",	"Percent increase")
rownames(DicAdds)<-c("Verbs", "Rules" ,"Rule Synsets")
DicAdds<-as.table(DicAdds)
DicAdds






#####################################################################
#####################################################################
# Prepare data for Figures 5-7


##################################################
# Merge tables

# Submission -> CameoEntry
SubmissionCameo <- merge(x=Submission,y=CameoEntry,by.x = c("cameoId"),by.y = c("id"),all.y = TRUE)

# SynsetVerdict -> SubmissionCameo
SynsetVerdictSubmissionCameo <- merge(x=SynsetVerdict,y=SubmissionCameo,by.x = c("submissionId"),by.y = c("id"),all.y = TRUE)

# Eliminate NA from verdict
SynsetVerdictSubmissionCameo <- subset(SynsetVerdictSubmissionCameo, (!is.na(SynsetVerdictSubmissionCameo$verdict)))

# Change labels
SynsetVerdictSubmissionCameo$Verdict[SynsetVerdictSubmissionCameo$verdict=="ic"]<- "Incorrect"
SynsetVerdictSubmissionCameo$Verdict[SynsetVerdictSubmissionCameo$verdict=="c"]<- "Correct"
SynsetVerdictSubmissionCameo$Verdict[SynsetVerdictSubmissionCameo$verdict=="a"]<- "Ambiguous"


##################################################
# Count the number of verdicts by CAMEO category

NumVerdictsCameo <-count(SynsetVerdictSubmissionCameo, 'concept')
NumVerdictsCameo

NumVerdictsCameo<-data.frame(NumVerdictsCameo)
summary(NumVerdictsCameo)


##################################################
#  Generate proportion of verdicts by Cameo category

# Generate verdict count by category  
NumVerdictsCameo$ic <-ddply(SynsetVerdictSubmissionCameo,.(concept),
                            summarise,
                            count = length(concept[Verdict == "Incorrect"]))

NumVerdictsCameo$c <-ddply(SynsetVerdictSubmissionCameo,.(concept),
                           summarise,
                           count = length(concept[Verdict == "Correct"]))

NumVerdictsCameo$a <-ddply(SynsetVerdictSubmissionCameo,.(concept),
                           summarise,
                           count = length(concept[Verdict == "Ambiguous"]))

# Generate verdict proportion by category
NumVerdictsCameo$ic_prop <- NumVerdictsCameo$ic.count/NumVerdictsCameo$freq
NumVerdictsCameo$c_prop <-(NumVerdictsCameo$c.count/NumVerdictsCameo$freq) 
NumVerdictsCameo$a_prop <-(NumVerdictsCameo$a.count/NumVerdictsCameo$freq) 
# List variables
ls(NumVerdictsCameo)
# Data frame
NumVerdictsCameo<-data.frame(NumVerdictsCameo)
# Descriptive statistics
summary(NumVerdictsCameo)



#####################################################################
#####################################################################
# Figure 5. Verdict by CAMEO category

# Percentage by CAMEO category
PercentageByCameoCategory<-ggplot(data=SynsetVerdictSubmissionCameo, aes(x = concept)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)*100), position = 'stack') + 
  ylab("Percent") + xlab("CAMEO categories") +
  coord_flip()
PercentageByCameoCategory

ggsave("graphs/bar_PercentageByCameoCategory.pdf", width = 8, height = 5)



#####################################################################
#####################################################################
# Replicate Figure 6. Verdict by CAMEO category - Fill at 100%
VerdictByCameoCategoryFill<-ggplot(data=SynsetVerdictSubmissionCameo, 
  aes(x = concept, y = Verdict, fill = Verdict)) + 
  scale_fill_manual(values=c("gray20", "forestgreen", "firebrick1")) +
  geom_bar(aes(y = (..count..)/sum(..count..)*100), position = 'fill') + 
  ylab("Percent") + xlab("CAMEO categories") +
  coord_flip()
VerdictByCameoCategoryFill

ggsave("graphs/bar_VerdictByCameoCategoryFill.pdf", width = 8, height = 5)


# This is another version of Figure 6 without the stretch at 100%

# Verdict by CAMEO category
VerdictByCameoCategory<-ggplot(data=SynsetVerdictSubmissionCameo, aes(x = concept, y = Verdict, fill = Verdict)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)*100), position = 'stack') + 
  scale_fill_manual(values=c("gray20", "forestgreen", "firebrick1")) +
  ylab("Percent") + xlab("CAMEO categories") +
  coord_flip()
VerdictByCameoCategory

ggsave("graphs/bar_VerdictByCameoCategory.pdf", width = 8, height = 5)





    
#####################################################################
#####################################################################
# Replicate Figure 7. Intercoder Verdict Consistency
    
#################################################  
# Structure data for intercoder reliability analysis
    
# Generate variables for incorrect (ic), correct (c), and ambiguous (a)
SynsetVerdictSubmissionCameo$ic<-ifelse(SynsetVerdictSubmissionCameo$verdict=="ic",1,0)
SynsetVerdictSubmissionCameo$c<-ifelse(SynsetVerdictSubmissionCameo$verdict=="c",1,0)
SynsetVerdictSubmissionCameo$a<-ifelse(SynsetVerdictSubmissionCameo$verdict=="a",1,0)
SynsetVerdictSubmissionCameo<-data.frame(SynsetVerdictSubmissionCameo)
    
# Subset data data
    InterCoder<-subset(SynsetVerdictSubmissionCameo, select=c(wordId, cameoId, userId, ic, c, a))
    InterCoder<-data.frame(InterCoder)
    
# Generate dummy variable per user
    InterCoder$user_a<-ifelse(InterCoder$userId=="4511342715207680",1,0)
    InterCoder$user_b<-ifelse(InterCoder$userId=="5405723685027840",1,0)	
    InterCoder$user_c<-ifelse(InterCoder$userId=="5641206004449280",1,0)	
    InterCoder$user_d<-ifelse(InterCoder$userId=="5652096565116928",1,0)	
    InterCoder$user_e<-ifelse(InterCoder$userId=="5656236913590272",1,0)	
    InterCoder$user_f<-ifelse(InterCoder$userId=="5669720560762880",1,0)	
    InterCoder$user_g<-ifelse(InterCoder$userId=="5679660591480832",1,0)	
    InterCoder$user_h<-ifelse(InterCoder$userId=="5699478375890944",1,0)	
    InterCoder$user_i<-ifelse(InterCoder$userId=="5713990399295488",1,0)	
    InterCoder$user_j<-ifelse(InterCoder$userId=="5714315743068160",1,0)	
    InterCoder$user_k<-ifelse(InterCoder$userId=="5727270706610176",1,0)	
    InterCoder$user_l<-ifelse(InterCoder$userId=="5745493850193920",1,0)	
    InterCoder$user_m<-ifelse(InterCoder$userId=="5759266065481728",1,0)	
    InterCoder$user_n<-ifelse(InterCoder$userId=="6125487189393408",1,0)	
    InterCoder$user_o<-ifelse(InterCoder$userId=="6246801829003264",1,0)	
    InterCoder$user_p<-ifelse(InterCoder$userId=="6596511420907520",1,0)	
    
# Aggregate data by cameo, word, user
    InterCoder <-aggregate(InterCoder, by=list(InterCoder$cameoId,InterCoder$wordId,InterCoder$userId), FUN=sum, na.rm=TRUE)
    InterCoder <-data.frame(InterCoder)
    
# Generate proportion correct
    InterCoder$total<-(InterCoder$ic+InterCoder$c+InterCoder$a)
    InterCoder$correct<-(InterCoder$c/InterCoder$total)
    InterCoder$incorrect<-(InterCoder$ic/InterCoder$total)
    InterCoder$ambiguous<-(InterCoder$a/InterCoder$total)
    
# Generar ID for merge using Cameo and Word IDs
    InterCoder$CameoWordID<-(InterCoder$cameoId+InterCoder$wordId)
    InterCoder<-InterCoder[order(InterCoder$CameoWordID),]

#################################################          
# Generate data subsets by user (alpha version)
    data_a<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_a>0)
    data_b<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_b>0)
    data_c<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_c>0)
    data_d<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_d>0)
    data_e<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_e>0)
    data_f<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_f>0)
    data_g<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_g>0)
    data_h<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_h>0)
    data_i<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_i>0)
    data_j<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_j>0)
    data_k<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_k>0)
    data_l<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_l>0)
    data_m<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_m>0)
    data_n<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_n>0)
    data_o<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_o>0)
    data_p<-subset(InterCoder, select=c(CameoWordID,ic,c,a,incorrect,correct,ambiguous,total),user_p>0)

# Declare as data frame    
    data_a<-data.frame(data_a)
    data_b<-data.frame(data_b)
    data_c<-data.frame(data_c)
    data_d<-data.frame(data_d)
    data_e<-data.frame(data_e)
    data_f<-data.frame(data_f)
    data_g<-data.frame(data_g)
    data_h<-data.frame(data_h)
    data_i<-data.frame(data_i)
    data_j<-data.frame(data_j)
    data_k<-data.frame(data_k)
    data_l<-data.frame(data_l)
    data_m<-data.frame(data_m)
    data_n<-data.frame(data_n)
    data_o<-data.frame(data_o)
    data_p<-data.frame(data_p)
    
# change variable names per database
    colnames(data_a)<-c("CameoWordID", "ic.a", "c.a", "a.a", "incorrect.a", "correct.a", "ambiguous.a", "total.a")
    colnames(data_b)<-c("CameoWordID", "ic.b", "c.b", "a.b", "incorrect.b", "correct.b", "ambiguous.b", "total.b")
    colnames(data_c)<-c("CameoWordID", "ic.c", "c.c", "a.c", "incorrect.c", "correct.c", "ambiguous.c", "total.c")
    colnames(data_d)<-c("CameoWordID", "ic.d", "c.d", "a.d", "incorrect.d", "correct.d", "ambiguous.d", "total.d")
    colnames(data_e)<-c("CameoWordID", "ic.e", "c.e", "a.e", "incorrect.e", "correct.e", "ambiguous.e", "total.e")
    colnames(data_f)<-c("CameoWordID", "ic.f", "c.f", "a.f", "incorrect.f", "correct.f", "ambiguous.f", "total.f")
    colnames(data_g)<-c("CameoWordID", "ic.g", "c.g", "a.g", "incorrect.g", "correct.g", "ambiguous.g", "total.g")
    colnames(data_h)<-c("CameoWordID", "ic.h", "c.h", "a.h", "incorrect.h", "correct.h", "ambiguous.h", "total.h")
    colnames(data_i)<-c("CameoWordID", "ic.i", "c.i", "a.i", "incorrect.j", "correct.i", "ambiguous.i", "total.i")
    colnames(data_j)<-c("CameoWordID", "ic.j", "c.j", "a.j", "incorrect.i", "correct.j", "ambiguous.j", "total.j")
    colnames(data_k)<-c("CameoWordID", "ic.k", "c.k", "a.k", "incorrect.k", "correct.k", "ambiguous.k", "total.k")
    colnames(data_l)<-c("CameoWordID", "ic.l", "c.l", "a.l", "incorrect.l", "correct.l", "ambiguous.l", "total.l")
    colnames(data_m)<-c("CameoWordID", "ic.m", "c.m", "a.m", "incorrect.m", "correct.m", "ambiguous.m", "total.m")
    colnames(data_n)<-c("CameoWordID", "ic.n", "c.n", "a.n", "incorrect.n", "correct.n", "ambiguous.n", "total.n")
    colnames(data_o)<-c("CameoWordID", "ic.o", "c.o", "a.o", "incorrect.o", "correct.o", "ambiguous.o", "total.o")
    colnames(data_p)<-c("CameoWordID", "ic.p", "c.p", "a.p", "incorrect.p", "correct.p", "ambiguous.p", "total.p")
    
# Merge datasets
    InterCoder2<- merge(data_a,data_b, by="CameoWordID", all=T)
    InterCoder2<- merge(InterCoder2,data_c, by="CameoWordID", all=T)
    InterCoder2<- merge(InterCoder2,data_d, by="CameoWordID", all=T)
    InterCoder2<- merge(InterCoder2,data_e, by="CameoWordID", all=T)
    InterCoder2<- merge(InterCoder2,data_f, by="CameoWordID", all=T)
    InterCoder2<- merge(InterCoder2,data_g, by="CameoWordID", all=T)
    InterCoder2<- merge(InterCoder2,data_h, by="CameoWordID", all=T)
    InterCoder2<- merge(InterCoder2,data_i, by="CameoWordID", all=T)
    InterCoder2<- merge(InterCoder2,data_j, by="CameoWordID", all=T)
    InterCoder2<- merge(InterCoder2,data_k, by="CameoWordID", all=T)
    InterCoder2<- merge(InterCoder2,data_l, by="CameoWordID", all=T)
    InterCoder2<- merge(InterCoder2,data_m, by="CameoWordID", all=T)
    InterCoder2<- merge(InterCoder2,data_n, by="CameoWordID", all=T)
    InterCoder2<- merge(InterCoder2,data_o, by="CameoWordID", all=T)
    InterCoder2<- merge(InterCoder2,data_p, by="CameoWordID", all=T)
    

# List variables    
    ls(InterCoder2)
    
#################################################      
# Calculate average proportion of correct verdicts across coders by CameoWordID    
    coders_correct<-subset(InterCoder2, select=c(correct.a,correct.b,correct.c,correct.d,correct.e,correct.f,correct.g,correct.h,correct.i,correct.j,correct.k,correct.l,correct.m,correct.n,correct.o,correct.p))
    coders_correct<-data.frame(coders_correct)
    coders_correct$mean<- rowMeans(coders_correct, na.rm = TRUE, dims = 1)
    
    # Exploratory Graphs - proportion of correct 
    hist(coders_correct$mean)
    plot(coders_correct$mean)    
    summary(coders_correct$mean)
    
# Calculate average proportion of incorrect verdicts across coders by CameoWordID    
    coders_incorrect<-subset(InterCoder2, select=c(incorrect.a,incorrect.b,incorrect.c,incorrect.d,incorrect.e,incorrect.f,incorrect.g,incorrect.h,incorrect.i,incorrect.j,incorrect.k,incorrect.l,incorrect.m,incorrect.n,incorrect.o,incorrect.p))
    coders_incorrect<-data.frame(coders_incorrect)
    coders_incorrect$mean<- rowMeans(coders_incorrect, na.rm = TRUE, dims = 1)
    
    # Exploratory Graphs - proportion of incorrect 
    hist(coders_incorrect$mean)
    plot(coders_incorrect$mean)    
    summary(coders_incorrect$mean)    
    
# Calculate average proportion of ambiguous verdicts across coders by CameoWordID    
    coders_ambiguous<-subset(InterCoder2, select=c(ambiguous.a,ambiguous.b,ambiguous.c,ambiguous.d,ambiguous.e,ambiguous.f,ambiguous.g,ambiguous.h,ambiguous.i,ambiguous.j,ambiguous.k,ambiguous.l,ambiguous.m,ambiguous.n,ambiguous.o,ambiguous.p))
    coders_ambiguous<-data.frame(coders_ambiguous)
    coders_ambiguous$mean<- rowMeans(coders_ambiguous, na.rm = TRUE, dims = 1)
    
    # Exploratory Graphs - proportion of ambiguous 
    hist(coders_ambiguous$mean)
    plot(coders_ambiguous$mean)    
    summary(coders_ambiguous$mean)
    
# Generate plot and correlation for average correct and incorrect across coders    
    plot(coders_incorrect$mean,coders_correct$mean)
    cor(coders_incorrect$mean,coders_correct$mean)
 
    
#################################################         
# Replicate Figure 7. 3D graph of correct, incorrect, ambiguous
    pdf("graphs/ambiguity3D_v1.pdf",width=6,height=6,paper='special') 
    scatter3D(coders_incorrect$mean, coders_correct$mean,coders_ambiguous$mean, 
              bty = "b2",  pch = 1, cex=2 , lwd = 3, theta = 60, phi = 0, 
              col = ramp.col(c("cyan3", "blue", "red")), 
              xlab="Incorrect", ylab="Correct", zlab= "Ambiguous",
              clab=c("Ambiguity"), colkey = list(length = 0.5))
    dev.off()

    
    
#####################################################################
#####################################################################
# Additional analysis
    
    
##################################################
# IDENTIFY AMBIGUOUS WORDS

# Merge words and verdicts
Word.ambiguous <- merge(SynsetVerdict, SynsetWord, by="submissionId")
# Subset data selecting variables
Word.ambiguous <-  Word.ambiguous[,c("verdict", "word")]
# Sort data
Word.ambiguous <- Word.ambiguous[order(Word.ambiguous$verdict, Word.ambiguous$word),]
# Subset data only ambiguous
Word.ambiguous <- subset(Word.ambiguous, verdict=='a')  
  
Word.ambiguous



    
########################
# End of script    
########################