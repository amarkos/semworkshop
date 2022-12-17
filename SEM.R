#############################################
#####    Μοντελοποίηση Δομικών Εξισώσεων     #####
#####      με το πακέτο lavaan            #####
#####         Εισαγωγή στην R             #####
#############################################

# If Greek fonts don't display properly, try
# File --> Reopen with Encoding... 
# and change to ISO-8859-7

# Εγκατάσταση απαραίτητων πακέτων
install.packages(c("foreign","lavaan","MPsychoR","psych","corrplot","semPlot","MVN","semTools"))

# Δημιουργία διανύσματος με τη συνάρτηση c()
newData1 <- c(4,5,3,6,9)
newData1 

# Ανάγνωση αρχείου .csv από το δίσκο
# και αποθήκευσή του ως data.frame
newData2 <- read.csv(file.choose())

# Ανάγνωση αρχείου .txt από το δίσκο
# και αποθήκευσή του ως data.frame
newData3 <- read.table(file.choose(), header = TRUE)

# Ανάγνωση αρχείων SPSS με χρήση συνάρτησης από το πακέτο 'foreign'
library(foreign)
# αν το πακέτο δεν έχει εγκατασταθεί τότε εγκαταστήστε με install.package("foreign")
# αναζήτηση βοήθειας για τη συνάρτηση
?read.spss

# πρέπει να ζητήσουμε να μετατραπεί σε data.frame
newData.spss <- read.spss(file.choose(), to.data.frame = TRUE)

# φόρτωση του πακέτου στο περιβάλλον της R
library(MPsychoR)
# φόρτωση του συνόλου δεδομένων
data(Bergh)
# βοήθεια για το σύνολο δεδομένων
?Bergh

# Περιγραφική ανάλυση

# ας δούμε τα δεδομένα
View(Bergh)
# ας δούμε το μέγεθος του data.frame
dim(Bergh)
# ας δούμε τη δομή του συνόλου δεδομένων
str(Bergh)

library(psych)
?describe
# Βασικοί δείκτες της περιγραφικής στατιστικής, χρήσιμοι στην ψυχομετρία
describe(Bergh)

# συχνότητες εμφάνισης τιμών της μεταβλητής Gender
table(Bergh$gender) 

# συσχετίσεις Pearson (εξαιρούμε την 11η στήλη - Gender)
cor(Bergh[,-11])

# στρογγυλοποίηση σε δύο δεκαδικά
cormat <- round(cor(Bergh[,-11]),2)

# διάγραμμα του πίνακα συσχετίσεων
corrplot(cormat,type = 'lower')

attach(Bergh)
# Μέγεθος δείγματος
nrow(Bergh)
## [1] 861

## Δημιουργία σύνθετων μεταβλητών
Bergh$Open <- (O1+O2+O3)/3
Bergh$Agree <- (A1+A2+A3)/3
Bergh$Prejudice <- (EP+SP+DP+HP)/4

#### Model 1: Μοντέλο γραμμικής παλινδρόμησης με δύο ανεξάρτητες μεταβλητές
#### Καθορισμός, εκτίμηση και αξιολόγηση της προσαρμογής του μοντέλου

# Βήμα 1: Καθορισμός του μοντέλου
model1 <- '
# Structural model
Prejudice ~ b1*Open + b2*Agree
# Covariance structure of exogenous variables
Open ~~ Open + Agree'

# Βήμα 2: Εκτίμηση του μοντέλου
model1.fit <- sem(model1,
                  data = Bergh,
                  meanstructure = FALSE,
                  estimator = "ML")

# Βήμα 3: Αξιολόγηση του μοντέλου
summary(model1.fit,
        rsquare = TRUE,
        fit.measures = TRUE,
        standardized = TRUE)

## Έλεγχος υπόθεσης διαφοράς των b1 και b2
lavTestWald(model1.fit, constraints = "b1 == b2")

library(semPlot)
# Διάγραμμα
semPaths(model1.fit,
         what = "std", edge.label.cex = 0.7, esize = 1, intercepts = FALSE, rotation = 4, 
         edge.color = 1, asize = 2.5, sizeMan = 5, mar = c(1, 1.5, 1.5, 3), fade = FALSE)

# Ο πίνακας διακύμανσης - συνδιακύμανσης του μοντέλου
fitted(model1.fit)

# Τα υπόλοιπα του μοντέλου (just-identified)
resid(model1.fit)

# Ο αρχικός πίνακας διακύμανσης - συνδιακύμανσης
cov(cbind(Bergh$Prejudice,Bergh$Open,Bergh$Agree))

# Οι (τυποποιημένες) τιμές των παραμέτρων του μοντέλου
parameterEstimates(model1.fit, standardized = TRUE)

####  Model 2: Μοντέλο μεσολάβησης με παρατηρούμενες μεταβλητές

# Βήμα 1: Καθορισμός του μοντέλου
model2 <- '
# Structural model
Prejudice ~ b1*Open + b2*Agree
Open ~ b3*Agree
# New parameters 
# indirect effect
ind := b1*b3
# total effect
total := b2 + (b1*b3)
'
# Βήμα 2: Εκτίμηση του μοντέλου
model2.fit <- sem(model2,
                  data = Bergh,
                  estimator = "ML")

# Βήμα 3: Αξιολόγηση του μοντέλου
summary(model2.fit,
        rsquare = TRUE,
        fit.measures = TRUE,
        standardized = TRUE)



# Εκτίμηση του μοντέλου με χρήση bootstrapping
model2.fitB <- sem(model2,
                   data = Bergh,
                   estimator = "ML",
                   se = "bootstrap")

# Αξιολόγηση του μοντέλου
summary(model2.fitB,
        rsquare = TRUE,
        fit.measures = TRUE,
        standardized = TRUE)

semPaths(model2.fit,what = "std", edge.label.cex = 0.7, esize = 1, intercepts = FALSE, rotation = 4, 
         edge.color = 1, asize = 2.5, sizeMan = 5, mar = c(1, 1.5, 1.5, 3), fade = FALSE)

##### Μοντέλο μέτρησης - CFA
#############################

# Πίνακας συσχετίσεων (Correlation matrix)
Bergh.cor <- cor(Bergh[,1:10], method = "pearson", use = "pairwise.complete.obs")
Bergh.cor

# Διάγραμμα του πίνακα συσχετίσεων (Correlogram)
corrplot(Bergh.cor, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 60,
         addCoef.col = "white",
         number.cex = 0.75,
         cl.cex = 1,
         tl.cex = 0.9)

# Βήμα 1: Καθορισμός του μοντέλου
model3a <- '
# Measurement models
OP =~ O1 + O2 + O3
AG =~ A1 + A2 + A3
PR =~ EP + SP + HP + DP
# Covariance structure
OP ~~ OP + AG + PR
AG ~~ AG + PR
'

# Βήμα 2: Εκτίμηση του μοντέλου
model3a.fit <- sem(model3a,
                   data = Bergh,
                   estimator = "ML")


# Βήμα 3: Αξιολόγηση του μοντέλου
summary(model3a.fit,
        rsquare = TRUE,
        fit.measures = TRUE,
        standardized = TRUE)


# Διάγραμμα 
semPaths(model3a.fit,
         what = "std", edge.label.cex = 0.7, esize = 1, intercepts = FALSE, rotation = 4, 
         edge.color = 1, asize = 2.5, sizeMan = 5, mar = c(1, 1.5, 1.5, 3), fade = FALSE)

# Δείκτες τροποποίησης 
modindices(model3a.fit,sort = TRUE, minimum.value = 10)

## ----- MODEL 3B
##### Τροποποίηση του μοντέλου 3A

# Βήμα 1: Καθορισμός του μοντέλου
model3b <- '
# Measurement models
OP =~ O1 + O2 + O3
AG =~ A1 + A2 + A3
PR =~ EP + SP + HP + DP
# Covariance structure
OP ~~ OP + AG + PR
AG ~~ AG + PR
# Residual covariance
A1 ~~ A3
'
# Βήμα 2: Εκτίμηση του μοντέλου
model3b.fit <- sem(model3b,
                   data = Bergh,
                   estimator = "ML")


# Βήμα 3: Αξιολόγηση του μοντέλου
summary(model3b.fit,
        rsquare = TRUE,
        fit.measures = TRUE,
        standardized = TRUE)

# Διάγραμμα 
semPaths(model3b.fit, what = "std", edge.label.cex = 0.7, esize = 1, intercepts = FALSE, rotation = 4, 
         edge.color = 1, asize = 2.5, sizeMan = 5, mar = c(1, 1.5, 1.5, 3), fade = FALSE)


## Σύγκριση των μοντέλων: Model 3 vs. refined Model 3
anova(model3a.fit, model3b.fit)

# Δείκτες τροποποίησης 
modindices(model3b.fit, sort = TRUE,minimum.value = 10)


##### Το μοντέλο των Bergh et at. 
#############################

model4 <- '# Measurement models
          OP =~ O1 + O2 + O3
          AG =~ A1 + A2 + A3
          PR =~ EP + SP + HP + DP
          # Residual covariance
          A1 ~~ A3
          # Structural model
          PR ~ b1*OP + b2*AG'

model4.fit <- sem(model4, data = Bergh, estimator = "ML")

summary(model4.fit, standardized = TRUE, fit.measures = TRUE,rsquare = TRUE)

#Άλλοι δείκτες προσαρμογής
fitmeasures(model4.fit)

semPaths(model4.fit, what = "std", edge.label.cex = 0.7, esize = 1, intercepts = FALSE, rotation = 4, 
         edge.color = 1, asize = 2.5, sizeMan = 5, mar = c(1, 1.5, 1.5, 3), fade = FALSE)

# Έλεγχος υπόθεσης διαφοράς των b1 και b2
lavTestWald(model4.fit, constraints = "b1 == b2")


#########
# Έλεγχος πολυμεταβλητής κανονικότητας
library(MVN)
mvn(Bergh[,1:10],mvnTest = "mardia",multivariatePlot = "qq")

# Προσαρμογή του μοντέλου
# με το διορθωμένο στατιστικό χ2 κατά Yuan-Bentler και ανθεκτικά τυπικά σφάλματα
model4.fit <- sem(model4, data = Bergh, estimator = "MLR")
#εναλλακτικά
#MLM για Satorra-Bentler

summary(model4.fit, standardized = TRUE, fit.measures = TRUE,rsquare = TRUE)

semPaths(model4.fit, what = "std", edge.label.cex = 0.7, esize = 1, intercepts = FALSE, rotation = 4, 
         edge.color = 1, asize = 2.5, sizeMan = 5, mar = c(1, 1.5, 1.5, 3), fade = FALSE)

