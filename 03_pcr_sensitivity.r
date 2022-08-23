#sensitivity and specificity of pcr approach

data <- read.csv(file = "/path_to_file/qpcr_gel_ct.csv") #file including qpcr and gel data, in repository

length(which(data$Ct > 37)) #total negatives = 143
length(which(data$Ct > 37 & data$Band.intensity == 0)) # true negatives = 114
length(which(data$Ct < 37 & data$Band.intensity == 0)) #false negatives = 120
length(which(data$Ct > 37 & data$Band.intensity > 0)) # false positives = 29
length(which(data$Ct < 37)) # total positives = 217 
length(which(data$Ct < 37 & data$Band.intensity > 0)) #true positives = 95

#values inputed into spreadsheet
#specificity = true negative / true negative + false positive
#sensitivity = true positive / total positive