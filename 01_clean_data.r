#clean up the data frame, filter out
rwanda_data <- readRDS(rwanda_data, file = "/path_to_file/rwanda_data.rds") #read in dataframe from last script
rwanda_data1 <- rwanda_data #make copy to edit

#clean up rdt variable (hml35)

#set 6 (other) and 9(missing) to NA 
rwanda_data1$hml35[which(rwanda_data1$hml35 == 6)] = NA
rwanda_data1$hml35[which(rwanda_data1$hml35 == 9)] = NA

#intial look at data
length(which(rwanda_data1$hml35 ==1)) #594 positives


#looking at barcode/id variables to link to physical samples
#going based on hiv weights (that have nas), fill in weird blanks in barcode variables with NAs 
rwanda_data1$hb62[which(is.na(rwanda_data1$hb69))] <- NA
rwanda_data1$ha62[which(is.na(rwanda_data1$ha69))] <- NA

#check to see if this is what we want
View(cbind(rwanda_data1$ha69,  rwanda_data1$ha62,rwanda_data1$hb69, rwanda_data1$hb62))

#combine the 2 separate columns into 1--women and men have different barcode variables 
for(i in 1:nrow(rwanda_data1)){
  if(is.na(rwanda_data1$ha62[i]) == F){#if there is a barcode in ha62, set that as the barcode
    rwanda_data1$barcode[i] = rwanda_data1$ha62[i]
  }
  else{
    rwanda_data1$barcode[i] = rwanda_data1$hb62[i] 
  }
}

#check that it did what we want
View(cbind(rwanda_data1$ha62, rwanda_data1$hb62, rwanda_data1$barcode)) #can see that this merged the 2 columns


#saveRDS(rwanda_data1, file = "/path_to_file/rwanda_data1.rds") #save file

#make a new malaria variable
#if either hml32 or hml35 is 1 --> 1
#denominator--> number of people who received either test

#if both NA--set to NA
#if either is 1--set to 1
#else--set to 0 (both or one are 0)

for(i in 1:nrow(rwanda_data1)){
  if(is.na(rwanda_data1$hml35[i]) & is.na(rwanda_data1$hml32[i]) == TRUE){ #if both are NA
    rwanda_data1$malaria[i] <- NA #set to NA
  }
  #if one is NA and the other isn't, take non-na value
  else if(is.na(rwanda_data1$hml35[i]==T)){#if only hml35 is NA
    rwanda_data1$malaria[i] <- rwanda_data1$hml32[i] #take value from hml32
  }
  else if(is.na(rwanda_data1$hml32[i]==T)){#if only hml32 is NA
    rwanda_data1$malaria[i] <- rwanda_data1$hml35[i] #take value from hml35
    
  }
  #if neither are NA, take positive if there is one
  else if(rwanda_data1$hml35[i] == 1){#if rdt is positive
    rwanda_data1$malaria[i] <- 1 #set to positive
  }
  else if(rwanda_data1$hml32[i] == 1){#if microscopy is positive
    rwanda_data1$malaria[i] <- 1 #set to positive
  }
  else{ #otherwise (if both are 0)
    rwanda_data1$malaria[i] <- 0 #set to negative
  }
}


View(cbind(rwanda_data1$hml35,rwanda_data1$hml32,rwanda_data1$malaria))#check and see if it did what we want--yes it did!
table(rwanda_data1$malaria) #table confirms 620 positives which is what was expected

#link together HIV dataset and household member recode
#cluster, household, and line number

rwanda_hiv_data <- readRDS(rwanda_hiv_data, file = "/path_to_file/rwanda_hiv_data.rds") #read in hiv dataframe 

rwanda_hiv_data$unique_id <- paste(rwanda_hiv_data$hivclust,rwanda_hiv_data$hivnumb,rwanda_hiv_data$hivline, sep = ",")

rwanda_data1$unique_id <- paste(rwanda_data1$hv001,rwanda_data1$hv002,rwanda_data1$hvidx, sep = ",")

#want to transfer over hiv05(sample weight), hiv03(blood test result), hiv02 (lab number), hiv01 (barcode)

rwanda_data4 <- left_join(rwanda_data1, rwanda_hiv_data, by="unique_id")

#saveRDS(rwanda_data4, file = "/path_to_file/rwanda_data4.rds") #save file



