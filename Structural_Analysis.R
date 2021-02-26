rm(list = ls())
options(scipen = 999)
setwd("C:\\Users\\AXIOM\\Desktop\\data\\PD_Analysis")

# libraries & functions:
GetMode = function(D){
  UniqD = unique(D)
  UniqD[which.max(tabulate(match(D,UniqD)))]
}

# data from : 
#======> https://code.datasciencedojo.com/datasciencedojo/datasets/raw/master/Default%20of%20Credit%20Card%20Clients/default%20of%20credit%20card%20clients.csv
data = read.csv("https://code.datasciencedojo.com/datasciencedojo/datasets/raw/master/Default%20of%20Credit%20Card%20Clients/default%20of%20credit%20card%20clients.csv",
                header = T,stringsAsFactors = F)

for(i in 1:ncol(data)){
  colnames(data)[i] = paste0(data[1,i])
  print(paste0("fixing col :",colnames(data)[i]))
}
data = data[2:nrow(data),]

#====== data structure ====>
data_structure = data.frame()
for(i in 1:ncol(data)){
  datum = data.frame(col_name = colnames(data)[i],
                     entry_type = typeof(data[,i]),
                     unique_values = length(unique(data[,i])),
                     unique_entries = ifelse(length(unique(data[,i])) < 6,paste(unique(data[,i]),collapse = "-"),"Too Many Entries"),
                     na_ = sum(is.na(data[,i])),
                     null_ = sum(is.null(data[,i])),
                     nan_ = sum(is.nan(data[,i])),
                     mean_ = ifelse(typeof(data[,i]) != "character",mean(data[,i],na.rm = T),"Not Applicable"),
                     median_ = ifelse(typeof(data[,i]) != "character",median(data[,i],na.rm = T),"Not Applicable"),
                     mode_ = GetMode(data[,i]),stringsAsFactors = F)
  
  data_structure = rbind.data.frame(data_structure,datum,make.row.names = F)
  print(paste0("decomposing column :",colnames(data)[i]))
}

# NOT RUN{
"Columns LIMIT_BAL, AGE, PAY_0, PAY_2, PAY_3, 
PAY_4, PAY_5, PAY_6, BILL_AMT1, BILL_AMT2, BILL_AMT3, 
BILL_AMT4, BILL_AMT5, BILL_AMT6, PAY_AMT1, PAY_AMT2, PAY_AMT3,
PAY_AMT4 ,PAY_AMT5, PAY_AMT6, default payment next month 
has type character and must be transformed from character to numeric "
# }

#===== trnasforming cols: character --> numeric ====> 
cols_list = colnames(data)[c(2,6:25)]
for(i in 1:length(cols_list)){
  data[,cols_list[i]] = as.numeric(data[,cols_list[i]])
}

# NOT RUN{
" re-run code block 23-38 to obtain the structural summary of the modified data "
# }

#=== write --> csv
write.csv(data_structure,
          file = paste0("data_structure-",format(Sys.Date(),"%d-%m-%Y"),".csv"))
