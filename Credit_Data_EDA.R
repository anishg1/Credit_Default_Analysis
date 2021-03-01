rm(list=ls())

options(warn = -1)

library(reshape2)

data1 = data

## Proportion of defaults distributed across different groups of different variables ##

prop = function(data_input,var){
  tryCatch({
    
    ## Proportion of defaults distributed across different groups of different variables
    
    prop1 = as.data.frame(round(prop.table(table(data_input[,var],data_input[,"default payment next month"]))*100,2))
    prop1 = as.data.frame(dcast(prop1,Var1 ~ Var2,value.var = "Freq"))
    names(prop1) = c(var,"Default = 0","Default = 1")
    prop1[,1] = as.character(prop1[,1])
    prop1 = rbind(prop1,c("Total",sum(prop1[,2]),sum(prop1[,3])))
    
    ## Condition for handling variables with less than or equal to 2 categories
    
    if(length(unique(data_input[,var])) <= 2){
      print(paste0("The variable ",var," has only ",length(unique(data_input[,var]))," categories"))
      break()
    }
    else{
      prop2 = prop1[-nrow(prop1),]
      prop2[,c(2:3)] <- lapply(prop2[,c(2:3)], function(x) as.numeric(as.character(x)))
      
      ## Setting the cut-off value to determine which categories need to be clubbed
      
      crit1 = ifelse(is.infinite(min(prop2[,2][prop2[,2] > 2 & prop2[,2] < 5]))==T,0,min(prop2[,2][prop2[,2] > 2 & prop2[,2] < 5]))
      crit2 = ifelse(is.infinite(min(prop2[,3][prop2[,3] > 2 & prop2[,3] < 5]))==T,0,min(prop2[,3][prop2[,3] > 2 & prop2[,3] < 5]))
      
      cut_off_val = ifelse((crit1 == 0 | crit2 == 0) & (crit1 != crit2),max(crit1,crit2),ifelse(crit1 == 0 && crit2 == 0,5,min(crit1,crit2)))
      
      prop2$club_0 = ifelse(prop2[,2] < cut_off_val,1,0)
      prop2$club_1 = ifelse(prop2[,3] < cut_off_val,1,0)
      
      ## Clubbing criteria implementation
      
      for(i in 1:(nrow(prop2)-1)){
        if(prop2$club_0[i] == 1 & prop2$club_0[i] == prop2$club_0[i+1]){
          prop2$club_flag[i] = print(paste0(prop2[,1][i],"-",prop2[,1][i+1]))
          prop2$club_flag[i+1] = print(paste0(prop2[,1][i],"-",prop2[,1][i+1]))
        }
        else{
          if(prop2$club_0[i] == 1 & prop2$club_0[i] != prop2$club_0[i+1]){
            prop2$club_flag[i] = print(paste0(prop2[,1][i],"-",prop2[,1][i+1]))
            prop2$club_flag[i+1] = print(paste0(prop2[,1][i],"-",prop2[,1][i+1]))
          }
          else{
            if(prop2$club_0[i] == 0 & prop2$club_0[i] != prop2$club_0[i+1]){
              prop2$club_flag[i] = "No club"
              prop2$club_flag[i+1] = print(paste0(prop2[,1][i],"-",prop2[,1][i+1]))
            }
            else{
              prop2$club_flag[i] = print("No club")
            }
          }
        }
      }
      
      club = unique(prop2$club_flag[prop2$club_0==1])
      
      club_list = strsplit(club,"-")
      for(i in 1:(length(club_list)-1)){
        if(club_list[[i]][2] == club_list[[i+1]][1]){
          club_list[[i]] = c(club_list[[i]],club_list[[i+1]][2])
          club_list[[i+1]] = c(club_list[[i]][1],club_list[[i+1]])
        }
      }
      club_list = unique(club_list)
      
      for(i in 1:length(club_list)){
        if(length(club_list[[i]]) == 2){
          data_input[,var][data_input[,var] == club_list[[i]][2]] = club_list[[i]][1]
        }
        else{
          for(j in 2:length(club_list[[i]])){
            data_input[,var][data_input[,var] == club_list[[i]][j]] = club_list[[i]][1]
          }
        }
      }
      
      dat_frame = NULL
      for(i in 1:length(club_list)){
        a = paste0("The ",var," categories which need to be clubbed : ",paste(club_list[[i]],collapse = "-"))
        dat_frame = rbind(dat_frame,a)
      }
      dat_frame = data.frame(Clubbing_Criteria = dat_frame)
      rownames(dat_frame) = NULL
      
    } 
    
    ## Proportion of defaults distributed across different groups of different variables (clubbed)
    
    prop3 = as.data.frame(round(prop.table(table(data_input[,var],data_input[,"default payment next month"]))*100,2))
    prop3 = as.data.frame(dcast(prop3,Var1 ~ Var2,value.var = "Freq"))
    names(prop3) = c(var,"Default = 0","Default = 1")
    prop3[,1] = as.character(prop3[,1])
    prop3 = rbind(prop3,c("Total",sum(prop3[,2]),sum(prop3[,3])))
    
    final_list = list(prop1,dat_frame,prop3)
    
    return(final_list)
  }, error = function(e) {NULL})
}

edu_prop_prev = prop(data1,"EDUCATION")[[1]]
edu_prop_club_crit = prop(data1,"EDUCATION")[[2]]
edu_prop_new = prop(data1,"EDUCATION")[[3]]
