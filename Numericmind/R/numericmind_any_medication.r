#' 3rd function: any_medication()
#' syntax: any_medication(dataset, new,  summ_by, summ_of)

#' dataset: It is the whole imported dataset.
#' new:     It is the filtered dataset(a subset of whole dataset) that you want to summarize.
#' summ_by: It is the variable that you use to summarize the table: " "
#' summ_of: It is the collection of variables that you want to summarize: c(" ", " ", )
#' Important notes:

#' For any_medication(): make sure the table has a unique variable 'USUBJID'

#' @export
any_medication<- function(dataset, new,  summ_by, summ_of){
  library(tidyverse)
  library(haven)
  dataset1<-dataset %>% select(USUBJID, summ_by)
  dataset1$summarise_by = ""
  for (i in seq(nrow(dataset1))){
    dataset1[i, "summarise_by"] = dataset1[i, summ_by]
  }
  
  table_x<- dataset1 %>% select(USUBJID, summarise_by) %>% group_by(USUBJID) %>% distinct(summarise_by)
  
  c1<- c(unique(table_x$summarise_by))
  
  c_count = vector()
  for ( i in seq(length(c1))){
    c_count[i]<- nrow(table_x %>% filter(summarise_by==c1[i]))
    
  }
  
  colmn= vector()
  for (i in seq(length(c1))){
    colmn[i]<- paste(c1[i], "(N= ", as.numeric((c_count[i])), ")", sep = "")
  }
  
  colmn[length(colmn)+1] = paste('Total', "(N= ", sum(c_count), ")")
  for (i in seq(length(colmn))){
    new[colmn[i]]  = ""
  }
  new$summary_of = ""
  for(i in seq(nrow(new))){
    new[i, "summary_of"] = new[i, summ_of]
  }
  new$summarise_by = ""
  for(i in seq(nrow(new))){
    new[i, "summarise_by"] = new[i, summ_by]
  }
  
  #1st step: giving count of q2ms and dtg+rpv to each values:
  #what are the distinct values of adecod that we filtered?
  
  new_x<- new %>% group_by(summary_of) %>% distinct(summary_of)
  new_x1<- c(unique(new_x$summary_of))
  
  
  new_x2<- data.frame(give_name = new_x1)
  
  for ( i in seq(length(c1))){
    new_x2[c1[i]]= 0
  }
  
  
  for( i in seq(nrow(new))){
    val555<- which(new_x2$give_name==as.character(new[i, "summary_of"]))
    print(val555)
    val556<- as.character(new[i, "summarise_by"])
    new_x2[val555, val556] = new_x2[val555, val556]+1
    
  }
  for(i in seq(length(colmn))){
    new_x2[colmn[i]]= ""
  }
  new_x2<- new_x2 %>% add_row("give_name" = 'Any medication', .before = 1)
  
  for ( i in seq(length(c1))){
    new_x2[1, as.character(c1[i])]= 0
  }
  
  for (i in seq(length(c1))){
    vals<- new %>% filter(summarise_by==c1[i]) %>% group_by(USUBJID) %>% distinct(USUBJID)
    vals2<- nrow(vals)
    new_x2[1, c1[i]]= new_x2[1, c1[i]]+as.numeric(vals2)
    
  }
  
  
  new_x2[, ncol(new_x2)] = 0
  #after the layout has been created, give formula for final output:
  #here, we have to calculate the percentage:
  for(i in seq(nrow(new_x2))){
    for(j in seq(length(c1))){
      count_val = new_x2[i, as.character(c1[j])]
      percent_val<- round((as.numeric(count_val)/as.numeric(c_count[j]))*100, digits = 0)
      new_x2[i, as.character(colmn[j])] = paste(count_val, " ", "(", percent_val, "%", ")", sep = "")
      new_x2[i, ncol(new_x2)] = as.numeric(new_x2[i, ncol(new_x2)])+ as.numeric(count_val)
      
    }
    total_percent<- round((as.numeric(new_x2[i, ncol(new_x2)])/sum(c_count))*100, digits = 0)
    new_x2[i, ncol(new_x2)] = paste(new_x2[i, ncol(new_x2)], " ", "(", total_percent, "%)", sep = "")
    
  }
  
  new_x2<- new_x2 %>% select(-c1)
  new_x2<- new_x2 %>% rename(Ingredients = 'give_name')
  return(new_x2)
  
}
