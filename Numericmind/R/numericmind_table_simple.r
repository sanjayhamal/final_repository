#' package: Numericmind
#' Currently, it generates a normal table, a stratified table and summary of a filtered table from a whole table.
#' 1st function: nm_table()
#' syntax: nm_table(dataset, summ_of, summ_by, Count_cat, Counts, Mean, Median, SD, Min_val, Max_val)

#' dataset: It is the table that you want to summarize.
#' summ_of: It is the collection of variables that you want to summarize: c(" ",  )
#' summ_by: It is the variable by which you summarize: " "
#' The rest of the syntax can be turned on by "Y" and off by "N"
#' Important notes:

#' For nm_table(), if you are creating summary table for a numeric type variable(continuous variable),
#' make sure to omit the NA values before using the function. If the table only consists of categorical variables, there is no need to omit the NA values.

#' @export


nm_table<- function(dataset, summ_of, summ_by, Count_cat, Counts, Mean, Median, SD, Min_val, Max_val){

  #remove na values or any unwanted rows that you do not want to summarize:
  x1<- dataset
  #x1<- na.omit(x1)
  x1

  #now creating layout of data frame on the basis of column by which you want to summarize:
  x1$summarize_by = ""
  x1


  x1[, 'summarize_by'] = x1[,summ_by]
  x1

  summary_col<- c(unique(x1$summarize_by))
  summary_col

  summary_col_count<- vector()
  for (i in seq(length(summary_col))){

    summary_col_count[i]<- nrow(x1 %>% filter(summarize_by == summary_col[i]))

  }
  summary_col_count

  summary_col_updated<- vector()

  for( i in seq(length(summary_col))){
    summary_col_updated[i]<- paste(summary_col[i], "(N=", as.numeric((summary_col_count[i])), ")", sep = "")
  }
  summary_col_updated[length(summary_col_updated)+1] = paste("Total ", "(N=", sum(as.numeric(summary_col_count)), ")", sep = "" )
  summary_col_updated

  x2<- c(summ_by, 'summarize_by')
  x3<- x1[, !(names(x1) %in% x2)]

  col_names<- c(colnames(x3))

  final_df<- data.frame(summarized_cols = "")


  for(i in seq(length(summary_col_updated))){
    final_df[summary_col_updated[i]] = ""
  }

  #--------------------------------------------------------------------------------------------
  #start loop here for column names:-----------------------------------------------------------
  for(j in seq(length(summ_of))){


    fm= summ_of[j]

    individual<- x1[, c(fm, 'summarize_by')]

    individual[, 'b'] = individual[, fm]









    #for continuous variables------------------------------------------------------------------

    if(is.numeric(individual$b)==T){



      ex1<- data.frame(summarized_cols = fm)
      for(i in seq(length(summary_col_updated))){
        ex1[summary_col_updated[i]] = ""
      }


      if (Counts=='Y'){
        ex1<- ex1 %>% add_row("summarized_cols" = 'n')
        for (i in seq(length(summary_col))){
          ex2<- individual %>% filter(summarize_by==summary_col[i])
          ex3<- nrow(ex2)
          ex4<- which(ex1$summarized_cols== 'n')
          ex1[ex4, summary_col_updated[i]] = ex3
        }
        ex5<- nrow(individual)
        ex1[ex4, summary_col_updated[length(summary_col_updated)]] = ex5


      }



      #Mean
      if (Mean=='Y'){
        ex1<- ex1 %>% add_row("summarized_cols" = 'Mean')
        for (i in seq(length(summary_col))){
          ex2<- individual %>% filter(summarize_by==summary_col[i])
          ex3<- round(mean(ex2$b), digits = 1)
          ex4<- which(ex1$summarized_cols== 'Mean')
          ex1[ex4, summary_col_updated[i]] = ex3
        }
        ex5<- round(mean(individual$b), digits = 1)
        ex1[ex4, summary_col_updated[length(summary_col_updated)]] = ex5


      }


      #standard deviation
      if (SD=='Y'){
        ex1<- ex1 %>% add_row("summarized_cols" = 'SD')
        for (i in seq(length(summary_col))){
          ex2<- individual %>% filter(summarize_by==summary_col[i])
          ex3<- round(sd(ex2$b), digits = 2)
          ex4<- which(ex1$summarized_cols== 'SD')
          ex1[ex4, summary_col_updated[i]] = ex3
        }
        ex5<- round(sd(individual$b), digits = 1)
        ex1[ex4, summary_col_updated[length(summary_col_updated)]] = ex5

      }

      #Median
      if (Median=='Y'){
        ex1<- ex1 %>% add_row("summarized_cols" = 'Median')
        for (i in seq(length(summary_col))){
          ex2<- individual %>% filter(summarize_by==summary_col[i])
          ex3<- round(median(ex2$b), digits = 1)
          ex4<- which(ex1$summarized_cols== 'Median')
          ex1[ex4, summary_col_updated[i]] = ex3
        }
        ex5<- round(median(individual$b), digits = 1)
        ex1[ex4, summary_col_updated[length(summary_col_updated)]] = ex5

      }



      #Mode
      #if (Mode=='Y'){
      #ex1<- ex1 %>% add_row("summarized_cols" = 'Mode')
      #for (i in seq(length(summary_col))){
      #ex2<- individual %>% filter(summarize_by==summary_col[i])
      #ex3<- mode(ex2$b)
      #ex4<- which(ex1$summarized_cols== 'Mode')
      #ex1[ex4, summary_col_updated[i]] = ex3
      #}
      #ex5<- mode(individual$b)
      #ex1[ex4, summary_col_updated[length(summary_col_updated)]] = ex5

      #}






      #Minimum value
      if (Min_val=='Y'){
        ex1<- ex1 %>% add_row("summarized_cols" = 'Min.')
        for (i in seq(length(summary_col))){
          ex2<- individual %>% filter(summarize_by==summary_col[i])
          ex3<- round(min(ex2$b), digits = 2)
          ex4<- which(ex1$summarized_cols== 'Min.')
          ex1[ex4, summary_col_updated[i]] = ex3
        }
        ex5<- round(min(individual$b), digits = 2)
        ex1[ex4, summary_col_updated[length(summary_col_updated)]] = ex5

      }


      #Maximum value
      if (Max_val=='Y'){
        ex1<- ex1 %>% add_row("summarized_cols" = 'Max.')
        for (i in seq(length(summary_col))){
          ex2<- individual %>% filter(summarize_by==summary_col[i])
          ex3<- round(max(ex2$b), digits = 2)
          ex4<- which(ex1$summarized_cols== 'Max.')
          ex1[ex4, summary_col_updated[i]] = ex3
        }
        ex5<- round(max(individual$b), digits = 2)
        ex1[ex4, summary_col_updated[length(summary_col_updated)]] = ex5

      }
      #adding white space infront of names
      for(i in seq(2,nrow(ex1))){
        ex1[i, "summarized_cols"] = paste("   ", ex1[i, "summarized_cols"], sep = "")
      }

      #empty space:
      ex6<- data.frame(summarized_cols = " ")
      for(i in seq(length(summary_col_updated))){
        ex6[summary_col_updated[i]] = ""
      }
      ex1<- bind_rows(ex1, ex6)
      ex6
      final_df<- bind_rows(final_df, ex1)
    }








    #for categorical variables-----------------------------------------------------------------------------
    if(is.character(individual$b)==T){


      ex11<- data.frame(summarized_cols = fm)
      for(i in seq(length(summary_col_updated))){
        ex11[summary_col_updated[i]] = ""
      }




      if (Count_cat=='Y'){
        ex11<-ex11 %>%  add_row('summarized_cols' = 'n')
        for(i in seq(length(summary_col))){
          ex11a<- nrow(individual %>% filter(summarize_by==summary_col[i]))
          ex11b<- which(ex11$summarized_cols=='n')
          ex11[ex11b, summary_col_updated[i]] = ex11a
        }
        ex11c<- nrow(individual %>% select(b))
        ex11[ex11b, summary_col_updated[length(summary_col_updated)]] = ex11c
      }



      ex22<- individual %>% select(b) %>% distinct()
      ex23 = c(ex22$b)
      ex11<- ex11 %>% add_row('summarized_cols' = ex23 )


      for(i in seq(length(ex23))){
        ex24<- ex23[i]
        ex25<- individual %>% filter(b==ex24)
        total = 0
        for(k in seq(length(summary_col))){
          ex26<- nrow(ex25 %>% filter(summarize_by==summary_col[k]))
          ex27<- which(ex11$summarized_cols==ex24)



          total = total+ex26
          #calculating percentage as well:
          pct= (ex26/as.numeric(summary_col_count[k]))*100
          pct = round(pct, digits = 0)
          ex11[ex27, summary_col_updated[k]] = paste(ex26, " (", pct, "%)", sep = "")
        }


        pct_total = (total/(sum(summary_col_count)))*100
        pct_total = round(pct_total, digits = 0)
        ex11[ex27, summary_col_updated[length(summary_col_updated)]] = paste(total, " (", pct_total, "%)", sep = "")

      }

      #adding whitespace in front of the name of columns
      for(i in seq(2,nrow(ex11))){
        ex11[i, "summarized_cols"] = paste("   ", ex11[i, "summarized_cols"], sep = "")
      }

      #empty space:
      ex6<- data.frame(summarized_cols = " ")
      for(i in seq(length(summary_col_updated))){
        ex6[summary_col_updated[i]] = ""
      }

      ex11<- bind_rows(ex11, ex6)
      final_df<- bind_rows(final_df, ex11)


    }




  }

  final_df<-final_df %>% rename(" " = "summarized_cols")
  final_df<- final_df[-c(1, nrow(final_df)),  ]

  #setting row numbers correctly
  final_df$row_num= as.numeric("")
  for(i in seq(nrow(final_df))){
    final_df[i, "row_num"]= i
  }
  row.names(final_df) = final_df$row_num
  final_df<-final_df %>% select(-row_num)


  return(final_df)

}

