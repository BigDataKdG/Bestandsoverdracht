
###########################################################################################
###########################################################################################
####################################-~_Opdracht #1-~-######################################
###########################################################################################
###########################################################################################

#_____________________________________Load packages_____________________________________#
{
  
  library("car")
  library("gtools")
  library("grid")
  library("gridExtra")
  library("gtable")

  
  # set home: home = "C:/Users/Fheylen/" of "~"
  wd <- paste(home,"/Dropbox/Bestandoverdracht/Data/FW__CIG_Survey_Polish_Data", sep = "")
  setwd(wd)
  
  options(stringsAsFactors = F)
  
}
#______________________________________Load data________________________________________#
{
  df <- read.csv2("Polish survey dataset_csv.csv", sep=",")
  
  vid <- read.csv2("vid.csv")
  View(vid)
  View(df)
}
#______________________________________Recodes__________________________________________#
{
  df$q16p_23 <- df$q16_23pl 
  df$q16_23pl <- NULL 
  
  
  df$q32p_23 <- df$q32pl_01
  df$q32p_23 <- df$q32pl_02
  df$q32p_23 <- df$q32pl_03
  df$q32p_23 <- df$q32pl_04
  df$q32p_23 <- df$q32pl_05 
  df$q32p_23 <- df$q32pl_06
  df$q32p_23 <- df$q32pl_07
  
  
  df$q32pl_01 <- NULL
  df$q32pl_02 <- NULL
  df$q32pl_03 <- NULL
  df$q32pl_04 <- NULL
  df$q32pl_05 <- NULL
  df$q32pl_06 <- NULL
  df$q32pl_07 <- NULL
  
  df$q52 <- Recode(df$q52, "-5:-1=NA")
  
  df$q52p_02 <- df$q52pl_02
  
  df$q52pl_02 <- NULL 
  
  df$q54 <- Recode(df$q54, "1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10")
  
  df$q55 <- Recode(df$q55, "1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10")
  df$q56 <- Recode(df$q56, "1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10")
  df$q53 <- Recode(df$q53, "1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10")
}
#______________________________________rapport__________________________________________#

df<- data(cars)





##to do :

# probleem met grid.table(table) oplossen, dit geeft nu een dim-fout. Een alternatieve manier van tabel plot werd gebruikt (maar deze is niet aan te passen)
# de main loop stopt abrupt omdat de dataset verkleint per itteratie (er wordt dus ook over alle elementen geloopt!)
# q48_01 : en volgende, worden niet correct geplot 
# de breedte van de afbeeldingen zou dynamisch moeten worden in gesteld (nu soms afgekapt), maar mss is dit anders in markdown



rapport_maker <- function(df) {
  stamList <- c("some initialization because R is too stupid to deal with empty vectors when using grepl?")
  for(i in 1:length(names(df))) {
    volgnummer <- i
    
    naam <- names(df)[i]
    
    stam <- substring(naam, 0, regexpr("_", naam)) #vindt stam van de variabelen set
    
    if (is.element(stam, stamList)) {
      message(volgnummer, " is geskipt")
      next
    } else {
      stamList = c(stamList, stam)
    }
    
    #als er geen match wordt gevonden is de stam leeg, waarna deze lege string matched met alle variabelen
    if (stam != "")  {
      set <- grep(stam, names(df), value = TRUE)
    } else {
      set <- naam 
    }
    
    len <- length(set) # lengte van de set
    
    vraag <-vid[which(vid$VID == naam ),which(names(vid)=="QID.Text")] 
    
    if (identical(vraag, character(0))) {
      message(volgnummer,". " ," ", "Voor variabele -",naam , "- geen match gevonden in file! Deze verwerking is overgeslagen.")
      next
    }
    
    if (typeof(df[[i]]) == "character" ) {
      message(volgnummer,". " ," ", "Variabele -",naam , "- is van type STRING! Deze verwerking is overgeslagen.")
      next
    }
    
    if (length(as.data.frame(as.matrix(table(df[[i]])))[[1]]) > 15) {
      message(volgnummer," ", naam, " werd overgeslagen omdat het aantal categorie? n hoger was dan 15.")
      next
    }
    
    qid <- vid[which(vid$VID == naam ),which(names(vid)=="QID")] 
    
    vid_text <-  vid[which(vid$VID == naam ),which(names(vid)=="VID.Text")] 
    
    #als responses.P leeg is, maak dan gebruik van responses general
    if (identical(vid[which(vid$VID == naam ),which(names(vid)=="Responses.P")],"")) {
      labels <- vid[which(vid$VID == naam ),which(names(vid)=="Responses")]
    } else {
      labels <- vid[which(vid$VID == naam ),which(names(vid)=="Responses.P")]
    }
    
    x <- sapply(labels, function(x) strsplit(x, ";")[[1]], USE.NAMES=FALSE)
    
    if (len==1) {
      table <- t(round(as.data.frame(as.matrix(prop.table(table(df[[i]])))),2))
      
      row.names(table) <- naam #vid_text
      
      title <- textGrob(paste0(qid ," ",vraag),gp=gpar(fontsize=17))
      padding <- unit(10,"mm")
      
      tt3 <- ttheme_default(
        colhead=list(fg_params=list(fontface= 1L)))
      
      table <- tableGrob(table, theme=tt3)
      
      t <- gtable_add_rows(
        table, 
        heights = grobHeight(title) + padding,
        pos = 0)
      table <- gtable_add_grob(
        t, 
        title, 
        1, 1, 1, ncol(table))
      
      
      mypath <- file.path(getwd(), "resultaat", paste(naam, ".png", sep = ""))
      png(file=mypath)
      
      grid.newpage()
      grid.draw(table) 
      
      dev.off()
      
      message(volgnummer," ",naam," werd verwerkt")
      
    } else{
      for (i in 1:length(set)) {
        if (i==1) {
          df[[ which(names(df)==set[i]) ]] <- factor(df[[ which(names(df)==set[i]) ]], labels = x, levels = 1:length(x) )
          
          df_resultaat <-  round( as.data.frame(  as.matrix ( prop.table( table( df[[  which(names(df) == set[1])  ]] ) ) )),2)
          names(df_resultaat)[1] <- set[1]
          
          df_resultaat <- t(df_resultaat)
          
          message(volgnummer," ", set[i]," werd verwerkt")
          
        } else {
          
          df[[ which(names(df)==set[i]) ]] <- factor(df[[ which(names(df)==set[i]) ]], labels = x, levels = 1:length(x) )
          
          df_resultaat_1 <-  round( as.data.frame(  as.matrix ( prop.table( table( df[[  which(names(df) == set[i])  ]] ) ) )),2)
          names(df_resultaat_1)[1] <- set[i]
          
          df_resultaat_1 <- t(df_resultaat_1)
          
          df_resultaat <- smartbind(df_resultaat, df_resultaat_1) #probleem hier is nog als het eerste antwoord niet alle cat's heeft, dat ze ook niet gebonden zullen worden
          
          message(volgnummer," ",set[i]," werd verwerkt")
        }
      }
      
      row.names(df_resultaat) <- set
      
      title <- textGrob(paste0(qid ," ",vraag),gp=gpar(fontsize=17))
      padding <- unit(10,"mm")
      
      tt3 <- ttheme_default(
        colhead=list(fg_params=list(fontface= 1L)))
      
      table <- tableGrob(df_resultaat, theme=tt3)
      
      table <- gtable_add_rows(
        table, 
        heights = grobHeight(title) + padding,
        pos = 0)
      table <- gtable_add_grob(
        table, 
        title, 
        1, 1, 1, ncol(table))
      
      mypath <- file.path(getwd(), "resultaat", paste(stam, ".png", sep = ""))
      png(file=mypath)
      #grid.table(table)
      grid.newpage()
      grid.table(df_resultaat)
      dev.off()
    }
  }
}
rapport_maker(df)

library(gridExtra)
library(grid)




grid.newpage()






#checks
{
  utils::View(df)
  
  table(df$q01)
  
  table(df$q01_02)
  
  table(df$q02)
  
  table(df$q03)
  
  table(df$q04_05)
  
  table(df$q04_02)
  
  table(df$q04_08)
  
  table(df$q04_07)
  
  for(i in 1:length(grep("q05", names(df), value = TRUE)))
  {
    
    x <- grep("q05", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  
  table(df$q06)
  
  table(df$q07)
  
  table(df$q08)
  
  for(i in 1:length(grep("q09", names(df), value = TRUE)))
  {
    
    x <- grep("q09", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q10_", names(df), value = TRUE)))
  {
    
    x <- grep("q10_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q11_", names(df), value = TRUE)))
  {
    
    x <- grep("q11_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  
  for(i in 1:length(grep("q12_", names(df), value = TRUE)))
  {
    
    x <- grep("q12_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q13_", names(df), value = TRUE)))
  {
    
    x <- grep("q13_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q15_", names(df), value = TRUE)))
  {
    
    x <- grep("q15_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q16_", names(df), value = TRUE)))
  {
    
    x <- grep("q16_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  
  for(i in 1:length(grep("q17_", names(df), value = TRUE)))
  {
    
    x <- grep("q17_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  table(df$q18)
  
  for(i in 1:length(grep("q19_", names(df), value = TRUE)))
  {
    
    x <- grep("q19_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q20_", names(df), value = TRUE)))
  {
    
    x <- grep("q20_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q21_", names(df), value = TRUE)))
  {
    
    x <- grep("q21_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q22_", names(df), value = TRUE)))
  {
    
    x <- grep("q22_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  table(df$q23)
  
  
  for(i in 1:length(grep("q24_", names(df), value = TRUE)))
  {
    
    x <- grep("q24_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  table(df$q25)
  
  for(i in 1:length(grep("q26_", names(df), value = TRUE)))
  {
    
    x <- grep("q26_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  
  table(df$q27)
  
  for(i in 1:length(grep("q28_", names(df), value = TRUE)))
  {
    
    x <- grep("q28_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q29_", names(df), value = TRUE)))
  {
    
    x <- grep("q29_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  
  for(i in 1:length(grep("q30_", names(df), value = TRUE)))
  {
    
    x <- grep("q30_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q31_", names(df), value = TRUE)))
  {
    
    x <- grep("q31_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q32pl_", names(df), value = TRUE)))
  {
    
    x <- grep("q32pl_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  for(i in 1:length(grep("q33_", names(df), value = TRUE)))
  {
    
    x <- grep("q33_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q34_", names(df), value = TRUE)))
  {
    
    x <- grep("q34_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q35_", names(df), value = TRUE)))
  {
    
    x <- grep("q35_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  table(df$q36)
  
  table(df$q37)
  
  
  for(i in 1:length(grep("q38_", names(df), value = TRUE)))
  {
    
    x <- grep("q38_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  
  for(i in 1:length(grep("q39_", names(df), value = TRUE)))
  {
    
    x <- grep("q39_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q40_", names(df), value = TRUE)))
  {
    
    x <- grep("q40_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  
  table(df$q41)
  
  table(df$q42)
  
  
  for(i in 1:length(grep("q43_", names(df), value = TRUE)))
  {
    
    x <- grep("q43_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q44_", names(df), value = TRUE)))
  {
    
    x <- grep("q44_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  table(df$q45)
  
  table(df$q46)
  
  for(i in 1:length(grep("q47_", names(df), value = TRUE)))
  {
    
    x <- grep("q47_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q48_", names(df), value = TRUE)))
  {
    
    x <- grep("q48_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  table(df$q49)
  
  table(df$q50)
  
  table(df$q51)
  
  table(df$q52)
  
  
  
  table(df$q53)
  
  
  table(df$q54)
  
  
  
  table(df$q56)
  
  
  table(df$q55)
  
  
  table(df$q70)
  
  table(df$q58)
  
  table(df$q59)
  
  for(i in 1:length(grep("q60_", names(df), value = TRUE)))
  {
    
    x <- grep("q60_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q61_", names(df), value = TRUE)))
  {
    
    x <- grep("q61_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q62_", names(df), value = TRUE)))
  {
    
    x <- grep("q62_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  for(i in 1:length(grep("q63_", names(df), value = TRUE)))
  {
    
    x <- grep("q63_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  table(df$q57)
  
  
  for(i in 1:length(grep("q69_", names(df), value = TRUE)))
  {
    
    x <- grep("q69_", names(df), value = TRUE)
    print(x[i])
    print(table( df[[  which(names(df) == x[[i]])  ]] ))
  }
  
  table(df$q71)
  
  table(df$q64)
  
  table(df$q66)
  
  table(df$q74)
  
  
  
}


