library(RMySQL)

dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}


loadBase <-function(cnes, especialidade){
  mydb <- dbConnect(MySQL(), user="master", dbname = "mais_saude", password = "analytics", host = "35.193.221.159")
  df <- dbSendQuery(mydb, paste0("select * from mais_saude.notas_especialidades where especialidade='",
                                especialidade, "' and cnes=",cnes))
  
  dta <- fetch(df, n=-1)
  
  dbDisconnect(mydb)
  
  dta["data"] <- substr(dta$data, 1, 7)
  
  return(dta)
}

especialidades <- function(cnes){
  mydb <- dbConnect(MySQL(), user = "master", dbname = "mais_saude", password = "analytics",
                    host = "35.193.221.159")
  df = dbSendQuery(mydb, paste0("select distinct cnes, especialidade from mais_saude.notas_especialidades
                                where cnes=", cnes))
  dta <- fetch(df, n=-1)
  dbDisconnect(mydb)
  
  return(dta)
}

loadBaseCnes <- function(cnes){
  mydb <- dbConnect(MySQL(), user="master", dbname = "mais_saude", password = "analytics", host = "35.193.221.159")
  df <- dbSendQuery(mydb, paste0("select * from mais_saude.notas_especialidades where cnes=", cnes))
  dta <- fetch(df, n=-1)
  dbDisconnect(mydb)
  
  dta["data"] <- substr(dta$data, 1, 7)
  
  return(dta)
  
}

plotData <- function(data, item){
    
    if(item == 5 | item == 9){
      leg <- "Equipamentos"
    }else if(item == 6 | item == 10){
      leg <- "Leitos"
    }else if(item == 7 | item == 11){
      leg = "Profissionais"
    }else{
      leg = "Habilitações"
    }
    M <- data$data
    H <- data[item]
    H <- unlist(H, use.names = FALSE)
    y_max <- max(H)
    par(mar=c(10,10,3,3),cex.axis=1, cex.lab=1.2)
    barplot(H, names.arg = M, ylab = "comparação com o valor médio", main = paste("Evolução das notas de ", leg), 
            col="#151D3E", las=2, ylim = c(0, y_max))
  
  
}



plotData2 <- function(data, item){
  if(item == 5 | item == 9){
    leg <- "Equipamentos"
  }else if(item == 6 | item == 10){
    leg <- "Leitos"
  }else if(item == 7 | item == 11){
    leg = "Profissionais"
  }else{
    leg = "Habilitações"
  }
  M <- data$data
  H <- data[item]
  H <- unlist(H, use.names = FALSE)
  y_max <- max(H)
  par(mar=c(10,10,5,5),cex.axis=1, cex.lab=1.2)
  
  ggplot(data = data, aes(x = M, y = H)) + geom_bar(stat = "identity", fill = "#3895E1") + 
    theme(panel.background = element_rect(fill = "white"),
          plot.margin = margin(1, 1, 1, 1, "cm"),
          plot.background = element_rect(
            fill = "grey90",
            colour = "black",
            size = 1),
          axis.text.x =  element_text(angle = 45, hjust = 0, vjust = 0)) + ggtitle(paste("Evolução das notas de ", leg)) + 
    labs(y = "Comparação com o valor médio", x = "")
       
}

plotData3 <- function(dta_espec, item, cnes){
  for(i in 1:dim(dta_espec)[1]){
    base <- loadBase(cnes, dta_espec$especialidade[i])
    plotData2(base, item)
  }
}

sumarioCnes <- function(cnes, comp ){
  df <- loadBaseCnes(cnes)
  if(comp=="uf"){
    df2 <- aggregate(x=df[c("equip_uf", "leitos_uf", "prof_uf", "hab_uf")], by = df["especialidade"], FUN=mean)
  }else{
    df2 <- aggregate(x=df[c("equip_br", "leitos_br", "prof_br", "hab_br")], by = df["especialidade"], FUN=mean)
  }
  
  return(df2)
}

plotaSumario <- function(cnes, comp){
  df <- sumarioCnes(cnes, comp)
  d <- melt(df)
  ggplot(d, aes(especialidade, value, fill = variable)) +  geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set1") + ggtitle(paste("Sumário das Especialidades - Cnes :  ", cnes)) + 
    labs(y = "Comparação com o valor médio", x = "Especialidades") + theme(plot.margin = margin(1, 1, 1, 1, "cm"))
}

notaPond <- function(cnes, comp){
  df <- sumarioCnes(cnes, comp)
  pesos <- data.frame("especialidade" = c("cardiologia", "radiologia", "nefrologia", "odontologia", 
                                            "fisioterapia", "oftalmologia", "neurologia", "psiquiatria",
                                            "oncologia", "fonoaudiologia", "hematologia", "pediatria",
                                            "obstetricia", "ortopedia"),
                        "equip" = c(2,3,3,2,0,3,1,0,0,0,0,0,0,0),
                        "leitos" = c(3,0, 1,0,0,2,3,3,2,0,3,2,3,2),
                        "prof" = c(2,3,3,3,3,2,3,3,2,3,2,3,3,3),
                        "hab" = c(1,1,1,1,0,1,1,1,1,1,1,1,1,1))
  df2 <- merge(df, pesos, all.x = TRUE)
  df2["nota_equip"] <- df2[2]*df2["equip"]
  df2["nota_leitos"] <- df2[3]*df2["leitos"]
  df2["nota_prof"] <- df2[4]*df2["prof"]
  df2["nota_hab"] <- df2[5]*df2["hab"]
  
  df2["nota_equip"] <- df2["nota_equip"]/sum(pesos["equip"])
  df2["nota_leitos"] <- df2["nota_leitos"]/sum(pesos["leitos"])
  df2["nota_prof"] <- df2["nota_prof"]/sum(pesos["prof"])
  df2["nota_hab"] <- df2["nota_hab"]/sum(pesos["hab"])
  
  df2["nota_equip"] <- sum(df2["nota_equip"])
  df2["nota_leitos"] <- sum(df2["nota_leitos"])
  df2["nota_prof"] <- sum(df2["nota_prof"])
  df2["nota_hab"] <- sum(df2["nota_hab"])
  df2 <- df2[1, 10:13]
  return(df2)
  
}

plotNotaPond <- function(cnes, comp){
  df <- notaPond(cnes, comp)
  df2 <- data.frame("item" = c("Equipamentos", "Leitos", "Profissionais", "Habilitações"),
                    "nota" = c(df$nota_equip, df$nota_leitos, df$nota_prof, df$nota_hab))
  
  M <- df2$item
  H <- df2["nota"]
  H <- unlist(H, use.names = FALSE)
  #y_max <- max(H)
  #par(mar=c(10,10,5,5),cex.axis=1, cex.lab=1.2)
  
  ggplot(data = df2, aes(x = M, y = H)) + geom_bar(stat = "identity", fill = "#3895E1", width = 0.3) + 
    ggtitle(paste("Notas Finais Cnes : ", cnes)) + 
    labs(y = "Comparação com o valor médio", x = "") + theme(plot.margin = margin(1, 1, 1, 1, "cm"),
                                                             axis.title.y = element_text(size= 8))
  

  
}


