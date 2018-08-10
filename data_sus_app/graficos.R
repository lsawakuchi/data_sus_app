library(RMySQL)
especialidade <- "cardiologia"
cnes <- 3808
## Connecting to database ----

mydb <- dbConnect(MySQL(), user="master", password="analytics", dbname="mais_saude", host="35.193.221.159")

## tables in the database ----

dbListTables(mydb)

## running queries ----

stt = paste("select * from mais_saude.notas_especialidades where especialidade='", especialidade, "' and cnes=",cnes)

df <- dbSendQuery(mydb, stt)

# getting data into R ----
dta <- fetch(df, n=-1)

## closing connection ----
dbDisconnect(mydb)

## new column with formated date ----
dta["data2"] <- substr(dta$data, 1,10)

# creating a bar plot for the data ----
H <- dta$equip_uf
M <- dta$data2

## changing fontsize of labels ----
ds <- par(no.readonly = TRUE)
par(cex.lab = 1.2, cex.axis=0.5)
par(mar=c(10,10,3,3),cex.axis=0.8, cex.lab=1.2)
barplot(H, names.arg = M, ylab = "quantidade", main = "Evolução Temporal", col="darkblue", las=2)

