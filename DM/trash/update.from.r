update.from.db = function(
      target = "Data.ref"
      source = "akasprzyk.tabela1"
      target.var = "wplaty"
      target.id = "id_produkt"
      source.var = target.var
      source.id = id.target
){
#ch <- odbcConnect("PostgreSQL35W", uid = "akasprzyk", pwd = "akasprzyk")
update.table <- sqlQuery(ch, paste0( "SELECT * from ",source))
#odbcClose(ch)

variable = datfram[,variable.name]   ....

get(target,pos=-1)       ## The default of -1 indicates the current environment of the call to get.

}