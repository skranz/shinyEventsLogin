
dbInsert = function(conn, table, vals,sql=NULL,run=TRUE, mode=c("insert","replace")[1]) {
  restore.point("dbInsert")
  cols = names(vals)

  if (is.null(sql)) {
    sql <- paste0(mode, " into ", table," values (",
      paste0(":",cols,collapse=", "),")")
  }
  if (!run) return(sql)
  ret = dbSendQuery(conn, sql, params=vals)
  invisible(ret)
}


dbDelete = function(conn, table, params, sql=NULL) {
  restore.point("dbDelete")
  if (is.null(sql)) {
    if (length(params)==0) {
      where = ""
    } else {
      where = paste0(" where ", paste0(names(params)," = :",names(params), collapse= " AND "))
    }
    sql = paste0('delete from ', table, where)
  }
  rs = dbSendQuery(conn, sql, params=params)
  rs
}


dbGetRow = function(conn, table, params, sql=NULL) {
  restore.point("dbGetRow")
  if (is.null(sql)) {
    if (length(params)==0) {
      where = ""
    } else {
      where = paste0(" where ", paste0(names(params)," = :",names(params), collapse= " AND "))
    }
    sql = paste0('select * from ', table, where)
  }
  rs = dbSendQuery(conn, sql, params=params)
  res = dbFetch(rs)
  if (NROW(res)==0) return(NULL)
  res
}

dbCreateSchemaTables = function(conn,schema=NULL, schema.file=NULL, overwrite=FALSE,silent=FALSE) {
  restore.point("dbCreateSchemaTables")

  if (is.null(schema)) {
    schema = yaml.load_file(schema.file)
  }

  tables = names(schema)
  table = "loginlinks"
  lapply(tables, function(table) {
    s = schema[[table]]
    if (overwrite)
      try(dbRemoveTable(conn, table), silent=silent)
    if (!dbExistsTable(conn, table)) {
      # create table
      sql = paste0("CREATE TABLE ", table,"(",
        paste0(names(s$table), " ", s$table, collapse=",\n"),
        ")"
      )
      dbSendQuery(conn,sql)

      # create indexes
      for (index in s$indexes) {
        err = try(dbSendQuery(conn,index), silent=TRUE)
        if (is(err,"try-error")) {
          msg = as.character(err)
          msg = str.right.of(msg,"Error :")
          msg = paste0("When running \n", index,"\n:\n",msg)
          stop(msg)
        }
      }
    }
  })
  invisible(schema)
}
