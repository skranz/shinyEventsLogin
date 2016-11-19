examples.db = function() {
  library(RSQLite)

  app = eventsApp()
  lop = make.lop()
  set.lop(lop)

  #conn = lop.create.db(overwrite=TRUE)
  conn = lop.connect.db(lop)

  lop.get.user("Seb")

  user = list(userid="jon", email="sebkranz@gmail.com", confirmed=FALSE)
  user = lop.insert.user(conn=conn,user=user, mode="replace")

  users = lop.fetch.users(conn)

  user = set.list(user,make.password.hash(password= "password"))
  user = lop.insert.user(conn=conn,user=user, mode="insert or replace")


  con <- dbConnect(SQLite(), ":memory:")
  data(USArrests)
  dbWriteTable(con, "USArrests", USArrests)

  rs <- dbSendQuery(con, "select * from USArrests")
  d1 <- fetch(rs, n = 10)      # extract data in chunks of 10 rows
  dbHasCompleted(rs)
  d2 <- fetch(rs, n = -1)      # extract all remaining data
  dbHasCompleted(rs)
  dbClearResult(rs)
  dbListTables(con)
  rs
  d1
}

lop.connect.db = function(db.arg=lop$db.arg,lop=get.lop()) {
  #if (require(pool)) {
  #  lop$conn = dbPool(db.arg$drv, db.arg$dbname)
  #} else {
  #  lop$conn = dbConnect(db.arg$drv, db.arg$dbname)
  #}
  lop$conn = dbConnect(db.arg$drv, db.arg$dbname)
  invisible(lop$conn)
}

#' Create a new database for logins
#' it stores users and temporary links
create.login.db = function(db.arg=lop$db.arg, overwrite=FALSE, schema.file = paste0(path.package("loginPart"),"/schema/logindb.yaml"),lop=get.lop()) {
  restore.point("lop.create.db")
  conn = do.call(dbConnect, db.arg)
  dbCreateSchemaTables(conn =conn,schema.file = schema.file,overwrite = overwrite)
  conn
}

lop.insert.user = function(conn=lop$conn,userid=NULL, email="",salt="",hash="",confirmed=NULL, created=as.integer(Sys.time()), user=list(), mode="replace", lop=get.lop()) {
  restore.point("lop.insert.user")
  user = fill.defaults(user, nlist(userid,email,salt,hash,created,confirmed))
  dbInsert(conn,"users",user, mode=mode)
  invisible(user)
}


lop.insert.link = function(conn=lop$conn,lop=get.lop(),link) {
  restore.point("lop.insert.user")
  user = fill.defaults(user, nlist(userid,email,salt,hash,created,confirmed))
  dbInsert(conn,"loginlinks",user, mode=mode)
  invisible(user)
}


lop.get.users = function(conn) {
  dbReadTable(conn,"users")
}

lop.get.user = function(userid,conn=lop$conn, lop=get.lop()) {
  restore.point("lop.get.user")

  dbGetRow(conn,table = "users",params = list(userid=userid))
}

examples.lop.insert.smtp = function() {
  setwd("D:/libraries/lopLogin")
  txt = paste0(readLines("sender.yaml"),collapse="\n")
  lop.create.db()
  lop.insert.smtp(txt)
  lop.get.smtp()
}

lop.insert.smtp = function(yaml,conn=lop$conn, lop=get.lop()) {
  restore.point("lop.insert.smtp")
  li =   yaml.load(yaml)
  dbInsert(conn,table = "smtp",vals = list(smptid=li$from,yaml=yaml),mode="replace")
}


lop.get.smtp = function(smtpid=NULL,conn=lop$conn, lop=get.lop()) {

  if (is.null(smtpid) & !is.null(conn)) {
    dat = dbReadTable(conn,"smtp")
  } else if (!is.null(conn)) {
    dat = dbGetSingleRow(conn, "smtp",list(smptid=smptid))
  } else {
    return(NULL)
  }
  if (NROW(dat)==0) return(NULL)
  yaml = dat$yaml[[1]]
  yaml.load(yaml)
}
