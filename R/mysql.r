test.mysql=function() {
  setwd("D:/libraries/lopLogin")
  library(DBI)
  library(RMySQL)

  db.arg = lop.db.arg(dbname="testdb",drv=MySQL(),username="admin",password="test")

  db.arg = lop.db.arg(dbname="testdb",drv=SQLite())

  lop = make.lop(db.arg = db.arg)
  set.lop(lop)
  conn = lop.create.db(lop = lop, overwrite=TRUE)

  user = list(userid="jon", email="sebkranz@gmail.com", confirmed=TRUE)
  user = set.list(user,make.password.hash(password= "password"))
  user = lop.insert.user(conn=conn,user=user, mode="replace")

  lop.get.users(conn)


}
