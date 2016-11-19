#' Manually create a user in the user data base
#' mainly for local testing and debugging where email service does not work
create.user.in.db = function(conn=dbConnect(db.arg$drv, db.arg$dbname), userid, email, password, db.arg=list(dbname=paste0(db.dir,"/",dbname),drv=drv), dbname="userDB.sqlite",drv=SQLite(), db.dir="") {
  restore.point("create.user.in.db")

  # Don't store password in database

  salt = make.salt()
  hash = make.password.hash(password = password, salt=salt)$hash

  user = list(userid=userid, email=email, salt=salt, hash=hash, confirmed=TRUE, create_time=Sys.time())

  dbInsert(conn, "users",user, mode="replace")
}

