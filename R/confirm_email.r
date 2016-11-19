examples.make.user = function() {
  random.password(nchar=5, chars=letters)
}

lop.default.create.passwd.ui = function(lop, ns=lop$ns, lang=lop$lang, ...) {
  restore.point("lop.create.passwd.ui")
  copy.into.env(source = lop$crepa)

  userid = lop$userid
  email=userid

  widgets = list(
    h4(lop$app.title),
    HTML(paste0("<p>Create password for ",userid,"</p><br>")),
    uiOutput(ns("passwd.display")),
    actionButton(ns("createPasswdBtn"), "Redraw password"),
    actionButton(ns("acceptPasswdBtn"), "Accept password and login"),
    actionButton(ns("cancelPasswdBtn"), "Cancel and remove link"),
    uiOutput(ns("createPasswdInfo"))
  )
  ui = wellPanel(widgets)
  setUI(ns("passwd.display"),"")
  setUI(ns("createPasswdInfo"),"")
  ui
}

lop.create.password.handlers = function(lop,ns=lop$ns,...) {
  buttonHandler(ns("createPasswdBtn"),generate.passwd.click, lop=lop)
  buttonHandler(ns("acceptPasswdBtn"),accept.passwd.click, lop=lop)
  buttonHandler(ns("cancelPasswdBtn"),cancel.passwd.click,lop=lop)
  generate.passwd.click(lop)
}


show.confirm.email = function(lop,linkid, app=getApp(),...) {
  restore.point("show.confirm.email")

  #setUI(lop$container.id, lop$create.password.ui(lop=lop))

  link = dbGetRow(conn = lop$conn,table = "loginlinks",params = list(linkid=linkid))
  if (is.null(link)) {
    ui = list(
      HTML("<p> The confirmation code in the link is not valid </p>")
    )
    setUI(lop$container.id, ui)
  } else {
    app$is.authenticated = TRUE
    link = as.list(link[1,,drop=FALSE])
    lop$userid = link$userid
    lop$email = link$userid
    lop$linkid = link$linkid
    setUI(lop$container.id, lop$create.passwd.ui.fun(lop=lop))
    lop.create.password.handlers(lop=lop)
    generate.passwd.click(lop=lop)
  }

}

generate.passwd.click = function(lop,ns=lop$ns, ..., app=getApp()) {
  restore.point("generate.passwd.click")

  lop$passwd = random.password(nchar=5, chars=letters)
  setUI(ns("passwd.display"), HTML(
    paste0("<p>Password: <b>", lop$passwd,"</b></p>")
  ))
}

accept.passwd.click = function(lop,ns=lop$ns, app=getApp(),...) {
  restore.point("accept.passwd.click")

  #cat("\n\npaswwd: ", lop$passwd)
  salt = make.salt()
  res = make.password.hash(password = lop$passwd, salt=salt)
  restore.point("accept.passwd.click2")

  # Remove comment: for debugging purposes password will be stored
  #user = list(userid=lop$userid, email=lop$email, salt=salt, hash=res$hash,passwd=lop$passwd, confirmed=TRUE, create_time=Sys.time())

  # Don't store password in database
  user = list(userid=lop$userid, email=lop$email, salt=salt, hash=res$hash, confirmed=TRUE, create_time=Sys.time())


  dbInsert(lop$conn, "users",user, mode="replace")

  # Remove link
  dbDelete(lop$conn,"loginlinks", list(linkid=lop$linkid))

  lop$login.fun(userid=user$userid, lop=lop)
}


cancel.passwd.click = function(lop, app=getApp(),...) {
  restore.point("cancel.passwd.click")
  # Remove link
  dbDelete(lop$conn,"loginlinks", list(linkid=lop$linkid))
  # Show login form
  show.login.ui(lop)
}

