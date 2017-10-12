examples.make.user = function() {
  random.password(nchar=5, chars=letters)
}

lop.default.create.passwd.ui = function(lop, ns=lop$ns, lang=lop$lang, ...) {
  restore.point("lop.create.passwd.ui")
  copy.into.env(source = lop$crepa)

  userid = lop$userid
  email=userid

  if (identical(lang,"de")) {
    widgets = list(
      h4(lop$app.title),
      if (!lop$create.userid) {
        HTML(paste0("<p>Erstelle Passwort für ",userid,"</p><br>"))
      } else {
        tagList(
          HTML(paste0("<p>Erstelle Nutzername und Passwort für ",email,"</p><br>")),
          textInput(ns("createUserid"),"Nutzername",value="")
        )
      },
      uiOutput(ns("passwd.display")),
      actionButton(ns("createPasswdBtn"), "Ziehe anderes Passwort"),
      if (!lop$create.userid) {
        actionButton(ns("acceptPasswdBtn"), "Akzeptiere Passwort und weiter")
      } else {
        actionButton(ns("acceptPasswdBtn"), "Akzeptieren und weiter")
      },
      actionButton(ns("cancelPasswdBtn"), "Abbruch und Link löschen"),
      uiOutput(ns("createPasswdInfo"))
    )

  } else {
    widgets = list(
      h4(lop$app.title),
      if (!lop$create.userid) {
        HTML(paste0("<p>Create password for ",userid,"</p><br>"))
      } else {
        tagList(
          HTML(paste0("<p>Create username and password for ",email,"</p><br>")),
          textInput(ns("createUserid"),"Username",value="")
        )
      },
      uiOutput(ns("passwd.display")),
      actionButton(ns("createPasswdBtn"), "Redraw password"),
      if (!lop$create.userid) {
        actionButton(ns("acceptPasswdBtn"), "Accept password and login")
      } else {
        actionButton(ns("acceptPasswdBtn"), "Accept and login")
      },
      actionButton(ns("cancelPasswdBtn"), "Cancel and remove link"),
      uiOutput(ns("createPasswdInfo"))
    )
  }
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

    if (!is.null(lop$allowed.userids)) {
      if (!isTRUE(link$userid %in% lop$allowed.userids)) {
        setUI(lop$container.id, HTML("The user in the confirmation code has no access to this application."))
        return()
      }
    }

    lop$link_type = link$link_type
    lop$userid = link$userid


    lop$email = link$userid
    lop$linkid = link$linkid

    lop$create.userid = !(lop$userid.equals.email | lop$link_type == "reset_password")

    # If user exists and userid can be different to email, load existing userid
    if (!lop$userid.equals.email & !lop$create.userid) {
      db.user = dbGetRow(lop$conn, "users",list(email=lop$email))
      lop$userid = db.user$userid
    }

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

parse.created.userid = function(lop, ns=lop$ns, app=getApp()) {
  restore.point("parse.created.userid")

  userid = getInputValue(ns("createUserid"))
  if (isTRUE(lop$only.lowercase)) {
    userid = tolower(userid)
  }


  valid = lop$validate.userid.fun(userid)
  if (!valid$ok) return(valid)

  if (!is.null(lop$allowed.userids)) {
    if (!isTRUE(userid %in% lop$allowed.userids)) {
      return(list(ok=FALSE, msg=paste0("The user ",userid," has no access to this application.")))
    }
  }

  user = dbGetRow(lop$conn, "users",nlist(userid))

  if (NROW(user)>0) {
    return(list(ok=FALSE, msg=paste0("A user with username ",userid," already exists.")))
  }
  return(list(ok=TRUE, msg="",userid=userid))
}

accept.passwd.click = function(lop,ns=lop$ns, app=getApp(),...) {
  restore.point("accept.passwd.click")

  # userid will also be created
  if (lop$create.userid) {
    res = parse.created.userid(lop=lop, ns=ns, app=app)
    if (!res$ok) {
      show.html.warning(ns("createPasswdInfo"), res$msg)
      return()
    }
    # user created a valid, not yet existing userid
    lop$userid = res$userid
  }

  if (!is.null(lop$allowed.userids)) {
    if (!isTRUE(lop$userid %in% lop$allowed.userids)) {
      return(list(ok=FALSE, msg=paste0("The user ",lop$userid," has no access to this application.")))
    }
  }


  #cat("\n\npaswwd: ", lop$passwd)
  salt = make.salt()
  res = make.password.hash(password = lop$passwd, salt=salt)
  restore.point("accept.passwd.click2")
  user = list(userid=lop$userid, email=lop$email, salt=salt, hash=res$hash, confirmed=TRUE, create_time=Sys.time())
  dbInsert(lop$conn, "users",user, mode="replace")
  # Remove link
  dbDelete(lop$conn,"loginlinks", list(linkid=lop$linkid))

  lop$login.fun(userid=user$userid, lop=lop, login.mode="manual")
}


cancel.passwd.click = function(lop, app=getApp(),...) {
  restore.point("cancel.passwd.click")
  # Remove link
  dbDelete(lop$conn,"loginlinks", list(linkid=lop$linkid))
  # Show login form
  show.login.ui(lop)
}

default.validate.userid.fun = function(userid) {
  restore.point("default.validate.userid.fun")


  if (nchar(userid)==0) {
    return(list(ok=FALSE,msg="Please enter a username."))
  }

  alnum = grepl("^[a-zA-Z0-9_]*$",userid)
  if (!alnum) {
    return(list(ok=FALSE,msg="Please only use letters, digits or _ in your username."))
  }
  return(list(ok=TRUE,msg=""))
}
