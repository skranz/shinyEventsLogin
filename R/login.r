show.login.ui = function(lop,...) {
  restore.point("show.login.ui")

  # login directly
  if (!lop$need.userid & !lop$need.password) {
    init.userid = lop$init.userid
    if (is.function(init.userid)) init.userid = init.userid()

    lop$login.fun(userid=init.userid, password=lop$init.password, lop=lop,...)
    return()
  }

  setUI(lop$container.id, lop$login.ui.fun(lop=lop,...))
  show.html.message(lop$ns("loginAlert"),"")
}

login.default.ui = function(lop=NULL,ns=lop$ns, init.userid=lop$init.userid, init.password=lop$init.password, title.html = lop$login.title,help.text=lop$login.help,lang = lop$lang,...) {
  restore.point("login.default.ui")
  sel = ids2sel(c(ns("loginUser"),ns("loginPassword")))

  if (is.function(init.userid)) init.userid = init.userid()

  if (identical(lang,"de")) {
    widgets = list(
      HTML(title.html),
      if (lop$need.userid) textInput(ns("loginUser"), "Nutzer", value = init.userid),
      if (lop$need.password) passwordInput(ns("loginPassword"), "Passwort", value = init.password),
      actionButton(ns("loginBtn"), "Anmelden", "data-form-selector"=sel),
      if (lop$use.signup) actionButton(ns("loginSignupBtn"), "Registrieren"),
      if (lop$use.signup) actionButton(ns("loginResetBtn"), "Passwort vergessen"),
      uiOutput(ns("loginAlert")),
      HTML(help.text)
    )
  } else {
    widgets = list(
      HTML(title.html),
      if (lop$need.userid) textInput(ns("loginUser"), "User", value = init.userid),
      if (lop$need.password) passwordInput(ns("loginPassword"), "Password", value = init.password),
      actionButton(ns("loginBtn"), "log in", "data-form-selector"=sel),
      if (lop$use.signup) actionButton(ns("loginSignupBtn"), "sign up"),
      if (lop$use.signup) actionButton(ns("loginResetBtn"), "reset password"),
      uiOutput(ns("loginAlert")),
      HTML(help.text)
    )
  }
  setUI("loginAlert","")
  ui = wellPanel(widgets)
  ui
}

lop.login.handlers = function(lop, ns=lop$ns,...) {
  restore.point("lop.login.handlers")

  buttonHandler(ns("loginBtn"),lop.login.btn.click,lop=lop,no.authentication.required = TRUE)
  buttonHandler(ns("loginSignupBtn"),lop.signup.btn.click,lop=lop,no.authentication.required = TRUE)
  buttonHandler(ns("loginResetBtn"),lop.reset.btn.click,lop=lop,no.authentication.required = TRUE)

}


lop.signup.btn.click = function(app=getApp(),lop,...) {
  if (!is.null(lop$signup.fun)) {
    lop$signup.fun(lop=lop,...)
  }
}

lop.reset.btn.click = function(app=getApp(),lop,...) {
  if (!is.null(lop$reset.fun)) {
    lop$reset.fun(lop=lop,...)
  }
}

lop.login.btn.click = function(app=getApp(),lop,formValues,ns=lop$ns,...) {
  userid = formValues[[ns("loginUser")]]
  if (isTRUE(lop$only.lowercase)) {
    userid = tolower(userid)
  }


  password = formValues[[ns("loginPassword")]]

  cat("userid = ", userid, " password = ", password)

  res = lop.check.login(userid=userid,password = password, lop=lop)
  restore.point("lop.login.btn.click")
  if (res$ok==TRUE) {
    app$is.authenticated = TRUE
    lop$login.fun(userid=userid, password=password, lop=lop,...)
  } else {
    app$is.authenticated = FALSE
    lop$login.failed.fun(userid=userid, password=password, msg=res$msg, lop=lop,...)
  }
}

lop.default.failed.login = function(app=getApp(),lop=get.lop(),msg="Log-in failed.",ns=lop$ns,...) {
  if (lop$login.by.query.key=="require") {
    cat("\nlog-in failed: ",msg)
    setUI(lop$container.id, HTML(colored.html(msg)))
    return()
  }
  show.html.warning(ns("loginAlert"),msg)
  cat("\nlog-in failed: ",msg)
}

lop.check.login = function(userid, password, lop=get.lop()) {
  restore.point("lop.check.login")

  # login via a fixed password
  if (lop$use.fixed.password) {
    if (!identical(password, lop$fixed.password)) {
      return(list(ok=FALSE,msg="Wrong password."))
    }
    return(list(ok=TRUE,msg=""))
  }

  # just pick a username without any password
  if (!lop$use.signup) {
    return(lop$validate.userid.fun(userid))
  }

  if (nchar(userid)==0)
    return(list(ok=FALSE,msg="No user name entered."))


  user = lop.get.user(userid=userid, lop=lop)
  if (NROW(user)==0) {
    return(list(ok=FALSE,msg="User does not exist."))
  }
  ok = check.password(password = password, salt=user$salt,hash=user$hash)
  if (ok) {
    return(list(ok=TRUE,msg=""))
  }
  return(list(ok=FALSE,msg="Wrong password."))
}


check.email.domain = function(email, domain) {
  ok = str.ends.with(email, domain)
  if (!ok) {
    return(list(ok=ok, msg=paste0("You can only create an account with an email that ends with ", domain)))
  }
  return(list(ok=ok, msg=""))
}



