examples.loginPart = function() {

  restore.point.options(display.restore.point = TRUE)

  setwd("D:/libraries/shinyEventsLogin")
  app = eventsApp()

  login.fun = function(app=getApp(),userid,lop=get.lop(),...) {
    cat("Successfully logged in as ", userid)
    setUI("mainUI", wellPanel(actionButton("successBtn", paste0("Success... ", userid, " log in again"))))
    buttonHandler("successBtn", function(app,...) {
      initLoginDispatch(lop=get.lop())
    })
  }

  check.email.fun = function(email="",...) {
    restore.point("check.email.fun")
    if (!isTRUE(email=="sebastian.kranz@uni-ulm.de" |
                email=="sebkranz@gmail.com")) {
      return(list(ok=FALSE, msg="Please only send to your own email adresses!"))
    }
    list(ok=TRUE,msg="")
  }

  sender.file = "sender.txt"
  db.arg = list(dbname="testdb",drv=SQLite())
  #lop.create.db(db.arg,overwrite = TRUE)

  lop = loginModule(db.arg = db.arg, login.fun=login.fun, check.email.fun=check.email.fun,app.url="http://127.0.0.1:4915", app.title="Ulm-WiWi Seminarvergabe",container.id = "mainUI", init.userid = "", init.password = "", need.password = TRUE, need.userid=TRUE, fixed.password="Omkoo", use.signup=TRUE
  )


  appInitHandler(function(session,...) {
    initLoginDispatch(lop)
  })

  ui = fluidPage(uiOutput("mainUI"))
  app$lop = lop
  restore.point.options(display.restore.point = TRUE)
  runEventsApp(app,ui = ui, launch.browser=rstudio::viewer)

}

loginModule = function(id="loginModule",container.id = NULL,db.arg=lop.db.arg(),conn=NULL,login.fun=NULL, signup.fun = default.signup.fun, reset.fun = default.reset.fun, check.email.fun=NULL, email.text.fun = default.email.text.fun, app.url = NULL, app.title=id, init.userid="", init.password="", email.domain=NULL, smtp=NULL, set.need.authentication = TRUE, login.link = FALSE, app=getApp(),
lang="en",login.title=NULL,help.text=NULL, connect.db=use.signup, load.smtp=FALSE,
  login.failed.fun = lop.default.failed.login,
  login.ui.fun = login.default.ui,
  create.email.ui.fun = lop.default.create.email.user.ui,
  reset.email.ui.fun = lop.default.reset.email.user.ui,
  create.passwd.ui.fun = lop.default.create.passwd.ui,

  userid.equals.email=TRUE,
  need.password=TRUE,
  need.userid=TRUE,
  userid.or.email=TRUE,
  fixed.password=NULL,
  validate.userid.fun = default.validate.userid.fun,
  use.signup = need.userid & need.password,

  only.lowercase=TRUE,
  login.by.query.key = c("no","allow","require")[1],
  token.dir = NULL,
  fixed.query.key = NULL,

  ...
)
{
  restore.point("loginPart")

  if (is.null(container.id))

  if (set.need.authentication)
    app$need.authentication = TRUE

  if (is.null(check.email.fun)) {
    if (!is.null(email.domain)) {
      check.email.fun = function(email,...) {
        check.email.domain(email, email.domain)
      }
    } else {
      check.email.fun = function(email,...) {
        list(ok=TRUE,msg="")
      }
    }
  }

  lop = list(
    ns = NS(id),
    app.title = app.title,
    app.url = app.url,
    db.arg = db.arg,
    conn = conn,
    login.fun = login.fun,
    signup.fun = signup.fun,
    reset.fun = reset.fun,
    check.email.fun = check.email.fun,
    email.domain = email.domain,
    email.text.fun = email.text.fun,
    init.userid = init.userid,
    init.password = init.password,
    smtp = smtp,
    login.link = login.link,

    lang = lang,
    login.title=login.title,
    help.text=help.text,
    login.failed.fun = login.failed.fun,
    container.id = container.id,

    login.ui.fun = login.ui.fun,
    create.email.ui.fun = create.email.ui.fun,
    reset.email.ui.fun = reset.email.ui.fun,
    create.passwd.ui.fun = create.passwd.ui.fun,

    userid.equals.email=userid.equals.email,
    need.password=need.password,
    need.userid=need.userid,
    userid.or.email=userid.or.email,
    fixed.password=fixed.password,
    use.fixed.password = !is.null(fixed.password) & need.password,
    validate.userid.fun = validate.userid.fun,
    use.signup = use.signup,

    only.lowercase = only.lowercase,
    login.by.query.key = login.by.query.key,
    token.dir = token.dir,
    fixed.query.key = fixed.query.key
  )
  if (need.password & !need.userid & lop$use.fixed.password) {
    stop("If need.userid==FALSE and need.password==TRUE, you must provide a fixed.password to loginModule.")
  }

  if (!is.null(lop$sender.file)) {
    sender.txt = readLines(lop$sender.file)
    txt = poor.decrypt(sender.txt)
    lop$sender = yaml.load(txt)
  }

  lop = as.environment(lop)
  if (connect.db) {
    lop.connect.db(lop=lop)
    if (load.smtp){
      if (is.null(smtp)) lop$smtp = lop.get.smtp(lop=lop)
    }

  }



  lop
}


#' This function must be called in the initHandler of the app
initLoginDispatch = function(lop, container.id=lop$container.id, app=getApp()) {
  restore.point("initLoginDispatch")
  session = app$session

  # Very important: make session specific copy of lop
  lop = as.environment(as.list(lop, all.names=TRUE))
  set.lop(lop)

  lop$container.id = container.id
  lop.login.handlers(lop=lop)
  observe(priority = -100,x = {
    query <- parseQueryString(session$clientData$url_search)
    restore.point("loginDispatchObserver")
    if (lop$login.by.query.key == "allow" | lop$login.by.query.key == "require") {
      res = login.by.query.key(lop,query = query)
      # successful login via url key

      if (res$ok) {
        restore.point("sfhfbhbf")
        app$is.authenticated = TRUE
        if (is.null(lop$login.fun)) {
          stop("No login.fun defined.")
        }

        do.call(lop$login.fun, c(res$tok,list(lop=lop)))
        return(invisible())
      } else if (lop$login.by.query.key == "require") {
        app$is.authenticated = FALSE
        lop$login.failed.fun(msg=res$msg, lop=lop)
        return(invisible())
      }
    }

    if ("confirm" %in% names(query)) {
      show.confirm.email(lop=lop, linkid=query$confirm)
    } else {
      show.login.ui(lop)
    }
  })
}

get.lop = function(app=getApp(), field="..lop.LOGIN") {
  app[[field]]
}

set.lop = function(lop,app=getApp(), field="..lop.LOGIN") {
  app[[field]] = lop
}

lop.db.arg = function(dbname="testdb",drv=SQLite(),...) {
  args = list(...)
  fill.defaults(args, nlist(dbname,drv))
}

show.html.message = function(id,msg="") {
  cat("\nhtml.message: ", msg)
  setUI(id,HTML(msg))
}

show.html.warning = function(id,msg="", color="red") {
  cat("\nhtml.warning: ", msg)
  html = paste0('<bold><font color="',color,'">',msg,'</font></bold>')
  setUI(id,HTML(html))
}


default.signup.fun = function(lop,...) {
  restore.point("default.signup.fun")
  lop.create.email.handlers(lop)
  setUI(lop$container.id, lop$create.email.ui.fun(lop=lop,...))
}


default.reset.fun = function(lop,...) {
  restore.point("default.reset.fun")
  lop.create.email.handlers(lop)
  setUI(lop$container.id, lop$reset.email.ui.fun(lop=lop,...))
}
