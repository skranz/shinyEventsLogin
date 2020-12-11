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

  lop = loginModule(db.arg = db.arg, login.fun=login.fun, check.email.fun=check.email.fun,app.url="http://127.0.0.1:4915", app.title="Ulm-WiWi Seminarvergabe",container.id = "mainUI", init.userid = "", init.password = "", need.password = TRUE, need.userid=TRUE, fixed.password="test", use.signup=TRUE, allowed.userids="test"
  )


  appInitHandler(function(session,...) {
    initLoginDispatch(lop)
  })

  ui = fluidPage(uiOutput("mainUI"))
  app$lop = lop
  restore.point.options(display.restore.point = TRUE)
  runEventsApp(app,ui = ui, launch.browser=rstudio::viewer)

}

loginModule = function(id="loginModule",container.id = NULL,db.arg=lop.db.arg(dbname = dbname), dbname=NULL,  conn=NULL,login.fun=NULL, signup.fun = default.signup.fun, reset.fun = default.reset.fun, check.email.fun=NULL, email.text.fun = default.email.text.fun, app.url = NULL, app.title=id, init.userid="", init.password="", email.domain=NULL, smtp=NULL, set.need.authentication = TRUE, login.link = FALSE, app=getApp(),
lang="en",login.title=NULL,help.text=NULL, connect.db=use.signup & !is.null(db.arg$dbname), load.smtp=FALSE,
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
  login.by.cookie = login.by.query.key,
  set.successful.query.key.as.cookie = FALSE,
  cookie.name = "shinyEventsLoginCookie",
  allowed.userids = NULL,
  store.all.cookies = TRUE,
  load.cookies.handler =NULL,
  ...
)
{
  restore.point("loginModule")

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
    fixed.query.key = fixed.query.key,
    login.by.cookie = login.by.cookie,
    set.successful.query.key.as.cookie = set.successful.query.key.as.cookie,
    cookie.name = cookie.name,
    store.all.cookies = store.all.cookies,
    load.cookies.handler = load.cookies.handler,

    allowed.userids = allowed.userids
  )
  if (need.password & !need.userid & !lop$use.fixed.password) {
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



  # We add the corresponding flags and handlers
  # to deal with authentification by cookie and token
  app$..cookies.were.loaded = FALSE
  if (lop$login.by.cookie != "no") {
    restore.point("init.login.by.cookie")
    eventId = "loginCookieLoad"

    onload.cookies = if (!lop$store.all.cookies) lop$cookie.name

    addEventsAppExtraTags(cookiesHeader(onload.cookies = onload.cookies, eventId=eventId))
    loadPageCookiesHandler(eventId=eventId, function(cookies,nonce,..., app=getApp(),session=getAppSession(app)) {
      restore.point("loadLoginCookieHandler")

      app$..cookies.were.loaded = TRUE
      setLoadedCookies(cookies, app=app)
      cookie = cookies[[lop$cookie.name]]
      if (!is.null(cookie)) {
        app$..loginCookieReactive = reactiveValues(cookie=c(cookie, list(has.cookie = TRUE, nonce=nonce)))
      } else {
        app$..loginCookieReactive = reactiveValues(cookie=list(has.cookie = FALSE, nonce=nonce))
      }
      if (!is.null(lop$load.cookies.handler)) {
        lop$load.cookies.handler(cookies)
      }
    })
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

  # If we don't have a container.id
  # we don't show login inputs
  # but only use automatic inputs
  if (!is.null(container.id))
    lop.login.handlers(lop=lop)

  observe(priority = -100,x = {

    query <- parseQueryString(session$clientData$url_search)
    restore.point("loginDispatchObserver")

    # Don't authenticate again if authenticated
    # this is relevant since the observer
    # may be called once with the query string
    # and then once again when cookies are
    # loaded
    if (isTRUE(app$is.authenticated)) return()


    # If there is no query key and we allow cookies
    # but cookies are not yet loaded
    # wait for a small while
    if ((is.null(query[["key"]]) | lop$login.by.query.key == "no") & lop$login.by.cookie != "no" & app$..cookies.were.loaded == FALSE) {
      cat("\nWait until cookies are loaded...")
      shiny::invalidateLater(100)
      return()
    }


    cookie = app$..loginCookieReactive$cookie


    restore.point("loginDispatchObserver")

    if (lop$login.by.query.key == "allow" | lop$login.by.query.key == "require" | lop$login.by.cookie == "require" | (!is.null(cookie) & lop$login.by.cookie != "no")) {
      query.or.cookie = query
      use.query = TRUE
      # take information from cookie
      # if there is no query$key
      # or we are not allowed to use a cookie
      if (lop$login.by.cookie != "no" & !is.null(cookie) & (is.null(query[["key"]]) | lop$login.by.query.key == "no")) {
        use.query = FALSE
        query.or.cookie = cookie
      }

      res = login.by.query.key(lop,query = query.or.cookie)

      # successful login via url key
      if (res$ok) {
        restore.point("successfull.authenticated")
        cat("\nsuccessfully authenticated by", ifelse(use.query," query key.","cookie."))
        app$is.authenticated = TRUE
        if (is.null(lop$login.fun)) {
          stop("No login.fun defined.")
        }

        # set query as cookie
        # the cookie can be relevant if another
        # app from the same server is opened
        if (use.query & isTRUE(lop$set.successful.query.key.as.cookie)) {
          cat("\nset cookie key = ", query$key)
          setCookie(lop$cookie.name, list(key=query$key, code=query$code))
        }

        do.call(lop$login.fun, c(res$tok,list(lop=lop, tok=res$tok, login.mode=ifelse(use.query,"query","cookie") )))
        return(invisible())
      } else if (lop$login.by.query.key == "require" | lop$login.by.cookie=="require") {
        app$is.authenticated = FALSE
        cat("\nAuthentification failed: ", res$msg)

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

lop.db.arg = function(dbname=NULL,drv=SQLite(),...) {
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
