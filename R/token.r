# We can also perform login via tokens
# the query in the app url may contain fields key and code
# If we allow or require login via token
# look in the token directory if a field with filename key exists
# if the file exists load it
# token files are simple json format.
# Its fields, e.g. userid will be passed to the login function
# If it contains a field code, compare whether the code is the same
# code than in the query. Otherwise deny access.

examples.token.login = function() {
  token.dir = "D:/libraries/shinyEventsLogin/tokens"
  tok = list(userid="test", validUntil = as.integer(Sys.time())+60*60)
  key =write.login.token(tok, token.dir, key="testkey")
  query = c(list(key=key),tok)
  check.login.token(token.dir, query)


  app = eventsApp()

  lop = loginModule(app.title="Login Test",container.id = "mainUI", init.userid = "", init.password = "", need.password = TRUE, need.userid=TRUE, fixed.password="Omkoo", use.signup=TRUE,login.by.query.key = "require",fixed.query.key = "hurra",token.dir = token.dir, login.fun = function(...) {
    setUI("mainUI",p("Successful log-in!"))
  }
  )


  appInitHandler(function(session,...) {
    initLoginDispatch(lop)
  })

  app$ui = fluidPage(uiOutput("mainUI"))
  app$lop = lop
  restore.point.options(display.restore.point = TRUE)
  viewApp(app)

}

login.by.query.key = function(lop, query) {
  restore.point("login.by.query.key")

  # If a fixed key is provided us it instead of tokens
  if (!is.null(lop[["fixed.query.key"]])) {
    if (isTRUE(lop$fixed.query.key == query[["key"]])) {
      return(list(ok=TRUE, tok=list()))
    } else {
      return(list(ok=FALSE, msg = "Wrong key provided in the url."))
    }
  }

  # no fixed dir => use tokens
  token.dir = lop[["token.dir"]]
  if (is.null(token.dir)) {
    return(list(ok=FALSE, msg = "Error: The app specifies login via url key but does neither provide a key nor a token directory."))
  }

  return(check.login.token(token.dir,query))

}

check.login.token = function(token.dir, query) {
  key = query[["key"]]
  if (is.null(key)) return(list(ok=FALSE, msg="No key provided in the url."))

  key.file = file.path(token.dir,key)

  if (!file.exists(key.file))
    return(list(ok=FALSE, msg="Wrong key provided in the url."))

  json = readLines(key.file,warn=FALSE)
  tok = fromJSON(json)

  if (!is.null(tok[["code"]])) {
    if (!isTRUE(query[["code"]] == tok$code)) {
      return(list(ok=FALSE, msg="No correct code provided in url."))
    }
  }

  if (!is.null(tok[["validUntil"]])) {
    validUntil = as.integer(tok$validUntil)
    if (validUntil < as.integer(Sys.time())) {
      return(list(ok=FALSE, msg="Access key is no longer valid."))
    }
  }

  return(list(ok=TRUE, tok=tok))

}

write.login.token = function(tok, token.dir, key = make.login.token.key(tok=tok)) {
  json = toJSON(tok)
  writeLines(json, file.path(token.dir, key))
  key
}

make.login.token.key = function(tok=NULL, userid=tok[["userid"]], validUntil=tok[["validUntil"]], nchar=120) {
  key = random.string(1,nchar)
  if (is.null(userid)) {
    userhash=""
  } else {
    userhash=digest(userid)
  }
  if (is.null(validUntil)) {
    validUntil = ""
  }
  key = paste0(validUntil,"_",key,"_",userhash)
  key
}

random.string = function(n=1,nchar=14) {
  chars = sample(c(letters,LETTERS,0:9),nchar*n, replace = TRUE)
  if (n == 1) return(paste0(chars, collapse=""))
  mat = as.data.frame(matrix(chars, n, nchar))
  do.call(paste0,mat)
}