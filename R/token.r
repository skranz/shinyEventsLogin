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
  key = write.login.token(tok, token.dir, key="testkey")
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

  res = check.login.token(token.dir,query)

  # Check if userid is allowed
  if (res$ok & !is.null(lop$allowed.userids)) {
    if (!isTRUE(res$tok$userid %in% lop$allowed.userids)) {
      return(list(ok=FALSE, msg = "Your URL query / cookie refers to a user that has no access to this application."))
    }
  }

  return(res)
}

check.login.token = function(token.dir, query) {
  restore.point("check.login.token")
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

write.login.token = function(tok, token.dir, key = make.login.token.key(tok=tok),code=NULL) {
  json = toJSON(tok)
  writeLines(json, file.path(token.dir, key))
  key
}

#' Create a typical login token
#'
#' You can also manually create a token
#' It must be a list that has at least the element key
#' Generally, you will also specify a userid
#' If the token is valid for a limited time, you should
#' at a validUntil (a datetime stored as integer) value
#'
#' You can add a code to add an extra field besides key that
#' will be stored inside the file. The key will be part
#' of a tokens file name, the code will only be stored inside the
#' token file.
#' Hence using a code in addition to the allows longer codes in
#' the URL without running in restrictions of file name length
make.login.token = function(userid=NULL, key=NULL, code=NULL, validUntil=NULL, validMinutes=NULL, nchar.key = 120, make.key=is.null(key)) {
  restore.point("make.login.token")

  tok = list(userid=userid,key=key)
  if (!is.null(validMinutes)) {
    validUntil = as.integer(Sys.time()) + as.integer(validMinutes)*60L
  }
  if (!is.null(code)) tok$code = code
  if (!is.null(validUntil))
    tok$validUntil = as.integer(validUntil)
  if (make.key)
    tok$key = make.login.token.key(tok=tok, nchar=nchar.key)
  tok
}

# Remove expired login tokens assuming
# The key follows the canonical namining konvention
remove.expired.login.tokens = function(token.dir) {
  restore.point("remove.expired.login.tokens")
  if (is.null(token.dir)) return()

  files = list.files(token.dir)
  files = files[substring(files,1,3)=="_vu"]

  time = str.left.of(files,"_", not.found=rep(NA, length(files)))
  files = files[!is.na(time)]
  time = time[!is.na(time)]
  time = as.integer(substring(time,3))

  expired = time < as.integer(Sys.time())
  files = files[expired]

  try(file.remove(file.path(token.dir, files)))

}

make.login.token.key = function(tok=NULL, userid=tok[["userid"]], validUntil=tok[["validUntil"]], key=tok$key, nchar=60) {
  if (!is.null(key))
    return(key)
  key = random.string(1,nchar)
  if (is.null(userid)) {
    userhash=""
  } else {
    userhash=digest(userid)
  }
  if (is.null(validUntil)) {
    validUntil = ""
  } else {
    validUntil = paste0("vu",validUntil)
  }
  key = paste0("_",validUntil,"_",key,"_",userhash)
  key
}

token.login.url = function(base.url,tok=NULL,key=tok$key, code=tok$code) {
  if (is.null(code)) {
    url = paste0(base.url,"?key=",key)
  } else {
    url = paste0(base.url,"?code=",key,"&key=",key)
  }
  url
}

random.string = function(n=1,nchar=14) {
  chars = sample(c(letters,LETTERS,0:9),nchar*n, replace = TRUE)
  if (n == 1) return(paste0(chars, collapse=""))
  mat = as.data.frame(matrix(chars, n, nchar))
  do.call(paste0,mat)
}

# Set a browser cookie that allows automatic
# login based on the provided login token
# Can be useful if one app (like TeacherHub)
# where the user has logged in
# starts another app (like presenterApp) for
# which we automatically want to log in
set.login.token.cookie = function(tok, cookieId="shinyEventsLoginCookie", expires=NULL) {
  setCookie(cookieId, list(key=tok$key, code=tok$code), expires=expires)
}