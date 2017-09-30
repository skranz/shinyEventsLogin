# Functionality to get and set cookies

cookies.login.example = function() {
  restore.point.options(display.restore.point=TRUE)
  app = eventsApp()

  app$ui = fluidPage(
    p("My app"),
    uiOutput("mainUI")
  )


  lop = loginModule(login.by.query.key = "require", fixed.query.key = "abcd",init.userid = "guest",login.by.cookie = "allow", container.id="mainUI", set.successful.query.key.as.cookie = TRUE, allowed.userids = "guest2",
    login.fun = function(...) {
      setUI("mainUI",p(paste0("Successful login.", sample.int(1000,1))))
    },
    login.failed.fun = function(...) {
      cat("\nlogin.failed")
      setUI("mainUI",p(paste0("Failed login.", sample.int(1000,1))))
    }
  )
  appInitHandler(function(...) {
    initLoginDispatch(lop)
  })
  viewApp(url.args = list(key="abcd"))
  viewApp(url.args = list())

  viewApp(url.args = list(key="abcd"), launch.browser = TRUE)


}

cookies.examples = function() {
  app = eventsApp()

  app$ui = fluidPage(
    cookiesHeader(onload.cookies="mycookie"),
    actionButton("setCookieBtn","Set Cookie"),
    actionButton("getCookieBtn","Get Cookie"),
    uiOutput("uiCookie")
  )
  customEventHandler("getCookieClick",id = NULL,css.locator = "#getCookieBtn", event="click",extra.shiny.value.code = "hello: 'Help', cookies: Cookies.getJSON()",fun = function(cookies,...) {
    args = list(...)
    restore.point("get.cookie.click")
    txt = cookies$mycookie$txt
    ui = HTML(paste0("mycookie$txt: ",txt))
    setUI("uiCookie",ui)
    dsetUI("uiCookie",ui)

    cat("\nget.cookie.click")
  })

  buttonHandler("setCookieBtn", function(...) {
    setCookie("mycookie",list(txt=paste0("mycookie", sample.int(10000,1))))
    cat("\ncookie set")
  })
  loadPageCookiesHandler(fun= function(cookies,...) {
    cat("\nLoad page cookies...")
    restore.point("loadPageCookies")
    cookie = cookies$mycookie
    if (!is.null(cookie)) {
      ui = HTML(cookie$txt)
      setUI("uiCookie",ui)
      dsetUI("uiCookie",ui)
    }
  })

  #viewApp(app, launch.browser = TRUE)

  viewApp(app)
}

loadPageCookiesHandler = function(fun, eventId="loadPageCookies",no.authentication.required=TRUE) {
  eventHandler(eventId=eventId,id=eventId, fun=fun,no.authentication.required = no.authentication.required)
}

cookiesHeader = function(onload.cookies=NULL, eventId="loadPageCookies",...) {
  library(htmltools)
  src = c(file=system.file("www", package="shinyEventsLogin"))
  js = NULL
  if (!is.null(onload.cookies)) {
    js = paste0('
$(document).on("shiny:sessioninitialized", function(event) {
  shinyEventsSendCookies("', eventId,'",[',paste0('"', onload.cookies,'"', collapse=","),']);
});
  ')
    js = singleton(tags$script(HTML(js)))
  }
  tagList(
    htmlDependency("js.cookie",version="2.1.4",src=src,script = "js.cookie.js"),
    htmlDependency("shiny.events.cookies",version="0.1",src=src,script = "shiny.events.cookies.js"),
    js
  )
}

setCookie = function(id, values) {
  restore.point("setCookie")
  callJS("Cookies.set", id,values);
}

