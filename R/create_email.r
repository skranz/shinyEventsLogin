examples.make.user = function() {
  random.password(nchar=6)
}

lop.default.reset.email.user.ui = function(lop,ns=lop$ns, ...) {
  restore.point("lop.default.reset.email.user.ui")
  sel = ids2sel(ns("lopCreateEmail"))

  widgets = list(
    HTML("<h3>Reset password for user account</h3>"),
    flexTextInput(ns("lopCreateEmail"), "Email", value = ""),
    actionButton(ns("lopResetEmailBtnBtn"), "Send email to confirm account reset.","data-form-selector"=sel),
    actionButton(ns("lopCancelBtn"), "Cancel"),
    uiOutput(ns("lopCreateInfo"))
  )
  ui = wellPanel(widgets)
  setUI(ns("lopCreateInfo"),"")

  ui
}


lop.default.create.email.user.ui = function(lop,ns=lop$ns, ...) {
  restore.point("lop.default.create.email.user.ui")
  sel = ids2sel(ns("lopCreateEmail"))

  widgets = list(
    HTML("<h3>Create new user account</h3>"),
    flexTextInput(ns("lopCreateEmail"), "Email", value = ""),
    actionButton(ns("lopCreateBtn"), "Send email to confirm account.","data-form-selector"=sel),
    actionButton(ns("lopCancelBtn"), "Cancel"),
    uiOutput(ns("lopCreateInfo"))
  )
  ui = wellPanel(widgets)
  setUI(ns("lopCreateInfo"),"")

  ui
}


lop.create.email.handlers = function(lop, ns=lop$ns,...) {
  buttonHandler(ns("lopCreateBtn"),create.email.user.click, lop=lop,no.authentication.required = TRUE, mode="create_user")
  buttonHandler(ns("lopResetEmailBtn"),create.email.user.click, lop=lop,no.authentication.required = TRUE, mode="reset_password")
  buttonHandler(ns("lopCancelBtn"),cancel.create.email.user.click, lop=lop,no.authentication.required = TRUE)

}

create.email.user.click = function(lop, ns=lop$ns, passwd.len=6,formValues,mode="create_user",...) {
  user = email = formValues[[ns("lopCreateEmail")]]
  if (isTRUE(lop$only.lowercase)) {
    user = tolower(user)
    email = tolower(email)
  }

  restore.point("create.email.user.click")

  if (is.null(lop$smtp)) {
    warning("lop$smtp not initialized")
    return(NULL)
  }

  if (!is.null(lop$check.email.fun)) {
    res = lop$check.email.fun(email)
    if (!res$ok) {
      show.html.warning(ns("lopCreateInfo"),res$msg)
      return(NULL)
    }
  }

  link = lop.create.link(userid=user,link_type=mode,lop=lop)
  res = lop$email.text.fun(lop,email,link)
  subject = res$subject; body = res$body; msg = res$msg

  mail = c(list(subject=subject,body=body,to=email), lop$smtp)
  if (!isTRUE(try(require(mailR)))) {
    show.html.message("Error mailR package is not installed! Cannot send email for sign-up.")
    return()
  }
  res = try(do.call(mailR::send.mail, mail))
  if (is(res,"try-error")) {
    show.html.message("An error occured while trying to send the sign-up email.")
    return()
  }


    do.call(mailR::send.mail, mail)

  show.html.message(ns("lopCreateInfo"),msg)
}


cancel.create.email.user.click = function(lop, ...) {
  restore.point("cancel.create.email.user.click")
  show.login.ui(lop)
}


lop.create.link = function(userid,link_type="confirm", lop=get.lop(), valid_secs=60*60*24*7) {
  restore.point("lop.create.link")

  linkid = random.password(nchar = 40)
  url = paste0(lop$app.url,"/?confirm=",linkid)
  create_time = Sys.time()
  valid_until = Sys.time() + valid_secs

  link = nlist(linkid, userid,link_type, url, create_time, valid_until, valid_secs)

  dbInsert(lop$conn,"loginlinks",link, mode="insert")

  link
}


default.email.text.fun = function(lop, email,link,...) {
  subject = paste0("Confirm user account for ", lop$app.title)

  body = paste0("
Hi,

you get this email, because you want to sign-up on ",lop$app.title," with this email adress. To confirm your user account and to choose a password, please follow the link below:\n\n ", link$url,
"\n\nIf you have not registred on ",lop$app.title,", someone else unsuccessfully tried to sign up with your email address. Then please ignore this email."
  )

  msg = paste0("I have send a confirmation email to ", email," from ",lop$smtp$from,".<br>The email contains a link to generate a password and activate your account.")

  nlist(subject, body, msg)
}
