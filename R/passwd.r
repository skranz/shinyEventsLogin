
examples.make.password.hash = function() {
  make.password.hash(user="jondoe",password= "password")
  res = make.password.hash(user="jondoe",password= "password")
  pw.df = as.data.frame(res)

  check.password(user="jondoe",password="password",pw.df=pw.df)

}


make.salt = function(bytes=100) {
  salt = paste0(rand_bytes(bytes),collapse="")
  salt
}

make.password.hash = function(password, salt =  make.salt(), hash.fun=sha512) {
  restore.point("make.password.hash")
  raw = paste0(password, salt)
  hash = hash.fun(raw)
  list(hash=hash, salt=salt)
}

check.password = function(password,salt,hash, hash.fun=sha512) {
  restore.point("check.password")
  cur.hash = make.password.hash(password, salt=salt, hash.fun=hash.fun)$hash
  if (hash==cur.hash)
    return(TRUE)
  return(FALSE)
}

random.password <- function(nchar=8, chars = c(setdiff(0:9,0), setdiff(letters,"o"), setdiff(LETTERS,"O")))
{
  paste0(sample(chars,nchar, replace=TRUE),collapse="")
}
