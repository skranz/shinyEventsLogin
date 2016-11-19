
fill.defaults = function(li, def.li) {
  restore.point("fill.defaults")
  if (is.null(li)) li = list()
  all.names = unique(c(names(def.li),names(li)))
  missing.names = setdiff(names(def.li),names(li))
  li[missing.names] = def.li[missing.names]
  li[all.names]
}

nlist=function (...)
{
    li = list(...)
    li.names = names(li)
    names = unlist(as.list(match.call())[-1])
    if (!is.null(li.names)) {
        no.names = li.names == ""
        names(li)[no.names] = names[no.names]
    }
    else {
        names(li) = names
    }
    li
}

set.list = function(li, new) {
  li[names(new)] = new
  li
}

to.datetime = function(x, origin="1970-01-01") {
  as.POSIXct(x)
}
