#' Create a new Webdriver session
#' 
#' This function creates a new Webdriver session with specificed capabilities
#' @param root a string  giving the Java-Server IP
#' @param browser a string giving the browser under which the session is started
#' @param takesScreenshot a logical value indicating whether the session supports taking screenshots of the current page
#' @param handlesAlerts a logical value indicating whether the session can interact with modal popups, such as window.alert and window.confirm
#' @param databaseEnabled a logical value indicating whether the session can interact database storage
#' @param cssSelectorsEnabled a logical value indicating whether the session supports CSS selectors when searching for elements
#' @param javascriptEnabled a logical value indicating whether the session supports executing user supplied JavaScript in the context of the current page
#' @return A list describing the session's capabilities.
#' @export
#' 
start_session <- function(root=NULL, browser = "firefox", javascriptEnabled=TRUE, takesScreenshot=TRUE, handlesAlerts=TRUE, databaseEnabled=TRUE, cssSelectorsEnabled=TRUE){
  
  server <- list(desiredCapabilities = list(browserName=browser,
                                            javascriptEnabled=javascriptEnabled, 
                                            takesScreenshot=takesScreenshot, 
                                            handlesAlerts=handlesAlerts, 
                                            databaseEnabled=databaseEnabled, 
                                            cssSelectorsEnabled=cssSelectorsEnabled))
  
  newSession <- getURL(paste0(root,"session"),
                       customrequest = "POST",
                       httpheader = c('Content-Type'='application/json;charset=UTF-8'),
                       postfields = toJSON(server))
  
  serverDetails <- fromJSON(rawToChar(getURLContent(paste0(root, "sessions"),
                                                    binary=TRUE)))
  sessionList <- list(time=Sys.time(), sessionURL = paste0(root, "session/", serverDetails$value[[1]]$id))
  
  class(sessionList) <- "RSelenium"
  print("Started new session. sessionList created.")
  seleniumSession <<- sessionList
}

#' Quite the current session
#' 
#' Deletes current session on the Webdriver Java-Server
#' @export
quit_session <- function(){
  invisible(getURL(paste0(seleniumSession$sessionURL),
                   customrequest = "DELETE",
                   httpheader = c('Content-Type'='application/json;charset=UTF-8')))
}


#' Retrieve status information on current session
#' 
#' Retrieves status information including sessions capabilities
#' @export
status <- function(){
  fromJSON(rawToChar(getURLContent(url = paste0(seleniumSession$sessionURL),
                                   customrequest = "GET",
                                   httpheader = c('Content-Type'='application/json;charset=UTF-8'),
                                   binary = T)))
}

#' Retrieve information on currently active session
#' 
#' Retrieves session information
#' @export
active_sessions <- function(){
  fromJSON(rawToChar(getURLContent(url = paste0(seleniumSession$sessionURL),
                                   customrequest = "GET",
                                   httpheader = c('Content-Type'='application/json;charset=UTF-8'),
                                   binary = T)))
}

#' Set implicit waiting time 
#' 
#' Set the amount of time the driver should wait when searching for elements
#' @param ms numerical value indicating the amount of time (in milliseconds) to wait for elements to appear on the page
#' @export
implicit_wait <- function(ms = 0){
  invisible(getURL(url=paste0(seleniumSession$sessionURL,"/timeouts", "/implicit_wait"),
                   customrequest = "POST",
                   httpheader = c('Content-Type' = 'application/json;charset=UTF-8'),
                   postfields = toJSON(list(ms = ms))))
}

#' Navigate to a new URL
#' 
#' Opens the specified webpage
#' @param url a string indicating the webpage URL
#' @export
post.url <- function(url = NULL){
  
  url <- ifelse(grepl("^(http)", url), url, paste0("http://", url))
  
  invisible(getURL(paste0(seleniumSession$sessionURL,"/url"),
                   customrequest = "POST",
                   httpheader = c('Content-Type' = 'application/json;charset=UTF-8'),
                   postfields = toJSON(list(url = url))))
}

#' Retrieve the URL of the current page
#' 
#' The currently opened webpage
#' @export
get.url <- function(){
  currentURL <- fromJSON(rawToChar(getURLContent(paste0(seleniumSession$sessionURL,"/url"),
                                                 customrequest = "GET",
                                                 httpheader = c('Content-Type' = 'application/json;charset=UTF-8'),
                                                 binary = T)))
  
  return(currentURL$value)
}

#' Retrieve the current window handle
#' 
#' Retrieves the handle for the activated window
#' @export
window_handle <- function(){
  windowHandle <- fromJSON(rawToChar(getURLContent(paste0(seleniumSession$sessionURL,"/window_handle"),
                                                   customrequest = "GET",
                                                   httpheader = c('Content-Type' = 'application/json;charset=UTF-8'), 
                                                   binary = T)))
  
  return(windowHandle$value)
}

#' Retrieve all window handle
#' 
#' Retrieve the list of all window handles available to the session
#' @export
window_handles <- function(){
  
  windowHandles <- fromJSON(rawToChar(getURLContent(paste0(seleniumSession$sessionURL,"/window_handles"),
                                                    customrequest = "GET",
                                                    httpheader = c('Content-Type' = 'application/json;charset=UTF-8'), 
                                                    binary = T)))
  
  return(windowHandles$value)
}

#' Maximize window size
#' 
#' Maximize the specified window if not already maximized.
#' @param handle a string indicating the handle of the window to be maximized
#' @export
window_max <- function(handle = NULL){
  invisible(getURL(paste0(seleniumSession$sessionURL,"/window/", handle, "/maximize"),
                   customrequest = "POST",
                   httpheader = c('Content-Type' = 'application/json;charset=UTF-8')))
}

#' Resize window
#' 
#' Change the size of the specified window
#' @param size a numeric vector (of length 2) that specifies width and height of the window 
#' @param handle a string indicating the handle of the window to be maximized
#' @export
post.window_size <- function(size = NULL, handle = NULL){
  
  if(is.null(handle)){handle <- window_handle()}
  
  invisible(getURL(paste0(seleniumSession$sessionURL,"/window/", handle, "/size"),
                   customrequest = "POST",
                   httpheader = c('Content-Type' = 'application/json;charset=UTF-8'),
                   postfields = toJSON(list(width = size[2], height = size[1]))))
}

#' Get window size
#' 
#' Get the size of the specified window.
#' @param handle a string indicating the handle of the window to be maximized
#' @export
get.window_size <- function(handle = NULL){
  
  if(is.null(handle)){handle <- window_handle()}
  
  currentSize <- fromJSON(rawToChar(getURLContent(paste0(seleniumSession$sessionURL,"/window/", handle, "/size"),
                                                  customrequest = "GET",
                                                  httpheader = c('Content-Type' = 'application/json;charset=UTF-8'),
                                                  binary = T)))
  
  return(unlist(currentSize$value[c("height", "width")]))	
}

#' Change position of a window
#' 
#' Get the size of the specified window.
#' @param position a numeric vector (of length 2) that specifies x and y coordinates for the new position
#' @param handle a string indicating the handle of the window to be maximized
#' @export
post.window_position <- function(position = NULL, handle = NULL){
  
  if(is.null(handle)){handle <- window_handle()}
  
  invisible(getURL(paste0(seleniumSession$sessionURL,"/window/", handle, "/position"),
                   customrequest = "POST",
                   httpheader = c('Content-Type' = 'application/json;charset=UTF-8'),
                   postfields = toJSON(list(x = position[1], y = position[2]))))
}

#' Get current position of a window
#' 
#' Get the position of the specified window
#' @param handle a string indicating the handle of the window to be maximized
#' @return a vector (of length 2) specifying x and y coordinates of the window
#' @export
get.window_position <- function(handle = NULL){
  
  if(is.null(handle)){handle <- window_handle()}
  
  windowPosition <- fromJSON(rawToChar(getURLContent(paste0(seleniumSession$sessionURL,"/window/", handle, "/position"),
                                                     customrequest = "GET",
                                                     httpheader = c('Content-Type' = 'application/json;charset=UTF-8'),
                                                     binary = T)))
  
  return(unlist(windowPosition$value[c("x", "y")]))
}

#' Get the page title
#' 
#' Retrieve the page title of the current page
#' @export
page_title <- function(){
  windowTitle <- fromJSON(rawToChar(getURLContent(paste0(seleniumSession$sessionURL, "/title"),
                                                  customrequest = "GET",
                                                  httpheader = c('Content-Type' = 'application/json;charset=UTF-8'),
                                                  binary = T)))
  
  return(windowTitle$value)	
}

#' Change window focus
#' 
#' Change focus to another window. The window to change focus to may be specified by its server assigned window handle, or by the value of its name attribute
#' @param handle a string indicating the handle of the window to be maximized
#' @export
window_change <- function(handle = NULL){
  
  if(is.null(handle)){stop("Missing handle")}
  
  rawToChar(getURLContent(paste0(seleniumSession$sessionURL,"/window"),
                          customrequest="POST",
                          httpheader=c('Content-Type'='application/json;charset=UTF-8'),
                          postfields = toJSON(list(name = handle))))
}

#' Close window
#' 
#' Close currently activated window
#' @export
window_close <- function(){
  
  getURL(paste0(seleniumSession$sessionURL,"/window"),
         customrequest="DELETE",
         httpheader=c('Content-Type'='application/json;charset=UTF-8'))
}

#' Refresh page
#' 
#' Refresh the current page
#' @export
page_refresh <- function(){
  invisible(getURL(paste0(seleniumSession$sessionURL,"/refresh"),
                   customrequest="POST",
                   httpheader=c('Content-Type'='application/json;charset=UTF-8')))
}

#' Navigate backwards
#' 
#' Navigate backwards in the browser history, if possible
#' @param times a numeric value indicating the number of pages to go back
#' @export
page_back <- function(times = 1){
  
  for(i in 1:times){
    invisible(getURL(paste0(seleniumSession$sessionURL,"/back"),
                     customrequest="POST",
                     httpheader=c('Content-Type'='application/json;charset=UTF-8')))
  }
}

#' Navigate forwards
#' 
#' Navigate forwards in the browser history, if possible
#' @param times a numeric value indicating the number of pages to go forward
#' @export
page_forward <- function(times = 1){
  
  for(i in 1:times){
    invisible(getURL(paste0(seleniumSession$sessionURL,"/forward"),
                     customrequest="POST",
                     httpheader=c('Content-Type'='application/json;charset=UTF-8')))
  }
}

#' Retrieve page source
#' 
#' Get the current page source
#' @export
#' @return a string containing the page source
page_source <- function(){
  source <- fromJSON(rawToChar(getURLContent(paste0(seleniumSession$sessionURL,"/source"),
                                             customrequest="GET",
                                             httpheader=c('Content-Type'='application/json;charset=UTF-8'),
                                             binary=T)))
  
  return(source$value)
}

#' Find an element via XPath
#' 
#' Search for an element on the page, starting from the document root
#' @param value a string specifying a XML Path Language statement
#' @export
#' @return ID element(s) that match the XPath expression
element_xpath_find <- function(value){
  
  elementDetails <- fromJSON(rawToChar(getURLContent(paste0(seleniumSession$sessionURL, "/elements"),
                                                     customrequest = "POST",
                                                     httpheader = c('Content-Type'='application/json;charset=UTF-8'),
                                                     postfields = toJSON(list(using = "xpath",value = value)),
                                                     binary = TRUE)))
  
  elementID <- elementDetails$value
  names(elementID) <- NULL
  class(elementID) <- "selenium.id"
  return(elementID)
}


#' Find an element via CSS selector
#' 
#' Search for an element on the page via XPath, starting from the document root
#' @param value a string specifying a CSS selector
#' @export
#' @return ID element(s) that match the CSS selector
element_css_find <- function(value){
  
  elementDetails <- fromJSON(rawToChar(getURLContent(paste0(seleniumSession$sessionURL, "/elements"),
                                                     customrequest = "POST",
                                                     httpheader = c('Content-Type'='application/json;charset=UTF-8'),
                                                     postfields = toJSON(list(using = "css",value = value)),
                                                     binary = TRUE)))
  
  elementID <- elementDetails$value
  names(elementID) <- NULL
  class(elementID) <- "selenium.id"
  return(elementID)
}

#' Find an element via partial text matching
#' 
#' Search for an element on the page via CSS selector, starting from the document root
#' @param value a string of text that is searched for in the page
#' @export
#' @return ID element(s) whose visible text partially matches the search value 
element_ptext_find <- function(value){
  
  elementDetails <- fromJSON(rawToChar(getURLContent(paste0(seleniumSession$sessionURL, "/elements"),
                                                     customrequest = "POST",
                                                     httpheader = c('Content-Type'='application/json;charset=UTF-8'),
                                                     postfields = toJSON(list(using = "partial link text",value = value)),
                                                     binary = TRUE)))
  
  elementID <- elementDetails$value
  names(elementID) <- NULL
  class(elementID) <- "selenium.id"
  return(elementID)
}


#' Click element
#' 
#' Click on a specified element
#' @param ID an ID element
#' @param times numerical value indicating the number of clicks
#' @param button a string indicating left or right mouse button
#' @export
element_click <- function(ID = ID, times = 1, button = "left"){
  
  if(length(ID)>1){
    ID = ID[[1]]
    warning("Multiple elements. Used first.")
  }
  
  if(button=="left"){button <- 0}else{
    if(button=="right"){button <- 2}else{
      button <- 1}
  }
  
  for(i in 1:times){
    rawToChar(getURLContent(paste0(seleniumSession$sessionURL, "/element/", ID, "/click"),
                            customrequest = "POST",
                            httpheader = c('Content-Type'='application/json;charset=UTF-8'),
                            postfields = toJSON(list(button = button)),
                            binary = T))
  }
}


#' Locate and determine element's screen position
#' 
#' Determine an element's location on the screen once it has been scrolled into view
#' @param ID an ID element on which the click is performed
#' @return a vector (of length 2) specifying x and y coordinates of the window
#' @export
liv <- function(ID = ID){
  
  if(length(ID)>1){
    ID = ID[[1]]
    warning("Multiple elements. Used first.")
  }
  
  elementLocation <- fromJSON(rawToChar(getURLContent(paste0(seleniumSession$sessionURL, "/element/", ID, "/location_in_view"),
                                                      customrequest = "GET",
                                                      httpheader = c('Content-Type'='application/json;charset=UTF-8'),
                                                      binary = T)))
  
  return(c(x=elementLocation$value$x, y=elementLocation$value$y))
}


#' Send keys
#' 
#' Send a sequence of key strokes to the active element
#' @param term a string that describe the key strokes to be send to the session
#' @export
keys <- function(term = NULL){
  getURLContent(paste0(seleniumSession$sessionURL, "/keys"),
                customrequest = "POST",
                httpheader = c('Content-Type'='application/json;charset=UTF-8'),
                postfields = toJSON(list(value = list("\uE009","a","\uE009",'\b', term))))
}


#' Clear element input
#' 
#' Clear a textarea or text input element's value.
#' @param ID an ID element
#' @export
element_clear <- function(ID = NULL){
  
  if(length(ID)>1){
    ID = ID[[1]]
    warning("Multiple elements. Used first.")
  }
  
  invisible(rawToChar(getURLContent(paste0(seleniumSession$sessionURL, "/element/", ID, "/clear"),
                          customrequest = "POST",
                          httpheader = c('Content-Type'='application/json;charset=UTF-8'),
                          binary = TRUE)))
}
