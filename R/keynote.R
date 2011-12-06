library(Rcompression)
library(XML)

if(FALSE) {

 k = keynote("inst/sampleDocs/Day1.key")
}


setClass("KeynoteDoc", contains = "ZipFileArchive")

KeynoteNS =
  structure(c("http://developer.apple.com/namespaces/sfa", "http://developer.apple.com/namespaces/sf", 
            "http://www.w3.org/2001/XMLSchema-instance", "http://developer.apple.com/namespaces/keynote2"),
           .Names = c("sfa", "sf", "xsi", "key"), class = c("SimplifiedXMLNamespaceDefinitions", "XMLNamespaceDefinitions"))

keynote =
function(filename, class = "KeynoteDoc")
{
  zipArchive(filename, class = class)
}

setAs("character", "KeynoteDoc",
       function(from)
           keynote(from))

setGeneric('getDoc', function(x, ...) standardGeneric("getDoc"))
setMethod("getDoc", "character", 
function(x, ...)
{  
  getDoc(as(x, "KeynoteDoc"), ...)
})

setMethod("getDoc", "KeynoteDoc",
           function(x, ...)
               xmlParse(x[["index.apxl"]])
          )
setClass("APXL", contains = "XMLInternalDocument")
#setAs("")


setGeneric("getSlideNodes", function(doc, ...) standardGeneric("getSlideNodes"))

setMethod("getSlideNodes", "character",
    function(doc, ...)
      getSlideNodes(as(doc, "KeynoteDoc"), ...))

setMethod("getSlideNodes", "KeynoteDoc",
    function(doc, ...)
      getSlideNodes(getDoc(doc), ...))

setMethod("getSlideNodes", "XMLInternalDocument",
function(doc, ...)
{
  ans = getNodeSet(doc, "//key:slide-list", KeynoteNS)
  if(length(ans))
    ans[[1]]
  else
    list()
})


setGeneric("getText", function(doc, ...) standardGeneric("getText"))

setMethod("getText", "character",
    function(doc, ...)
      getText(as(doc, "KeynoteDoc"), ...))

setMethod("getText", "KeynoteDoc",
    function(doc, ...) {
       slides = getSlideNodes(getDoc(doc), ...)
       xmlApply(slides, getText)
    })

setMethod("getText", "XMLInternalNode",
    function(doc, ...) {
       b = doc[["body-placeholder"]]
       tt = getNodeSet(b, ".//sf:text-body", KeynoteNS)
       if(length(tt) == 0)
          return(character())

       xmlSApply(tt[[1]], xmlValue)
  })

#getText =function()  
