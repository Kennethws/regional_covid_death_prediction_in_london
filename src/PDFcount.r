PDFcount <- function(FileName, Pages=TRUE) {
  #######################################################################
  #
  #   Function to count the number of words in a PDF file. This
  #   requires the pdftools and tm libraries to be installed. 
  #   Arguments:
  #
  #   FileName    Name of PDF file in current working directory
  #   Pages       Either TRUE to count words on all pages, or a 
  #               numeric vector giving the numbers of pages to 
  #               include.
  #
  #   The function returns a vector containing the total numbers 
  #   of words on each of the specified pages.
  #
  #   Example: if your report is in a file called "MyReport.pdf", 
  #   with text on the first 6 pages and graphs and tables on 
  #   pages 7 and 8, then you can get the required count by going
  #   
  #   PDFcount("MyReport.pdf",Pages=1:6)
  #
  #######################################################################
  library(pdftools); library(tm)
  Words <- unlist(pdf_text(FileName))[Pages]
  Words <- gsub("\\$"," ",Words) # Some PDFs have a dollar sign as a word separator
  Words <- stripWhitespace(Words)
  Words <- gsub("- ","",Words) # Join words that were split over two lines
  Words <- iconv(Words,"latin1","ASCII",sub="")
  Words <- removePunctuation(Words)
  Words <- stripWhitespace(Words)
  sapply(as.list(Words),FUN=function(x) length(unlist(strsplit(x," "))))
}
