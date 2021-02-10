
#' https://datastudio.google.com/embed/u/0/reporting/1PLVi5amcc_R5Gh928gTE8-8r8-fLXJQF/page/0dbJB
#' adjust start date to 1 APR
#' => copy the body element to parse_pak_dashboard.htm

require(rvest)

pg <- read_html("pak_dashboard.htm")
#' get the chart nodes
nds <- html_nodes(pg, "svg[aria-label='A chart.']")

#' only need info from the charts about
#' TODO: extract color for the relevant charts
#' tests = 81c784 (rect)
#' cum cases = 64b5f6 (rect)
#' cum deaths = ff0000 (line)

#' @examples 
#' lapply(nds, function(nd) html_text(html_nodes(nd, "text")))
#' # indicates 10 is Total Tests plot

write_html(nds[[10]], "test.svg") 
