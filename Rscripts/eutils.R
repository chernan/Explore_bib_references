
# Functions dedicated to retrieve information using the eutils API
#
# See documentation on the NCBI website
# https://dataguide.nlm.nih.gov/eutilities/utilities.html
# 
# Some of the code was inspired by this blog entry :
# Retrieve pubmed citation data, Ewen Harrison, March 2013, www.datasurg.net
# http://www.datasurg.net/2013/03/31/r-function-to-retrieve-pubmed-citations-from-pmid-number/?utm_source=rss&utm_medium=rss&utm_campaign=r-function-to-retrieve-pubmed-citations-from-pmid-number


# After a call to source() these functions will be available in the eutils environment.
eutils = new.env()

#------------------------------
# Wrappers
#------------------------------

# Function to fetch, parse and extract medline citation data.
eutils$fetch_PUBMED_info <- function(to_fetch){
    require(RCurl)
    require(XML)
    require(plyr)
    
    # Post PMID (UID) numbers
    url <- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=", to_fetch, "&retmode=XML", sep="")
    
    # Medline fetch
    fetched_data <- llply(url,
                          .progress = progress_tk(label = "Fetching and parse Pubmed records ..."),
                          function(x) {
                              xmlTreeParse(rawToChar(GET(x)$content), useInternalNodes = TRUE)
                          })
    # Using given format and xml tree structure, paste here the specific fields you wish to extract
    # example: https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=9989491&retmode=XML
    iddata <- ldply(
        fetched_data,
        .progress = progress_tk(label = "Creating dataframe ..."),
        function(x) {
            a <- getNodeSet(x, "/PubmedArticleSet/*/MedlineCitation")
            pmid_l <- sapply (a, function(a) xpathSApply(a, "./PMID", xmlValue))
            yearJ_l <- sapply (a, function(a) xpathSApply(a, "./Article/Journal/JournalIssue/PubDate/Year", xmlValue))
            yearA_l <- sapply (a, function(a) xpathSApply(a, "./Article/ArticleDate/Year", xmlValue))
            journal_l <- sapply (a, function(a) xpathSApply(a, "./Article/Journal/Title", xmlValue))
            title_l <- sapply (a, function(a) xpathSApply(a, "./Article/ArticleTitle", xmlValue))
            author_l <- sapply (a, function(a) xpathSApply(a, "./Article/AuthorList/Author[1]/LastName", xmlValue))
            references_l <- sapply (a, function(a) xpathSApply(a, "./NumberOfReferences", xmlValue))
            return(data.frame(pmid = to_pasted_string(pmid_l), 
                              yearJ = to_pasted_string(yearJ_l), 
                              yearA = to_pasted_string(yearA_l),
                              journal = to_pasted_string(journal_l), 
                              title = to_pasted_string(title_l), 
                              author = to_pasted_string(author_l), 
                              references = to_pasted_string(references_l),
                              stringsAsFactors = FALSE))
        })
    
    return(iddata)
}

eutils$fetch_PMC_citations <- function(to_fetch) {
    require(RCurl)
    require(XML)
    require(plyr)
    
    # Post PMID (UID) numbers to retrieve citations in Pubmed central
    url <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&id=", to_fetch, "&cmd=neighbor&linkname=pubmed_pmc_refs", sep="")
    
    # Fetch citations
    fetched_data <- llply(url,
                          .progress = progress_tk(label = "Fetching and parse citations ..."),
                          function(x) {
                              xmlTreeParse(rawToChar(GET(x)$content), useInternalNodes = TRUE)
                          })
    
    
    # Using given format and xml tree structure, paste here the specific fields you wish to extract
    # example: https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&id=9989491&cmd=neighbor&linkname=pubmed_pmc_refs
    citation_data <- ldply(
        fetched_data,
        .progress = progress_tk(label = "Creating second dataframe ..."),
        function(x) {
            a <- getNodeSet(x, "/eLinkResult/LinkSet")
            pmid_l <- sapply (a, function(a) xpathSApply(a, "./IdList/Id", xmlValue))
            citations_l <- sapply (a, function(a) xpathSApply(a, "./LinkSetDb/Link/Id", xmlValue))
            return(data.frame(cpmid = to_pasted_string(pmid_l),
                              citationsnb = ifelse(class(citations_l)=="list" && length(citations_l[[1]])==0, 0, length(citations_l)), 
                              citations = to_pasted_string(citations_l),
                              stringsAsFactors = FALSE))
        })
    
    return(citation_data)
}

#------------------------------
# Utilities
#------------------------------


# Batch a vector of PMIDs into groups of 20
eutils$split_into_batch <- function(pmid=pmid, max=20){

    pmid <- pmid[!is.na(pmid) && pmid!=""]
    pmid_seq <- seq_along(pmid)
    return(split(pmid, ceiling(pmid_seq/max)))

}

# Simple function to manage data returned by eutils
# Note that if data is void, it is returned by eutils as a list of one empty element (thus the return(NA) if length is 0)
# And if data is not empty, it is either a matrix or a character object which can be directly returned.
eutils$to_pasted_string <- function(data) {

    if(class(data)=="matrix") { 
        return(paste0(as.vector(data), collapse = ":"))
    }
    if(class(data)=="list") { 
        if (length(data)==0 || length(data[[1]])==0) return(NA)
        return(paste0(unlist(data), collapse = ":"))
    }
    if(class(data)=="NULL") { 
        return(NA)
    }
    return(data)
    
}



while("eutils" %in% search())
    detach("eutils")
attach(eutils)