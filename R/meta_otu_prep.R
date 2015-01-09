#' A Normalization by participant function. 
#'
#' This function normalizes the raw OTU counts by participant.
#' @param df is otu.normed
#' @keywords error match sort
#' @export
#' @examples
#' meta_ID_match, meta_ID_error90, meta_otu_sort 

#
# **************************************************************************** #
# ***************                 meta_group_sort.R            *************** #
# **************************************************************************** #

# first sort metadata according to groups (ex. lean --> obese)

meta_group_sort <- function(data, variable) {

# Sort metadata according to groups
  data.sort=data[order(variable),]
  
  return(data.sort)
  
} # END FUNCTION

# Test: works!
# meta_group_sort(meta, meta$bmi_group)

# **************************************************************************** #
# ***************                 meta_otu_sort.R              *************** #
# **************************************************************************** #

meta_otu_sort <- function(otu_table,meta_sample_id) {
  
  # Sort otu_table according to vector of ID's  
  otu.sort=otu_table[,match(meta_sample_id, names(otu_table))]
    
  return(otu.sort)
}
  
# Test: works!  
# df=meta_otu_sort(otu.species, sample.name)


# **************************************************************************** #
# ***************                 meta_otu_sort.R              *************** #
# **************************************************************************** #
  
  
# **************************************************************************** #
# ***************                 meta_otu_sort.R              *************** #
# **************************************************************************** #

# Groups
#groups=as.factor(as.numeric(metadata.8infants.sort$Maternal_group));groups
#group.names=unique(as.character(metadata.8infants.sort$Maternal_group))

# Names of samples
#sample.name=as.character(metadata.8infants.sort$study_id);sample.name

# Number of samples
#sample.count=length(metadata.8infants.sort$study_id);sample.count



# **************************************************************************** #
# ***************                 meta_ID_match.R            *************** #
# **************************************************************************** #


meta_ID_match <- function(data, otu) {
  
  # Sort metadata according to groups
  data.match=subset(data, !(is.na(pmatch(data$study_id, names(otu)))))
  return(data.match)}
  
meta_ID_error <- function(data, otu) {
    
    # Sort metadata according to groups
    data.error=subset(data, (is.na(pmatch(data$study_id, names(otu)))))
    return(data.error)}

 # END FUNCTION

# Test: works!
#meta_ID_match(meta, otu.species); 


