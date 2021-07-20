# De vijf parameters hieronder zijn aan te passen
# Bij grote aantallen runs 'verbose' op 0 zetten, anders duurt het erg lang
#
# 2x2 abbreviations:
#
#                           condition positive (CP)  condition negative (CN)
#
#  predicted positive (PP)    true positive (TP)       false positive (FP)
#
#  predicted negative (PN)    false negative (FN)       true negative (TN)
#
#

# parameters, arbitrair gekozen:

group_size     <- 50    # individuals per initial blood test
positive_split <- 2     # split into x subgroups when group is positive
runs           <- 1000  # how many groups to simulate

prevalence     <- .02   # infection prevalence

sens           <- .70   # test sensitivity
spec           <- .95   # test specificity

verbose        <- 0     # show simulation output (2 = all, 1 = some, 0 = none)

#===========================================
# code below

# print function with verbosity
vcat <- function(level = 1, ...) {
  if(level <= verbose) {
    cat(...)
  }
}

# function to test one (sub)group
test_group <- function(samples, level = 0) {
  
  # print text
  spacer <- paste0(" ", strrep("  ", level))
  vcat(1, spacer,"testing",length(samples),"people: ")
  
  # keep count of how many tests we're using
  tests <<- tests+1
  
  # true outcome would be if "samples" contains at least one 1,
  # then take sens and spec into account
  true_condition <- sum(samples) > 0
  if(true_condition) outcome <- rbinom(1,1,sens) == 1
  else               outcome <- rbinom(1,1,spec) == 0
  
  # if the test result is positive:
  if(outcome) {
    
    if(true_condition) vcat(1, "true positive ✅☣️\n")
    else               vcat(1, "false positive ❗️☣️\n")
    
    # split into subgroups
    if(length(samples) > 1) {
      subgroups <- split(samples, ceiling(seq(length(samples))/(length(samples)/positive_split)))
      # test every subgroup and collect results
      ret <- c()
      for(g in subgroups) {
        vcat(2, spacer,"split:",g,"\n")
        ret <- append(ret,test_group(g, level+1))
      }
      # return results
      return(ret)
      
    } else {
      # return status 1 for current person
      return(c(1))
    }
  
  # if the test result is negative:
  } else {
    
    if(true_condition) vcat(1, "false negative ❗️✅️\n")
    else               vcat(1, "true negative ✅✅️\n")
    
    # return outcome 0 for all people in sample
    return(rep(0,length(samples)))
    
  }
  
}

# print info
total_people <- group_size*runs
cat("# Testing",total_people,"people in",runs,"groups of",group_size,"and splitting in",positive_split,"subgroups when positive\n")

# count number of tests
tests <- 0

# store results
results <- c(0,0,0,0) # TP, FP, FN, TN

# generate sample populations and start tests
for(i in 1:runs) {

  samples <- rbinom(group_size, 1, prevalence)  # create a group of random samples (0 = not infected, 1 = infected)
  vcat(1,"\nnew group:",group_size,"people,",sum(samples),"true positive(s)\n")  # show spoiler: true number of cases in group

  outcome <- test_group(samples) # start testing this group
  
  vcat(1,"true condition:",samples,"\n")
  vcat(1,"test result:   ",outcome,"\n")
  
  TP <- sum(outcome == TRUE & samples == outcome)
  FP <- sum(outcome == TRUE & samples != outcome)
  FN <- sum(outcome == FALSE & samples != outcome)
  TN <- sum(outcome == FALSE & samples == outcome)
  results <- results + c(TP, FP, FN, TN)
  
}

# print result
cat("\n# Results after everybody was tested once:\n")
cat("Predicted Positive: ",results[1]+results[2],"\n")
cat("Predicted Negative: ",results[3]+results[4],"\n\n")

cat("True Positives: ",results[1],"\n")
cat("False Positives:",results[2],"\n")
cat("False Negatives:",results[3],"\n")
cat("True Negatives: ",results[4],"\n\n")

#
# Second run of tests
#

# store results
results2 <- c(results[1],results[2],0,0) # TP, FP, FN, TN

# second test for negative people
cat("\n# Testing negative-tested people again, groupwise")

total <- results[3]+results[4]
pos <- results[3]
groups <- ceiling(total/group_size)
for(i in 1:groups) {
  size <- min(group_size, total)
  samples <- rbinom(size, 1, pos/total)  # create a group of random samples (0 = not infected, 1 = infected)
  vcat(1,"\nnew group: ",size," people,",sum(samples),"true positive(s)\n")  # show spoiler: true number of cases in group
  
  total <- total - size
  pos   <- pos - sum(samples)
  
  outcome <- test_group(samples)
  
  vcat(1,"true condition:",samples,"\n")
  vcat(1,"test result:   ",outcome,"\n")
  
  TP <- sum(outcome == TRUE & samples == outcome)
  FP <- sum(outcome == TRUE & samples != outcome)
  FN <- sum(outcome == FALSE & samples != outcome)
  TN <- sum(outcome == FALSE & samples == outcome)
  results2 <- results2 + c(TP, FP, FN, TN)
  
}

# print result
cat("\n# Results after those testing negative were tested again:\n")
cat("# (those tested postive still counted as positive)\n")
cat("Predicted Positive: ",results2[1]+results2[2],"\n")
cat("Predicted Negative: ",results2[3]+results2[4],"\n\n")

cat("True Positives: ",results2[1],"\n")
cat("False Positives:",results2[2],"\n")
cat("False Negatives:",results2[3],"\n")
cat("True Negatives: ",results2[4],"\n\n")

cat("Sensitivity:    ",results2[1]/(results2[1]+results2[3]),"\n")
cat("Specificity:    ",results2[4]/(results2[2]+results2[4]),"\n")
cat("PPV:            ",results2[1]/(results2[1]+results2[2]),"\n")
cat("NPV:            ",results2[4]/(results2[3]+results2[4]),"\n\n")

cat("Required",tests,'tests for',total_people,"people\n= 1 test per",total_people/tests,"diagnosed persons\n= 1 test per",(results2[3]+results2[4])/tests,"persons back to work\n")
