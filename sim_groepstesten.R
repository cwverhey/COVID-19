# De vijf parameters hieronder zijn aan te passen
# Bij grote aantallen runs 'verbose' op 0 zetten, anders duurt het erg lang

group_size     <- 50    # individuals per initial blood test
positive_split <- 2     # split into x subgroups when group is positive
runs           <- 5000  # how many groups to simulate

prevalence     <- .02   # infection prevalence

verbose        <- 0     # show simulation output (2 = all, 1 = some, 0 = none)

#===========================================

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
  
  # test if whole group is negative
  tests <<- tests+1
  if(sum(samples) == 0) {
    vcat(1, "negative ✅️\n")
    
    if(level == 0) {
      negative_groups <<- negative_groups + 1
    }
    
  } else {
    vcat(1, "positive ☣️\n")
    
    # split into subgroups
    if(length(samples) > 1) {
      subgroups <- split(samples, ceiling(seq(length(samples))/(length(samples)/positive_split)))
      for(g in subgroups) {
        vcat(2, spacer,"split:",g,"\n")
        test_group(g, level+1)
      }
    }
    
  }
  
}

# print info
total_people <- group_size*runs
cat("testing",total_people,"people in",runs,"groups of",group_size,"and splitting in",positive_split,"subgroups when positive\n")

# count number of tests
tests <- 0
negative_groups <- 0

# generate sample populations and start tests
for(i in 1:runs) {

  samples <- rbinom(group_size, 1, prevalence)  # create group from random samples (0 = not infected, 1 = infected)
  vcat(1,"\ngenerate new group of",group_size,"people,",sum(samples),"cases\n")  # show spoiler: true number of cases in group
  test_group(samples)                           # start testing this group
}

# print result
cat("\n\nRequired",tests,'tests for',total_people,"people\n= 1 test per",total_people/tests,"diagnosed persons\n= 1 test per",total_people/tests*(1-prevalence),"persons back to work\n
If you wouldn\'t retest positive subgroups:",negative_groups,"complete groups tested negative with",runs,"tests\n= 1 test per",negative_groups*group_size/runs,"persons who can go back to work.\n")
