CheckClassification <-
function(group, n) {
  # 
  # Checks classification for problems, terminates program if necessary.
  #
  # Args:
  #   group: Classification of cases (vector).
  #       n: Sample size (scalar).
  #
  # Returns:
  #   Nothing; text output if problem occurs.
  # 
  if (sum(group == 1) + sum(group == 2) < n) {
    stop("* Final column needs to classify cases (1 = complement, 2 = taxon)\n")
  }
  if (sum(group == 1) == 0) {
    stop("* No cases assigned to complement (coded as 1)\n")
  }
  if (sum(group == 2) == 0) {
    stop("* No cases assigned to taxon (coded as 2)\n")
  }
}
