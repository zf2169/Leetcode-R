##########31. Next Permutation##########
nextPermutation<- function(nums) {
  n<- length(nums)
  i<- n
  while(nums[i] <= nums[i-1]) {
    i<- i-1
  }
  if(i!=n) {
    ex<- nums[i:n]
    index<- which.min(ex[ex>nums[i-1]])
    a<- nums[i-1]
    nums[i-1]<- ex[index]
    ex[index]<- a
    ex<- sort(ex)
    return(c(nums[1:(i-1)], ex))
  }
  else {
    a<- nums[n]
    nums[n]<- nums[n-1]
    nums[n-1]<- a
    return(nums)
  }
}
nums<- c(6, 5, 4, 8, 7, 5, 1)
nextPermutation(nums)
nextPermutation(c(1,1,5))

##########32. Longest Valid Parentheses##########
Valid_Parentheses<- function(str) {
  punct_front<- c("(", "[", "{")
  punct_back<- c(")", "]", "}")
  x<-unlist(strsplit(str, split = ""))
  index_front<- which(x%in%punct_front)
  index_back<- which(x%in%punct_back)
  judge<- 0
  if(length(index_front)==length(index_back)) {  
    if(sum(index_front > index_back)==0) {
      judge<- 1
      n<- length(index_front)
      for(i in 1:n) {
        x_front<- x[index_front[n+1-i]]
        i_back<- min(index_back[index_back>index_front[n+1-i]])
        if(which(x_front %in% punct_front)!=
           which(x[i_back] %in% punct_back)) {
          judge<- 0
          break
        }
      }
    }
  }
  return(as.logical(judge))
}

longestValidParentheses<- function(s) {
  n<- nchar(s)
  ans<- ""
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      x<- substr(s, start = i, stop = j)
      judge<- Valid_Parentheses(x)
      if(judge & nchar(x)>nchar(ans)) {ans<- x}
    }
  }
  return(list(ans, nchar(ans)))
}
longestValidParentheses(")()())")
longestValidParentheses("(()")

##########33. Search in Rotated Sorted Array##########






