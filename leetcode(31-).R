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
search<- function(nums, target) {
  return(ifelse(target %in% nums, which(target == nums), -1))
}
search(c(4,5,6,7,0,1,2), 6)
search(c(4,5,6,7,0,1,2), 8)

##########34. Search for a Range##########
searchRange<- function(nums, target) {
  if(target %in% nums) {return(which(nums == target))}
  else {return(c(-1,-1))}
}
searchRange(c(5, 7, 7, 8, 8, 10), 8)
searchRange(c(5, 7, 7, 8, 8, 10), 2)

##########35. Search Insert Position##########
searchInsert<- function(nums, target) {
  if(!(target %in% nums)) {nums<- sort(c(nums, target))}
  return(which(nums==target)-1)
}
n<- c(1,3,5,6)
searchInsert(n, 5)
searchInsert(n, 2)
searchInsert(n, 7)
searchInsert(n, 0)

##########36. Valid Sudoku##########
isValidSudoku<- function(sudoku) {
  valid<- function(nums) {
    nums<- as.numeric(nums[-which(nums==".")])
    judge<- 0
    if(sum(duplicated(nums))==0) {
      if(sum(nums>9)==0) {
        judge<- 1
      }
    }
    return(judge)
  }
  j1<- NULL
  for(i in c(1,3,6)) {
    for(j in c(1,3,6)) {
      x<- c(sudoku[i,j:(j+2)], sudoku[(i+1),j:(j+2)],
            sudoku[(i+2), j:(j+2)])
      j1<- rbind(j,valid(x))
    }
  }
  j2<- apply(sudoku, 1, valid)
  j3<- apply(sudoku, 2, valid)
  if(sum(!j1,!j2,!j3)==0) {return("is valid")}
  else {return("not valid")}
}

sudoku<- matrix(c(5,6,".",8,4,7,".",".",".",
              3,".",9,".",".",".",6,".",".",
              ".",".",8,".",".",".",".",".",".",
              ".","1",".",".","8",".",".","4",".",
              "7","9",".","6",".","2",".","1","8",
              ".","5",".",".","3",".",".","9",".",
              ".",".",".",".",".",".","2",".",".",
              ".",".","6",".",".",".","8",".","7",
              ".",".",".",3,1,6,".",5,9), nrow = 9)
isValidSudoku(sudoku)

sudoku2<- matrix(c(8,6,".",8,4,7,".",".",".",
                  3,".",9,".",".",".",6,".",".",
                  ".",".",8,".",".",".",".",".",".",
                  ".","1",".",".","8",".",".","4",".",
                  "7","9",".","6",".","2",".","1","8",
                  ".","5",".",".","3",".",".","9",".",
                  ".",".",".",".",".",".","2",".",".",
                  ".",".","6",".",".",".","8",".","7",
                  ".",".",".",3,1,6,".",5,9), nrow = 9)
isValidSudoku(sudoku2)

##########37. Sudoku Solver##########

##########38. Count and Say##########
countAndSay<- function(n) {
  count<- function(nums) {
    nums<- unlist(strsplit(as.character(nums), split = ""))
    n<- length(nums)
    l<- 1
    a<- 1
    b<- nums[1]
    for(i in 1:(n-1)) {
      if(nums[i+1]==nums[i]) {
        a[l]<- a[l]+1
        b[l]<- nums[i]
        }
      else {
        l<- l+1
        a[l]<- 1
        b[l]<- nums[i+1]
        }
    }
    return(as.numeric(paste(a, b,sep = "", collapse = "")))
  }
  if(n==1) {return(1)}
  if(n==2) {return(11)}
  else {
    ans<- 11
    for(i in 2:(n-1)) {
      ans<- count(ans)
    }
  }
  return(ans)
}
countAndSay(4)
countAndSay(5)
countAndSay(6)
countAndSay(7)

##########39. Combination Sum##########
combinationSum<- function(candidates, target) {
  library(gtools)
  candidates<- sort(candidates)
  n<- floor(target/candidates[1])
  ans<- vector("list")
  k<- 1
  for(i in 1:n) {
    index<- permutations(n=length(candidates),r=i, repeats.allowed = T)
    ans[[k]]<- candidates[index[which(rowSums(matrix(candidates[index],ncol = i))
            == target),]]
    if(length(ans[[k]])==0) {next}
    else {
      ans[[k]]<- t(apply(matrix(ans[[k]], ncol = i), 1, sort))
      ans[[k]]<- ans[[k]][!duplicated(ans[[k]]),]
      k<- k+1
    }
  }
  return(ans)
}
combinationSum(c(2,3,6,7), 7)
combinationSum(c(2,3,6,7), 6)
combinationSum(c(2,3,6,7), 15)

##########40. Combination Sum II##########
combinationSum2<- function(candidates, target) {
  library(gtools)
  candidates<- sort(candidates)
  n<- length(candidates)
  ans<- vector("list")
  k<- 1
  for(i in 1:n) {
    index<- permutations(n=n,r=i)
    ans[[k]]<- candidates[index[which(rowSums(matrix(candidates[index],ncol = i))
                                      == target),]]
    if(length(ans[[k]])==0) {next}
    else {
      ans[[k]]<- t(apply(matrix(ans[[k]], ncol = i), 1, sort))
      ans[[k]]<- ans[[k]][!duplicated(ans[[k]]),]
      k<- k+1
    }
  }
  return(ans)
}
combinationSum2(c(10, 1, 2, 7, 6, 1, 5), 8)
combinationSum(c(2,3,6,7), 6)
combinationSum(c(2,3,6,7), 15)

##########41. First Missing Positive#########
firstMissingPositive<- function(nums) {
  nums<- sort(nums)
  f<- which(nums>0)[1] #extra constant space
  for(i in f:length(nums)) {
    if(nums[i]!= (i-f+1)) { break}
  }
  if(i==length(nums)) {return(i-f+2)}
  else {return(i-f+1)}
}
firstMissingPositive(c(3,1,2,0))
firstMissingPositive(c(3,4,-1,1,-2))

##########42. Trapping Rain Water#########
trap<- function(height) {
  n<- length(height)
  i<- 1
  max_left<-0
  max_right<- 0
  ans<- 0
  plot(c(0, rep(1:(n-1),rep(2,(n-1))), n), rep(height, rep(2,n)), type = "l")
  while(i<n) {
    if(height[i]<= height[n]) {
      max_left<- max(max_left, height[i])
      ans<- ans+(max_left- height[i])
      i<- i+1
    }
    else {
      max_right<- max(max_right, height[n])
      ans<- ans+(max_right- height[n])
      n<- n-1
    }
  }
  
  return(ans)
}
trap(c(0,1,0,2,1,0,1,3,2,1,2,1))
trap(c(0,1,0,0,1,0,1,3,2,1,2,1))





