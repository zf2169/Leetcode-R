##########61. Rotate List##########
rotateRight<- function(head, k) {
  n<- length(head)
  r<- k %% n
  return(c(head[(n-r+1):n], head[1:(n-r)]))
}
rotateRight(c(1,2,3,4,5), 2)

##########62. Unique Paths##########
uniquePaths_method1<- function(m, n) {
  #the number of paths to one grid is the sum of paths of the
  #grid on its left and the grid above it
  ans<- matrix(NA, m, n)
  for(i in 1:m) {
    for(j in 1:n) {
      if(i==1||j==1) {ans[i,j]<- 1}
      else { ans[i,j]<- ans[i-1,j]+ ans[i,j-1] }
    }
  }
  return(ans[m,n])
} 
uniquePaths_method1(6,7)

# in permutation and combination, the problem can be easily
# solved. 
uniquePaths_method2<- function(m, n) {
  return(choose(m+n-2, m-1))
}
uniquePaths_method2(7,3)

##########63. Unique Paths II##########
uniquePathsWithObstacles<- function(obstacle) {
  m<- nrow(obstacle)
  n<- ncol(obstacle)
  ans<- matrix(NA,m,n)
  for(i in 1:m) {
    for(j in 1:n) {
      if(obstacle[i,j]==1) { ans[i,j]<- 0 }
      else {
        if(i==1) {
          if(j==1) {ans[i,j]<- 1}
          else {ans[i,j]<- ans[i,j-1]}
        }
        else {
          if(j==1) {ans[i,j]<- ans[i-1,j]}
          else {ans[i,j]<- ans[i-1,j]+ans[i,j-1]}
        }
      }
    }
  }
  return(ans[m,n])
}
obstacle<- rbind(c(0,0,0), c(0,1,0), c(0,0,0))
uniquePathsWithObstacles(obstacle)

##########64. Minimum Path Sum##########
minPathSum<- function(grid) {
  library(gtools)
  m<- nrow(grid); n<- ncol(grid);
  index<- permutations(n=m+n-2, r=m-1)
  for(i in nrow(index)) {
    rownum<-1; colnum<- 1; sum<- grid[1,1]; ans<- Inf;
    step<- rep("right", m+n-2); step[index[i,]]<- "down"
    for(j in 1:(m+n-2)) {
      if(step[j]=="right") {
        colnum<- colnum+1
        sum<- sum+grid[rownum,colnum]
        }
      else {
        rownum<- rownum+1
        sum<- sum+grid[rownum,colnum]
      }
    }
    if(sum < ans) {ans<- sum; ans_step<- step}
  }
  return(list(ans, ans_step))
}
grid<-matrix(1:21,3,7)
minPathSum(grid)

##########65. Valid Number##########
isNumber<- function(s) {
  alpha<- "[a-zA-z]{1}"
  punct<- "[[:punct:]]{1}"
  # whether there are more than one punctuation
  if(length(unlist(regmatches(s, gregexpr(pattern = punct, s))))>1)
  {return(FALSE)}
  else {
    if(attr(gregexpr(pattern = "e", s)[[1]], "match.length")!=-1) {
      if(length(unlist(regmatches(s, gregexpr(pattern = punct, s))))>0)
      {return(FALSE)}
      else {return(TRUE)}
    }
    else {
      if(attr(gregexpr(pattern = alpha, s)[[1]], "match.length")!=-1)
      {return(FALSE)}
      else {return(TRUE)}
    }
  }
}
isNumber("1 a")
isNumber("12e10")
isNumber("123")

##########66. Plus One##########
plusOne<- function(digits) {
  n<- length(digits)
  ans<- NULL
  carry<- 1
  for(i in n:1){
    if((digits[i]+carry) >= 10) { ans<- c(ans,0) }
    else { 
      ans<- c(ans, digits[i]+carry)
      carry<- 0
      }
  }
  if((digits[1]+carry) >= 10) {ans<- c(ans, 1)}
  return(rev(ans))
}
plusOne(c(1,0,0,1))
plusOne(c(9,9,9,9))

##########67. Add Binary##########
addBinary<- function(a, b) {
  a<- as.numeric(unlist(strsplit(as.character(a), split = "")))
  b<- as.numeric(unlist(strsplit(as.character(b), split = "")))
  n<- max(length(a), length(b))
  aaa<- c(rep(0, n-length(a)), a)
  bbb<- c(rep(0, n-length(b)), b)
  carry<- 0; ans<- NULL
  for(i in n:1) {
    sum<- aaa[i]+bbb[i]+carry
    if(sum>1) { ans<- c(ans, sum-2); carry<- 1 }
    else { ans<- c(ans, sum); carry<- 0 }
  }
  if(aaa[1]+bbb[1]+carry > 1) {ans<- c(ans, 1)}
  return(rev(ans))
}
addBinary(11,10)
addBinary(11,1)

##########68. Text Justification##########
fullJustify<- function(words, maxWidth) {
  # do the function only when words exist
  if(length(words)!=0) { 
    n<- sapply(words, nchar)
    # the cumulative space of blanks in the words vector
    blank<- c(0:(length(words)-1))
    cumulative<- cumsum(n)+blank
    keep_index<- max(which(cumulative<= maxWidth))
    keep_words<- words[1: keep_index]
    if(keep_index==1) {
      # if there is only one word satisfy the width limitation
      # directly add spaces to the end of the word
      space<- paste(rep(" ", maxWidth- nchar(keep_words)),
                    collapse = "")
      display<- paste(keep_words, space, sep = "")
      ans<- rbind(display, Recall(words[-(1:keep_index)], maxWidth))
      return(unname(ans))
    }
    # the case that more than one words satisfy the limitation
    # is much more complicated
    else {
      # control the spaces inserted, choose 'ceiling' to ensure the right spaces 
      # are more than the ones on the left
      space_num<- ceiling((maxWidth - cumulative[keep_index])/(keep_index-1))
      space_num<- c(rep(space_num, keep_index-2), 
                    (maxWidth - cumulative[keep_index])-
                      space_num*(keep_index-2))
      display<- keep_words[1]
      # paste the words and the spaces
      for(i in 1:(keep_index-1)) {
        space<- paste(rep(" ", space_num[i]+1), collapse = "")
        display<- paste(display, space, keep_words[i+1], sep = "")
        ans<- rbind(display, Recall(words[-(1:keep_index)], maxWidth))
      }
      return(unname(ans))
    }
  }
  # if the words are all done, return NULL and stop the function
  else { return(NULL) }
}
words<- c("This", "is", "an", "example", "of", "text", "justification.","alal")
fullJustify(words, 16)

##########69. Sqrt(x)##########
mySqrt<- function(x) {
  # when x is very large, the Bisection method still cost
  # very long time, thus we call the function recursively
  # to reduce the calculation complexity
  if(x > 100) {
    return(10*Recall(x/100))
  }
  else {
    i = 0; j = x/2 + 1;
    while (i < j) {
      mid = (i + j) / 2
      sq = mid*mid;
      if (abs(sq-x)< 1e-10) { return(mid) }
      else {
        if(sq < x) {i<- mid}
        else {j<- mid}
        }
    }
  }
}
mySqrt(1e13)
sqrt(1e13)

##########70. Climbing Stairs##########
climbStairs<- function(n) {
  #first way: all 1 step
  ans<- 1
  for(i in 1:(n%/%2)) {
    ans<- ans+choose(n%/%2,i)
  }
  return(ans)
}
climbStairs(9)

##########71. Simplify Path##########
simplifyPath<- function(path) {
  n<- nchar(path)
  n %in% gregexpr("/", path)[[1]]
  
}

##########73. Set Matrix Zeroes##########
setZeroes<- function(matrix) {
  m<- nrow(matrix); n<- ncol(matrix);
  zeros<- which(matrix==0)
  for(i in 1:length(zeros)) {
    col_index<- ceiling(zeros[i]/m)
    row_index<- zeros[i]- m*(col_index-1)
    matrix[, col_index]<- 0
    matrix[row_index, ]<- 0
  }
  return(matrix)
}
mat<- matrix(1:12,3)
mat[1,2]<- 0; mat[2,3]<- 0
setZeroes(mat)

##########74. Search a 2D Matrix##########
searchMatrix<- function(matrix, target) {
  nums<- c(t(matrix))
  i<- 0; j<- length(nums);
  while(i<(j-1)) {
    mid<- (i+j) %/% 2
    if(nums[mid]==target) { return(TRUE) }
    else {
      if(nums[mid]< target) { i<- mid }
      else { j<- mid }
    }
  }
  return(FALSE)
}
matrix<- rbind(c(1,3,5,7),c(10,11,16,20),c(23,30,34,50))
searchMatrix(matrix, 21)

##########75. Sort Colors##########
sortColors<- function(nums) {
  amount<- sapply(c(0,1,2), 
                  function(x) {return(sum(nums==x))})
  ans<- sapply(0:2, function(x)
    {return(rep(x, amount[x+1]))})
  return(unlist(ans))
}
sortColors(c(0,1,2,2,2,2,2,1,1,1,0,0,0))

##########76. Minimum Window Substring##########
#construct a window and move the left and right index,
#thus the complexity would be O(n) because it at most 
#traverse all the alphabets in s
minWindow<- function(s, t) {
  sss<- unlist(strsplit(s, split = ""))
  ttt<- unlist(strsplit(t, split = ""))
  index<- lapply(ttt, function(x) {return(which(sss==x))})
  n<- length(sss); left<- 1; right<- n
  if(sum(sapply(index, function(x) { 
    return(sum(x %in% 1:n==0)) }))!=0) {return("")}
  
  #windoe searches from left
  repeat {
    #if it is other alphabets, directly move the left index
    if(!(sss[left] %in% ttt)) {left<- left+1; next;}
    else {
      #when the function return TRUE means the string do not
      #contain this alphabet
      judge_1<- sapply(index, function(x, start, end) {
        return(sum(x %in% c(start:end))==0) }, 
        start=left+1, end=right)
      if(sum(judge_1)==0) { left<- left+1 }
      else {
        if(!(sss[right] %in% ttt)) {right<- right-1}
        else {
          judge_2<- sapply(index, function(x, start, end) {
            return(sum(x %in% c(start:end))==0) }, 
            start=left, end=right-1)
          if(sum(judge_2)==0) { right<- right-1 }
          else {break}
        }
      }
    }
  }
  ans<- c(left, right); minlen<- right-left+1
  left<- 1; right<- n;
  # search from right
  repeat {
    if(!(sss[right] %in% ttt)) {right<- right-1; next;}
    else {
      #when the function return TRUE means the string do not
      #contain this alphabet
      judge_1<- sapply(index, function(x, start, end) {
        return(sum(x %in% c(start:end))==0) }, 
        start=left, end=right-1)
      if(sum(judge_1)==0) { right<- right-1 }
      else {
        if(!(sss[left] %in% ttt)) {left<- left+1}
        else {
          judge_2<- sapply(index, function(x, start, end) {
            return(sum(x %in% c(start:end))==0) }, 
            start=left+1, end=right)
          if(sum(judge_2)==0) { left<- left+1 }
          else {break}
        }
      }
    }
  }
  if((right-left+1) <= minlen) {
    return(paste(sss[left:right], collapse = ""))
  }
  else {
    return(paste(sss[ans[1]:ans[2]], collapse = ""))
  }
}
s = "ADOBECODEBANC"
t = "ABC"
minWindow(s, t)
minWindow("abcdddddddddda","abc")

##########77. Combinations##########
combine<- function(n, k) {
  sub<- function(nums,k) {
    if(k==1) { return(matrix(nums, ncol=1))  }
    else {
      if(k==length(nums)) {return(matrix(nums, nrow = 1))}
      else {
        ans<- rbind(cbind(nums[1], Recall(nums[-1],k-1)),
                    Recall(nums[-1], k))
        return(ans)
        }
    }
  }
  result<- sub(1:n, k)
  return(result)
}
combine(6,4)
combine(6,1)
combine(6,6)

##########78. Subsets##########
subsets<- function(nums) {
  n<- length(nums)
  ans<- vector("list")
  for(i in 2:(n-1)) {
    index<- combine(n, i)
    ans[[i]]<-t(apply(index, 1, function(x){nums[x]}))
  }
  ans[[1]]<- matrix(nums, ncol = 1)
  ans[[i+1]]<- nums
  ans[[i+2]]<- ""
  return(ans)
}
subsets(c(1,2,3,4))

##########79. Word Search##########
exist<- function(board, word) {
  
}
board<- rbind(c('A','B','C','E'),c('S','F','C','S'),
               c('A','D','E','E'))
word <- "ABCCED"

##########80. Remove Duplicates from Sorted Array II##########
removeDuplicates<- function(nums) {
  amount<- table(nums)
  dup<- ifelse(amount>=2, 2, amount)
  ans<- NULL
  for(i in 1:length(dup)) {
    ans<- c(ans, rep(as.numeric(names(amount))[i], dup[i]))
  }
  return(ans)
}
removeDuplicates(c(1,7,1,2,2,3,4,4,4,4,4))

##########81. Search in Rotated Sorted Array II##########
search<- function(nums, target) {
  left<- 1; right<-  length(nums)
  while(left<= right) {
   mid<- (left+right) %/% 2
   if(nums[mid]==target) { return(TRUE) }
   else {
     if(nums[mid]==nums[left]) {left<- left+1}
     else {
       if(nums[mid]> nums[left]) { 
         if(target>= nums[left] & target< nums[mid]) {right<- mid}
         else {left<- mid}
       }
       else {
         if(target> nums[mid] & target<= nums[right]) {left<- mid}
         else {right<- mid}
       }
     }
   }
  }
  return(FALSE)
}
search(c(6,7,9,10,1,2,3,5),8)
search(c(6,7,9,10,1,2,3,5),1)

##########82. Remove Duplicates from Sorted List II##########
deleteDuplicatesII<- function(head) {
  ans<- NULL
  for(i in 1:length(head)) {
    if(i==1) {
      if(head[i]!=head[i+1]) {ans<- c(ans, head[i])}
    }
    else {
      if(i==length(head)) {
       if(head[i]!=head[i-1]) {ans<- c(ans, head[i])}
     }
     else {
       if(head[i]!=head[i-1] & head[i]!=head[i+1]) {ans<- c(ans, head[i])}
     }
    }
  }
  return(ans)
}
deleteDuplicatesII(c(1,2,3,3,4,4,5))

83. Remove Duplicates from Sorted List##########
deleteDuplicates<- function(head) {
  ans<- NULL
  for(i in 1:length(head)) {
    if(i==1) {
      if(head[i+1]!=head[i]) {ans<- c(ans,head[i])}
    }
    else {
      if(head[i]!= head[i-1]) {ans<- c(ans, head[i])}
    }
  }
  return(ans)
}
deleteDuplicates(c(1,2,3,3,4,4,5))

##########84. Largest Rectangle in Histogram##########
#Compute the area of each possible rectangle:
#For one bar(A), looking at the bars from both sides. 
#Take one side at first, When the height beside is larger,
#add the height of A once and continue to look at the next 
#bar. Repeat the work and stop when the height is smaller 
#than A.
largestRectangleArea<- function(heights) {
  n<- length(heights)
  ans<- NULL
  for(i in 1:n) {
    area<- -heights[i]
    for(j in i:length(heights)) {
      if(heights[j]>=heights[i]) {area<- area+heights[i]}
      else {break}
      }
    for(j in i:1) {
      if(heights[j]>= heights[i]) {area<- area+heights[i]}
      else {break}
    }
    ans<- c(ans, area)
  }
  return(max(ans))
}
largestRectangleArea(c(2,1,5,6,2,3))

##########85. Maximal Rectangle##########
maximalRectangle<- function(matrix) {
  m<- nrow(matrix); n<- ncol(matrix)
  ans<- 0
  for(i in 1:m) { for(j in 1:n) {
      if(matrix[i,j]!=0) {
        for(l in i:m) { for(k in j:n) {
            area<- (l-i+1)*(k-j+1)
            if(sum(matrix[i:l, j:k])==area) {ans<- max(ans,area)}
            else {break}
        }}
        for(k in j:n) { for(l in i:m) {
          area<- (l-i+1)*(k-j+1)
          if(sum(matrix[i:l, j:k])==area) {ans<- max(ans,area)}
          else {break}
        }}
      }
  }}
  return(ans)
}
matrix<- t(matrix(c(1,0,1,0,0,
                    1,0,1,1,1,
                    1,1,1,1,1,
                    1,0,0,1,0), 5))
maximalRectangle(matrix)





