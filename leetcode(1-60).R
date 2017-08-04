solution1 <- function(str) {
  s<- strsplit(str, split = " ")
  s<- unlist(s)
  n<- length(s)
  result<- s[n]
  while(n>=1) {
    n<- n-1
    result<- cbind(result, s[n])
  }
  return(result)
}

solution2<- function(number) {
  num<- strsplit(as.character(number), split = "")
  num<- unlist(num)
  for (i in 0:9) {
    judge<- sum(num==as.character(i))
    if(judge==1) {break}
  }
  return(i)
}

##########1.Two Sum##########
# twosum<- function(nums, target) {
#   n<-length(nums)
#   judge<-0
#   for(i in 1:(n-1)) {
#     for(j in (i+1):n) {
#       if(nums[i]+nums[j]==target) {
#         judge<- 1
#         break}
#     }
#     if(judge==1) {break}
#   }
#   return(c(i-1,j-1))
# }
# twosum(c(2,7,11,15), 9)

twosum<- function(nums, target) {
  n<-length(nums)
  judge<-0
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      if(nums[i]+nums[j]==target) { return(c(i-1,j-1))}
    }
  }
}
twosum(c(2,7,11,15), 13)

##########2.Add Two numbers#########
addtwonumber<- function(a,b) {
  n<- max(length(a), length(b))
  anew<- rbind(a, rep(0, n - length(a)))
  bnew<- rbind(b, rep(0, n - length(b)))
  c<- rep(NA, n)
  j<- 0
  for(i in 1:n) {
    x<- a[i]+b[i]+j
    c[i]<- ifelse(x<10, x, x-10)
    j<- ifelse(x<10, 0, 1)
  }
  return(c)
}
addtwonumber(c(8,5,3), c(5,6,4))

###########3.longest substring##########
longestsubatr<- function(s) {
  n<- nchar(s)
  ans_length<- 0
  for(i in 1:n) {
    for(j in i:n) {
      subs<- substr(s, start = i, stop = j)
      judge_rep<- max(table(strsplit(subs,"")))
      judge_len<- length(table(strsplit(subs,"")))
      if(judge_rep==1&judge_len>ans_length) {
        ans<- subs
        ans_length<- nchar(subs)
      }
    }
  }
  return(data.frame(answer=ans, ans_length ))
  
}
longestsubatr("abcabcbb")
longestsubatr("bbbbbb")
longestsubatr("pwwkew")

##########4.Median of Two Sorted Arrays#########
sol_4<- function(nums1,nums2) {
  num<- c(nums1, nums2)
  return(median(num))
}
sol_4(c(1,3), c(2))
sol_4(c(1,2), c(3,4))

##########5.Longest Palindromic Substring##########
sol_5<- function(input) {
  n<- nchar(input)
  out_len<- 0
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      subs<- substr(input, start = i, stop = j)
      ss<- unlist(strsplit(subs, ""))
      judge<- 1
      nn<- nchar(subs)
      for(i in 1:(floor(nn/2))) {
        if(ss[i]!=ss[nn+1-i]) {
          judge<- 0
          break
        }
      }
      if(judge==1&nn>out_len) {
        output<- subs
        out_len<- nchar(output)
      }
    }
  }
  return(output)
}
sol_5("babad")
sol_5("babab")
sol_5("cbb")

##########6.ZigZag Conversion##########
sol_6<- function(string, row_num) {
  ss<- unlist(strsplit(string, ""))
  count<- nchar(string)
  ans<- rep(NULL, row_num)
  c<- 1
  while(c<= count) {
    part<- matrix("", nrow = row_num, ncol = row_num-1)
    for(i in 1:row_num) {
      part[i,1]<- ss[c]
      c<- c+1
    }
    for(j in 2:(row_num-1)) {
      part[(row_num+1-j),j]<-ss[c]
      c<- c+1
    }
    ans<- cbind(ans, part)
  }
  ans<- ifelse(is.na(ans), "", ans)
  ans<- ans[, colSums(ans=="")!=row_num]
  result<- paste(t(ans), collapse = "")
  return(list(ans, result))
}
sol_6("PAYPALISHIRING",3)
sol_6("PAYPALISHIRING",5)

##########7.Reverse Integer##########
sol_7<- function(x) {
  char<- unlist(strsplit(as.character(abs(x)), ""))
  digit<- length(char)
  reverse<- NULL
  for(i in 1:digit) {
    reverse<- paste(reverse, char[digit+1-i], sep = "")
  }
  reverse<- as.double(reverse)
  reverse<- ifelse(x>0, reverse, -reverse)
  imax<- .Machine$integer.max
  if(reverse>imax || reverse<(-imax)) {return(0)}
  else {return(reverse)}
}
sol_7(123)
sol_7(-12323423)
sol_7(1000000002)

##########8.String to Integer (atoi)##########
sol_8<- function(string) {
  expr<- "[0-9]{1}"
  ans<- unlist(regmatches(string, gregexpr(pattern = expr, string)))
  return(paste(ans, collapse = ""))
}
sol_8("2342jjhj00798---=-=#@%^$939")

#########9.Palindrome Number##########
sol_9<- function(number) {
  num<- unlist(strsplit(as.character(number), split = ""))
  n<- length(num)
  judge<- 1
  for(i in 1:floor(n/2)) {
    if(num[i]!=num[n+1-i]) { judge<- 0 }
  }
  return(ifelse(judge==1, "yes", "no"))
}
sol_9(123321)
sol_9(1234321)
sol_9(12345789)
sol_9(123456)

##########10.Regular Expression Matching##########
isMatch<- function(string1, string2) {
  if(nchar(string2) < nchar(string1)) {return(as.logical(0))}
  n<- nchar(string2)
  ans<- 0
  for(i in 1:n) {
    for(j in 1:n) {
      sub2<- substr(string2, start = i, stop = j)
      matchjudge<- unlist(regmatches(string1, 
                                gregexpr(pattern = sub2, string1)))
      judge<- sum(matchjudge==string1)
      if(judge!=0) {ans<- 1}
    }
  }
  return(as.logical(ans))
}
isMatch("aa","a")
isMatch("aa","aa")
isMatch("aab", "c*a*b")
isMatch("ab", ".*")

##########11.Container With Most Water##########
sol_11<- function(vec) {
  n<- length(vec)
  ans<- 0
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      height<- min(vec[1], vec[j])
      width<- j-i
      volume<- height*width
      if(volume>ans) {
        ans<- volume
        index<- cbind(x=c(i,j), y=c(vec[i], vec[j]))
      }
    }
  }
  return(list(ans, index))
}
sol_11(c(3,5,2,1,1))

##########12.Integer to Roman##########
as.roman(1233)

##########13.Roman to Integer##########
sol_13<- function(roman) {
  return(as.numeric(as.roman(roman)))
}
sol_13("MCCXXXIII")

##########14.Longest Common Prefix##########
sol_14<- function(strarray) {
  n<- length(strarray)
  l<- min(sapply(strarray, nchar))
  for(i in 1:l) {
    subarray<- sapply(strarray, substr, start=1, stop=i)
    test<- subarray[1]
    if(sum(subarray==test)!=n) {
      test<- substr(strarray[1], start = 1, stop = (i-1))
      break}
  }
  return(test)
}

strarray<- c("abcdasf", "abcdwet", "abcdyuiiyu","abcdjlhjkh")
strarray<- rep("abcd", 5)
sol_14(strarray)

##########15.3Sum##########
threesum<- function(numarray) {
  n<- length(numarray)
  result<- NULL
  for(i in 1:(n-2)) {
    for(j in (i+1):(n-1)) {
      for(k in (j+1):n) {
        s<- sum(numarray[i]+numarray[j]+numarray[k])
        if(s==0) {
          ans<- c(numarray[i], numarray[j], numarray[k])
          result<- rbind(result, ans)
        }
      }
    }
  }
  result<- result[!duplicated(t(apply(result, 1, sort))),]
  return(result)
}
s<- c(-1, 0, 1, 2, -1, -4)
threesum(s)

##########16.3Sum Closest##########
sol_16<- function(array, target) {
  n<- length(array)
  ans_diff<- Inf
  for(i in 1:(n-2)) {
    for(j in (i+1):(n-1)) {
      for(k in (j+1):n) {
        s<- sum(array[i], array[j], array[k])
        diff<- abs(s - target)
        if(diff==0) {next}
        if(diff<=ans_diff) {
          ans_diff<- diff
          ans_sum<- s
          ans_index<- c(i,j,k)
          }
      }
    }
  }
  return(list(number=array[ans_index],sum=ans_sum))
}

array<- c(-1,2,1,-4)
target<- 1
sol_16(array, target)

##########17.Letter Combinations of a Phone Number##########
letter<- list(NA,
              c("a", "b", "c"),
              c("d", "e", "f"),
              c("g", "h", "i"),
              c("j", "k", "l"),
              c("m", "n", "o"),
              c("p", "q", "r", "s"),
              c("t", "u", "v"),
              c("w", "x", "y", "z"))
#subfunction to combine two arrays
combine<- function(v1, v2) {
  n1<- length(v1)
  n2<- length(v2)
  ans<- NULL
  for(i in 1:n1) {
    for(j in 1:n2) {
      ans<- cbind(ans, paste(v1[i], v2[j], sep = ""))
    }
  }
  return(ans)
}
#main function
sol_17<- function(numarray) {
  numarray<- as.numeric(unlist(strsplit(numarray, split = "")))
  n<- length(numarray)
  result<- letter[[numarray[1]]]
  for(i in 2:n) {
    result<- combine(result, letter[[numarray[i]]])
  }
  return(result)
}
sol_17("234")

##########18.4Sum##########
library(gtools)
foursum<- function(array) {
  index<- permutations(n=length(array), r=4)
  nums<- matrix(array[index], ncol = 4)
  nums_clean<- nums[!duplicated(t(apply(nums,1,sort))),]
  ans<- nums_clean[rowSums(nums_clean)==0,]
  return(ans)
}
array<- c(1, 0, -1, 0, -2, 2)
foursum(array)

##########19. Remove Nth Node From End of List##########
sol_19<- function(numlist, n) {
  l<- length(numlist)
  ans<- numlist[-(l-n+1)]
  return(ans)
}
sol_19(c(1,2,3,4,5), 2)

##########20. Valid Parentheses##########
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

str<- "()"
str<- "()[]{}"
Valid_Parentheses(str)

##########21. Merge Two Sorted Lists##########
sol_21<- function(l1,l2) {
  return(c(l1,l2))
}

##########22. Generate Parentheses##########
Generate_Parentheses<- function(n) {
  library(gtools)
  v<- rep(c("(", ")"),n)
  index<- permutations(n=2*n,r=2*n)
  ans<- NULL
  for(i in 1:nrow(index)) {
    a<- v[index[i,]]
    a<- paste(a, collapse = "")
    if(Valid_Parentheses(a)) {ans<- rbind(ans,a)}
  }
  ans<- ans[!duplicated(ans),]
  return(ans)
}
Generate_Parentheses(4)

##########23. Merge k Sorted Lists##########
#It is quite easy for R because R do not have issues about nodes
#and can just combine several lists
sol_23<- function(lists) {
  ans<- NULL
  k<- length(lists)
  for(i in 1:k) {
    ans<- c(ans, lists[[i]])
  }
  return(list(ans))
}
lists<- list(c(1,2,3), c(4,5))
sol_23(lists)

##########24. Swap Nodes in Pairs##########
swap<- function(l) {
  n<- length(l)
  i<- 1
  while(i<=(n-1)) {
    a<- l[i]
    l[i]<- l[i+1]
    l[i+1]<- a
    i<- i+2
  }
  return(l)
}
swap(c(1,2,3,4))

##########25. Reverse Nodes in k-Group##########
reverse_k_group<- function(l,k) {
  n<- length(l)
  num<- floor(n/k)
  ans<- NULL
  for(i in 1:num) {
   subl<- l[((i-1)*k+1):(i*k)]
   subans<- rep(NA, k)
     for(j in 1:k) {
       subans[k+1-j]<- subl[j]
     }
   ans<- c(ans, subans)
  }
  tail<- l[-(1:(num*k))]
  ans<- c(ans, tail)
  return(ans)
}
l<- c("a","b","c","d","e","f")
reverse_k_group(l, 4)

##########26. Remove Duplicates from Sorted Array##########
removeDuplicates<- function(nums) {
  r<- nums[!duplicated(nums)]
  return(list(number= r, length= length(r)))
}
nums<- c(1,1,2,3,3,3,3,4,5,1)
removeDuplicates(nums)

##########27. Remove Element##########
removeElement<- function(nums, val) {
  ans<- nums[!(val==nums)]
  return(list(number= ans, length= length(ans)))
}
removeElement(c(3,2,1,2,3), 3)

##########28. Implement strStr()##########
strstr<- function(haystack, str) {
  n<- nchar(haystack)
  ans<- -1
  for(i in 1:n) {
    for(j in i:n) {
      test<- substr(haystack, start = i, stop = j)
      if(test==str) {
        ans<- i
        break
      }
    }
  }
  return(ans)
}
strstr("abcdefg", "cde")
strstr("abcdefg", "ede")

##########29. Divide Two Integers##########
divide<- function(dividend, divisor) {
  i<- 0
  d<- dividend
  while(divisor <= d) {
    d<- d - divisor
    i<- i+1
  }
  ans<- paste(dividend, "/", divisor, "=", i, sep = "")
  if(d!=0) {ans<- paste(ans, "...", d, sep = "")}
  return(ifelse(i==0, "MAX_INT", ans))
}
divide(45,3)
divide(45,2)

##########30. Substring with Concatenation of All Words#######
findSubstring<- function(s, words) {
  library(gtools)
  ans<- NULL
  k<- length(words)
  #index that can rearrange the words
  index<- permutations(n=k, r=k)
  for(i in 1:nrow(index)) {
    w<- words[index[i,]]
    w<- paste(w, collapse = "")
    ans<- c(ans, as.numeric(regexpr(pattern = w, text = s)))
  }
  #delete the cases that rearrange words do not appear in s
  ans<- ans[ans>0]
  return(sort(ans-1))
}
words<- c("foo", "bar")
words<- c("foo", "ba")
s<- "barfoothefoobarman"
findSubstring(s, words)

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

##########43. Multiply Strings##########
MultiplyStrings<- function(num1,num2) {
  n1<- length(num1)
  n2<- length(num2)
  mat<- matrix(0, nrow = n1*n2, ncol = n1+n2)
  n<- 1
  for(i1 in 1:n1) {
    for(i2 in 1:n2) {
      mat[n, (i1+i2-1)]<- (num1[i1]*num2[i2])%/%10
      mat[n, (i1+i2)]<- (num1[i1]*num2[i2])%%10
      n<- n+1
    }
  }
  k<- 1
  tens<- 0
  ans<- rep(0,(n1+n2))
  for(j in rev(1:(n1+n2))) {
    a<- sum(mat[,j])+tens
    tens<- a%/%10
    ans[k]<- a%%10
    k<- k+1
  }
  if(a >= 10) {ans[k]<- tens}
  return(rev(ans))
}
MultiplyStrings(c(1,9,7,9,9), c(9,9,8))

##########44. Wildcard Matching##########
isMatch<- function(s, p) {
  if(nchar(p)<= nchar(s)) {
    expr<- unlist(strsplit(p ,split = ""))
    expr<- ifelse(expr=="?", ".{1}", expr)
    expr<- ifelse(expr=="*", ".*", expr)
    expr<- paste(expr, collapse = "")
    if(regmatches(s, regexpr(pattern = expr, s))==s) {return(TRUE)}
    else {return(FALSE)}
  }
  else {return(FALSE)}
}
isMatch("aa","a")
isMatch("aa","aa")
isMatch("aa", "*")
isMatch("aab", "c*a*b")
isMatch("ab", "?*")

##########45. Jump Game II##########
jump<- function(nums) {
  n<- length(nums)
  a<- 1
  b<- 0
  k<- 0
  for(i in 1:n) {
    if(a<i) {
      a<- b
      k<- k+1
    }   
    b<- max(b, i+nums[i])
  }
  return(k)
}
jump(c(2,3,1,1,4))
jump(c(2,3,1,1,4,1,1))

##########46. Permutations##########

##########47. Permutations II##########

##########48. Rotate Image##########

##########49. Group Anagrams##########
groupAnagrams<- function(strs) {
  a<- sapply(strs, strsplit, split="")
  a<- lapply(a, sort)
  a<- unlist(lapply(a, paste, collapse=""))
  index<- lapply(unique(a), function(x) {which(a==x)})
  ans<- lapply(index, function(x) {strs[x]})
  return(ans)
}
groupAnagrams(c("eat", "tea", "tan", "ate", "nat", "bat"))

##########50. Pow(x, n)##########
myPow<- function(x, n) {
  if(n==1) {return(x)}
  else {
    if(n%%2==0) {return(Recall(x, n/2)*Recall(x, n/2))}
    else {return(x*Recall(x, n-1))}
  }
}
myPow(2,8)
myPow(3,3)

##########51. N-Queens##########
solveNQueens<- function(n) {
  library(gtools)
  # generate all possible arrangements
  index<- permutations(n=n, r=n)
  ans<- NULL
  for(k in 1:nrow(index)) {
    judge<- 1
    for(i in 1:(n-1)) {
      test<- abs(index[k, (i+1):n]-index[k,i])
      if(sum(test==c(1:(n-i)))!=0) {
        judge<- 0
        break
      }
    }
    if(judge==1 & i==(n-1)) {ans<- rbind(ans, index[k,])}
  }
  # the function to visilize the chessboard
  display<- apply(ans, 1, function(vec) {
    l<- length(vec)
    mat<- matrix(".",l,l)
    for(i in 1:l) {mat[i,vec[i]]<- "Q"}
    mat<- apply(mat, 1, paste, collapse="")
    return(mat)
  })
  colnames(display)<- paste("solution", 1:nrow(ans), sep = "")
  return(display)
}
solveNQueens(4)
solveNQueens(5)

##########52. N-Queens II##########
totalNQueens<- function(n) {
  ans<- solveNQueens(n)
  return(ncol(ans))
}
totalNQueens(4)
totalNQueens(5)

##########53. Maximum Subarray##########
maxSubArray<- function(nums) {
  n<- length(nums)
  ans<- 0
  for(i in 1:n) {
    for(j in 1:n) {
      s<- sum(nums[i:j])
      if(s>ans) {ans<- s
      index<- c(i,j)}
    }
  }
  return(nums[index[1]:index[2]])
}
maxSubArray(c(-2,1,-3,4,-1,2,1,-5,4))

##########54. Spiral Matrix##########
spiralOrder<- function(matrix) {
  m<- nrow(matrix)
  n<- ncol(matrix)
  if(is.null(m)) {ans<- matrix}
  else {
    if(m==2) {ans<- c(matrix[1,], rev(matrix[2,]))}
    else {
      mat<- matrix[-c(1,m),-c(1,n)]
      ans<- c(matrix[1, 1:n], matrix[2:m,n], 
              rev(matrix[m, 1:(n-1)]),
              rev(matrix[2:(m-1),1]), 
              Recall(mat))
    }
  }
  return(ans)
}
spiralOrder(matrix)
spiralOrder(matrix(1:8,4))

##########55. Jump Game##########
canJump<- function(nums) {
  n<- length(nums)
  judge<- 0
  for(i in 1:n) {
    max_now<- i+nums[i]
    if(max_now>=n) {judge<- 1}
    if(max_now<=i) {break}
  }
  return(as.logical(judge))
}

canJump(c(2,3,1,1,4))
canJump(c(3,2,1,0,1))

##########56. Merge Intervals##########
mergeintervals<- function(intervals) {
  n<- nrow(intervals)
  inter<- intervals[order(intervals[,1]),]
  i<- 2
  while(i <= n) {
    if(inter[i,1] %in% inter[i-1,1]:inter[i-1,2]){
      new<- c(inter[i-1,1], max(inter[i-1,2], inter[i,2]))
      inter[i-1,]<- new
      inter<- inter[-i,]
      n<- n-1
    }
    else (i<- i+1)
  }
  return(inter)
}
intervals<- rbind(c(1,3), c(2,6), c(8,10), c(15,18))
mergeintervals(intervals)

##########57. Insert Interval##########
insert<- function(intervals, newInterval) {
  int<- rbind(intervals, newInterval)
  return(mergeintervals(int))
}
insert(rbind(c(1,3),c(6,9)), c(2,5))
insert(rbind(c(1,2), c(3,5), c(6,7), c(8,10),c(12,16)), 
       c(4,9))

##########58. Length of Last Word##########
lengthOfLastWord<- function(s) {
  str<- unlist(strsplit(s, split = " "))
  n<- length(str)
  return(nchar(str[n]))
}
s <- "Hello World"
lengthOfLastWord(s)

##########59. Spiral Matrix II##########
generateMatrix<- function(n) {
  num<- 1; top<- 1;  bottom<- 1; left<- 1;  right<- 1
  ans<- matrix(NA, nrow=n, ncol=n)
  repeat {
    # generate the top element
    for(i in left:(n-right+1)) {
      ans[top, i]<- num
      num<- num+1
    }
    top<- top+1
    if(top+bottom-2==n) {break}
    
    # generate the right element
    for(i in top:(n-bottom+1)) {
      ans[i, n-right+1]<- num
      num<- num+1
    }   
    right<- right+1
    if(left+right-2==n) {break}
    
    # generate the bottom element
    for(i in (n-right+1):left) {
      ans[n-bottom+1, i]<- num
      num<- num+1
    }
    bottom<- bottom+1
    if(top+bottom-2==n) {break}
    
    # generate the left element
    for(i in (n-bottom+1):top) {
      ans[i,left]<- num
      num<- num+1
    }
    left<- left+1
    if(left+right-2==n) {break}
  }
  return(ans)
}
generateMatrix(4)
generateMatrix(5)
generateMatrix(7)

##########60. Permutation Sequence##########
getPermutation<- function(n,k) {
  library(gtools)
  all<- permutations(n=n, r=n)
  return(all[k,])
}
getPermutation(4,20)