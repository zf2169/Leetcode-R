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