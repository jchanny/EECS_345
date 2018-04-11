--Programming Assignment 3
--Jeremy Chan jsc126

-- #1
removedups lis =
  if (tail lis) == []
  then
    lis
    else
    if (head lis) == (head (tail lis))
    then
      removedups (tail lis)
    else
      (head lis) : removedups (tail lis)

-- #2
removedupsCps lis return =
  if (tail lis) == []
  then
    return lis
    else
    if (head lis) == (head (tail lis))
    then
      removedupsCps (tail lis) return
      else
      removedupsCps (tail lis) (\v -> return ((head lis): v))

-- #3
data MixedList = 
    
  
