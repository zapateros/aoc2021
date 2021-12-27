# I was hoping for an Int-problem this year and this one was probably the most fun. I tried bruteforcing in R and C# but this
# was probably not the best idea. Even after optimizations my quickest brute force solution would take 3.5 days in the worst case.
# It was clear that the input could be cut into almost equivalent blocks with just different parameters. After this I tried working
# back from z14 (the z-value after the whole operation) is zero, but this was not the way to go. When I started at the top again 
# I finally saw what the operations actually did. In the end I did the whole solution manually. These were my steps:

# EDIT after writing it: It probably doesn't make sense if you haven't tried a solution that looks like mine, as not all steps are 
# completely written out (I have other things to do). When I tried writing these steps just now, I sometimes thought: uhh what, why again?.
# I solved it after our christmas meal where wine flooded the table so probably that helps when solving these types of problems

# STEP 1: write the separate blocks into seperate operations
# Here is one example (the first block) and then the 14 resulting operations. The digits d1 to d14 are obviously the modelnumber digits
(w, x, y, z) = (w0, x0, y0, z0) = (0, 0, 0, 0)
inp w1		w = d1
mul x 0		x = 0
add x z		x = z0
mod x 26	x = z0 %% 26
div z 1		z = floor(z0 / 1)
add x 15	x = (z0 %% 26) + 15
eql x w		if(((z0 %% 26) + 15) == d1){x = 1}else{x = 0}	
eql x 0		if(((z0 %% 26) + 15) == d1){x = 0}else{x = 1}
mul y 0		y = 0
add y 25	y = 25
mul y x		if(((z0 %% 26) + 15) == d1){x = 0, y = 0}else{x = 1, y = 25}
add y 1		if(((z0 %% 26) + 15) == d1){x = 0, y = 1}else{x = 1, y = 26}
mul z y		if(((z0 %% 26) + 15) == d1){x = 0, y = 1, z = floor(z0 / 1)}else{x = 1, y = 26, z = 26 * floor(z0 / 1)}
mul y 0		y = 0
add y w		y = d1
add y 13	y = d1 + 13
mul y x		if(((z0 %% 26) + 15) == d1){x = 0, y = 0, z = floor(z0 / 1)}else{x = 1, y = d1 + 13, z = 26 * floor(z0 / 1)}
add z y		if(((z0 %% 26) + 15) == d1){x = 0, y = 0, z = floor(z0 / 1)}else{x = 1, y = d1 + 13, z = 26 * floor(z0 / 1) + d1 + 13}

# Only the resulting z-value is important to take to the next operation. After the fist block (until the next digit input):
if(((z0 %% 26) + 15) == d1){z1 = floor(z0 / 1)}else{z1 = 26 * floor(z0 / 1) + d1 + 13}

# All the 14 operations are:
if(((z0 %% 26) + 15) == d1){z1 = floor(z0 / 1)}else{z1 = 26 * floor(z0 / 1) + d1 + 13}
if(((z1 %% 26) + 10) == d2){z2 = floor(z1 / 1)}else{z2 = 26 * floor(z1 / 1) + d2 + 16}
if(((z2 %% 26) + 12) == d3){z3 = floor(z2 / 1)}else{z3 = 26 * floor(z2 / 1) + d3 + 2}
if(((z3 %% 26) + 10) == d4){z4 = floor(z3 / 1)}else{z4 = 26 * floor(z3 / 1) + d4 + 8}
if(((z4 %% 26) + 14) == d5){z5 = floor(z4 / 1)}else{z5 = 26 * floor(z4 / 1) + d5 + 11}
if(((z5 %% 26) -11) == d6){z6 = floor(z5 / 26)}else{z6 = 26 * floor(z5 / 26) + d6 + 6}  #6th operation
if(((z6 %% 26) + 10) == d7){z7 = floor(z6 / 1)}else{z7 = 26 * floor(z6 / 1) + d7 + 12}
if(((z7 %% 26) - 16) == d8){z8 = floor(z7 / 26)}else{z8 = 26 * floor(z7 / 26) + d8 + 2}
if(((z8 %% 26) - 9) == d9){z9 = floor(z8 / 26)}else{z9 = 26 * floor(z8 / 26) + d9 + 2}
if(((z9 %% 26) + 11) == d10){z10 = floor(z9 / 1)}else{z10 = 26 * floor(z9 / 1) + d10 + 15}
if(((z10 %% 26) - 8) == d11){z11 = floor(z10 / 26)}else{z11 = 26 * floor(z10 / 26) + d11 + 1}
if(((z11 %% 26) - 8) == d12){z12 = floor(z11 / 26)}else{z12 = 26 * floor(z11 / 26) + d12 + 10}
if((z12 %% 26 -10) == d13){z13 = floor(z12 / 26)}else{z13 = 26 * floor(z12 / 26) + d13 + 14}
if((z13 %% 26 -9) == d14){z14= floor(z13 / 26)}else{z14 = 26 * floor(z13 / 26) + (d14 + 10)}

# STEP 2: find out what actually happens when doing for example floor(z/1) or 26 * floor(z / 26) and MOD 26
# This actually took me some time to understand but this was the trick to solve this puzzle
# The if-statements with a + in it, alway are FALSE, as z is always positive and the digits d1 to d14 are 1 to 9
# z0 = 0, so z1 = d1 + 13, z2 = 26 * (d1 + 13) + d2 + 16, etc.
# The first 5 operations result into: 
z5 = 26 * (26 * (26 * (26 * (d1 + 13) + d2 + 16) + d3 + 2) + d4 + 8) + d5 + 11
z5 = 26^4 * (d1 + 13) + 26^3 * (d2 + 16) + 26^2 * (d3 + 2) + 26 * (d4 + 8) + d5 + 11
# I wrote it in this form because the 26-digits are very important. To help explain why, let's write the formula different:
z5 = x1 + x2
# with:
x1 = 26^4 * (d1 + 13) + 26^3 * (d2 + 16) + 26^2 * (d3 + 2) + 26 * (d4 + 8)
x2 = d5 + 11
# for whatever d1, d2, d3, d4 x1 %% 26 == 0 (x1 MOD 26 == 0) as they all are multiplied by 26
# So (x1 + x2) MOD 26 == x2 (if x2 < 26, otherwise do a MOD again, but not relevant today)
# So now we know the relevant values that make up z1 to z13 to take to the next operation.
# Now we still have to figure out what the x2 is after every operation.
# I think this is the hard part of today's problem
# For this we have to check what the 'floor'-operations do. Important is to see what it brings to the next operation (after MOD 26 of the next operation)
# take the example: z6 = 26 * floor(z5 / 26) + d6 + 6 (with z5 stated above). Mind the powers of 26. 26^x / 26 == 26^(x-1)
# floor(26^3 * (d1 + 13) + 26^2 * (d2 + 16) + 26^1 * (d3 + 2) +  (d4 + 8) + (d5 + 11)/26) will drop the "(d5 + 11)/26" - part 
# So when doing the next operation, the if-statement, only the (d4 + 8) is important (remember (x1 + x2) MOD 26 == x2)
# So floor(x / 26) removes the part of x that doesn't include a 26^y factor. For example floor(26^2 * z + 10) == floor(26^2 * z)
# 26 * floor(x/26) first removes the part that doesn't incluse a 26^y factor, but then adds a 26 factor to all numbers and make these numbers irrelevant when doing MOD 26 in the next operation
# floor(x/1) doesn't remove anything.
# With a bit of mental gymnastics you can now rewrite the problem to a vector with digits that represent the relevant parts that are brought to the next operation
# The operations then remove from and add digits to it.

# STEP 3: rewrite the problem
# In this problem the vector after the first 5 operations is:
nums <- c((d5 + 11), (d4 + 8), (d3 + 2), (d2 + 16), (d1 + 13)) 
# Note the digits are the relevant parts after doing the MOD 26'es
# now every operation either adds or removes a digit.
# an operation floor(z / 1) does nothing.
# an operation 26 * floor(z5 / 26) + d6 + 6, first removes the first num (nums[1]) and then puts in front of nums: (d6 + 6)
# an operation 26 * floor(z6 / 1) + d7 + 12, only adds (d7 + 12) to the front of nums.
# Now we can rewrite the operations to:
nums <- c((d5 + 11), (d4 + 8), (d3 + 2), (d2 + 16), (d1 + 13)) 
if(nums[1] - 11 == d6){ # which means if(d5 == d6){etc.}etc. Also not that this is the 6th operation from above because the first 5 operations give the nums-vector described
  nums <- nums[-1]
}else{
  nums <- nums[-1]
  nums <- c((d6 + 6), nums)
}
nums <- c((d7 + 12), nums)
if((nums[1] - 16) == d8){
  nums <- nums[-1]
}else{
  nums <- nums[-1]
  nums <- c((d8 + 2), nums)
}
if((nums[1] - 9) == d9){
  nums <- nums[-1]
}else{
  nums <- nums[-1]
  nums <- c((d9 + 2), nums)
}
nums <- c((d10 + 15), nums)
if((nums[1] - 8) == d11){
  nums <- nums[-1]
}else{
  nums <- nums[-1]
  nums <- c((d11 + 1), nums)
}
if((nums[1] - 8) == d12){
  nums <- nums[-1]
}else{
  nums <- nums[-1]
  nums <- c((d12 + 10), nums)
}
if((nums[1] - 10) == d13){
  nums <- nums[-1]
}else{
  nums <- nums[-1]
  nums <- c((d13 + 14), nums)
}
if((nums[1] - 9) == d14){
  nums <- nums[-1]
}else{
  nums <- nums[-1]
  nums <- c((d13 + 10), nums)
}

# Now the problem is already optimized much further but we are not there yet. With these operations you still have to try out every digit to find out when z is zero in the end
# Luckily - or probaby not lucky but calculated - we can reduce the problem once more. 
# When I tried to find out the conditions when z is zero in the end, I found out that it basically means that the nums-vector is completely reduced to nums == NULL
# after counting the operations and when it adds or removes a number from vector nums, it turned out every operation has to be TRUE for it to be reduced to NULL in the end.
# This reduces the operations to:
nums <- c((d5 + 11), (d4 + 8), (d3 + 2), (d2 + 16), (d1 + 13)) 
if(nums[1] - 11 == d6){ 
  nums <- nums[-1]
}
nums <- c((d7 + 12), nums)
if((nums[1] - 16) == d8){
  nums <- nums[-1]
}
if((nums[1] - 9) == d9){
  nums <- nums[-1]
}
nums <- c((d10 + 15), nums)
if((nums[1] - 8) == d11){
  nums <- nums[-1]
}
if((nums[1] - 8) == d12){
  nums <- nums[-1]
}
if((nums[1] - 10) == d13){
  nums <- nums[-1]
}
if((nums[1] - 9) == d14){
  nums <- nums[-1]
}

# STEP 4: Solve it manually
# I'm not going through every operation but now you can fill in all the numbers (for both star 1 and star 2)
# For example, the first one is true if d5 == d6. So you fill in d5 = 9 and d6 = 9
# the second operation (the second if-statement) gives (d7 + 12) - 16 == d8, wich gives d7 - 4 == d8, so d7 = 9 and d8 = 5.
# Etc, Etc
# the highest number is 53999995829399 and the lowest: 11721151118175
