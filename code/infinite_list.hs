fib::Integer->Integer->[Integer]
fib a b = a:(fib b (a+b))
nums = fib 0 1

f 0 = [0]
f a = (a + head (f (a-1))) : f (a+1)

sumElts = f 1
