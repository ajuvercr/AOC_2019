
# d = sum(int(x)/3 - 2 for x in open('input.txt').readlines())
D = 0

for d in open('input.txt').readlines():
    d = int(d) / 3 - 2
    while d > 0:
      D += d
      d = d / 3 - 2
      # D += d
print(D)
