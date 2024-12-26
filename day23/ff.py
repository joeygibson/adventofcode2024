from collections import defaultdict

data = open("input1.txt").read().splitlines()

N = defaultdict(list)

for line in data:
    s, d = line.split('-', 1)
    N[s].append(d)
    N[d].append(s) 

SN = []
for k, v in N.items():
    sv = set(v)
    sv.add(k)
    SN.append(sv)
print(len(SN))
count = defaultdict(int)
for i in range(len(SN)):
    for j in range(i+1,len(SN)):
        p1, p2 = SN[i], SN[j]
        if len(p1 & p2) > 10:
            count[tuple(sorted(list(p1 & p2)))] += 1

# for k, v in count.items():
    # print(f'{k} -> {v}')
print(len(count))
print([k for k,v in count.items() if len(k) * (len(k) - 1) / 2 == v])
answer = [k for k,v in count.items() if len(k) * (len(k) - 1) / 2 == v][0]
print(','.join(sorted(answer)))
