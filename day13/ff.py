# Input: lines like
#          Button A: X+#, Y+#
#          Button B: X+#, Y+#
#          Prize: X=#, Y=# (add 10 trillion to each)
#        separated by blank lines
# Output: if it costs 3 tokens to push A and 1 to push B,
#         what's the cheapest way to get from 0, 0 to prize location?
#         total cost across all sets of three lines
 
def get_cheapest_cost(ax, ay, bx, by, px, py):
  # ax*na + bx*nb = px
  # ay*na + by*nb = py
  # thus
  # ay*ax*na + ay*bx*nb = ay*px
  # ax*ay*na + ax*by*nb = ax*py
  # thus
  # (ay*bx - ax*by)*nb = ay*px - ax*py
  nb = int((ay*px - ax*py) / (ay*bx - ax*by))
  na = int((px - nb*bx) / ax)
  if (na * ax) + (nb * bx) == px and (na * ay) + (nb * by) == py:
    return 3 * na + nb
  return -1
 
button_a = []
button_b = []
prize = []
 
file = open("input1.txt", "r")
line_count = 0
for line in file:
  line = line.replace("\n", "")
  line_count += 1
  if line_count % 4 == 1:
    line = line.replace("Button A: X+", "")
    line = line.replace(", Y+", " ")
    parts = line.split(" ")
    button_a.append([int(parts[0]), int(parts[1])])
    continue
  if line_count % 4 == 2:
    line = line.replace("Button B: X+", "")
    line = line.replace(", Y+", " ")
    parts = line.split(" ")
    button_b.append([int(parts[0]), int(parts[1])])
    continue
  if line_count % 4 == 3:
    line = line.replace("Prize: X=", "")
    line = line.replace(", Y=", " ")
    parts = line.split(" ")
    prize.append([int(parts[0]) + 10000000000000, int(parts[1]) + 10000000000000])
    continue
 
total = 0
for index in range(len(button_a)):
  cost = get_cheapest_cost(button_a[index][0], button_a[index][1], button_b[index][0], button_b[index][1], prize[index][0], prize[index][1])
  if cost >= 0:
    total += cost
print (total)
