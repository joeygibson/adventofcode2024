import math

def market():  
    # load data
    with open("input2.txt", 'r') as file:
        dataInput = file.readlines()
    sum = 0
    REPETITIONS = 2000
    sequences = []
    changes = []    
    for number in dataInput:
        num = int(number.strip())
        sequence = [num % 10]
        change = []
        for i in range(0, REPETITIONS):
            prev = num
            # step 1
            res = num * 64
            num = res ^ num
            num = num % 16777216

            # step 2
            res = math.floor(num / 32)
            num = res ^ num
            num = num % 16777216

            # step 3    
            res = num * 2048
            num = res ^ num
            num = num % 16777216

            sequence.append(num % 10)
            change.append(num % 10 - prev % 10)

        #print(f"After {REPETITIONS} repetitions, number {number.strip()} became {num}")
        sum += num
        sequences.append(sequence)
        changes.append(change)

    print(f"Lists of sequences and changes created.")
    #print(f"{sequences=}")
    #print(f"{changes=}")

    quadruples = []
    for number in range(0, len(sequences)):
        quadruple = {}
        for i in range(3, REPETITIONS):
            name = "{:+}".format(changes[number][i-3]) + "{:+}".format(changes[number][i-2]) + "{:+}".format(changes[number][i-1]) + "{:+}".format(changes[number][i])
            if name not in quadruple:
                quadruple[name] = sequences[number][i+1]
        quadruples.append(quadruple)
    
    #print(f"{quadruples=}")

    sums = {}
    for number in range(0, len(sequences)):
        for key, value in quadruples[number].items():
            sums[key] = sums.get(key, 0) + value

    #print(f"{sums=}")            

    print(f"Maximal number of bananas one can get is {max(sums.values())}.")

if __name__ == "__main__":
    market()
