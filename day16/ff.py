import heapq

MapList = []
with open("input1.txt", "r") as data:
    for t in data:
        Line = t.strip()
        MapList.append(Line)

OpenSet = set()
WallSet = set()
for y, m in enumerate(MapList):
    for x, c in enumerate(m):
        if c == ".":
            OpenSet.add((x,y))
        elif c == "#":
            WallSet.add((x,y))
        elif c == "S":
            StartPoint = (x,y)
            OpenSet.add((x,y))
        elif c == "E":
            EndPoint = (x,y)
            OpenSet.add((x,y))

DirectionDict = {"E": ((1,0,1,"E"),(0,1,1001,"S"),(0,-1,1001,"N")), 
                 "W": ((-1,0,1,"W"),(0,1,1001,"S"),(0,-1,1001,"N")), 
                 "N": ((0,-1,1,"N"),(1,0,1001,"E"),(-1,0,1001,"W")),
                 "S": ((0,1,1,"S"),(1,0,1001,"E"),(-1,0,1001,"W"))}


LocationScoreDict = {}
BestLocationsSet = set()
ImperialCore = set()
ImperialFrontier = []
heapq.heappush(ImperialFrontier, (0,StartPoint,"E",None))
CacheDirction = {"S": "S", "N": "S", "E": "E", "W": "E"}

Part1Answer = None

while ImperialFrontier:
    Score, Location, Direction, HistoryTuple = heapq.heappop(ImperialFrontier)
    if HistoryTuple == None:
        HistorySet = set()
    else:
        HistorySet = set(HistoryTuple)
    CD = CacheDirction[Direction]
    if (Location, CD) in ImperialCore and Score > LocationScoreDict[(Location, CD)]:
        continue
    if Part1Answer != None and Score > Part1Answer:
        break
    ImperialCore.add((Location, CD))
    LocationScoreDict[(Location, CD)] = Score
    X, Y = Location
    HistorySet.add(Location)
    if Location == EndPoint:
        Part1Answer = Score
        BestLocationsSet = BestLocationsSet | HistorySet
        continue
    NewHistoryTuple = tuple(HistorySet)

    for DX, DY, AddScore, NewD in DirectionDict[Direction]:
        NX, NY = X+DX, Y+DY
        NewLoc = (NX, NY)
        if NewLoc not in OpenSet:
            continue
        NewScore = Score + AddScore
        heapq.heappush(ImperialFrontier, (NewScore, NewLoc, NewD, NewHistoryTuple))

print(len(ImperialCore), len(ImperialFrontier))

Part2Answer = len(BestLocationsSet)
print(f"{Part1Answer = }")
print(f"{Part2Answer = }")
