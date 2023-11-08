from itertools import cycle

numList = ['Let it happen', 'Roses', 'violent crimes','INFO','dancing queen','3005']

circularList = cycle(numList)

numList.append("NEW")

n = 0
c = True

while c:
    for num in circularList:
        print(num)
        n = n + 1
        if n >= 4:
            c = False
            break