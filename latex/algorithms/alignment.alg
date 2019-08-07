def alignTraces(leftTrace, rightTrace):
    results := []
    score := 0

    if leftTrace.size() == 0 || rightTrace.size() == 0
        || leftTrace[0] != rightTrace[0]:
        return (-1, [])

    while leftTrace.size() > 0 || rightTrace.size() > 0:
        if leftTrace.size() == 0:
            results.append(map(Right, rightTrace))
            rightTrace := []
        elif rightTrace.size() == 0:
            results.append(map(Left, leftTrace))
            leftTrace := []
        else:
            (leftEvent:leftTail) := leftTrace
            (rightEvent:rightTrail) := rightTrace

            if leftEvent == rightEvent:
                score := score + 1
                results.append(Common(leftEvent))
                leftTrace := leftTail
                rightTrace := rightTail
            elif leftEvent.stack.size() <= rightEvent.stack.size():
                result.append(Right(rightEvent))
                rightTrace := rightTail
            else:
                result.append(Left(leftEvent))
                leftTrace := leftTail

    return (score, results)