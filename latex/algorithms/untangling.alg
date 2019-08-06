def untangleEvents(events):
    closedTraces := $\emptyset$
    openTraces := $\emptyset$

    for event in events:
        if event.type == FunctionEntry and |event.stack| == 1:
            openTraces.insert([event])
        else:
            matchingSubtrace := findMatchingSubtrace(event, openTraces)
            openTraces.remove(matchingSubtrace)
            matchingSubtrace.append(event)

            if event.type == FunctionExit and |event.stack| == 0:
                closedTraces.insert(matchingSubtrace)
            else:
                openTraces.insert(matchingSubtrace)

    return closedTraces

def findMatchingSubtrace(ev, traces):
    st := case
        ev.type in [FunctionEnter, GeneratorEnter] $\rightarrow$ tail ev.stack
        ev.type in [FunctionExit, GeneratorSuspend] $\rightarrow$ (ev.loc : ev.stack)
        otherwise $\rightarrow$ ev.stack

    return trace whose last event's stack equals st