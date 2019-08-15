def stableMatching(men, women):
    for m in men:
        m.mathing = free
    for w in women:
        w.matching = free

    while there exists a free man m who still has a women to propose to:
        w := first women on m list that he has not yet proposed to
        if w.matching == free:
            m.matching := w
            w.matching := m
        else:
            m' := w.matching // current provisional partner of w
            if w.preferences[m] > w.preferences[m']:
                m'.matching = free
                m.matching = w
                w.matching = m
            else:
                pass // w and m' remain engaged