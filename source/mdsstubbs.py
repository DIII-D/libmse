import MDSplus


mdsserver = None


def mdsconnect(server):
    global mdsserver
    mdsserver = MDSplus.Connection(server)


def mdsdisconnect():
    global mdsserver
    mdsserver = None


def mdsvalue(exp, *largs):
    try:
        return mdsserver.get(exp, arglist=largs)
    except Exception as status:
        print('Mdsvalue: ', status)
        print('          ', exp)
        raise


def mdsput(node, exp, *largs):
    if largs:
        args = largs[0]
    else:
        args = ()
    pargs = [node, exp]
    putexp = "TreePut($,$"
    for a in args:
        putexp = putexp+",$"
        pargs.append(a)
    putexp = putexp+")"
    try:
        return mdsserver.get(putexp, arglist=pargs)
    except Exception as status:
        print('Mdsput: ', status)
        print('        ', node, exp)
        raise
    return status


def mdssetdefault(d):
    return mdsserver.setDefault(d)


def mdsopen(t, s):
    return mdsserver.openTree(t, s)


def mdsclose(t, s):
    return mdsserver.closeTree(t, s)
