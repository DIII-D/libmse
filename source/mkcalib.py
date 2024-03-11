#
#add a command to open a tree for edit
#environment path variable must be set
#for tree mydiag use something like
#export mydiag_path=./trees\;
#
from MDSplus import tree
from MDSplus import compound as cmpnd
outtree = tree.Tree(tree='mse',shot=-1,mode="EDIT")
node = outtree.addNode('.CALIB',usage='STRUCTURE')
if True:
    try:
       node = outtree.addNode('.CALIB:BSIGNAL',usage='TEXT')
    except:
       pass
    node.addTag('MSE_BSIGNAL')
    try:
       node = outtree.addNode('.CALIB:GAIN',usage='SIGNAL')
    except:
       pass
    node.addTag('MSE_GAIN')
    try:
       node = outtree.addNode('.CALIB:PHASE',usage='SIGNAL')
    except:
       pass
    node.addTag('MSE_PHASE')
    try:
       node = outtree.addNode('.CALIB:BT_SCALE',usage='SIGNAL')
    except:
       pass
    node.addTag('MSE_SCALE')
    try:
       node = outtree.addNode('.CALIB:DC_OFFSET',usage='SIGNAL')
    except:
       pass
    node.addTag('MSE_OFFSET')
outtree.write()
