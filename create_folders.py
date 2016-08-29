import os
import shutil

folder = "sources"

for file in os.listdir(folder+"/"):
    source = folder+"/"+file
    newpath = folder+"/"+file.split(".")[0]
    os.makedirs(newpath)
    print "Folder " + file.split(".")[0] + "was created"
    shutil.move(source, newpath+"/")
