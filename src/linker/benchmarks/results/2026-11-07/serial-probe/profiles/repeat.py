import subprocess,sys
for _ in range(int(sys.argv[1])):
 subprocess.run(sys.argv[2:],check=True,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)
