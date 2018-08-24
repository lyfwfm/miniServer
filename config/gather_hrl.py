# 将所有配置的头文件record定义 放到一个文件中
import time,os,json

def readHrlFile(fileName):
	print("reading hrl file:"+fileName)
	hrlFile = open(fileName, 'r')
	hrlContent=hrlFile.read()
	print(hrlContent)
	hrlFile.close()
	return hrlContent


def main():
	gatherFile = open("../include/cfg_record.hrl",'w')
	gatherContent=""
	rootdir = os.getcwd()
	list = os.listdir(rootdir) #列出文件夹下所有的目录与文件
	for i in range(0,len(list)):
		fileName=list[i]
		filePath = os.path.join(rootdir,fileName)
		if os.path.isfile(filePath):
			if os.path.splitext(fileName)[1]=='.hrl':
				#你想对文件的操作
				hrlContent=readHrlFile(fileName)
				gatherContent=gatherContent+hrlContent
				print("reading hrl file:"+fileName+" success")
		
	gatherFile.write(gatherContent)
	gatherFile.close()			

	time.sleep(99999)

main()

