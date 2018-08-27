import time,os,json

def readJsonFile(fileName):
	print("making cfg file:"+fileName)
	f = open(fileName, encoding='utf-8')
	jsonTuple = json.load(f)
	return jsonTuple

# 是否是整数或者小数
def isDigital(value):
	if type(value) == int:
		return True
	elif type(value) == str:
		dotCount = value.count('.')
		if dotCount >= 1:
			value1=value.split('.')
			return (str.isdigit(value1[0])) and (str.isdigit(value1[1]))
		else :
			return str.isdigit(value)
	else :
		return False

def make_hrl_content(fileName,jsonTupleList):
	recordName=os.path.splitext(fileName)[0]
	hrlContent="-record("+recordName+",{"
	for jsonKey in jsonTupleList[0]:
		hrlContent=hrlContent+jsonKey+","
	hrlContent=hrlContent[:-1]+"}).\r\n"
	hrlFile = open(recordName+".hrl",'w')
	hrlFile.write(hrlContent)
	hrlFile.close()
	
def make_content(fileName,jsonTupleList):
	recordName=os.path.splitext(fileName)[0]
	content="-module("+recordName+").\r\n-export([get/1]).\r\n"
	for i in range(0,len(jsonTupleList)):
		# 对每个json对象操作
		content = content+"get("+str(list(jsonTupleList[i].values())[0])+")->{"+recordName
		for jsonKey in jsonTupleList[i]:
			value=jsonTupleList[i][jsonKey]
			isdigital = isDigital(value)
			if isdigital:
				content = content+","+str(value)
			else :
				content = content+",\""+str(value)+"\""
		content=content+"};\r\n"
	content=content+"get(_)->{}."
	erlFile = open(recordName+".erl",'w',encoding='utf-8')
	erlFile.write(content)
	erlFile.close()

def main():
	rootdir = os.getcwd()
	fileList = os.listdir(rootdir) #列出文件夹下所有的目录与文件
	for i in range(0,len(fileList)):
		fileName=fileList[i]
		filePath = os.path.join(rootdir,fileName)
		if os.path.isfile(filePath):
			if os.path.splitext(fileName)[1]=='.json':
				#你想对文件的操作
				jsonTuple=readJsonFile(fileName)
				make_hrl_content(fileName,jsonTuple)
				make_content(fileName,jsonTuple)
				print("make cfg file:"+fileName+" success")
				
	time.sleep(99999)

main()

