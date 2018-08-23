import time,os,json

def readJsonFile(fileName):
	print(fileName)
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
	
def make_content(fileName,jsonTupleList):
	recordName=os.path.splitext(fileName)[0]
	content=""
	for i in range(0,len(jsonTupleList)):
		# 对每个json对象操作
		content = content+"get("+str(jsonTupleList[i]['id'])+")->{"+recordName
		for jsonKey in jsonTupleList[i]:
			value=jsonTupleList[i][jsonKey]
			isdigital = isDigital(value)
			print("----------")
			print(value)
			print(isdigital)
			print("----------")
			if isdigital:
				content = content+","+str(value)
			else :
				content = content+",\""+str(value)+"\""
		content=content+"};\r\n"
	content=content+"get(_)->{}."
	erlFile = open(recordName+".erl",'w')
	erlFile.write(content)
	erlFile.close()

rootdir = os.getcwd()

print(rootdir)

list = os.listdir(rootdir) #列出文件夹下所有的目录与文件
for i in range(0,len(list)):
	fileName=list[i]
	filePath = os.path.join(rootdir,fileName)
	if os.path.isfile(filePath):
		if os.path.splitext(fileName)[1]=='.json':
			print(os.path.splitext(fileName)[0])
			#你想对文件的操作
			print(fileName)
			jsonTuple=readJsonFile(fileName)
			make_content(fileName,jsonTuple)
			

time.sleep(99999)

