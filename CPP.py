import ply.lex as lex
import ply.yacc as yacc
import operator
ops = { "+": operator.add, "-": operator.sub,"*": operator.mul,"/": operator.div,"%": operator.mod }

types = {'int' : 'number','string' : 'string','char' : 'char','double' : 'number','bool':'bool'}

pythontypes = {'double':float,'int': float, 'string': str ,'bool':bool}
array = []



datatypes = (
	'int',
	'double',
	'string',
	'char',
	'bool'
)
tokens = (
	'MAIN',
	'COLON',
	'CASE',
	'SWITCH',
	'BREAK',
	'LT',
	'GT',
	'LE',
	'GE',
	'PLUS',
	'MINUS',
	'MUL',
	'DIV',
	'MOD',
	'PLUSPLUS',
	'MINUSMINUS',
	'EQUAL',
	'DOUBLEEQUAL',
	'NOTEQUAL',
	'VAR',
	'IDENTIFIERCHAR',
	'IDENTIFIERSTR',
	'NUMBER',
	'SEMICOLON',
	'LPAREN',
	'RPAREN',
	'LBRACE',
	'RBRACE',
	'LSBRACK',
	'RSBRACK',
	'COMMA',
	'TYPE',
	'COUT',
	'LOP',
	'DO',
	'WHILE',
	'ENDL',
	'INCLUDE',
	'NAMESPACE',
	'USING',
	'IO',
	'STD',
	'IDENTIFIERBOOL',
	'DEFAULT',
	'RETURN'
	)

t_COMMA = r','
t_ignore = ' \t\v\r'
t_MOD = r'%'
t_DIV = r'/'
t_MUL = r'\*'
t_MINUSMINUS = r'\-\-'
t_PLUSPLUS = r'\+\+'
t_DOUBLEEQUAL = r'\=\='
t_EQUAL = r'='
t_GE =  r'>='
t_GT =   r'>'
t_LE =  r'<='
t_LPAREN = r'\('
t_LT = r'<'
t_MINUS = r'\-'
t_NOTEQUAL = r'!='
t_PLUS = r'\+'
t_RPAREN =  r'\)'
t_SEMICOLON =   r';'
t_LSBRACK = r'\['
t_RSBRACK = r'\]'
t_COLON = r':'
t_RBRACE = r'\}'
t_LBRACE = r'\{'
def t_RETURN(t):
	r'return'
	return t
def t_DEFAULT(t):
	r'default'
	return t
def t_MAIN(t):
	r'main'
	return t
def t_NAMESPACE(t):
	r'using namespace std;'
	return t
def t_USING(t):
	r'using'
	return t
def t_STD(t):
	r'std'
	return t		

def t_INCLUDE(t):
	r'\#include'
	return t;	
def t_IO(t):
	r'iostream'
	return t

def t_LOP (t):
	r'<<'
	return t

def t_NUMBER(token):
        r'-?[0-9]+(?:\.[0-9]*)?'
        token.value = float(token.value)
        return token
def t_IDENTIFIERSTR(token):
	r'"[^"]*"'
	token.value = token.value[1:-1]
	return token
def t_IDENTIFIERCHAR(token):
	r"'[^']'"
	token.value = token.value[1:-1]
	return token
def t_IDENTIFIERBOOL(token):
	r"true | false"
	return token		
def t_ENDL(t):
	r'endl'
	return t	
def t_CASE(t):
	r'case'
	return t;

def t_SWITCH(t):
	r'switch'
	return t

def t_BREAK(t):
	r'break'
	return t;	

def t_WHILE(t):
	r'while'
	return t

def t_TYPE(token):
	r'int | double | bool | string | char | void'
	return token
def t_DO(t):
	r'do'
	return t	

def t_COUT (token):
	r'cout'
	return token

def t_VAR(token):
	r'[A-Za-z_][A-Za-z_0-9]*'
	return token

def print_no_newline(string):
    import sys
    sys.stdout.write(string+" ")
    sys.stdout.flush()


def t_newline(t):
        r'\n'
        t.lexer.lineno += 1

def t_error(t):
	global flag_for_err
	flag_for_err = 1
	print "c++ Lexer : Illegal character " + t.value[0] + ' at line number :' + str(t.lexer.lineno)
	t.lexer.skip(1)

def p_header(p):
	'header : INCLUDE LT IO GT program'
	p[0] = p[5]

def p_header1(p):
	'header : program'
	p[0] = p[1]	


def p_program_funcdef(p):
	'program : funcdef program'
	p[0] = [p[1]] + p[2]

def p_program_statement(p):
	'program : statement'
	p[0] = p[1]	
def p_funcdef(p):
	'funcdef : TYPE VAR LPAREN arg RPAREN LBRACE statement RBRACE'
	p[0] = ('funcdef',p[1],p[2],p[4],p[7],p.lineno(1))
def p_arg(p):
	'''arg : TYPE VAR COMMA arg'''
	p[0] = p[4] + [p[1],p[2]]			
def p_arg1(p):
	'arg : TYPE VAR'
	p[0] = [p[1],p[2]]
def p_arg2(p):
	'arg : '
	p[0] = []

def p_statement(p):
	'''statement : assign statement
		| declare statement	
		| access statement 
		| switch statement
		| increment statement
		| print statement
		| case statement
		| break statement
		| dowhile statement
		| functioncall statement
		| main statement
		| return statement'''
	p[0] = [p[1]] + p[2]

def p_ret(p):
	'return : RETURN exp SEMICOLON'
	p[0] = ('return',p[2])

def p_main(p):
	'main : TYPE MAIN LPAREN arg1 RPAREN LBRACE statement RBRACE'
	p[0] = ('main',p[7],p.lineno(1))

def p_funcall (p):
	'functioncall : VAR LPAREN arg1 RPAREN SEMICOLON'
	p[0] = ('functioncall',p[1],p[3],p.lineno(1))
def p_argf(p):
	'''arg1 : exp COMMA arg1'''
	p[0] = p[3] + [p[1]]
def p_argf1(p):
	'arg1 : exp'
	p[0] = [p[1]]
def p_argf2(p):
	'arg1 : '
	p[0] = []	

def p_break(p):
	'break : BREAK SEMICOLON'
	p[0] = ('break',p[1])		
def p_increment(p):
 	'''increment : VAR PLUSPLUS SEMICOLON
		| VAR MINUSMINUS SEMICOLON'''
	p[0] = ('incrementstatement',p[1],p[2],p.lineno(1))

def p_increment2(p):
 	'''increment : PLUSPLUS VAR SEMICOLON
		| MINUSMINUS VAR SEMICOLON'''
	p[0] = ('incrementstatement2',p[2],p[1],p.lineno(1))

def p_statementempty(p):
	'statement : '
	p[0] = [ ]


def p_print(p):
	'print : COUT arrow SEMICOLON'
	p[0] = ('print',p[2],p.lineno(1))

def p_func11(p):
	'arrow : LOP exp arrow'
	p[0] = [p[2]] +p[3]
def p_func12(p):
	'arrow : LOP ENDL arrow'
	p[0] = [p[2]] +p[3]	
def p_func13(p):
	'arrow : '	
	p[0] = [] 

def p_switch(p):
	'switch : SWITCH LPAREN exp RPAREN LBRACE statement default RBRACE'
	p[0]= ('switch',p[1],p[3],p[6],p[7],p.lineno(1))	

def p_case(p):
	'case : CASE exp COLON statement'
	p[0] = ('case',p[1],p[2],p[4],p.lineno(1))
def p_caseemp(p):
	'case : '
	p[0]= []
def p_default(p):
	'default : DEFAULT COLON statement'
	p[0] = p[3]
def p_defaultemp(p):
	'default : '
	p[0] = []


def p_assignmentvaraccess(p):
	'access : VAR EQUAL exp SEMICOLON '
	p[0] = ('accessingvar',p[1],p[2],p[3],p.lineno(1))

def p_assignmentTYPE(p):
	'assign : TYPE VAR EQUAL exp SEMICOLON '
	p[0] = ('assignmentvar',p[1],p[2],p[3],p[4],p.lineno(1))

def p_declaration(p):
	'declare : TYPE VAR SEMICOLON ' 
	p[0] = ('decvar',p[1],p[2],p.lineno(1))		

def p_array_decl(p):
	'declare : TYPE VAR LSBRACK size RSBRACK SEMICOLON'  
	p[0] = ('arraydec',p[1],p[2],p[4],p.lineno(1))

def p_array_assignment(p):
	'assign : TYPE VAR LSBRACK size RSBRACK EQUAL array SEMICOLON' 
	p[0] = ('arrayassign',p[1],p[2],p[4],p[6],p[7],p.lineno(1))

def p_array_assign(p):
	'assign : VAR LSBRACK size RSBRACK EQUAL exp SEMICOLON'  
	p[0] = ('arrayassignindex',p[1],p[3],p[5],p[6],p.lineno(1))

# def p_array_assign1(p):
# 	'assign : VAR EQUAL VAR SEMICOLON'
# 	p[0] = ('arrayassignarray',p[1],p[2],p[3],p.lineno(1))	

def p_size(p):
	'size : exp'
	p[0] = p[1]

def p_arr(p):
	'array : LBRACE data RBRACE'
	p[0] = p[2]	

def p_data1(p):
	'''data : data COMMA exp'''
	p[0] = p[1] + [p[3]] 

def p_arr1(p):
	'array : VAR'
	p[0] = ('VAR',p[1],p.lineno(1))

def p_data2(p):
	'data : exp'
	p[0] = [p[1]]
# def p_data3(p):
# 	'data : IDENTIFIERSTR'
# 	p[0] = ('string',p[1])

# def p_data7(p):
# 	'data : IDENTIFIERCHAR'
# 	p[0] = ('char',p[1])
	

def p_exp(p):
	'exp : exp op exp'
	p[0] = ('expression',p[1],p[2],p[3],p.lineno(1))

def p_exp2(p):
	'exp : NUMBER'
	p[0] = ('number',p[1],p.lineno(1))

def p_exp3(p):
	'exp : IDENTIFIERSTR'
	p[0] = ('string',p[1],p.lineno(1))

def p_exp4(p):
	'exp : IDENTIFIERCHAR'
	p[0] = ('char',p[1],p.lineno(1))

def p_expbool(p):
	'exp : IDENTIFIERBOOL'
	p[0] = ('bool',p[1],p.lineno(1))	

def p_exp6(p):
	'exp : VAR'
	p[0] = ('VAR' , p[1],p.lineno(1))

def p_expparen(p):
	'exp : LPAREN exp RPAREN'
	p[0] = p[2]

def p_exppmm(p):
	'''exp : VAR PLUSPLUS 
		| VAR MINUSMINUS'''
	p[0] = ('expressionppmm', p[1],p[2],p.lineno(1))
def p_exppmm2(p):
	'''exp : PLUSPLUS VAR
		| MINUSMINUS VAR'''
	p[0] = ('ppmmexpression', p[1],p[2],p.lineno(1))	

def p_arrayaccess(p):
	'exp : VAR LSBRACK exp RSBRACK'
	p[0] = ('arrayelemaccess',p[1],p[3],p.lineno(1))
def p_funcalll(p):
	'exp : VAR LPAREN arg1 RPAREN'
	p[0]=('functioncall',p[1],p[3],p.lineno(1))
def p_op(p):
	'''op : PLUS
		| MINUS
		| EQUAL
		| DOUBLEEQUAL 
		| GE 
		| LE 
		| LT
		| NOTEQUAL 
		| GT
		| MOD
		| MUL
		| DIV'''

	p[0] = p[1]

def p_dowhile(p):
	'dowhile : DO LBRACE statement RBRACE WHILE LPAREN exp RPAREN SEMICOLON'
	p[0] = ('dowhile',p[3],p[7],p.lineno(1))
def p_error(p):
	global flag_for_error
	flag_for_error = 1
	if(p==None):
		print('syntax error at line no : ',dlexer.lineno)
		print ('bracket or semicolon missing or invalid token');
	if (p.lineno!=1):
		print('syntax error at line no : ',p.lineno-1)
		print ('bracket or semicolon missing or invalid token');
	else:
		print('error at line no : ',p.lineno)
		print ('bracket or semicolon missing or invalid token');	

def check_type(t):
	if(t[0]!='expression' and t[0] !='functioncall'):
		array.append(t[0])
	if(t[0]=='expression'):
		check_type(t[1])
		check_type(t[3])

	return array

def eval_expression1(t,en):
	if(t[0]=='number' or t[0] == 'string' or t[0] == 'bool' or t[0] =='char'):
		return t[1];
	if(t=='endl'):
		return 'endl'
	elif (t[0]=='expressionppmm'):
		var = t[1]
		op = t[2]
		value = envir_lookup(var,en)
		if (value == None):
			print ("Variable is not defined : ",t[1])
			print ("line number",t[3])
		elif (value =='empty'):
			print('VARIABLE has no value, just declared :',t[1])
			print ("line number",t[3])
		else:
			if(op =='++'):
				update = value[1] + 1
				newval  = (value[0],update)
			else:
				update = value[1] - 1
				newval  = (value[0],update)
			envir_update(var,newval,environment)
			return value[1]
	elif (t[0] =='ppmmexpression'):
		var = t[2]
		op = t[1]
		value = envir_lookup(var,en)
		if (value == None):
			print ("Variable is not defined : ",t[2])
			print ("line number",t[3])
		elif (value =='empty'):
			print('VARIABLE has no value, just declared :',t[2])
			print ("line number",t[3])
		else:
			if(op =='++'):
				update = value[1] + 1
				newval  = (value[0],update)
			else:
				update = value[1] - 1
				newval  = (value[0],update)
			envir_update(var,newval,environment)
			return update
					
	elif (t[0] =='expression'):
		op = t[2]


		l=eval_expression1(t[1],en)
		r=eval_expression1(t[3],en)
		if (l!=None and r!=None):
			if (op=='<'):
				return (l<r)
			elif (op=='=='):
				return (l==r)
			elif (op=='!='):
				return (not(l==r))
			elif (op=='>'):
				return (l>r)
			elif (op=='<='):
				return (l<=r)
			elif (op=='>='):
				return (l>=r)
			else:						
				return (ops[op](l,r))
		else:
			return None		
	
	elif (t[0] == 'VAR'):
		value = envir_lookup(t[1],en)
	
		if (value == None):
			print ("Variable is not defined : ",t[1])
			print ("line number",t[2])
			return None
		elif (value[1] == 'empty'):
			print('VARIABLE has no value, just declared :',t[1])
			print ("line number",t[2])	
			return None	
		else :
			return value[1]
	elif (t[0]=='arrayelemaccess'):
		name = t[1]
		index = eval_expression1(t[2],en)
		index = int(index)

		value = envir_lookup(name,en)
		if(index < 0 or index >= value[1]):
			print("ERROR: INDEX OUR OF RANGE")
			print ("at line no:" ,	t[3])
			return None

		elif(index > len(value[0])-1):
			return 0;
		else:	
			if (value == None):
				print ("Array is not defined : ",t[1])
				print ("line no:" , t[3])
				return None
			elif (value[1] == 'empty'):
				print('Array has no elements, just declared :',t[1])
				print ("line no:" , t[3])
				return None		
			else :
				return value[0][index]
def eval_expression(t,en,ty):
	if((t[0]=='number' and types[ty]==t[0]) or (t[0]=='string' and types[ty]==t[0]) or (t[0]=='bool' and types[ty]==t[0]) or (t[0]=='char' and types[ty]==t[0])):
		return t[1];
	elif(t=='endl'):
		return 'endl'
	elif (t[0]=='expressionppmm'):
		var = t[1]
		op = t[2]
		value = envir_lookup(var,en)
		if (value == None):
			print ("Variable is not defined with name : ",t[1])
			print ("line number",t[3])
			return None
		elif (value =='empty'):
			print('VARIABLE has no value, just declared :',t[1])
			print ("line number",t[3])
			return None
		else:
			if(op =='++'):
				update = value[1] + 1
				newval  = (value[0],update)
			else:
				update = value[1] - 1
				newval  = (value[0],update)
			envir_update(var,newval,environment)
			return value[1]
	elif (t[0] =='ppmmexpression'):
		var = t[2]
		op = t[1]
		value = envir_lookup(var,en)
		if (value == None):
			print ("Variable is not defined : ",t[2])
			print ("line number",t[3])
			return None
		elif (value[1] =='empty'):
			print('VARIABLE has no value, just declared :',t[2])
			return None
		else:
			if(op =='++'):
				update = value[1] + 1
				newval  = (value[0],update)
			else:
				update = value[1] - 1
				newval  = (value[0],update)
			envir_update(var,newval,environment)
			return update
					
	elif (t[0] =='expression'):
		op = t[2]
		l=eval_expression(t[1],en,ty)
		r=eval_expression(t[3],en,ty)
		if(l!=None and r!=None):		
			if (op=='<'):
				return (l<r)
			elif (op=='=='):
				return (l==r)
			elif (op=='!='):
				return (not(l==r))
			elif (op=='>'):
				return (l>r)
			elif (op=='<='):
				return (l<=r)
			elif (op=='>='):
				return (l>=r)
			else:						
				return (ops[op](l,r))
		else :
			return None
	
	elif (t[0] == 'VAR'):
		value = envir_lookup(t[1],en)
		if (value == None):
			print ("Variable is not defined : ",t[1])
			print ("line no:" , t[2])
			return None

		typee = value[0]
		data = value[1]
		if ty!=typee and not(type(data) is pythontypes[ty]):
			print ('TYPE MIS MATCH ERROR FOR VARIABLE',t[1])
			print ("line no:" , t[2])
			return None
		if (value == None):
			print ("Variable is not defined : ",t[1])
			print ("line no:" , t[2])
			return None
		elif (data == 'empty'):
			print('VARIABLE has no value, just declared :',t[1])
			print ("line no:" , t[2])		
			return None
		else :
			return value[1]
	elif (t[0]=='arrayelemaccess'):
		name = t[1]
		index = eval_expression(t[2],en,ty)
		index = int(index)

		value = envir_lookup(name,en)
		if(index < 0 or index >= value[1]):
			print("ERROR: INDEX OUR OF RANGE")
			print ("line no:" , t[3])
		elif(index > len(value[0])-1):
			return 0;
		else:	
			if (value == None):
				print ("Array is not defined : ",t[1])
				print ("line no:" , t[3])
				return None
			elif (value[1] == 'empty'):
				print('Array has no elements, just declared :',t[1])
				print ("line no:" ,	 t[3])
				return None
			else :
				return value[0][index]
	elif (t[0] =='functioncall'):
		funcname = t[1]
		typesss=[]
		args = list(map((lambda x: (x[1],eval_expression1(x,en))),t[2]))
		argtypes = []
		for a in args:
			argtypes.append(type((a[1])))
		value = envir_lookup(funcname,en)
		functionargstypes = value[2]
		ind = 0
		for ty in argtypes:
			if (not(ty is pythontypes[functionargstypes[ind]])):
				print ("TYPE MIS MATCH ERROR IN FUNCTION ARGUMENTS")
				print("line no : " , t[3])
				return -1;
			ind=ind+1


		if (value == None):
			print " no such function defined with name : ",funcname
			print ("line no : " , t[3])
			return -1
		else:
			statements = value[0]
			functionargs = value[1] #it is map
			functionargstypes = value[2]
			
			# if(functionargstypes!=typesss):
			# 	print('ERROR: TYPE MISMATCH IN ARGUMENTS OF CALLING THE FUNCION ',  funcname)
			# 	print ("line no : " , t[3])
			# 	return -1   # [a,b]
			if (len(functionargs) > len(args)):
				print "ERROR:few arguments provided than expected"
				print ("line no : " , t[3])
				return -1
			elif (len(functionargs) < len(args)):
				print "ERROR : more arguments provided than expected"
				print ("line no : " , t[3])
				return -1	
			index = 0;
			# print functionargs
			args = list(reversed(args))
			newmap = {}
			for i in functionargs:
				newmap[i] = args[index]
				index=index+1
			newenvironment = (en,newmap)
			checkerror = 0
			for s in statements:
				if (checkerror!=-1):
					if (s[0]=='dowhile'):
						whileenvironment = (newenvironment,{})
						checkerror=eval_statements(s,whileenvironment,1)
					if(s[0] == 'return'):
						return (eval_expression1(s[1],newenvironment))
						break;

					else:	
						checkerror=eval_statements(s,newenvironment,0)
	else:
		print ('TYPE MIS MATCH ERROR')
		print ("line no:" , t[2])
		return None;			

def eval_statements(t,environment,check):
	nodetype = t[0]
	if (nodetype == 'assignmentvar'): # int x = something
		vartype = t[1]
		varname = t[2]
		check_type(t[4])
		if(len(array)!=0):
			checklist = list(map((lambda x : (types[t[1]] == x or x=='VAR' or x=='expressionppmm' or x=='ppmmexpression')),array))
			while len(array) > 0 : 
	 				array.pop()
			if (False in checklist):
				print ("TYPE MISMATCH ERROR : ",varname)
				print ("line no : " , t[5])
				return -1;
			else:
				value = eval_expression(t[4],environment,vartype)
				if (value==None):
					return -1;
				
				if (envir_lookupinenv(varname,environment)==None):
					envir_updateinenv(varname , (vartype,value) ,environment)
					# the variable can be declared, not initially used
				elif(check == 0):
					print('variable named already defined, REDEFINING error, nameofvariable : ', varname)
					print ("line no : " , t[5])
					return -1;
				else:
					envir_updateinenv(varname ,(vartype,value),environment)
		else :
			value = eval_expression(t[4],environment,vartype)
			if (value==None):
				return -1;
				
			if (envir_lookupinenv(varname,environment)==None):
				envir_updateinenv(varname , (vartype,value) ,environment)
				# the variable can be declared, not initially used
			elif(check == 0):
				print('variable named already defined, REDEFINING error, nameofvariable : ', varname)
				print ("line no : " , t[5])
				return -1;
			else:
				envir_updateinenv(varname ,(vartype,value),environment)



		while len(array) > 0 : 
			array.pop()
	elif (nodetype == 'print'):
				   #cout << something
		toprint = t[1]
		# print tobeprint    #array of expressions or string to be printed;
		for i in (list (map((lambda x : eval_expression1(x,environment)),toprint))):
			if (i!=None):
				if(i=='endl'):
					print
				else:
					print_no_newline(str(i))
	elif (nodetype=='accessingvar'):   # x = something
		val = envir_lookup(t[1],environment)
		if(val) == None:
			print ('variable is not defined with name :',t[1])
			print ("line no : " , t[4])
			return -1;
		else :
			v = eval_expression(t[3],environment,val[0])
			if v==None:
				return -1
			envir_update(t[1],(val[0],v),environment)		
	elif(nodetype=='expressionstat'):
		eval_expression1(t[1],environment)

	elif (nodetype=='decvar'): # int x;
		if(envir_lookupinenv(t[2],environment)==None):
			envir_updateinenv(t[2],(t[1],'empty'),environment)
		elif(check==0):
			print('variable named already defined, REDEFINING error, nameofvariable : ', t[2])
			print ("line no : " , t[3])
			return -1;
	elif (nodetype == 'arraydec'):
		if(envir_lookupinenv(t[2],environment)==None):
			size = eval_expression1(t[3],environment)
			size = int(size);
			envir_updateinenv(t[2],([None]*size,size),environment)
		elif(check==0):
			print('variable named already defined, REDEFINING error, nameofvariable : ', t[2])
			print ("line no : " , t[4])
			return -1;
	elif(nodetype =='arrayassign'):
		if(envir_lookupinenv(t[2],environment)==None):
			elements= []
			typ = t[1]
			size = eval_expression1(t[3],environment)
			tupplearray = t[5]
			
			elements = list(map((lambda x : x[1]),tupplearray))
			typess = list(map((lambda x : x[0]),tupplearray))
			check =  list(map((lambda x : types[typ]==x ),typess))
			if False in check:
				print ("TYPE MISMATCH ERROR IN ARRAY ASSIGNMENT with name : " , t[2])
				print ("line no : " , t[6])
				return -1

			if(len(elements) <= size):
				envir_updateinenv(t[2],(elements,size),environment)
			else:
				print("ERROR, ELEMENTS ARE MORE THAN size")
				print ("line no : " , t[6])
				return -1;	
		elif (check==0):
			print('variable named already defined, REDEFINING error, nameofvariable : ', t[2])
			print ("line no : " , t[6])
			return -1;
		while len(array) > 0 : 
 				array.pop()

	elif(nodetype == 'arrayassignindex'):
		name = t[1]
		index = eval_expression1(t[2],environment)
		if (index==None):
			return -1
		index = int(index);
		value = envir_lookup(t[1],environment)
		if(value==None):
			print("ERROR : NO array defined with name : ",t[1])
			print ("line no : " , t[5])
			return -1;
		elif(index < 0 or index >= value[1]):
			print("ERROR: INDEX OUR OF RANGE")
			print ("line no : " , t[5])
			return -1;
		else: 
			value[0][index] = eval_expression1(t[4],environment)
			if(value[0][index]==None):
				return -1;
			envir_update(name,value,environment)
	elif(nodetype == 'incrementstatement'):
		name= t[1]
		op = t[2]
		value = envir_lookup(name,environment)
		if (value==None):
			print('ERROR: NO SUCH VARIABLE DEFINED :',name)
			print ("line no : " , t[3])
			return -1;
		if(value[1]=='empty'):
			print('ERROR: VARIABLE HAS NO VALUE iNitialized', name)
			print ("line no : " , t[3])
			return -1;
		else:
			if(op =='++'):
				update = value[1] + 1
				newval  = (value[0],update)
			else:
				update = value[1] - 1 
				newval  = (value[0],update)
			envir_update(name,newval,environment)
	elif(nodetype == 'incrementstatement2'):
		name= t[1]
		op = t[2]
		value = envir_lookup(name,environment)
		if (value==None):
			print('ERROR: NO SUCH VARIABLE DEFINED :',name)
			print ("line no : " , t[3])
			return -1;
		if(value[1]=='empty'):
			print('ERROR: VARIABLE HAS NO VALUE iNitialized', name)
			print ("line no : " , t[3])
			return -1;
		else:
			if(op =='++'):
				update = value[1] + 1
				newval  = (value[0],update)
			else:
				update = value[1] - 1
				newval  = (value[0],update)
			envir_update(name,newval,environment)		


	# elif(nodetype == 'arrayassignarray'):
	# 	name1 =t[1]
	# 	name2 = t[3]
	# 	value1 = envir_lookup(name1,environment)
	# 	value2 = envir_lookup(name2,environment)
	# 	if (value1==None):
	# 		print('ERROR: NO SUCH VARIABLE or Array DEFINED :',name1)
	# 		print ("line no : " , t[4])
	# 		return -1;
	# 	if(value2==None):
	# 		print('ERROR: NO SUCH VARIABLE or Array DEFINED :',name2)
	# 		print ("line no : " , t[4])
	# 		return -1;
	# 	else:
	# 		value1 = value2
	# 		envir_update(name1,value1,environment)
	elif(nodetype == 'dowhile'):
		newenvironment = (environment,{})
		statements = t[1]
		for stats in statements:
			if (stats[0] == 'dowhile'):
				error = eval_statements(stats,newenvironment,1)
				if(error==-1):
					return -1

			else :
				error = eval_statements(stats,environment,1)
				if(error==-1):
					return -1
		condition = eval_expression1(t[2],environment)

		while condition:
			for stats in statements:
				if (stats[0] == 'dowhile'):
					check = eval_statements(stats,newenvironment,1)
					if (check ==-1):
						return -1
				else :
					check=eval_statements(stats,environment,1)
					if (check ==-1):
						return -1
			condition = eval_expression1(t[2],environment)
	elif(nodetype=='switch'):
		conditionvar = eval_expression1(t[2],environment)
		statementscase = t[3]
		for s in statementscase:
			if(s[0]=='case'):
				newenvironment = (environment,{})
				# eval_case(s,newenvironment,conditionvar,environment,0)
				if (eval_case(s,newenvironment,conditionvar,environment,0)==None):
					for s in t[4]:
						newenvironment1 = (environment,{})
						eval_statements(s,newenvironment1,0)
	elif(nodetype== 'funcdef'):
		returntype = t[1]
		funcname = t[2]
		map_args = extract_arg(t[3])
		mynewmap={}
		typess = []
		for i in  map_args:
			for k in range(0,len(t[3])):
				if t[3][k] == i:
					typess.append(t[3][k-1])

		
		statements = t[4]
		value = (statements,map_args,typess)
		envir_update(funcname,value,environment)
	elif (nodetype=='functioncall'):
		funcname = t[1]
		typesss=[]
		args = list(map((lambda x: (x[1],eval_expression1(x,environment))),t[2]))
		argtypes = []
		for a in args:
			argtypes.append(type((a[1])))
		value = envir_lookup(funcname,environment)
		functionargstypes = value[2]
		ind = 0
		for ty in argtypes:
			if (not(ty is pythontypes[functionargstypes[ind]])):
				print ("TYPE MIS MATCH ERROR IN FUNCTION ARGUMENTS")
				print("line no : " , t[3])
				return -1;
			ind=ind+1


		if (value == None):
			print " no such function defined with name : ",funcname
			print ("line no : " , t[3])
			return -1
		else:
			statements = value[0]
			functionargs = value[1] #it is map
			functionargstypes = value[2]
			
			# if(functionargstypes!=typesss):
			# 	print('ERROR: TYPE MISMATCH IN ARGUMENTS OF CALLING THE FUNCION ',  funcname)
			# 	print ("line no : " , t[3])
			# 	return -1   # [a,b]
			if (len(functionargs) > len(args)):
				print "ERROR:few arguments provided than expected"
				print ("line no : " , t[3])
				return -1
			elif (len(functionargs) < len(args)):
				print "ERROR : more arguments provided than expected"
				print ("line no : " , t[3])
				return -1	
			index = 0;
			# print functionargs
			args = list(reversed(args))
			newmap = {}
			for i in functionargs:
				newmap[i] = args[index]
				index=index+1
			newenvironment = (environment,newmap)
			checkerror = 0
			for s in statements:
				if (checkerror!=-1):
					if (s[0]=='dowhile'):
						whileenvironment = (newenvironment,{})
						checkerror=eval_statements(s,whileenvironment,1)
					if(s[0] == 'return'):
						break;

					else:	
						checkerror=eval_statements(s,newenvironment,0)
					

		while len(array) > 0 : 
 				array.pop()
 	elif(nodetype=='main'):
 		checkerror = 0
 		for s in t[1]:
 			if(checkerror!=-1):
 				checkerror=eval_statements(s,environment,0)


def eval_case(s,ownenv,condition,globalenv,check):
	flag = 0
	default = 1
	casevalue = eval_expression1(s[2],globalenv)
	statements = s[3]
	if(casevalue==condition or check==1):
		if(casevalue==condition):
			default=0;
		for s in statements:
			eval_statements(s,ownenv,0)
			if (s[0] =='break'):
				flag = 1;
		if(flag==1):
			return flag
		else:
			for s in statements:
				if (s[0]=='case'):
					newenvironment = (globalenv,{})
					return eval_case(s,newenvironment,condition,globalenv,1)					
	else:
		for st in statements:
			if (st[0]=='case'):
				newenvironment = (globalenv,{})

				return eval_case(st,newenvironment,condition,globalenv,0)		




 
def envir_update(vname, value, env):
	if vname in env[1]:
		env[1][vname] = value
	elif not env[0] == None:
		envir_update(vname,value, env[0])
	else:
		env[1][vname] = value

def envir_updateinenv(vname, value, env):
		env[1][vname] = value		

def envir_lookup(vname, env):
	if vname in env[1]:
		return env[1][vname]
	elif env[0] == None:
		return None
	else:
		return envir_lookup(vname, env[0])
def envir_lookupinenv(vname, env):
	if vname in env[1]:
		return env[1][vname]
	else:
		return None

def get_elements(t,string):
	
	if t[0] == types[string]:
		array.append(t[1])
	else:
		array.append(t[1])
		get_elements(t[0],string)	
	return array

def extract_arg(t):	
	mymap = {}
	i = 0
	types = []
	names = []
	for a in t:
		if(i%2==1):
			names.append(a)
		i=i+1
	names = list(reversed(names))
	return names

	
string = 'int b =1; b++; int array[3] = {5,6,7}; array[1] = 3; cout << array[1] << b;'
File = open("test.cpp","r")
code= File.read()
flag_for_error = 0
flag_for_err = 0
dlexer = lex.lex()

dparser = yacc.yacc()
tree = dparser.parse(code, lexer=dlexer)
arg = 0
environment = (None,{})
checkerror=0;
if (flag_for_error!=1 and flag_for_err !=1):
	for t in tree:
 	# 	print get_elements(t[5],t[1])
 		if(checkerror!=-1):
 			if(t[0]=='dowhile' or t[0] == 'main'):
 				newenvironment = (environment,{})
 				checkerror = eval_statements(t,newenvironment,1)
 			else:
 				checkerror=eval_statements(t,environment,0)

 			while len(array) > 0 : 
 				array.pop()
		else:
			print "PROGRAM STOPPED"				
else:
 	print('INVALID SYNTAX ')