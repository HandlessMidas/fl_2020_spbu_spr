import ply.lex as lex
import ply.yacc as yacc
from collections import defaultdict
import sys


tokens = (
	'VAR',
	'ID',
	'GOAL_SPLIT',
	'RELATION_SPLIT',
	'COMMA',
	'RIGHT_BRACKET',
	'LEFT_BRACKET',
	'STOP'
)


t_VAR = r'[A-Z][0-9a-zA-Z]*'
t_ID = r'[a-z][0-9a-zA-Z]*'
t_GOAL_SPLIT = r'\?-'
t_RELATION_SPLIT = r':-'
t_COMMA = r','
t_LEFT_BRACKET = r'\('
t_RIGHT_BRACKET = r'\)'
t_STOP = r'\.'

t_ignore  = ' \t\n'


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lex.lex()


relations = []
goal = None
atoms = set()
varrs = set()


def p_program(p):
	'''program : relation goal'''
	global goal
	goal = p[2]


def p_program_no_relation(p):
	'''program : goal'''
	global goal
	goal = p[1]


def p_relation_single(p):
	'''relation : atom RELATION_SPLIT body'''
	relations.append((p[1], p[3])) 


def p_relation_multiple(p):
	'''relation : atom RELATION_SPLIT body relation'''
	relations.append((p[1], p[3])) 


def p_relation_empty(p):
	'''relation : atom STOP'''
	relations.append((p[1], '')) 



def p_relation_empty_multiple(p):
	'''relation : atom STOP relation'''
	relations.append((p[1], '')) 


def p_goal(p):
	'''goal : GOAL_SPLIT body'''
	p[0] = p[2]


def p_atom(p):
	'''atom : ID LEFT_BRACKET args RIGHT_BRACKET'''
	atoms.add(p[1] + '(' + ''.join(p[3]) + ')')
	p[0] = p[1] + '(' + ''.join(p[3]) + ')'


def p_atom_empty(p):
	'''atom : ID'''
	atoms.add(p[1])
	p[0] = p[1]


def p_args_var_single(p):
	'''args : VAR'''
	p[0] = []
	p[0].append(p[1])
	varrs.add(p[1])


def p_args_var_multiple(p):
	'''args : VAR COMMA args'''
	p[0] = []
	p[0].append(p[1] + ', ' + ''.join(p[3]))
	varrs.add(p[1])


def p_args_atom_single(p):
	'''args : atom'''
	p[0] = p[1]
	atoms.add(p[1])


def p_args_atom_multiple(p):
 	'''args : atom COMMA args'''
 	p[0] = []
 	p[0].append(p[1] + ', ' + ''.join(p[3]))
 	atoms.add(p[1])


def p_body_single(p):
	'''body : atom STOP'''
	p[0] = p[1]
	atoms.add(p[1])


def p_body_multiple(p):
	'''body : atom COMMA body'''
	p[0] = p[1] + ', ' + p[3]
	atoms.add(p[1])


syntax_error = False


def p_error(p):
	global syntax_error
	syntax_error = True
	print (f'Syntax error at {p}')


def parse(s):
	global relations, goal, atoms, syntax_error, varrs
	syntax_error = False
	relations = []
	goal = None
	atoms = set()
	varrs = set()
	yacc.parse(s)
	return relations.copy(), goal, atoms.copy(), varrs.copy(), syntax_error


yacc.yacc()
def parse_file(file):
	with open(file, 'r') as f:
		program = f.read()
		return parse(program)