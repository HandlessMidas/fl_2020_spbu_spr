import ply.lex as lex
import ply.yacc as yacc
from collections import defaultdict
import sys


tokens = (
	'TERM',
	'NONTERM',
	'ARROW',
	'SPLIT',
	'NEWLINE',
	'EPS'
)


t_TERM = r'[a-z]'
t_NONTERM = r'[A-Z]+'
t_ARROW = r'=>'
t_SPLIT = r'\|'
t_EPS = r'eps'


def t_NEWLINE(t):
    r'(\n)+'
    t.lexer.lineno += t.value.count('\n')
    return t


t_ignore  = ' \t'


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lex.lex()


terms = set()
nonterms = set()
rules = defaultdict(list)


def p_rule_single(p):
	'''rule : NONTERM ARROW rhses'''
	nonterms.add(p[1])
	rules[p[1]].append(p[3])


def p_rule_single_line(p):
	'''rule : NONTERM ARROW rhses NEWLINE'''
	nonterms.add(p[1])
	rules[p[1]].append(p[3])

def p_rule_multiple(p):
	'''rule : NONTERM ARROW rhses NEWLINE rule'''
	nonterms.add(p[1])
	rules[p[1]].append(p[3])

def p_rhses_single(p):
	'''rhses : rhs'''
	p[0] = [p[1]]

def p_rhses_multiple(p):
	'''rhses : rhs SPLIT rhses''' 
	p[0] = [p[1].copy()] + p[3]

def p_rhs_term_single(p):
	'''rhs : TERM'''
	for t in p[1]:
		terms.add(t)
	p[0] = []
	p[0].append(p[1])


def p_rhs_term_multiple(p):
	'''rhs : rhs TERM'''
	for t in p[2]:
		terms.add(t)
	p[0] = p[1].copy()
	p[0].append(p[2])


def p_rhs_nonterm_single(p):
    '''rhs : NONTERM'''
    nonterms.add(p[1])
    p[0] = []
    p[0].append(p[1])


def p_rhs_nonterm_multiple(p):
    '''rhs : rhs NONTERM'''
    nonterms.add(p[2])
    p[0] = p[1].copy()
    p[0].append(p[2])

def p_eps(p):
    """rhs : EPS"""
    p[0] = ['']


syntax_error = False


def p_error(p):
	global syntax_error
	syntax_error = True
	print (f'Syntax error at {p}')


def parse(s):
	global syntax_error, terms, nonterms, rules
	syntax_error = False
	terms = set()
	nonterms = set()
	rules = defaultdict(list)
	yacc.parse(s)
	return terms.copy(), nonterms.copy(), rules.copy(), syntax_error



yacc.yacc()
if __name__ == '__main__':
	file = sys.argv[1]
	with open(file, 'r') as f:
		grammar = f.read()
		parse(grammar)
		print('Terms:', ' '.join(terms))
		print('Nonterms:', ' '.join(nonterms))
		print('Rules:')
		for (key, value) in rules.items():
			for rhses in value:
				print(key, '=>', end = ' ')
				rule = []
				for rhs in rhses:
					rule.append(' '.join(rhs))
				print(' | '.join(rule))