import ply.lex as lex
import ply.yacc as yacc
from collections import defaultdict
import sys


tokens = (
	'TERM',
	'NONTERM',
	'ARROW',
	'SPLIT',
	'NEWLINE'
)


t_TERM = r'[a-z]+'
t_NONTERM = r'[A-Z]+'
t_ARROW = r'=>'
t_SPLIT = r'\|'


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
	'''rule : NONTERM ARROW rhs'''
	nonterms.add(p[1])
	rules[p[1]].append(p[3])


def p_rule_single_line(p):
	'''rule : NONTERM ARROW rhs NEWLINE'''
	nonterms.add(p[1])
	rules[p[1]].append(p[3])

def p_rule_multiple(p):
	'''rule : NONTERM ARROW rhs NEWLINE rule'''
	nonterms.add(p[1])
	rules[p[1]].append(p[3])


def p_rhs_term_single(p):
	'''rhs : TERM'''
	for t in p[1]:
		terms.add(t)
	p[0] = []
	p[0].append(p[1])


def p_rhs_term_multiple(p):
	'''rhs : rhs SPLIT TERM'''
	for t in p[3]:
		terms.add(t)
	p[0] = p[1].copy()
	p[0].append(p[3])


def p_rhs_nonterm_single(p):
    '''rhs : NONTERM'''
    nonterms.add(p[1])
    p[0] = []
    p[0].append(p[1])


def p_rhs_nonterm_multiple(p):
    '''rhs : rhs SPLIT NONTERM'''
    nonterms.add(p[3])
    p[0] = p[1].copy()
    p[0].append(p[3])


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
			for rhs in value:
				print(key, '=>', ' | '.join(rhs))