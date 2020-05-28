import sys
import re
from new_parser import parse_file

def generate_tree(file):
	relations, goal, atoms, varrs, syntax_error = parse_file(file)
	tree = ''
	for rel in reversed(relations):
		tree += rel[0]
		if (rel[1] != ''):
			tree += ' :- '
			tree += rel[1] + '.'
		else:
			tree += '.'
		tree += '\n'
	tree +='?- ' + goal + '.'
	return tree


if __name__ == '__main__':
	with open('tree.txt', 'w') as file:
		relations, goal, atoms, varrs, syntax_error = parse_file(sys.argv[1])
		file.write(str(relations))
		file.write(generate_tree(sys.argv[1]))