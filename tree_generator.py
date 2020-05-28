import sys
import re
from new_parser import parse_file

def generate_tree(file):
	relations, goal, atoms, varrs, syntax_error = parse_file(file)
	tree = ''
	for key in reversed(list(relations.keys())):
		tree += key
		if (relations[key][0] != ''):
			tree += ' :- '
			tree += relations[key][0] + '.'
		else:
			tree += '.'
		tree += '\n'
	tree +='?- ' + goal + '.'
	return tree


if __name__ == '__main__':
	with open('tree.txt', 'w') as file:
		file.write(generate_tree(sys.argv[1]))