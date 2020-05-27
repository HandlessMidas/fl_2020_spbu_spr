import sys
import re
from new_parser import parse_file

def insert_tabs(string, index, tabs):
	for i in range(tabs):
		string = string[:index] + '_' + string[index:] 
	return string


def generate_tree(relations, goal, atoms):
	tree = 'program \n['
	for key in reversed(list(relations.keys())):
		tree += '\n:-\n(\n'
		tree += key
		if (relations[key] != ''):
			tree += relations[key][0]
		else:
			tree += '()'
		tree += '\n)'
	tree +='\n' + goal
	tree += '\n]'
	tab = 0
	i = 0
	while i < len(tree):
		if tree[i] == '(' and tree[i - 1] == ' ':
			tree = tree[:i - 1] + '\n' + tree[i:]
			i += 1
		elif tree[i] == '(' and tree[i - 1] == ')':
			tree = tree[:i] + '\n' + tree[i:]
			i += 2
		else:
			i += 1 
	i = 0
	while i < len(tree):
		if tree[i] == '(':
			tab += 2
			i += 1
		elif tree[i] == ')':
			tab -= 2
			i += 1
		elif tree[i] == '\n':
			if tree[i + 1] == ')':
				tab -= 2
				tree = insert_tabs(tree, i + 1, tab)
				i += tab + 1
				tab += 2
			else:
				tree = insert_tabs(tree, i + 1, tab)
				i += tab + 1
		else:
			i += 1
	return tree


if __name__ == '__main__':
	relations, goal, atoms = parse_file(sys.argv[1])
	with open('tree.txt', 'w') as file:
		file.write(generate_tree(relations, goal, atoms))