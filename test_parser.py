import pytest

from new_parser import parse, parse_file


def test_1():
	relations, goal, atoms, varrs, syntax_error = parse_file('test1.txt')
	assert not syntax_error
	assert relations == {'and(X, Y, R)': ['nand(X, Y, Xy), nand(Xy, Xy, R)'], 'or(X, Y, R)': ['nand(X, X, Xx), nand(Y, Y, Yy), nand(Xx, Yy, R)'], 'neg(X, R)': ['nand(X, X, R)'], 'nand': [''], 'nand(true, true, false)': [''], 'nand(true, false, true)': [''], 'nand(false, true, true)': [''], 'nand(false, false, true)': [''], 'elem(succ(N), cons(H, T), V)': ['elem(N, T, V)'], 'elem(zero, cons(H, T), H)': [''], 'eval(St, not(X), U)': ['eval(St, X, V), neg(U, V)'], 'eval(St, disj(X, Y), U)': ['eval(St, X, V), eval(St, Y, W), or(V, W, U)'], 'eval(St, conj(X, Y), U)': ['eval(St, X, V), eval(St, Y, W), and(V, W, U)'], 'eval(St, var(X), U)': ['elem(X, St, U)']}
	assert goal == 'eval(St, conj(disj(X, Y), not(var(Z))), true), eval'
	assert atoms == {'conj(X, Y)', 'and(X, Y, R)', 'nand(Xy, Xy, R)', 'eval', 'nand(Xx, Yy, R)', 'nand(X, Y, Xy)', 'nand(true, false, true)', 'conj(disj(X, Y), not(var(Z)))', 'not(var(Z))', 'eval(St, not(X), U)', 'nand(Y, Y, Yy)', 'cons(H, T)', 'elem(N, T, V)', 'not(X)', 'var(X)', 'eval(St, conj(X, Y), U)', 'and(V, W, U)', 'elem(X, St, U)', 'neg(X, R)', 'elem(succ(N), cons(H, T), V)', 'eval(St, X, V)', 'nand(X, X, Xx)', 'eval(St, conj(disj(X, Y), not(var(Z))), true)', 'true', 'succ(N)', 'nand(false, true, true)', 'zero', 'var(Z)', 'eval(St, var(X), U)', 'nand(X, X, R)', 'nand(false, false, true)', 'nand(true, true, false)', 'eval(St, Y, W)', 'or(X, Y, R)', 'false', 'nand', 'neg(U, V)', 'elem(zero, cons(H, T), H)', 'or(V, W, U)', 'disj(X, Y)', 'eval(St, disj(X, Y), U)'}

def test_2():
	relations, goal, atoms, varrs, syntax_error = parse_file('test2.txt')
	assert not syntax_error
	assert relations == {'length(cons(X, Xs), succ(N))': ['length(Xs, N)'], 'length(nil, zero)': [''], 'concat(cons(X, Xs), Ys, cons(Z, Zs))': ['eq(X, Z), concat(Xs, Ys, Zs)'], 'concat(nil, Ys, Zs)': ['eq(Ys, Zs)'], 'eq(cons(X, Xs), cons(Y, Ys))': ['eq(X, Y), eq(Xs, Ys)'], 'eq(nil, nil)': ['']}
	assert goal == 'concat(cons(one, cons(two, nil)), const(three, cons(four, nil)), R)'
	assert atoms == {'three', 'two', 'four', 'concat(nil, Ys, Zs)', 'const(three, cons(four, nil))', 'nil', 'cons(Z, Zs)', 'cons(Y, Ys)', 'length(nil, zero)', 'one', 'concat(cons(one, cons(two, nil)), const(three, cons(four, nil)), R)', 'zero', 'cons(one, cons(two, nil))', 'eq(Xs, Ys)', 'concat(Xs, Ys, Zs)', 'eq(Ys, Zs)', 'cons(two, nil)', 'cons(four, nil)', 'eq(X, Y)', 'eq(cons(X, Xs), cons(Y, Ys))', 'cons(X, Xs)', 'concat(cons(X, Xs), Ys, cons(Z, Zs))', 'eq(nil, nil)', 'length(cons(X, Xs), succ(N))', 'length(Xs, N)', 'eq(X, Z)', 'succ(N)'}

def test_fail_1():
	relations, goal, atoms, varrs, syntax_error = parse('fail')
	assert syntax_error