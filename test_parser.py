import pytest

from parser import parse


def test_single_rule_1():
    s = '''S => a
        '''
    terms, nonterms, rules, syntax_error = parse(s)
    assert not syntax_error
    assert terms == {'a'}
    assert nonterms == {'S'}
    assert rules == {'S': [['a']]}


def test_single_rule_2():
    s = '''S => A 
        '''
    terms, nonterms, rules, syntax_error = parse(s)
    assert not syntax_error
    assert terms == set()
    assert nonterms == {'S', 'A'}
    assert rules == {'S': [['A']]}


def test_single_rule_3():
    s = '''S => b | a | A 
        '''
    terms, nonterms, rules, syntax_error = parse(s)
    assert not syntax_error
    assert terms == {'b', 'a'}
    assert nonterms == {'S', 'A'}
    assert rules == {'S': [['b', 'a', 'A']]}


def test_multiple_rules():
    s = '''S => A | B
        A => A | a
        B => B | b
        '''
    terms, nonterms, rules, syntax_error = parse(s)
    assert not syntax_error
    assert terms == {'a', 'b'}
    assert nonterms == {'S', 'A', 'B'}
    assert rules == {'S': [['A', 'B']], 'A': [['A', 'a']], 'B': [['B', 'b']]}


def test_syntax_error():
    s = '''S => A | B
        A => A | a
        B => B a
        '''
    terms, nonterms, rules, syntax_error = parse(s)
    assert syntax_error


if __name__ == '__main__':
    pytest.main()

