
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'COMMA GOAL_SPLIT ID LEFT_BRACKET RELATION_SPLIT RIGHT_BRACKET STOP VARprogram : relation goalprogram : goalrelation : atom RELATION_SPLIT bodyrelation : atom RELATION_SPLIT body relationrelation : atom STOPrelation : atom STOP relationgoal : GOAL_SPLIT bodyatom : ID LEFT_BRACKET args RIGHT_BRACKETatom : IDargs : VARargs : VAR COMMA argsargs : atomargs : atom COMMA argsbody : atom STOPbody : atom COMMA body'
    
_lr_action_items = {'GOAL_SPLIT':([0,2,9,13,14,15,20,21,],[5,5,-5,-3,-6,-14,-4,-15,]),'ID':([0,5,8,9,12,13,15,16,21,23,24,],[6,6,6,6,6,6,-14,6,-15,6,6,]),'$end':([1,3,7,10,15,21,],[0,-2,-1,-7,-14,-15,]),'RELATION_SPLIT':([4,6,22,],[8,-9,-8,]),'STOP':([4,6,11,22,],[9,-9,15,-8,]),'LEFT_BRACKET':([6,],[12,]),'COMMA':([6,11,18,19,22,],[-9,16,23,24,-8,]),'RIGHT_BRACKET':([6,17,18,19,22,25,26,],[-9,22,-10,-12,-8,-11,-13,]),'VAR':([12,23,24,],[18,18,18,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'program':([0,],[1,]),'relation':([0,9,13,],[2,14,20,]),'goal':([0,2,],[3,7,]),'atom':([0,5,8,9,12,13,16,23,24,],[4,11,11,4,19,4,11,19,19,]),'body':([5,8,16,],[10,13,21,]),'args':([12,23,24,],[17,25,26,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> program","S'",1,None,None,None),
  ('program -> relation goal','program',2,'p_program','new_parser.py',46),
  ('program -> goal','program',1,'p_program_no_relation','new_parser.py',52),
  ('relation -> atom RELATION_SPLIT body','relation',3,'p_relation_single','new_parser.py',58),
  ('relation -> atom RELATION_SPLIT body relation','relation',4,'p_relation_multiple','new_parser.py',63),
  ('relation -> atom STOP','relation',2,'p_relation_empty','new_parser.py',68),
  ('relation -> atom STOP relation','relation',3,'p_relation_empty_multiple','new_parser.py',74),
  ('goal -> GOAL_SPLIT body','goal',2,'p_goal','new_parser.py',79),
  ('atom -> ID LEFT_BRACKET args RIGHT_BRACKET','atom',4,'p_atom','new_parser.py',84),
  ('atom -> ID','atom',1,'p_atom_empty','new_parser.py',90),
  ('args -> VAR','args',1,'p_args_var_single','new_parser.py',96),
  ('args -> VAR COMMA args','args',3,'p_args_var_multiple','new_parser.py',103),
  ('args -> atom','args',1,'p_args_atom_single','new_parser.py',110),
  ('args -> atom COMMA args','args',3,'p_args_atom_multiple','new_parser.py',116),
  ('body -> atom STOP','body',2,'p_body_single','new_parser.py',123),
  ('body -> atom COMMA body','body',3,'p_body_multiple','new_parser.py',129),
]
