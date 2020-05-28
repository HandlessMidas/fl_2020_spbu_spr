
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'COMMA GOAL_SPLIT ID LEFT_BRACKET RELATION_SPLIT RIGHT_BRACKET STOP VARprogram : relation goalrelation : atom RELATION_SPLIT bodyrelation : atom RELATION_SPLIT body relationrelation : atom STOPrelation : atom STOP relationgoal : GOAL_SPLIT bodyatom : ID LEFT_BRACKET args RIGHT_BRACKETatom : IDargs : VARargs : VAR COMMA argsargs : atomargs : atom COMMA argsbody : atom STOPbody : atom COMMA body'
    
_lr_action_items = {'ID':([0,6,7,8,9,12,17,18,21,22,23,],[4,4,4,4,4,4,-13,4,4,4,-14,]),'$end':([1,5,10,17,23,],[0,-1,-6,-13,-14,]),'GOAL_SPLIT':([2,8,12,13,17,19,23,],[6,-4,-2,-5,-13,-3,-14,]),'RELATION_SPLIT':([3,4,20,],[7,-8,-7,]),'STOP':([3,4,11,20,],[8,-8,17,-7,]),'LEFT_BRACKET':([4,],[9,]),'COMMA':([4,11,15,16,20,],[-8,18,21,22,-7,]),'RIGHT_BRACKET':([4,14,15,16,20,24,25,],[-8,20,-9,-11,-7,-10,-12,]),'VAR':([9,21,22,],[15,15,15,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'program':([0,],[1,]),'relation':([0,8,12,],[2,13,19,]),'atom':([0,6,7,8,9,12,18,21,22,],[3,11,11,3,16,3,11,16,16,]),'goal':([2,],[5,]),'body':([6,7,18,],[10,12,23,]),'args':([9,21,22,],[14,24,25,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> program","S'",1,None,None,None),
  ('program -> relation goal','program',2,'p_program','new_parser.py',46),
  ('relation -> atom RELATION_SPLIT body','relation',3,'p_relation_single','new_parser.py',52),
  ('relation -> atom RELATION_SPLIT body relation','relation',4,'p_relation_multiple','new_parser.py',57),
  ('relation -> atom STOP','relation',2,'p_relation_empty','new_parser.py',62),
  ('relation -> atom STOP relation','relation',3,'p_relation_empty_multiple','new_parser.py',67),
  ('goal -> GOAL_SPLIT body','goal',2,'p_goal','new_parser.py',72),
  ('atom -> ID LEFT_BRACKET args RIGHT_BRACKET','atom',4,'p_atom','new_parser.py',77),
  ('atom -> ID','atom',1,'p_atom_empty','new_parser.py',83),
  ('args -> VAR','args',1,'p_args_var_single','new_parser.py',89),
  ('args -> VAR COMMA args','args',3,'p_args_var_multiple','new_parser.py',96),
  ('args -> atom','args',1,'p_args_atom_single','new_parser.py',102),
  ('args -> atom COMMA args','args',3,'p_args_atom_multiple','new_parser.py',108),
  ('body -> atom STOP','body',2,'p_body_single','new_parser.py',115),
  ('body -> atom COMMA body','body',3,'p_body_multiple','new_parser.py',120),
]
