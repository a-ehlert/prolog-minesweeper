:- use_module(library(clpfd)), use_module(library(random)), use_module(library(ordsets)).

%Cells is a list of rows of cells
%Valid moves: Reveal,flag,
test(H,W,N,C,M,S):-
	initialize(minesweeper,(H,W,N,C,M,S)).

play(Game) :-
	%initialize(Game,State),
	State = (10,10,5,_Cells,_Mines,_Satisfied),
	initialize(Game,State), 
	display_game(State),
	play(State,_Result).

%play(State,Result) :-
%	game_over(State,Result),
%	!, announce(Result).

play(State,Result) :-
	choose_move(State,Move),
	legal_move(State,Move),
	write('legal move!'), nl,
	move(State,Move),
	display_game(State),
	!, play(State,Result).

display_game((_Height,Width,_NMines,Cells,_Mines,_S)) :-
	display_cells(Cells,Width,0),
	nl.


choose_move(State,Move) :-
	player_move(State,Move).
choose_move(State,Move) :-
	write('ai move.'), nl,
	ai_move(State,Move).
choose_move(_,_) :-
	write('No move.'), nl.

player_move(_State,(Y,X)) :-
	write('enter move:'),
	nl,
	catch(read((Y,X)), _Error, false).

ai_move((_H,_W,_,Cells,_M)) :-
	get_clue(Cells,Clue),
	all_adjacent_unrevealed(Clue,Cells,Adjs),
	fail.


get_clue(Cells,Clue) :-
	member(Clue,Cells),
	revealed(Clue).

	

legal_move((Height,Width,_,_,_),(Y,X)) :-
	\+((Y<0; X<0; Height=<Y; Width=<X)).


move((_Height,_Width,_NMines,Cells,Mines),Move) :-
	write('Time to move...'), nl,
	%trace,
	reveal(Move,Cells,Mines).

%prompt(X) :-
	

game_over(_State,_Result) :-
	fail.

initialize(minesweeper,(Height,Width,NMines,Cells,Mines,Satisfied)) :-
	Height>0, Width>0,
	%assert((invalidcoord(Y,X):- Y<0; X<0; Height=<Y; Width=<X)),
	NCells is Height*Width,
	NMines =< NCells,
	build_cells(0,0,Height,Width,Cells),
	place_mines(NMines,Cells,Mines).

build_cells(Y,X,H,W,[cell((Y,X),_)|Cells]) :-
	Y < H,
	X1 is (X+1) mod W,
	Y1 is Y+((X+1)//W),
	build_cells(Y1,X1,H,W,Cells).
build_cells(H,_,H,_,[]).


place_mines(M,Cells,[Mine|Mines]) :-
	M > 0,!,
	random_select(Mine,Cells,Cells1),
	Mine = cell((_,_),('*',1)),
	M1 is M-1,
	place_mines(M1,Cells1,Mines).
place_mines(0,_,[]).

%If no adjacent, reveal neighbours

reveal((Y,X),Cells,Mines) :-
	%write('reveal 1 called'),nl,
	n_adjacent(cell((Y,X),_),Mines,0),!,
	C = cell((Y,X),(0,0)),
	memberchk(C,Cells),
	all_adjacent_unrevealed(C,Cells,Adj),
	fill_reveal(Adj,Cells,Mines).
reveal((Y,X),Cells,Mines) :-
	%write('reveal 2 called'),nl,
	n_adjacent(cell((Y,X),_),Mines,N),
	memberchk(cell((Y,X),(N,0)),Cells).

fill_reveal([cell((Y,X),_)|Cs],Cells,Mines) :-
	reveal((Y,X),Cells,Mines),
	fill_reveal(Cs,Cells,Mines).
fill_reveal([],_,_).

	

reveal(cell(_Pos,(N,0)),N).
flag(cell(_Pos,('*',1))).
	

unrevealed(cell(_Pos,(Content,_))) :-
	var(Content).
revealed(cell(_Pos,(Content,_))) :-
	integer(Content).

%move((Y,X),(Cells,Mines)) :-
	


adjacent(cell(A,_),cell(B,_)) :-
	adjacent(A,B).

adjacent((Y1,X1),(Y2,X2)) :-
	abs(Y1-Y2) #=< 1,
	abs(X1-X2) #=< 1,
	labeling([],[Y1,X1,Y2,X2]),
	(Y1,X1)\=(Y2,X2).


%adjacent(A,B) :-
%	beside(A,B).
%adjacent(A,B) :-
%	stacked(A,B).
%adjacent(A,B) :-
%	diagonal(A,B).

%coords(cell((Y,X),_),Y,X).

%Adj is a list of all adjacent cells to Pos in Cells.
all_adjacent(Cell,Cells,Adj) :-
	findall(C,(adjacent(Cell,C),memberchk(C,Cells)),Adj).

all_adjacent_unrevealed(Cell,Cells,Adj) :-
	findall(C,(adjacent(Cell,C),memberchk(C,Cells),unrevealed(C)),Adj).

n_adjacent(Cell,Cells,N) :-
	all_adjacent(Cell,Cells,Adj), length(Adj,N).

display_cells([Cell|Cells],Width,N) :-
	N < Width, N1 is N+1,
	display_cell(Cell), display_cells(Cells,Width,N1).
display_cells([Cell|Cells],Width,Width) :-
	nl,
	display_cells([Cell|Cells],Width,0).
display_cells([],_,_).

display_cell(cell(_Pos,(Content,_))) :-
	%memberchk(cell,)
	var(Content),
	format("-",[]).

display_cell(cell(_Pos,(Content,_))) :-
	var(Content),
	format("-",[]).
display_cell(cell(_Pos,(Content,_))) :-
	integer(Content),
	format("~d",[Content]).
display_cell(cell(_Pos,(Content,_))) :-
	nonvar(Content),
	format(Content,[]).