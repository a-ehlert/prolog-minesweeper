:- use_module(library(lists)), use_module(library(random)), use_module(library(ordsets)).

%Cells is a list of rows of cells
%Valid moves: Reveal,flag,

play(Game) :-
	%initialize(Game,State),
	State = (10,10,15,_Cells,_Mines),
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

display_game((_Height,Width,_NMines,Cells,_Mines)) :-
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

ai_move(_State,(_Y,_X)) :-
	false.

legal_move((Height,Width,_,_,_),(Y,X)) :-
	\+((Y<0; X<0; Height=<Y; Width=<X)).


move((_Height,_Width,_NMines,Cells,Mines),Move) :-
	write('Time to move...'), nl,
	reveal(Move,Cells,Mines).

%prompt(X) :-
	

game_over(_State,_Result) :-
	fail.

initialize(minesweeper,(Height,Width,NMines,Cells,Mines)) :-
	Height>0, Width>0,
	%assert((invalidcoord(Y,X):- Y<0; X<0; Height=<Y; Width=<X)),
	NCells is Height*Width,
	NMines =< NCells,
	build_cells(0,0,Height,Width,Cells),
	place_mines(NMines,Cells,Mines).

build_cells(Y,X,H,W,[cell(Y,X,_)|Cells]) :-
	Y < H,
	X1 is (X+1) mod W,
	Y1 is Y+((X+1)//W),
	build_cells(Y1,X1,H,W,Cells).
build_cells(H,_,H,_,[]).


place_mines(M,Cells,[Mine|Mines]) :-
	M > 0,!,
	random_select(Mine,Cells,Cells1),
	Mine = cell(_,_,*),
	M1 is M-1,
	place_mines(M1,Cells1,Mines).
place_mines(0,_,[]).

%If no adjacent, reveal neighbours
reveal((Y,X),Cells,Mines) :-
	%write('reveal 1 called'),nl,
	n_adjacent(cell(Y,X,_),Mines,0),!,
	memberchk(cell(Y,X,' '),Cells),
	all_adjacent_unrevealed(cell(Y,X,_),Cells,Adj),
	fill_reveal(Adj,Cells,Mines).
reveal((Y,X),Cells,Mines) :-
	%write('reveal 2 called'),nl,
	n_adjacent(cell(Y,X,_),Mines,N),
	memberchk(cell(Y,X,N),Cells).

fill_reveal([cell(Y,X,_)|Cs],Cells,Mines) :-
	reveal((Y,X),Cells,Mines),
	fill_reveal(Cs,Cells,Mines).
fill_reveal([],_,_).

unrevealed(cell(_Y,_X,Content)) :-
	var(Content).

%move((Y,X),(Cells,Mines)) :-
	


	



%move(cell()).
%cell(X,Y,_) :- \+ invalidcoord(X,Y).


up_down(cell(Y1,_,_),cell(Y2,_,_)) :-
	integer(Y1), !, Y2 is Y1+1.
up_down(cell(Y1,_,_),cell(Y2,_,_)) :-
	integer(Y2), !, Y1 is Y2-1.

left_right(cell(_,X1,_),cell(_,X2,_)) :-
	integer(X1), !, X2 is X1+1.
left_right(cell(_,X1,_),cell(_,X2,_)) :-
	integer(X2), !, X1 is X2-1.

left_or_right(A,B) :- left_right(A,B).
left_or_right(A,B) :- left_right(B,A).

up_or_down(A,B) :- up_down(A,B).
up_or_down(A,B) :- up_down(B,A).

same_col(cell(_,X,_),cell(_,X,_)).
same_row(cell(Y,_,_),cell(Y,_,_)).
	
beside(A,B) :-
	same_row(A,B),left_or_right(A,B).

stacked(A,B) :-
	same_col(A,B),up_or_down(A,B).

diagonal(A,B) :-
	left_or_right(A,B),up_or_down(A,B).

adjacent(A,B) :-
	beside(A,B).
adjacent(A,B) :-
	stacked(A,B).
adjacent(A,B) :-
	diagonal(A,B).

coords(cell(Y,X,_),Y,X).

%Adj is a list of all adjacent cells to Cell in Cells.
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

display_cell(cell(_Y,_X,Content)) :-
	var(Content),
	format("-",[]).
display_cell(cell(_Y,_X,Content)) :-
	integer(Content),
	format("~d",[Content]).
display_cell(cell(_Y,_X,Content)) :-
	nonvar(Content),
	format(Content,[]).