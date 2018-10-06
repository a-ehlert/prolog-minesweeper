:- use_module(library(lists)), use_module(library(random)), use_module(library(ordsets)).

%Cells is a list of rows of cells

play(Game) :-
	initialize(Game,State),
	display_game(State).
	%play(State,Result).

initialize(minesweeper,(Height,Width,Mines,Cells)) :-
	Height>0, Width>0,
	assert((invalidcoord(Y,X):- Y<0; X<0; Height=<Y; Width=<X)),
	NCells is Height*Width,
	Mines =< NCells,
	build_cells(0,0,Height,Width,Cells),
	place_mines(Cells,Mines),
	display_cells(Cells,Width,0).

build_cells(Y,X,H,W,[cell(Y,X,_)|Cells]) :-
	Y < H,
	X1 is (X+1) mod W,
	Y1 is Y+((X+1)//W),
	build_cells(Y1,X1,H,W,Cells).
build_cells(H,_,H,_,[]).


place_mines(Cells,M) :-
	M > 0,
	random_select(C,Cells,Cells1),
	assert(mine(C)),
	C=cell(_,_,*),
	M1 is M-1,
	place_mines(Cells1,M1).
place_mines(_,0).

reveal(cell(Y,X,Content),Cells) :-
	memberchk(cell(Y,X,Content),Cells).


%move(cell()).
%cell(X,Y,_) :- \+ invalidcoord(X,Y).


left_right(cell(X1,_,_),cell(X2,_,_)) :-
	integer(X1), !, X2 is X1+1.
left_right(cell(X1,_,_),cell(X2,_,_)) :-
	integer(X2), !, X1 is X2-1.

up_down(cell(_,Y1,_),cell(_,Y2,_)) :-
	integer(Y1), !, Y2 is Y1+1.
up_down(cell(_,Y1,_),cell(_,Y2,_)) :-
	integer(Y2), !, Y1 is Y2-1.

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

%Adj is a list of all adjacent cells to Cell in Cells.
all_adjacent(Cell,Cells,Adj) :-
	findall(C,(adjacent(Cell,C),memberchk(C,Cells)),Adj).

display_cells([Cell|Cells],Width,N) :-
	N < Width, N1 is N+1,
	display_cell(Cell), display_cells(Cells,Width,N1).
display_cells(Cells,Width,Width) :-
	format("~n",[]),
	display_cells(Cells,Width,0).
display_cells([],_,_).

display_cell(cell(X,Y,Content)) :-
	var(Content),
	format("-",[]).
display_cell(cell(X,Y,Content)) :-
	nonvar(Content),
	format(Content,[]).