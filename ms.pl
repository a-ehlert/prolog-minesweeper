:- use_module(library(clpfd)), use_module(library(random)), use_module(library(ordsets)).

%Cells is a list of rows of cells
%Valid moves: Reveal,flag,
test(H,W,N,C,M):-
	initialize(minesweeper,(H,W,N,C,M)).

play(Game) :-
	%initialize(Game,State),
	State = (10,10,5,_Cells,_Mines),
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
	move(State,Move,State1),
	display_game(State1),
	!, play(State1,Result).

display_game((_Height,Width,_NMines,(A,B,C),_Mines)) :-
	length(A,La),length(B,Lb),length(C,Lc),
	format("Dos: ~d, Cons: ~d, Dones: ~d~n",[La,Lb,Lc]),
	ord_union([A,B,C],Board),
	display_cells(Board,Width,0),
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

ai_move((_H,_W,_,(Dos,_Cons,_Dones),_M),(Y,X)) :-
	trace,
	member(cell((Y,X),N),Dos),		%Generate
	nonvar(N),N=0.					%Test

ai_move((_H,_W,_,(Dos,_Cons,_Dones),_M),flag(Y,X)) :-
	trace,
	member(cell((Y,X),(_,N)),Dos),	%Generate
	nonvar(N), N=1.					%Test


valid_mines(Cells,[Con|Cons]) :-
	all_adjacent(Cells,Con,Ads),
	Con = cell(_,(_,N)),
	sum_mines(Ads,N),
	valid_mines(Cells,Cons).
valid_mines(_,[]).

	


	

legal_move((Height,Width,_,_,_),(Y,X)) :-
	\+((Y<0; X<0; Height=<Y; Width=<X)).


move(State,(Y,X),State1) :-
	write('Time to move...'), nl,
	State = (H,W,N,Cells,Mines),
	State1 = (H,W,N,Cells1,Mines),
	%trace,
	reveal((Y,X),Cells,Mines,Cells1).

%move(State,flag(Y,X),State1) :-
	%State = (H,W,N,Cells,Mines).


%prompt(X) :-
	

game_over(_State,_Result) :-
	fail.

initialize(minesweeper,(Height,Width,NMines,(Dos,[],[]),Mines)) :-
	Height>0, Width>0,
	%assert((invalidcoord(Y,X):- Y<0; X<0; Height=<Y; Width=<X)),
	NCells is Height*Width,
	NMines =< NCells,
	build_cells(0,0,Height,Width,Dos),
	place_mines(NMines,Dos,Mines).

build_cells(Y,X,H,W,[cell((Y,X),(_,Mine))|Cells]) :-
	Mine in 0..1,
	Y < H,
	X1 is (X+1) mod W,
	Y1 is Y+((X+1)//W),
	build_cells(Y1,X1,H,W,Cells).
build_cells(H,_,H,_,[]).


place_mines(M,Cells,[cell(Pos,('*',1))|Mines]) :-
	M > 0,!,
	random_select(cell(Pos,_),Cells,Cells1),
	%Mine = cell((_,_),(_,1)),
	M1 is M-1,
	place_mines(M1,Cells1,Mines).
place_mines(0,_,[]).

%If no adjacent, reveal neighbours
%To fucking do: redo all the reveal
%Outline > Reveal current tile, reveal neighbour tile recursively until no unrevealed neighbours available.
%Maybe have a first function that checks how many nearby mines and then two variant reveal functions for either 0 or else
%
%build a list recursively then move them after the initial call returns
reveal((Y,X),Cells,Mines,Cells1) :-
	C = cell((Y,X),(N,0)),
	n_adjacent(C,Mines,N),
	reveal(C,N,Cells,Mines,Cells1).

reveal(C,Cells,Mines,Cells1) :-
	C = cell(_,(N,0)),
	n_adjacent(C,Mines,N),
	reveal(C,N,Cells,Mines,Cells1).

reveal(C,0,Cells,Mines,Cells2) :-
	%write('reveal 2 called'),nl,
	Cells = (Dos,Cons,Dones),
	memberchk(C,Dos),
	ord_del_element(Dos,C,Dos1),
	ord_add_element(Dones,C,Dones1),
	Cells1 = (Dos1,Cons,Dones1),
	reveal_neighbours(C,Cells1,Mines,Cells2).

reveal(C,N,Cells,_Mines,Cells1) :-
	%write('reveal 3 called'),nl,
	N > 0,
	Cells = (Dos,Cons,Dones),
	memberchk(C,Dos),
	ord_del_element(Dos,C,Dos1),
	ord_add_element(Cons,C,Cons1),
	all_adjacent(C,Dos1,Ads),
	%trace,
	sum_mines(Ads,N),
	write(Ads),
	memberchks(Ads,Dos1),
	Cells1 = (Dos1,Cons1,Dones).

reveal_neighbours(C,Cells,Mines,Cells2):-
	Cells = (Dos,_Cons,_Dones),
	get_adjacent(C,Dos,Ad),
	n_adjacent(Ad,Mines,M),
	Ad = cell(_,(M,0)),
	reveal(Ad,M,Cells,Mines,Cells1),
	reveal_neighbours(C,Cells1,Mines,Cells2).

reveal_neighbours(C,(Dos,Cons,Dones),_M,(Dos,Cons,Dones)):-
	n_adjacent(C,Dos,0).



%reveal((Y,X),(Dos,Cons,Dones),Mines,Cells2) :-
%	n_adjacent(cell((Y,X),_),Mines,0),!,
%	%Cells = (Dos,Cons,Dones),
%	Cells1 = (Dos2,Cons,Dones1),
%	%Cells2 = (Dos2,Cons2,Dones2),
%	write('reveal 1 called'),nl,
%	C = cell((Y,X),(0,0)),
%	memberchk(C,Dos),
%	ord_del_element(Dos,C,Dos1),
%	ord_add_element(Dones,C,Dones1),
%	all_adjacent(C,Dos1,Adj),
%	%ord_subtract(Dos1,Adj,Dos2),

%	fill_reveal(Adj,Cells1,Mines,Cells2).

%reveal((Y,X),Cells,Mines,Cells1) :-
%	Cells = (Dos,Cons,Dones),
%	Cells1 = (Dos1,Cons1,Dones),
%	write('reveal 2 called'),nl,
%	C = cell((Y,X),(N,0)),
%	n_adjacent(C,Mines,N),
%	memberchk(C,Dos),
%	ord_del_element(Dos,C,Dos1),
%	ord_add_element(Cons,C,Cons1).

%fill_reveal([cell((Y,X),_)|Cs],Cells,Mines,Cells2) :-
%	reveal((Y,X),Cells,Mines,Cells1),
%	fill_reveal(Cs,Cells1,Mines,Cells2).
%fill_reveal([],Cs,_,Cs).


	

%reveal(cell(_Pos,(N,0)),N).
%flag(cell(_Pos,('*',1))).

%flag(Pos,Cells,Mines,Cells1) :-
%	memberchk(cell(Pos,_),Mines),

	

unrevealed(cell(_Pos,(Content,_))) :-
	var(Content).
revealed(cell(_Pos,(Content,_))) :-
	integer(Content).

%mine(cell(_,(_,X))) :-
%	nonvar(X), X=1.
%nomine(cell(_,(_,X))) :-
%	nonvar(X), X=0.

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
memberchks([M|Ms],Xs) :-
	memberchk(M,Xs),
	memberchks(Ms,Xs).
memberchks([],_).

all_adjacent(Cell,Cells,Ads) :-
	findall(C,(member(C,Cells),adjacent(Cell,C)),Ads).

get_adjacent(C,Cells,Ad) :-
	member(Ad,Cells), adjacent(C,Ad).


sum_mines([cell(_,(_,Mine))|Cs],Sum) :-
	Sum #= Sum1 + Mine,
	sum_mines(Cs,Sum1).
sum_mines([],0).

n_adjacent(Cell,Cells,N) :-
	all_adjacent(Cell,Cells,Adj), length(Adj,N).

display_cells([Cell|Cells],Width,N) :-
	N < Width, N1 is N+1,
	display_cell(Cell), display_cells(Cells,Width,N1).
display_cells([Cell|Cells],Width,Width) :-
	%format("|~n|",[]),
	nl,
	display_cells([Cell|Cells],Width,0).
display_cells([],_W,_).
	%format("~*_",[W]).

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