:- use_module(library(clpfd)), use_module(library(random)), use_module(library(ordsets)), use_module(library(lists)).



%Cells is a list of rows of cells
%Valid moves: Reveal,flag,
test(H,W,N,C,M):-
	initialize(minesweeper,(H,W,N,C,M)).

%startprompt(Height,Width,N).


play :-
	%initialize(Game,State),
	State = (5,5,5,_Cells,_Mines),
	initialize(State), 
	display_game(State),
	play(State,_Result).

play(State,Result) :-
	game_over(State,Result),
	!, announce(Result).

play(State,Result) :-
	choose_move(State,Move),
	legal_move(State,Move),
	%write('legal move!'), nl,
	move(State,Move,State1),
	display_game(State1),
	!, play(State1,Result).

game_over((_,_,0,([],[],_),_),'You win!').

game_over((_,_,N,([],[],_),_),'You lose!') :-
	N > 0.

announce(Result) :-
	write(Result), nl.
	

display_game((_Height,Width,_NMines,(A,B,C),_Mines)) :-
	length(A,La),length(B,Lb),length(C,Lc),
	format("Dos: ~d, Cons: ~d, Dones: ~d~n",[La,Lb,Lc]),
	ord_union([A,B,C],Board),
	display_cells(Board,Width,0),
	nl.

initialize((Height,Width,NMines,(Dos,[],[]),Mines)) :-
	Height>0, Width>0,
	%assert((invalidcoord(Y,X):- Y<0; X<0; Height=<Y; Width=<X)),
	NCells is Height*Width,
	NMines =< NCells,
	build_cells(0,0,Height,Width,Dos),
	place_mines(NMines,Dos,Mines).

build_cells(Y,X,H,W,[cell((Y,X),(_,_))|Cells]) :-
	%Mine in 0..1,
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


choose_move(State,Move) :-
	player_move(State,Move).
choose_move(State,Move) :-
	write('ai move.'), nl,
	ai_evaluate_constraints(State),
	ai_move(State,Move).
choose_move(_,nomove) :-
	write('No move.'), nl.



player_move(_State,Move) :-
	write('enter move:'),
	nl,
	catch(read(Move), _Error, false).


ai_move((_H,_W,_,(Dos,_Cons,_Dones),_M),flag(Y,X)) :-
	%trace,
	member(cell((Y,X),(Flag,N)),Dos),	%Generate
	N==1, var(Flag).				%Test

ai_move((_H,_W,_,(Dos,_Cons,_Dones),_M),(Y,X)) :-
	%trace,
	member(cell((Y,X),(_,N)),Dos),		%Generate
	N==0.					%Test

ai_move((_H,_W,_,(Dos,_Cons,_Dones),_M),Pos) :-
	write('Guess: '),nl,
	%trace,
	getvars(Dos,Vars),
	findall(Vars,(labeling([],Vars)),L),
	transpose(L,L1),
	sumlists(L1,Sumlist),
	min_member(Min,Sumlist),
	nth0(Index,Sumlist,Min),
	nth0(Index,Dos,cell(Pos,_)),
	write(Sumlist),nl,
	write(Pos),nl.

sumlists([L|Ls],[Sum|Sums]) :-
	sumlist(L,Sum),
	sumlists(Ls,Sums).
sumlists([],[]).



ai_evaluate_constraints((_H,_W,N,(Dos,Cons,_Dones),_M)) :-
	valid_mines(Dos,Cons,N).

getvars([cell(_,(_,Var))|Cells],[Var|Vars]) :-
	getvars(Cells,Vars).
getvars([],[]).

%ai_move().




valid_mines(Cells,[Con|Cons],N) :-
	all_adjacent(Con,Cells,Ads),
	Con = cell(_,(NAdj,_)),
	sum_mines(Ads,NAdj),
	memberchks(Ads,Cells),
	valid_mines(Cells,Cons,N).
valid_mines(Cells,[],N) :-
	sum_mines(Cells,N).

	


legal_move(State,flag(Y,X)) :-
	legal_move(State,(Y,X)).

legal_move((Height,Width,_,_,_),(Y,X)) :-
	\+((Y<0; X<0; Height=<Y; Width=<X)).

legal_move(_,nomove).

%If attempting to reveal a mine, end game by moving all cells to Dones
move(State,(Y,X),State1) :-
	State = (H,W,N,(Dos,Cons,Dones),Mines),
	memberchk(cell((Y,X),_),Mines),
	write('Uh-oh! Revealed a mine!'), nl,
	%trace,
	reveal_mine((Y,X),Dos),
	ord_union(Dos,Cons,Cs),
	ord_union(Cs,Dones,Dones1),
	State1 = (H,W,N,([],[],Dones1),Mines).

%Reveal safe cell
move(State,(Y,X),State1) :-
	State = (H,W,N,Cells,Mines),
	\+(memberchk(cell((Y,X),_),Mines)),
	write('Revealing: '), write((Y,X)), nl,
	%trace,
	reveal((Y,X),Cells,Mines,Cells1),
	prune_done(Cells1,Mines,Cells2),
	State1 = (H,W,N,Cells2,Mines).

move(State,flag(Y,X),State1) :-
	State = (H,W,N,(Dos,Cons,Dones),Mines),
	\+memberchk(cell((Y,X),_),Mines),
	flag((Y,X),(Dos,Cons,Dones)),
	write('Whoops! Flagged a safe tile! at '), write((Y,X)), nl,
	%trace,
	ord_union(Dos,Cons,Cs),
	ord_union(Cs,Dones,Dones1),
	State1 = (H,W,N,([],[],Dones1),Mines).


%Flag cell
move(State,flag(Y,X),State1) :-
	State = (H,W,N,Cells,Mines),
	%N1 is N - 1,
	memberchk(cell((Y,X),_),Mines),
	write('Flagging: '), write((Y,X)), nl,
	flag((Y,X),Cells),
	%trace,
	prune_done(Cells,Mines,Cells1),
	State1 = (H,W,N,Cells1,Mines).
	


move(S,nomove,S).


%prompt(X) :-
	




flag((Y,X),(Dos,_,_)) :-
	C = cell((Y,X),_),
	flagged(C),
	memberchk(C,Dos).

%prune_done(Cells,[cell((Y,X),_)|Mines],Cells2) :-
%	%trace,
%	Cells = (Dos,Cons,Dones),
%	C = cell((Y,X),_),
%	memberchk(C,Dos),
%	all_adjacent(cell((Y,X),_),Cons,Ads),
%	cons_satisfied(Ads,Dos),
%	%trace,
%	%trace,
%	ord_del_element(Dos,C,Dos1),
%	write('deleted: '), write(C), nl,
%	ord_add_element(Dones,C,Dones1),
%	ord_subtract(Cons,Ads,Cons1),
%	ord_union(Dones1,Ads,Dones2),
%	Cells1 = (Dos1,Cons1,Dones2),
%	prune_done(Cells1,Mines,Cells2).
%prune_done(Cells,[_|Mines],Cells1) :-
	%prune_done(Cells,Mines,Cells1).
prune_done(Cells,_,Cells).

%prune_satisfied(a).


%If no adjacent, reveal neighbours
%To do: redo all the reveal
%Outline > Reveal current tile, reveal neighbour tile recursively until no unrevealed neighbours available.
%Maybe have a first function that checks how many nearby mines and then two variant reveal functions for either 0 or else
%
%build a list recursively then move them after the initial call returns
reveal_mine((Y,X),Dos) :-
	memberchk(cell((Y,X),('*',1)),Dos).

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

adjacent_flags(Cell,Cells,N) :-
	findall(C,(member(C,Cells),adjacent(Cell,C),is_flagged(C)),Ads),
	length(Ads,N).

constraint(cell(_,(N,0)),N).

%Constraints have all surrounding mines in Dos flagged
cons_satisfied([C|Cs],Dos) :-
	constraint(C,N),
	adjacent_flags(C,Dos,N),
	cons_satisfied(Cs,Dos).
cons_satisfied([],_).



flagged(cell(_,('F',1))).
is_flagged(cell(_,Content)) :- Content == ('F',1).
	

sum_mines([cell(_,(_,Mine))|Cs],Sum) :-
	Mine in 0..1,
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
	var(Content),
	format('-',[]).
display_cell(cell(_Pos,(Content,_))) :-
	integer(Content),
	Content \= 0,
	format("~d",[Content]).
display_cell(cell(_Pos,(0,_))) :-
	format('.',[]).
display_cell(cell(_Pos,(Content,_))) :-
	nonvar(Content),
	format(Content,[]).