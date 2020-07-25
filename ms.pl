:- use_module(library(clpfd)), use_module(library(random)), use_module(library(ordsets)), use_module(library(lists)).

%Anton Ehlert

%Cells is a list of rows of cells
%Valid moves: Reveal,flag,
test(H,W,N,C,M):-
	initialize(minesweeper,(H,W,N,C,M)).

%startprompt(Height,Width,N).

play(mini) :-
	play(5,6,8).
play(easy) :-
	play(8,8,10).
play(normal) :-
	play(16,16,40).
play(hard) :-
	play(16,40,99).

play(H,W,N) :-
	%initialize(Game,State),
	State = (H,W,N,_Cells,_Mines),
	initialize(State,State1), 
	display_game(State1),
	play(State1,_Result).

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
	%length(A,La),length(B,Lb),length(C,Lc),
	%format("Dos: ~d, Cons: ~d, Dones: ~d~n",[La,Lb,Lc]),
	ord_union([A,B,C],Board),
	display_cells(Board,Width,0),
	nl.

initial_display(Width,Cells) :-
	format("How to play: Enter a position 'Y,X.' (without quotes) to reveal row Y column X, where 0,0. is the top left corner. To place a flag, enter 'flag(Y,X).' Revealing a mine or flagging a safe tile will end the game.",[]),nl,
	format("Enter '.' or any invalid move to let the AI select a guaranteed safe move. If fewer than 25 tiles and 10 mines are in play, the AI may use brute force.",[]),nl,
	display_cells(Cells,Width,0),
	nl.

initialize((Height,Width,NMines,(Dos,[],[]),Mines),State1) :-
	Height>0, Width>0,
	%assert((invalidcoord(Y,X):- Y<0; X<0; Height=<Y; Width=<X)),
	NCells is Height*Width,
	NMines < NCells,
	build_cells(0,0,Height,Width,Dos),
	initial_display(Width,Dos),
	choose_move((Height,Width,NMines,_,_),Startmove),
	legal_move((Height,Width,NMines,_,_),Startmove),
	move_coords(Startmove,Startpos),
	delete(Dos,cell(Startpos,_),Dos1),
	place_mines(NMines,Dos1,Mines),
	State = (Height,Width,NMines,(Dos,[],[]),Mines),
	move(State,Startmove,State1).

move_coords((Y,X),(Y,X)).
move_coords(flag(Y,X),(Y,X)).

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
	write('AI moving.'), nl,
	ai_evaluate_constraints(State),
	ai_move(State,Move).
choose_move(_,nomove) :-
	write('No safe move found.'), nl.



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

ai_move((_H,_W,N,(Dos,_Cons,_Dones),_M),Pos) :-
	length(Dos,M),
	M =< 25, N =< 10,
	write('Brute force: '),nl,
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

move(S,nomove,S).

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
move(State,(Y,X),State2) :-
	State = (H,W,N,Cells,Mines),
	\+(memberchk(cell((Y,X),_),Mines)),
	write('Revealing: '), write((Y,X)), nl,
	%trace,
	reveal((Y,X),Cells,Mines,Cells1),
	State1 = (H,W,N,Cells1,Mines),
	%trace,
	prune(State1,State2).
	

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
	State = (_H,_W,_N,Cells,Mines),
	%N1 is N - 1,
	memberchk(cell((Y,X),_),Mines),
	%notrace,
	write('Flagging: '), write((Y,X)), nl,
	%trace,
	flag((Y,X),Cells),
	%trace,
	prune(State,State1).
	

flag((Y,X),(Dos,_,_)) :-
	C = cell((Y,X),_),
	flagged(C),
	memberchk(C,Dos).

prune(State,State1):-
	State = (H,W,N,Cells,Mines),
	prune_done(Cells,Mines,N,N1,Cells1),
	State1 = (H,W,N1,Cells1,Mines).
prune_done(Cells,[cell((Y,X),_)|Mines],N,N2,Cells2) :-
	Cells = (Dos,Cons,Dones),
	C = cell((Y,X),_),
	memberchk(C,Dos),
	is_flagged(C),
	all_adjacent(C,Dos,AdDos),
	all_flagged(AdDos),
	all_adjacent(C,Cons,Ads),
	cons_satisfied(Ads,Dos),
	ord_del_element(Dos,C,Dos1),
	N1 is N-1,
	%write('pruning flagged: '), write(C), nl,
	ord_add_element(Dones,C,Dones1),
	ord_subtract(Cons,Ads,Cons1),
	%write('pruning constraints: '), write(Ads), nl,
	ord_union(Dones1,Ads,Dones2),
	Cells1 = (Dos1,Cons1,Dones2),
	prune_done(Cells1,Mines,N1,N2,Cells2).
prune_done(Cells,[_|Mines],N,N1,Cells1) :-
	prune_done(Cells,Mines,N,N1,Cells1).
prune_done(Cells,_,N,N,Cells).

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

get_adjacent_flags(Cell,Cells,Ads) :-
	findall(C,(member(C,Cells),adjacent(Cell,C),is_flagged(C)),Ads).

get_adjacent_flags([D|Ds],Cells,Ads2) :-
	findall(C,(member(C,Cells),adjacent(D,C),is_flagged(C)),Ads),
	ord_union(Ads,Ads1,Ads2),
	get_adjacent_flags(Ds,Cells,Ads1).
get_adjacent_flags([],_,[]).

constraint(cell(_,(N,0)),N).

%Constraints have all surrounding mines in Dos flagged
cons_satisfied([C|Cs],Dos) :-
	all_adjacent(C,Dos,Adj),
	all_flagged(Adj),
	cons_satisfied(Cs,Dos).
cons_satisfied([],_).

flagged(cell(_,('X',1))).

is_flagged(cell(A,B)):-
	all_flagged([cell(A,B)]).
all_flagged([cell(_,Content)|Cs]) :-
	Content == ('X',1),
	all_flagged(Cs).
all_flagged([]).
	
	

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