
%%% The actual query to begin the code. Q is a question or ask for a recommendation, A is the answer %%%

query(Q,A) :-
	query_helper(Q,A).

%%% Sifts at a basic level through queries. All allowed openings for recommendations as well as searching for who was in a movie, or the genre of a movie.

query_helper([what,is,B|Q],A) :-
	aan(B),
	query_recommend(Q,A).
query_helper([recommend,B|Q],A) :-
	aan(B),
	query_recommend(Q,A).
query_helper([recommend,me,B|Q],A) :-
	aan(B),
	query_recommend(Q,A).
query_helper([give,me,B|Q],A) :-
	aan(B),
	query_recommend(Q,A).
query_helper([show,me,B|Q],A) :-
	aan(B),
	query_recommend(Q,A).

query_helper([who|Q],A) :-
	query_starsearch(Q,A).

query_helper([what,genre,is|B],A) :-
	movie(B,_,A,_,_).



query_helper([i,enjoy|B],_) :-
	query_helper([i,like|B],_).
query_helper([i,love|B],_) :-
	query_helper([i,like|B],_).
query_helper([im,a,fan,of|B],_) :-
	query_helper([i,like|B],_).
query_helper([i,dont,mind|B],_) :-
	query_helper([i,like|B],_).


query_helper([i,like,B,movies],_) :-
	movie(_,B,_,_,_),
	remove_assertion(quality,dislike,B).
query_helper([i,dislike,B,movies],_) :-
	movie(_,B,_,_,_),
	add_assertion(quality,dislike,B).

query_helper([i,like,B,movies],_) :-
	movie(_,_,B,_,_),
	remove_assertion(genre,dislike,B).
query_helper([i,dislike,B,movies],_) :-
	movie(_,_,B,_,_),
	add_assertion(genre,dislike,B).


query_helper([i,like|B],_) :-
	\+ early_check(B),
	movie(_,_,_,B,_),
	remove_assertion(actor,dislike,B).
query_helper([i,like|B],_) :-
	\+ early_check(B),
	movie(_,_,_,_,B),
	remove_assertion(actor,dislike,B).

query_helper([i,hate|B],_) :-
	query_helper([i,dislike|B],_).
query_helper([i,dont,like|B],_) :-
	query_helper([i,dislike|B],_).
query_helper([im,not,a,fan,of|B],_) :-
	query_helper([i,dislike|B],_).

query_helper([i,dislike|B],_) :-
	\+ early_check(B),
	movie(_,_,_,B,_),
	add_assertion(actor,dislike,B).
query_helper([i,dislike|B],_) :-
	\+ early_check(B),
	movie(_,_,_,_,B),
	add_assertion(actor,dislike,B).

query_helper([i,like|B],_) :-
	movie(B,_,_,_,_),
	remove(movie,dislike,B).
query_helper([i,dislike|B],_) :-
	movie(B,_,_,_,_),
	add_assertion(movie,dislike,B).


early_check([_,movies]).
aan(a).
aan(an).

	
%Q will be something like “action movie with Ryan gosling and Harrison ford” or “good horror movie”

%% This is the code for the main recommendation function. The function returns all possible movies that fit the criteria asked for. findall adds them to a list, random selects a number at random, and nth0 allows us to fetch the number in that list (effectively generating a random movie).

%% This function works by recursing down the data and shaving a bit off each time, starting with the quality, genre, movie, actors.

query_recommend(Q,A) :-
	findall(D, query_recommend_helper(Q,D),Z),
	nth0(0,Z,A).

query_recommend_helper(Q,A) :-	 	
	quality_movie_nl(Q,Quality,Genre,Actor1,Actor2),
	recommend(Quality,Genre,Actor1,Actor2,A).

quality_movie_nl([H|T],H,Genre,Actor1,Actor2) :-
	quality_nl(H),
	genre_movie_nl(T,Genre,Actor1,Actor2).

quality_movie_nl([H|T],_,Genre,Actor1,Actor2) :-
	\+ quality_nl(H),
	genre_movie_nl([H|T],Genre,Actor1,Actor2).

genre_movie_nl([H|T],H,Actor1,Actor2) :-
	movie(_,_,H,_,_),
	general_movie_nl(T,Actor1,Actor2).

genre_movie_nl([H|T],_,Actor1,Actor2) :-
	\+ movie(_,_,H,_,_),
	general_movie_nl([H|T],Actor1,Actor2).


general_movie_nl(T0,Actor1,Actor2) :-
	movie_check_with(T0,T1),
	actor_check(T1,Actor1,Actor2).

general_movie_nl(T0,_,_) :-
	\+ movie_check_with(T0,_),
	movie_check(T0).
	
movie_check_with([movie,with|T1],T1).
movie_check([movie]).

%Allowable movie qualities.
quality_nl(good).
quality_nl(bad).

%Because actors may have 3 names or 1 name, this checks to format them correctly.

actor_check([Firstname,and|T1],[Firstname],T1).
actor_check([Firstname,Lastname,and|T1],[Firstname,Lastname],T1).
actor_check([Firstname,Middlename,Lastname,and|T1],[Firstname,Middlename,Lastname],T1).

actor_check([Firstname],[Firstname],_).
actor_check([Firstname,Lastname],[Firstname,Lastname],_).
actor_check([Firstname,Middlename,Lastname],[Firstname,Middlename,Lastname],_) :-
	dif(and,Middlename).

/*
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
*/

%% Starsearch allows us to get the leads of a movie.

query_starsearch(Q,A) :-
	starsearch_trim(Q,M),
	stars_of_movie(M,A).

starsearch_trim([A,the,C,D|E],E) :-
	trans(A),
	stars(C),
	offor(D).

starsearch_trim([A,in|E],E) :-
	trans(A).

starsearch_trim([starred,in|E],E).


trans(is).
trans(was).
trans(are).
stars(stars).
stars(star).
stars(lead).
stars(leads).
offor(of).
offor(in).
offor(for).

stars_of_movie(M,[A1,and,A2]) :-
	movie(M,_,_,A1,A2).

/*
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
*/

%recommend(Quality,Genre,Actor1,Actor2,X)
% Returns X = a single random movie that meets the criteria

recommend(Q,G,A1,A2,X) :- 
	random_movie_from_list(Q,G,A1,A2,X).



%add_assertion(criterion, dislike, value)
%add_assertion(genre, dislike, action) <- example

%% This creates a rule using =.. and adds it to the database.

add_assertion(X, Z, Y) :-
    Assertion =.. [Z, X, Y],
    assert(Assertion).


%remove_assertion(criterion, dislike, value)
%remove_assertion(genre, dislike, action) <- example

%% This creates a rule using =.. and removes it from the database (if it was already added).

remove_assertion(X, Z, Y) :-
    Assertion =.. [Z, X, Y],
    retract(Assertion).

%random_movie_from_list(Quality,Genre,Actor1,Actor2,X).
% Returns a random movies elected from the list
random_movie_from_list(Q,G,A1,A2,X) :-
	recommend_list(Q,G,A1,A2,List),
	length(List,Length),
	random(0,Length,Index),
	nth0(Index,List,X).


%recommend_list(Quality,Genre,Actor1,Actor2,X).
% returns a X of all movies with that criteria
recommend_list(Q,G,A1,A2,Z) :-
	findall(X,recommend_helper(Q,G,A1,A2,X),Z),
	\+ length(Z,0).

recommend_helper(Q,G,A1,A2,X) :-
	movie(X,Q,G,A1,A2),
	\+ dislike(quality,Q),
	\+ dislike(genre,G),
	\+ dislike(actor,A1),
	\+ dislike(actor,A2),
	\+ dislike(movie,X).


recommend_helper(Q,G,A1,A2,X) :-
	movie(X,Q,G,A2,A1),
	\+ dislike(quality,Q),
	\+ dislike(genre,G),
	\+ dislike(actor,A1),
	\+ dislike(actor,A2),
	\+ dislike(movie,X).

%movie(name,quality,genre,actor1,actor2)
% Returns all movies that fit the desired criteria. Leave criteria as variables if they do not matter.

%  The Database of Movies

% movie([name], quality, genre, [actor1], [actor2]).
% note: we could've added year to differentiate between movies with the same name.
% quality is good or bad
% genre is action, sci-fi, drama, comedy, horror, romance or animation

movie([pirates,of,the,caribbean],good,action,[johnny,depp],[keira,knightley]).
movie([harry,potter,and,the,philosophers,stone],good,acion,[daniel,radcliffe],[emma,watson]).
movie([blade,runner,2049], good, sci-fi, [ryan,gosling], [harrison,ford]).
movie([the,dark,knight], goood, action, [christian,bale], [heath,ledger]).
movie([fight,club], good, drama, [edward,norton], [brad,pitt]).
movie([inception], good, action, [leonardo,dicaprio], [ellen,page]).
movie([the,matrix], good, action, [keanu,reeves], [laurence,fishburne]).
movie([spirited,away], good, animation, [rumi,hiiragi], [miyu,irino]).
movie([interstellar], good, sci-fi, [matthew,mcconaughey], [anne,hathaway]).
movie([memento], good, drama, [guy,pearce], [carrie-anne,moss]).
movie([django,unchained], good, drama, [jamie,foxx], [christoph,waltz]).
movie([requiem,for,a,dream], good, drama, [jared,leto], [ellen,burstyn]).
movie([3,idiots], good, comedy, [aamir,khan], [kareena,kapoor]).
movie([good,will,hunting], good, drama, [matt,damon], [robin,williams]).
movie([inglorious,basterds], good, drama, [brad,pitt], [christoph,waltz]).
movie([scarface], good, drama, [al,pacino], [michelle,pfeiffer]).
movie([die,hard], good, action, [bruce,willis], [alan,rickman]).
movie([john,wick], good, action, [keanu,reeves], [michael,nyqvist]).
movie([la,la,land], good, romance, [ryan,gosling], [emma,stone]).
movie([blade,runner], good, sci-fi, [harrison,ford], [sean,young]).
movie([v,for,vendetta], good, action, [natalie,portman], [hugo,weaving]).
movie([the,wolf,of,wall,street], good, drama, [leonardo,dicaprio], [jonah,hill]).
movie([finding,nemo], good, animation, [ellen,degeneres], [albert,brooks]).
movie([finding,dory], good, animation, [ellen,degeneres], [albert,brooks]).
movie([gone,girl], good, drama, [ben,affleck], [rosamund,pike]).
movie([catwoman], bad, action, [halle,berry], [benjamin,bratt]).
movie([the,last,airbender], bad, action, [noah,ringer], [dev,patel]).
movie([get,out], good, horror, [daniel,kaluuya], [allison,williams]).
movie([psycho], good, horror, [anthony,perkins], [janet,leigh]).
movie([jaws], good, horror, [roy,schneider], [robert,shaw]).
movie([it,follows], good, horror, [maika,monroe], [keir,gilchrist]).
movie([the,babadook], good, horror, [essie,davis], [barbara,west]).
movie([the,emoji,movie], bad, animation, [tj,miler], [james,corden]).
movie([dragonball,evolution], bad, action, [justin,chatwin], [yun-fat,chow]).
movie([the,hangover], good, comedy, [zach,galifianakis], [bradley,cooper]).
movie([the,florida,project], good, drama, [willem,dafoe], [valeria,cotto]).
movie([troll,2], bad, horror, [michael,stephenson], [george,hardy]).
movie([avatar], good, action, [sam,worthington], [zoe,saldana]).
movie([8,mile], good, drama, [marshall,mathers], [kim,basinger]).
movie([the,room], bad, drama, [tommy,wiseau], [greg,sestero]).
movie([titanic], good, romance, [leonardo,dicaprio], [kate,winslet]).
movie([the,notebook], good, romance, [ryan,gosling], [rachel,mcadams]).
movie([the,big,sick], good, romance, [kumail,nanjiani], [zoe,kazan]).
movie([her], good, romance, [joaquin,phoenix], [scarlett,johansson]).
movie([the,fault,in,our,stars], good, drama, [shailene,woodley], [ansel,elgort]).
movie([baby,driver], good, action, [ansel,elgort], [kevin,spacey]).
movie([dunkirk], good, drama, [harry,styles], [tom,hardy]).
movie([wonder,woman], good, action, [gal,gadot], [chris,pine]).
movie([deadpool], good, action, [ryan,reynolds], [tj,miller]).
movie([zootopia], good, animation, [ginnifer,goodwin], [jason,bateman]).
movie([office,christmas,party], bad, comedy, [tj,miller], [jennifer,aniston]).
movie([twilight], bad, romance, [kristen,stewart], [robert,pattinson]).
movie([eragon], bad, action, [ed,speelers], [jeremy,irons]).

dislike(genre,table).

/*
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=—=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=
*/

/* Try the following queries:
?- ask([what,is,a,movie],A).
?- ask([what,is,a,good,movie],A).
?- ask([what,is,a,good,action,movie],A).
?- ask([what,is,a,good,movie,with,ryan,gosling],A).
?- ask([what,is,a,good,movie,with,ryan,gosling,and,harrison,ford],A).
?- ask([what,is,a,movie,with,harrison,ford],A).					
?- ask([who,are,the,stars,of,blade,runner,2049],A).
?- ask([who,are,the,leads,in,the,room],A).
?- ask([i,dislike,action,movies],A).
?- ask([i,like,action,movies],A).
?- ask([i,hate,action,movies],A).
?- ask([im,not,a,fan,of,good,movies],A).
?- ask([i,love,good,movies],A).
?- ask([what,genre,is,blade,runner,2049],A).
*/



