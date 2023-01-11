%----------------------------------------------------------
% Challenge 3: Maze Solver
% Christopher Stickle
% CSC 366
%----------------------------------------------------------
% This was given from class

% Adjacent cells
adjacent([2,1],[2,2]).
adjacent([2,2],[3,2]).
adjacent([3,2],[3,3]).
adjacent([3,3],[4,3]).
adjacent([4,3],[5,3]).
adjacent([5,3],[5,2]).
adjacent([5,3],[5,4]).
adjacent([5,4],[5,5]).
adjacent([3,3],[3,4]).
adjacent([3,4],[3,5]).
adjacent([3,5],[3,6]).
adjacent([3,6],[2,6]).


start([2,1]).
end([2,6]).
%end([5,5]).

validMove([X,Y],[NewX,NewY]) :- adjacent([X,Y],[NewX,NewY]).
validMove([NewX,NewY],[X,Y]) :- adjacent([X,Y],[NewX,NewY]).

%Base Case
findPath(CurrentPosition,End,_,[End]) :-
    CurrentPosition = End.

findPath(CurrentPosition,End,SeenSpots,[CurrentPosition|Path]) :-
    % Find the adjacent cell.
    validMove(CurrentPosition, NewPosition),
    % Ensure we haven't been there yet
    \+ member(NewPosition, SeenSpots),
    % Keep going until done...
    findPath(NewPosition, End, [NewPosition|SeenSpots], Path).

solve(Path) :-
    start(CurrentPosition),
    end(End),
    findPath(CurrentPosition, End, [CurrentPosition], Path).

drawCell(Column, Row, _) :- \+ validMove([Column, Row],_), write("X"), !.
drawCell(Column, Row, Path) :- member([Column, Row], Path), write("*"), !.
drawCell(Column, Row, _) :- start([Column, Row]), write("S"), !.
drawCell(Column, Row, _) :- end([Column, Row]), write("E"), !.
drawCell(_, _, _) :- write("O").

drawRow(Row, Path) :-
    drawCell(1, Row, Path), tab(1),
    drawCell(2, Row, Path), tab(1),
    drawCell(3, Row, Path), tab(1),
    drawCell(4, Row, Path), tab(1),
    drawCell(5, Row, Path), tab(1),
    drawCell(6, Row, Path), nl.

draw :-
    drawRow(1, []),
    drawRow(2, []),
    drawRow(3, []),
    drawRow(4, []),
    drawRow(5, []),
    drawRow(6, []).
draw(Path) :-
    drawRow(1, Path),
    drawRow(2, Path),
    drawRow(3, Path),
    drawRow(4, Path),
    drawRow(5, Path),
    drawRow(6, Path).

% Submition code below this line
%-----------------------------------------------------------------------
% Task: 1
% Follow the left wall.
% I set up headings along with what headeing are left and right of a given heading.
% I build some tryLeft predicates to try to move left, if it fails,
% it turns right and trys again.
% The IF -> THEN; ELSE built in was hugely helpful in the the solve predicate.
/*
Expected Output:
?- solveLeft(Path), draw(Path),nl.
X * X X X X
X * * X * X
X X * * * X
X X * X * X
X X * X * X
X * * X X X

Path = [[2, 1], [2, 1], [2, 2], [3, 2], [3, 2], [3, 2], [3, 3], [4, 3],
[4, 3], [5, 3], [5, 2], [5, 2], [5, 2], [5, 2], [5, 3], [5, 3], [5, 4],
[5, 4], [5, 5], [5, 5], [5, 5], [5, 5], [5, 4], [5, 4], [5, 3], [4, 3],
[4, 3], [3, 3], [3, 4], [3, 4], [3, 5], [3, 5], [3, 6], [3, 6], [3, 6],
[2, 6]]

NOTE: As it's currently set up, each turn in a cell is counted as visiting
      the cell again. I tried to clean this up but to no avail.
*/
%-----------------------------------------------------------------------
heading(s).
heading(n).
heading(e).
heading(w).

% leftOf(X,Y). X is to the left of Y
leftOf(n,e).
leftOf(s,w).
leftOf(e,s).
leftOf(w,n).

% rightOf(X,Y). X is to the right of Y
rightOf(n,w).
rightOf(s,e).
rightOf(e,n).
rightOf(w,s).

getLeftHeading(Heading,NewHeading) :- rightOf(Heading,NewHeading).
getRightHeading(Heading,NewHeading) :- leftOf(Heading,NewHeading).

% heading North
tryLeft([X,Y],[NewX,NewY],n) :-
    NewX is X-1,
    NewY = Y.
% heading South
tryLeft([X,Y],[NewX,NewY],s) :-
    NewX is X+1,
    NewY = Y.
% heading East
tryLeft([X,Y],[NewX,NewY],e) :-
    NewY is Y-1,
    NewX = X.
% heading West
tryLeft([X,Y],[NewX,NewY],w) :-
    NewY is Y+1,
    NewX = X.

%base case
findLeftWallPath(CurrentPosition,End,_,[End]) :- CurrentPosition = End.
%recursive case
% get left and right headings
% try to go left, if it works, go left, update heading to left heading and recurse
% if it doesn't work, face right heading and recurse
findLeftWallPath(CurrentPosition,End,CurrentHeading,[CurrentPosition|Path]) :-
    getLeftHeading(CurrentHeading,NewHeadingL),
    getRightHeading(CurrentHeading,NewHeadingR),
    tryLeft(CurrentPosition, NewPosition, CurrentHeading),
    (validMove(CurrentPosition, NewPosition) ->
        findLeftWallPath(NewPosition, End, NewHeadingL, Path);
        findLeftWallPath(CurrentPosition,End,NewHeadingR,Path)
    ).

solveLeft(Path) :- start(CurrentPosition),end(End),
    findLeftWallPath(CurrentPosition, End, s, Path).
%-----------------------------------------------------------------------
% Task: 2
% Go, generally, in the direction of the 'End' point.
% for this I find the distance to the end for each available valid move
% and then choose the one with the smallest distance
% I calculate distance using the Euclidean distance formula
/*
Expected Output:

?- solveDir(Path), draw(Path), nl.
X * X X X X
X * * X O X
X X * O O X
X X * X O X
X X * X O X
X * * X X X

Path = [[2, 1], [2, 2], [3, 2], [3, 3], [3, 4], [3, 5], [3, 6], [2, 6]]

With end([5,5]):
?- solveDir(Path), draw(Path), nl.
X * X X X X
X * * X O X
X X * * * X
X X O X * X
X X O X * X
X O O X X X

Path = [[2, 1], [2, 2], [3, 2], [3, 3], [4, 3], [5, 3], [5, 4], [5, 5]]
*/
%-----------------------------------------------------------------------

% Find the distance between two points
closerToEnd([X, Y], [NewX, NewY]) :-
    end([EndX, EndY]),
    sqrt((EndX-NewX)**2 + (EndY-NewY)**2) < sqrt((EndX-X)**2 + (EndY-Y)**2).

%Base Case
findPathDir(CurrentPosition,End,_,[End]) :-
    CurrentPosition = End.

findPathDir(CurrentPosition,End,SeenSpots,[CurrentPosition|Path]) :-
    % Find the adjacent cell.
    validMove(CurrentPosition, NewPosition),
    %write("Trying "),write(NewPosition),nl, % debug
    % Check to see if that cell is closer to the end that the current cell
    closerToEnd(CurrentPosition, NewPosition),
    % Ensure we haven't been there yet
    \+ member(NewPosition, SeenSpots),
    % Keep going until done...
    findPathDir(NewPosition, End, [NewPosition|SeenSpots], Path).

% otherwise, try the other adjacent cell
findPathDir(CurrentPosition,End,SeenSpots,[CurrentPosition|Path]) :-
    % Find the adjacent cell.
    validMove(CurrentPosition, NewPosition),
    %write("Instead "),write(NewPosition),nl, % debug
    % Ensure we haven't been there yet
    \+ member(NewPosition, SeenSpots),
    % Keep going until done...
    findPathDir(NewPosition, End, [NewPosition|SeenSpots], Path).

solveDir(Path) :-
    start(CurrentPosition),
    end(End),
    findPathDir(CurrentPosition, End, [CurrentPosition], Path).

%-----------------------------------------------------------------------
% Task: 3
% Do my own thing.
% chunk the maze into 2 parts
% solve from the start to the "middle"
% solve from the end to the "middle"
% Concat the two paths

/*
Expected Output:
?- solveInChunks(P), draw(P), nl.
X * X X X X
X * * X O X
X X * * * X
X X * X O X
X X * X O X
X * * X X X

P = [[2, 1], [2, 2], [3, 2], [3, 3], [4, 3], [5, 3],
    [5, 3], [4, 3], [3, 3], [3, 4], [3, 5], [3, 6], [2, 6]]

As I was thinking about this part of the challenge I thought about how I solve large mazes.
Often, if I spend a considerable amount of time pathing fom the Start I will switch and start
working from the End.  I may continue to chunk the maze into smaller pieces and solve each
until I find one complete path.
I thought this would be a fun way to solve the maze.

For the purposes of this maze, 2 parts is the only fesable way to do this as the size of this
maze is so small.  We declare an end() and start() in the begining of the program.  We could
also define a middle([X,Y]) and use that as the dividing point.  We could instead declare
size([X,Y]) and use that to compute a mid point. But both of those are to easy and felt
unworthy of a "challenge" so I didnt use those.  I wanted to find a way have prolog figure it
out instead.

Firstly, I figured I could get the dementions of a maze by going thought all the cells and
looking for the max X and Y values.  I could then use those to compute a mid point.  To do this
I needed a list of all the cells. I created cells/1 to do this, it uses validmove/2 to find
all the cells.

This statrted down a path that didnt work out.  I used maxX/1 and maxY/1 to find build the size,
then middle/2 to find a middle point.  This however was not guarenteed to find a valid mid point.
In fact in this case the point it finds [2,3] is not path-able.  I turns out dynamically finding
the next best point is not so easy so i switched gears.  I left in that part of code as a comment
block in case you wanted to see it.

Back to cells/1.  I decided an easier, way to split the maze is take the middle cell in the list.
This way we will always have a vailid point to path to but at the cost of possibly not being the
best middle point. This meant I needed to clean out any duplicates and sort he list of pairs.

On to the solveInChunks/1.  Basically, I wanted to solve from the begining to the middle point,
and then from the end to the middle point or when it interesects the first path.  The second part
was harder than I thought.   I left in two different attempts with output in comment blocks for
your review.  Both complied and ran but with unexpected results. The first is cleaner but won't
add [3,3] to the end-2-middle path.  the second does add it but with a global variable thing
as well.  I didnt really like either so I left those out and went with the current solution
which paths from the start to the middle and then from end to the middle and then concats the
two paths.  While this was not ideal it works and is easy to read.

Side note:  This mid point technique will only guarantee a a valid middle point if there is
      not a disjoint set of cells in the maze that are not connected to the rest of the maze.
consider this case:
O S X X X X
O X X X X X
O X O O X X
O X O O X X
O X X X X X
O E X X X X

the middle point may end up being a point that neither start nor end could path to.
*/

%-----------------------------------------------------------------------

% Make a list of all unique cells in the maze from the 'adjacent' predicates
%   remove duplicates
%   sort the list
cells(SortedList) :-
    findall([X,Y], (validMove([X,Y],_), X > 0, Y > 0), Bag),
    removeDuplicates(Bag, List),
    sortPairs(List, SortedList).

% Remove duplicates from a list
removeDuplicates([],[]).
removeDuplicates([H|T],List) :-
    member(H,T),
    removeDuplicates(T,List).
removeDuplicates([H|T],[H|T1]) :-
    \+ member(H,T),
    removeDuplicates(T,T1).

% sort a list of pairs. Grouped by Y's
sortPairs(List, Sorted) :-
    findall([X,Y], order_by([asc(Y),asc(X)], member([X,Y], List)), Sorted).

% The middle point is the middle of the list of cells
middlePoint(Middle) :-
    cells(List),
    length(List, Length),
    MiddleIndex is Length // 2,
    nth0(MiddleIndex, List, Middle).

/*
% find the max dimension of the maze
maxX(MaxX) :-
    cells(Cells),
    maxX(Cells, MaxX).
maxX([], 0).
maxX([[X, _]|T], MaxX) :-
    maxX(T, MaxXT),
    MaxX is max(X, MaxXT).
maxY(MaxY) :-
    cells(Cells),
    maxY(Cells, MaxY).
maxY([], 0).
maxY([[_, Y]|T], MaxY) :-
    maxY(T, MaxYT),
    MaxY is max(Y, MaxYT).

size([MaxX, MaxY]) :-
    maxX(MaxX),
    maxY(MaxY).

% find the middle of the maze
middle([MiddleX, MiddleY]) :-
    size([MaxX, MaxY]),
    MiddleX is div(MaxX, 2),
    MiddleY is div(MaxY, 2).

% find if middle exists in Cells
closestMiddle(List, [NewX, NewY]) :-
    middle([MiddleX, MiddleY]),
    member([NewX, NewY], List),
    NewX = MiddleX,
    NewY = MiddleY.
% if not find the next best fit for Middle in Cells
%    this turned out to be a whole different can of worms
%    which encouraged my deviation to the simpler methods now implemented
*/

% reverse list
reverseList([],[]).
reverseList([H|T],Reversed) :-
    reverseList(T,ReversedT),
    append(ReversedT,[H],Reversed).

% find the path from the start to the middle
solveFromStart(Path_S,Middle) :-
    start(CurrentPosition),
    findPath(CurrentPosition, Middle, [CurrentPosition], Path_S).

% find the path from the end to the middle
solveFromEnd(Path_E,Middle) :-
    end(CurrentPosition),
    findPath(CurrentPosition, Middle, [CurrentPosition], Path_E).

% combine the two paths
solveInChunks(Path) :-
    middlePoint(Middle),
    solveFromStart(Path_S,Middle),
    solveFromEnd(Path_E,Middle),
    reverseList(Path_E, Path_E_Rev),
    append(Path_S,Path_E_Rev,Path).
/*
findPathFromEnd(CurrentPosition,End,SeenSpots,[CurrentPosition|Path], Path_S) :-
    validMove(CurrentPosition, NewPosition),
    \+ member(NewPosition, SeenSpots),
    (member(NewPosition,Path_S) -> !, [[NewPosition]|Path] ;
    findPathFromEnd(NewPosition, End, [NewPosition|SeenSpots], Path, Path_S)
    ).

     ?- solveInChunks(P), draw(P).
    ERROR: source_sink `[3,3]' does not exist
    X * X X X X
    X * * X O X
    X X * * * X
    X X * X O X
    X X * X O X
    X * * X X X
    P = [[2, 1], [2, 2], [3, 2], [3, 3], [4, 3], [5, 3], [3, 4], [3, 5], [3, 6], [2, 6]] .
*/
/*
findPathFromEnd(CurrentPosition,End,SeenSpots,[CurrentPosition|Path], Path_S) :-
    validMove(CurrentPosition, NewPosition),
    \+ member(NewPosition, SeenSpots),
    (member(NewPosition,Path_S) -> !, append([NewPosition],[Path],Path) ;
    findPathFromEnd(NewPosition, End, [NewPosition|SeenSpots], Path, Path_S)
    ).

    [1]  ?- solveInChunks(P), draw(P).
    X * X X X X
    X * * X O X
    X X * * * X
    X X * X O X
    X X * X O X
    X * * X X X
    P = [[2, 1], [2, 2], [3, 2], [3, 3], [4, 3], [5, 3], '$VAR'('_S1'), [3, 3], [3, 4], [3, 5], [3, 6], [2, 6]],
        % where _S1 = [[3, 3], '$VAR'('_S1')]
*/
