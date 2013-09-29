% Jolly Roger

% Logicbae
% Deck rooms
room(jail).
room(corridor).
room('death room').
room(tavern).
room('pirate museum').
room('forbidden room').
room('weapon room').
room('captain room').
room('lounge').
room('heavy metal dark room').
room('exit').

% Door connections
:- dynamic(door/3).

door(jail, corridor, unlocked).
door(corridor, 'death room', unlocked).
door('death room', tavern, unlocked).
door(tavern, 'pirate museum', unlocked).
door(tavern, 'heavy metal dark room', unlocked).
door(tavern, 'weapon room', unlocked).
door('weapon room', 'captain room', unlocked).
door('captain room', lounge, unlocked).
door('pirate museum', 'heavy metal dark room', unlocked).
door('heavy metal dark room', 'forbidden room', locked).
door('heavy metal dark room', lounge, unlocked).
door(lounge, exit, locked).

:- forall(door(X,Y,Z), assert(door(Y,X,Z))).

% Objects
:- dynamic(position/2).

position(object('sharp edge', heavy), jail).
position(object('rum bottle', light), jail).
position(object(chest, heavy), corridor).
position(object(axe, light), chest).
position(object(rope, light), chest).
position(object('pirate hat', light), chest).
position(object('pirate skeleton', light), 'death room').
position(object(candle, light), 'death room').
position(object(pirate, heavy), tavern).
position(object(rum, light), tavern).
position(object('fruit container', heavy), tavern).
position(object(bannana, light), 'fruit container').
position(object(apple, light), 'fruit container').
position(object(annanas, light), 'fruit container').
position(object('pirate clothes', light), 'pirate museum').
position(object('chest key', light), 'forbidden room').
position(object(gold, light), 'forbidden room').
position(object(lion, heavy), 'forbidden room').
position(object('armor box', heavy), 'weapon room').
position(object(gun, light), 'armor box').
position(object(bullets, light), 'armor box').
position(object('cannon balls', heavy), 'armor box').
position(object(rope, light), 'armor box').
position(object('gun powder', light), 'armor box').
position(object(cutlass, light), 'weapon room').
position(object(matches, light), 'captain room').
position(object('treasure map', light), 'captain room').
position(object('forbidden room key', light), 'captain room').
position(object(candle, light), 'captain room').
position(object(pianist, heavy), lounge).

% Extra object/room facts
edible(bannana).
edible(apple).
edible(annanas).
edible(rum).

killable(pirate).
killable(lion).
killable(pianist).

container('fruit container', unlocked).
container(chest, locked).
container('armor box', unlocked).

dark('heavy metal dark room').

% Character info
:- dynamic(location/1).
location('weapon room').

:- dynamic(inventory/1).
inventory([]).


% Gameplay!
% Moving around
move(Container):-
	container(Container, _),
	speak(['Are you out of your mind?! What are you going to do in the ', Container, '?!']), !, fail.
move(Room):-
	check_room_exists(Room),
	location(CurrentLocation), !,
	check_door_state(CurrentLocation, Room),
	retract(location(CurrentLocation)),
	asserta(location(Room)),
	speak(['You walked into the ', Room, '!']),
	look_around, !.

check_room_exists(Room):-
	room(Room).
check_room_exists(_):-
	speak(['There is no such room!']), fail.

check_door_state(CurrentLocation, Room):-
	door(CurrentLocation, Room, unlocked).
check_door_state(CurrentLocation, Room):-
	door(CurrentLocation, Room, locked),
	speak(['The door is locked, maybe you can use something to unlock it!']), !, fail.
check_door_state(CurrentLocation, CurrentLocation):-
	speak(['You are already there!']), !, fail.
check_door_state(CurrentLocation, Room):-
	speak(['You can not get to the ', Room, ' from the ', CurrentLocation, '!']), fail.


% Looking around
look_around:-
	location(CurrentLocation),
	list_room_items(CurrentLocation),
	speak(['From here you can go to: ']),
	print_adjacent_rooms(CurrentLocation).

list_room_items(Room):-
	position(object(_, _), Room),
	speak(['In ', Room, ' you can find: ']),
	print_items(Room), !.
list_room_items(_):-
	speak(['There are no items in this room!']).


% Interacting with objects
get_object(Name):-
	location(CurrentLocation),
	position(object(Name, light), CurrentLocation),
	retract(position(object(Name, _), CurrentLocation)),
	inventory_add(Name), !.
get_object(Name):-
	location(CurrentLocation),
	position(object(Name, heavy), CurrentLocation),
	speak(['The object is too heavy to pick!']), !.
get_object(_):-
	speak(['There is no such object in here!']).

put_object(Name):-
	location(CurrentLocation),
	inventory_remove(Name),
	asserta(position(object(Name, light), CurrentLocation)), !.
put_object(_):-
	speak(['There is no such object in your inventory!']).

inspect_object(Container):-
	location(CurrentLocation),
	position(object(Container, _), CurrentLocation),
	container(Container, unlocked),
	position(object(_, _), Container),
	speak(['In the ', Container, ' you can find:']),
	print_items(Container),
	add_objects_to_location(Container), !.
inspect_object(Container):-
	location(CurrentLocation),
	position(object(Container, _), CurrentLocation),
	container(Container, unlocked),
	\+ position(object(_, _), Container),
	speak(['The ', Container, ' is now empty!']), !.
inspect_object(Container):-
	location(CurrentLocation),
	position(object(Container, _), CurrentLocation),
	container(Container, locked),
	speak(['The ', Container, ' is locked, you can not look inside of it!']), !.
inspect_object(Name):-
	location(CurrentLocation),
	position(object(Name, _), CurrentLocation),
	\+ container(Name, _),
	speak(['There is nothing to inspect on ', Name, '! It is just a ', Name, '!']), !.
inspect_object(_):-
	speak(['There is no such object in this room!']).


% Auxiliary methods
% User output
speak([]):- nl.
speak([H|T]):- write(H), speak(T).

print_items(Room):-
	position(object(Name, Weight), Room), tab(4), speak([Name, ' (', Weight, ')']), fail.
print_items(_).

print_adjacent_rooms(Room):-
	door(Room, AdjacentRoom, Status), tab(4), speak([AdjacentRoom, ' (Door is ', Status, ')']), fail.
print_adjacent_rooms(_).

% List manipulation
inventory_check(Name):-
	inventory(List),
	inventory_check0(Name, List).

inventory_check0(Name, [Name|_]).
inventory_check0(Name, [_|T]):- inventory_check0(Name, T).

inventory_add(Name):-
	inventory(OldList),
	retract(inventory(_)),
	asserta(inventory([Name|OldList])).

inventory_remove(Name):-
	inventory_check(Name),
	inventory(List),
	inventory_remove0(Name, List, Result),
	retract(inventory(_)),
	asserta(inventory(Result)).

inventory_remove0(Name, [Name|Rest], Rest).
inventory_remove0(Name, [H|Rest], [H|T]):-
	inventory_remove0(Name, Rest, T).

% Misc
add_objects_to_location(Container):-
	location(CurrentLocation),
	position(object(Object, light), Container),
	asserta(position(object(Object, light), CurrentLocation)), 
	retract(position(object(Object, light), Container)), fail.
add_objects_to_location(_).
