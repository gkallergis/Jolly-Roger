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

% Objects & Characters
:- dynamic(position/2).

position(character(pirate, alive), tavern).
position(character(lion, alive), 'forbidden room').
position(character(pirate, alive), lounge).

position(object('sharp edge', heavy), jail).
position(object('rum bottle', light), jail).
position(object(chest, heavy), corridor).
position(object(axe, light), chest).
position(object(rope, light), chest).
position(object('pirate skeleton', light), 'death room').
position(object(candle, light), 'death room').
position(object(rum, light), tavern).
position(object('fruit container', heavy), tavern).
position(object(bannana, light), 'fruit container').
position(object(apple, light), 'fruit container').
position(object(annanas, light), 'fruit container').
position(object('pirate clothes', light), 'pirate museum').
position(object('chest key', light), 'forbidden room').
position(object(gold, light), 'forbidden room').
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

% Extra object/room facts
edible(bannana).
edible(apple).
edible(annanas).
edible(rum).

wearable('pirate skeleton').
wearable('pirate clothes').

killable(pirate).
killable(lion).

container('fruit container', unlocked).
container(chest, locked).
container('armor box', unlocked).

dark('heavy metal dark room').

% Character info
:- dynamic(location/1).
location(jail).

:- dynamic(inventory/1).
inventory([]).

:- dynamic(clothing/1).
clothing([]).

:- dynamic(jail_assets/1).
jail_assets(['rope']).

% Gameplay!
% Moving around
move(Container):-
	container(Container, _),
	speak(['Are you out of your mind?! What are you going to do in the ', Container, '?!']), !, fail.
move(Room):-
	room_exists(Room),
	location(CurrentLocation), !,
	door_unlocked(CurrentLocation, Room), !,
	room_entrance_condition(Room),
	retract(location(CurrentLocation)),
	asserta(location(Room)),
	speak(['You walked into the ', Room, '!']),
	look_around, !.

room_exists(Room):-
	room(Room).
room_exists(_):-
	speak(['There is no such room!']), fail.

door_unlocked(CurrentLocation, Room):-
	door(CurrentLocation, Room, unlocked).
door_unlocked(CurrentLocation, Room):-
	door(CurrentLocation, Room, locked),
	speak(['The door is locked, maybe you can use something to unlock it!']), !, fail.
door_unlocked(CurrentLocation, CurrentLocation):-
	speak(['You are already there!']), !, fail.
door_unlocked(CurrentLocation, Room):-
	speak(['You can not get to the ', Room, ' from the ', CurrentLocation, '!']), fail.

room_entrance_condition(corridor):-
	jail_assets(JailList),
	list_check('rope', JailList),
	speak(['Blackbeard - "I think I can''t do much with my hands tied, can I?!"']), !, fail.
room_entrance_condition(tavern):-
	position(character(pirate, alive), tavern),
	clothing(ClothingList),
	\+ list_check('pirate skeleton', ClothingList),
	speak(['Blackbeard - "I think there is someone in there! I better not go in unprepared!"']), !, fail.
room_entrance_condition(tavern):-
	position(character(pirate, alive), tavern),
	retract(position(character(pirate, alive), tavern)),
	asserta(position(character(pirate, dead), tavern)),
	speak(['Blackbeard - "Arrr!"']),
	speak(['Pirate - "Aaaaaaa...*splash*!"']),
	speak(['Blackbeard - "Oh, that guy was scared!"']), nl.
room_entrance_condition('heavy metal dark room'):-
	inventory(InventoryList),
	list_check('lit candle', InventoryList),
	speak(['Blackbeard - "Arrr!! Running Wild concert!! Aye aye!! http://youtu.be/zwnipCkNgDw "']), nl.
room_entrance_condition('heavy metal dark room'):-
	speak(['Blackbeard - "It''s too dark in there! I can''t see a thing!"']), !, fail.
room_entrance_condition('forbidden room'):-
	position(character(lion, alive), 'forbidden room'),
	inventory(InventoryList),
	\+ list_check('loaded gun', InventoryList),
	speak(['Blackbeard - "Arrr!! There is a lion guarding the treasure, I need to get rid of it somehow!"']), !, fail.
room_entrance_condition('forbidden room'):-
	position(character(lion, alive), 'forbidden room'),
	retract(position(character(lion, alive), 'forbidden room')),
	asserta(position(character(lion, dead), 'forbidden room')),
	speak(['* Bang Bang *']),
	speak(['Lion - Roooooooar...* dap *!']),
	speak(['Blackbeard - "Arrr!! That was messy!"']), nl.
room_entrance_condition(lounge):-
	position(character(pirate, alive), lounge),
	clothing(ClothingList),
	\+ list_check('pirate clothes', ClothingList),
	speak(['Blackbeard - "Arrr!! The pianist will get me if I go in there looking like that!"']), !, fail.
room_entrance_condition(lounge):-
	position(character(pirate, alive), lounge),
	clothing(ClothingList),
	list_check('pirate clothes', ClothingList),
	speak(['Pianist pirate singing']),
	speak(['"Rising the flag on the masthead']),
	speak(['The sails and the ropes'' holding tight']),
	speak(['The gunners are eager to fire']),
	speak(['Well prepared for the fight"']),
	speak(['Blackbeard - "Arrrr!! A piano player! He is too distracted, but I''d better keep an eye on him though..."']), nl.
room_entrance_condition(exit):-
	speak(['Blackbeard - "Arrrrrrrrrrrrrrrrrrr!!"']), nl.
room_entrance_condition(_).


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
	inventory(OldList),
	list_add(Name, OldList, NewList),
	retract(inventory(_)),
	asserta(inventory(NewList)), !.
get_object(Name):-
	location(CurrentLocation),
	position(object(Name, heavy), CurrentLocation),
	speak(['The object is too heavy to pick!']), !.
get_object(_):-
	speak(['There is no such object in here!']).

put_object(Name):-
	location(CurrentLocation),
	inventory(OldList),
	list_remove(Name, OldList, NewList),
	retract(inventory(_)),
	asserta(inventory(NewList)),
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

execute(cut, 'rope'):-
	location(jail),
	jail_assets(JailList),
	list_check('rope', JailList),
	list_remove('rope', JailList, NewList),
	retract(jail_assets(_)),
	asserta(jail_assets(NewList)),
	speak(['Blackbeard - "Arrr! I''m free! I have to get back to the deck and reclaim my ship! Arrr!"']), nl, !.
execute(unlock, chest):-
	location(corridor),
	inventory(InventoryList),
	list_check('chest key', InventoryList),
	retract(container(chest, locked)),
	asserta(container(chest, unlocked)),
	speak(['Blackbeard - "Arrr! The chest has opened!"']), nl, !.
execute(dress, _):-
	clothing(ClothingList),
	\+ list_is_empty(ClothingList),
	speak(['Blackbeard - "Arrr! I can not wear this over what I am already wearing!"']), nl, !.
execute(dress, Clothing):-
	wearable(Clothing),
	location(CurrentLocation),
	position(object(Clothing, light), CurrentLocation),
	clothing(ClothingList),
	list_is_empty(ClothingList),
	list_add(Clothing, ClothingList, NewList),
	retract(clothing(_)),
	asserta(clothing(NewList)),
	retract(position(object(Clothing, light), CurrentLocation)),
	speak(['Blackbeard - "Arrr! That ', Clothing, ' is tight!"']), nl, !.
execute(undress, Clothing):-
	location(CurrentLocation),
	clothing(ClothingList),
	list_check(Clothing, ClothingList),
	list_remove(Clothing, ClothingList, NewList),
	retract(clothing(_)),
	asserta(clothing(NewList)),
	asserta(position(object(Clothing, light), CurrentLocation)),
	speak(['Blackbeard - "Arrr! Now I am shy!"']), nl, !.
execute(unlock, 'forbidden room'):-
	location('heavy metal dark room'),
	inventory(InventoryList),
	list_check('forbidden room key', InventoryList),
	unlock_door('heavy metal dark room', 'forbidden room'),
	speak(['Blackbeard - "Arrr! The door is now unlocked!"']), nl, !.
execute(break, exit):-
	location(lounge),
	position(character(pirate, dead), lounge),
	inventory(InventoryList),
	list_check('axe', InventoryList),
	unlock_door(lounge, exit),
	speak(['Blackbeard - "Arrr! The door is now broken! Time to get my revenge! Let''s go up!"']), nl, !.
execute(break, exit):-
	location(lounge),
	position(character(pirate, alive), lounge),
	inventory(InventoryList),
	list_check('axe', InventoryList),
	unlock_door(lounge, exit),
	speak(['Blackbeard - "Arrr! The door is now broken! Time to get my revenge! Let''s go up!"']),
	speak(['Pianist - "Not so fast! * crash *!"']), nl, !.
execute(kill, pirate):-
	location(CurrentLocation),
	position(character(pirate, alive), CurrentLocation),
	inventory(InventoryList),
	(list_check('rum bottle', InventoryList); list_check('loaded gun', InventoryList); list_check('cutlass', InventoryList); list_check('axe', InventoryList)),
	retract(position(character(pirate, alive), CurrentLocation)),
	asserta(position(character(pirate, dead), CurrentLocation)),
	speak(['Blackbeard - "Arrr! Diiiiiiiiiiie!"']), nl, !.
execute(bribe, pirate):-
	location(CurrentLocation),
	position(character(pirate, alive), CurrentLocation),
	inventory(InventoryList),
	list_check('gold', InventoryList),
	list_remove('gold', InventoryList, NewList),
	retract(inventory(_)),
	asserta(inventory(NewList)),
	retract(position(character(pirate, alive), CurrentLocation)),
	asserta(position(character(pirate, dead), CurrentLocation)),
	speak(['Blackbeard - "Arrr! Take this gold and get lost!"']), nl, !.
execute(light, candle):-
	inventory(InventoryList),
	list_check(candle, InventoryList),
	list_check(matches, InventoryList),
	list_remove(candle, InventoryList, NewList),
	list_remove(matches, NewList, FinalList),
	list_add('lit candle', FinalList, UpdatedList),
	retract(inventory(_)),
	asserta(inventory(UpdatedList)),
	speak(['Blackbeard - "Arrr! The candle is now lit! Maybe I can see in dark rooms now!"']), nl, !.
execute(load, gun):-
	inventory(InventoryList),
	list_check(gun, InventoryList),
	list_check('gun powder', InventoryList),
	list_check(bullets, InventoryList),
	list_remove(gun, InventoryList, NewList),
	list_remove('gun powder', NewList, NewNewList),
	list_remove(bullets, NewNewList, FinalList),
	list_add('loaded gun', FinalList, UpdatedList),
	retract(inventory(_)),
	asserta(inventory(UpdatedList)),
	speak(['Blackbeard - "Arrr! The gun is now loaded! Let the killing begin!"']), nl, !.
execute(eat, Edible):-
	edible(Edible),
	inventory(InventoryList),
	list_check(Edible, InventoryList),
	list_remove(Edible, InventoryList, UpdatedList),
	retract(inventory(_)),
	asserta(inventory(UpdatedList)),
	speak(['Blackbeard - "Arrr! Yuuum! I really needed that!!"']), nl, !.
execute(Action, Object):-
	speak(['Blackbeard - "Arrr! I can not ', Action, ' the ', Object, '!"']), nl.


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
list_check(Name, List):-
	list_check0(Name, List).

list_check0(Name, [Name|_]).
list_check0(Name, [_|T]):- list_check0(Name, T).

list_add(Name, OldList, NewList):-
	NewList = [Name|OldList].

list_remove(Name, OldList, NewList):-
	list_check(Name, OldList),
	list_remove0(Name, OldList, NewList).

list_remove0(Name, [Name|Rest], Rest).
list_remove0(Name, [H|Rest], [H|T]):-
	list_remove0(Name, Rest, T).

list_is_empty([]).

% Misc
add_objects_to_location(Container):-
	location(CurrentLocation),
	position(object(Object, light), Container),
	asserta(position(object(Object, light), CurrentLocation)), 
	retract(position(object(Object, light), Container)), fail.
add_objects_to_location(_).

unlock_door(Room1,Room2):-
	retract(door(Room1, Room2, locked)),
	retract(door(Room2, Room1, locked)),
	asserta(door(Room1, Room2, unlocked)),
	asserta(door(Room2, Room1, unlocked)).
