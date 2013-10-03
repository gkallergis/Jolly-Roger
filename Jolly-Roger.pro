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

% Combinations
combination('lit candle', [candle, matches]).
combination('loaded gun', [gun, bullets, 'gun powder']).

% Extra object/room facts
edible(bannana).
edible(apple).
edible(annanas).
edible(rum).

wearable('pirate skeleton').
wearable('pirate clothes').

killable(pirate).
killable(lion).

action('sharp edge', cut, rope).
action(axe, break, door).
action(axe, kill, pirate).
action('pirate skeleton', scare, pirate).
action('chest key', unlock, chest).
action(cutlass, kill, pirate).
action('forbidden room key', unlock, 'forbidden room').
action('loaded gun', kill, pirate).
action('loaded gun', kill, lion).
action('rum bottle', kill, pirate).


:- dynamic(container/2).
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
	speak(['Blackbeard - "Arrr!! Running Wild concert!! Aye aye!! http://youtu.be/9Q91-999gEM "']), nl.
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
execute(dress, Clothing):-
	wearable(Clothing),
	clothing(ClothingList),
	\+ list_is_empty(ClothingList),
	speak(['Blackbeard - "Arrr! I can not wear this over what I am already wearing!"']), nl, !.
execute(dress, Clothing):-
	wearable(Clothing),
	inventory(InventoryList),
	list_check(Clothing, InventoryList),
	list_remove(Clothing, InventoryList, NewInvList),
	retract(inventory(_)),
	asserta(inventory(NewInvList)),
	clothing(ClothingList),
	list_is_empty(ClothingList),
	list_add(Clothing, ClothingList, NewClothingList),
	retract(clothing(_)),
	asserta(clothing(NewClothingList)), 
	speak(['Blackbeard - "Arrr! That ', Clothing, ' is tight!"']), nl, !.
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
	combination('lit candle', ItemsList),
	combine(InventoryList, ItemsList, UpdatedList),	
	retract(inventory(_)),
	asserta(inventory(UpdatedList)),
	speak(['Blackbeard - "Arrr! The candle is now lit! Maybe I can see in dark rooms now!"']), nl, !.
execute(load, gun):-
	inventory(InventoryList),
	combination('loaded gun', ItemsList),
	combine(InventoryList, ItemsList, UpdatedList),
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

cheat('find room', TargetRoom):-
	location(CurrentRoom),
	room(TargetRoom),
	speak(['Jolly Roger - To go to the ', TargetRoom, ' you can follow the following path(s):']),
	print_available_paths(CurrentRoom, TargetRoom), !.
cheat('find room', _):-
	speak(['Jolly Roger - There is no such room here! Maybe another game?!']).
cheat('find object', Object):-
	position(object(Object, _), _),
	speak(['Jolly Roger - The ', Object, ' is at the folloowing room(s): ']),
	print_rooms(Object), !.
cheat('find object', _):-
	speak(['Jolly Roger - There is no such object anywhere! Maybe another game?!']).
%cheat('find combo', Object):-
	
cheat('find combo', _):-
	speak(['Jolly Roger - You can not really do anything with that! It is just there to make things a bit harder for you!!']).


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

print_available_paths(CurrentRoom, TargetRoom):-
	find_room_path(CurrentRoom, TargetRoom, Path),
	print_path(Path), fail.
print_available_paths(_, _).

print_path([H|[]]):- write(H), nl, !.
print_path([H|T]):-
	write(H), write(' --> '),
	print_path(T).

print_rooms(Object):-
	position(object(Object, Size), Room),
	\+ container(Room, _),
	write('    '), write(Room), write(' ('), write(Size), write(')'), nl, fail.
print_rooms(Object):-
	position(object(Object, Size), Container),
	position(object(Container, _), Room),
	write('    '), write(Container), write(' which is in the '), write(Room), write(' ('), write(Size), write(')'), nl, fail.
print_rooms(_).

% List manipulation
list_check(Name, [Name|_]).
list_check(Name, [_|T]):- list_check(Name, T).

list_match([], _).
list_match([ItemsH|ItemsT], List):-
	list_check(ItemsH, List),
	list_match(ItemsT, List).

list_add(Name, OldList, NewList):-
	NewList = [Name|OldList].

list_remove(Name, OldList, NewList):-
	list_check(Name, OldList),
	list_remove0(Name, OldList, NewList).

list_remove0(Name, [Name|Rest], Rest).
list_remove0(Name, [H|Rest], [H|T]):-
	list_remove0(Name, Rest, T).

list_remove_list([], NewList, NewList).
list_remove_list([ItemsH|ItemsT], List, UpdatedList):-
	list_remove(ItemsH, List, NewList),
	list_remove_list(ItemsT, NewList, UpdatedList).

list_is_empty([]).

list_reverse(List, NewList):- list_reverse(List, [], NewList).
list_reverse([], Acc, Acc).
list_reverse([H|T], Acc, NewList):- list_reverse(T, [H|Acc], NewList).

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

combine(InventoryList, ItemsList, UpdatedList):-
	list_match(ItemsList, InventoryList),
	list_remove_list(ItemsList, InventoryList, NewInvList),
	combination(CombinedObj, ItemsList),
	list_add(CombinedObj, NewInvList, UpdatedList).

find_room_path(CurrentRoom, TargetRoom, FinalPath):-
	find_room_path0(CurrentRoom, TargetRoom, [CurrentRoom], Path),
	list_reverse(Path, FinalPath).

find_room_path0(TargetRoom, TargetRoom, Visited, Visited).
find_room_path0(CurrentRoom, TargetRoom, Visited, Path):-
	door(CurrentRoom, AdjacentRoom, _),
	\+ list_check(AdjacentRoom, Visited),
	find_room_path0(AdjacentRoom, TargetRoom, [AdjacentRoom|Visited], Path).
