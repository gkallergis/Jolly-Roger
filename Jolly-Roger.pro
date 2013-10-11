% Jolly Roger

% Logicbae
% Deck rooms
room(jail, 'Try cutting the rope with the sharp edge on the wall! If you have done so already, try moving on!').
room(corridor, 'Unlock the chest if you have the key (check at another room maybe?)').
room('death room', 'A pirate skeleton can scare someone, right?').
room(tavern, 'You can eat some fruits! Vitamins are good for you!').
room('pirate museum', 'If you wear a pirate outfit you might look like the others! Interesting!').
room('forbidden room', 'A key is in here, right? I wonder!').
room('weapon room', 'Maybe you should protect yourself, you never know what you might find in your way!').
room('captain room', 'Lots of stuff in here, but few of real value!').
room('lounge', 'Pirates are greedy for money! Although you are also heavyly armed! Hmmm!').
room('heavy metal dark room', 'Enjoy the music!').
room('exit', 'Just go up! Time to reclaim your ship! Wait for Jolly Roger 2!').

% Door connections
:- dynamic(door/4).

door(jail, corridor, unlocked, 'jail key').
door(corridor, 'death room', unlocked, 'death room key').
door('death room', tavern, unlocked, 'tavern key').
door(tavern, 'pirate museum', unlocked, 'pirate museum key').
door(tavern, 'heavy metal dark room', unlocked, 'heavy metal dark room key').
door(tavern, 'weapon room', unlocked, 'weapon room key').
door('weapon room', 'captain room', unlocked, 'captin room key').
door('captain room', lounge, unlocked, 'lounge key').
door('pirate museum', 'heavy metal dark room', unlocked, 'heavy metal dark room key').
door('heavy metal dark room', 'forbidden room', locked, 'forbidden room key').
door('heavy metal dark room', lounge, unlocked, 'lounge key').
door(lounge, exit, locked, '').

:- forall(door(X,Y,Z,W), assert(door(Y,X,Z,W))).

% Objects & Characters
:- dynamic(position/2).

position(character(pirate, 'deck boy', alive), tavern).
position(character(lion, guard, alive), 'forbidden room').
position(character(pirate, pianist, alive), lounge).

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
position(object('hand rope', light), captain).

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

cutable('hand rope').

scareable('deck boy').

killable(pianist).
killable('deck boy').
killable(guard).

animal(lion).

distracted(pianist).


:- dynamic(container/3).
container('fruit container', unlocked, 'fruit container key').
container(chest, locked, 'chest key').
container('armor box', unlocked, 'armor box key').

dark('heavy metal dark room').

% Character info
:- dynamic(location/1).
location('heavy metal dark room').

:- dynamic(inventory/1).
inventory(['chest key', 'forbidden room key']).

:- dynamic(clothing/1).
clothing([]).

% Game start
play:-
	write('Jolly Roger - A Pirate''s Story'),nl,
	write('Course Project by Georgios Kallergis (geokal@kth.se) & Thomas Fattal (tfattal@kth.se)'),nl,
	write('Released under the creative commons Attribution-ShareAlike 3.0 Unported license'),nl,
	nl,
	write('Captain Blackbeard was roaming the seas of the Caribean.'),nl,
	write('Being the richest pirate the surface of creation has ever seen,'),nl,
	write('he became arrogant and treated his crew in a bad way.'),nl,
	nl,
	write('One night, the crew turned against him, captured him and'),nl,
	write('locked him into the jail on the ship cellar. Everyday'),nl,
	write('they bring food to their old captain but he can not go outside'),nl,
	write('anymore.'),nl,
	nl,
	write('Ever since he was captured he was planning his revenge.'),nl,
	write('The day that he will rule the seas will come again and'),nl,
	write('everyone will pay for betraying him. Captain Blackbeard is'),nl,
	write('you. Today, the deck boy that braught you food forgot to lock the'),nl,
	write('jail door. This is your chance! Will you make it to the deck'),nl,
	write('and reclaim your ship? '),nl,
	nl,
	write('(Use natural language to execute actions in the game.)'),nl,nl,
	loop.

% Game loop and command calling
loop:-
	repeat,
	prompt_input(InputList),
	parse_command(InputList, Command),
	execute(Command),
	finished(InputList), !.

execute(end):- !.
execute(quit):- !.
execute(avast):- !.
execute(Command):- call(Command), !.

prompt_input(InputList):-
	ansi_format([bold, fg(green)], '[~w@ship]# ', ['Jolly-Roger']),
	read_line_to_codes(user_input, UserSentenceASCIICodes),
	remove_special_chars(UserSentenceASCIICodes, FilteredList),
	string_to_atom(FilteredList, Atom),
	atomic_list_concat(InputList,' ',Atom), !.

parse_command(InputList, Command):-
	nlp_command(InputList, OutputList),
	Command =.. OutputList, !.

finished(InputList):-
	[I|_] = InputList,
	(I == end; I == quit; I == avast), !.

remove_special_chars(ASCIIList, FilteredList):-
	remove_special_chars(ASCIIList, [], RevFilteredList), !,
	list_reverse(RevFilteredList, FilteredList).

remove_special_chars([], Acc, Acc):- !.
remove_special_chars([H|T], FilteredList, Acc):-
	(H >= 0'a, H =< 0'z; H == 32),
	remove_special_chars(T, [H|FilteredList], Acc).
remove_special_chars([H|T], FilteredList, Acc):-
	H >= 0'A, H =< 0'Z,
	N is H + 32,
	remove_special_chars(T, [N|FilteredList], Acc).
remove_special_chars([_|T], FilteredList, Acc):-
	remove_special_chars(T, FilteredList, Acc).

% NLP
nlp_command(InputList, OutputList):-
	OutputList = InputList.

% Gameplay!
% Moving around
move(Container):-
	container(Container, _, _),
	speak(['Are you out of your mind?! What are you going to do in the ', Container, '?!']), !, fail.
move(Room):-
	room_exists(Room),
	location(CurrentLocation), !,
	door_unlocked(CurrentLocation, Room), !,
	room_entrance_condition(Room),
	transit(Room, CurrentLocation).

transit(Room, CurrentLocation):-
	room_entry_action(Room),
	retract(location(CurrentLocation)),
	asserta(location(Room)),
	speak(['You walked into the ', Room, '!']),
	look_around, !.

room_entry_action(tavern):-
	position(character(pirate, 'deck boy', alive), tavern),
	retract(position(character(pirate, 'deck boy', alive), tavern)),
	asserta(position(character(pirate, 'deck boy', dead), tavern)),
	speak(['Blackbeard - "Arrr!"']),
	speak(['Pirate - "Aaaaaaa...*splash*!"']),
	speak(['Blackbeard - "Oh, that guy was scared!"']), nl.
room_entry_action('heavy metal dark room'):-
	speak(['Blackbeard - "Arrr!! Running Wild concert!! Aye aye!! http://youtu.be/9Q91-999gEM "']), nl.
room_entry_action('forbidden room'):-
	position(character(lion, guard, alive), 'forbidden room'),
	retract(position(character(lion, guard, alive), 'forbidden room')),
	asserta(position(character(lion, guard, dead), 'forbidden room')),
	speak(['* Bang Bang *']),
	speak(['Lion - Roooooooar...* dap *!']),
	speak(['Blackbeard - "Arrr!! That was messy!"']), nl.
room_entry_action(lounge):-
	position(character(pirate, pianist, alive), lounge),
	speak(['Pianist pirate singing']),
	speak(['"Rising the flag on the masthead']),
	speak(['The sails and the ropes'' holding tight']),
	speak(['The gunners are eager to fire']),
	speak(['Well prepared for the fight"']),
	speak(['Blackbeard - "Arrrr!! A piano player! He is too distracted, but I''d better keep an eye on him though..."']), nl.
room_entry_action(exit):-
	speak(['Blackbeard - "Arrrrrrrrrrrrrrrrrrr!!"']), nl.
room_entry_action(_).

room_exists(Room):-
	room(Room, _).
room_exists(_):-
	speak(['There is no such room!']), fail.

door_unlocked(CurrentLocation, Room):-
	door(CurrentLocation, Room, unlocked, _).
door_unlocked(CurrentLocation, Room):-
	door(CurrentLocation, Room, locked, _),
	speak(['The door is locked, maybe you can use something to unlock it!']), !, fail.
door_unlocked(CurrentLocation, CurrentLocation):-
	speak(['You are already there!']), !, fail.
door_unlocked(CurrentLocation, Room):-
	speak(['You can not get to the ', Room, ' from the ', CurrentLocation, '!']), fail.

room_entrance_condition(_):-
	inventory(InventoryList),
	list_check('hand rope', InventoryList),
	speak(['Blackbeard - "I think I can''t do much with my hands tied, can I?!"']), !, fail.
room_entrance_condition(Room):-
	position(character(Character, Role, alive), Room),
	animal(Character),
	\+ equiped(Role),
	speak(['Blackbeard - "Arrr!! There is a ', Character, ' guarding that room, I need to get rid of it somehow!"']), !, fail.
room_entrance_condition(Room):-
	position(character(_, Role, alive), Room),
	scareable(Role),
	clothing(ClothingList),
	\+ list_check('pirate skeleton', ClothingList),
	speak(['Blackbeard - "I think there is someone in there! I better not go in unprepared!"']), !, fail.
room_entrance_condition(Room):-
	position(character(_, Role, alive), Room),
	killable(Role),
	not(distracted(Role)),
	\+ equiped(Role),
	speak(['Blackbeard - "I think someone is in there! I better not go in unprepared!"']), !, fail.
room_entrance_condition(Room):-
	position(character(_, Role, alive), Room),
	killable(Role),
	distracted(Role),
	\+ equiped(Role),
	speak(['Blackbeard - "Arrr!! The pianist will get me if I go in there looking like that!"']), !, fail.
room_entrance_condition(Room):-
	dark(Room),
	inventory(InventoryList),
	\+ list_check('lit candle', InventoryList),
	speak(['Blackbeard - "It''s too dark in there! I can''t see a thing!"']), !, fail.
room_entrance_condition(_).

equiped(pianist):-
	clothing(ClothingList),
	list_check('pirate clothes', ClothingList).
equiped(guard):-
	inventory(InventoryList),
	list_check('loaded gun', InventoryList).
equiped('deck boy'):-
	clothing(ClothingList),
	list_check('pirate skeleton', ClothingList).

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
get(Name):-
	location(CurrentLocation),
	position(object(Name, light), CurrentLocation),
	retract(position(object(Name, _), CurrentLocation)),
	inventory(OldList),
	list_add(Name, OldList, NewList),
	retract(inventory(_)),
	asserta(inventory(NewList)), !.
get(Name):-
	location(CurrentLocation),
	position(object(Name, heavy), CurrentLocation),
	speak(['The object is too heavy to pick!']), !.
get(_):-
	speak(['There is no such object in here!']).

put(Name):-
	location(CurrentLocation),
	inventory(OldList),
	list_remove(Name, OldList, NewList),
	retract(inventory(_)),
	asserta(inventory(NewList)),
	asserta(position(object(Name, light), CurrentLocation)), !.
put(_):-
	speak(['There is no such object in your inventory!']).

inspect(Container):-
	location(CurrentLocation),
	position(object(Container, _), CurrentLocation),
	container(Container, unlocked, _),
	position(object(_, _), Container),
	speak(['In the ', Container, ' you can find:']),
	print_items(Container),
	add_objects_to_location(Container), !.
inspect(Container):-
	location(CurrentLocation),
	position(object(Container, _), CurrentLocation),
	container(Container, unlocked, _),
	\+ position(object(_, _), Container),
	speak(['The ', Container, ' is now empty!']), !.
inspect(Container):-
	location(CurrentLocation),
	position(object(Container, _), CurrentLocation),
	container(Container, locked, _),
	speak(['The ', Container, ' is locked, you can not look inside of it!']), !.
inspect(Name):-
	location(CurrentLocation),
	position(object(Name, _), CurrentLocation),
	\+ container(Name, _, _),
	speak(['There is nothing to inspect on ', Name, '! It is just a ', Name, '!']), !.
inspect(_):-
	speak(['There is no such object in this room!']).

cut(Object):-
	cutable(Object),
	inventory(InventoryList),
	list_check(Object, InventoryList),
	list_remove(Object, InventoryList, NewInvList),
	retract(inventory(_)),
	asserta(inventory(NewInvList)),
	speak(['Blackbeard - "Arrr! I just cut my ', Object, '!"']), nl, !.
cut(Object):-
	speak(['Blackbeard - "Arrr! I can not cut the ', Object, '!"']), nl.

unlock(Container):-
	container(Container, locked, Key),
	location(CurrentLocation),
	position(object(Container, _), CurrentLocation),
	inventory(InventoryList),
	list_check(Key, InventoryList),
	retract(container(Container, locked, Key)),
	asserta(container(Container, unlocked, Key)),
	speak(['Blackbeard - "Arrr! The ', Container, ' has opened!"']), nl, !.
unlock(Door):-
	location(CurrentLocation),
	door(Door, CurrentLocation, locked, Key),
	inventory(InventoryList),
	list_check(Key, InventoryList),
	unlock_door(Door, CurrentLocation),
	speak(['Blackbeard - "Arrr! The door is now unlocked!"']), nl, !.
unlock(Thing):-
	speak(['Blackbeard - "Arrr! I can not unlock the ', Thing, '! Are you sure you are next to a locked door?!"']), nl.

dress(Clothing):-
	wearable(Clothing),
	clothing(ClothingList),
	\+ list_is_empty(ClothingList),
	speak(['Blackbeard - "Arrr! I can not wear this over what I am already wearing!"']), nl, !.
dress(Clothing):-
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
dress(Clothing):-
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
dress(Object):-
	speak(['Blackbeard - "Arrr! I can not dress myself with the ', Object, '!"']), nl.

undress(Clothing):-
	location(CurrentLocation),
	clothing(ClothingList),
	list_check(Clothing, ClothingList),
	list_remove(Clothing, ClothingList, NewList),
	retract(clothing(_)),
	asserta(clothing(NewList)),
	asserta(position(object(Clothing, light), CurrentLocation)),
	speak(['Blackbeard - "Arrr! Now I am shy!"']), nl, !.
undress(Object):-
	speak(['Blackbeard - "Arrr! I can not get this away from me! I am not wearing a ', Object, '!"']), nl.

break(exit):-
	location(lounge),
	position(character(pirate, pianist, dead), lounge),
	inventory(InventoryList),
	list_check('axe', InventoryList),
	unlock_door(lounge, exit),
	speak(['Blackbeard - "Arrr! The door is now broken! Time to get my revenge! Let''s go up!"']), nl, !.
break(exit):-
	location(lounge),
	position(character(pirate, pianist, alive), lounge),
	inventory(InventoryList),
	list_check('axe', InventoryList),
	unlock_door(lounge, exit),
	speak(['Blackbeard - "Arrr! The door is now broken! Time to get my revenge! Let''s go up!"']),
	speak(['Pianist - "Not so fast! * crash *!"']), nl, !.
break(Object):-
	speak(['Blackbeard - "Arrr! I can not break the ', Object, '!"']), nl.

kill(pirate):-
	location(CurrentLocation),
	position(character(pirate, Role, alive), CurrentLocation),
	inventory(InventoryList),
	(list_check('rum bottle', InventoryList); list_check('loaded gun', InventoryList); list_check('cutlass', InventoryList); list_check('axe', InventoryList)),
	retract(position(character(pirate, Role, alive), CurrentLocation)),
	asserta(position(character(pirate, Role, dead), CurrentLocation)),
	speak(['Blackbeard - "Arrr! Diiiiiiiiiiie!"']), nl, !.
kill(Object):-
	speak(['Blackbeard - "Arrr! I can not kill the ', Object, '!"']), nl.


bribe(pirate):-
	location(CurrentLocation),
	position(character(pirate, Role, alive), CurrentLocation),
	inventory(InventoryList),
	list_check('gold', InventoryList),
	list_remove('gold', InventoryList, NewList),
	retract(inventory(_)),
	asserta(inventory(NewList)),
	retract(position(character(pirate, Role, alive), CurrentLocation)),
	asserta(position(character(pirate, Role, dead), CurrentLocation)),
	speak(['Blackbeard - "Arrr! Take this gold and get lost!"']), nl, !.
bribe(Object):-
	speak(['Blackbeard - "Arrr! I can not bribe the ', Object, '!"']), nl.

light(candle):-
	inventory(InventoryList),
	combination('lit candle', ItemsList),
	combine(InventoryList, ItemsList, UpdatedList),	
	retract(inventory(_)),
	asserta(inventory(UpdatedList)),
	speak(['Blackbeard - "Arrr! The candle is now lit! Maybe I can see in dark rooms now!"']), nl, !.
light(Object):-
	speak(['Blackbeard - "Arrr! I can not light the ', Object, '!"']), nl.

load(gun):-
	inventory(InventoryList),
	combination('loaded gun', ItemsList),
	combine(InventoryList, ItemsList, UpdatedList),
	retract(inventory(_)),
	asserta(inventory(UpdatedList)),
	speak(['Blackbeard - "Arrr! The gun is now loaded! Let the killing begin!"']), nl, !.
load(Object):-
	speak(['Blackbeard - "Arrr! I can not load the ', Object, '!"']), nl.

eat(Edible):-
	edible(Edible),
	inventory(InventoryList),
	list_check(Edible, InventoryList),
	list_remove(Edible, InventoryList, UpdatedList),
	retract(inventory(_)),
	asserta(inventory(UpdatedList)),
	speak(['Blackbeard - "Arrr! Yuuum! I really needed that!!"']), nl, !.
eat(Object):-
	speak(['Blackbeard - "Arrr! I can not eat the ', Object, '!"']), nl.

% Cheats!!
find(Room):-
	location(CurrentRoom),
	room(Room, _),
	speak(['Jolly Roger - To go to the ', Room, ' you can follow the following path(s):']),
	print_available_paths(CurrentRoom, Room), !.
find(Object):-
	position(object(Object, _), _),
	speak(['Jolly Roger - The ', Object, ' is at the following room(s): ']),
	print_rooms(Object), !.
find(Thing):-
	speak(['Jolly Roger - ', Thing, '?! Maybe another game?!']).

combo(Object):-
	position(object(Object, _), _),
	item_combo(_, Object),
	speak(['Jolly Roger - You can do the following things with the ', Object, ':']),
	print_item_combo(Object), !.
combo(Object):-
	position(object(Object, _), _),
	speak(['Jolly Roger - You can not really combine this with anything! Maybe use it alone? Or not at all?!']), !.
combo(_):-
	speak(['Jolly Roger - That object does not exist here! Maybe another game?!']).

hint:-
	location(CurrentLocation),
	room(CurrentLocation, Hint),
	speak(['Jolly Roger - You are in the ', CurrentLocation, '! ', Hint]).

help:-
	speak(['Jolly Roger - Help will go here!']).


% Auxiliary methods
% User output
speak([]):- nl.
speak([H|T]):- write(H), speak(T).

print_items(Room):-
	position(object(Name, Weight), Room), tab(4), speak([Name, ' (', Weight, ')']), fail.
print_items(_).

print_adjacent_rooms(Room):-
	door(Room, AdjacentRoom, Status, _), tab(4), speak([AdjacentRoom, ' (Door is ', Status, ')']), fail.
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
	\+ container(Room, _, _),
	write('    '), write(Room), write(' ('), write(Size), write(')'), nl, fail.
print_rooms(Object):-
	position(object(Object, Size), Container),
	position(object(Container, _), Room),
	write('    '), write(Container), write(' which is in the '), write(Room), write(' ('), write(Size), write(')'), nl, fail.
print_rooms(_).

print_item_combo(Object):-
	item_combo(Combination, Object),
	[Combo|_] = Combination,
	combination(Combo, CombinationList),
	list_remove(Object, CombinationList, RemainingObjects),
	write('    '), write(Combo), write(' (you also need: '), write(RemainingObjects), write(')'), nl, fail.
print_item_combo(_).

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
	retract(door(Room1, Room2, locked, Key1)),
	retract(door(Room2, Room1, locked, Key2)),
	asserta(door(Room1, Room2, unlocked, Key1)),
	asserta(door(Room2, Room1, unlocked, Key2)).

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

item_combo(Combination, Item):-
	length(Combination, 1),
	gen_combination(Combination),
	is_set(Combination),
	[Comb|_] = Combination,
	combination(Comb, CombinationList),
	list_check(Item, CombinationList).

possible_combo(Combination):-
	length(Combination, 1),
	gen_combination(Combination),
	is_set(Combination),
	[Comb|_] = Combination,
	combination(Comb, CombinationList),
	inventory(InventoryList),
	list_match(CombinationList, InventoryList).

gen_combination([]).
gen_combination([X|Xs]):-
    combination(X, _),
    gen_combination(Xs).
