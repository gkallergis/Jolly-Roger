% Jolly Roger

% Logicbase
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
door('weapon room', 'captain room', unlocked, 'captain room key').
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
position(object('pirate skeleton', light), 'death room').
position(object(candle, light), 'death room').
position(object(rum, light), tavern).
position(object('fruit container', heavy), tavern).
position(object(banana, light), 'fruit container').
position(object(apple, light), 'fruit container').
position(object(annanas, light), 'fruit container').
position(object('pirate clothes', light), 'pirate museum').
position(object('chest key', light), 'forbidden room').
position(object(gold, light), 'forbidden room').
position(object('armor box', heavy), 'weapon room').
position(object(gun, light), 'armor box').
position(object(bullets, light), 'armor box').
position(object('cannon balls', heavy), 'armor box').
position(object('gun powder', light), 'armor box').
position(object(cutlass, light), 'weapon room').
position(object(matches, light), 'captain room').
position(object('treasure map', light), 'captain room').
position(object('forbidden room key', light), 'captain room').
position(object(candle, light), 'captain room').
position(object(rope, light), captain).

% Combinations
combination('lit candle', [candle, matches]).
combination('loaded gun', [gun, bullets, 'gun powder']).

% Extra object/room facts
edible(banana).
edible(apple).
edible(annanas).
edible(rum).

wearable('pirate skeleton').
wearable('pirate clothes').

cutable(rope).

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

:- dynamic(game_over/1).
game_over(false).

% Character info
:- dynamic(location/1).
location(jail).

:- dynamic(inventory/1).
inventory(['rope']).

:- dynamic(clothing/1).
clothing([]).

% Game start
play:-
	write('Jolly Roger - A Pirate''s Story'),nl,
	write('Course Project by Georgios Kallergis (geokal@kth.se) & Thomas Fattal (tfattal@kth.se)'),nl,
	write('Released under the creative commons Attribution-ShareAlike 3.0 Unported license'),nl, nl,
	write('Captain Blackbeard was roaming the seas of the Caribean.'),nl,
	write('Being the richest pirate the surface of creation has ever seen,'),nl,
	write('he became arrogant and treated his crew in a bad way.'),nl, nl,
	write('One night, the crew turned against him, captured him and'),nl,
	write('locked him into the jail on the ship cellar. Everyday'),nl,
	write('they bring food to their old captain but he can not go outside'),nl,
	write('anymore.'),nl, nl,
	write('Ever since he was captured he was planning his revenge.'),nl,
	write('The day that he will rule the seas will come again and'),nl,
	write('everyone will pay for betraying him. Captain Blackbeard is'),nl,
	write('you. Today, the deck boy that braught you food forgot to lock the'),nl,
	write('jail door. This is your chance! Will you make it to the deck'),nl,
	write('and reclaim your ship? '),nl, nl,
	write('Type ''help'' for more information'), nl, nl,
	write('(Use natural language to execute actions in the game.)'),nl,nl,
	loop.

% Game loop and command calling
loop:-
	repeat,
	prompt_input(InputList),
	parse_command(InputList, OutputList),
	execute(OutputList),
	finished(OutputList), !.

% Execute a command unless the user requests to end the game
execute([end]):- !.
execute(OutputList):- 
	Command =.. OutputList,
	call(Command), !.

% Prompt for user input and create a list of the entered words
% removing any special characters used.
prompt_input(InputList):-
	ansi_format([bold, fg(green)], '[~w@ship]# ', ['Jolly-Roger']),
	read_line_to_codes(user_input, UserSentenceASCIICodes),
	remove_special_chars(UserSentenceASCIICodes, FilteredList),
	string_to_atom(FilteredList, Atom),
	atomic_list_concat(InputList,' ',Atom), !.

% Generate a list containing the predicate and the arguments we need
% to call as a result of the entered user command. The input list is
% the list of words which make up the user entered sentence.
parse_command(InputList, OutputList):-
	nlp_transformation(OutputList, InputList, []), !.
parse_command(_, _):-
	nl, speak(['Jolly Roger - "Dude, what are you talking about?!"']), nl, fail.

% Check if the game is over. The game can end if the user dies, if the user
% reaches the exit or if the user requests to terminate the game.
finished(_):-
	game_over(true),
	nl, ansi_format([bold, fg(red)], 'Game Over!', []), nl.
finished(_):-
	location(exit),
	nl, ansi_format([bold, fg(magenta)], 'Congratulations! See you in Jolly Roger 2!', []), nl.
finished(InputList):-
	[end|_] = InputList, !.

% Rule that removes the special characters by checking their ASCII
% code to be within a specific range. At the end we reverse the list
% to keep the original order of words before we parse it using NLP.
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

% NLP - This is the grammar of our game. The transformation will take in
% The list of words entered by the user and progressively identify them
% as verbs and nounphrases. The "return" value is the aforementioned list
% with the predicate name and arguments to execute.
nlp_transformation([Action, Object]) --> verb(Action), nounphrase(Object).
nlp_transformation([Action]) --> verb(Action).

verb(move) --> [go, to].
verb(move) --> [move, to].
verb(move) --> [move, into].
verb(move) --> [move, in, to].
verb(move) --> [move].
verb(look_around) --> [look].
verb(look_around) --> [look, around].
verb(show_inventory) --> [inventory].
verb(help) --> [help].
verb(hint) --> [hint].
verb(end) --> [end].
verb(end) --> [quit].
verb(end) --> [avast].
verb(get) --> [get].
verb(get) --> [pick, up].
verb(get) --> [take].
verb(put) --> [put].
verb(put) --> [put, down].
verb(inspect) --> [inspect].
verb(inspect) --> [look, in].
verb(inspect) --> [look, into].
verb(cut) --> [cut].
verb(unlock) --> [unlock].
verb(unlock) --> [open].
verb(dress) --> [dress, with].
verb(dress) --> [put, on].
verb(dress) --> [wear].
verb(undress) --> [undress].
verb(break) --> [break].
verb(break) --> [destroy].
verb(break) --> [tear, down].
verb(kill) --> [kill].
verb(bribe) --> [bribe].
verb(light) --> [light].
verb(load) --> [load].
verb(eat) --> [eat].
verb(find) --> [find].
verb(combo) --> [cheat].

nounphrase(Object) --> det, noun(Object).
nounphrase(Object) --> noun(Object).

det --> [this].
det --> [that].
det --> [the].
det --> [a].

% To avoid writing a rule for each noun in the game,
% we use this notation to include all rooms with just
% one rule. Unfortunatelly, this is not possible to do
% for double words or more.
noun(Room) --> [Room], {room(Room, _)}.
noun('death room') --> [death, room].
noun('pirate museum') --> [pirate, museum].
noun('pirate museum') --> [museum].
noun('forbidden room') --> [forbidden, room].
noun('weapon room') --> [weapon, room].
noun('captain room') --> [captain, room].
noun('heavy metal dark room') --> [heavy, metal, dark, room].
noun('heavy metal dark room') --> [heavy, room].
noun('heavy metal dark room') --> [metal, room].
noun('heavy metal dark room') --> [dark, room].

noun(Object) --> [Object], {position(character(Object, _, _), _)}.
noun(Object) --> [Object], {position(object(Object, _), _)}.
noun(Object) --> [Object], {inventory(InventoryList), list_check(Object, InventoryList)}.
noun('rum bottle') --> [rum, bottle].
noun('pirate skeleton') --> [pirate, skeleton].
noun('pirate skeleton') --> [skeleton].
noun('fruit container') --> [fruit, container].
noun('pirate clothes') --> [pirate, clothes].
noun('pirate clothes') --> [clothes].
noun('chest key') --> [chest, key].
noun('armor box') --> [armor, box].
noun('cannon balls') --> [cannon, balls].
noun('gun powder') --> [gun, powder].
noun('treasure map') --> [treasure, map].
noun('forbidden room key') --> [forbidden, room, key].
noun('lit candle') --> [lit, candle].
noun('loaded gun') --> [loaded, gun].


% Gameplay!
% Rule to move the user around the map of the game.
move(Container):-
	container(Container, _, _),
	nl, speak(['Are you out of your mind?! What are you going to do in the ', Container, '?!']), nl, !, fail.
move(Room):-
	room_exists(Room),
	location(CurrentLocation), !,
	door_unlocked(CurrentLocation, Room), !,
	room_entrance_condition(Room),
	transit(Room, CurrentLocation).

% Rule that performs the actual transition
% by updating the logic base
transit(Room, CurrentLocation):-
	room_entry_action(Room),
	retract(location(CurrentLocation)),
	asserta(location(Room)),
	nl, speak(['You walked into the ', Room, '!']),
	look_around, !.

% To avoid a Java like rule (with predicates that have atmos as arguments)
% in game logic rules, we only do that to specify the message to show. Another
% way would be to add this to the room fact as an extra argument but we decided
% to keep the facts only about the game facts and separate them from the presentation
% we want to use for the user (as an example of this though, we implemented that in 
% the hint rule below).
room_entry_action(tavern):-
	position(character(pirate, 'deck boy', alive), tavern),
	retract(position(character(pirate, 'deck boy', alive), tavern)),
	asserta(position(character(pirate, 'deck boy', dead), tavern)),
	nl, speak(['Blackbeard - "Arrr!"']),
	speak(['Pirate - "Aaaaaaa...*splash*!"']),
	speak(['Blackbeard - "Oh, that guy was scared!"']).
room_entry_action('heavy metal dark room'):-
	nl, speak(['Blackbeard - "Arrr!! Running Wild concert!! Aye aye!! http://youtu.be/9Q91-999gEM"']).
room_entry_action('forbidden room'):-
	position(character(lion, guard, alive), 'forbidden room'),
	retract(position(character(lion, guard, alive), 'forbidden room')),
	asserta(position(character(lion, guard, dead), 'forbidden room')),
	nl, speak(['* Bang Bang *']),
	speak(['Lion - Roooooooar...* dap *!']),
	speak(['Blackbeard - "Arrr!! That was messy!"']).
room_entry_action(lounge):-
	position(character(pirate, pianist, alive), lounge),
	nl, speak(['Pianist pirate singing']),
	speak(['"Rising the flag on the masthead']),
	speak(['The sails and the ropes'' holding tight']),
	speak(['The gunners are eager to fire']),
	speak(['Well prepared for the fight"']),
	nl, speak(['Blackbeard - "Arrrr!! A piano player! He is too distracted, but I''d better keep an eye on him though..."']).
room_entry_action(exit):-
	nl, speak(['Blackbeard - "Arrrrrrrrrrrrrrrrrrr!!"']), nl.
room_entry_action(_).

% Rule to check if a room exists in our logic base.
room_exists(Room):-
	room(Room, _).
room_exists(_):-
	nl, speak(['There is no such room!']), nl, fail.

% A rule to check if the door between two rooms is unlocked.
door_unlocked(CurrentLocation, Room):-
	door(CurrentLocation, Room, unlocked, _).
door_unlocked(CurrentLocation, Room):-
	door(CurrentLocation, Room, locked, _),
	nl, speak(['The door is locked, maybe you can use something to unlock it!']), nl, !, fail.
door_unlocked(CurrentLocation, CurrentLocation):-
	nl, speak(['You are already there!']), nl, !, fail.
door_unlocked(CurrentLocation, Room):-
	nl, speak(['You can not get to the ', Room, ' from the ', CurrentLocation, '!']), nl, fail.

% A condition to enter a room. Before entering a room we can mandate some
% conditions to be true. For example, the user might be required to posses
% a specific item before entering a room.
room_entrance_condition(_):-
	inventory(InventoryList),
	list_check(rope, InventoryList),
	nl, speak(['Blackbeard - "I think I can''t do much with my hands tied, can I?!"']), nl, !, fail.
room_entrance_condition(Room):-
	position(character(Character, Role, alive), Room),
	animal(Character),
	\+ equiped(Role),
	nl, speak(['Blackbeard - "Arrr!! There is a ', Character, ' guarding that room, I need to get rid of it somehow!"']), nl, !, fail.
room_entrance_condition(Room):-
	position(character(_, Role, alive), Room),
	scareable(Role),
	clothing(ClothingList),
	\+ list_check('pirate skeleton', ClothingList),
	nl, speak(['Blackbeard - "I think there is someone in there! I better not go in unprepared!"']), nl, !, fail.
room_entrance_condition(Room):-
	position(character(_, Role, alive), Room),
	killable(Role),
	not(distracted(Role)),
	\+ equiped(Role),
	nl, speak(['Blackbeard - "I think someone is in there! I better not go in unprepared!"']), nl, !, fail.
room_entrance_condition(Room):-
	position(character(_, Role, alive), Room),
	killable(Role),
	distracted(Role),
	\+ equiped(Role),
	nl, speak(['Blackbeard - "Arrr!! The pianist will get me if I go in there looking like that!"']), nl, !, fail.
room_entrance_condition(Room):-
	dark(Room),
	inventory(InventoryList),
	\+ list_check('lit candle', InventoryList),
	nl, speak(['Blackbeard - "It''s too dark in there! I can''t see a thing!"']), nl, !, fail.
room_entrance_condition(_).

% Rule to check if the user is equiped for a specific situtation.
equiped(pianist):-
	clothing(ClothingList),
	list_check('pirate clothes', ClothingList).
equiped(guard):-
	inventory(InventoryList),
	list_check('loaded gun', InventoryList).
equiped('deck boy'):-
	clothing(ClothingList),
	list_check('pirate skeleton', ClothingList).

% Rule to check the current room the user is in. A few utility
% rules are called in the chain to show the items in the room
% as well as the adjacent rooms to the current one.
look_around:-
	location(CurrentLocation),
	list_room_items(CurrentLocation),
	speak(['From here you can go to: ']),
	print_adjacent_rooms(CurrentLocation), nl.

% Rule to list the items inside of a room.
list_room_items(Room):-
	position(object(_, _), Room),
	nl, speak(['In the ', Room, ' you can find: ']),
	print_items(Room), !.
list_room_items(_):-
	nl, speak(['There are no items in this room!']), nl.


% Rule to get an object found in the current room.
% The object must be light enough to pick and also
% in the room we are currently in!
get(Object):-
	location(CurrentLocation),
	position(object(Object, light), CurrentLocation),
	retract(position(object(Object, _), CurrentLocation)),
	inventory(OldList),
	list_add(Object, OldList, NewList),
	retract(inventory(_)),
	asserta(inventory(NewList)),
	nl, speak(['The ', Object, ' is now in your inventory!']), nl, !.
get(Object):-
	location(CurrentLocation),
	position(object(Object, heavy), CurrentLocation),
	nl, speak(['The ', Object, ' is too heavy to be picked!']), nl, !.
get(_):-
	nl, speak(['There is no such object in here!']), nl.

% Rule to put an object down. The object can later be found in the
% same place it was left at.
put(Object):-
	location(CurrentLocation),
	inventory(OldList),
	list_remove(Object, OldList, NewList),
	retract(inventory(_)),
	asserta(inventory(NewList)),
	asserta(position(object(Object, light), CurrentLocation)),
	nl, speak(['You left the ', Object, ' in the ', CurrentLocation, '!']), nl, !.
put(_):-
	nl, speak(['There is no such object in your inventory!']), nl.

% Rule to inspect an element that can contain other elements.
% Containers can be locked so the different cases are checked
% to provide the different actions to take in each.
inspect(Container):-
	location(CurrentLocation),
	position(object(Container, _), CurrentLocation),
	container(Container, unlocked, _),
	position(object(_, _), Container),
	nl, speak(['In the ', Container, ' you can find:']),
	print_items(Container), nl,
	add_objects_to_location(Container), !.
inspect(Container):-
	location(CurrentLocation),
	position(object(Container, _), CurrentLocation),
	container(Container, unlocked, _),
	\+ position(object(_, _), Container),
	nl, speak(['The ', Container, ' is now empty!']), nl, !.
inspect(Container):-
	location(CurrentLocation),
	position(object(Container, _), CurrentLocation),
	container(Container, locked, _),
	nl, speak(['The ', Container, ' is locked, you can not look inside of it!']), nl, !.
inspect(Name):-
	location(CurrentLocation),
	position(object(Name, _), CurrentLocation),
	\+ container(Name, _, _),
	nl, speak(['There is nothing to inspect on ', Name, '! It is just a ', Name, '!']), nl, !.
inspect(_):-
	nl, speak(['There is no such object in this room!']), nl.

% Rule to cut an object. For example, the rope that holds the user's hands
% at the beggining of the game. The object is lost forever, once cut off.
cut(Object):-
	cutable(Object),
	inventory(InventoryList),
	list_check(Object, InventoryList),
	list_remove(Object, InventoryList, NewInvList),
	retract(inventory(_)),
	asserta(inventory(NewInvList)),
	nl, speak(['Blackbeard - "Arrr! I just cut my ', Object, '!"']), nl, !.
cut(Object):-
	nl, speak(['Blackbeard - "Arrr! I can not cut the ', Object, '!"']), nl.

% Rule to unlock lockable objects. A door or a chest can be such examples
% and the user needs a specific key to unlock them (specified in the logic
% base).
unlock(Container):-
	container(Container, locked, Key),
	location(CurrentLocation),
	position(object(Container, _), CurrentLocation),
	inventory(InventoryList),
	list_check(Key, InventoryList),
	retract(container(Container, locked, Key)),
	asserta(container(Container, unlocked, Key)),
	nl, speak(['Blackbeard - "Arrr! The ', Container, ' has opened!"']), nl, !.
unlock(Door):-
	location(CurrentLocation),
	door(Door, CurrentLocation, locked, Key),
	inventory(InventoryList),
	list_check(Key, InventoryList),
	unlock_door(Door, CurrentLocation),
	nl, speak(['Blackbeard - "Arrr! The door is now unlocked!"']), nl, !.
unlock(Thing):-
	nl, speak(['Blackbeard - "Arrr! I can not unlock the ', Thing, '! Are you sure you are next to a locked door?!"']), nl.

% Rule to dress the users with clothing found in the rooms of the game.
% Double clothing is not allowed (wearing clothes over other clothes).
dress(Clothing):-
	wearable(Clothing),
	clothing(ClothingList),
	\+ list_is_empty(ClothingList),
	nl, speak(['Blackbeard - "Arrr! I can not wear this over what I am already wearing!"']), nl, !.
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
	nl, speak(['Blackbeard - "Arrr! That ', Clothing, ' is tight!"']), nl, !.
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
	nl, speak(['Blackbeard - "Arrr! That ', Clothing, ' is tight!"']), nl, !.
dress(Object):-
	nl, speak(['Blackbeard - "Arrr! I can not dress myself with the ', Object, '!"']), nl.

% Rule to remove clothing from the user. The clothing removed
% cab be later found in the room were it was removed in.
undress(Clothing):-
	location(CurrentLocation),
	clothing(ClothingList),
	list_check(Clothing, ClothingList),
	list_remove(Clothing, ClothingList, NewList),
	retract(clothing(_)),
	asserta(clothing(NewList)),
	asserta(position(object(Clothing, light), CurrentLocation)),
	nl, speak(['Blackbeard - "Arrr! Now I am shy!"']), nl, !.
undress(Object):-
	nl, speak(['Blackbeard - "Arrr! I can not get this away from me! I am not wearing a ', Object, '!"']), nl.

% Rule to brake the exit. To do so, the user has to have a specific object (i.e. an axe).
break(exit):-
	location(lounge),
	position(character(pirate, pianist, dead), lounge),
	inventory(InventoryList),
	list_check('axe', InventoryList),
	unlock_door(lounge, exit),
	nl, speak(['Blackbeard - "Arrr! The door is now broken! Time to get my revenge! Let''s go up!"']), nl, !.
break(exit):-
	location(lounge),
	position(character(pirate, pianist, alive), lounge),
	inventory(InventoryList),
	list_check('axe', InventoryList),
	unlock_door(lounge, exit),
	retract(game_over(false)),
	asserta(game_over(true)),
	nl, speak(['Blackbeard - "Arrr! The door is now broken! Time to get my revenge! Let''s go up!"']),
	speak(['Pianist - "Not so fast! * crash *!"']), nl, !.
break(Object):-
	nl, speak(['Blackbeard - "Arrr! I can not break the ', Object, '!"']), nl.

% Rule to kill a pirate! The user has to be equiped to do so and the possible
% weapong that can be used are checked here for existence.
kill(pirate):-
	location(CurrentLocation),
	position(character(pirate, Role, alive), CurrentLocation),
	inventory(InventoryList),
	(list_check('rum bottle', InventoryList); list_check('loaded gun', InventoryList); list_check('cutlass', InventoryList); list_check('axe', InventoryList)),
	retract(position(character(pirate, Role, alive), CurrentLocation)),
	asserta(position(character(pirate, Role, dead), CurrentLocation)),
	nl, speak(['Blackbeard - "Arrr! Diiiiiiiiiiie!"']), nl, !.
kill(Object):-
	nl, speak(['Blackbeard - "Arrr! I can not kill the ', Object, '!"']), nl.

% Rule to bribe a pirate!
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
	nl, speak(['Blackbeard - "Arrr! Take this gold and get lost!"']), nl, !.
bribe(Object):-
	nl, speak(['Blackbeard - "Arrr! I can not bribe the ', Object, '!"']), nl.

% Rule to light a candle. This requires a combination of two items which must
% be in the possetion of the user.
light(candle):-
	inventory(InventoryList),
	combination('lit candle', ItemsList),
	combine(InventoryList, ItemsList, UpdatedList),	
	retract(inventory(_)),
	asserta(inventory(UpdatedList)),
	nl, speak(['Blackbeard - "Arrr! The candle is now lit! Maybe I can see in dark rooms now!"']), nl, !.
light(Object):-
	nl, speak(['Blackbeard - "Arrr! I can not light the ', Object, '!"']), nl.

% Rule to load a gun. This also requires a set of items to be found in the
% repository of the user.
load(gun):-
	inventory(InventoryList),
	combination('loaded gun', ItemsList),
	combine(InventoryList, ItemsList, UpdatedList),
	retract(inventory(_)),
	asserta(inventory(UpdatedList)),
	nl, speak(['Blackbeard - "Arrr! The gun is now loaded! Let the killing begin!"']), nl, !.
load(Object):-
	nl, speak(['Blackbeard - "Arrr! I can not load the ', Object, '!"']), nl.

% Rule to eat an edible object!
eat(Edible):-
	edible(Edible),
	inventory(InventoryList),
	list_check(Edible, InventoryList),
	list_remove(Edible, InventoryList, UpdatedList),
	retract(inventory(_)),
	asserta(inventory(UpdatedList)),
	nl, speak(['Blackbeard - "Arrr! Yuuum! I really needed that!!"']), nl, !.
eat(Object):-
	nl, speak(['Blackbeard - "Arrr! I can not eat the ', Object, '!"']), nl.

% Cheats!!
% Rule to find the possible paths from the current room to the requested room and
% objects in the game.
find(Room):-
	location(CurrentRoom),
	room(Room, _),
	nl, speak(['Jolly Roger - To go to the ', Room, ' you can follow the following path(s):']),
	print_available_paths(CurrentRoom, Room), nl, !.
find(Object):-
	position(object(Object, _), _),
	nl, speak(['Jolly Roger - The ', Object, ' is at the following room(s): ']),
	print_rooms(Object), nl, !.
find(Thing):-
	nl, speak(['Jolly Roger - ', Thing, '?! Maybe another game?!']), nl.

% Rule to check what an object can be used for. An object must be available in
% at least on combo for this rule to succeed.
combo(Object):-
	inventory(InventoryList),
	(position(object(Object, _), _); list_check(Object, InventoryList)),
	item_combo(_, Object),
	nl, speak(['Jolly Roger - You can do the following things with the ', Object, ':']),
	print_item_combo(Object), nl, !.
combo(Object):-
	position(object(Object, _), _),
	nl, speak(['Jolly Roger - You can not really combine this with anything! Maybe use it alone? Or not at all?!']), nl, !.
combo(_):-
	nl, speak(['Jolly Roger - That object does not exist here! Maybe another game?!']), nl.

% Rule to show the inventory contents.
show_inventory:-
	inventory(InventoryList),
	nl, speak(['In your inventory you have:']),
	print_inventory(InventoryList), nl.

% Rule to show a hint for the current room the user is in (as discussed above,
% this is just showing a message retrieved from the logic base).
hint:-
	location(CurrentLocation),
	room(CurrentLocation, Hint),
	nl, speak(['Jolly Roger - You are in the ', CurrentLocation, '! ', Hint]), nl.

% Rule to print a simple help message.
help:-
	nl, write('To play the game simple imperative sentences can be used.'),nl,
	write('For example, to go to the corridor room you can say one of the'),nl,
	write('following:'), nl,
	write('   move to corridor'), nl,
	write('   move to the corridor'), nl,
	write('   move into the corridor'), nl,
	write('   etc.'), nl, nl,
	write('To see the things in your inventory simply type inventory.'), nl,
	write('Use find or cheat to use the built in cheat engine.'), nl, nl.


% Auxiliary methods
% User output. Methods to pretty print lists
% and other information to the user.
speak([]):- nl.
speak([H|T]):- write(H), speak(T).

print_items(Room):-
	position(object(Name, Weight), Room), tab(4), speak([Name, ' (', Weight, ')']), fail.
print_items(_).

print_adjacent_rooms(Room):-
	door(Room, AdjacentRoom, Status, _), tab(4), speak([AdjacentRoom, ' (Door is ', Status, ')']), fail.
print_adjacent_rooms(_).

print_inventory([H|[]]):- write('    '), write(H), nl, !.
print_inventory([H|T]):-
	write('    '), write(H), nl,
	print_inventory(T).

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
	combination(Combination, CombinationList),
	list_remove(Object, CombinationList, RemainingObjects),
	write('    '), write(Combination), write(' (you also need: '), write(RemainingObjects), write(')'), nl, fail.
print_item_combo(_).

% List manipulation - Methods to manipulate lists in the game
% Rule to check if an item exists in a list.
list_check(Name, [Name|_]).
list_check(Name, [_|T]):- list_check(Name, T).

% Rule to check if all the elements of one list can be found in another list.
list_match([], _).
list_match([ItemsH|ItemsT], List):-
	list_check(ItemsH, List),
	list_match(ItemsT, List).

% Rule to prepend an element in a list.
list_add(Name, OldList, NewList):-
	NewList = [Name|OldList].

% Rule to remove a specific element from a list if it exists.
list_remove(Name, OldList, NewList):-
	list_check(Name, OldList),
	list_remove0(Name, OldList, NewList).

list_remove0(Name, [Name|Rest], Rest).
list_remove0(Name, [H|Rest], [H|T]):-
	list_remove0(Name, Rest, T).

% Rule to remove a list of elements from a list.
% If not all elements can be found in the second list
% only the ones found are removed.
list_remove_list([], NewList, NewList).
list_remove_list([ItemsH|ItemsT], List, UpdatedList):-
	list_remove(ItemsH, List, NewList),
	list_remove_list(ItemsT, NewList, UpdatedList).

% Trivial predicate to check for the empty list.
list_is_empty([]).

% Rule to reverse a list by using an accumulator.
list_reverse(List, NewList):- list_reverse(List, [], NewList).
list_reverse([], Acc, Acc).
list_reverse([H|T], Acc, NewList):- list_reverse(T, [H|Acc], NewList).

% Misc
% Rule to move objects from one place to another during the game.
add_objects_to_location(Container):-
	location(CurrentLocation),
	position(object(Object, light), Container),
	asserta(position(object(Object, light), CurrentLocation)), 
	retract(position(object(Object, light), Container)), fail.
add_objects_to_location(_).

% Rule to unlock a door.
unlock_door(Room1,Room2):-
	retract(door(Room1, Room2, locked, Key1)),
	retract(door(Room2, Room1, locked, Key2)),
	asserta(door(Room1, Room2, unlocked, Key1)),
	asserta(door(Room2, Room1, unlocked, Key2)).

% Rule to combine items in a list based on facts found in the
% logic base. The logic base acts as the recipie and the list
% is updated accordingly.
combine(InventoryList, ItemsList, UpdatedList):-
	list_match(ItemsList, InventoryList),
	list_remove_list(ItemsList, InventoryList, NewInvList),
	combination(CombinedObj, ItemsList),
	list_add(CombinedObj, NewInvList, UpdatedList).

% Rule to traverse a simple graph with memory to avoid circular paths.
% A posible circular path could be in our taver, pirate museum and 
% heavy metal dark room room arangement.
find_room_path(CurrentRoom, TargetRoom, FinalPath):-
	find_room_path0(CurrentRoom, TargetRoom, [CurrentRoom], Path),
	list_reverse(Path, FinalPath).

find_room_path0(TargetRoom, TargetRoom, Visited, Visited).
find_room_path0(CurrentRoom, TargetRoom, Visited, Path):-
	door(CurrentRoom, AdjacentRoom, _, _),
	\+ list_check(AdjacentRoom, Visited),
	find_room_path0(AdjacentRoom, TargetRoom, [AdjacentRoom|Visited], Path).

% Rule to check in what combinations an item can be used in.
item_combo(Combination, Item):-
	combination(Combination, CombinationList),
	list_check(Item, CombinationList).
