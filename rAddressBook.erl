-module(rAddressBook).
-export([start/0, stop/0, init/0]).
-export([addContact/2, addEmail/3, addPhone/3, removeContact/2, removeEmail/1, removePhone/1, getEmails/2, getPhones/2, findByEmail/1, findByPhone/1, csvExport/1]).
-export([crash/0]).

start() ->
	register(server, spawn(rAddressBook, init, [])).
	%% register(server, self()),
	%% init().
	
init() ->
	Book = addressBook:createAddressBook(),
	loop(Book).

loop(Book) ->
	receive
		stop -> terminate();
		{addContact, FirstName, LastName, Pid} -> 
			NewBook = addressBook:addContact(FirstName, LastName, Book), Pid ! ok, loop(NewBook);
		{addEmail, FirstName, LastName, Email, Pid} -> 
			NewBook = addressBook:addEmail(FirstName, LastName, Email, Book), Pid ! ok, loop(NewBook);
		{addPhone, FirstName, LastName, Email, Pid} -> 
			NewBook = addressBook:addPhone(FirstName, LastName, Email, Book), Pid ! ok, loop(NewBook);
		{removeContact, FirstName, LastName, Pid} -> 
			NewBook = addressBook:removeContact(FirstName, LastName, Book), Pid ! ok, loop(NewBook);
		{removeEmail, Email, Pid} -> 
			NewBook = addressBook:removeEmail(Email, Book), Pid ! ok, loop(NewBook);
		{removePhone, Phone, Pid} -> 
			NewBook = addressBook:removePhone(Phone, Book), Pid ! ok, loop(NewBook);
		{getEmails, FirstName, LastName, Pid} ->
			Emails = addressBook:getEmails(FirstName, LastName, Book), Pid ! Emails, loop(Book);
		{getPhones, FirstName, LastName, Pid} ->
			Phones = addressBook:getPhones(FirstName, LastName, Book), Pid ! Phones, loop(Book);
		{findByEmail, Email, Pid} ->
			Name = addressBook:findByEmail(Email, Book), Pid ! Name, loop(Book);
		{findByPhone, Phone, Pid} ->
			Name = addressBook:findByPhone(Phone, Book), Pid ! Name, loop(Book);
		{csvExport, Path, Pid} ->
			addressBook:csvExport(Path, Book), Pid ! ok, loop(Book);
		crash -> 8/0
	end.
	
terminate() -> ok.

stop() -> server ! stop.

addContact(FirstName, LastName) ->
	call({addContact, FirstName, LastName, self()}).
addEmail(FirstName, LastName, Email) ->
	call({addEmail, FirstName, LastName, Email, self()}).
addPhone(FirstName, LastName, Phone) ->
	call({addPhone, FirstName, LastName, Phone, self()}).
removeContact(FirstName, LastName) ->
	call({removeContact, FirstName, LastName, self()}).
removeEmail(Email) ->
	call({removeEmail, Email, self()}).
removePhone(Phone) ->
	call({removePhone, Phone, self()}).
getEmails(FirstName, LastName) ->
	call({getEmails, FirstName, LastName, self()}).
getPhones(FirstName, LastName) ->
	call({getPhones, FirstName, LastName, self()}).
findByEmail(Email) ->
	call({findByEmail, Email, self()}).
findByPhone(Phone) ->
	call({findByPhone, Phone, self()}).
csvExport(Path) -> call({csvExport, Path, self()}).
	
call(Signal) ->
	server ! Signal,
	
	receive
		Result -> Result
	end.

crash() -> server ! crash.

	


