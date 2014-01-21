-module(addressBook).
-record(name, { firstName = "", lastName = "" }).
-record(contact, { name = #name{}, phones = [], emails = [] }).

-export([createAddressBook/0, createAddressBook/1, addContact/3, addEmail/4, addPhone/4, removeContact/3, removeEmail/2, removePhone/2, getEmails/3, getPhones/3, findByEmail/2, findByPhone/2, csvExport/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

createAddressBook() -> [].

createAddressBook(Path) -> 
	{ok, IODevice} = file:open(Path, [read]),
	csvImport(IODevice, []).

addContact(FirstName, LastName, Book) ->
	Name = createName(FirstName, LastName),
	Contains = contains(#contact.name, Name, Book),
	
	if
		Contains == not_found -> [#contact{name = Name} | Book];
		true -> Book
	end.

addEmail(FirstName, LastName, Email, Book) ->
	Name = createName(FirstName, LastName),
	Contains = contains(#contact.emails, Email, Book),
	
	if
		Contains /= not_found -> Book;
		true -> updateRecord(#contact.emails, Name, Email, Book)
	end.

addPhone(FirstName, LastName, Phone, Book) ->
	Name = createName(FirstName, LastName),
	Contains = contains(#contact.phones, Phone, Book),
	
	if
		Contains /= not_found -> Book;
		true -> updateRecord(#contact.phones, Name, Phone, Book)
	end.

removeContact(FirstName, LastName, Book) ->
	Contact = contains(#contact.name, createName(FirstName, LastName), Book),
	if
		Contact == not_found -> Book;
		true ->	lists:delete(Contact, Book)
	end.

removeEmail(Email, Book) ->
	Contact = contains(#contact.emails, Email, Book),

	if
		Contact == not_found -> Book;
		true -> removeElement(#contact.emails, Email, Contact, Book)
	end.
	
removePhone(Phone, Book) ->
	Contact = contains(#contact.phones, Phone, Book),
	if
		Contact == not_found -> Book;
		true -> removeElement(#contact.phones, Phone, Contact, Book)
	end.
	
%% Dodaje do ksi¹¿ki adresowej Book nowy element o nazwie Name i polu typu Type o wartoœci Value b¹dŸ je auktualnia
updateRecord(Type, Name, Value, Book) ->
	Contact = contains(#contact.name, Name, Book),
	if
		Contact == not_found -> [setelement(Type, #contact{name = Name}, [Value]) | Book];
		true -> Updated = setelement(Type, Contact, [Value | element(Type, Contact)]),
			[Updated | lists:delete(Contact, Book)]
	end.

getEmails(FirstName, LastName, Book) ->
	Element = contains(#contact.name, createName(FirstName, LastName), Book),
	if
		Element == not_found -> [];
		true -> Element#contact.emails
	end.

getPhones(FirstName, LastName, Book) ->
	Element = contains(#contact.name, createName(FirstName, LastName), Book),
	if
		Element == not_found -> [];
		true -> Element#contact.phones
	end.
	
findByEmail(Email, Book) ->
	Element = contains(#contact.emails, Email, Book),
	if
		Element == not_found -> "";
		true -> Element#contact.name
	end.
	
findByPhone(Phone, Book) ->
	Element = contains(#contact.phones, Phone, Book),
	if
		Element == not_found -> "";
		true -> Element#contact.name
	end.
	
csvExport(Path, Book) ->
	{ok, IODevice} = file:open(Path, [write]),
	writeLine(IODevice, Book).
	
writeLine(IODevice, []) -> file:close(IODevice);
writeLine(IODevice, [H | T]) ->
	file:write(IODevice, createRecordString(H)),
	io:nl(IODevice),
	writeLine(IODevice, T).

csvImport(IODevice, Book) ->
	Line = readLine(IODevice),
	Data = (if is_atom(Line) == true -> null; is_tuple(Line) == true -> null; true -> re:split(Line, "[;]", [{return,list}]) end),
	
	if
		Data == null -> Book;
		true ->
			[FirstName, LastName, Phones, Emails] = Data,
			PhoneList = (if Phones == [] -> []; true -> re:split(Phones, "[,]", [{return,list}]) end),
			EmailList = (if Emails == [] -> []; true -> re:split(Emails, "[,]", [{return,list}]) end),
			NewRecord = #contact{name = createName(FirstName, LastName), phones = PhoneList, emails = EmailList},
			NewBook = [ NewRecord | Book ],
			csvImport(IODevice, NewBook)
	end.
	
readLine(IODevice) ->
	case io:get_line(IODevice, "") of
		eof -> file:close(IODevice);
		Line -> string:substr(Line, 1, string:len(Line) - 1) % usuniecie znaku ~n
	end.
	
createRecordString(#contact{name = #name{firstName = FirstName, lastName = LastName}, phones = Phones, emails = Emails}) ->
	string:join([FirstName, LastName, string:join(Phones, ","), string:join(Emails, ",")], ";").
	
%% usuwa podany element z rekordu i uaktualnia ksi¹¿kê
removeElement(Type, Value, Record, Book) ->
	UpdatedList = lists:delete(Value, element(Type, Record)),
	Updated = setelement(Type, Record, UpdatedList),
	[Updated | lists:delete(Record, Book)].

%% tworzy rekord zawieraj¹cy imiê i nazwisko
createName(FirstName, LastName) ->
	#name{firstName = FirstName, lastName = LastName}.

%% Zwraca pierwszy element zawieraj¹cy dan¹ typu Type
contains(_, _, []) -> not_found;
contains(Type, Value, [H | T]) ->
	Current = element(Type, H),
	InList = is_list(Current) andalso lists:member(Value, Current),
	if
		InList orelse Current == Value -> H;
		true -> contains(Type, Value, T)
	end.
