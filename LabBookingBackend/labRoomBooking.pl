% Import the facts file
:- consult('roomBookedFacts.pl').

% Include room definitions
:- consult('roomDefinitions.pl').


:- dynamic booked/4. % room, date, time, person

% Dynamic predicate for room definitions
:- dynamic room/3.

% Define time ranges for each slot
morning('09:00', '12:00').
afternoon('13:00', '16:00').
evening('16:30', '19:30').

% Check if a time is in a given range
time_in_range(Time, Start, End) :-
    atom_number(Time, TimeVal),
    atom_number(Start, StartVal),
    atom_number(End, EndVal),
    TimeVal >= StartVal,
    TimeVal =< EndVal.

% Preferred time slot
preferred_time_slot(morning, Time) :-
    morning(Start, End),
    time_in_range(Time, Start, End).

preferred_time_slot(afternoon, Time) :-
    afternoon(Start, End),
    time_in_range(Time, Start, End).

preferred_time_slot(evening, Time) :-
    evening(Start, End),
    time_in_range(Time, Start, End).


% Check if the room is already booked
is_room_booked(Room, Date, Time) :-
    booked(Room, Date, Time, _). % Facts now include the person's name

% Book the lab room
book_lab_room(Room, Date, Month, Year, Time, PeopleCount, PreferredSlot, PersonName) :-
    format_date(Date, Month, Year, FullDate),
    room(Room, _, Capacity),
    PeopleCount =< Capacity,
    (   % If room is available, add the booking
        \+ is_room_booked(Room, FullDate, Time)
    ->  assertz(booked(Room, FullDate, Time, PersonName)), % Add booking to facts file
        save_booked_fact(Room, FullDate, Time, PersonName),
        write('Booking confirmed for '), write(Room), write(' on '),
        write(FullDate), write(' at '), write(Time), write(' by '),
        write(PersonName), nl
    ;   % Suggest alternatives
        suggest_alternative_room(Room, FullDate, Time, SuggestedRoom)
    ->  write('Requested room unavailable. Suggested room: '),
        write(SuggestedRoom), nl
    ;   suggest_alternative_date(Room, Time, PreferredSlot, AlternativeDate)
    ->  write('Requested room unavailable. Suggested alternative date: '),
        write(AlternativeDate), nl
    ).

% Save booking information to roomBookedFacts.pl
save_booked_fact(Room, FullDate, Time, PersonName) :-
    open('roomBookedFacts.pl', append, Stream),
    write(Stream, 'booked('), write(Stream, Room), write(Stream, ', '),
    write(Stream, '\''), write(Stream, FullDate), write(Stream, '\''), write(Stream, ', '),
    write(Stream, '\''), write(Stream, Time), write(Stream, '\''), write(Stream, ', '),
    write(Stream, '\''), write(Stream, PersonName), write(Stream, '\''), write(Stream, ').'), nl(Stream),
    close(Stream).

% Display all bookings
list_all_bookings :-
    findall((Room, Date, Time, Person), booked(Room, Date, Time, Person), Bookings),
    write('All Bookings:'), nl,
    print_bookings(Bookings).

% Helper to print bookings
print_bookings([]).
print_bookings([(Room, Date, Time, Person) | Rest]) :-
    write('Room: '), write(Room),
    write(', Date: '), write(Date),
    write(', Time: '), write(Time),
    write(', Booked by: '), write(Person), nl,
    print_bookings(Rest).

% Suggest alternative room
suggest_alternative_room(PreferredRoom, FullDate, Time, SuggestedRoom) :-
    room(SuggestedRoom, _, _),
    SuggestedRoom \= PreferredRoom,
    \+ is_room_booked(SuggestedRoom, FullDate, Time).

% Suggest alternative date (skipping weekends)
suggest_alternative_date(Room, Time, PreferredSlot, AlternativeDate) :-
    preferred_time_slot(PreferredSlot, Time),
    generate_alternative_date(AlternativeDate),
    \+ is_room_booked(Room, AlternativeDate, Time).

% Generate the next valid date (skipping weekends)
generate_alternative_date(AlternativeDate) :-
    get_time(Now),
    stamp_date_time(Now, date(Year, Month, Day, _, _, _, _, _, _), 'UTC'),
    NextDay is Day + 1,
    skip_weekend(NextDay, Month, Year, ValidDay, ValidMonth, ValidYear),
    format_date(ValidDay, ValidMonth, ValidYear, AlternativeDate).

% Skip weekends (Saturday = 6, Sunday = 7)
skip_weekend(Day, Month, Year, ValidDay, ValidMonth, ValidYear) :-
    date_time_stamp(date(Year, Month, Day, 0, 0, 0, 0, -, -), Timestamp),
    stamp_date_time(Timestamp, date(_, _, _, _, _, _, Weekday, _, _), 'UTC'),
    (   Weekday = 6
    ->  NextDay is Day + 2 % Skip Saturday
    ;   Weekday = 7
    ->  NextDay is Day + 1 % Skip Sunday
    ;   NextDay is Day
    ),
    adjust_date(NextDay, Month, Year, ValidDay, ValidMonth, ValidYear).

% Adjust date for month-end overflow
adjust_date(Day, Month, Year, ValidDay, ValidMonth, ValidYear) :-
    days_in_month(Month, Year, MaxDays),
    (   Day > MaxDays
    ->  NewDay is Day - MaxDays,
        NewMonth is Month + 1,
        adjust_year(NewMonth, Year, ValidYear),
        ValidMonth is NewMonth mod 12 + 1,
        ValidDay is NewDay
    ;   ValidDay is Day,
        ValidMonth is Month,
        ValidYear is Year
    ).

% Adjust year overflow
adjust_year(Month, Year, ValidYear) :-
    (   Month > 12
    ->  ValidYear is Year + 1
    ;   ValidYear is Year
    ).

% Number of days in a month
days_in_month(2, Year, 28) :- \+ leap_year(Year). % February (non-leap year)
days_in_month(2, Year, 29) :- leap_year(Year).    % February (leap year)
days_in_month(Month, _, 30) :- member(Month, [4, 6, 9, 11]). % 30-day months
days_in_month(_, _, 31). % All other months

% Leap year check
leap_year(Year) :-
    (   Year mod 4 =:= 0, Year mod 100 =\= 0
    ;   Year mod 400 =:= 0
    ).

% Format date
format_date(Date, Month, Year, FullDate) :-
    atomic_list_concat([Year, Month, Date], '-', FullDate).

% Edit a booking by first deleting it and then creating a new one
edit_booking(Room, OldDate, OldTime, OldPerson, NewRoom, NewDate, NewTime, NewPerson) :-
    retract(booked(Room, OldDate, OldTime, OldPerson)),
    assertz(booked(NewRoom, NewDate, NewTime, NewPerson)),
    write('Booking updated successfully: '), nl,
    write('Old: Room: '), write(Room), write(', Date: '), write(OldDate),
    write(', Time: '), write(OldTime), write(', Person: '), write(OldPerson), nl,
    write('New: Room: '), write(NewRoom), write(', Date: '), write(NewDate),
    write(', Time: '), write(NewTime), write(', Person: '), write(NewPerson), nl.

% Search bookings by person name
search_bookings_by_person(Person) :-
    findall((Room, Date, Time, Person), booked(Room, Date, Time, Person), Bookings),
    (   Bookings = []
    ->  write('No bookings found for person: '), write(Person), nl
    ;   write('Bookings for person: '), write(Person), nl,
        print_bookings(Bookings)
    ).

% Combined filter by date, room, and person
search_bookings(Date, Room, Person) :-
    findall((Room, Date, Time, Person), booked(Room, Date, Time, Person), Bookings),
    (   Bookings = []
    ->  write('No bookings found matching criteria.'), nl
    ;   write('Filtered Bookings:'), nl,
        print_bookings(Bookings)
    ).
