% define not
not(X) :- X, !, fail.
not(_).

% harversine formula in miles
harversine_formula( Lat1, Lon1, Lat2, Lon2, Dist) :-
    Dlon is Lon2 - Lon1,
    Dlat is Lat2 - Lat1,
    A is sin(Dlat / 2) ** 2 + cos(Lat1) * cos(Lat2) * sin(Dlon / 2) ** 2,
    C is 2 * atan2(sqrt(A), sqrt(1 - A)),
    Dist is 3956 * C.

% calculate radians
radians( degmin( Deg, Min), Rad) :- Rad is (Deg + Min / 60) * pi / 180.

% calculate distance between airports in miles
distance( Airpt1, Airpt2, Dist) :- 
    airport( Airpt1, _, Lat1, Lon1),
    airport( Airpt2, _, Lat2, Lon2),
    radians( Lat1, Lat1_Radians),
    radians( Lat2, Lat2_Radians),
    radians( Lon1, Lon1_Radians),
    radians( Lon2, Lon2_Radians),
    harversine_formula( Lat1_Radians, Lon1_Radians, Lat2_Radians, Lon2_Radians, Dist).

% calculate arrival time for each flight
arrivalTime( time( H, M), Dist, ArrivalTime) :-
    Hours is Dist / 500,
    ArrivalTime is H + Hours + M / 60.    

% find routes according to Depart and Arrive
findRoutes( Depart, Arrive, [[Depart, Arrive, time( H, M), ArrivalTime]], time( H, M), PrevFlights) :- 
    flight( Depart, Arrive, time(H, M)),
    not(member( Arrive, PrevFlights)),
    distance( Depart, Arrive, Dist),
    arrivalTime( time( H,M), Dist, ArrivalTime).

findRoutes( Depart, Arrive, [[Depart, Transfer, time( H, M), ArrivalTime]|Rs], time( H, M), PrevFlights) :- 
    flight( Depart, Transfer, time( H, M)), 
    not(member( Transfer, PrevFlights)),
    distance( Depart, Transfer, Dist),
    arrivalTime( time( H, M), Dist, ArrivalTime),
    flight( Transfer, _, time( H1, M1)),
    (ArrivalTime + 0.5) < (H1 + M1/60),
    findRoutes( Transfer, Arrive, Rs, time( H1, M1), [Transfer|PrevFlights]).

% write routes 
writeRoutes( []) :- nl,!.
writeRoutes( [[ Depart, Arrive, time( H, M), ArrivalTime]|Rs]) :- 
    airport( Depart, DepartName, _, _),
    airport( Arrive, ArriveName, _, _),
    upcase_atom( Depart, Depart_),
    upcase_atom( Arrive, Arrive_),
    write("depart  "), write( Depart_), write("  "), write( DepartName), print_time( H, M), nl,
    write("arrive  "), write( Arrive_), write("  "), write( ArriveName), print_time( ArrivalTime), nl,
    writeRoutes( Rs).

print_time( Hour_, Minute) :- 
    (Hour_ >= 24, Hour is Hour_ - 24; Hour is Hour_ + 0),
    (Hour >= 10, format("~d:", Hour); format("0~d:", Hour)), 
    (Minute >= 10, print(Minute); format("0~d", Minute)).
print_time( Time) :- 
    Hour is floor(Time),
    Minute is round((Time - Hour)*60),
    print_time( Hour, Minute).

print_trip( Action, Code, Name, time( Hour, Minute)) :-
   upcase_atom( Code, Upper_code),
   format( "~6s  ~3s  ~s~26|  ",
           [Action, Upper_code, Name]),
   print_time( Hour, Minute),
   nl.

test :-
   print_trip( depart, nyc, 'New York City', time( 9, 3)),
   print_trip( arrive, lax, 'Los Angeles', time( 14, 22)).

fly( Depart, Arrive) :-
    findRoutes( Depart, Arrive, Rs, _, [Depart]),
    writeRoutes( Rs),
    !,true.

main:-
    read( Airpt1),
    read( Airpt2),
    fly( Airpt1, Airpt2).

% database
airport( atl, 'Atlanta         ', degmin(  33,39 ), degmin(  84,25 ) ).
airport( bos, 'Boston-Logan    ', degmin(  42,22 ), degmin(  71, 2 ) ).
airport( chi, 'Chicago         ', degmin(  42, 0 ), degmin(  87,53 ) ).
airport( den, 'Denver-Stapleton', degmin(  39,45 ), degmin( 104,52 ) ).
airport( dfw, 'Dallas-Ft.Worth ', degmin(  32,54 ), degmin(  97, 2 ) ).
airport( lax, 'Los Angeles     ', degmin(  33,57 ), degmin( 118,24 ) ).
airport( mia, 'Miami           ', degmin(  25,49 ), degmin(  80,17 ) ).
airport( nyc, 'New York City   ', degmin(  40,46 ), degmin(  73,59 ) ).
airport( sea, 'Seattle-Tacoma  ', degmin(  47,27 ), degmin( 122,17 ) ).
airport( sfo, 'San Francisco   ', degmin(  37,37 ), degmin( 122,23 ) ).
airport( sjc, 'San Jose        ', degmin(  37,22 ), degmin( 121,56 ) ).

flight( bos, nyc, time( 7,30 ) ).
flight( dfw, den, time( 8, 0 ) ).
flight( atl, lax, time( 8,30 ) ).
flight( chi, den, time( 8,45 ) ).
flight( mia, atl, time( 9, 0 ) ).
flight( sfo, lax, time( 9, 0 ) ).
flight( sea, den, time( 10, 0 ) ).
flight( nyc, chi, time( 11, 0 ) ).
flight( sea, lax, time( 11, 0 ) ).
flight( den, dfw, time( 11,15 ) ).
flight( sjc, lax, time( 11,15 ) ).
flight( atl, lax, time( 11,30 ) ).
flight( atl, mia, time( 11,30 ) ).
flight( chi, nyc, time( 12, 0 ) ).
flight( lax, atl, time( 12, 0 ) ).
flight( lax, sfo, time( 12, 0 ) ).
flight( lax, sjc, time( 12, 15 ) ).
flight( nyc, bos, time( 12,15 ) ).
flight( bos, nyc, time( 12,30 ) ).
flight( den, chi, time( 12,30 ) ).
flight( dfw, den, time( 12,30 ) ).
flight( mia, atl, time( 13, 0 ) ).
flight( sjc, lax, time( 13,15 ) ).
flight( lax, sea, time( 13,30 ) ).
flight( chi, den, time( 14, 0 ) ).
flight( lax, nyc, time( 14, 0 ) ).
flight( sfo, lax, time( 14, 0 ) ).
flight( atl, lax, time( 14,30 ) ).
flight( lax, atl, time( 15, 0 ) ).
flight( nyc, chi, time( 15, 0 ) ).
flight( nyc, lax, time( 15, 0 ) ).
flight( den, dfw, time( 15,15 ) ).
flight( lax, sjc, time( 15,30 ) ).
flight( chi, nyc, time( 18, 0 ) ).
flight( lax, atl, time( 18, 0 ) ).
flight( lax, sfo, time( 18, 0 ) ).
flight( nyc, bos, time( 18, 0 ) ).
flight( sfo, lax, time( 18, 0 ) ).
flight( sjc, lax, time( 18,15 ) ).
flight( atl, mia, time( 18,30 ) ).
flight( den, chi, time( 18,30 ) ).
flight( lax, sjc, time( 19,30 ) ).
flight( lax, sfo, time( 20, 0 ) ).
flight( lax, sea, time( 22,30 ) ).