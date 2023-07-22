:- use_module(library(clpfd)).
:- [utilities].

is_drutam(drutam).
is_anudrutam(anudrutam).

is_laghu(laghu).
has_laghu(Angas) :-
    include(is_laghu, Angas, Ls),
    non_empty(Ls).

% Gati is the subdivision of the beat into equal parts
gati(3).
gati(4).
gati(5).
gati(7).
gati(9).

laghu_jati(3, "Tisra").
laghu_jati(4, "Chatursa").
laghu_jati(5, "Khanda").
laghu_jati(7, "Misra").
laghu_jati(9, "Sankirna").

% Jathi is the accenting of groups of beats
jathi(3).
jathi(4).
jathi(5).
jathi(7).


% Anga are beat groupings with certain
% rules about their placements adjacent to
% eachother
anga([], [], [], []).
anga([laghu|LS], Ds, As, [laghu|Result]) :-
    anga(LS, Ds, As, Result).
anga(LS, [drutam|Ds], As, [drutam|Result]) :-
    anga(LS, Ds, As, Result).
anga(LS, [drutam|Ds], [anudrutam], [drutam, anudrutam | Result]) :-
    anga(LS, Ds, [], Result).
anga(LS, [drutam|Ds], [anudrutam], [anudrutam, drutam | Result]) :-
    anga(LS, Ds, [], Result).

anga_to_beats(N, laghu, N).
anga_to_beats(_, drutam, 2).
anga_to_beats(_, anudrutam, 1).

% Generate arbitrary, formally correct tala
% Tala are cycles, subdivided into anga
tala(Angas) :-
    member(N, [1,2,3,4,5,6]),
    length(Angas, N),
    [First|_] = Angas,
    % laghu restrictions
    member(Ln, [1,2,3]),
    length(Ls, Ln),
    % drutam restrictions
    member(Dn, [0,1,2]),
    length(Ds, Dn),
    % anudrutam restrictions
    member(An, [0,1]),
    length(As, An),
    (An > 0 -> Dn > 0 ; true),
    last(Angas, End),
    (is_laghu(End) ; is_drutam(End)),
    (is_laghu(First) ; is_drutam(First)),
    anga(Ls, Ds, As, Angas).

%! suladi_tala(-Category, -Name, -Beats).
% A categorization of differen tala.
suladi_tala("Dhruva", Name, Beats) :-
    Tala = [laghu, drutam, laghu, laghu],
    maplist(anga_to_beats(L), Tala, Beats),
    (L = 3, Name = "Mani";
     L = 4, Name = "Sriakara";
     L = 5, Name = "Pramana";
     L = 7, Name = "Purna";
     L = 9, Name = "Bhuvana").

suladi_tala("Matya", Name, Beats) :-
    Tala = [laghu, drutam, laghu],
    maplist(anga_to_beats(L), Tala, Beats),
    (L = 3, Name = "Sara";
     L = 4, Name = "Sama";
     L = 5, Name = "Udaya";
     L = 7, Name = "Urdina";
     L = 9, Name = "Rava").

suladi_tala("Rupaka", Name, Beats) :-
    Tala = [drutam, laghu],
    maplist(anga_to_beats(L), Tala, Beats),
    (L = 3, Name = "Chakra";
     L = 4, Name = "Patti";
     L = 5, Name = "Raja";
     L = 7, Name = "Kula";
     L = 9, Name = "Bindu").

suladi_tala("Jhampa", Name, Beats) :-
    Tala = [laghu, anudrutam, drutam],
    maplist(anga_to_beats(L), Tala, Beats),
    (L = 3, Name = "Kadamba";
     L = 4, Name = "Madhura";
     L = 5, Name = "Chana";
     L = 7, Name = "Sura";
     L = 9, Name = "Kara").

suladi_tala("Triputa", Name, Beats) :-
    Tala = [laghu, drutam, drutam],
    maplist(anga_to_beats(L), Tala, Beats),
    (L = 3, Name = "Sankha";
     L = 4, Name = "Adi";
     L = 5, Name = "Dushkara";
     L = 7, Name = "Lila";
     L = 9, Name = "Bhoga").

suladi_tala("Ata", Name, Beats) :-
    Tala = [laghu, laghu, drutam, drutam],
    maplist(anga_to_beats(L), Tala, Beats),
    (L = 3, Name = "Gupta";
     L = 4, Name = "Lekha";
     L = 5, Name = "Vidala";
     L = 7, Name = "Loya";
     L = 9, Name = "Dhira").

suladi_tala("Eka", Name, Beats) :-
    Tala = [laghu],
    maplist(anga_to_beats(L), Tala, Beats),
    (L = 3, Name = "Sudha";
     L = 4, Name = "Mana";
     L = 5, Name = "Rata";
     L = 7, Name = "Raga";
     L = 9, Name = "Vasu").

%! random_tala(-Category, -Name, -Jati, -Beats)
% Randomly selects a tala, giving it's
% categorization and name if one exists.
random_tala(Category, Name, Jati, Beats) :-
    random_solution(tala, Tala),
    random_permutation([3,4,5,7,9], Laghus),
    member(L, Laghus),
    laghu_jati(L, Jati),
    maplist(anga_to_beats(L), Tala, Beats),
    ( suladi_tala(Category, Name, Beats) -> true
    ; Category = "Unclassified", Name = "Unknown"
    ),
    !.

offset(X, Z, Y) :- plus(X, Y, Z).

% Sometimes gati + jathi combinations do not
% end at the beginning of an anga, so an offset
% must be applied such that they sync up on a
% downbeat or 'sam'
layering_offsets([H|T], Gati, Jathi, Offsets) :-
    foldl(plus, T, H, NumBeats),
    scanl(plus, T, H, Sams),
    Cycle is mod(Gati * Jathi, NumBeats),
    maplist(offset(Cycle), Sams, Offsets).

% the bhedam tree relates different gati/jathi combos,
% ie. it defines the valid transitions between these
% combos.
%
% #1 is via dropping the jathi
bhedam_tree(gati(_), jathi(_), gati(H), none) :-
    gati(H).
% #2 make the jathi into the gati
bhedam_tree(gati(_), jathi(J), gati(J), none).
% #3 hold gati, but change jathi
bhedam_tree(gati(G), jathi(J), gati(G), jathi(K)) :-
    jathi(K),
    K =\= J,
    K =\= G.
% #4 hold jathi, but change gati
bhedam_tree(gati(G), jathi(J), gati(H), jathi(J)) :-
    gati(H),
    H =\= J,
    H =\= G.

% The most common type of Mykthay, a phrase repeated
% three times with variations between repetitions
% using beat offsets or jhati accents
%
% Three possibilities
% - displace from start, moving jhati groupings with phrase
% - introduce new jhati groupings
% - displace phrase, but keeping original jhati groups
sama_mukthay(Cycles, Beats, gati(G), [P, P, E],  GapLength) :-
    member(Cycles, [1, 2]),
    Matras is Beats * G * Cycles,
    % How long the phrases will be
    Pala is Matras div 3,
    % With these gaps in between
    Gap is Matras rem 3,
    % We can take some beats off the
    % last Pala and make the gaps
    % longer
    shift_gap(Pala, Gap, P, E, GapLength),
    GapLength < 5.

shift_gap(Pala, Gap, P, E, G) :-
    shift_from_all(Pala, Gap, P1, GTotal),
    shift_from_end(P1, GTotal, P, E, GTotal2),
    N is float_fractional_part(GTotal2),
    N =:= 0.0,
    M is floor(GTotal2),
    M mod 2 =:= 0,
    G is M div 2.
shift_gap(Pala, Gap, P, E, G) :-
    shift_from_end(Pala, Gap, P, E, GTotal),
    GTotal mod 2 =:= 0,
    G is GTotal div 2.
shift_gap(Pala, Gap, P, P, G) :-
    shift_from_all(Pala, Gap, P, GTotal),
    GTotal mod 2 =:= 0,
    G is GTotal div 2.

shift_from_end(Pala, Gap, P, E, GTotal) :-
    between(1, Pala, Shift),
    P = Pala,
    E is Pala - Shift,
    GTotal is Gap + Shift.

shift_from_all(Pala, Gap, P, GTotal) :-
    between(1, Pala, Shift),
    Shift mod 3 =:= 0,
    P is Pala - (Shift div 3),
    GTotal is Gap + Shift.

% A 3-Fold Mukthay repeats a phrase twice
% in one gati and then again in a different one
%
% Type A
% If the gatis are X and Y, then the first two
% phrases will be Y groups of X, with the last
% one being X groups of Y.
%
% Type B
% Same as A, except the second repetition is
% double the speed of the first
%
% Type C
% Uses three gati, two are related in the usual
% Y of X then X of Y pattern, but the third
% phrase is picked so an offset is needed at
% the start. The offset phrase always begins
% the mukthay
three_fold_mukthay().

% A sequence of repeated phrases with a consistent
% gap where the variation is adding N matras
% per repetition in differing places with an
% accent at each insertion point
%
% Usually calculated to end on a sam, otherwise
% followed by a short makthay to make up the
% difference
srotovahayati().

% The reverse process of srotovahayati
gopuchayati().

% A srotovahayati then a gopuchayati
%
% With considerations
% - Additions or omissions do not need to
%   be symmetric
% - Gap variations between two halfs, so
%   long as they are consistent within the
%   half
% - The second half can be in a different gati
%   than the first, otherwise there is an exact
%   repetition at the end of the first and
%   beginning of the second
% - The first phrase of the second half can be
%   elided into the last phrase of the first half
mridangamyati().

% A gopuchayati then a srotovahayati
%
% All the same procedures for mridangamyati
% apply, but also without the risk of the
% repetive middle section
damaruyati().
