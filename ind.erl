-module(ind).
-export([get_words_from_file/1, index_listing/2, line_numbers/1, main/0, main/1]).
-define(is_alpha(Char), (Char >= $A andalso Char =< $Z) orelse (Char >= $a andalso Char =< $z)).
-import(index, [get_file_contents/1]).
-import(lists, [member/2]).

create_word_set([]) ->
    [];
create_word_set([Word|Words]) ->
    [ Word | create_word_set( [W || W <- Words, W =/= Word] ) ].

no_punc(Char) when ?is_alpha(Char) ->
    Char;
no_punc(_) ->
    32.

to_lower(Char) when $A =< Char andalso Char =< $Z ->
    Char + 32;
to_lower(Char) -> Char.

to_lowercase(Chars) ->
    [ to_lower(Char) || Char <- Chars ].

remove_punctuation(Chars) -> 
    [ no_punc(Char) || Char <- Chars ].

get_words_from_file(Filename) ->
    Contents = get_file_contents(Filename),
    ConcatContents = lists:concat(Contents),
    get_words_line(ConcatContents).

get_words_line(Line) ->
    NoPunct = remove_punctuation(Line),
    Downcase = to_lowercase(NoPunct),
    Tokens = string:tokens(Downcase, " "),
    create_word_set(Tokens).
    
line_numbers(Lines) ->
    line_numbers(1, Lines).

line_numbers(_, []) ->
    [];
line_numbers(N, [Line|Lines]) ->
    [ { N, get_words_line(Line) } | line_numbers(N+1, Lines) ].
			      
index_listing(Word, Lines) ->
    LineMatches = get_matches(Word, Lines),
    {Word, collapse(LineMatches)}.

get_matches(_, []) ->
    [];
get_matches(Word, [{Number, Line}|Lines]) ->
    case member(Word, Line) of
	true ->
	    [ Number | get_matches(Word, Lines) ];
	false ->
	    get_matches(Word, Lines)
    end.

collapse([]) ->
    [];
collapse([H|T]) ->
    collapse([H|T], H+1, H, H).
collapse([], _Expected, Prev, Start) ->
    if 
	Start =:= Prev -> [{Start}];
	true -> [{Start, Prev}]
    end;
collapse([H|T], Expected, Prev, Start) ->
    if 
	H =:= Expected -> collapse(T, H + 1, H, Start);
	Start =:= H -> collapse(T, H + 1, H, H);
	true -> [ bind({Start, Prev}) | collapse(T, H+1, H, H) ]				 
    end.

bind({A, A}) ->
    {A};
bind({A, B}) ->
    {A, B}.	    

main(Filename) ->
    Lines = get_file_contents(Filename),
    FileWordSet = get_words_from_file(Filename),
    LineNumbers = line_numbers(Lines),
    [ index_listing(Word, LineNumbers) || Word <- FileWordSet ].

main() ->
    Filename = "dickens-christmas.txt",
    Lines = get_file_contents(Filename),
    FileWordSet = get_words_from_file(Filename),
    LineNumbers = line_numbers(Lines),
    [ index_listing(Word, LineNumbers) || Word <- FileWordSet ].
