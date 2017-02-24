-module(ind).
-export([get_words_from_file/1, index_listing/2, line_numbers/1, main/0]).
-define(is_alpha(Char), (Char >= $A andalso Char =< $Z) orelse (Char >= $a andalso Char =< $z)).
-import(index, [get_file_contents/1]).

% { word, [{Start, End}] }

word_set([]) -> 
    [];
word_set([Word|Words]) ->
    [ Word | word_set( [W || W <- Words, W =/= Word] ) ].

no_punc(Char) when ?is_alpha(Char) ->
    Char;
no_punc(_) ->
    32.

to_lower(Char) when $A =< Char andalso Char =< $Z ->
    Char + 32;
to_lower(Char) -> Char.

to_lowercase(Chars) ->
    [ to_lower(Char) || Char <- Chars ].


remove_punc(Chars) -> 
    [ no_punc(Char) || Char <- Chars ].

get_words_from_file(Filename) ->
    Contents = get_file_contents(Filename),
    ConcatContents = lists:concat(Contents),
    NoPunct = remove_punc(ConcatContents),
    Downcase = to_lowercase(NoPunct),
    Tokens = string:tokens(Downcase, " "),
    Word_set = word_set(Tokens),
    Word_set.

get_words_line(Line) ->
    NoPunct = remove_punc(Line),
    Downcase = to_lowercase(NoPunct),
    Tokens = string:tokens(Downcase, " "),
    Word_set = word_set(Tokens),
    Word_set.

line_numbers(Lines) ->
    line_numbers(1, Lines).

line_numbers(_, []) ->
    [];
line_numbers(N, [Line|Lines]) ->
    [ { N, get_words_line(Line) } | line_numbers(N+1, Lines) ].
			      
index_listing(Word, Lines) ->
    LineMatches = get_matches(Word, Lines),
    {Word, LineMatches}.

get_matches(_, []) ->
    [];
get_matches(Word, [{Number, Line}|Lines]) ->
    case lists:member(Word, Line) of
	true ->
	    [ Number | get_matches(Word, Lines) ];
	false ->
	    get_matches(Word, Lines)
    end.

main() ->
    Filename = "dickens-christmas.txt",
    Lines = get_file_contents(Filename),
    FileWordSet = get_words_from_file(Filename),
    LineWords = line_numbers(Lines),
    [ index_listing(Word, LineWords) || Word <- FileWordSet ].
