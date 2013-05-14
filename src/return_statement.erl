%% Copyright (c) 2013 Sergey Urbanovich
%% http://github.com/urbanserj/cowboy_halt
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(return_statement).

-export([parse_transform/2]).


parse_transform(Ast, _Options) ->
    Ast0 = parse_transform_util:transform(Ast, fun return_statement_last_func/1),
    parse_transform_util:transform(Ast0, fun return_statement/1).

%%===================================================================

return_statement({function, Line, Name, Arity, Ast}) ->
    {function, Line, Name, Arity, return_statement_func(Ast)};

return_statement({'fun', Line, {clauses, Ast}}) ->
    {'fun', Line, {clauses, return_statement_func(Ast)}};

return_statement({call, L0, {atom, L1, return}, [Return]}) ->
    put(return_statement, true),
    {call, L0, {atom, L1,throw},
        [{tuple, L1, [{atom, L1, return_statement}, Return]}]};

return_statement(Ast) ->
    Ast.


return_statement_func(Ast) when is_list(Ast) ->
    [ return_statement_func(A) || A <- Ast ];

return_statement_func({clause, Line, Args, Guards, Ast}) ->
    State = put(return_statement, false),
    Ast0 = parse_transform_util:transform(Ast, fun return_statement/1),
    Ast1 = {clause, Line, Args, Guards, Ast0},
    case put(return_statement, State) of
        true -> catch_return_statement(Ast1);
        false -> Ast1
    end.


catch_return_statement({clause, _Line = L, Args, Guards, Ast}) ->
    Var = list_to_atom("Return-Statement-" ++ integer_to_list(L)),
    Ast0 = parse_transform_util:transform(Ast, fun return_statement/1),
    {clause, L, Args, Guards, [
        {'try', L, Ast0, [], [{clause, L,
            [{tuple, L,
                [{atom, L, throw},
                 {tuple, L, [{atom, L, return_statement}, {var, L, Var}]},
                 {var, L, '_'}
                ]
            }], [], [{var, L, Var}]
        }], []}
    ]}.

%%===================================================================

return_statement_last_func({function, _Line, _Name, _Arity, _Ast} = Ast) ->
    {function, Line, Name, Arity, Ast0} = return_statement_last(Ast),
    Ast1 = parse_transform_util:transform(Ast0,
            fun return_statement_last_func/1),
    {function, Line, Name, Arity, Ast1};

return_statement_last_func({'fun', _Line, {clauses, _Ast}} = Ast) ->
    {'fun', Line, Ast0} = return_statement_last(Ast),
    Ast1 = parse_transform_util:transform(Ast0,
            fun return_statement_last_func/1),
    {'fun', Line, Ast1};

return_statement_last_func(Ast) ->
    Ast.


return_statement_last([Ast]) ->
    [return_statement_last(Ast)];

return_statement_last([A|Ast]) ->
    [A | return_statement_last(Ast)];

return_statement_last({function, Line, Name, Arity, Ast}) ->
    {function, Line, Name, Arity, return_statement_last_all(Ast)};

return_statement_last({'fun', Line, {clauses, Ast}}) ->
    {'fun', Line, {clauses, return_statement_last_all(Ast)}};

return_statement_last({clause, Line, Args, Guards, Ast}) ->
    {clause, Line, Args, Guards, return_statement_last(Ast)};

return_statement_last({'if', Line, Ast}) ->
    {'if', Line, return_statement_last_all(Ast)};

return_statement_last({'case', Line, St, Ast}) ->
    {'case', Line, St, return_statement_last_all(Ast)};

return_statement_last({'try', Line, St, Matches, Catches, After}) ->
    {'try', Line,
        return_statement_last(St),
        return_statement_last(Matches),
        return_statement_last(Catches),
        return_statement_last(After)
    };

return_statement_last({'catch', Line, Ast}) ->
    {'catch', Line, return_statement_last(Ast)};

return_statement_last({'receive', Line, Ast}) ->
    {'receive', Line, return_statement_last(Ast)};

return_statement_last({match, Line, Pattern, Ast}) ->
    {match, Line, Pattern, return_statement_last(Ast)};

return_statement_last({block, Line, Ast}) ->
    {block, Line, return_statement_last(Ast)};

return_statement_last({'op', Line, Op, A, B})
        when Op =:= 'orelse'; Op =:= 'or';
             Op =:= 'andalso'; Op =:= 'and' ->
    {'op', Line, Op, A, return_statement_last(B)};

return_statement_last({'op', Line, Op, Ast}) ->
    {'op', Line, Op, return_statement_last(Ast)};

return_statement_last({call, _L0, {atom, _L1, return}, [Return]}) ->
    Return;

return_statement_last(Ast) ->
    Ast.


return_statement_last_all(Ast) ->
    [return_statement_last(A) || A <- Ast].
