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

-module(parse_transform_util).

-export([transform/2]).


transform(Ast, Fun) when is_list(Ast) ->
    [transform_f(A, Fun) || A <- Ast];

transform({function, Line, Name, Arity, Ast}, Fun) ->
    {function, Line, Name, Arity, transform_f(Ast, Fun)};

transform({'fun', Line, {clauses, Ast}}, Fun) ->
    {'fun', Line, {clauses, transform_f(Ast, Fun)}};

transform({clause, Line, Args, Guards, Ast}, Fun) ->
    {clause, Line, Args, Guards, transform_f(Ast, Fun)};

transform({'if', Line, Ast}, Fun) ->
    {'if', Line, transform_f(Ast, Fun)};

transform({'case', Line, St, Ast}, Fun) ->
    {'case', Line, St, transform_f(Ast, Fun)};

transform({'try', Line, St, Matches, Catches, After}, Fun) ->
    {'try', Line,
        transform_f(St, Fun),
        transform_f(Matches, Fun),
        transform_f(Catches, Fun),
        transform_f(After, Fun)
    };

transform({'catch', Line, Ast}, Fun) ->
    {'catch', Line, transform_f(Ast, Fun)};

transform({'receive', Line, Ast}, Fun) ->
    {'receive', Line, transform_f(Ast, Fun)};

transform({'op', Line, Op, A, B}, Fun) ->
    {'op', Line, Op, transform_f(A, Fun), transform_f(B, Fun)};

transform({'op', Line, Op, Ast}, Fun) ->
    {'op', Line, Op, transform_f(Ast, Fun)};

transform({match, Line, Pattern, Ast}, Fun) ->
    {match, Line, Pattern, transform_f(Ast, Fun)};

transform({lc, Line, Ast, Generate}, Fun) ->
    {lc, Line, transform_f(Ast, Fun), transform_f(Generate, Fun)};

transform({generate, Line, Var, Ast}, Fun) ->
    {generate, Line, Var, transform_f(Ast, Fun)};

transform({bc, Line, Ast, Generate}, Fun) ->
    {bc, Line, transform_f(Ast, Fun), transform_f(Generate, Fun)};

transform({b_generate, Line, Match, Ast}, Fun) ->
    {b_generate, Line, Match, transform_f(Ast, Fun)};

transform({call, Line, FunName, Args}, Fun) ->
    {call, Line, FunName, transform_f(Args, Fun)};

transform({block, Line, Ast}, Fun) ->
    {block, Line, transform_f(Ast, Fun)};

transform({tuple, Line, Args}, Fun) ->
    {tuple, Line, transform_f(Args, Fun)};

transform({cons, Line, Car, Cdr}, Fun) ->
    {cons, Line, transform_f(Car, Fun), transform_f(Cdr, Fun)};

transform({bin, Line, Args}, Fun) ->
    {bin, Line, transform_f(Args, Fun)};

transform({bin_element, Line, Args, T0, T1}, Fun) ->
    {bin_element, Line, transform_f(Args, Fun), T0, T1};

transform({record, Line, Name, Fields}, Fun) ->
    {record, Line, Name, transform_f(Fields, Fun)};

transform({record_field, Line, Key, Value}, Fun) ->
    {record_field, Line, Key, transform_f(Value, Fun)};

transform(Ast, _Fun) ->
    Ast.

%%===================================================================

transform_f(Ast, Fun) ->
    case Fun(Ast) of
        Ast -> transform(Ast, Fun);
        Ast0 -> Ast0
    end.
