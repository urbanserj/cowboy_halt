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

-module(cowboy_loop).

-export([parse_transform/2]).


parse_transform(Ast, _Options) ->
    Ast1 = parse_transform_util:transform(Ast, fun cowboy_loop/1),
    [ erl_pp:form(A) || A <- Ast1 ],
    Ast1.

cowboy_loop({function, Line, Name, Arity, Ast}) ->
    {function, Line, Name, Arity, cowboy_loop_func(Ast)};

cowboy_loop({'fun', Line, {clauses, Ast}}) ->
    {'fun', Line, {clauses, cowboy_loop_func(Ast)}};

cowboy_loop({call, L0, {remote, _L1,
                {atom, _L2, cowboy_halt},
                Action={atom, _L3, Fun}}, Args})
        when Fun =:= loop; Fun =:= ok ->
    [_Msg, Req, State] = get(cowboy_loop_args),
    [Req0, State0] =
        case Args of
            [] -> [Req, State];
            [R] -> [R, State];
            [R, S] -> [R, S]
        end,
    case Fun of
        ok -> ok;
        loop -> put(cowboy_loop, {Req0, State0})
    end,
    {tuple, L0, [Action, Req0, State0]};

cowboy_loop(Ast) ->
    Ast.


cowboy_loop_func(Ast) when is_list(Ast) ->
    [ cowboy_loop_func(A) || A <- Ast ];

cowboy_loop_func({clause, Line, Args, Guards, Ast}) ->
    LoopState = put(cowboy_loop, false),
    LoopArgs = put(cowboy_loop_args, Args),
    Ast0 = parse_transform_util:transform(Ast, fun cowboy_loop/1),
    Ast1 = {clause, Line, Args, Guards, Ast0},
    put(cowboy_loop_args, LoopArgs),
    case put(cowboy_loop, LoopState) of
        {Req, State} -> catch_cowboy_loop(Ast1, Req, State);
        false -> Ast1
    end.


catch_cowboy_loop({clause, _Line = L,
            Args=[_Msg, {var, _L0, Req}, _State], Guards, Ast},
        {var, L1, Req}, State) ->
    {clause, L, Args, Guards, [
        {'try', L, Ast, [], [{clause, L,
            [{tuple, L,
                [{atom, L, error},
                 {tuple, L, [
                     {atom, L, badmatch},
                     {tuple, L, [
                         {atom, L, error},
                         {atom, L, closed}
                     ]}
                 ]},
                 {var, L, '_'}
                ]
            }], [], [{tuple, L, [
                {atom, L, ok}, {var, L1, Req}, State
                ]}]
        }], []}
    ]}.
