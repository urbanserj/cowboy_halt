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

-module(cowboy_halt).

-export([parse_transform/2]).


parse_transform(Ast, Options) ->
    Ast0 = parse_transform_util:transform(Ast, fun cowboy_halt/1),
    Ast1 = return_statement:parse_transform(Ast0, Options),
    cowboy_loop:parse_transform(Ast1, Options).

cowboy_halt({function, Line, Name, Arity, Ast}) ->
    {function, Line, Name, Arity, cowboy_halt_func(Ast)};

cowboy_halt({'fun', Line, {clauses, Ast}}) ->
    {'fun', Line, {clauses, cowboy_halt_func(Ast)}};

cowboy_halt({call, L0, {remote, L1,
                {atom, L2, cowboy_halt},
                {atom, L3, reply}}, Args}) ->
    put(cowboy_halt, true),
    Var = list_to_atom("Cowboy-Halt-Req-" ++ integer_to_list(L0)),
    {block, L0, [
        {match, L0,
            {tuple, L0, [{atom, L0, ok}, {var, L0, Var}]},
            {call, L0, {remote, L1,
                {atom, L2, cowboy_req},
                {atom, L3, reply}}, Args}},
        {call, L0, {atom, L1, return}, [{tuple, L0, [
            {atom, L0, halt},
            {var, L0, Var},
            {var, L0, get(cowboy_halt_state)}]}
        ]}
    ]};

cowboy_halt(Ast) ->
    Ast.


cowboy_halt_func(Ast) when is_list(Ast) ->
    [ cowboy_halt_func(A) || A <- Ast ];

cowboy_halt_func({clause, Line, Args, Guards, Ast}) ->
    Var = list_to_atom("Cowboy-Halt-State-" ++ integer_to_list(Line)),
    State = put(cowboy_halt, false),
    Var0 = put(cowboy_halt_state, Var),
    Ast0 = parse_transform_util:transform(Ast, fun cowboy_halt/1),
    Args0 =
        case put(cowboy_halt, State) of
            true -> set_state_var(Args, Var);
            false -> Args
        end,
    put(cowboy_halt_state, Var0),
    {clause, Line, Args0, Guards, Ast0}.


set_state_var([Arg], Var) ->
    Line = element(2, Arg),
    [{match, Line, Arg,
     {var, Line, Var}}];
set_state_var([Arg|Args], Var) ->
    [Arg | set_state_var(Args, Var)].
