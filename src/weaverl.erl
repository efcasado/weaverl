%%% ==========================================================================
%%% Weaverl is a parse transformation that enables Aspect-Oriented
%%% Programing (AOP) in Erlang.
%%%
%%% Aspect-oriented programming is a way of modularizing crosscutting
%%% concerns. Weaverl is an implementation of aspect-oriented programming
%%% for Erlang.
%%%
%%% At its core, aspect-oriented programming relies on the Joint Point Model
%%% (JPM). A JPM consists of three elements:
%%%
%%%     * Join point: Point in a running program where additional behaviour
%%%       can be usefully joined (e.g. a function call).
%%%
%%%     * Pointcut: Means of identifying join points. A pointcut determines
%%%       whether a join point matches (e.g. MFA, all functions within a
%%%       module, etc.).
%%%
%%%     * Advice: Means of specifying code to run at a join point. An
%%%       advice can run before, after and around a join point.
%%%
%%% An aspect is the modularisation of a crosscutting concern.
%%%
%%%
%%% Functions used within advices need to comply to the following rules:
%%%
%%%     * Functions used in 'before' advices MUST be of the same arity as the
%%%       adviced function. These functions will be called using the same
%%%       arguments as the adviced function. Note that these functions are
%%%       always executed before the adviced functions get executed.
%%%
%%%     * Functions used in 'after_returning' advices MUST be of arity one.
%%%       The result of calling the adviced function will be used as the only
%%%       argument when calling these functions. Note that these functions will
%%%       only be called if the adviced functions returns normally.
%%%
%%%     * Functions used in 'after_throwing' advices MUST be of arity one. The
%%%       exception or error thrown when calling the adviced function will be used
%%%       as the only argument when calling these functions.
%%%
%%%     * Functions used in 'around' adives MUST be of arity three. These functions
%%%       are called instead of the adviced function. The arguments of these
%%%       functions are the module, function and arguments of the adviced function.
%%%
%%%
%%% Weaverl's AOP functionality is provided by means of a parse transformation.
%%% Aspects are specified as an Erlang compiler option using the 'weaverl_aspects'
%%% key.
%%%
%%%     erlc -o ebin +{weaverl_aspects, Aspects} src/foo.erl
%%%
%%%
%%% TL;DR
%%% Weaverl provides a means of altering the behaviour of an Erlang
%%% application without requiring the manual modification of the target
%%% modules and functions.
%%%
%%%
%%% Author: Enrique Fernandez <efcasado@gmail.com>
%%% Creation date: April, 2014
%%% ==========================================================================
-module(weaverl).

%% TODO:
%%     #1 - How to handle functions declared in header files?
%%     #2 - Run-time evaluation of meta-calls.
%%     #5 - Implement a user-friendly mechanism for specifying advices and
%%          pointcuts.

-export([parse_transform/2]).



%% Weaverl's parse transformation.
-spec parse_transform(list(), list()) -> list().
parse_transform(Forms, Opts) ->
    ModuleName = read_module_name(Forms),
    LocallyAccessibleFunctions =
        read_imported_functions(Forms) ++
        read_local_functions(Forms, ModuleName) ++
        read_included_functions(Forms, ModuleName),
    apply_aspects(get_aspects(ModuleName, Opts),
                  LocallyAccessibleFunctions,
                  Forms).


%% Returns a list of all defined aspects, in their canonical form.
-spec get_aspects(string(), list()) -> list().
get_aspects(ModuleName, Opts) ->
    UserProvidedAspects = proplists:get_value(weaverl_aspects, Opts, []),
    CanonisedAspects = canonise_aspects(UserProvidedAspects),
    %% Filter out aspects not applicable to this module.
    filter_aspects(ModuleName, CanonisedAspects).

%% Converts a list of aspects to their canonical form.
canonise_aspects(Aspects) ->
    [ canonise_aspect(A) || A <- Aspects ].

%% Converts an aspect to its canonical form.
%% Aspect -> {Pointcut, Advice} -> {{M,F,A}, {Type, {M,F}}, Within}
canonise_aspect({Pointcut = {_,_,_}, Advice = {_AdvType, _AdvMF = {_,_}}}) ->
    {Pointcut, Advice, ".*"};
canonise_aspect(Aspect = {_PcMFA = {_,_,_}, {_AdvType, _AdvMF = {_,_}}, _Within}) ->
    Aspect.


%% Filters out aspects not applicable to this module.
filter_aspects(ModuleName, Aspects) ->
    [ A || A = {_Pc, _Adv, Within} <- Aspects,
           re:run(ModuleName, Within) =/= nomatch ].


apply_aspects(Aspects, LocalFunctions, Forms) ->
    lists:foldl(
      fun(Aspect = {_Pointcut, Advice, _Within}, UpdatedForms) ->
              unclutter(
                forms:map(
                  fun(Form = {call, _L, _F, _Args}) ->
                          case is_weaveable(Form, Aspect, LocalFunctions) of
                              true ->
                                  weave(Form, Advice);
                              false ->
                                  Form
                          end;
                     (OtherForm) ->
                          OtherForm
                  end,
                  UpdatedForms))
      end,
      Forms,
      Aspects).

%% Convert all weaverl_call pseudo-forms to proper call forms. weaverl_call
%% pseudo-forms are used to avoid infinite loops when weaving forms.
unclutter(Forms) ->
    forms:map(
      fun({weaverl_call, L, F, Args}) ->
              {call, L, F, Args};
         (OtherForm) ->
              OtherForm
      end,
      Forms).


is_weaveable({call,_L,{atom,_,F}, Args}, Aspect, LocalFs) ->
    A = length(Args),
    M = get_module({F, A}, LocalFs),
    is_weaveable_(M, atom_to_list(F), A, Aspect);
is_weaveable({call,_L,{remote,_,{atom,_,erlang},{atom,_,apply}},
              [{atom,_,M},{atom,_,F},Args]},
             Aspects,
             _LocalFs) ->
    is_weaveable_(atom_to_list(M), atom_to_list(F), cons_length(Args), Aspects);
is_weaveable({call,_L,{remote,_,{atom,_,M},{atom,_,F}},Args}, Aspects, _LocalFs) ->
    is_weaveable_(atom_to_list(M), atom_to_list(F), length(Args), Aspects);
is_weaveable(_OtherCall, _Aspects, _LocalFs) ->
    false.

is_weaveable_(M, F, A, {{PcM, PcF, PcA}, _Advice, _Within}) ->
    is_match([M, F, A], [PcM, PcF, PcA]).

is_match([], []) ->
    true;
is_match([S| Subjects], [RE| REs])
  when is_list(S) andalso is_list(RE) ->
    case re:run(S, RE) of
        nomatch ->
            false;
        _Other ->
            is_match(Subjects, REs)
    end;
is_match([X| Xs], [Y| Ys]) ->
    case X == Y of
        false ->
            false;
        true ->
            is_match(Xs, Ys)
    end.

get_module({F, A}, Functions) ->
    proplists:get_value({F, A}, Functions, erlang).


%% Alters the captured function's behaviour according to the specified advice.
%%
%% Weaved function calls use the 'weaverl_call' pseudo-form to avoid infinite
%% loops.
weave({call, L, Pc, Args}, {before, {M, F}}) ->
    {weaverl_call, L, Pc,
     [{weaver_call, L, {remote, L, {atom, L, M}, {atom, L, F}}, Args}]};
weave({call, L, _Pc, Args}, {around, {M, F}}) ->
    {weaverl_call, L, {remote, L, {atom, L, M}, {atom, L, F}}, Args};
weave({call, L, Pc, Args}, {after_returning, {M, F}}) ->
    {weaverl_call, L, {remote, L, {atom, L, M}, {atom, L, F}},
     [{weaverl_call, L, Pc, Args}]};
weave({call, L, Pc, Args}, {after_throwing, {M, F}}) ->
    {'try', L,
     [{weaverl_call, L, Pc, Args}],
     [],
     [{clause, L,
       [{tuple, L, [{var, L, 'C'}, {var, L, 'E'}, {var, L, '_'}]}],
       [],
       [{weaverl_call, L, {remote, L, {atom, L, M}, {atom, L, F}},
         [{var, L, 'C'}, {var, L, 'E'}]}]}]}.



%% =========================================
%%  Helper functions for manipulating forms
%% =========================================


%% Returns the name of the module being parse transformed.
read_module_name([{attribute, _L, file, {Module, _}}| _Forms]) ->
    filename:basename(Module, ".erl").

read_imported_functions(Forms) ->
    lists:flatten(
      forms:reduce(
        fun({attribute, _L, import, {M, Fs}}, Acc) ->
                [[ {{F, A}, M} || {F, A} <- Fs ]| Acc];
           (_OtherForm, Acc) ->
                Acc
        end,
        [],
        Forms)).

read_local_functions(Forms, M) ->
    forms:reduce(
      fun({function, _L, F, A, _Clauses}, Acc) ->
              [{{F, A}, M}| Acc];
         (_OtherForm, Acc) ->
              Acc
      end,
      [],
      Forms).

read_included_functions(Forms, M) ->
    IncludedFiles =
        forms:reduce(
          fun({attribute, L, file, {File, _}}, Acc) when L =/= 1 ->
                  [File| Acc];
             (_OtherForm, Acc) ->
                  Acc
          end,
          [],
          Forms),

    lists:foldl(
      fun(F, Acc) ->
              lists:append(Acc, read_local_functions(read_forms(F), M))
      end,
      [],
      IncludedFiles).

%% Returns the length of a const form.
cons_length({cons, _, {nil, _}, {nil, _}}) ->
    1;
cons_length({cons, _, _H, {nil, _}}) ->
    1;
cons_length({cons, _, _H, Cons}) ->
    cons_length_(Cons, 1).

cons_length_({cons, _, _H, Cons}, Length) ->
    cons_length_(Cons, Length + 1);
cons_length_({nil, _}, Length) ->
    Length.

%% Reads the Erlang forms of the specified Erlang source file.
read_forms(File) ->
    case epp:parse_file(File, [], []) of
        {ok, Forms} ->
            Forms;
        _Error ->
            throw({forms_not_found, File})
    end.
