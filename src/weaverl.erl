%% Weaverl is a simple Aspect-oriented Programming (AOP) framework for the
%% Erlang/OTP programming language.
%%
%% Weaverl allows you to express cross-cutting concerns (e.g. logging, ...) in
%% stand-alone modules called aspects. Cross-cutting functionality can be injected
%% to specified points in the execution of a program by making use of the so
%% called advices. An advice consists of:
%%
%%   1) pointcut: way to specify a point in a running program where the advice 
%%   is to be applied.
%%
%%   2) advice type: specified whether the advice is applied before, after or
%%   around the execution point matched by the pointcut.
%%
%%   3) proxy function: function to be invoked when the advice is applied.
%%
%% At the time of this writing, weaverl only supports compile-time weaving [1],
%% which has been implemented using Erlang's parse transformation capabilities [2].
%%
%% [1] http://www.eclipse.org/aspectj/doc/next/devguide/ltw.html
%% [2] http://www.erlang.org/doc/man/erl_id_trans.html

-module(weaverl).

-export([parse_transform/2]).

-record(weaverl_forms,
        {
          files    = [],
          module,
          exports  = [],
          imports  = [],
          compiles = [],
          records  = [],
          wilds    = [],
          funs     = [],
          errors   = [],
          warnings = [],
          eof
        }).

-type weaverl_forms() :: #weaverl_forms{}.

-record(state, 
        {
          advices                                     :: list(),
          context = {undefined, undefined, undefined} :: {atom(), {atom(), integer()}},
          forms   = #weaverl_forms{}                  :: weaverl_forms()
        }).


%%------------------------------------------------------------------------------
%% @doc
%% Function called by the compiler. It takes the abstract format of the module
%% being compiled and a list of compiler options as input parameters. If the
%% {weaverl_adf, File} compiler option is set, all advice definitions are read
%% from that file. Else, advice definitions are expected to be placed in a
%% advices.adf file sitting in the root of the project.
%% @end
%%------------------------------------------------------------------------------
parse_transform(Forms, Options) ->
    ADFFile = proplists:get_value(adf_file, Options, "advices.adf"),
    process_forms(Forms, #state{advices = read_advices(ADFFile)}).

%%------------------------------------------------------------------------------
%% @doc
%% Read all advice definitions from the specified file.
%% @end
%%------------------------------------------------------------------------------
-spec read_advices(string()) -> list().
read_advices(ADFFile) ->
    case file:consult(ADFFile) of
        {ok, [{advices, Advices0}]} ->
            lists:foldl(
              fun({Type, Pointcuts, Proxy, Context}, D0) ->
                      lists:foldl(fun(MFA, D1) ->
                                          D1 ++ [{MFA, {Type, Proxy, Context}}]
                                  end,
                                  D0,
                                  Pointcuts)
              end,
              [],
              Advices0);
        %% If advice definitions are malformed, returns an empty list.
        {ok, _} ->
            [];
        {error, _} ->
            []
    end.

process_forms(Forms0, State0) ->
    #state{forms = Forms} = 
        lists:foldl(
          fun(Form, State) ->
              process_form(Form, State)
          end,
          State0,
          Forms0),
    forms_to_list(Forms).

%% This function is called every time a function call (local or remote) is found.
handle_call({{M, F, A}, Args}, State) ->
    case get_applicable_advices({M, F, A}, State) of
        []           -> {{fun2abs({M, F}, State), Args}, State};
        [Advice| _]  -> apply_advice(Advice, {M, F, Args}, State)
    end.

get_applicable_advices(Fun, #state{advices = AllAdvices, context = Context}) ->
    lists:foldl(
      fun({Pointcut, {Type, ProxyFun, AContext}}, Acc) ->
              case is_jointpoint(Fun, {Pointcut, AContext}, Context) of
                  true  -> Acc ++ [{Type, ProxyFun}];
                  false -> Acc
              end
      end,
      [],
      AllAdvices
     ).

%%------------------------------------------------------------------------------
%% @doc
%% Returns true if there is a pointcut that matches the specified function.
%% Otherwise, false is returned.
%% @end
%%------------------------------------------------------------------------------
is_jointpoint({M, F, A}, {{PcM, PcF, PcA}, AContext}, {CM, CF, CA}) ->
    try
        case AContext of
            {module, Mod} ->
                {match, _} = re:run(atom_to_list(CM), Mod);
            {function, {Mod, Fun, Arity}} ->
                re:run(atom_to_list(CM), Mod),
                re:run(atom_to_list(CF), Fun),
                Arity = CA
        end,
        {match, _} = re:run(atom_to_list(M), PcM),
        {match, _} = re:run(atom_to_list(F), PcF),
        A = PcA,
        true
    catch
        _:_ ->
            false
    end.


forms_to_list(#weaverl_forms{files = Fs, module = M, exports = Es, imports = Is,
                             compiles = Cs, records = Rs, wilds = Ws, funs = Funs,
                             errors = Errs, warnings = Wgs, eof = EOF}) ->
    lists:append([[M], Es, Fs, Is, Cs, Rs, Ws, Funs, Errs, Wgs, [EOF]]).


%%------------------------------------------------------------------------------
%% @doc
%% Apply the specified advice to the provided function.
%%
%% TBD
%% @end
%%------------------------------------------------------------------------------
apply_advice({before, {PM, PF}}, {M, F, A}, State) ->
    {{fun2abs({M, F}, State), [{call, 0, fun2abs({PM, PF}, State), A}]}, State};
apply_advice({'after', {PM, PF}}, {M, F, A}, State) ->
    {{fun2abs({PM, PF}, State), [{call, 0, fun2abs({M, F}, State), A}]}, State};
apply_advice({around, {PM, PF}}, {M, F, A}, State0) ->
    case is_exported_fun({PF, length(A)}, State0) of
        true ->
            {{fun2abs({PM, PF}, State0), 
              [{atom, 0, M}, {atom, 0, F}, args2abs(A)]}, State0};
        false ->
            State = export_fun({F, length(A)}, State0),
            {{fun2abs({PM, PF}, State0), 
              [{atom, 0, M}, {atom, 0, F}, args2abs(A)]}, State}
    end.




%%==============================================================================
%% Functions to operate with forms
%%==============================================================================

is_imported_fun(Fun, #state{forms = #weaverl_forms{imports = Is}}) ->
    %% TODO: Check compile exclude auto-import
    case is_autoimported_bif(Fun) of
        true  ->
            {true, erlang};
        false ->
            case Is of
                [] ->
                    false;
                _  ->
                    case lists:keyfind(Fun, 1, all_imported_funs(Is)) of
                        false    -> false;
                        {Fun, M} -> {true, M}
                    end
            end
    end.

%% TODO: Not all functions exported by the erlang module are auto-imported bifs.
is_autoimported_bif(Fun) -> lists:member(Fun, erlang:module_info(exports)).

%% Iterates over all import attributes and returns a list
%% of all the imported functions in their {F, A} form.
all_imported_funs(IAttrs) ->
    lists:foldl(fun({attribute, _, imports, IFuns}, Acc) ->
                        Acc ++ IFuns
                end,
                [],
                IAttrs).

is_exported_fun({F, A}, #state{forms = #weaverl_forms{exports = Es}}) ->
    lists:member({F, A}, all_exported_funs(Es)).

%% Iterates over all export attributes and returns a list
%% of all the exported functions in their {F, A} form.
all_exported_funs(EAttrs) ->
    lists:foldl(fun({attribute, _, export, EFuns}, Acc) ->
                        Acc ++ EFuns
                end,
                [],
                EAttrs).

%% TODO: export_funs
export_fun(Fun, State) -> add_export_attr({attribute, 0, export, [Fun]}, State).

%%------------------------------------------------------------------------------
%% @doc
%% Converts a function call in abstract form to its {M, F, A} format.
%% end
%%------------------------------------------------------------------------------
abs2fun({{remote, _, {atom, _, M}, {atom, _, F}}, Args}, _State) ->
    {{M, F, length(Args)}, Args};
abs2fun({{atom, _, F}, Args}, State = #state{context = {M1, _, _}}) ->
    case is_imported_fun({F, length(Args)}, State) of
        {true, M2} -> {{M2, F, length(Args)}, Args};
        false      -> {{M1, F, length(Args)}, Args}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Given a function name and its module, it generates its abstract form.
%% @end
%%------------------------------------------------------------------------------
fun2abs({M, F}, #state{context = {M, _, _}})   -> {atom, 0, F};
fun2abs({M1, F}, #state{context = {M2, _, _}}) -> {remote, 0, {atom, 0, M1}, {atom, 0, F}}.

%% Convert a list of arguments into its abstract form counterpart.
args2abs([])        -> {nil, 0};
args2abs([A| Args]) -> {cons, 0,  A, args2abs(Args)}.

add_file_attr(File, State = #state{forms = Forms = #weaverl_forms{files = Files}}) ->
    State#state{forms = Forms#weaverl_forms{files = Files ++ [File]}}.

set_module_attr(Mod, State = #state{forms = Forms}) ->
    State#state{forms = Forms#weaverl_forms{module = Mod}}.

add_export_attr(Exp, State = #state{forms = Forms = #weaverl_forms{exports = Exps}}) ->
    State#state{forms = Forms#weaverl_forms{exports = Exps ++ [Exp]}}.

add_import_attr(Imp, State = #state{forms = Forms = #weaverl_forms{imports = Imps}}) ->
    State#state{forms = Forms#weaverl_forms{files = Imps ++ [Imp]}}.

add_compile_attr(C, State = #state{forms = Forms = #weaverl_forms{compiles = Cs}}) ->
    State#state{forms = Forms#weaverl_forms{compiles = Cs ++ [C]}}.

add_record_attr(R, State = #state{forms = Forms = #weaverl_forms{records = Rs}}) ->
    State#state{forms = Forms#weaverl_forms{records = Rs ++ [R]}}.

add_wild_attr(W, State = #state{forms = Forms = #weaverl_forms{wilds = Ws}}) ->
    State#state{forms = Forms#weaverl_forms{wilds = Ws ++ [W]}}.

add_fun(F, State = #state{forms = Forms = #weaverl_forms{funs = Fs}}) ->
    State#state{forms = Forms#weaverl_forms{funs = Fs ++ [F]}}.

add_warning(W, State = #state{forms = Forms = #weaverl_forms{warnings = Ws}}) ->
    State#state{forms = Forms#weaverl_forms{warnings = Ws ++ [W]}}.

add_error(E, State = #state{forms = Forms = #weaverl_forms{errors = Es}}) ->
    State#state{forms = Forms#weaverl_forms{errors = Es ++ [E]}}.

set_eof(EOF, State = #state{forms = Forms}) ->
    State#state{forms = Forms#weaverl_forms{eof = EOF}}.


%%==============================================================================
%% Abtract format tree traversal.
%%==============================================================================

process_form({attribute, L, module, M}, State = #state{context = {_, F, A}}) ->
    %% Module attribute is used to set the context
    set_module_attr({attribute, L, module, M}, State#state{context = {M, F, A}});   
process_form({attribute, L, export, EFuns}, State) ->
    add_export_attr({attribute, L, export, EFuns}, State);
process_form({attribute, L, import, IFuns}, State) ->
    add_import_attr({attribute, L, import, IFuns}, State);
process_form({function, L, Name, Arity, Clauses0}, State0 = #state{context = {M, _, _}}) ->
    %% Function name and arity are used to set the context
    {Clauses, State1} = 
        lists:foldl(
          fun(C0, {Cs, S0}) ->
                  {C, S} = process_clause(C0, S0),
                  {Cs ++ [C], S}
          end,
          {[], State0#state{context = {M, Name, Arity}}},
          Clauses0),
    add_fun({function, L, Name, Arity, Clauses}, State1);
process_form(Attr = {attribute, _L1, file, {_File, _L2}}, State) ->
    add_file_attr(Attr, State);
process_form(Attr = {attribute, _L, compile, _Options}, State) ->
    add_compile_attr(Attr, State);
process_form(Attr = {attribute, _L, record, _Record}, State) ->
    add_record_attr(Attr, State);
process_form(Attr = {attribute, _L, _A, _T}, State) ->
    add_wild_attr(Attr, State);
process_form({warning, W}, State) ->
    add_warning({warning, W}, State);
process_form({error, E}, State) ->
    add_error({error, E}, State);
process_form({eof, EOF}, State) ->
    set_eof({eof, EOF}, State).

process_clause({clause, L, H0, G, B0}, State0) ->
    {H, State1} = 
        lists:foldl(
          fun(P0, {H, S0}) ->
                  {P, S} = process_pattern(P0, S0),
                  {H ++ [P], S}
          end,
          {[], State0},
          H0),
    {B, State2} = 
        lists:foldl(
          fun(E0, {B, S0}) ->
                  {E, S} = process_expression(E0, S0),
                  {B ++ [E], S}
          end,
          {[], State1},
          B0),
    {{clause, L, H, G, B}, State2}.

process_pattern({match, Line, L0, R0}, State0) -> 
    {L, State1} = process_pattern(L0, State0),
    {R, State2} = process_pattern(R0, State1),
    {{match, Line, L, R}, State2};
process_pattern({cons, Line, H0, T0}, State0) -> 
    {H, State1} = process_pattern(H0, State0),
    {T, State2} = process_pattern(T0, State1),
    {{cons, Line, H, T}, State2};
process_pattern({tuple, Line, Ps0}, State0) ->
    {Ps, State1} = 
        lists:foldl(
          fun(P0, {B, S0}) ->
                  {P, S} = process_pattern(P0, S0),
                  {B ++ [P], S}
          end,
          {[], State0},
          Ps0),
    {{tuple, Line, Ps}, State1};
process_pattern({record, Line, Name, Pfs0}, State0) -> 
    {Pfs, State1} = pattern_fields(Pfs0, State0),
    {{record, Line, Name, Pfs}, State1};
process_pattern({record_index, Line, Name, Field0}, State0) -> 
    {Field, State1} = process_pattern(Field0, State0),
    {{record_index, Line, Name, Field}, State1};
process_pattern({record_field, Line, Rec0, Name, Field0}, State0) -> 
    {Rec, State1} = process_expression(Rec0, State0),
    {Field, State2} = process_expression(Field0, State1),
    {{record_field, Line, Rec, Name, Field}, State2};
process_pattern({bin, Line, Fs0}, State0) ->
    {Fs, State1} = pattern_grp(Fs0, State0),
    {{bin, Line, Fs}, State1};
process_pattern(Pattern, State) -> {Pattern, State}.  

pattern_grp([], State) -> {[], State};
pattern_grp([{bin_element, L, E0, default, T}| Fs0], State0) ->
    {E, State1} = process_expression(E0, State0),
    {Fs, State2} = process_expression(Fs0, State1),
    {[{bin_element, L, E, T}| Fs], State2};
pattern_grp([{bin_element, L, E0, S0, T}| Fs0], State0) ->
    {E, State1} = process_expression(E0, State0),
    {S, State2} = process_expression(S0, State1),
    {Fs, State3} = pattern_grp(Fs0, State2),
    {[{bin_element, L, E, S, T}| Fs], State3}.

pattern_fields([], State) -> {[], State};
pattern_fields([{record_field, Lf, {atom, La, F}, P0}| Pfs0], State0) ->
    {P, State1} = process_pattern(P0, State0),
    {Pfs, State2} = pattern_fields(Pfs0, State1),
    {[{record_field, Lf, {atom, La, F}, P}| Pfs], State2};
pattern_fields([{record_field, Lf, {var, La, '_'}, P0}| Pfs0], State0) ->
    {P, State1} = process_pattern(P0, State0),
    {Pfs, State2} = pattern_fields(Pfs0, State1),
    {[{record_field, Lf, {var, La, '_'}, P}| Pfs], State2}.

process_expression({cons, Line, H0, T0}, State0) -> 
    {H, State1} = process_expression(H0, State0),
    {T, State2} = process_expression(T0, State1),
    {{cons, Line, H, T}, State2};
process_expression({lc, Line, E0, Qs0}, State0) ->
    {Qs, State1} = 
        lists:foldl(
          fun({G, L, P0, Ex0}, {Qs, S0}) ->
                  {P, S1} = process_pattern(P0, S0),
                  {Ex, S2} = process_expression(Ex0, S1),
                  {Qs ++ [{G, L, P, Ex}], S2}
          end,
          {[], State0},
          Qs0),
    {E, State2} = process_expression(E0, State1),
    {{lc, Line, E, Qs}, State2};
process_expression({bc, Line, E0, Qs0}, State0) ->
    {Qs, State1} = 
        lists:foldl(
          fun({G, L, P0, Ex0}, {Qs, S0}) ->
                  {P, S1} = process_pattern(P0, S0),
                  {Ex, S2} = process_expression(Ex0, S1),
                  {Qs ++ [{G, L, P, Ex}], S2}
          end,
          {[], State0},
          Qs0),
    {E, State2} = process_expression(E0, State1),
    {{bc, Line, E, Qs}, State2};
process_expression({tuple, Line, Es0}, State0) ->
    {Es, State1} = 
        lists:foldl(
          fun(E0, {Es, S0}) ->
                  {E, S} = process_expression(E0, S0),
                  {Es ++ [E], S}
          end,
          {[], State0},
          Es0),
    {{tuple, Line, Es}, State1};
process_expression({record_index, Line, Name, Field0}, State0) -> 
    {Field, State1} = process_expression(Field0, State0),
    {{record_index, Line, Name, Field}, State1};
process_expression({record, Line, Name, Inits0}, State0) -> 
    {Inits, State1} = record_inits(Inits0, State0),
    {{record, Line, Name, Inits}, State1};
process_expression({record_field, Line, Rec0, Name, Field0}, State0) -> 
    {Rec, State1} = process_expression(Rec0, State0),
    {Field, State2} = process_expression(Field0, State1),
    {{record_field, Line, Rec, Name, Field}, State2};
process_expression({record, Line, Rec0, Name, Upds0}, State0) ->
    {Rec, State1} = process_expression(Rec0, State0),
    {Upds, State2} = record_updates(Upds0, State1),
    {{record, Line, Rec, Name, Upds}, State2};
process_expression({record_field, Line, Rec0, Field0}, State0) ->
    {Rec, State1} = process_expression(Rec0, State0),
    {Field, State2} = process_expression(Field0, State1),
    {{record_field, Line, Rec, Field}, State2};
process_expression({block, Line, Es0}, State0) -> 
    {Es, State1} = 
        lists:foldl(
          fun(E0, {Es, S0}) ->
                  {E, S} = process_expression(E0, S0),
                  {Es ++ [E], S}
          end,
          {[], State0},
          Es0),
    {{block, Line, Es}, State1};
process_expression({'if', Line, Cs0}, State0) ->
    {Cs, State1} = 
        lists:foldl(
          fun(C0, {Cs, S0}) ->
                  {C, S} = process_expression(C0, S0),
                  {Cs ++ [C], S}
          end,
          {[], State0},
          Cs0),
    {{'if', Line, Cs}, State1};
process_expression({'case', Line, E0, Cs0}, State0) ->
    {E, State1} = process_expression(E0, State0),
    {Cs, State2} = 
        lists:foldl(
          fun(C0, {Cs, S0}) ->
                  {C, S} = process_expression(C0, S0),
                  {Cs ++ [C], S}
          end,
          {[], State1},
          Cs0),
    {{'case', Line, E, Cs}, State2};
process_expression({'receive', Line, Cs0}, State0) ->
    {Cs, State1} = 
        lists:foldl(
          fun(C0, {Cs, S0}) ->
                  {C, S} = process_expression(C0, S0),
                  {Cs ++ [C], S}
          end,
          {[], State0},
          Cs0),
    {{'receive', Line, Cs}, State1};
process_expression({'receive', Line, Cs0, To0, ToEs0}, State0) ->
    {ToEs, State1} = 
        lists:foldl(
          fun(E0, {Es, S0}) ->
                  {E, S} = process_expression(E0, S0),
                  {Es ++ [E], S}
          end,
          {[], State0},
          ToEs0),
    {Cs, State2} = 
        lists:foldl(
          fun(C0, {Cs, S0}) ->
                  {C, S} = process_expression(C0, S0),
                  {Cs ++ [C], S}
          end,
          {[], State1},
          Cs0),
    {To, State3} = process_expression(To0, State2),
    {{'receive', Line, Cs, To, ToEs}, State3};
process_expression({'try', Line, Es0, Scs0, Ccs0, As0}, State0) ->
    {Es, State1} = 
        lists:foldl(
          fun(E0, {Es, S0}) ->
                  {E, S} = process_expression(E0, S0),
                  {Es ++ [E], S}
          end,
          {[], State0},
          Es0),
    {As, State2} = 
        lists:foldl(
          fun(A0, {As, S0}) ->
                  {A, S} = process_expression(A0, S0),
                  {As ++ [A], S}
          end,
          {[], State1},
          As0),
    {Ccs, State3} = 
        lists:foldl(
          fun(C0, {Ccs, S0}) ->
                  {C, S} = process_clause(C0, S0),
                  {Ccs ++ [C], S}
          end,
          {[], State2},
          Ccs0),
    {Scs, State4} = 
        lists:foldl(
          fun(C0, {Scs, S0}) ->
                  {C, S} = process_clause(C0, S0),
                  {Scs ++ [C], S}
          end,
          {[], State3},
          Scs0),
    {{'try', Line, Es, Scs, Ccs, As}, State4};
process_expression({'fun', Line, Body}, State0) ->
    case Body of
        {clauses, Cs0} ->
            {Cs, State1} = 
                lists:foldl(
                  fun(C0, {Cs, S0}) ->
                          {C, S} = process_clause(C0, S0),
                          {Cs ++ [C], S}
                  end,
                  {[], State0},
                  Cs0),
            {{'fun', Line, {clauses, Cs}}, State1};
        {function, F, A} ->
            {{'fun', Line, {function, F, A}}, State0};
        {function, M, F, A} when is_atom(M), is_atom(F), is_integer(A) ->
            {{'fun', Line, {function, M, F, A}}, State0};
        {function, M0, F0, A0} ->
            {M, State1} = process_expression(M0, State0),
            {F, State2} = process_expression(F0, State1),
            {A, State3} = process_expression(A0, State2),
            {{'fun', Line, {function, M, F, A}}, State3}
    end;
process_expression({call, Line, F0, As0}, State0) ->
    {F1, State1} = process_expression(F0, State0),
    {As1, State2} = 
        lists:foldl(
          fun(C0, {Cs, S0}) ->
                  {C, S} = process_expression(C0, S0),
                  {Cs ++ [C], S}
          end,
          {[], State1},
          As0),
    {{F, As}, State3} = handle_call(abs2fun({F1, As1}, State2), State2),
    {{call, Line, F, As}, State3};
process_expression({'catch', Line, E0}, State0) -> 
    {E, State1} = process_expression(E0, State0),
    {{'catch', Line, E}, State1};
process_expression({match, Line, P, E0}, State0) -> 
    {E, State1} = process_expression(E0, State0),
    {{match, Line, P, E}, State1};
process_expression({bin, Line, Fs}, State) -> 
    {{bin, Line, Fs}, State};
process_expression({op, Line, Op, A0}, State0) ->
    {A, State1} = process_expression(A0, State0), 
    {{op, Line, Op, A}, State1};
process_expression({op, Line, Op, L0, R0}, State0) -> 
    {L, State1} = process_expression(L0, State0),
    {R, State2} = process_expression(R0, State1),
    {{op, Line, Op, L, R}, State2};
process_expression({remote, Line, M0, F0}, State0) ->
    {M, State1} = process_expression(M0, State0),
    {F, State2} = process_expression(F0, State1),
    {{remote, Line, M, F}, State2};
process_expression(Expr, State) -> 
    {Expr, State}.

record_inits([], State) -> {[], State};
record_inits([{record_field, Lf, {atom, La, F}, Val0}| Is0], State0) ->
    {Val, State1} = process_expression(Val0, State0),
    {Is, State2} = record_inits(Is0, State1),
    {[{record_field, Lf, {atom, La, F}, Val}| Is], State2};
record_inits([{record_field, Lf, {var, La, '_'}, Val0}| Is0], State0) ->
    {Val, State1} = process_expression(Val0, State0),
    {Is, State2} = record_inits(Is0, State1),
    {[{record_field, Lf, {var, La, '_'}, Val}| Is], State2}.

record_updates([], State) -> {[], State};
record_updates([{record_field, Lf, {atom, La, F}, Val0}| Us0], State0) ->
    {Val, State1} = process_expression(Val0, State0),
    {Us, State2} = record_inits(Us0, State1),
    {[{record_field, Lf, {atom, La, F}, Val}| Us], State2}.


