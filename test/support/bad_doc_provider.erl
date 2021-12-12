-module(bad_doc_provider). 

-export([init/1,
         do/1,
         format_error/1]).

init(State) -> 
    Provider = providers:create([{name, bad_doc},
                                 {module, ?MODULE},
                                 {namespace, default},
                                 {bare, true},
                                 {deps, [compile]},
                                 {example, "bad_doc_provider good_luck"},
                                 {short_desc, "I behave badly"},
                                 {desc, "Nothing but failure all day long"},
                                 {profiles, [docs]},
                                 {opts, []}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

do(_) -> {error, eh}.

format_error(_) -> eh.
