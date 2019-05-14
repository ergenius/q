%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jul 2017 8:46 AM
%%%-------------------------------------------------------------------
-author("madalin").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Q translate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Sets the language for the current process
%% The specified language will be used by ?Q_T(Param) macro into the current process.
%% Other processes languages are not affected.
-define(Q_T_SET_LANGUAGE(Language), erlang:put(q_language, Language)).

%% Translate the specified string, binary or translation id into current process language.
%%
%% Our translation system will use the parameter as a key for locating the proper translation into translations database.
%% If no translation could be found, the parameter converted to binary is returned.
%%
%% All Q list or binaries that may require translation must be enclosed using this macro or ?Q_T(Format, Params) macro.
%% Calling process can set it's current language by calling ?Q_T_SET_LANGUAGE(Language) macro.
%%
%% == Usage examples ==
%%
%% Q_T(submit_button)
%% Q_T("Submit")
%% Q_T(<<"Submit">>)
-define(Q_T(Key), q_i18n_translator:translate(?MODULE, erlang:get(q_language), Key)).

%% Format the specified parameters according to the current process language.
%%
%% Calling process can set it's current language by calling ?Q_T_SET_LANGUAGE(Language) macro.
%%
%% == Macro parameters ==
%%
%% - Key -
%% The key must be an atom, binary or list unique for the module the macro is used in. Key is used to locate
%% the translation into the proper module language file. If the key can not be found, the system will
%% not crash and handle this situation by returning <<"MISSING TRANSLATION {Key}">> binary.
%%
%% - Params -
%% Must contain a proplist with all parameters to be inserted into the translation.
%% Params list must be indexed by keys containing params names, and data. Data can simply contain
%% parameters value or a proplist of parameter value and additional type information for the translation
%% system to understand how complex parameters like integer times should be formatted. If type information is not
%% provided, the translation system will automatically detect the best way to display the specified parameter according
%% to the current language rules.
%%
%% == Translation modules ==
%%
%% Translation modules are erlang modules. Translation modules are implemented into native erlang codes and they are very fast.
%% Most of the time translating something will simply involve a native erlang function. No complex parsing
%% or tables lookups are needed to translate something. This is as fast you can get and translating strings will have almost zero
%% performance impact on your code.
%%
%% Translation modules can be generated and compilled from more simple translation files than can be
%% easily handled by translators.
%%
%% Translators with basic erlang knowledge can also directly write or alter complicated translation modules
%% very easy.
%%
%% == Examples ==
%%
%% Simple translation containing one counter and no parameter type information:
%% Entries in english translation source file:
%% {dogs_counter, [
%%     {template, <<"There {dogs_count}.">>},
%%     {dogs_count, [
%%          {{equal, 1}, <<"is one dog">>},
%%          {{equal, 2}, <<"are two dogs">>},
%%          {{interval, 3, 5}, <<"are a few dogs">>},
%%          {{greater, 999999}, <<"are millions of dogs">>},
%%          {default, <<"are {value} dogs">>}
%%     ]}
%% ]}
%% Output of the ?Q_T() macro for dogs_counter entry.
%% ?Q_T(dogs_counter, [{dogs_count, 1}]) -> <<"There is one dog.">>.
%% ?Q_T(dogs_counter, [{dogs_count, 2}]) -> <<"There are two dogs.">>.
%% ?Q_T(dogs_counter, [{dogs_count, 3}]) -> <<"There are a few dogs.">>.
%% ?Q_T(dogs_counter, [{dogs_count, 4}]) -> <<"There are a few dogs.">>.
%% ?Q_T(dogs_counter, [{dogs_count, 5}]) -> <<"There are a few dogs.">>.
%% ?Q_T(dogs_counter, [{dogs_count, 234}]) -> <<"There are 234 dogs.">>.
%% ?Q_T(dogs_counter, [{dogs_count, 20000000}]) -> <<"There are millions of dogs.">>.
%%
%% Translation containing many different counters and parameters:
%% Entries in english translation source file:
%% {dogs_and_cats_counter, [
%%     {template, <<"At {time} there {dogs_count} and {cats_count} in the {box_name}.">>},
%%     {dogs_count, [
%%          {{less, 1}, <<"where no dogs">>},
%%          {{equal, 1}, <<"where one dog">>},
%%          {{equal, 2}, <<"where two dogs">>},
%%          {{interval, 3, 5}, <<"where a few dogs">>},
%%          {{greater, 999999}, <<"where millions of dogs">>},
%%          {default, <<"where {value} dogs">>}
%%     ]},
%%     {cats_count, [
%%          {{less, 1}, <<"no cats">>},
%%          {{equal, 1}, <<"one cat">>},
%%          {{equal, 2}, <<"two cats">>},
%%          {{interval, 3, 5}, <<"a few cats">>},
%%          {{greater, 999999}, <<"millions of cats">>},
%%          {default, <<"{value} cats">>}
%%     ]}
%% ]}
%% ?Q_T(dogs_and_cats_counter, [{dogs_count, 0}, {cats_count, 0}, {box_name, "red box"}, {date, [{unit, unix_second}, {format, full}, {value, 1491453893}]}]) ->
%% <<"At Thursday, 06-Apr-17 there where no dogs and no cats in the red box.">>.
%% ?Q_T(dogs_and_cats_counter, [{dogs_count, 1}, {cats_count, 1}, {box_name, "black box"}, {time, [{unit, unix_second}, {format, long}, {value, 1491453893}]}]) ->
%% <<"There where one dog and one cat in the black box.">>.
%% ?Q_T(dogs_and_cats_counter, [{dogs_count, 2}, {cats_count, 1}, {box_name, "bag"}, {time, [{unit, unix_second}, {format, medium}, {value, 1491453893}]}]) ->
%% <<"There where two dogs and one cat in the bag.">>.
%% ?Q_T(dogs_and_cats_counter, [{dogs_count, 12}, {cats_count, 32}, {box_name, "white car"}, {time, [{unit, unix_second}, {format, short}, {value, 1491453893}]}]) ->
%% <<"There where 12 dogs and 32 cats in the white car.">>.
%% ?Q_T(dogs_and_cats_counter, [{dogs_count, 586777584}, {cats_count, 99990000}, {box_name, "red box"}, {time, [{unit, unix_millisecond}, {value, 1491453893000}]}]) ->
%% <<"There where millions of dogs and millions of cats in the red box.">>.
-define(Q_T(Key, Params), q_i18n_translator:translate(?MODULE, Key, Params, erlang:get(q_language))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Q errors, logging and exceptions
%%
%% Using native Erlang logging functions may result in a high amount of events being send to
%% various error logger handlers. This can easily become overwhelming for systems that generate a big amount of logs.
%%
%% Even if your error handler is written to ignore some particular type of log events, the error logging handler will still be
%% flooded with large amount of data moving around and consuming unnecessary resources. In order to avoid this we use our
%% own error logging facility based on macros that you can abuse in testing and completely disable in production
%% releases with no need to modify your code or comment out your unnecessary error_logger calls.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================
%% Q log
%%=============================

%% Q log record
-record(q_log, {
    namespace       = q :: atom() | binary(),
    code 	        = error :: atom() | binary(),
    type            = debug :: debug | warning | error | exception,
    params          = undefined :: undefined | [{atom() | binary() | string(), any()}],
    application     = undefined :: undefined | atom(),
    module          = undefined :: undefined | atom(),
    file            = undefined :: undefined | string(),
    line            = undefined :: undefined | integer(),
    function_name   = undefined :: undefined | atom(),
    function_arity  = undefined :: undefined | integer()
}).

%% Q log type
-type q_log() :: #q_log{}.

%% Q log create macros
-ifdef(q_debug).
-define(Q_LOG(Type, Namespace, Code, Params), q_logger:log(Type, Namespace, Code, Params, ?MODULE, ?FILE, ?LINE, ?FUNCTION_NAME, ?FUNCTION_ARITY)).
-else.
-define(Q_LOG(Type, Namespace, Code, Params),
    case Type of
        error -> q_logger:log(Type, Namespace, Code, Params, ?MODULE, ?FILE, ?LINE, ?FUNCTION_NAME, ?FUNCTION_ARITY);
        _ -> ok
    end).
-endif.

-define(Q_LOG(Type, Namespace, Code), ?Q_LOG(Type, Namespace, Code, [])).

%%=============================
%% Q error
%%=============================

%% Q error create macros
-define(Q_ERROR(), {error, undefined}).
-define(Q_ERROR(Info),
	case Info of
		{error, _} -> Code;
		_ -> {error, Info}
	end).

%%=============================
%% Q exception
%%=============================

%% Q exception record
-record(q_exception, {
    namespace   = q :: atom() | binary(),
    code 	    = error :: atom() | binary(),
    params      = undefined :: undefined | [{atom() | binary() | string(), any()}]
}).

%% Q exception type
-type q_exception() :: #q_exception{}.

%% Q throw exception macro
-define(Q_THROW(), throw(#q_exception{})).
-define(Q_THROW(Code), throw(#q_exception{code = Code})).
-define(Q_THROW(Namespace, Code), throw(#q_exception{namespace = Namespace, code = Code})).
-define(Q_THROW(Namespace, Code, Params), throw(#q_exception{namespace = Namespace, code = Code, params = Params})).

%%=============================
%% Q safe apply/call
%%=============================

%% Q safe apply macros
-define(Q_SAFE_APPLY(Module, Function, Params), try erlang:apply(Module, Function, Params) catch _:_ -> ?Q_ERROR(safe_apply_exception) end).

%% Q safe call macros. If the number of arguments are known at compile time, the call is better made using call macro.
-define(Q_SAFE_CALL(Module, Function, P1), try Module:Function(P1) catch _:_ -> ?Q_ERROR(safe_call_exception) end).
-define(Q_SAFE_CALL(Module, Function, P1, P2), try Module:Function(P1, P2) catch _:_ -> ?Q_ERROR(safe_call_exception) end).
-define(Q_SAFE_CALL(Module, Function, P1, P2, P3), try Module:Function(P1, P2, P3) catch _:_ -> ?Q_ERROR(safe_call_exception) end).
-define(Q_SAFE_CALL(Module, Function, P1, P2, P3, P4), try Module:Function(P1, P2, P3, P4) catch _:_ -> ?Q_ERROR(safe_call_exception) end).
-define(Q_SAFE_CALL(Module, Function, P1, P2, P3, P4, P5), try Module:Function(P1, P2, P3, P4, P5) catch _:_ -> ?Q_ERROR(safe_call_exception) end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Q PATHS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Path to config directory
-define(Q_PATH_CONFIG,    application:get_env(q, path_config)).

%% Path to data directory
-define(Q_PATH_DATA,      application:get_env(q, path_data)).

%% Path to temporary directory
-define(Q_PATH_TMP,       application:get_env(q, path_tmp)).

%% Path to assets directory
-define(Q_PATH_ASSETS,    application:get_env(q, path_assets)).

%% Path to views directory
-define(Q_PATH_VIEWS,     application:get_env(q, path_views)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(q_extension_specs, {
  type    = undefined :: undefined | q_extension_type(),
  config  = undefined :: undefined | proplists()
}).
-type q_extension_specs() :: #q_extension_specs{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Q ETS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ETS storing extensions
-define(Q_ETS_EXTENSIONS,               q_ets_extensions).

%% ETS storing cache keyspaces
-define(Q_ETS_CACHE_KEYSPACES,          q_ets_cache_keyspaces).

%% ETS containing translator texts
-define(Q_ETS_I18N_TRANSLATOR_TEXTS,    q_ets_i18n_translator_texts).

%% ETS containing file assets
-define(Q_ETS_ASSETS_FILES,             q_ets_assets_files).

%% ETS containing file consult assets
-define(Q_ETS_ASSETS_FILES_CONSULT,     q_ets_assets_files_consult).

%% ETS containing compiled templates assets
-define(Q_ETS_ASSETS_TEMPLATES,         q_ets_assets_templates).

%% ETS storing universe, clusters, nodes and local options merged
-define(Q_ETS_CONFIG_OPTIONS,           q_ets_config_options).

%% ETS storing nodes options
-define(Q_ETS_CONFIG_OPTIONS_NODES,     q_ets_config_options).

%% ETS storing clusters options
-define(Q_ETS_CONFIG_OPTIONS_CLUSTERS,  q_ets_config_options).

%% ETS storing universe options
-define(Q_ETS_CONFIG_OPTIONS_UNIVERSE,  q_ets_config_options).

%% List containing all ETS that must be created when Q starts
%% ETS are optimized based on their usage. Benchmarks where done to pick up the best settings.
%% If you add any new Q ETS please add it to this table and the ETS will be automatically created
%% when Q starts.
-define(Q_ETS_LIST, [

    {?Q_ETS_CACHE_KEYSPACES,            [set, named_table, public, {read_concurrency, true}]},
    {?Q_ETS_I18N_TRANSLATOR_TEXTS,      [set, named_table, public, {read_concurrency, true}]},

    {?Q_ETS_ASSETS_FILES,               [set, named_table, public, {read_concurrency, true}]},
    {?Q_ETS_ASSETS_FILES_CONSULT,       [set, named_table, public, {read_concurrency, true}]},
    {?Q_ETS_ASSETS_TEMPLATES,           [set, named_table, public, {read_concurrency, true}]},

    {?Q_ETS_CONFIG_OPTIONS,             [set, named_table, public, {read_concurrency, true}]},
    {?Q_ETS_CONFIG_OPTIONS_NODES,       [set, named_table, public, {read_concurrency, true}]},
    {?Q_ETS_CONFIG_OPTIONS_CLUSTERS,    [set, named_table, public, {read_concurrency, true}]},
    {?Q_ETS_CONFIG_OPTIONS_UNIVERSE,    [set, named_table, public, {read_concurrency, true}]}

]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Q MODULES PREFIX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(Q_MODULE_PREFIX_STORAGE_DRIVER, <<"qext_storage_namespace_">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Q serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(Q_SERIALIZE_TYPE_CONSULT, consult). %% Erlang terms, separated by '.'
-define(Q_SERIALIZE_TYPE_ETF,     etf).     %% Erlang External Term Format
-define(Q_SERIALIZE_TYPE_BERT,    bert).    %% BERT (Binary ERlang Term)
-define(Q_SERIALIZE_TYPE_BERP,    berp).    %% BERP (Binary ERlang Packet)
-define(Q_SERIALIZE_TYPE_JSON,    json).    %% Json

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STORAGE KEYSPACE DEFINITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% A keyspace with high QOS
%% The system will not be able to recover and function properly in the event of loss of data from this keyspace
%% and the proper functionality of the entire system will be hardly affected.
-define(Q_STORAGE_KEYSPACE_QOS_HIGH,   <<"qosh">>).

%% A keyspace with medium qos.
%% The system should be able to recover and function in the event of complete loss of data from this keyspace
%% but the functionality will be hardly affected.
-define(Q_STORAGE_KEYSPACE_QOS_MEDIUM,   <<"qosm">>).

%% A keyspace with the lowest qos.
%% The system should be able to recover and function in the event of complete loss of data from this keyspace
%% and the functionality of the system will be only superficially disrupted.
-define(Q_STORAGE_KEYSPACE_QOS_LOW,      <<"qosl">>).

%% List containing all keyspaces. If you add a keyspace please also add the keyspace to this list.
-define(Q_STORAGE_KEYSPACES, [
    {?Q_STORAGE_KEYSPACE_QOS_HIGH, <<"CREATE KEYSPACE qosh WITH REPLICATION = { 'class' : 'NetworkTopologyStrategy', 'dc1' : 1 }">>},
    {?Q_STORAGE_KEYSPACE_QOS_MEDIUM, <<"CREATE KEYSPACE qosm WITH REPLICATION = { 'class' : 'NetworkTopologyStrategy', 'dc1' : 1 }">>},
    {?Q_STORAGE_KEYSPACE_QOS_LOW, <<"CREATE KEYSPACE qosl WITH REPLICATION = { 'class' : 'NetworkTopologyStrategy', 'dc1' : 1 }">>}
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TIME
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Seconds corresponding to the Q epoch on January 1st, 2017 at UTC
%% 1483228800 seconds from Unix epoch
-define(Q_TIME_SECONDS_Q_EPOCH,             1483228800).

%% Milliseconds corresponding to the Q epoch on January 1st, 2017 at UTC
-define(Q_TIME_MILLISECONDS_Q_EPOCH,        1483228800000).

%% Microseconds corresponding to the Q epoch on January 1st, 2017 at UTC
-define(Q_TIME_MICROSECONDS_Q_EPOCH,        1483228800000000).

%% Nanoseconds corresponding to the Q epoch on January 1st, 2017 at UTC
-define(Q_TIME_NANOSECONDS_Q_EPOCH,         1483228800000000000).

%% Datetime corresponding to the Q epoch on January 1st, 2017 at UTC
-define(Q_DATETIME_Q_EPOCH,                 {{2017, 1, 1}, {0, 0, 0}}).

%% Seconds corresponding to the Unix Epoch on January 1st, 1970 at UTC
%% This value is returned by calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(Q_TIME_SECONDS_UNIX_EPOCH,          62167219200).

-record(q_srv_time_strict_monotonic_state, {
    io_device         = undefined :: undefined | term(),
    last_nanoseconds  = undefined :: undefined | pos_integer(),
    last_milliseconds = undefined :: undefined | pos_integer(),
    last_microseconds = undefined :: undefined | pos_integer(),
    last_seconds      = undefined :: undefined | pos_integer()
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% validator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(q_validator_rule, {

    %% Validator rule type
    type = q_validator_rule :: atom(),

    %% list of attributes to be validated
    attributes = none :: none | all | [binary()],

    %% Validator error code.
    %% The validator fun may return custom error messages and error codes that can overwrite this one.
    error_code = <<>>,

    %% Validator error message. Different validators may define various
    %% placeholders in the message that are to be replaced with actual values.
    %% The validator fun may return custom error messages and error codes that can overwrite this one.
    error_message = <<>>,

    %% true to skip validating empty values or false otherwise
    allow_empty = false,

    %% Always validate the specified values
    allow_list = undefined :: undefined | [term()],

    %% Always fail validation for the specified values
    deny_list = undefined :: undefined | [term()],

    %% true if the validation chain should be stopped when this validation rule trigger an error or false otherwise.
    %% Defaults to false.
    stop_on_error = false,

    %% true whether this validation rule should be skipped when there is already a validation
    %% error for the current attribute. Defaults to false.
    skip_on_error = false,

    %% List of scenarios names that the validator should not be applied to.
    %% This list has priority over scenarios_on list
    skip_scenarios = none :: none | all | binary() | [binary()],

    %% List of scenarios names that the validator should be applied to.
    apply_scenarios = all :: none | all | [binary()],

    %% boolean whether attributes listed with this validator should be considered safe for massive assignment. Defaults to true.
    safe = true :: true | false,

    %% boolean whether to perform client-side validation. Defaults to true.
    enable_client_validation = true :: true | false,

    %% Validation fun
    fun_validate = fun(Validator, ValidatorRule, Model) -> ok end :: fun((q_validator(), q_validator_rule(), q_node()) -> ok | error | {error, binary(), binary()})

}).
-type q_validator_rule() :: #q_validator_rule{}.

-record(q_validator, {

    %% Validator type
    type = q_validator :: atom(),

    %% Validator rules
    rules   = [] :: [q_validator_rule()],

    %% Validator errors
    errors  = [] :: [q_validator_error()],

    %% True to skip the remaining validation rules when there is already any validation error into the validator or false otherwise.
    %% Defaults to false.
    skip_on_error = false :: true | false,

    %% True to stop any remaining validation when the first validation error is encountered in a validation rule.
    %% Defaults to false.
    stop_on_error = false :: true | false

}).
-type q_validator() :: #q_validator{}.

%% Q validator error
-record(q_validator_error, {
	code 	        = error :: atom() | binary(),
	params          = undefined :: undefined | [{atom() | binary() | string(), any()}],
	msg             = undefined :: undefined | list() | binary()
}).

%% Q error type
-type q_validator_error() :: #q_validator_error{}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GENDER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Biological sex
-define(Q_GENDER_BIOLOGICAL_SEX_UNDECLARED,      0).
-define(Q_GENDER_BIOLOGICAL_SEX_MALE,            1).
-define(Q_GENDER_BIOLOGICAL_SEX_FEMALE,          2).
-define(Q_GENDER_BIOLOGICAL_SEX_HERMAPHRODITE,   3).

-type q_gender_biological_sex() :: ?Q_GENDER_BIOLOGICAL_SEX_UNDECLARED..?Q_GENDER_BIOLOGICAL_SEX_HERMAPHRODITE.

-define(Q_GENDER_IDENTITY_UNDECLARED,             0).
-define(Q_GENDER_IDENTITY_AGENDER,                1).
-define(Q_GENDER_IDENTITY_ANDROGYNE,              2).
-define(Q_GENDER_IDENTITY_AROMANTIC,              3).
-define(Q_GENDER_IDENTITY_ASEXUAL,                4).
-define(Q_GENDER_IDENTITY_BIGENDER,               5).
-define(Q_GENDER_IDENTITY_BISEXUAL,               6).
-define(Q_GENDER_IDENTITY_CROSS_DRESSER,          7).
-define(Q_GENDER_IDENTITY_DEMIROMANTIC,           8).
-define(Q_GENDER_IDENTITY_DEMISEXUAL,             9).
-define(Q_GENDER_IDENTITY_FEMALE,                 10).
-define(Q_GENDER_IDENTITY_GAY,                    11).
-define(Q_GENDER_IDENTITY_GENDER_BENDER,          12).
-define(Q_GENDER_IDENTITY_GENDERQUEER,            13).
-define(Q_GENDER_IDENTITY_HIJRA,                  14).
-define(Q_GENDER_IDENTITY_LESBIAN,                15).
-define(Q_GENDER_IDENTITY_MALE,                   16).
-define(Q_GENDER_IDENTITY_PANGENDER,              17).
-define(Q_GENDER_IDENTITY_THIRD_GENDER,           18).
-define(Q_GENDER_IDENTITY_TRANS_MAN,              19).
-define(Q_GENDER_IDENTITY_TRANS_WOMAN,            20).
-define(Q_GENDER_IDENTITY_TRANSFEMININE,          21).
-define(Q_GENDER_IDENTITY_TRANSMASCULINE,         22).
-define(Q_GENDER_IDENTITY_TRANSSEXUAL,            23).
-define(Q_GENDER_IDENTITY_TRIGENDER,              24).

-type q_gender_identity() :: ?Q_GENDER_IDENTITY_UNDECLARED..?Q_GENDER_IDENTITY_TRIGENDER.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% user roles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(Q_USER_ROLE_DEFAULT,        <<"user">>).
-define(Q_USER_ROLE_ROOT,           <<"root">>).
-define(Q_USER_ROLE_ADMINISTRATOR,  <<"admin">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Holds Q node attribute specifications
-record(q_node_attribute_spec, {
    name        = undefined,
    type        = undefined,
    unique      = false,
    optional    = true,
    default     = null,

    %% Custom attribute label
    %% If this is undefined labels are automatically generated from attributes names.
    %% This is done by replacing everything else except alphabetic and numeric characters with space
    %% and changing the first letter of each word to upper case.
    %% For example, 'department_name' or 'DepartmentName' will generate 'Department Name'.
    %%
    %% The generation routine supports unicode attributes names, alphabetic letter from all languages
    %% are automatically detected.
    label       = undefined,

    %% Attribute description
    description = undefined
}).

-type q_node_attribute_spec() :: #q_node_attribute_spec{}.

%% Holds Q node key specifications
-record(q_node_key_spec, {
    attributes              = undefined,
    unique                  = false :: true | false,
    type                    = default :: default | textsearch | location,
    replicate_attributes    = false
}).

-type q_node_key_spec() :: #q_node_key_spec{}.

%% This record holds Q node type specifications
-record(q_node_type_spec, {

    %% Storage keyspace
    storage_keyspace = <<"default">> :: binary(),

    %% Storage default insert TTL
    storage_ttl = 0 :: integer(),

    %% Storage time table type
    storage_time_table_type = permanent :: permanent | minute | hour | day | week | month,

    %% Storage time table archive behaviour
    storage_time_table_archive = keep :: keep | delete | archive,

    %% Enable or disable caching
    %%
    %% 'undefined' to disable model caching or another atom specifying the q_cache module to be used for caching this model
    %% If cache is enabled the model is stored into the specified cache and can be later loaded from the cache very fast.
    %% Model CRUD operations will automatically inform the cache module about any model modifications.
    cache = false :: true | false,

    %% Cache ttl
    %%
    %% 'default' atom to use the default cache module TTL or integer value containing the number of seconds to store this model into cache.
    %% If model cache is enabled this holds the TTL used when for storing this module into cache.
    cache_ttl = default,

    %% Attributes
    %%
    %% Proplist containing attributes specifications
    attributes = [] :: [q_node_attribute_spec()],

    %% Keys
    %%
    %% List containing model attributes that can be used as keys
    keys = [] :: [q_node_key_spec()]

}).

-type q_node_type_spec() :: #q_node_type_spec{}.



%% Nodes types
-define(Q_NODES_TYPES_SPEC, [

    %% user
    {<<"user">>, #q_node_type_spec{

        storage_keyspace                = ?Q_STORAGE_KEYSPACE_QOS_HIGH,
        storage_ttl                     = 0,
        storage_time_table_type         = permanent,
        storage_time_table_archive      = keep,
        cache                           = true,
        cache_ttl                       = default,
        attributes                      = [
            #q_node_attribute_spec{
                name        = role,
                type        = varchar,
                unique      = false,
                optional    = true,
                default     = ?Q_USER_ROLE_DEFAULT
            },
            #q_node_attribute_spec{
                name        = username,
                type        = varchar,
                unique      = true,
                optional    = true,
                default     = null
            },
            #q_node_attribute_spec{
                name        = phone,
                type        = varchar,
                unique      = true,
                optional    = true,
                default     = null
            },
            #q_node_attribute_spec{
                name        = password,
                type        = varchar,
                unique      = false,
                optional    = true,
                default     = {auto, password}
            },
            #q_node_attribute_spec{
                name        = email,
                type        = varchar,
                unique      = true,
                optional    = true,
                default     = null
            },
            #q_node_attribute_spec{
                name        = first_name,
                type        = varchar,
                unique      = false,
                optional    = true,
                default     = null
            },
            #q_node_attribute_spec{
                name        = last_name,
                type        = varchar,
                unique      = false,
                optional    = true,
                default     = null
            },
            #q_node_attribute_spec{
                name        = birth_year,
                type        = int,
                unique      = false,
                optional    = true,
                default     = 0
            },
            #q_node_attribute_spec{
                name        = birth_month,
                type        = int,
                unique      = false,
                optional    = true,
                default     = 0
            },
            #q_node_attribute_spec{
                name        = birth_day,
                type        = int,
                unique      = false,
                optional    = true,
                default     = 0
            },
            #q_node_attribute_spec{
                name        = birth_hour,
                type        = int,
                unique      = false,
                optional    = true,
                default     = -1
            },
            #q_node_attribute_spec{
                name        = birth_minute,
                type        = int,
                unique      = false,
                optional    = true,
                default     = -1
            },
            #q_node_attribute_spec{
                name        = gender_sex,
                type        = int,
                unique      = false,
                optional    = true,
                default     = ?Q_GENDER_BIOLOGICAL_SEX_UNDECLARED
            },
            #q_node_attribute_spec{
                name        = gender_identity,
                type        = int,
                unique      = false,
                optional    = true,
                default     = ?Q_GENDER_IDENTITY_UNDECLARED
            },
            #q_node_attribute_spec{
                name        = country,
                type        = varchar,
                unique      = false,
                optional    = true,
                default     = null
            },
            #q_node_attribute_spec{
                name        = fid_profile_picture,
                type        = varchar,
                unique      = false,
                optional    = true,
                default     = null
            },
            #q_node_attribute_spec{
                name        = status,
                type        = int,
                unique      = false,
                optional    = true,
                default     = null
            }
        ],
        keys = [
            #q_node_key_spec{
                attributes              = username,
                unique                  = true,
                replicate_attributes    = false
            },
            #q_node_key_spec{
                attributes              = email,
                unique                  = true,
                replicate_attributes    = false
            },
            #q_node_key_spec{
                attributes              = [phone_country_code, phone],
                unique                  = true,
                replicate_attributes    = false
            }
        ]

    }}

]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Q request
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This record holds Q request information
-record(q_request, {

    %% Request underlying transport protocol
    transport_protocol = undefined :: undefined | tcp | ssl | udp,

    %% Request application layer protocol
    application_protocol = undefined :: undefined | htttp | websocket,

    %% Request state
    %%
    %% Depends on underlying transport protocol and application layer protocol
    %% A q request allows a common usage of many protocols without the caller to be aware
    %% of the underlying transport or application protocol
    state = undefined :: undefined | term()

}).

-type q_request() :: #q_request{}.

%% This record holds Q http request information
-record(q_request_http, {

	%% Request time
	time                        = undefined,

	%% Content type expected by remote user agent
	content_type                = <<"text/html">>,

	%% Method invoked by remote user agent
	method                      = undefined,

	%% Remote user info
	remote_ip                   = undefined,
	remote_port                 = undefined,
	remote_user_agent           = undefined,

	%% HTTP stack
	http_stack                  = cowboy,

	%% Current requested path
	path                        = undefined,

	%% Current extension
	extension                   = undefined,

	%% Current extension module
	extension_module            = undefined,

	%% Current extension module operation
	extension_module_operation  = undefined,

	%% Current extension module full name (binary)
	extension_module_name       = undefined,

	%% Params that must be perpetuated if set between all modules of current extension
	extension_keep_params       = [],

	%% A proplist that by default contains the contents of variables_get, variables_post and variables_cookies
	variables                   = undefined,
	%% A proplist that contains the request headers
	variables_headers           = undefined,
	%% A proplist that contains the request post variables
	variables_post              = undefined,
	%% A proplist that contains the request get variables
	variables_get               = undefined,
	%% A proplist that contains the request cookies variables
	variables_cookies           = undefined,

	%% Current session
	session                     = undefined

}).

-type q_request_http() :: #q_request_http{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Q cache storage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(q_cache_keyspace, {
    name            = undefined :: atom(),
    storage_module  = q_cache_storage_ram :: atom(),
    storage_options = []
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Q various servers states
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% State of the file system safe file server
-record(q_fs_srv_safe_file_state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Q UNIVERSE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(q_universe_bridge, {
  cluster     = undefined :: undefined | binary(),
  node        = undefined :: undefined | node()
}).
-type q_universe_bridge() :: #q_universe_bridge{}.

-record(q_universe_node, {
    cluster     = undefined :: undefined | binary(),
    node        = undefined :: undefined | node()
}).
-type q_universe_node() :: #q_universe_node{}.

-record(q_universe_pid, {
    cluster     = undefined :: undefined | binary(),
    pid         = undefined :: undefined | pid()
}).
-type q_universe_pid() :: #q_universe_pid{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STORAGE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(Q_STORAGE_CONSISTENCY_ANY,            any).
-define(Q_STORAGE_CONSISTENCY_LOCAL_QUORUM,   local_quorum).
-define(Q_STORAGE_CONSISTENCY_GLOBAL_QUORUM,  global_quorum).
-define(Q_STORAGE_CONSISTENCY_ONE,            one).
-define(Q_STORAGE_CONSISTENCY_TWO,            two).
-define(Q_STORAGE_CONSISTENCY_THREE,          three).
-define(Q_STORAGE_CONSISTENCY_ALL,            all).
-define(Q_STORAGE_CONSISTENCY_DEFAULT_DELETE, ?Q_STORAGE_CONSISTENCY_LOCAL_QUORUM).
-type q_storage_consistency() :: ?Q_STORAGE_CONSISTENCY_ANY |
  ?Q_STORAGE_CONSISTENCY_LOCAL_QUORUM |
  ?Q_STORAGE_CONSISTENCY_GLOBAL_QUORUM |
  ?Q_STORAGE_CONSISTENCY_ONE |
  ?Q_STORAGE_CONSISTENCY_TWO |
  ?Q_STORAGE_CONSISTENCY_THREE |
  ?Q_STORAGE_CONSISTENCY_ALL.

-define(Q_STORAGE_NAMESPACE_STATUS_AVAILABLE,        available).
-define(Q_STORAGE_NAMESPACE_STATUS_UNAVAILABLE,      unavailable).
-type q_storage_namespace_status() :: ?Q_STORAGE_NAMESPACE_STATUS_AVAILABLE | ?Q_STORAGE_NAMESPACE_STATUS_UNAVAILABLE.

-type q_storage_driver_name() :: binary() | atom() | list().
-type q_storage_namespace_name() :: binary() | atom() | list().
-type q_storage_table_name() :: binary() | atom() | list().

-record(q_storage_namespace_status, {
  available   = undefined :: undefined | false | true,
  details     = undefined :: undefined | atom()
}).
-type q_storage_namespace_status() :: #q_storage_namespace_status{}.

-record(q_storage_table, {
  namespace   = undefined :: q_storage_namespace_name(),
  name        = undefined :: q_storage_table_name()
}).
-type q_storage_table() :: #q_storage_table{}.

-record(q_storage_table_specs, {
  table      = undefined :: q_storage_table(),
  options    = [] :: proplists()
}).
-type q_storage_table_specs() :: #q_storage_table_specs{}.

-record(q_storage_row, {
  uuid        = null :: null | binary(),
  father_uuid = null :: null | binary(),
  type        = null :: null | binary(),
  revision    = null :: null | integer(),
  attributes  = null :: null | [{binary(), term()}],
  create_time = null :: null | integer(),
  update_time = null :: null | integer()
}).
-type q_storage_row() :: #q_storage_row{}.

%% Select optimal limit
-define(Q_STORAGE_SELECT_OPTIMAL_LIMIT, 50).

%% System storage tables
-define(Q_STORAGE_TABLE_NODES,            <<"nodes">>).
-define(Q_STORAGE_TABLE_NODES_PROPERTIES, <<"nodes_properties">>).
-define(Q_STORAGE_TABLE_NODES_CHILDREN,   <<"nodes_children">>).
-define(Q_STORAGE_TABLE_NODES_EDGES,      <<"nodes_edges">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STORAGE schema
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(Q_STORAGE_NAMESPACE_TEMPORARY,          <<"t">>).
-define(Q_STORAGE_NAMESPACE_HIGH_PERFORMANCE,   <<"h">>).
-define(Q_STORAGE_NAMESPACE_NORMAL_PERFORMANCE, <<"n">>).
-define(Q_STORAGE_NAMESPACE_LOW_PERFORMANCE,    <<"l">>).
-define(Q_STORAGE_NAMESPACE_ARCHIVE,            <<"a">>).

%% Namespaces schema list
-define(Q_STORAGE_SCHEMA_NAMESPACES_LIST, [
  ?Q_STORAGE_NAMESPACE_TEMPORARY,
  ?Q_STORAGE_NAMESPACE_HIGH_PERFORMANCE,
  ?Q_STORAGE_NAMESPACE_NORMAL_PERFORMANCE,
  ?Q_STORAGE_NAMESPACE_LOW_PERFORMANCE,
  ?Q_STORAGE_NAMESPACE_ARCHIVE]).

%% Namespaces schema drivers list
-define(Q_STORAGE_SCHEMA_NAMESPACES_DRIVERS_LIST, [
  {?Q_STORAGE_NAMESPACE_TEMPORARY, qext_storage_driver_fs},
  {?Q_STORAGE_NAMESPACE_HIGH_PERFORMANCE, qext_storage_driver_fs},
  {?Q_STORAGE_NAMESPACE_NORMAL_PERFORMANCE, qext_storage_driver_fs},
  {?Q_STORAGE_NAMESPACE_LOW_PERFORMANCE, qext_storage_driver_fs},
  {?Q_STORAGE_NAMESPACE_ARCHIVE, qext_storage_driver_fs}
]).

%% Namespaces specifications
-define(Q_STORAGE_SCHEMA_NAMESPACES_SPECS, [
  {?Q_STORAGE_NAMESPACE_TEMPORARY, [path = "/temporary"]},
  {?Q_STORAGE_NAMESPACE_HIGH_PERFORMANCE, [path = "/high_performance"]},
  {?Q_STORAGE_NAMESPACE_NORMAL_PERFORMANCE, [path = "/normal_performance"]},
  {?Q_STORAGE_NAMESPACE_LOW_PERFORMANCE, [path = "/low_performance"]},
  {?Q_STORAGE_NAMESPACE_ARCHIVE, [path = "/archive"]}
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% QRI (Q resource identifier)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(Q_QRI_SCHEME_STORAGE, <<"qs">>).

-record(q_qri_storage_components, {
  namespace = Namespace,
  table = Table,
  id = Id
}).
-type q_qri_storage_components() :: #q_qri_storage_components{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GDB
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This record holds Q GDB node information
-record(q_gdb_node, {

  %% Uuid
  uuid            = null :: q_uuid(),

  %% Type
  type            = null :: binary(),

  %% Site this node belongs to
  site            = null :: binary(),

  %% Owner node uuid
  owner           = null :: null | binary(),

  %% Version
  version         = null :: binary(),

  %% Created
  time_created    = null :: integer(),

  %% Attributes values
  attributes      = null :: null | [{atom(), term()}]

}).

-type q_gdb_node() :: #q_gdb_node{}.