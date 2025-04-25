#!/usr/bin/env escript
%%! -smp enable

main([SourcePath]) ->
    case filelib:is_dir(SourcePath) of
        true ->
            Patterns = [SourcePath ++ "/**/*.erl", SourcePath ++ "/*.erl"],
            Files = lists:flatten([filelib:wildcard(Pattern) || Pattern <- Patterns]);
        false ->
            Files = [SourcePath]
    end,
    
    % Process each file and extract dependencies
    Dependencies = lists:foldl(
        fun(File, Acc) ->
            {ok, Content} = file:read_file(File),
            Lines = binary:split(Content, <<"\n">>, [global]),
            
            % Look for -include and -include_lib directives
            IncludeDeps = extract_includes(Lines),
            
            % Look for module imports
            ModuleDeps = extract_imports(Lines),
            
            % Add to accumulator
            IncludeDeps ++ ModuleDeps ++ Acc
        end, [], Files),
    
    % Output unique dependencies
    UniqueDeps = lists:usort(Dependencies),
    io:format("~p~n", [UniqueDeps]).

extract_includes(Lines) ->
    IncludePattern = "-include\\(.*?\\)\\.",
    IncludeLibPattern = "-include_lib\\(.*?\\)\\.",
    
    Includes = lists:foldl(
        fun(Line, Acc) ->
            LineStr = binary_to_list(Line),
            case re:run(LineStr, IncludePattern) of
                {match, _} ->
                    {match, [Include]} = re:run(LineStr, "\"(.*?)\"", [{capture, all_but_first, list}]),
                    [Include | Acc];
                nomatch ->
                    case re:run(LineStr, IncludeLibPattern) of
                        {match, _} ->
                            {match, [Include]} = re:run(LineStr, "\"(.*?)\"", [{capture, all_but_first, list}]),
                            [Include | Acc];
                        nomatch ->
                            Acc
                    end
            end
        end, [], Lines),
    Includes.

extract_imports(Lines) ->
    % Look for usage of remote modules
    ModuleUsagePattern = "[A-Za-z][A-Za-z0-9_]*:[A-Za-z][A-Za-z0-9_]*\\(",
    
    Modules = lists:foldl(
        fun(Line, Acc) ->
            LineStr = binary_to_list(Line),
            case re:run(LineStr, ModuleUsagePattern) of
                {match, _} ->
                    Matches = re:run(LineStr, "([A-Za-z][A-Za-z0-9_]*):", [{capture, all_but_first, list}, global]),
                    case Matches of
                        {match, ListOfModules} ->
                            FlattenedModules = lists:flatten(ListOfModules),
                            FlattenedModules ++ Acc;
                        _ ->
                            Acc
                    end;
                nomatch ->
                    Acc
            end
        end, [], Lines),
    
    % Filter out standard modules that are part of OTP
    StdModules = ["io", "lists", "maps", "proplists", "file", "erlang", "gen_server", "application",
                  "supervisor", "crypto", "ssl", "inets", "string", "binary", "code", "gen_statem",
                  "gen_event", "timer", "os", "filelib", "re", "proc_lib", "sys", "unicode"],
    
    lists:filter(fun(Module) -> not lists:member(Module, StdModules) end, lists:usort(Modules)).
