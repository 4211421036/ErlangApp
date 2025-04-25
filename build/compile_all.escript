#!/usr/bin/env escript
%%! -smp enable

main([OutputDir | _]) ->
    % Scan for all .erl files in current directory and subdirectories
    Files = filelib:fold_files(
        ".", 
        ".*\\.erl$", 
        true,
        fun(F, Acc) -> [F | Acc] end,
        []
    ),
    
    io:format("Found ~p Erlang source files~n", [length(Files)]),
    
    % Compile each file
    Results = [compile_file(F, OutputDir) || F <- Files],
    
    % Check for any compilation errors
    case lists:any(fun(R) -> R =:= error end, Results) of
        true -> io:format("Compilation failed~n"), halt(1);
        false -> io:format("All files compiled successfully~n")
    end.

compile_file(File, OutputDir) ->
    io:format("Compiling ~s~n", [File]),
    case compile:file(File, [debug_info, {outdir, OutputDir}]) of
        {ok, _Module} -> 
            io:format("Successfully compiled ~s~n", [File]),
            ok;
        error ->
            io:format("Failed to compile ~s~n", [File]),
            error
    end.
