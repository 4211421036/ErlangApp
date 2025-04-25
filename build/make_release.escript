#!/usr/bin/env escript
%%! -smp enable

main([MainModule | _]) ->
    % Convert string to atom
    MainModuleAtom = list_to_atom(MainModule),
    
    % Find all beam files
    ModuleFiles = filelib:wildcard("*.beam"),
    Modules = [list_to_atom(filename:basename(F, ".beam")) || F <- ModuleFiles],
    
    io:format("Found modules: ~p~n", [Modules]),
    io:format("Main module: ~p~n", [MainModuleAtom]),
    
    % Create .app file for our application
    AppFile = MainModule ++ ".app",
    AppContent = io_lib:format(
        "{application, ~s, [
            {description, \"Erlang Application\"},
            {vsn, \"1.0.0\"},
            {modules, ~p},
            {registered, []},
            {applications, [kernel, stdlib]},
            {mod, {~s, []}}
        ]}.", [MainModuleAtom, Modules, MainModuleAtom]),
    
    file:write_file(AppFile, AppContent),
    io:format("Created application file: ~s~n", [AppFile]),
    
    % Create .rel file
    RelFile = MainModule ++ ".rel",
    RelContent = io_lib:format(
        "{release, {\"~s\", \"1.0.0\"}, 
         {erts, \"~s\"},
         [{kernel, \"8.0\"}, 
          {stdlib, \"4.0\"},
          {\"~s\", \"1.0.0\"}]
        }.", [MainModule, erlang:system_info(version), MainModule]),
    
    file:write_file(RelFile, RelContent),
    io:format("Created release file: ~s~n", [RelFile]),
    
    % Generate boot scripts
    systools:make_script(MainModule, []),
    systools:make_tar(MainModule, [{erts, code:root_dir()}]),
    
    io:format("Release package created successfully~n").
