{   
    application, yunbi,
    [   
        {description, "get data from yunbi.com with websocket"},   
        {vsn, "0.1"},   
        {modules,[]},
        {registered, [ yunbi]},
        {applications, [kernel, stdlib, sasl]},   
        {mod, { yunbi_app, []}},
        {start_phases, []},
		{env, []}
    ]   
}. 