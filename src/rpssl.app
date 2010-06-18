{application, rpssl, [
    {description, "rpssl"},
    {vsn, "0.01"},
    {modules, [
        rpssl_app, webserver, gameserver, rpssl, uuid
    ]},
    {mod, {rpssl_app, []}},
    {applications, [kernel, stdlib]}]
}.
