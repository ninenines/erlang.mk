# Bootstrap templates.

tpl_appsrc_ext = .app.src
define tpl_appsrc
{application, $(n), [
	{description, ""},
	{vsn, "0.1.0"},
	{id, "git"},
	{modules, []},
	{registered, []},
	{applications, [
		kernel,
		stdlib
	]},
	$(if $(wildcard src/$(n)_app)$(app),,{mod$(comma) {$(if $(app),$(app),$(n)_app)$(comma) []}}$(comma))
	{env, []}
]}.
endef