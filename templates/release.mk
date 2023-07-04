# Release templates.

tpl_relx_config_dir = ./
tpl_relx_config_ext = .relx.config
define tpl_relx_config
{release, {$(n)_release, "1"}, [$(n), sasl, runtime_tools]}.
{dev_mode, false}.
{include_erts, true}.
{extended_start_script, true}.
{sys_config, "config/sys.config"}.
{vm_args, "config/vm.args"}.
endef

tpl_sys_config_dir = ./config/
tpl_sys_config_ext = .sys.config
define tpl_sys_config
[
].
endef

tpl_vm_args_dir = ./config/
tpl_vm_args_ext = .vm.args
define tpl_vm_args
-name $(n)@127.0.0.1
-setcookie $(n)
-heart
endef