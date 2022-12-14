libastatine_description = 'Astatine compiler library'

libastatine_public_include_dirs = [
    'include'
]

libastatine_public_headers = [
    join_paths(libastatine_public_include_dirs[0], 'astatine.h')
]

libastatine_source_files = [
    'astatine.c',
    'context.c',
    'version.c',
    join_paths('lexer', 'lexer.c'),
    join_paths('lexer', 'token.c'),
    join_paths('parser', 'parser.c'),
    join_paths('memory', 'arena.c'),
    join_paths('memory', 'list.c'),
    join_paths('memory', 'hashmap.c'),
    join_paths('ast', 'module.c'),
    join_paths('ast', 'node.c'),
    join_paths('ast', 'debug.c'),
    join_paths('ast', 'object.c'),
]

libastatine_target = library(
    'libastatine',
    libastatine_source_files,
    install: true,
    c_args: build_args,
    include_directories: libastatine_public_include_dirs,
)

# Make this library usable as a Meson subproject.
libastatine_dep = declare_dependency(
    include_directories: libastatine_public_include_dirs,
    link_with: libastatine_target
)
set_variable('libastatine' + '_dep', libastatine_dep)

# Make this library usable from the system's
# package manager.
install_headers(libastatine_public_headers, subdir : meson.project_name())

pkg_mod = import('pkgconfig')
pkg_mod.generate(
    name: 'astatine',
    filebase: 'astatine',
    description: libastatine_description,
    libraries: libastatine_target,
)