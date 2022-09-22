libastatine_description = 'Astatine compiler library'

libastatine_public_headers = [
    'include/'
]

libastatine_source_files = [
    'astatine.c'
]

project_target = static_library(
    'libastatine',
    libastatine_source_files,
    install: false,
    c_args: build_args,
    include_directories: libastatine_public_headers,
)

libastatine_dep = declare_dependency(
    include_directories: libastatine_public_headers,
    link_with: project_target
)
set_variable('libastatine' + '_dep', libastatine_dep)