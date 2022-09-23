#version_h = vcs_tag(
#    input: 'version.h.in',
#    output: 'version.h'
#)

astatc_source_files = [
    'main.c',
    'flags.c'
]

astatc_dependencies = [
    libastatine_dep
]

astatc_target = executable(
    'astatc',
    astatc_source_files,
    dependencies: astatc_dependencies,
    install : true,
    c_args : build_args,
)
