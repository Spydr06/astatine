project(
    'astatine',
    'c', 'cpp',
    version: '0.0.1',
    default_options: [
        'warning_level=3',
        'c_std=c11',
        'cpp_std=c++11'
    ]
)

build_args = [
    '-DPROJECT_NAME=' + meson.project_name(),
    '-DPROJECT_VERSION=' + meson.project_version(),
]

subdir('src/libastatine')
subdir('src/astatc')