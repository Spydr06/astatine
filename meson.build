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

commit_cmd = run_command('git', 'describe', '--always', '--dirty', check : true)
commit_id = commit_cmd.stdout().strip()

build_args = [
    '-DPROJECT_NAME="' + meson.project_name() + '"',
    '-DPROJECT_VERSION="' + meson.project_version() + '"',
    '-DPROJECT_BUILD="' + commit_id + '"',
]

subdir('src/libastatine')
subdir('src/astatc')
subdir('tests')