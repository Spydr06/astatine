# Builds google test as a dependency called "test_dep".

project_test_files = [
    'unit_tests.cpp',
    join_paths('internals', 'lexer.cpp')
]

if not meson.is_subproject()    

    gtest_dir = 'googletest/googletest'
    gtest_incdir = include_directories(join_paths(gtest_dir, 'include'), is_system: true)

    libgtest = static_library(
        'gtest',
        cpp_args : ['-w'],
        include_directories : [include_directories(gtest_dir), gtest_incdir],
        sources : [
            join_paths(gtest_dir, 'src', 'gtest-all.cc'),
            join_paths(gtest_dir, 'src', 'gtest_main.cc')
        ]
    )

    libastatine_incdir = join_paths('..', 'src', 'libastatine')

    test_dep = declare_dependency(
        dependencies : dependency('threads'),
        include_directories : [
            gtest_incdir,
            libastatine_incdir,
        ],
        link_with : libgtest
    )

    test('all_tests',
        executable(
            'run_tests',
            files(project_test_files),
            dependencies : [libastatine_dep, test_dep],
            install : false
        )
    )
endif